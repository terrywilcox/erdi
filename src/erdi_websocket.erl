-module(erdi_websocket).

-behaviour(gen_statem).

-include("erdi.hrl").

-export([start_link/0]).
-export([reconnect/0]).
-export([send/1]).
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).
-export([get_ws_url/3]).
-export([connecting_ws/3]).
-export([wait_hello/3]).
-export([identify/3]).
-export([ready/3]).
-export([resuming/3]).

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

reconnect() ->
  gen_statem:cast(?MODULE, reconnect).

send(Message) ->
  gen_statem:cast(?MODULE, {send, Message}).

callback_mode() ->
  [state_functions, state_enter].

init([]) ->
  Data =
    maps:from_list(
      application:get_all_env(erdi)),
  Data1 =
    Data#{gateway_tls => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}],
          ws_tls => [{verify, verify_none}, {cacerts, public_key:cacerts_get()}]},
  {ok, get_ws_url, Data1}.

get_ws_url(enter,
           _OldStateName,
           #{gateway_tls := TLSOpts,
             gateway_domain := Domain,
             port := Port,
             gateway_path := Path} =
             Data) ->
  open_http(Domain, Port, Path, TLSOpts),
  {keep_state, Data};
get_ws_url(info, {gun_response, _Pid, _Ref, nofin, _Code, _Headers}, Data) ->
  {keep_state, Data};
get_ws_url(info, {gun_data, _Pid, _Ref, nofin, HttpData}, Data) ->
  Data2 =
    case decode_ws_url(HttpData) of
      undefined ->
        Data;
      Url ->
        Data#{ws_url => binary_to_list(Url)}
    end,
  {keep_state, Data2};
get_ws_url(info, {gun_data, Pid, _Ref, fin, HttpData}, Data) ->
  Data2 =
    case decode_ws_url(HttpData) of
      undefined ->
        Data;
      Url ->
        Data#{ws_url => binary_to_list(Url)}
    end,
  gun:close(Pid),
  {next_state, connecting_ws, Data2}.

resuming(enter,
         _OldStateName,
         #{resume_gateway_url := Url,
           conn := Conn,
           port := Port,
           ws_tls := TLSOpts,
           ws_protocols := Protocols,
           ws_path := Path} =
           Data) ->
  gun:close(Conn),
  {ok, NewConn} = gun:open(Url, Port, #{tls_opts => TLSOpts, protocols => Protocols}),
  {ok, _Protocol} = gun:await_up(NewConn),
  gun:ws_upgrade(NewConn, Path),
  {keep_state, Data};
resuming(info,
         {gun_upgrade, Pid, Ref, [<<"websocket">>], _Headers},
         #{session_id := SessionId, seq := Seq} = Data) ->
  send_resume(Pid, Ref, SessionId, Seq),
  {next_state, ready, Data#{conn => Pid, ref => Ref}};
resuming(info, {gun_ws, _Pid, _Ref, {text, _Content}}, Data) ->
  {keep_state, Data};
resuming(info, {gun_response, _Pid, _, _, _Status, _Headers}, Data) ->
  {keep_state, Data}.

connecting_ws(enter,
              _OldStateName,
              #{ws_url := Url,
                port := Port,
                ws_tls := TLSOpts,
                ws_protocols := Protocols,
                ws_path := Path} =
                Data) ->
  {ok, Conn} = gun:open(Url, Port, #{tls_opts => TLSOpts, protocols => Protocols}),
  {ok, _Protocol} = gun:await_up(Conn),
  gun:ws_upgrade(Conn, Path),
  {keep_state, Data};
connecting_ws(info, {gun_upgrade, Pid, Ref, [<<"websocket">>], _Headers}, Data) ->
  {next_state, wait_hello, Data#{conn => Pid, ref => Ref}};
connecting_ws(info, {gun_response, _Pid, _, _, _Status, _Headers}, Data) ->
  {keep_state, Data}.

wait_hello(enter, _OldStateName, Data) ->
  {keep_state, Data};
wait_hello(info, {gun_ws, Pid, Ref, {text, Content}}, Data) ->
  #{?OPCODE := OpCode, ?DATA := DiscordData} = json:decode(Content),

  case OpCode of
    ?OP_HELLO ->
      #{?HEARTBEAT_INTERVAL := HeartbeatInterval} = DiscordData,
      erdi_heartbeat:start_beating(#{heartbeat_interval => HeartbeatInterval,
                                     conn => Pid,
                                     ref => Ref}),
      {next_state, identify, Data#{heartbeat_interval => HeartbeatInterval}};
    _ ->
      io:format(user, "what is this? ~p~n", [OpCode]),
      {keep_state, Data}
  end.

identify(enter,
         _OldStateName,
         #{conn := Pid,
           ref := Ref,
           intents := Intents} =
           Data) ->
  % send identify events, but no more than 1000 in a 24 hour period
  send_identify(Pid, Ref, Intents),
  {keep_state, Data};
identify(cast, reconnect, Data) ->
  {next_state, resuming, Data};
identify(info, {gun_ws, _, _, {text, Response}}, Data) ->
  ResponseTerm = json:decode(Response),
  OpCode = maps:get(?OPCODE, ResponseTerm),
  Return =
    case OpCode of
      ?OP_HEARTBEAT_ACK ->
        io:format(user, "heartbeat ACK ~p~n", [ResponseTerm]),
        {keep_state, Data};
      ?OP_DISPATCH ->
        {next_state, ready, Data, [postpone]};
      ?OP_RECONNECT ->
        io:format(user, "need to resume ~p~n", [ResponseTerm]),
        {keep_state, Data};
      ?OP_INVALID_SESSION ->
        io:format(user, "maybe resume ~p~n", [ResponseTerm]),
        {keep_state, Data};
      _ ->
        io:format(user, "No idea ~p~n", [ResponseTerm]),
        {keep_state, Data}
    end,
  Return.

ready(enter, _OldStateName, Data) ->
  {keep_state, Data};
ready(cast, reconnect, Data) ->
  {next_state, resuming, Data};
ready(cast, {send, Message}, #{conn := Conn, ref := Ref} = Data) ->
  send_message(Conn, Ref, Message),
  {keep_state, Data};
ready(info, {gun_ws, Pid, _, {close, _, _}}, Data) ->
  gun:close(Pid),
  {next_state, resuming, Data};
ready(info, {gun_down, Pid, _, _, _, _}, Data) ->
  gun:close(Pid),
  {next_state, resuming, Data};
ready(info, {gun_ws, _, _, {_, Response}}, Data) ->
  ResponseTerm = json:decode(Response),
  OpCode = maps:get(?OPCODE, ResponseTerm),
  Return =
    case OpCode of
      ?OP_HEARTBEAT_ACK ->
        io:format(user, "heartbeat ACK ~p~n", [ResponseTerm]),
        erdi_heartbeat:receive_ack(),
        {keep_state, Data};
      ?OP_DISPATCH ->
        Data2 = handle_message(ResponseTerm, Data),
        {keep_state, Data2};
      ?OP_INVALID_SESSION ->
        io:format(user, "maybe resume ~p~n", [ResponseTerm]),
        {next_state, resuming, Data};
      ?OP_RECONNECT ->
        io:format(user, "need to resume ~p~n", [ResponseTerm]),
        {next_state, resuming, Data};
      _ ->
        io:format(user, "No idea ~p~n", [ResponseTerm]),
        {keep_state, Data}
    end,
  Return.

code_change(_, _, _, _) ->
  ok.

terminate(_, _, _) ->
  ok.

secret_token() ->
  list_to_binary(os:getenv("DISCORD_SECRET_TOKEN")).

handle_message(#{?TYPE := <<"READY">>, ?SEQ := NewSeq} = Message, Data) ->
  DiscordData = erdi_discord_utils:message_info(Message),
  NewData = maps:merge(Data, DiscordData),
  erdi_dispatcher:update_state(NewData),
  increment_seq(NewSeq, NewData);
handle_message(#{?SEQ := NewSeq} = Message, Data) ->
  erdi_dispatcher:handle_message(Message, Data),
  increment_seq(NewSeq, Data).

increment_seq(NewSeq, #{seq := Seq} = Data) when is_integer(Seq), NewSeq > Seq ->
  erdi_heartbeat:set_sequence(NewSeq),
  Data#{seq => NewSeq};
increment_seq(_NewSeq, #{seq := Seq} = Data) when is_integer(Seq) ->
  Data;
increment_seq(NewSeq, Data) ->
  erdi_heartbeat:set_sequence(NewSeq),
  Data#{seq => NewSeq}.

open_http(Domain, 443 = Port, Path, TLSOpts) ->
  {ok, Conn} = gun:open(Domain, Port, #{tls_opts => TLSOpts}),
  {ok, _Protocol} = gun:await_up(Conn),
  StreamRef = gun:get(Conn, Path),
  {Conn, StreamRef};
open_http(Domain, Port, Path, _TLSOpts) ->
  {ok, Conn} = gun:open(Domain, Port),
  {ok, _Protocol} = gun:await_up(Conn),
  StreamRef = gun:get(Conn, Path),
  {Conn, StreamRef}.

decode_ws_url(<<>>) ->
  undefined;
decode_ws_url(HttpData) when is_binary(HttpData) ->
  Json = json:decode(HttpData),
  case maps:get(<<"url">>, Json) of
    <<"wss://", Url/binary>> ->
      Url;
    undefined ->
      undefined
  end;
decode_ws_url(HttpData) ->
  io:format(user, "what? ~p~n", [HttpData]),
  undefined.

send_identify(Pid, Ref, Intents) ->
  IdentifyMsg =
    #{op => ?OP_IDENTIFY,
      d =>
        #{<<"token">> => secret_token(),
          <<"intents">> => Intents,
          <<"properties">> =>
            #{<<"os">> => <<"Linux">>,
              <<"browser">> => <<"erdi_library">>,
              <<"device">> => <<"erdi_library">>}}},
  IdentifyJson = iolist_to_binary(json:encode(IdentifyMsg)),
  gun:ws_send(Pid, Ref, {text, IdentifyJson}).

send_resume(Pid, Ref, SessionId, Sequence) ->
  ResumeMsg =
    #{op => ?OP_RESUME,
      d =>
        #{<<"token">> => secret_token(),
          <<"session_id">> => SessionId,
          <<"seq">> => Sequence}},
  ResumeJson = iolist_to_binary(json:encode(ResumeMsg)),
  gun:ws_send(Pid, Ref, {text, ResumeJson}).

send_message(Pid, Ref, Message) ->
  Json = iolist_to_binary(json:encode(Message)),
  gun:ws_send(Pid, Ref, {text, Json}).
