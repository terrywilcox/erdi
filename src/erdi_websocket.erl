-module(erdi_websocket).

-behaviour(gen_statem).

-export([start_link/0]).
-export([connect/0]).
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).
-export([get_ws_url/3]).
-export([connecting_ws/3]).
-export([wait_hello/3]).
-export([identify/3]).
-export([ready/3]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    ok.

callback_mode() ->
    [state_functions, state_enter].

init([]) ->
    Data = maps:from_list(
        application:get_all_env(erdi)
    ),
    Data1 = Data#{
        gateway_tls =>
            [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}]
    },
    Data2 = Data1#{ws_tls => [{verify, verify_none}, {cacerts, public_key:cacerts_get()}]},
    {ok, get_ws_url, Data2}.

get_ws_url(
    enter,
    _OldStateName,
    #{
        gateway_tls := TLSOpts,
        gateway_domain := Domain,
        port := Port,
        gateway_path := Path
    } =
        Data
) ->
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

connecting_ws(
    enter,
    _OldStateName,
    #{
        ws_url := Url,
        port := Port,
        ws_tls := TLSOpts,
        ws_protocols := Protocols,
        ws_path := Path
    } =
        Data
) ->
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
    #{<<"op">> := OpCode, <<"d">> := DiscordData} = json:decode(Content),

    case OpCode of
        10 ->
            #{<<"heartbeat_interval">> := HeartbeatInterval} = DiscordData,
            erdi_heartbeat:start_beating(#{
                heartbeat_interval => HeartbeatInterval,
                conn => Pid,
                ref => Ref,
                seq => <<"null">>
            }),
            {next_state, identify, Data#{heartbeat_interval => HeartbeatInterval}};
        _ ->
            io:format(user, "what is this? ~p~n", [OpCode]),
            {keep_state, Data}
    end.

identify(
    enter,
    _OldStateName,
    #{} =
        #{
            conn := Pid,
            ref := Ref,
            intents := Intents
        } =
        Data
) ->
    % send identify events, but no more than 1000 in a 24 hour period
    IdentifyMsg =
        #{
            op => 2,
            d =>
                #{
                    <<"token">> => secret_token(),
                    <<"intents">> => Intents,
                    <<"properties">> =>
                        #{
                            <<"os">> => <<"Linux">>,
                            <<"browser">> => <<"erdi_library">>,
                            <<"device">> => <<"erdi_library">>
                        }
                }
        },
    IdentifyJson = iolist_to_binary(json:encode(IdentifyMsg)),
    gun:ws_send(Pid, Ref, {text, IdentifyJson}),
    {keep_state, Data};
identify(info, {gun_ws, _, _, {_, Response}}, Data) ->
    ResponseTerm = json:decode(Response),
    OpCode = maps:get(<<"op">>, ResponseTerm),
    Return =
        case OpCode of
            11 ->
                io:format(user, "heartbeat ACK ~p~n", [ResponseTerm]),
                {keep_state, Data};
            0 ->
                {next_state, ready, Data, [postpone]};
            _ ->
                io:format(user, "No idea ~p~n", [ResponseTerm]),
                {keep_state, Data}
        end,
    Return.

ready(enter, _OldStateName, Data) ->
    io:format(user, "entered ready~n", []),
    {keep_state, Data};
ready(info, {gun_ws, _, _, {_, Response}}, Data) ->
    ResponseTerm = json:decode(Response),
    OpCode = maps:get(<<"op">>, ResponseTerm),
    Return =
        case OpCode of
            11 ->
                io:format(user, "heartbeat ACK ~p~n", [ResponseTerm]),
                {keep_state, Data};
            0 ->
                Seq = maps:get(<<"s">>, ResponseTerm),
                erdi_heartbeat:set_sequence(Seq),
                io:format(user, "Message Sequence ~p: ~p~n", [Seq, ResponseTerm]),
                Data2 = handle_message(ResponseTerm, Data),
                {keep_state, Data2#{seq => Seq}};
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

handle_message(#{<<"t">> := <<"READY">>, <<"d">> := DiscordData} = _Message, Data) ->
    #{
        <<"resume_gateway_url">> := ResumeUrl,
        <<"session_id">> := SessionId,
        <<"guilds">> := Guilds
    } =
        DiscordData,
    io:format(user, " ready message: ~p, ~p, ~p~n", [ResumeUrl, SessionId, Guilds]),
    Data#{
        resume_url => ResumeUrl,
        session_id => SessionId,
        guilds => Guilds
    };
handle_message(#{<<"t">> := Type} = _Message, Data) ->
    io:format(user, " message type: ~p~n", [Type]),
    Data.

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
