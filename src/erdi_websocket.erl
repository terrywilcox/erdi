-module(erdi_websocket).

-behaviour(gen_statem).

-export([start_link/0]).
-export([connect/0]).

-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).

-export([get_url/3]).
-export([not_connected/3]).
-export([wait_hello/3]).
-export([identify/3]).
-export([ready/3]).

start_link() ->
    io:format("erdi_websocket start_link~n"),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    ok.

callback_mode() ->
    [state_functions, state_enter].

init([]) ->
    Data = maps:from_list(application:get_all_env(erdi)),
    Data1 = Data#{gateway_tls => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}]},
    Data2 = Data1#{websocket_tls => [{verify, verify_none}, {cacerts, public_key:cacerts_get()}]},
    {ok, get_url, Data2}.

get_url(
    enter,
    _OldStateName,
    #{
        gateway_tls := TLSOpts,
        gateway_domain := Domain,
        port := Port,
        gateway_path := Path
    } = Data
) ->
    {ok, Conn} = gun:open(Domain, Port, #{tls_opts => TLSOpts}),
    {ok, _Protocol} = gun:await_up(Conn),
    _StreamRef = gun:get(Conn, Path),
    {keep_state, Data};
get_url(info, {gun_response, _Pid, _Ref, nofin, _Code, _Headers}, Data) ->
    {keep_state, Data};
get_url(info, {gun_data, _Pid, _Ref, nofin, HttpData}, Data) ->
    #{<<"url">> := <<"wss://", Url/binary>>} = json:decode(HttpData),
    {keep_state, Data#{url => binary_to_list(Url)}};
get_url(info, {gun_data, Pid, _Ref, fin, _HttpData}, Data) ->
    gun:close(Pid),
    {next_state, not_connected, Data}.

not_connected(
    enter,
    _OldStateName,
    #{
        url := Url,
        port := Port,
        websocket_tls := TLSOpts,
        websocket_protocols := Protocols,
        websocket_path := Path
    } = Data
) ->
    {ok, Conn} = gun:open(Url, Port, #{tls_opts => TLSOpts, protocols => Protocols}),
    {ok, _Protocol} = gun:await_up(Conn),
    gun:ws_upgrade(Conn, Path),
    {keep_state, Data};
not_connected(info, {gun_upgrade, Pid, Ref, [<<"websocket">>], _Headers}, Data) ->
    {next_state, wait_hello, Data#{conn => Pid, ref => Ref}};
not_connected(info, {gun_response, _Pid, _, _, _Status, _Headers}, Data) ->
    {keep_state, Data}.

wait_hello(enter, _OldStateName, Data) ->
    {keep_state, Data};
wait_hello(info, {gun_ws, _Pid, _Ref, {text, Content}}, Data) ->
    #{<<"op">> := OpCode, <<"d">> := Dee} = json:decode(Content),
    #{<<"heartbeat_interval">> := HeartbeatInterval} = Dee,
    io:format(user, "wait hello op_code ~p~n", [OpCode]),
    io:format(user, "wait hello heartbeat_interval ~p~n", [HeartbeatInterval]),
    {next_state, identify, Data#{heartbeat_interval => HeartbeatInterval}}.

identify(enter, _OldStateName, #{} = #{conn := Pid, ref := Ref} = Data) ->
    % send identify events, but no more than 1000 in a 24 hour period
    IdentifyMsg = #{
        op => 2,
        d => #{
            <<"token">> => secret_token(),
            <<"intents">> => 513,
            <<"properties">> => #{
                <<"os">> => <<"Linux">>,
                <<"browser">> => <<"erdi_library">>,
                <<"device">> => <<"erdi_library">>
            }
        }
    },
    IdentifyJson = iolist_to_binary(json:encode(IdentifyMsg)),
    io:format(user, "Identify JSON: ~p~n", [IdentifyJson]),
    gun:ws_send(Pid, Ref, {text, IdentifyJson}),
    io:format(user, "********** sent Identify JSON: ~n", []),
    {keep_state, Data};
identify(info, {gun_ws, _, _, Frame}, Data) ->
    io:format(user, "response from identify: ~p~n", [Frame]),
    {keep_state, Data}.

ready(_, _, _) ->
    ok.

code_change(_, _, _, _) ->
    ok.

terminate(_, _, _) ->
    ok.

secret_token() ->
    list_to_binary(os:getenv("DISCORD_SECRET_TOKEN")).
