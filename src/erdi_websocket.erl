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
-export([wait_identify/3]).
-export([ready/3]).

start_link() ->
    io:format("erdi_websocket start_link~n"),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    ok.

callback_mode() ->
    [state_functions, state_enter].

init([]) ->
    io:format("erdi_websocket init~n"),
    {ok, get_url, _Data = #{}}.

get_url(enter, _OldStateName, _Data) ->
    io:format(user, "_OldStateName = ~p~n", [_OldStateName]),

    TLSOpts = [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}],
    {ok, Conn} = gun:open("discord.com", 443, #{tls_opts => TLSOpts}),
    io:format(user, "Conn = ~p~n", [Conn]),
    {ok, _Protocol} = gun:await_up(Conn),
    _StreamRef = gun:get(Conn, "/api/gateway"),
    {keep_state, #{}};
get_url(info, {gun_response, _Pid, _Ref, nofin, Code, _Headers}, _Data) ->
    io:format(user, "Code = ~p~n", [Code]),
    {keep_state, #{}};
get_url(info, {gun_data, _Pid, _Ref, nofin, HttpData}, _Data) ->
    io:format(user, "Data = ~p~n", [HttpData]),
    #{<<"url">> := <<"wss://", Url/binary>>} = json:decode(HttpData),
    io:format(user, "Url = ~p~n", [Url]),
    {keep_state, #{url => Url}};
get_url(info, {gun_data, _Pid, _Ref, fin, Data}, _Data) ->
    io:format(user, "Data = ~p~n", [Data]),
    {next_state, not_connected, #{}}.

not_connected(_, _, _) ->
    % start another gun connection to wss address
    %   - replacing wss with https
    % upgrade gun connection to websocket

    % GOTO wait_hello
    ok.

wait_hello(_, _, _) ->
    % receive hello

    % GOTO idenitfy
    ok.

identify(_, _, _) ->
    % send identify events

    % GOTO wait_identify
    ok.

wait_identify(_, _, _) ->
    % receive identify ack
    % Startup heartbeat ... process

    % GOTO ready
    ok.

ready(_, _, _) ->
    ok.

code_change(_, _, _, _) ->
    ok.

terminate(_, _, _) ->
    ok.
