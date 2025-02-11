-module(erdi_http).

-behaviour(gen_server).

-export([start_link/0, stop/0, get/1, send/3, send/2, send/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get(Path) ->
    gen_server:call(?MODULE, {get, Path}).

send(Method, Path, Content) when
    Method == put; Method == post; Method == patch; Method == delete
->
    gen_server:cast(?MODULE, {send, {Method, Path, Content}}).

send(Method, Path) when
    Method == put; Method == post; Method == patch; Method == delete
->
    gen_server:cast(?MODULE, {send, {Method, Path, <<>>}}).

send({Method, Path}) when
    Method == put; Method == post; Method == patch; Method == delete
->
    gen_server:cast(?MODULE, {send, {Method, Path, <<>>}}).

init(_) ->
    {ok, #{
        domain => "discord.com",
        port => 443,
        protocols => [http],
        tls_opts => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}],
        path => "/api/v10/",
        headers => headers()
    }}.

handle_call(
    {get, Path},
    _From,
    #{
        domain := Domain,
        port := Port,
        protocols := Protocols,
        tls_opts := TLSOpts,
        path := BasePath,
        headers := Headers
    } =
        State
) ->
    {ok, Conn} = gun:open(Domain, Port, #{protocols => Protocols, tls_opts => TLSOpts}),
    {ok, _Protocol} = gun:await_up(Conn),
    Ref = gun:get(Conn, BasePath ++ Path, Headers),
    {response, nofin, 200, _Headers} = gun:await(Conn, Ref),
    {ok, Body} = gun:await_body(Conn, Ref),
    {reply, json:decode(Body), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Any, _From, State) ->
    {reply, ok, State}.

handle_cast(
    {send, {Method, Path, Content}},
    #{
        domain := Domain,
        port := Port,
        protocols := Protocols,
        tls_opts := TLSOpts,
        path := BasePath,
        headers := Headers
    } =
        State
) ->
    {ok, Conn} =
        gun:open(
            Domain,
            Port,
            #{
                protocols => Protocols,
                tls_opts => TLSOpts,
                http_opts => #{keepalive => infinity}
            }
        ),
    {ok, _Protocol} = gun:await_up(Conn),
    send(Method, Conn, [BasePath, Path], Headers, Content),
    {noreply, State};
handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Stopping ~p~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(post, Conn, Path, Headers, Json) ->
    Path2 = iolist_to_binary(Path),
    io:format(user, "**** this is the stuff: ~p, ~p~n", [Path2, Json]),
    gun:post(Conn, Path2, Headers, Json);
send(put, Conn, Path, Headers, Json) ->
    gun:put(Conn, Path, Headers, Json);
send(delete, Conn, Path, Headers, _Json) ->
    gun:delete(Conn, Path, Headers);
send(patch, Conn, Path, Headers, Json) ->
    gun:patch(Conn, Path, Headers, Json).

headers() ->
    Token = secret_token(),
    [
        {<<"content-type">>, "application/json"},
        {<<"authorization">>, <<"Bot ", Token/binary>>},
        {<<"user-agent">>, <<"DiscordBot (https://github.com/terrywilcox/erdi, 0.1">>}
    ].

secret_token() ->
    list_to_binary(os:getenv("DISCORD_SECRET_TOKEN")).
