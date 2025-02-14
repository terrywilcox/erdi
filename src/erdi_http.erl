-module(erdi_http).

-behaviour(gen_server).

-export([start_link/0, stop/0, send/1]).
-export([get/1, send_await/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

get(Path) ->
  gen_server:call(?MODULE, {get, Path}).

-spec send_await({atom(), iolist()} | {atom(), iolist(), iolist()}) -> binary().
send_await({Method, Path}) ->
  gen_server:call(?MODULE, {send, Method, Path, <<>>});
send_await({Method, Path, Body}) ->
  gen_server:call(?MODULE, {send, Method, Path, Body}).

-spec send({atom(), iolist()} | {atom(), iolist(), iolist()}) -> ok.
send({Method, Path}) ->
  gen_server:cast(?MODULE, {send, Method, Path, <<>>});
send({Method, Path, Body}) ->
  gen_server:cast(?MODULE, {send, Method, Path, Body}).

init(_) ->
  {ok,
   #{domain => "discord.com",
     port => 443,
     protocols => [http],
     tls_opts => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}],
     path => "/api/v10/",
     headers => headers()}}.

handle_call({get, Path},
            _From,
            #{domain := Domain,
              port := Port,
              protocols := Protocols,
              tls_opts := TLSOpts,
              path := BasePath,
              headers := Headers} =
              State) ->
  {ok, Conn} = gun:open(Domain, Port, #{protocols => Protocols, tls_opts => TLSOpts}),
  {ok, _Protocol} = gun:await_up(Conn),
  Ref = gun:get(Conn, BasePath ++ Path, Headers),
  {response, nofin, 200, _Headers} = gun:await(Conn, Ref),
  {ok, Body} = gun:await_body(Conn, Ref),
  {reply, json:decode(Body), State};
handle_call({send, Method, Path, Body},
            _From,
            #{domain := Domain,
              port := Port,
              protocols := Protocols,
              tls_opts := TLSOpts,
              path := BasePath,
              headers := Headers} =
              State) ->
  {ok, Conn} =
    gun:open(Domain,
             Port,
             #{protocols => Protocols,
               tls_opts => TLSOpts,
               http_opts => #{keepalive => infinity}}),
  {ok, _Protocol} = gun:await_up(Conn),
  Ref = send(Method, Conn, BasePath, Path, Headers, Body),
  {response, Fin, Code, _Headers} = gun:await(Conn, Ref),
  io:format(user, "what is wrong? ~p, ~p~n", [Fin, Code]),
  {ok, ResponseBody} = gun:await_body(Conn, Ref),
  {reply, json:decode(ResponseBody), State};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Any, _From, State) ->
  {reply, ok, State}.

handle_cast({send, Method, Path, Body},
            #{domain := Domain,
              port := Port,
              protocols := Protocols,
              tls_opts := TLSOpts,
              path := BasePath,
              headers := Headers} =
              State) ->
  {ok, Conn} =
    gun:open(Domain,
             Port,
             #{protocols => Protocols,
               tls_opts => TLSOpts,
               http_opts => #{keepalive => infinity}}),
  {ok, _Protocol} = gun:await_up(Conn),
  send(Method, Conn, BasePath, Path, Headers, Body),
  {noreply, State};
handle_cast(_Any, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send(Method, Conn, BasePath, Path, Headers, Body) ->
  FullPath = full_path(BasePath, Path),
  send(Method, Conn, FullPath, Headers, iolist_to_binary(json:encode(Body))).

send(get, Conn, Path, Headers, _Body) ->
  gun:get(Conn, Path, Headers);
send(post, Conn, Path, Headers, Body) ->
  gun:post(Conn, Path, Headers, Body);
send(put, Conn, Path, Headers, Body) ->
  gun:put(Conn, Path, Headers, Body);
send(delete, Conn, Path, Headers, _Body) ->
  gun:delete(Conn, Path, Headers);
send(patch, Conn, Path, Headers, Body) ->
  gun:patch(Conn, Path, Headers, Body).

full_path(BasePath, Path) when is_binary(BasePath), is_binary(Path) ->
  iolist_to_binary([BasePath, Path]);
full_path(BasePath, Path) ->
  iolist_to_binary([iolist_to_binary(BasePath), iolist_to_binary(Path)]).

headers() ->
  Token = secret_token(),
  [{<<"content-type">>, "application/json"},
   {<<"authorization">>, <<"Bot ", Token/binary>>},
   {<<"user-agent">>, <<"DiscordBot (https://github.com/terrywilcox/erdi, 0.1">>}].

secret_token() ->
  list_to_binary(os:getenv("DISCORD_SECRET_TOKEN")).
