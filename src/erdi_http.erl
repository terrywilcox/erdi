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
  {response, _Fin, Code, _Headers} = gun:await(Conn, Ref),
  {ok, ResponseBody} = gun:await_body(Conn, Ref),
  ResponseTerm = json:decode(ResponseBody),
  Response =
    case Code >= 400 of
      true ->
        {error, Code, ResponseTerm};
      _ ->
        {ok, Code, ResponseTerm}
    end,
  io:format(user, "what is wrong? ~p, ~p~n", [Code, ResponseBody]),
  {reply, Response, State};
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

send(Method, Conn, BasePath, Path, Headers, Body) when is_binary(Body) ->
  FullPath = full_path(BasePath, Path),
  send(Method, Conn, FullPath, Headers, Body);
send(Method, Conn, BasePath, Path, Headers, Body) when is_list(Body) ->
  send(Method, Conn, BasePath, Path, Headers, iolist_to_binary(Body));
send(Method, Conn, BasePath, Path, Headers, Body) when is_map(Body) ->
  send(Method, Conn, BasePath, Path, Headers, iolist_to_binary(json:encode(Body))).

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
  full_path(iolist_to_binary(BasePath), iolist_to_binary(Path)).

headers() ->
  Token = secret_token(),
  [{<<"content-type">>, "application/json"},
   {<<"authorization">>, <<"Bot ", Token/binary>>},
   {<<"user-agent">>, <<"DiscordBot (https://github.com/terrywilcox/erdi, 0.1">>}].

secret_token() ->
  list_to_binary(os:getenv("DISCORD_SECRET_TOKEN")).
