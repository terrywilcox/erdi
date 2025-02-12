-module(erdi_dispatcher).

-behaviour(gen_server).

-include("erdi.hrl").

-export([start_link/0, stop/0, handle_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

handle_message(Message, UserId) ->
  gen_server:cast(?MODULE, {Message, UserId}).

init(_) ->
  {ok, Pid} = gen_event:start_link(),
  gen_event:add_handler(Pid, erdi_message_waver, []),
  gen_event:add_handler(Pid, erdi_message_replier, []),
  {ok, #{pid => Pid}}.

handle_call(_Any, _From, State) ->
  {reply, ok, State}.

handle_cast({Message, UserId}, #{pid := Pid} = State) ->
  Type = maps:get(?TYPE, Message),
  io:format(user, "dispatcher got message ~p~n", [Type]),
  io:format(user, "dispatching user id ~p~n", [UserId]),
  gen_event:notify(Pid, {Type, Message, UserId}),
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, #{pid := Pid} = _State) ->
  io:format("Stopping ~p~n", [?MODULE]),
  exit(Pid, normal),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
