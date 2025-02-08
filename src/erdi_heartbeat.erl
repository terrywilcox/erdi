-module(erdi_heartbeat).

-behaviour(gen_server).

-include("erdi.hrl").

-export([start_link/0, stop/0, start_beating/1, set_sequence/1, receive_ack/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

start_beating(Args) ->
  gen_server:cast(?MODULE, {start, Args}).

set_sequence(Sequence) ->
  gen_server:cast(?MODULE, {message_seq, Sequence}).

receive_ack() ->
  gen_server:cast(?MODULE, receive_ack).

init(_) ->
  {ok, #{unacked_beats => 0}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Any, _From, State) ->
  {reply, ok, State}.

handle_cast({start, #{heartbeat_interval := HeartbeatInterval} = NewState}, OldState) ->
  Jitter = rand:uniform(),
  Wait = round(Jitter * HeartbeatInterval),
  erlang:send_after(Wait, self(), beat_heart),
  State = maps:merge(NewState, OldState),
  {noreply, State};
handle_cast({message_seq, Seq}, State) ->
  {noreply, State#{seq => Seq}};
handle_cast(receive_ack, State) ->
  {noreply, State#{unacked_beats => 0}};
handle_cast(_Any, State) ->
  {noreply, State}.

handle_info(beat_heart,
            #{heartbeat_interval := HeartbeatInterval,
              seq := Sequence,
              conn := Conn,
              ref := Ref,
              unacked_beats := Unacked} =
              State) ->
  Heartbeat = #{<<"op">> => ?OP_HEARTBEAT, <<"d">> => Sequence},
  io:format(user, "sending heartbeat: ~p, unacked: ~p~n", [Heartbeat, Unacked]),
  Message = iolist_to_binary(json:encode(Heartbeat)),
  gun:ws_send(Conn, Ref, {text, Message}),
  erlang:send_after(HeartbeatInterval, self(), beat_heart),
  {noreply, State#{unacked_beats => Unacked + 1}}.

terminate(_Reason, _State) ->
  io:format("Stopping ~p~n", [?MODULE]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
