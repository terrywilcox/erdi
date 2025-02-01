-module(erdi_heartbeat).

-behaviour(gen_server).

-export([start_link/0, stop/0, start_beating/1, set_sequence/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

start_beating(Args) ->
    gen_server:cast(?MODULE, {start, Args}).

set_sequence(Sequence) ->
    gen_server:cast(?MODULE, {message_seq, Sequence}).

init(_) ->
    {ok, #{}}.

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
handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(
    beat_heart,
    #{
        heartbeat_interval := HeartbeatInterval,
        seq := Sequence,
        conn := Conn,
        ref := Ref
    } =
        State
) ->
    Heartbeat = #{<<"op">> => 1, <<"d">> => Sequence},
    io:format(user, "sending heartbeat: ~p~n", [Heartbeat]),
    Message = iolist_to_binary(json:encode(Heartbeat)),
    gun:ws_send(Conn, Ref, {text, Message}),
    erlang:send_after(HeartbeatInterval, self(), beat_heart),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Stopping ~p~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
