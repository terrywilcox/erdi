-module(erdi_dispatcher).

-behaviour(gen_server).

-include("erdi.hrl").

-export([start_link/0, stop/0, handle_message/1]).
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

handle_message(Message) ->
    gen_server:cast(?MODULE, {message, Message}).

init(_) ->
    {ok, #{}}.

handle_call(_Any, _From, State) ->
    {reply, ok, State}.

handle_cast({message, Message}, State) ->
    io:format(user, "dispatcher got message ~p~n", [Message]),
    case maps:get(?TYPE, Message, undefined) of
        <<"MESSAGE_CREATE">> ->
            io:format(user, "************ trying to react ~n", []),
            Data = maps:get(?DATA, Message, #{}),
            Author = maps:get(<<"author">>, Data),
            Username = maps:get(<<"username">>, Author),
            case Username of
                <<"earl_bot">> ->
                    ok;
                _ ->
                    ChannelId = maps:get(<<"channel_id">>, Data),
                    MessageId = maps:get(<<"id">>, Data),
                    Emoji = list_to_binary(uri_string:quote("👋")),
                    react(ChannelId, MessageId, Emoji)
            end;
        _ ->
            ok
    end,
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Stopping ~p~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

react(ChannelId, MessageId, Emoji) ->
    {Method, Message} = erdi_rest:react_add(ChannelId, MessageId, Emoji),
    erdi_http:send(Method, Message),
    {M2, Path2, Content} = erdi_rest:reply(ChannelId, MessageId, "You're a genius!"),
    erdi_http:send(M2, Path2, Content),
    timer:sleep(3000),
    erdi_http:send(delete, Message).
