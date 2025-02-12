-module(erdi_message_replier).

-behaviour(gen_event).

-include("erdi.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

init(Args) ->
  {ok, Args}.

handle_event({?TYPE_MESSAGE_CREATE, Message, UserId}, State) ->
  io:format(user, "************ trying to react ~n", []),
  Data = maps:get(?DATA, Message, #{}),
  Author = maps:get(?AUTHOR, Data),
  AuthorId = maps:get(?ID, Author),
  case UserId of
    X when X == AuthorId ->
      ok;
    _ ->
      ChannelId = maps:get(?CHANNEL_ID, Data),
      MessageId = maps:get(?MESSAGE_ID, Data),
      reply(ChannelId, MessageId, <<"Wrong!">>)
  end,
  {ok, State};
handle_event(_, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, {reply, ok}, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

reply(ChannelId, MessageId, Content) ->
  {Method, Path, Message} = erdi_rest:reply(ChannelId, MessageId, Content),
  erdi_http:send(Method, Path, Message).
