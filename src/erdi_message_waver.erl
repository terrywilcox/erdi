-module(erdi_message_waver).

-behaviour(gen_event).

-include("erdi.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

init(_Args) ->
  {ok, #{}}.

handle_event({update, Options}, State) ->
  NewState = maps:merge(State, Options),
  {ok, NewState};
handle_event({?TYPE_MESSAGE_CREATE, Message, #{user_id := UserId} = _Options}, State) ->
  Data = maps:get(?DATA, Message, #{}),
  Author = maps:get(?AUTHOR, Data),
  AuthorId = maps:get(?ID, Author),
  case UserId of
    X when X == AuthorId ->
      ok;
    _ ->
      ChannelId = maps:get(?CHANNEL_ID, Data),
      MessageId = maps:get(?MESSAGE_ID, Data),
      Emoji = list_to_binary(uri_string:quote("👋")),
      react(ChannelId, MessageId, Emoji)
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

react(ChannelId, MessageId, Emoji) ->
  {Method, Message} = erdi_rest:react_add(ChannelId, MessageId, Emoji),
  erdi_http:send({Method, Message}).
