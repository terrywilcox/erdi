-module(erdi_message_replier).

-behaviour(gen_event).

-include("erdi.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

init(Args) ->
  {ok, Args}.

handle_event({update, Options}, State) ->
  NewState = maps:merge(State, Options),
  {ok, NewState};
handle_event({?TYPE_MESSAGE_CREATE, Message, #{user_id := UserId} = _Options}, State) ->
  io:format(user, "****message: ~p~n", [Message]),
  #{?DATA := #{?CHANNEL_ID := ChannelId}} = Message,
  AuthorId = erdi_maps:get([?DATA, ?AUTHOR, ?ID], Message),
  case UserId of
    X when X == AuthorId ->
      ok;
    _ ->
      MessageCreateRequest = #{content => <<"Yo, this does not use a record!">>},
      Phil = discord_rest:create_message(ChannelId, MessageCreateRequest),
      io:format(user, "***********thing: ~p~n", [Phil])
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

%edit_message(ChannelId, MessageId, Content) ->
%  {Method, Path, Message} = erdi_rest:edit_message(ChannelId, MessageId, Content),
%  erdi_http:send({Method, Path, Message}).
