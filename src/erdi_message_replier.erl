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
  Data = maps:get(?DATA, Message, #{}),
  Author = maps:get(?AUTHOR, Data),
  AuthorId = maps:get(?ID, Author),
  case UserId of
    X when X == AuthorId ->
      ok;
    _ ->
      ChannelId1 = maps:get(?CHANNEL_ID, Data),
      Content =
        #{<<"content">> => <<"this is the message text!">>,
          <<"components">> =>
            [#{<<"type">> => 1,
               <<"components">> =>
                 [#{<<"type">> => 2,
                    <<"label">> => <<"I do nothing.">>,
                    <<"style">> => 1,
                    <<"custom_id">> => <<"do_nothing">>}]},
             #{<<"type">> => 1,
               <<"components">> =>
                 [#{<<"type">> => 3,
                    <<"custom_id">> => <<"useless">>,
                    <<"options">> =>
                      [#{<<"label">> => <<"Cow">>, <<"value">> => <<"cow">>},
                       #{<<"label">> => <<"Dog">>, <<"value">> => <<"dog">>}]}]}]},

      _Phil = json:encode(Content),
      _Bob = create_message(ChannelId1, Content)
  end,
  %#{?CHANNEL_ID := ChannelId, <<"id">> := MessageId} = Bob,
  %      timer:sleep(3000),
  %      edit_message(ChannelId, MessageId, <<"Ted!">>)
  {ok, State};
handle_event(_, State) ->
  {ok, State}.

      %      MessageId = maps:get(?MESSAGE_ID, Data),

handle_call(_Request, State) ->
  {ok, {reply, ok}, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

create_message(ChannelId, Content) ->
  {Method, Path, Message} = erdi_rest:create_message(ChannelId, Content),
  erdi_http:send_await({Method, Path, Message}).

%edit_message(ChannelId, MessageId, Content) ->
%  {Method, Path, Message} = erdi_rest:edit_message(ChannelId, MessageId, Content),
%  erdi_http:send({Method, Path, Message}).
