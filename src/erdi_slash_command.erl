-module(erdi_slash_command).

-behaviour(gen_event).

-define(SLASH_NAME, <<"honk">>).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

init(_Args) ->
  {ok, #{}}.

handle_event({update, #{application_id := ApplicationId, guilds := Guilds} = Options},
             State) ->
  [GuildId | _] = Guilds,
  Body = #{<<"name">> => ?SLASH_NAME, <<"description">> => <<"honk em">>},
  {P, Path, Thing} = erdi_rest:create_slash_command(ApplicationId, GuildId, Body),
  erdi_http:send({P, Path, Thing}),
  NewState = maps:merge(State, Options),

  {ok, NewState};
handle_event({<<"INTERACTION_CREATE">>,
              #{<<"d">> :=
                  #{<<"token">> := Token,
                    <<"type">> := 2,
                    <<"id">> := InteractionId,
                    <<"data">> := #{<<"name">> := ?SLASH_NAME}}} =
                _Message,
              _Options},
             State) ->
  Body = #{<<"type">> => 4, <<"data">> => #{<<"content">> => <<"honk em if you got em">>}},
  {P, Path, Thing} = erdi_rest:interaction_response(InteractionId, Token, Body),
  erdi_http:send({P, Path, Thing}),
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
