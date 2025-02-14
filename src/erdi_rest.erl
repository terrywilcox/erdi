-module(erdi_rest).

-export([react_add/3, react_delete/3]).
-export([create_slash_command/3]).
-export([interaction_response/3]).
-export([reply/3, create_message/2, edit_message/3]).

react_add(ChannelId, MessageId, Emoji) ->
  Path = ["channels/", ChannelId, "/messages/", MessageId, "/reactions/", Emoji, "/@me"],
  {put, Path}.

react_delete(ChannelId, MessageId, Emoji) ->
  Path = ["channels/", ChannelId, "/messages/", MessageId, "/reactions/", Emoji, "/@me"],
  {delete, Path}.

reply(ChannelId, MessageId, Message) ->
  Path = ["channels/", ChannelId, "/messages"],
  Body =
    #{<<"content">> => iolist_to_binary(Message),
      <<"message_reference">> => #{<<"message_id">> => iolist_to_binary(MessageId)}},
  {post, Path, Body}.

create_message(ChannelId, Message) when is_binary(Message) ->
  Path = ["channels/", ChannelId, "/messages"],
  Body = #{<<"content">> => iolist_to_binary(Message)},
  {post, Path, Body};
create_message(ChannelId, #{} = Body) ->
  Path = ["channels/", ChannelId, "/messages"],
  {post, Path, Body}.

edit_message(ChannelId, MessageId, NewMessage) ->
  Path = ["channels/", ChannelId, "/messages/", MessageId],
  Body = #{<<"content">> => iolist_to_binary(NewMessage)},
  {patch, Path, Body}.

create_slash_command(ApplicationId, GuildId, Content) ->
  Path = ["applications/", ApplicationId, "/guilds/", GuildId, "/commands"],
  Json = iolist_to_binary(json:encode(Content)),
  {post, Path, Json}.

interaction_response(InteractionId, Token, Content) ->
  Path = ["interactions/", InteractionId, "/", Token, "/callback"],
  {post, iolist_to_binary(Path), Content}.
