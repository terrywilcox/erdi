-module(erdi_rest).

-export([react_add/3, react_delete/3]).
-export([reply/3]).

react_add(ChannelId, MessageId, Emoji) ->
    Path = ["channels/", ChannelId, "/messages/", MessageId, "/reactions/", Emoji, "/@me"],
    {put, iolist_to_binary(Path)}.

react_delete(ChannelId, MessageId, Emoji) ->
    Path = ["channels/", ChannelId, "/messages/", MessageId, "/reactions/", Emoji, "/@me"],
    {delete, iolist_to_binary(Path)}.

reply(ChannelId, MessageId, Content) ->
    Path = ["channels/", ChannelId, "/messages"],
    Body =
        #{
            <<"content">> => iolist_to_binary(Content),
            <<"message_reference">> => #{<<"message_id">> => iolist_to_binary(MessageId)}
        },
    Json = iolist_to_binary(json:encode(Body)),
    {post, iolist_to_binary(Path), Json}.
