-module(erdi_discord_utils).

-include("erdi.hrl").

-export([message_info/1]).

message_info(#{?TYPE := <<"READY">>, ?DATA := DiscordData}) ->
  #{<<"resume_gateway_url">> := ResumeUrl,
    <<"session_id">> := SessionId,
    <<"application">> := #{<<"id">> := ApplicationId},
    <<"user">> := #{<<"id">> := UserId},
    <<"guilds">> := Guilds} =
    DiscordData,

  GuildIds =
    lists:foldl(fun(#{<<"id">> := GuildId}, Acc) -> [GuildId | Acc] end, [], Guilds),
  #{resume_gateway_url => ResumeUrl,
    session_id => SessionId,
    application_id => ApplicationId,
    user_id => UserId,
    guilds => GuildIds};
message_info(_) ->
  #{}.
