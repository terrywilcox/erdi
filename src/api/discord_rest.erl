-module(discord_rest).

-include("discord_types.hrl").

-export([update_my_application/1, get_my_application/0]).
-export([update_application/2, get_application/1]).
-export([applications_get_activity_instance/2]).
-export([upload_application_attachment/1]).
-export([bulk_set_application_commands/1, create_application_command/2,
         list_application_commands/1]).
-export([update_application_command/3, get_application_command/2,
         delete_application_command/2]).
-export([create_application_emoji/2, list_application_emojis/1]).
-export([update_application_emoji/3, get_application_emoji/2,
         delete_application_emoji/2]).
-export([create_entitlement/2, get_entitlements/1]).
-export([get_entitlement/2, delete_entitlement/2]).
-export([consume_entitlement/2]).
-export([bulk_set_guild_application_commands/2, create_guild_application_command/3,
         list_guild_application_commands/2]).
-export([list_guild_application_command_permissions/2]).
-export([update_guild_application_command/4, get_guild_application_command/3,
         delete_guild_application_command/3]).
-export([set_guild_application_command_permissions/4,
         get_guild_application_command_permissions/3]).
-export([update_application_role_connections_metadata/1,
         get_application_role_connections_metadata/1]).
-export([update_channel/2, get_channel/1, delete_channel/1]).
-export([follow_channel/2]).
-export([create_channel_invite/2, list_channel_invites/1]).
-export([create_message/2, list_messages/1]).
-export([bulk_delete_messages/2]).
-export([update_message/3, get_message/2, delete_message/2]).
-export([crosspost_message/2]).
-export([delete_all_message_reactions/2]).
-export([list_message_reactions_by_emoji/3, delete_all_message_reactions_by_emoji/3]).
-export([add_my_message_reaction/3, delete_my_message_reaction/3]).
-export([delete_user_message_reaction/4]).
-export([create_thread_from_message/3]).
-export([set_channel_permission_overwrite/3, delete_channel_permission_overwrite/2]).
-export([list_pinned_messages/1]).
-export([pin_message/2, unpin_message/2]).
-export([get_answer_voters/3]).
-export([poll_expire/2]).
-export([add_group_dm_user/3, delete_group_dm_user/2]).
-export([send_soundboard_sound/2]).
-export([list_thread_members/1]).
-export([join_thread/1, leave_thread/1]).
-export([add_thread_member/2, get_thread_member/2, delete_thread_member/2]).
-export([create_thread/2]).
-export([list_private_archived_threads/1]).
-export([list_public_archived_threads/1]).
-export([trigger_typing_indicator/1]).
-export([list_my_private_archived_threads/1]).
-export([create_webhook/2, list_channel_webhooks/1]).
-export([get_gateway/0]).
-export([get_bot_gateway/0]).
-export([create_guild/1]).
-export([create_guild_from_template/2, get_guild_template/1]).
-export([update_guild/2, get_guild/1, delete_guild/1]).
-export([list_guild_audit_log_entries/1]).
-export([create_auto_moderation_rule/1, list_auto_moderation_rules/1]).
-export([update_auto_moderation_rule/3, get_auto_moderation_rule/2,
         delete_auto_moderation_rule/2]).
-export([list_guild_bans/1]).
-export([ban_user_from_guild/3, get_guild_ban/2, unban_user_from_guild/2]).
-export([bulk_ban_users_from_guild/2]).
-export([create_guild_channel/2, bulk_update_guild_channels/2, list_guild_channels/1]).
-export([create_guild_emoji/2, list_guild_emojis/1]).
-export([update_guild_emoji/3, get_guild_emoji/2, delete_guild_emoji/2]).
-export([list_guild_integrations/1]).
-export([delete_guild_integration/2]).
-export([list_guild_invites/1]).
-export([list_guild_members/1]).
-export([update_my_guild_member/2]).
-export([search_guild_members/1]).
-export([add_guild_member/3, update_guild_member/3, get_guild_member/2,
         delete_guild_member/2]).
-export([add_guild_member_role/3, delete_guild_member_role/3]).
-export([set_guild_mfa_level/2]).
-export([get_guild_new_member_welcome/1]).
-export([put_guilds_onboarding/2, get_guilds_onboarding/1]).
-export([get_guild_preview/1]).
-export([prune_guild/2, preview_prune_guild/1]).
-export([list_guild_voice_regions/1]).
-export([create_guild_role/2, bulk_update_guild_roles/2, list_guild_roles/1]).
-export([update_guild_role/3, get_guild_role/2, delete_guild_role/2]).
-export([create_guild_scheduled_event/1, list_guild_scheduled_events/1]).
-export([update_guild_scheduled_event/3, get_guild_scheduled_event/2,
         delete_guild_scheduled_event/2]).
-export([list_guild_scheduled_event_users/2]).
-export([create_guild_soundboard_sound/2, list_guild_soundboard_sounds/1]).
-export([update_guild_soundboard_sound/3, get_guild_soundboard_sound/2,
         delete_guild_soundboard_sound/2]).
-export([create_guild_sticker/1, list_guild_stickers/1]).
-export([update_guild_sticker/3, get_guild_sticker/2, delete_guild_sticker/2]).
-export([create_guild_template/2, list_guild_templates/1]).
-export([sync_guild_template/2, update_guild_template/3, delete_guild_template/2]).
-export([get_active_guild_threads/1]).
-export([get_guild_vanity_url/1]).
-export([update_self_voice_state/2, get_self_voice_state/1]).
-export([update_voice_state/3, get_voice_state/2]).
-export([get_guild_webhooks/1]).
-export([update_guild_welcome_screen/2, get_guild_welcome_screen/1]).
-export([update_guild_widget_settings/2, get_guild_widget_settings/1]).
-export([get_guild_widget/1]).
-export([get_guild_widget_png/1]).
-export([create_interaction_response/3]).
-export([invite_resolve/1, invite_revoke/1]).
-export([get_my_oauth2_authorization/0]).
-export([get_my_oauth2_application/0]).
-export([get_public_keys/0]).
-export([get_openid_connect_userinfo/0]).
-export([get_soundboard_default_sounds/0]).
-export([create_stage_instance/1]).
-export([update_stage_instance/2, get_stage_instance/1, delete_stage_instance/1]).
-export([list_sticker_packs/0]).
-export([get_sticker_pack/1]).
-export([get_sticker/1]).
-export([update_my_user/1, get_my_user/0]).
-export([update_application_user_role_connection/2,
         get_application_user_role_connection/1, delete_application_user_role_connection/1]).
-export([create_dm/1]).
-export([list_my_connections/0]).
-export([list_my_guilds/0]).
-export([leave_guild/1]).
-export([get_my_guild_member/1]).
-export([get_user/1]).
-export([list_voice_regions/0]).
-export([update_webhook/2, get_webhook/1, delete_webhook/1]).
-export([execute_webhook/3, update_webhook_by_token/3, get_webhook_by_token/2,
         delete_webhook_by_token/2]).
-export([execute_github_compatible_webhook/3]).
-export([update_original_webhook_message/3, get_original_webhook_message/2,
         delete_original_webhook_message/2]).
-export([update_webhook_message/4, get_webhook_message/3, delete_webhook_message/3]).
-export([execute_slack_compatible_webhook/3]).

-spec update_my_application(Body :: application_form_partial()) ->
                             {ok, 200, private_application_response()} |
                             {error, integer(), client_error_response()}.
update_my_application(Body) ->
  Path = iolist_to_binary(["/applications/@me"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_my_application() ->
                          {ok, 200, private_application_response()} |
                          {error, integer(), client_error_response()}.
get_my_application() ->
  Path = iolist_to_binary(["/applications/@me"]),

  http_send(get, Path).

-spec update_application(ApplicationId :: snowflake_type(),
                         Body :: application_form_partial()) ->
                          {ok, 200, private_application_response()} |
                          {error, integer(), client_error_response()}.
update_application(ApplicationId, Body) ->
  Path = iolist_to_binary(["/applications/", ApplicationId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_application(ApplicationId :: snowflake_type()) ->
                       {ok, 200, private_application_response()} |
                       {error, integer(), client_error_response()}.
get_application(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId]),

  http_send(get, Path).

-spec applications_get_activity_instance(ApplicationId :: snowflake_type(),
                                         InstanceId :: string()) ->
                                          {ok, 200, embedded_activity_instance()} |
                                          {error, integer(), client_error_response()}.
applications_get_activity_instance(ApplicationId, InstanceId) ->
  Path =
    iolist_to_binary(["/applications/", ApplicationId, "/activity-instances/", InstanceId]),

  http_send(get, Path).

-spec upload_application_attachment(ApplicationId :: snowflake_type()) ->
                                     {ok, 200, activities_attachment_response()} |
                                     {error, integer(), client_error_response()}.
upload_application_attachment(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/attachment"]),

  http_send(post, Path).

-spec bulk_set_application_commands(ApplicationId :: snowflake_type()) ->
                                     {ok, 200, [application_command_response()]} |
                                     {error, integer(), client_error_response()}.
bulk_set_application_commands(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/commands"]),

  http_send(put, Path).

-spec create_application_command(ApplicationId :: snowflake_type(),
                                 Body :: application_command_create_request()) ->
                                  {ok, 200, application_command_response()} |
                                  {ok, 201, application_command_response()} |
                                  {error, integer(), client_error_response()}.
create_application_command(ApplicationId, Body) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/commands"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_application_commands(ApplicationId :: snowflake_type()) ->
                                 {ok, 200, [application_command_response()]} |
                                 {error, integer(), client_error_response()}.
list_application_commands(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/commands"]),

  http_send(get, Path).

-spec update_application_command(ApplicationId :: snowflake_type(),
                                 CommandId :: snowflake_type(),
                                 Body :: application_command_patch_request_partial()) ->
                                  {ok, 200, application_command_response()} |
                                  {error, integer(), client_error_response()}.
update_application_command(ApplicationId, CommandId, Body) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/commands/", CommandId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_application_command(ApplicationId :: snowflake_type(),
                              CommandId :: snowflake_type()) ->
                               {ok, 200, application_command_response()} |
                               {error, integer(), client_error_response()}.
get_application_command(ApplicationId, CommandId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/commands/", CommandId]),

  http_send(get, Path).

-spec delete_application_command(ApplicationId :: snowflake_type(),
                                 CommandId :: snowflake_type()) ->
                                  {ok, 204} | {error, integer(), client_error_response()}.
delete_application_command(ApplicationId, CommandId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/commands/", CommandId]),

  http_send(delete, Path).

-spec create_application_emoji(ApplicationId :: snowflake_type(), Body :: map()) ->
                                {ok, 201, emoji_response()} |
                                {error, integer(), client_error_response()}.
create_application_emoji(ApplicationId, Body) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/emojis"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_application_emojis(ApplicationId :: snowflake_type()) ->
                               {ok, 200, list_application_emojis_response()} |
                               {error, integer(), client_error_response()}.
list_application_emojis(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/emojis"]),

  http_send(get, Path).

-spec update_application_emoji(ApplicationId :: snowflake_type(),
                               EmojiId :: snowflake_type(),
                               Body :: map()) ->
                                {ok, 200, emoji_response()} |
                                {error, integer(), client_error_response()}.
update_application_emoji(ApplicationId, EmojiId, Body) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/emojis/", EmojiId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_application_emoji(ApplicationId :: snowflake_type(),
                            EmojiId :: snowflake_type()) ->
                             {ok, 200, emoji_response()} |
                             {error, integer(), client_error_response()}.
get_application_emoji(ApplicationId, EmojiId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/emojis/", EmojiId]),

  http_send(get, Path).

-spec delete_application_emoji(ApplicationId :: snowflake_type(),
                               EmojiId :: snowflake_type()) ->
                                {ok, 204} | {error, integer(), client_error_response()}.
delete_application_emoji(ApplicationId, EmojiId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/emojis/", EmojiId]),

  http_send(delete, Path).

-spec create_entitlement(ApplicationId :: snowflake_type(),
                         Body :: create_entitlement_request_data()) ->
                          {ok, 200, entitlement_response()} |
                          {error, integer(), client_error_response()}.
create_entitlement(ApplicationId, Body) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/entitlements"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec get_entitlements(ApplicationId :: snowflake_type()) ->
                        {ok, 200, [entitlement_response()]} |
                        {error, integer(), client_error_response()}.
get_entitlements(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/entitlements"]),

  http_send(get, Path).

-spec get_entitlement(ApplicationId :: snowflake_type(),
                      EntitlementId :: snowflake_type()) ->
                       {ok, 200, entitlement_response()} |
                       {error, integer(), client_error_response()}.
get_entitlement(ApplicationId, EntitlementId) ->
  Path =
    iolist_to_binary(["/applications/", ApplicationId, "/entitlements/", EntitlementId]),

  http_send(get, Path).

-spec delete_entitlement(ApplicationId :: snowflake_type(),
                         EntitlementId :: snowflake_type()) ->
                          {ok, 204} | {error, integer(), client_error_response()}.
delete_entitlement(ApplicationId, EntitlementId) ->
  Path =
    iolist_to_binary(["/applications/", ApplicationId, "/entitlements/", EntitlementId]),

  http_send(delete, Path).

-spec consume_entitlement(ApplicationId :: snowflake_type(),
                          EntitlementId :: snowflake_type()) ->
                           {ok, 204} | {error, integer(), client_error_response()}.
consume_entitlement(ApplicationId, EntitlementId) ->
  Path =
    iolist_to_binary(["/applications/",
                      ApplicationId,
                      "/entitlements/",
                      EntitlementId,
                      "/consume"]),

  http_send(post, Path).

-spec bulk_set_guild_application_commands(ApplicationId :: snowflake_type(),
                                          GuildId :: snowflake_type()) ->
                                           {ok, 200, [application_command_response()]} |
                                           {error, integer(), client_error_response()}.
bulk_set_guild_application_commands(ApplicationId, GuildId) ->
  Path =
    iolist_to_binary(["/applications/", ApplicationId, "/guilds/", GuildId, "/commands"]),

  http_send(put, Path).

-spec create_guild_application_command(ApplicationId :: snowflake_type(),
                                       GuildId :: snowflake_type(),
                                       Body :: application_command_create_request()) ->
                                        {ok, 200, application_command_response()} |
                                        {ok, 201, application_command_response()} |
                                        {error, integer(), client_error_response()}.
create_guild_application_command(ApplicationId, GuildId, Body) ->
  Path =
    iolist_to_binary(["/applications/", ApplicationId, "/guilds/", GuildId, "/commands"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_guild_application_commands(ApplicationId :: snowflake_type(),
                                      GuildId :: snowflake_type()) ->
                                       {ok, 200, [application_command_response()]} |
                                       {error, integer(), client_error_response()}.
list_guild_application_commands(ApplicationId, GuildId) ->
  Path =
    iolist_to_binary(["/applications/", ApplicationId, "/guilds/", GuildId, "/commands"]),

  http_send(get, Path).

-spec list_guild_application_command_permissions(ApplicationId :: snowflake_type(),
                                                 GuildId :: snowflake_type()) ->
                                                  {ok, 200, [command_permissions_response()]} |
                                                  {error, integer(), client_error_response()}.
list_guild_application_command_permissions(ApplicationId, GuildId) ->
  Path =
    iolist_to_binary(["/applications/",
                      ApplicationId,
                      "/guilds/",
                      GuildId,
                      "/commands/permissions"]),

  http_send(get, Path).

-spec update_guild_application_command(ApplicationId :: snowflake_type(),
                                       GuildId :: snowflake_type(),
                                       CommandId :: snowflake_type(),
                                       Body :: application_command_patch_request_partial()) ->
                                        {ok, 200, application_command_response()} |
                                        {error, integer(), client_error_response()}.
update_guild_application_command(ApplicationId, GuildId, CommandId, Body) ->
  Path =
    iolist_to_binary(["/applications/",
                      ApplicationId,
                      "/guilds/",
                      GuildId,
                      "/commands/",
                      CommandId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_application_command(ApplicationId :: snowflake_type(),
                                    GuildId :: snowflake_type(),
                                    CommandId :: snowflake_type()) ->
                                     {ok, 200, application_command_response()} |
                                     {error, integer(), client_error_response()}.
get_guild_application_command(ApplicationId, GuildId, CommandId) ->
  Path =
    iolist_to_binary(["/applications/",
                      ApplicationId,
                      "/guilds/",
                      GuildId,
                      "/commands/",
                      CommandId]),

  http_send(get, Path).

-spec delete_guild_application_command(ApplicationId :: snowflake_type(),
                                       GuildId :: snowflake_type(),
                                       CommandId :: snowflake_type()) ->
                                        {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_application_command(ApplicationId, GuildId, CommandId) ->
  Path =
    iolist_to_binary(["/applications/",
                      ApplicationId,
                      "/guilds/",
                      GuildId,
                      "/commands/",
                      CommandId]),

  http_send(delete, Path).

-spec set_guild_application_command_permissions(ApplicationId :: snowflake_type(),
                                                GuildId :: snowflake_type(),
                                                CommandId :: snowflake_type(),
                                                Body :: map()) ->
                                                 {ok, 200, command_permissions_response()} |
                                                 {error, integer(), client_error_response()}.
set_guild_application_command_permissions(ApplicationId, GuildId, CommandId, Body) ->
  Path =
    iolist_to_binary(["/applications/",
                      ApplicationId,
                      "/guilds/",
                      GuildId,
                      "/commands/",
                      CommandId,
                      "/permissions"]),
  BodyBin = body_to_binary(Body),
  http_send(put, Path, BodyBin).

-spec get_guild_application_command_permissions(ApplicationId :: snowflake_type(),
                                                GuildId :: snowflake_type(),
                                                CommandId :: snowflake_type()) ->
                                                 {ok, 200, command_permissions_response()} |
                                                 {error, integer(), client_error_response()}.
get_guild_application_command_permissions(ApplicationId, GuildId, CommandId) ->
  Path =
    iolist_to_binary(["/applications/",
                      ApplicationId,
                      "/guilds/",
                      GuildId,
                      "/commands/",
                      CommandId,
                      "/permissions"]),

  http_send(get, Path).

-spec update_application_role_connections_metadata(ApplicationId :: snowflake_type()) ->
                                                    {ok,
                                                     200,
                                                     [application_role_connections_metadata_item_response()]} |
                                                    {error, integer(), client_error_response()}.
update_application_role_connections_metadata(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/role-connections/metadata"]),

  http_send(put, Path).

-spec get_application_role_connections_metadata(ApplicationId :: snowflake_type()) ->
                                                 {ok,
                                                  200,
                                                  [application_role_connections_metadata_item_response()]} |
                                                 {error, integer(), client_error_response()}.
get_application_role_connections_metadata(ApplicationId) ->
  Path = iolist_to_binary(["/applications/", ApplicationId, "/role-connections/metadata"]),

  http_send(get, Path).

-spec update_channel(ChannelId :: snowflake_type(),
                     Body ::
                       private_channel_request_partial() |
                       update_guild_channel_request_partial() |
                       update_thread_request_partial()) ->
                      {ok,
                       200,
                       guild_channel_response() |
                       private_channel_response() |
                       private_group_channel_response() |
                       thread_response()} |
                      {error, integer(), client_error_response()}.
update_channel(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_channel(ChannelId :: snowflake_type()) ->
                   {ok,
                    200,
                    guild_channel_response() |
                    private_channel_response() |
                    private_group_channel_response() |
                    thread_response()} |
                   {error, integer(), client_error_response()}.
get_channel(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId]),

  http_send(get, Path).

-spec delete_channel(ChannelId :: snowflake_type()) ->
                      {ok,
                       200,
                       guild_channel_response() |
                       private_channel_response() |
                       private_group_channel_response() |
                       thread_response()} |
                      {error, integer(), client_error_response()}.
delete_channel(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId]),

  http_send(delete, Path).

-spec follow_channel(ChannelId :: snowflake_type(), Body :: map()) ->
                      {ok, 200, channel_follower_response()} |
                      {error, integer(), client_error_response()}.
follow_channel(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/followers"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec create_channel_invite(ChannelId :: snowflake_type(),
                            Body ::
                              create_group_dm_invite_request() | create_guild_invite_request()) ->
                             {ok,
                              200,
                              friend_invite_response() |
                              group_dm_invite_response() |
                              guild_invite_response()} |
                             {ok, 204} |
                             {error, integer(), client_error_response()}.
create_channel_invite(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/invites"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_channel_invites(ChannelId :: snowflake_type()) ->
                            {ok,
                             200,
                             [friend_invite_response() |
                              group_dm_invite_response() |
                              guild_invite_response()]} |
                            {error, integer(), client_error_response()}.
list_channel_invites(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/invites"]),

  http_send(get, Path).

-spec create_message(ChannelId :: snowflake_type(), Body :: message_create_request()) ->
                      {ok, 200, message_response()} | {error, integer(), client_error_response()}.
create_message(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_messages(ChannelId :: snowflake_type()) ->
                     {ok, 200, [message_response()]} | {error, integer(), client_error_response()}.
list_messages(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages"]),

  http_send(get, Path).

-spec bulk_delete_messages(ChannelId :: snowflake_type(), Body :: map()) ->
                            {ok, 204} | {error, integer(), client_error_response()}.
bulk_delete_messages(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages/bulk-delete"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec update_message(ChannelId :: snowflake_type(),
                     MessageId :: snowflake_type(),
                     Body :: message_edit_request_partial()) ->
                      {ok, 200, message_response()} | {error, integer(), client_error_response()}.
update_message(ChannelId, MessageId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages/", MessageId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_message(ChannelId :: snowflake_type(), MessageId :: snowflake_type()) ->
                   {ok, 200, message_response()} | {error, integer(), client_error_response()}.
get_message(ChannelId, MessageId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages/", MessageId]),

  http_send(get, Path).

-spec delete_message(ChannelId :: snowflake_type(), MessageId :: snowflake_type()) ->
                      {ok, 204} | {error, integer(), client_error_response()}.
delete_message(ChannelId, MessageId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages/", MessageId]),

  http_send(delete, Path).

-spec crosspost_message(ChannelId :: snowflake_type(), MessageId :: snowflake_type()) ->
                         {ok, 200, message_response()} |
                         {error, integer(), client_error_response()}.
crosspost_message(ChannelId, MessageId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages/", MessageId, "/crosspost"]),

  http_send(post, Path).

-spec delete_all_message_reactions(ChannelId :: snowflake_type(),
                                   MessageId :: snowflake_type()) ->
                                    {ok, 204} | {error, integer(), client_error_response()}.
delete_all_message_reactions(ChannelId, MessageId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages/", MessageId, "/reactions"]),

  http_send(delete, Path).

-spec list_message_reactions_by_emoji(ChannelId :: snowflake_type(),
                                      MessageId :: snowflake_type(),
                                      EmojiName :: string()) ->
                                       {ok, 200, [user_response()]} |
                                       {error, integer(), client_error_response()}.
list_message_reactions_by_emoji(ChannelId, MessageId, EmojiName) ->
  Path =
    iolist_to_binary(["/channels/",
                      ChannelId,
                      "/messages/",
                      MessageId,
                      "/reactions/",
                      EmojiName]),

  http_send(get, Path).

-spec delete_all_message_reactions_by_emoji(ChannelId :: snowflake_type(),
                                            MessageId :: snowflake_type(),
                                            EmojiName :: string()) ->
                                             {ok, 204} |
                                             {error, integer(), client_error_response()}.
delete_all_message_reactions_by_emoji(ChannelId, MessageId, EmojiName) ->
  Path =
    iolist_to_binary(["/channels/",
                      ChannelId,
                      "/messages/",
                      MessageId,
                      "/reactions/",
                      EmojiName]),

  http_send(delete, Path).

-spec add_my_message_reaction(ChannelId :: snowflake_type(),
                              MessageId :: snowflake_type(),
                              EmojiName :: string()) ->
                               {ok, 204} | {error, integer(), client_error_response()}.
add_my_message_reaction(ChannelId, MessageId, EmojiName) ->
  Path =
    iolist_to_binary(["/channels/",
                      ChannelId,
                      "/messages/",
                      MessageId,
                      "/reactions/",
                      EmojiName,
                      "/@me"]),

  http_send(put, Path).

-spec delete_my_message_reaction(ChannelId :: snowflake_type(),
                                 MessageId :: snowflake_type(),
                                 EmojiName :: string()) ->
                                  {ok, 204} | {error, integer(), client_error_response()}.
delete_my_message_reaction(ChannelId, MessageId, EmojiName) ->
  Path =
    iolist_to_binary(["/channels/",
                      ChannelId,
                      "/messages/",
                      MessageId,
                      "/reactions/",
                      EmojiName,
                      "/@me"]),

  http_send(delete, Path).

-spec delete_user_message_reaction(ChannelId :: snowflake_type(),
                                   MessageId :: snowflake_type(),
                                   EmojiName :: string(),
                                   UserId :: snowflake_type()) ->
                                    {ok, 204} | {error, integer(), client_error_response()}.
delete_user_message_reaction(ChannelId, MessageId, EmojiName, UserId) ->
  Path =
    iolist_to_binary(["/channels/",
                      ChannelId,
                      "/messages/",
                      MessageId,
                      "/reactions/",
                      EmojiName,
                      "/",
                      UserId]),

  http_send(delete, Path).

-spec create_thread_from_message(ChannelId :: snowflake_type(),
                                 MessageId :: snowflake_type(),
                                 Body :: create_text_thread_with_message_request()) ->
                                  {ok, 201, thread_response()} |
                                  {error, integer(), client_error_response()}.
create_thread_from_message(ChannelId, MessageId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/messages/", MessageId, "/threads"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec set_channel_permission_overwrite(ChannelId :: snowflake_type(),
                                       OverwriteId :: snowflake_type(),
                                       Body :: map()) ->
                                        {ok, 204} | {error, integer(), client_error_response()}.
set_channel_permission_overwrite(ChannelId, OverwriteId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/permissions/", OverwriteId]),
  BodyBin = body_to_binary(Body),
  http_send(put, Path, BodyBin).

-spec delete_channel_permission_overwrite(ChannelId :: snowflake_type(),
                                          OverwriteId :: snowflake_type()) ->
                                           {ok, 204} | {error, integer(), client_error_response()}.
delete_channel_permission_overwrite(ChannelId, OverwriteId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/permissions/", OverwriteId]),

  http_send(delete, Path).

-spec list_pinned_messages(ChannelId :: snowflake_type()) ->
                            {ok, 200, [message_response()]} |
                            {error, integer(), client_error_response()}.
list_pinned_messages(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/pins"]),

  http_send(get, Path).

-spec pin_message(ChannelId :: snowflake_type(), MessageId :: snowflake_type()) ->
                   {ok, 204} | {error, integer(), client_error_response()}.
pin_message(ChannelId, MessageId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/pins/", MessageId]),

  http_send(put, Path).

-spec unpin_message(ChannelId :: snowflake_type(), MessageId :: snowflake_type()) ->
                     {ok, 204} | {error, integer(), client_error_response()}.
unpin_message(ChannelId, MessageId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/pins/", MessageId]),

  http_send(delete, Path).

-spec get_answer_voters(ChannelId :: snowflake_type(),
                        MessageId :: snowflake_type(),
                        AnswerId :: integer()) ->
                         {ok, 200, poll_answer_details_response()} |
                         {error, integer(), client_error_response()}.
get_answer_voters(ChannelId, MessageId, AnswerId) ->
  Path =
    iolist_to_binary(["/channels/", ChannelId, "/polls/", MessageId, "/answers/", AnswerId]),

  http_send(get, Path).

-spec poll_expire(ChannelId :: snowflake_type(), MessageId :: snowflake_type()) ->
                   {ok, 200, message_response()} | {error, integer(), client_error_response()}.
poll_expire(ChannelId, MessageId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/polls/", MessageId, "/expire"]),

  http_send(post, Path).

-spec add_group_dm_user(ChannelId :: snowflake_type(),
                        UserId :: snowflake_type(),
                        Body :: map()) ->
                         {ok, 201, private_channel_response() | private_group_channel_response()} |
                         {ok, 204} |
                         {error, integer(), client_error_response()}.
add_group_dm_user(ChannelId, UserId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/recipients/", UserId]),
  BodyBin = body_to_binary(Body),
  http_send(put, Path, BodyBin).

-spec delete_group_dm_user(ChannelId :: snowflake_type(), UserId :: snowflake_type()) ->
                            {ok, 204} | {error, integer(), client_error_response()}.
delete_group_dm_user(ChannelId, UserId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/recipients/", UserId]),

  http_send(delete, Path).

-spec send_soundboard_sound(ChannelId :: snowflake_type(),
                            Body :: soundboard_sound_send_request()) ->
                             {ok, 204} | {error, integer(), client_error_response()}.
send_soundboard_sound(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/send-soundboard-sound"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_thread_members(ChannelId :: snowflake_type()) ->
                           {ok, 200, [thread_member_response()]} |
                           {error, integer(), client_error_response()}.
list_thread_members(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/thread-members"]),

  http_send(get, Path).

-spec join_thread(ChannelId :: snowflake_type()) ->
                   {ok, 204} | {error, integer(), client_error_response()}.
join_thread(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/thread-members/@me"]),

  http_send(put, Path).

-spec leave_thread(ChannelId :: snowflake_type()) ->
                    {ok, 204} | {error, integer(), client_error_response()}.
leave_thread(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/thread-members/@me"]),

  http_send(delete, Path).

-spec add_thread_member(ChannelId :: snowflake_type(), UserId :: snowflake_type()) ->
                         {ok, 204} | {error, integer(), client_error_response()}.
add_thread_member(ChannelId, UserId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/thread-members/", UserId]),

  http_send(put, Path).

-spec get_thread_member(ChannelId :: snowflake_type(), UserId :: snowflake_type()) ->
                         {ok, 200, thread_member_response()} |
                         {error, integer(), client_error_response()}.
get_thread_member(ChannelId, UserId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/thread-members/", UserId]),

  http_send(get, Path).

-spec delete_thread_member(ChannelId :: snowflake_type(), UserId :: snowflake_type()) ->
                            {ok, 204} | {error, integer(), client_error_response()}.
delete_thread_member(ChannelId, UserId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/thread-members/", UserId]),

  http_send(delete, Path).

-spec create_thread(ChannelId :: snowflake_type(),
                    Body ::
                      create_forum_thread_request() |
                      create_text_thread_without_message_request()) ->
                     {ok, 201, created_thread_response()} |
                     {error, integer(), client_error_response()}.
create_thread(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/threads"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_private_archived_threads(ChannelId :: snowflake_type()) ->
                                     {ok, 200, threads_response()} |
                                     {error, integer(), client_error_response()}.
list_private_archived_threads(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/threads/archived/private"]),

  http_send(get, Path).

-spec list_public_archived_threads(ChannelId :: snowflake_type()) ->
                                    {ok, 200, threads_response()} |
                                    {error, integer(), client_error_response()}.
list_public_archived_threads(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/threads/archived/public"]),

  http_send(get, Path).

-spec trigger_typing_indicator(ChannelId :: snowflake_type()) ->
                                {ok, 200, typing_indicator_response()} |
                                {ok, 204} |
                                {error, integer(), client_error_response()}.
trigger_typing_indicator(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/typing"]),

  http_send(post, Path).

-spec list_my_private_archived_threads(ChannelId :: snowflake_type()) ->
                                        {ok, 200, threads_response()} |
                                        {error, integer(), client_error_response()}.
list_my_private_archived_threads(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/users/@me/threads/archived/private"]),

  http_send(get, Path).

-spec create_webhook(ChannelId :: snowflake_type(), Body :: map()) ->
                      {ok, 200, guild_incoming_webhook_response()} |
                      {error, integer(), client_error_response()}.
create_webhook(ChannelId, Body) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/webhooks"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_channel_webhooks(ChannelId :: snowflake_type()) ->
                             {ok,
                              200,
                              [application_incoming_webhook_response() |
                               channel_follower_webhook_response() |
                               guild_incoming_webhook_response()]} |
                             {error, integer(), client_error_response()}.
list_channel_webhooks(ChannelId) ->
  Path = iolist_to_binary(["/channels/", ChannelId, "/webhooks"]),

  http_send(get, Path).

-spec get_gateway() ->
                   {ok, 200, gateway_response()} | {error, integer(), client_error_response()}.
get_gateway() ->
  Path = iolist_to_binary(["/gateway"]),

  http_send(get, Path).

-spec get_bot_gateway() ->
                       {ok, 200, gateway_bot_response()} |
                       {error, integer(), client_error_response()}.
get_bot_gateway() ->
  Path = iolist_to_binary(["/gateway/bot"]),

  http_send(get, Path).

-spec create_guild(Body :: guild_create_request()) ->
                    {ok, 201, guild_response()} | {error, integer(), client_error_response()}.
create_guild(Body) ->
  Path = iolist_to_binary(["/guilds"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec create_guild_from_template(Code :: string(), Body :: map()) ->
                                  {ok, 201, guild_response()} |
                                  {error, integer(), client_error_response()}.
create_guild_from_template(Code, Body) ->
  Path = iolist_to_binary(["/guilds/templates/", Code]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec get_guild_template(Code :: string()) ->
                          {ok, 200, guild_template_response()} |
                          {error, integer(), client_error_response()}.
get_guild_template(Code) ->
  Path = iolist_to_binary(["/guilds/templates/", Code]),

  http_send(get, Path).

-spec update_guild(GuildId :: snowflake_type(), Body :: guild_patch_request_partial()) ->
                    {ok, 200, guild_response()} | {error, integer(), client_error_response()}.
update_guild(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild(GuildId :: snowflake_type()) ->
                 {ok, 200, guild_with_counts_response()} |
                 {error, integer(), client_error_response()}.
get_guild(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId]),

  http_send(get, Path).

-spec delete_guild(GuildId :: snowflake_type()) ->
                    {ok, 204} | {error, integer(), client_error_response()}.
delete_guild(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId]),

  http_send(delete, Path).

-spec list_guild_audit_log_entries(GuildId :: snowflake_type()) ->
                                    {ok, 200, guild_audit_log_response()} |
                                    {error, integer(), client_error_response()}.
list_guild_audit_log_entries(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/audit-logs"]),

  http_send(get, Path).

-spec create_auto_moderation_rule(GuildId :: snowflake_type()) ->
                                   {ok,
                                    200,
                                    default_keyword_rule_response() |
                                    keyword_rule_response() |
                                    ml_spam_rule_response() |
                                    mention_spam_rule_response() |
                                    spam_link_rule_response()} |
                                   {error, integer(), client_error_response()}.
create_auto_moderation_rule(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/auto-moderation/rules"]),

  http_send(post, Path).

-spec list_auto_moderation_rules(GuildId :: snowflake_type()) ->
                                  {ok,
                                   200,
                                   [default_keyword_rule_response() |
                                    keyword_rule_response() |
                                    ml_spam_rule_response() |
                                    mention_spam_rule_response() |
                                    spam_link_rule_response()]} |
                                  {error, integer(), client_error_response()}.
list_auto_moderation_rules(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/auto-moderation/rules"]),

  http_send(get, Path).

-spec update_auto_moderation_rule(GuildId :: snowflake_type(),
                                  RuleId :: snowflake_type(),
                                  Body ::
                                    default_keyword_list_upsert_request_partial() |
                                    keyword_upsert_request_partial() |
                                    ml_spam_upsert_request_partial() |
                                    mention_spam_upsert_request_partial()) ->
                                   {ok,
                                    200,
                                    default_keyword_rule_response() |
                                    keyword_rule_response() |
                                    ml_spam_rule_response() |
                                    mention_spam_rule_response() |
                                    spam_link_rule_response()} |
                                   {error, integer(), client_error_response()}.
update_auto_moderation_rule(GuildId, RuleId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/auto-moderation/rules/", RuleId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_auto_moderation_rule(GuildId :: snowflake_type(), RuleId :: snowflake_type()) ->
                                {ok,
                                 200,
                                 default_keyword_rule_response() |
                                 keyword_rule_response() |
                                 ml_spam_rule_response() |
                                 mention_spam_rule_response() |
                                 spam_link_rule_response()} |
                                {error, integer(), client_error_response()}.
get_auto_moderation_rule(GuildId, RuleId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/auto-moderation/rules/", RuleId]),

  http_send(get, Path).

-spec delete_auto_moderation_rule(GuildId :: snowflake_type(),
                                  RuleId :: snowflake_type()) ->
                                   {ok, 204} | {error, integer(), client_error_response()}.
delete_auto_moderation_rule(GuildId, RuleId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/auto-moderation/rules/", RuleId]),

  http_send(delete, Path).

-spec list_guild_bans(GuildId :: snowflake_type()) ->
                       {ok, 200, [guild_ban_response()]} |
                       {error, integer(), client_error_response()}.
list_guild_bans(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/bans"]),

  http_send(get, Path).

-spec ban_user_from_guild(GuildId :: snowflake_type(),
                          UserId :: snowflake_type(),
                          Body :: map()) ->
                           {ok, 204} | {error, integer(), client_error_response()}.
ban_user_from_guild(GuildId, UserId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/bans/", UserId]),
  BodyBin = body_to_binary(Body),
  http_send(put, Path, BodyBin).

-spec get_guild_ban(GuildId :: snowflake_type(), UserId :: snowflake_type()) ->
                     {ok, 200, guild_ban_response()} | {error, integer(), client_error_response()}.
get_guild_ban(GuildId, UserId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/bans/", UserId]),

  http_send(get, Path).

-spec unban_user_from_guild(GuildId :: snowflake_type(), UserId :: snowflake_type()) ->
                             {ok, 204} | {error, integer(), client_error_response()}.
unban_user_from_guild(GuildId, UserId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/bans/", UserId]),

  http_send(delete, Path).

-spec bulk_ban_users_from_guild(GuildId :: snowflake_type(), Body :: map()) ->
                                 {ok, 200, bulk_ban_users_response()} |
                                 {error, integer(), client_error_response()}.
bulk_ban_users_from_guild(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/bulk-ban"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec create_guild_channel(GuildId :: snowflake_type(),
                           Body :: create_guild_channel_request()) ->
                            {ok, 201, guild_channel_response()} |
                            {error, integer(), client_error_response()}.
create_guild_channel(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/channels"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec bulk_update_guild_channels(GuildId :: snowflake_type(), Body :: list()) ->
                                  {ok, 204} | {error, integer(), client_error_response()}.
bulk_update_guild_channels(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/channels"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec list_guild_channels(GuildId :: snowflake_type()) ->
                           {ok,
                            200,
                            [guild_channel_response() |
                             private_channel_response() |
                             private_group_channel_response() |
                             thread_response()]} |
                           {error, integer(), client_error_response()}.
list_guild_channels(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/channels"]),

  http_send(get, Path).

-spec create_guild_emoji(GuildId :: snowflake_type(), Body :: map()) ->
                          {ok, 201, emoji_response()} | {error, integer(), client_error_response()}.
create_guild_emoji(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/emojis"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_guild_emojis(GuildId :: snowflake_type()) ->
                         {ok, 200, [emoji_response()]} |
                         {error, integer(), client_error_response()}.
list_guild_emojis(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/emojis"]),

  http_send(get, Path).

-spec update_guild_emoji(GuildId :: snowflake_type(),
                         EmojiId :: snowflake_type(),
                         Body :: map()) ->
                          {ok, 200, emoji_response()} | {error, integer(), client_error_response()}.
update_guild_emoji(GuildId, EmojiId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/emojis/", EmojiId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_emoji(GuildId :: snowflake_type(), EmojiId :: snowflake_type()) ->
                       {ok, 200, emoji_response()} | {error, integer(), client_error_response()}.
get_guild_emoji(GuildId, EmojiId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/emojis/", EmojiId]),

  http_send(get, Path).

-spec delete_guild_emoji(GuildId :: snowflake_type(), EmojiId :: snowflake_type()) ->
                          {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_emoji(GuildId, EmojiId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/emojis/", EmojiId]),

  http_send(delete, Path).

-spec list_guild_integrations(GuildId :: snowflake_type()) ->
                               {ok,
                                200,
                                [discord_integration_response() |
                                 external_connection_integration_response() |
                                 guild_subscription_integration_response()]} |
                               {error, integer(), client_error_response()}.
list_guild_integrations(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/integrations"]),

  http_send(get, Path).

-spec delete_guild_integration(GuildId :: snowflake_type(),
                               IntegrationId :: snowflake_type()) ->
                                {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_integration(GuildId, IntegrationId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/integrations/", IntegrationId]),

  http_send(delete, Path).

-spec list_guild_invites(GuildId :: snowflake_type()) ->
                          {ok,
                           200,
                           [friend_invite_response() |
                            group_dm_invite_response() |
                            guild_invite_response()]} |
                          {error, integer(), client_error_response()}.
list_guild_invites(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/invites"]),

  http_send(get, Path).

-spec list_guild_members(GuildId :: snowflake_type()) ->
                          {ok, 200, [guild_member_response()]} |
                          {error, integer(), client_error_response()}.
list_guild_members(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members"]),

  http_send(get, Path).

-spec update_my_guild_member(GuildId :: snowflake_type(), Body :: map()) ->
                              {ok, 200, private_guild_member_response()} |
                              {error, integer(), client_error_response()}.
update_my_guild_member(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/@me"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec search_guild_members(GuildId :: snowflake_type()) ->
                            {ok, 200, [guild_member_response()]} |
                            {error, integer(), client_error_response()}.
search_guild_members(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/search"]),

  http_send(get, Path).

-spec add_guild_member(GuildId :: snowflake_type(),
                       UserId :: snowflake_type(),
                       Body :: map()) ->
                        {ok, 201, guild_member_response()} |
                        {ok, 204} |
                        {error, integer(), client_error_response()}.
add_guild_member(GuildId, UserId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/", UserId]),
  BodyBin = body_to_binary(Body),
  http_send(put, Path, BodyBin).

-spec update_guild_member(GuildId :: snowflake_type(),
                          UserId :: snowflake_type(),
                          Body :: map()) ->
                           {ok, 200, guild_member_response()} |
                           {ok, 204} |
                           {error, integer(), client_error_response()}.
update_guild_member(GuildId, UserId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/", UserId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_member(GuildId :: snowflake_type(), UserId :: snowflake_type()) ->
                        {ok, 200, guild_member_response()} |
                        {error, integer(), client_error_response()}.
get_guild_member(GuildId, UserId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/", UserId]),

  http_send(get, Path).

-spec delete_guild_member(GuildId :: snowflake_type(), UserId :: snowflake_type()) ->
                           {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_member(GuildId, UserId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/", UserId]),

  http_send(delete, Path).

-spec add_guild_member_role(GuildId :: snowflake_type(),
                            UserId :: snowflake_type(),
                            RoleId :: snowflake_type()) ->
                             {ok, 204} | {error, integer(), client_error_response()}.
add_guild_member_role(GuildId, UserId, RoleId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/", UserId, "/roles/", RoleId]),

  http_send(put, Path).

-spec delete_guild_member_role(GuildId :: snowflake_type(),
                               UserId :: snowflake_type(),
                               RoleId :: snowflake_type()) ->
                                {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_member_role(GuildId, UserId, RoleId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/members/", UserId, "/roles/", RoleId]),

  http_send(delete, Path).

-spec set_guild_mfa_level(GuildId :: snowflake_type(), Body :: map()) ->
                           {ok, 200, guild_m_f_a_level_response()} |
                           {error, integer(), client_error_response()}.
set_guild_mfa_level(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/mfa"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec get_guild_new_member_welcome(GuildId :: snowflake_type()) ->
                                    {ok, 200, guild_home_settings_response()} |
                                    {ok, 204} |
                                    {error, integer(), client_error_response()}.
get_guild_new_member_welcome(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/new-member-welcome"]),

  http_send(get, Path).

-spec put_guilds_onboarding(GuildId :: snowflake_type(),
                            Body :: update_guild_onboarding_request()) ->
                             {ok, 200, guild_onboarding_response()} |
                             {error, integer(), client_error_response()}.
put_guilds_onboarding(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/onboarding"]),
  BodyBin = body_to_binary(Body),
  http_send(put, Path, BodyBin).

-spec get_guilds_onboarding(GuildId :: snowflake_type()) ->
                             {ok, 200, user_guild_onboarding_response()} |
                             {error, integer(), client_error_response()}.
get_guilds_onboarding(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/onboarding"]),

  http_send(get, Path).

-spec get_guild_preview(GuildId :: snowflake_type()) ->
                         {ok, 200, guild_preview_response()} |
                         {error, integer(), client_error_response()}.
get_guild_preview(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/preview"]),

  http_send(get, Path).

-spec prune_guild(GuildId :: snowflake_type(), Body :: map()) ->
                   {ok, 200, guild_prune_response()} | {error, integer(), client_error_response()}.
prune_guild(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/prune"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec preview_prune_guild(GuildId :: snowflake_type()) ->
                           {ok, 200, guild_prune_response()} |
                           {error, integer(), client_error_response()}.
preview_prune_guild(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/prune"]),

  http_send(get, Path).

-spec list_guild_voice_regions(GuildId :: snowflake_type()) ->
                                {ok, 200, [voice_region_response()]} |
                                {error, integer(), client_error_response()}.
list_guild_voice_regions(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/regions"]),

  http_send(get, Path).

-spec create_guild_role(GuildId :: snowflake_type(), Body :: map()) ->
                         {ok, 200, guild_role_response()} |
                         {error, integer(), client_error_response()}.
create_guild_role(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/roles"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec bulk_update_guild_roles(GuildId :: snowflake_type(), Body :: list()) ->
                               {ok, 200, [guild_role_response()]} |
                               {error, integer(), client_error_response()}.
bulk_update_guild_roles(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/roles"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec list_guild_roles(GuildId :: snowflake_type()) ->
                        {ok, 200, [guild_role_response()]} |
                        {error, integer(), client_error_response()}.
list_guild_roles(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/roles"]),

  http_send(get, Path).

-spec update_guild_role(GuildId :: snowflake_type(),
                        RoleId :: snowflake_type(),
                        Body :: map()) ->
                         {ok, 200, guild_role_response()} |
                         {error, integer(), client_error_response()}.
update_guild_role(GuildId, RoleId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/roles/", RoleId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_role(GuildId :: snowflake_type(), RoleId :: snowflake_type()) ->
                      {ok, 200, guild_role_response()} |
                      {error, integer(), client_error_response()}.
get_guild_role(GuildId, RoleId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/roles/", RoleId]),

  http_send(get, Path).

-spec delete_guild_role(GuildId :: snowflake_type(), RoleId :: snowflake_type()) ->
                         {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_role(GuildId, RoleId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/roles/", RoleId]),

  http_send(delete, Path).

-spec create_guild_scheduled_event(GuildId :: snowflake_type()) ->
                                    {ok,
                                     200,
                                     external_scheduled_event_response() |
                                     stage_scheduled_event_response() |
                                     voice_scheduled_event_response()} |
                                    {error, integer(), client_error_response()}.
create_guild_scheduled_event(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/scheduled-events"]),

  http_send(post, Path).

-spec list_guild_scheduled_events(GuildId :: snowflake_type()) ->
                                   {ok,
                                    200,
                                    [external_scheduled_event_response() |
                                     stage_scheduled_event_response() |
                                     voice_scheduled_event_response()]} |
                                   {error, integer(), client_error_response()}.
list_guild_scheduled_events(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/scheduled-events"]),

  http_send(get, Path).

-spec update_guild_scheduled_event(GuildId :: snowflake_type(),
                                   GuildScheduledEventId :: snowflake_type(),
                                   Body ::
                                     external_scheduled_event_patch_request_partial() |
                                     stage_scheduled_event_patch_request_partial() |
                                     voice_scheduled_event_patch_request_partial()) ->
                                    {ok,
                                     200,
                                     external_scheduled_event_response() |
                                     stage_scheduled_event_response() |
                                     voice_scheduled_event_response()} |
                                    {error, integer(), client_error_response()}.
update_guild_scheduled_event(GuildId, GuildScheduledEventId, Body) ->
  Path =
    iolist_to_binary(["/guilds/", GuildId, "/scheduled-events/", GuildScheduledEventId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_scheduled_event(GuildId :: snowflake_type(),
                                GuildScheduledEventId :: snowflake_type()) ->
                                 {ok,
                                  200,
                                  external_scheduled_event_response() |
                                  stage_scheduled_event_response() |
                                  voice_scheduled_event_response()} |
                                 {error, integer(), client_error_response()}.
get_guild_scheduled_event(GuildId, GuildScheduledEventId) ->
  Path =
    iolist_to_binary(["/guilds/", GuildId, "/scheduled-events/", GuildScheduledEventId]),

  http_send(get, Path).

-spec delete_guild_scheduled_event(GuildId :: snowflake_type(),
                                   GuildScheduledEventId :: snowflake_type()) ->
                                    {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_scheduled_event(GuildId, GuildScheduledEventId) ->
  Path =
    iolist_to_binary(["/guilds/", GuildId, "/scheduled-events/", GuildScheduledEventId]),

  http_send(delete, Path).

-spec list_guild_scheduled_event_users(GuildId :: snowflake_type(),
                                       GuildScheduledEventId :: snowflake_type()) ->
                                        {ok, 200, [scheduled_event_user_response()]} |
                                        {error, integer(), client_error_response()}.
list_guild_scheduled_event_users(GuildId, GuildScheduledEventId) ->
  Path =
    iolist_to_binary(["/guilds/",
                      GuildId,
                      "/scheduled-events/",
                      GuildScheduledEventId,
                      "/users"]),

  http_send(get, Path).

-spec create_guild_soundboard_sound(GuildId :: snowflake_type(),
                                    Body :: soundboard_create_request()) ->
                                     {ok, 201, soundboard_sound_response()} |
                                     {error, integer(), client_error_response()}.
create_guild_soundboard_sound(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/soundboard-sounds"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_guild_soundboard_sounds(GuildId :: snowflake_type()) ->
                                    {ok, 200, list_guild_soundboard_sounds_response()} |
                                    {error, integer(), client_error_response()}.
list_guild_soundboard_sounds(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/soundboard-sounds"]),

  http_send(get, Path).

-spec update_guild_soundboard_sound(GuildId :: snowflake_type(),
                                    SoundId :: snowflake_type(),
                                    Body :: soundboard_patch_request_partial()) ->
                                     {ok, 200, soundboard_sound_response()} |
                                     {error, integer(), client_error_response()}.
update_guild_soundboard_sound(GuildId, SoundId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/soundboard-sounds/", SoundId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_soundboard_sound(GuildId :: snowflake_type(),
                                 SoundId :: snowflake_type()) ->
                                  {ok, 200, soundboard_sound_response()} |
                                  {error, integer(), client_error_response()}.
get_guild_soundboard_sound(GuildId, SoundId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/soundboard-sounds/", SoundId]),

  http_send(get, Path).

-spec delete_guild_soundboard_sound(GuildId :: snowflake_type(),
                                    SoundId :: snowflake_type()) ->
                                     {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_soundboard_sound(GuildId, SoundId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/soundboard-sounds/", SoundId]),

  http_send(delete, Path).

-spec create_guild_sticker(GuildId :: snowflake_type()) ->
                            {ok, 201, guild_sticker_response()} |
                            {error, integer(), client_error_response()}.
create_guild_sticker(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/stickers"]),

  http_send(post, Path).

-spec list_guild_stickers(GuildId :: snowflake_type()) ->
                           {ok, 200, [guild_sticker_response()]} |
                           {error, integer(), client_error_response()}.
list_guild_stickers(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/stickers"]),

  http_send(get, Path).

-spec update_guild_sticker(GuildId :: snowflake_type(),
                           StickerId :: snowflake_type(),
                           Body :: map()) ->
                            {ok, 200, guild_sticker_response()} |
                            {error, integer(), client_error_response()}.
update_guild_sticker(GuildId, StickerId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/stickers/", StickerId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_sticker(GuildId :: snowflake_type(), StickerId :: snowflake_type()) ->
                         {ok, 200, guild_sticker_response()} |
                         {error, integer(), client_error_response()}.
get_guild_sticker(GuildId, StickerId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/stickers/", StickerId]),

  http_send(get, Path).

-spec delete_guild_sticker(GuildId :: snowflake_type(), StickerId :: snowflake_type()) ->
                            {ok, 204} | {error, integer(), client_error_response()}.
delete_guild_sticker(GuildId, StickerId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/stickers/", StickerId]),

  http_send(delete, Path).

-spec create_guild_template(GuildId :: snowflake_type(), Body :: map()) ->
                             {ok, 200, guild_template_response()} |
                             {error, integer(), client_error_response()}.
create_guild_template(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/templates"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_guild_templates(GuildId :: snowflake_type()) ->
                            {ok, 200, [guild_template_response()]} |
                            {error, integer(), client_error_response()}.
list_guild_templates(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/templates"]),

  http_send(get, Path).

-spec sync_guild_template(GuildId :: snowflake_type(), Code :: string()) ->
                           {ok, 200, guild_template_response()} |
                           {error, integer(), client_error_response()}.
sync_guild_template(GuildId, Code) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/templates/", Code]),

  http_send(put, Path).

-spec update_guild_template(GuildId :: snowflake_type(),
                            Code :: string(),
                            Body :: map()) ->
                             {ok, 200, guild_template_response()} |
                             {error, integer(), client_error_response()}.
update_guild_template(GuildId, Code, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/templates/", Code]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec delete_guild_template(GuildId :: snowflake_type(), Code :: string()) ->
                             {ok, 200, guild_template_response()} |
                             {error, integer(), client_error_response()}.
delete_guild_template(GuildId, Code) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/templates/", Code]),

  http_send(delete, Path).

-spec get_active_guild_threads(GuildId :: snowflake_type()) ->
                                {ok, 200, threads_response()} |
                                {error, integer(), client_error_response()}.
get_active_guild_threads(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/threads/active"]),

  http_send(get, Path).

-spec get_guild_vanity_url(GuildId :: snowflake_type()) ->
                            {ok, 200, vanity_url_response()} |
                            {error, integer(), client_error_response()}.
get_guild_vanity_url(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/vanity-url"]),

  http_send(get, Path).

-spec update_self_voice_state(GuildId :: snowflake_type(), Body :: map()) ->
                               {ok, 204} | {error, integer(), client_error_response()}.
update_self_voice_state(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/voice-states/@me"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_self_voice_state(GuildId :: snowflake_type()) ->
                            {ok, 200, voice_state_response()} |
                            {error, integer(), client_error_response()}.
get_self_voice_state(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/voice-states/@me"]),

  http_send(get, Path).

-spec update_voice_state(GuildId :: snowflake_type(),
                         UserId :: snowflake_type(),
                         Body :: map()) ->
                          {ok, 204} | {error, integer(), client_error_response()}.
update_voice_state(GuildId, UserId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/voice-states/", UserId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_voice_state(GuildId :: snowflake_type(), UserId :: snowflake_type()) ->
                       {ok, 200, voice_state_response()} |
                       {error, integer(), client_error_response()}.
get_voice_state(GuildId, UserId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/voice-states/", UserId]),

  http_send(get, Path).

-spec get_guild_webhooks(GuildId :: snowflake_type()) ->
                          {ok,
                           200,
                           [application_incoming_webhook_response() |
                            channel_follower_webhook_response() |
                            guild_incoming_webhook_response()]} |
                          {error, integer(), client_error_response()}.
get_guild_webhooks(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/webhooks"]),

  http_send(get, Path).

-spec update_guild_welcome_screen(GuildId :: snowflake_type(),
                                  Body :: welcome_screen_patch_request_partial()) ->
                                   {ok, 200, guild_welcome_screen_response()} |
                                   {error, integer(), client_error_response()}.
update_guild_welcome_screen(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/welcome-screen"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_welcome_screen(GuildId :: snowflake_type()) ->
                                {ok, 200, guild_welcome_screen_response()} |
                                {error, integer(), client_error_response()}.
get_guild_welcome_screen(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/welcome-screen"]),

  http_send(get, Path).

-spec update_guild_widget_settings(GuildId :: snowflake_type(), Body :: map()) ->
                                    {ok, 200, widget_settings_response()} |
                                    {error, integer(), client_error_response()}.
update_guild_widget_settings(GuildId, Body) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/widget"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_guild_widget_settings(GuildId :: snowflake_type()) ->
                                 {ok, 200, widget_settings_response()} |
                                 {error, integer(), client_error_response()}.
get_guild_widget_settings(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/widget"]),

  http_send(get, Path).

-spec get_guild_widget(GuildId :: snowflake_type()) ->
                        {ok, 200, widget_response()} | {error, integer(), client_error_response()}.
get_guild_widget(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/widget.json"]),

  http_send(get, Path).

-spec get_guild_widget_png(GuildId :: snowflake_type()) ->
                            {ok, 200, binary()} | {error, integer(), client_error_response()}.
get_guild_widget_png(GuildId) ->
  Path = iolist_to_binary(["/guilds/", GuildId, "/widget.png"]),

  http_send(get, Path).

-spec create_interaction_response(InteractionId :: snowflake_type(),
                                  InteractionToken :: string(),
                                  Body ::
                                    application_command_autocomplete_callback_request() |
                                    create_message_interaction_callback_request() |
                                    launch_activity_interaction_callback_request() |
                                    modal_interaction_callback_request() |
                                    pong_interaction_callback_request() |
                                    update_message_interaction_callback_request()) ->
                                   {ok, 200, interaction_callback_response()} |
                                   {ok, 204} |
                                   {error, integer(), client_error_response()}.
create_interaction_response(InteractionId, InteractionToken, Body) ->
  Path =
    iolist_to_binary(["/interactions/", InteractionId, "/", InteractionToken, "/callback"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec invite_resolve(Code :: string()) ->
                      {ok,
                       200,
                       friend_invite_response() |
                       group_dm_invite_response() |
                       guild_invite_response()} |
                      {error, integer(), client_error_response()}.
invite_resolve(Code) ->
  Path = iolist_to_binary(["/invites/", Code]),

  http_send(get, Path).

-spec invite_revoke(Code :: string()) ->
                     {ok,
                      200,
                      friend_invite_response() |
                      group_dm_invite_response() |
                      guild_invite_response()} |
                     {error, integer(), client_error_response()}.
invite_revoke(Code) ->
  Path = iolist_to_binary(["/invites/", Code]),

  http_send(delete, Path).

-spec get_my_oauth2_authorization() ->
                                   {ok, 200, oauth2_get_authorization_response()} |
                                   {error, integer(), client_error_response()}.
get_my_oauth2_authorization() ->
  Path = iolist_to_binary(["/oauth2/@me"]),

  http_send(get, Path).

-spec get_my_oauth2_application() ->
                                 {ok, 200, private_application_response()} |
                                 {error, integer(), client_error_response()}.
get_my_oauth2_application() ->
  Path = iolist_to_binary(["/oauth2/applications/@me"]),

  http_send(get, Path).

-spec get_public_keys() ->
                       {ok, 200, oauth2_get_keys()} | {error, integer(), client_error_response()}.
get_public_keys() ->
  Path = iolist_to_binary(["/oauth2/keys"]),

  http_send(get, Path).

-spec get_openid_connect_userinfo() ->
                                   {ok, 200, oauth2_get_open_id_connect_user_info_response()} |
                                   {error, integer(), client_error_response()}.
get_openid_connect_userinfo() ->
  Path = iolist_to_binary(["/oauth2/userinfo"]),

  http_send(get, Path).

-spec get_soundboard_default_sounds() ->
                                     {ok, 200, [soundboard_sound_response()]} |
                                     {error, integer(), client_error_response()}.
get_soundboard_default_sounds() ->
  Path = iolist_to_binary(["/soundboard-default-sounds"]),

  http_send(get, Path).

-spec create_stage_instance(Body :: map()) ->
                             {ok, 200, stage_instance_response()} |
                             {error, integer(), client_error_response()}.
create_stage_instance(Body) ->
  Path = iolist_to_binary(["/stage-instances"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec update_stage_instance(ChannelId :: snowflake_type(), Body :: map()) ->
                             {ok, 200, stage_instance_response()} |
                             {error, integer(), client_error_response()}.
update_stage_instance(ChannelId, Body) ->
  Path = iolist_to_binary(["/stage-instances/", ChannelId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_stage_instance(ChannelId :: snowflake_type()) ->
                          {ok, 200, stage_instance_response()} |
                          {error, integer(), client_error_response()}.
get_stage_instance(ChannelId) ->
  Path = iolist_to_binary(["/stage-instances/", ChannelId]),

  http_send(get, Path).

-spec delete_stage_instance(ChannelId :: snowflake_type()) ->
                             {ok, 204} | {error, integer(), client_error_response()}.
delete_stage_instance(ChannelId) ->
  Path = iolist_to_binary(["/stage-instances/", ChannelId]),

  http_send(delete, Path).

-spec list_sticker_packs() ->
                          {ok, 200, sticker_pack_collection_response()} |
                          {error, integer(), client_error_response()}.
list_sticker_packs() ->
  Path = iolist_to_binary(["/sticker-packs"]),

  http_send(get, Path).

-spec get_sticker_pack(PackId :: snowflake_type()) ->
                        {ok, 200, sticker_pack_response()} |
                        {error, integer(), client_error_response()}.
get_sticker_pack(PackId) ->
  Path = iolist_to_binary(["/sticker-packs/", PackId]),

  http_send(get, Path).

-spec get_sticker(StickerId :: snowflake_type()) ->
                   {ok, 200, guild_sticker_response() | standard_sticker_response()} |
                   {error, integer(), client_error_response()}.
get_sticker(StickerId) ->
  Path = iolist_to_binary(["/stickers/", StickerId]),

  http_send(get, Path).

-spec update_my_user(Body :: bot_account_patch_request()) ->
                      {ok, 200, user_pii_response()} | {error, integer(), client_error_response()}.
update_my_user(Body) ->
  Path = iolist_to_binary(["/users/@me"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_my_user() ->
                   {ok, 200, user_pii_response()} | {error, integer(), client_error_response()}.
get_my_user() ->
  Path = iolist_to_binary(["/users/@me"]),

  http_send(get, Path).

-spec update_application_user_role_connection(ApplicationId :: snowflake_type(),
                                              Body :: map()) ->
                                               {ok,
                                                200,
                                                application_user_role_connection_response()} |
                                               {error, integer(), client_error_response()}.
update_application_user_role_connection(ApplicationId, Body) ->
  Path = iolist_to_binary(["/users/@me/applications/", ApplicationId, "/role-connection"]),
  BodyBin = body_to_binary(Body),
  http_send(put, Path, BodyBin).

-spec get_application_user_role_connection(ApplicationId :: snowflake_type()) ->
                                            {ok, 200, application_user_role_connection_response()} |
                                            {error, integer(), client_error_response()}.
get_application_user_role_connection(ApplicationId) ->
  Path = iolist_to_binary(["/users/@me/applications/", ApplicationId, "/role-connection"]),

  http_send(get, Path).

-spec delete_application_user_role_connection(ApplicationId :: snowflake_type()) ->
                                               {ok, 204} |
                                               {error, integer(), client_error_response()}.
delete_application_user_role_connection(ApplicationId) ->
  Path = iolist_to_binary(["/users/@me/applications/", ApplicationId, "/role-connection"]),

  http_send(delete, Path).

-spec create_dm(Body :: create_private_channel_request()) ->
                 {ok, 200, private_channel_response() | private_group_channel_response()} |
                 {error, integer(), client_error_response()}.
create_dm(Body) ->
  Path = iolist_to_binary(["/users/@me/channels"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec list_my_connections() ->
                           {ok, 200, [connected_account_response()]} |
                           {error, integer(), client_error_response()}.
list_my_connections() ->
  Path = iolist_to_binary(["/users/@me/connections"]),

  http_send(get, Path).

-spec list_my_guilds() ->
                      {ok, 200, [my_guild_response()]} |
                      {error, integer(), client_error_response()}.
list_my_guilds() ->
  Path = iolist_to_binary(["/users/@me/guilds"]),

  http_send(get, Path).

-spec leave_guild(GuildId :: snowflake_type()) ->
                   {ok, 204} | {error, integer(), client_error_response()}.
leave_guild(GuildId) ->
  Path = iolist_to_binary(["/users/@me/guilds/", GuildId]),

  http_send(delete, Path).

-spec get_my_guild_member(GuildId :: snowflake_type()) ->
                           {ok, 200, private_guild_member_response()} |
                           {error, integer(), client_error_response()}.
get_my_guild_member(GuildId) ->
  Path = iolist_to_binary(["/users/@me/guilds/", GuildId, "/member"]),

  http_send(get, Path).

-spec get_user(UserId :: snowflake_type()) ->
                {ok, 200, user_response()} | {error, integer(), client_error_response()}.
get_user(UserId) ->
  Path = iolist_to_binary(["/users/", UserId]),

  http_send(get, Path).

-spec list_voice_regions() ->
                          {ok, 200, [voice_region_response()]} |
                          {error, integer(), client_error_response()}.
list_voice_regions() ->
  Path = iolist_to_binary(["/voice/regions"]),

  http_send(get, Path).

-spec update_webhook(WebhookId :: snowflake_type(), Body :: map()) ->
                      {ok,
                       200,
                       application_incoming_webhook_response() |
                       channel_follower_webhook_response() |
                       guild_incoming_webhook_response()} |
                      {error, integer(), client_error_response()}.
update_webhook(WebhookId, Body) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_webhook(WebhookId :: snowflake_type()) ->
                   {ok,
                    200,
                    application_incoming_webhook_response() |
                    channel_follower_webhook_response() |
                    guild_incoming_webhook_response()} |
                   {error, integer(), client_error_response()}.
get_webhook(WebhookId) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId]),

  http_send(get, Path).

-spec delete_webhook(WebhookId :: snowflake_type()) ->
                      {ok, 204} | {error, integer(), client_error_response()}.
delete_webhook(WebhookId) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId]),

  http_send(delete, Path).

-spec execute_webhook(WebhookId :: snowflake_type(),
                      WebhookToken :: string(),
                      Body ::
                        incoming_webhook_request_partial() |
                        incoming_webhook_update_request_partial()) ->
                       {ok, 200, message_response()} |
                       {ok, 204} |
                       {error, integer(), client_error_response()}.
execute_webhook(WebhookId, WebhookToken, Body) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec update_webhook_by_token(WebhookId :: snowflake_type(),
                              WebhookToken :: string(),
                              Body :: map()) ->
                               {ok,
                                200,
                                application_incoming_webhook_response() |
                                channel_follower_webhook_response() |
                                guild_incoming_webhook_response()} |
                               {error, integer(), client_error_response()}.
update_webhook_by_token(WebhookId, WebhookToken, Body) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_webhook_by_token(WebhookId :: snowflake_type(), WebhookToken :: string()) ->
                            {ok,
                             200,
                             application_incoming_webhook_response() |
                             channel_follower_webhook_response() |
                             guild_incoming_webhook_response()} |
                            {error, integer(), client_error_response()}.
get_webhook_by_token(WebhookId, WebhookToken) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken]),

  http_send(get, Path).

-spec delete_webhook_by_token(WebhookId :: snowflake_type(), WebhookToken :: string()) ->
                               {ok, 204} | {error, integer(), client_error_response()}.
delete_webhook_by_token(WebhookId, WebhookToken) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken]),

  http_send(delete, Path).

-spec execute_github_compatible_webhook(WebhookId :: snowflake_type(),
                                        WebhookToken :: string(),
                                        Body :: github_webhook()) ->
                                         {ok, 204} | {error, integer(), client_error_response()}.
execute_github_compatible_webhook(WebhookId, WebhookToken, Body) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/github"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

-spec update_original_webhook_message(WebhookId :: snowflake_type(),
                                      WebhookToken :: string(),
                                      Body :: incoming_webhook_update_request_partial()) ->
                                       {ok, 200, message_response()} |
                                       {error, integer(), client_error_response()}.
update_original_webhook_message(WebhookId, WebhookToken, Body) ->
  Path =
    iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/messages/@original"]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_original_webhook_message(WebhookId :: snowflake_type(),
                                   WebhookToken :: string()) ->
                                    {ok, 200, message_response()} |
                                    {error, integer(), client_error_response()}.
get_original_webhook_message(WebhookId, WebhookToken) ->
  Path =
    iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/messages/@original"]),

  http_send(get, Path).

-spec delete_original_webhook_message(WebhookId :: snowflake_type(),
                                      WebhookToken :: string()) ->
                                       {ok, 204} | {error, integer(), client_error_response()}.
delete_original_webhook_message(WebhookId, WebhookToken) ->
  Path =
    iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/messages/@original"]),

  http_send(delete, Path).

-spec update_webhook_message(WebhookId :: snowflake_type(),
                             WebhookToken :: string(),
                             MessageId :: snowflake_type(),
                             Body :: incoming_webhook_update_request_partial()) ->
                              {ok, 200, message_response()} |
                              {error, integer(), client_error_response()}.
update_webhook_message(WebhookId, WebhookToken, MessageId, Body) ->
  Path =
    iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/messages/", MessageId]),
  BodyBin = body_to_binary(Body),
  http_send(patch, Path, BodyBin).

-spec get_webhook_message(WebhookId :: snowflake_type(),
                          WebhookToken :: string(),
                          MessageId :: snowflake_type()) ->
                           {ok, 200, message_response()} |
                           {error, integer(), client_error_response()}.
get_webhook_message(WebhookId, WebhookToken, MessageId) ->
  Path =
    iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/messages/", MessageId]),

  http_send(get, Path).

-spec delete_webhook_message(WebhookId :: snowflake_type(),
                             WebhookToken :: string(),
                             MessageId :: snowflake_type()) ->
                              {ok, 204} | {error, integer(), client_error_response()}.
delete_webhook_message(WebhookId, WebhookToken, MessageId) ->
  Path =
    iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/messages/", MessageId]),

  http_send(delete, Path).

-spec execute_slack_compatible_webhook(WebhookId :: snowflake_type(),
                                       WebhookToken :: string(),
                                       Body :: slack_webhook()) ->
                                        {ok, 200, binary()} |
                                        {error, integer(), client_error_response()}.
execute_slack_compatible_webhook(WebhookId, WebhookToken, Body) ->
  Path = iolist_to_binary(["/webhooks/", WebhookId, "/", WebhookToken, "/slack"]),
  BodyBin = body_to_binary(Body),
  http_send(post, Path, BodyBin).

http_send(Method, Path) ->
  erdi_http:send_await({Method, Path}).

http_send(Method, Path, Body) ->
  erdi_http:send_await({Method, Path, Body}).

body_to_binary(Body) ->
  iolist_to_binary(json:encode(Body)).
