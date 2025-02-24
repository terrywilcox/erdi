-type activity_location_kinds() :: binary().

-type afk_timeouts() :: integer().

-type allowed_mention_types() :: binary().

-type application_command_handler() :: integer().

-type application_command_option_type() :: integer().

-type application_command_permission_type() :: integer().

-type application_command_type() :: integer().

-type application_explicit_content_filter_types() :: integer().

-type application_integration_type() :: integer().

-type application_types() :: integer().

-type audit_log_action_types() :: integer().

-type automod_action_type() :: integer().

-type automod_event_type() :: integer().

-type automod_keyword_preset_type() :: integer().

-type automod_trigger_type() :: integer().

-type available_locales_enum() :: binary().

-type button_style_types() :: integer().

-type channel_permission_overwrites() :: integer().

-type channel_types() :: integer().

-type client_error_response() :: error_response().

-type confetti_potion_create_request() :: map().

-type connected_account_providers() :: binary().

-type connected_account_visibility() :: integer().

-type entitlement_owner_types() :: integer().

-type entitlement_tenant_fulfillment_status_response() :: integer().

-type entitlement_types() :: integer().

-type entity_metadata_stage_instance() :: map().

-type entity_metadata_stage_instance_response() :: map().

-type entity_metadata_voice() :: map().

-type entity_metadata_voice_response() :: map().

-type error_details() :: error_details() | inner_errors().

-type error_response() :: map().

-type forum_layout() :: integer().

-type guild_explicit_content_filter_types() :: integer().

-type guild_features() :: binary().

-type guild_m_f_a_level() :: integer().

-type guild_n_s_f_w_content_level() :: integer().

-type guild_onboarding_mode() :: integer().

-type guild_scheduled_event_entity_types() :: integer().

-type guild_scheduled_event_privacy_levels() :: integer().

-type guild_scheduled_event_statuses() :: integer().

-type icon_emoji_response() :: map().

-type int53_type() :: integer().

-type integration_expire_behavior_types() :: integer().

-type integration_expire_grace_period_types() :: integer().

-type integration_types() :: binary().

-type interaction_callback_types() :: integer().

-type interaction_context_type() :: integer().

-type interaction_types() :: integer().

-type invite_target_types() :: integer().

-type invite_types() :: integer().

-type message_activity_response() :: map().

-type message_component_types() :: integer().

-type message_reference_type() :: integer().

-type message_type() :: integer().

-type metadata_item_types() :: integer().

-type ml_spam_trigger_metadata() :: map().

-type ml_spam_trigger_metadata_response() :: map().

-type new_member_action_type() :: integer().

-type oauth2_scopes() :: binary().

-type onboarding_prompt_type() :: integer().

-type poll_layout_types() :: integer().

-type premium_guild_tiers() :: integer().

-type premium_types() :: integer().

-type purchase_type() :: integer().

-type quarantine_user_action_metadata() :: map().

-type quarantine_user_action_metadata_response() :: map().

-type reaction_types() :: integer().

-type snowflake_select_default_value_types() :: binary().

-type snowflake_type() :: binary().

-type spam_link_trigger_metadata_response() :: map().

-type stage_instances_privacy_levels() :: integer().

-type sticker_format_types() :: integer().

-type sticker_types() :: integer().

-type team_membership_states() :: integer().

-type text_input_style_types() :: integer().

-type thread_auto_archive_duration() :: integer().

-type thread_sort_order() :: integer().

-type typing_indicator_response() :: map().

-type u_int32_type() :: integer().

-type user_notification_settings() :: integer().

-type user_primary_guild_response() :: map().

-type verification_levels() :: integer().

-type video_quality_modes() :: integer().

-type webhook_types() :: integer().

-type widget_image_styles() :: binary().

-type widget_user_discriminator() :: binary().

-type account_response() :: #{
  id := binary(),
  name => 'undefined' | binary()
}.

-type action_row_component_for_message_request() :: #{
  components := list(),
  type := integer()
}.

-type action_row_component_for_modal_request() :: #{
  components := list(),
  type := integer()
}.

-type action_row_component_response() :: #{
  components => 'undefined' | list(),
  id := integer(),
  type := integer()
}.

-type activities_attachment_response() :: #{
  attachment := attachment_response()
}.

-type application_command_attachment_option() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_attachment_option_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_autocomplete_callback_request() :: #{
  data := any(),
  type := integer()
}.

-type application_command_boolean_option() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_boolean_option_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_channel_option() :: #{
  channel_types => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_channel_option_response() :: #{
  channel_types => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_create_request() :: #{
  contexts => 'undefined' | list(),
  default_member_permissions => 'undefined' | integer(),
  description => 'undefined' | binary(),
  description_localizations => 'undefined' | any(),
  dm_permission => 'undefined' | boolean(),
  handler => 'undefined' | application_command_handler(),
  integration_types => 'undefined' | list(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  options => 'undefined' | list(),
  type => 'undefined' | application_command_type()
}.

-type application_command_integer_option() :: #{
  autocomplete => 'undefined' | boolean(),
  choices => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  max_value => 'undefined' | int53_type(),
  min_value => 'undefined' | int53_type(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_integer_option_response() :: #{
  autocomplete => 'undefined' | boolean(),
  choices => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  max_value => 'undefined' | int53_type(),
  min_value => 'undefined' | int53_type(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_interaction_metadata_response() :: #{
  authorizing_integration_owners := any(),
  id := snowflake_type(),
  original_response_message_id => 'undefined' | snowflake_type(),
  target_message_id => 'undefined' | snowflake_type(),
  target_user => 'undefined' | user_response(),
  type := integer(),
  user => 'undefined' | user_response()
}.

-type application_command_mentionable_option() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_mentionable_option_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_number_option() :: #{
  autocomplete => 'undefined' | boolean(),
  choices => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  max_value => 'undefined' | float(),
  min_value => 'undefined' | float(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_number_option_response() :: #{
  autocomplete => 'undefined' | boolean(),
  choices => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  max_value => 'undefined' | float(),
  min_value => 'undefined' | float(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_option_integer_choice() :: #{
  name := binary(),
  name_localizations => 'undefined' | any(),
  value := int53_type()
}.

-type application_command_option_integer_choice_response() :: #{
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  value := int53_type()
}.

-type application_command_option_number_choice() :: #{
  name := binary(),
  name_localizations => 'undefined' | any(),
  value := float()
}.

-type application_command_option_number_choice_response() :: #{
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  value := float()
}.

-type application_command_option_string_choice() :: #{
  name := binary(),
  name_localizations => 'undefined' | any(),
  value := binary()
}.

-type application_command_option_string_choice_response() :: #{
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  value := binary()
}.

-type application_command_patch_request_partial() :: #{
  contexts => 'undefined' | list(),
  default_member_permissions => 'undefined' | integer(),
  description => 'undefined' | binary(),
  description_localizations => 'undefined' | any(),
  dm_permission => 'undefined' | boolean(),
  handler => 'undefined' | application_command_handler(),
  integration_types => 'undefined' | list(),
  name => 'undefined' | binary(),
  name_localizations => 'undefined' | any(),
  options => 'undefined' | list()
}.

-type application_command_permission() :: #{
  id := snowflake_type(),
  permission := boolean(),
  type := application_command_permission_type()
}.

-type application_command_response() :: #{
  application_id := snowflake_type(),
  contexts => 'undefined' | list(),
  default_member_permissions => 'undefined' | binary(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  dm_permission => 'undefined' | boolean(),
  guild_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  integration_types => 'undefined' | list(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  nsfw => 'undefined' | boolean(),
  options => 'undefined' | list(),
  type := application_command_type(),
  version := snowflake_type()
}.

-type application_command_role_option() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_role_option_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_string_option() :: #{
  autocomplete => 'undefined' | boolean(),
  choices => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  max_length => 'undefined' | integer(),
  min_length => 'undefined' | integer(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_string_option_response() :: #{
  autocomplete => 'undefined' | boolean(),
  choices => 'undefined' | list(),
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  max_length => 'undefined' | integer(),
  min_length => 'undefined' | integer(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_subcommand_group_option() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  options => 'undefined' | list(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_subcommand_group_option_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  options => 'undefined' | list(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_subcommand_option() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  options => 'undefined' | list(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_subcommand_option_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  options => 'undefined' | list(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_update_request() :: #{
  contexts => 'undefined' | list(),
  default_member_permissions => 'undefined' | integer(),
  description => 'undefined' | binary(),
  description_localizations => 'undefined' | any(),
  dm_permission => 'undefined' | boolean(),
  handler => 'undefined' | application_command_handler(),
  id => 'undefined' | snowflake_type(),
  integration_types => 'undefined' | list(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  options => 'undefined' | list(),
  type => 'undefined' | application_command_type()
}.

-type application_command_user_option() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_command_user_option_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  description_localized => 'undefined' | binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  name_localized => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  type := integer()
}.

-type application_form_partial() :: #{
  cover_image => 'undefined' | binary(),
  custom_install_url => 'undefined' | binary(),
  description => 'undefined' | any(),
  explicit_content_filter => 'undefined' | application_explicit_content_filter_types(),
  flags => 'undefined' | integer(),
  icon => 'undefined' | binary(),
  install_params => 'undefined' | application_oauth2_install_params(),
  integration_types_config => 'undefined' | any(),
  interactions_endpoint_url => 'undefined' | binary(),
  max_participants => 'undefined' | integer(),
  role_connections_verification_url => 'undefined' | binary(),
  tags => 'undefined' | list(),
  team_id => 'undefined' | snowflake_type(),
  type => 'undefined' | application_types()
}.

-type application_incoming_webhook_response() :: #{
  application_id => 'undefined' | snowflake_type(),
  avatar => 'undefined' | binary(),
  channel_id => 'undefined' | snowflake_type(),
  guild_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  type := integer(),
  user => 'undefined' | user_response()
}.

-type application_integration_type_configuration() :: #{
  oauth2_install_params => 'undefined' | application_oauth2_install_params()
}.

-type application_integration_type_configuration_response() :: #{
  oauth2_install_params => 'undefined' | application_oauth2_install_params_response()
}.

-type application_oauth2_install_params() :: #{
  permissions => 'undefined' | integer(),
  scopes => 'undefined' | list()
}.

-type application_oauth2_install_params_response() :: #{
  permissions := binary(),
  scopes := list()
}.

-type application_response() :: #{
  bot => 'undefined' | user_response(),
  bot_public => 'undefined' | boolean(),
  bot_require_code_grant => 'undefined' | boolean(),
  cover_image => 'undefined' | binary(),
  custom_install_url => 'undefined' | binary(),
  description := binary(),
  flags := integer(),
  guild_id => 'undefined' | snowflake_type(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  install_params => 'undefined' | application_oauth2_install_params_response(),
  integration_types_config => 'undefined' | any(),
  max_participants => 'undefined' | integer(),
  name := binary(),
  primary_sku_id => 'undefined' | snowflake_type(),
  privacy_policy_url => 'undefined' | binary(),
  rpc_origins => 'undefined' | list(),
  slug => 'undefined' | binary(),
  tags => 'undefined' | list(),
  terms_of_service_url => 'undefined' | binary(),
  type => 'undefined' | application_types(),
  verify_key := binary()
}.

-type application_role_connections_metadata_item_request() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  key := binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  type := metadata_item_types()
}.

-type application_role_connections_metadata_item_response() :: #{
  description := binary(),
  description_localizations => 'undefined' | any(),
  key := binary(),
  name := binary(),
  name_localizations => 'undefined' | any(),
  type := metadata_item_types()
}.

-type application_user_role_connection_response() :: #{
  metadata => 'undefined' | any(),
  platform_name => 'undefined' | binary(),
  platform_username => 'undefined' | binary()
}.

-type attachment_response() :: #{
  application => 'undefined' | application_response(),
  clip_created_at => 'undefined' | binary(),
  clip_participants => 'undefined' | list(),
  content_type => 'undefined' | binary(),
  description => 'undefined' | binary(),
  duration_secs => 'undefined' | float(),
  ephemeral => 'undefined' | boolean(),
  filename := binary(),
  height => 'undefined' | integer(),
  id := snowflake_type(),
  proxy_url := binary(),
  size := integer(),
  title => 'undefined' | binary(),
  url := binary(),
  waveform => 'undefined' | binary(),
  width => 'undefined' | integer()
}.

-type audit_log_entry_response() :: #{
  action_type := audit_log_action_types(),
  changes => 'undefined' | list(),
  id := snowflake_type(),
  options => 'undefined' | any(),
  reason => 'undefined' | binary(),
  target_id => 'undefined' | snowflake_type(),
  user_id => 'undefined' | snowflake_type()
}.

-type audit_log_object_change_response() :: #{
  key => 'undefined' | binary(),
  new_value => 'undefined' | any(),
  old_value => 'undefined' | any()
}.

-type base_create_message_create_request() :: #{
  allowed_mentions => 'undefined' | message_allowed_mentions_request(),
  attachments => 'undefined' | list(),
  components => 'undefined' | list(),
  confetti_potion => 'undefined' | confetti_potion_create_request(),
  content => 'undefined' | binary(),
  embeds => 'undefined' | list(),
  flags => 'undefined' | integer(),
  poll => 'undefined' | poll_create_request(),
  sticker_ids => 'undefined' | list()
}.

-type basic_application_response() :: #{
  bot => 'undefined' | user_response(),
  cover_image => 'undefined' | binary(),
  description := binary(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary(),
  primary_sku_id => 'undefined' | snowflake_type(),
  type => 'undefined' | application_types()
}.

-type basic_message_response() :: #{
  interaction_metadata => 'undefined' | message_component_interaction_metadata_response(),
  embeds := list(),
  activity => 'undefined' | message_activity_response(),
  interaction => 'undefined' | message_interaction_response(),
  nonce => 'undefined' | binary(),
  mention_everyone := boolean(),
  timestamp := binary(),
  pinned := boolean(),
  mention_roles := list(),
  components := list(),
  resolved => 'undefined' | resolved_objects_response(),
  mention_channels => 'undefined' | list(),
  author := user_response(),
  edited_timestamp => 'undefined' | binary(),
  tts := boolean(),
  mentions := list(),
  channel_id := snowflake_type(),
  purchase_notification => 'undefined' | purchase_notification_response(),
  call => 'undefined' | message_call_response(),
  message_snapshots => 'undefined' | list(),
  id := snowflake_type(),
  webhook_id => 'undefined' | snowflake_type(),
  poll => 'undefined' | poll_response(),
  flags := integer(),
  application_id => 'undefined' | snowflake_type(),
  content := binary(),
  application => 'undefined' | basic_application_response(),
  message_reference => 'undefined' | message_reference_response(),
  position => 'undefined' | integer(),
  type := message_type(),
  attachments := list(),
  thread => 'undefined' | thread_response(),
  stickers => 'undefined' | list(),
  role_subscription_data => 'undefined' | message_role_subscription_data_response(),
  sticker_items => 'undefined' | list()
}.

-type block_message_action() :: #{
  metadata => 'undefined' | block_message_action_metadata(),
  type := integer()
}.

-type block_message_action_metadata() :: #{
  custom_message => 'undefined' | binary()
}.

-type block_message_action_metadata_response() :: #{
  custom_message => 'undefined' | binary()
}.

-type block_message_action_response() :: #{
  metadata := block_message_action_metadata_response(),
  type := integer()
}.

-type bot_account_patch_request() :: #{
  avatar => 'undefined' | binary(),
  banner => 'undefined' | binary(),
  username := binary()
}.

-type bulk_ban_users_response() :: #{
  banned_users := list(),
  failed_users := list()
}.

-type button_component_for_message_request() :: #{
  custom_id => 'undefined' | binary(),
  disabled => 'undefined' | boolean(),
  emoji => 'undefined' | component_emoji_for_message_request(),
  label => 'undefined' | binary(),
  sku_id => 'undefined' | snowflake_type(),
  style := button_style_types(),
  type := integer(),
  url => 'undefined' | binary()
}.

-type button_component_response() :: #{
  custom_id => 'undefined' | binary(),
  disabled => 'undefined' | boolean(),
  emoji => 'undefined' | component_emoji_response(),
  id := integer(),
  label => 'undefined' | binary(),
  sku_id => 'undefined' | snowflake_type(),
  style := button_style_types(),
  type := integer(),
  url => 'undefined' | binary()
}.

-type channel_follower_response() :: #{
  channel_id := snowflake_type(),
  webhook_id := snowflake_type()
}.

-type channel_follower_webhook_response() :: #{
  application_id => 'undefined' | snowflake_type(),
  avatar => 'undefined' | binary(),
  channel_id => 'undefined' | snowflake_type(),
  guild_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  source_channel => 'undefined' | webhook_source_channel_response(),
  source_guild => 'undefined' | webhook_source_guild_response(),
  type := integer(),
  user => 'undefined' | user_response()
}.

-type channel_permission_overwrite_request() :: #{
  allow => 'undefined' | integer(),
  deny => 'undefined' | integer(),
  id := snowflake_type(),
  type => 'undefined' | channel_permission_overwrites()
}.

-type channel_permission_overwrite_response() :: #{
  allow := binary(),
  deny := binary(),
  id := snowflake_type(),
  type := channel_permission_overwrites()
}.

-type channel_select_component_for_message_request() :: #{
  channel_types => 'undefined' | list(),
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type channel_select_component_response() :: #{
  channel_types => 'undefined' | list(),
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  id := integer(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type channel_select_default_value() :: #{
  id := snowflake_type(),
  type := binary()
}.

-type channel_select_default_value_response() :: #{
  id := snowflake_type(),
  type := binary()
}.

-type command_permission_response() :: #{
  id := snowflake_type(),
  permission := boolean(),
  type := application_command_permission_type()
}.

-type command_permissions_response() :: #{
  application_id := snowflake_type(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  permissions := list()
}.

-type component_emoji_for_message_request() :: #{
  id => 'undefined' | snowflake_type(),
  name := binary()
}.

-type component_emoji_response() :: #{
  animated => 'undefined' | boolean(),
  id => 'undefined' | snowflake_type(),
  name := binary()
}.

-type connected_account_guild_response() :: #{
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary()
}.

-type connected_account_integration_response() :: #{
  account := account_response(),
  guild := connected_account_guild_response(),
  id := binary(),
  type := integration_types()
}.

-type connected_account_response() :: #{
  friend_sync := boolean(),
  id := binary(),
  integrations => 'undefined' | list(),
  name => 'undefined' | binary(),
  revoked => 'undefined' | boolean(),
  show_activity := boolean(),
  two_way_link := boolean(),
  type := connected_account_providers(),
  verified := boolean(),
  visibility := connected_account_visibility()
}.

-type create_entitlement_request_data() :: #{
  owner_id := snowflake_type(),
  owner_type := entitlement_owner_types(),
  sku_id := snowflake_type()
}.

-type create_forum_thread_request() :: #{
  applied_tags => 'undefined' | list(),
  auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  message := base_create_message_create_request(),
  name := binary(),
  rate_limit_per_user => 'undefined' | integer()
}.

-type create_group_dm_invite_request() :: #{
  max_age => 'undefined' | integer()
}.

-type create_guild_channel_request() :: #{
  available_tags => 'undefined' | list(),
  bitrate => 'undefined' | integer(),
  default_auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  default_forum_layout => 'undefined' | forum_layout(),
  default_reaction_emoji => 'undefined' | update_default_reaction_emoji_request(),
  default_sort_order => 'undefined' | thread_sort_order(),
  default_thread_rate_limit_per_user => 'undefined' | integer(),
  name := binary(),
  nsfw => 'undefined' | boolean(),
  parent_id => 'undefined' | snowflake_type(),
  permission_overwrites => 'undefined' | list(),
  position => 'undefined' | integer(),
  rate_limit_per_user => 'undefined' | integer(),
  rtc_region => 'undefined' | binary(),
  topic => 'undefined' | binary(),
  type => 'undefined' | integer(),
  user_limit => 'undefined' | integer(),
  video_quality_mode => 'undefined' | video_quality_modes()
}.

-type create_guild_invite_request() :: #{
  max_age => 'undefined' | integer(),
  max_uses => 'undefined' | integer(),
  target_application_id => 'undefined' | snowflake_type(),
  target_type => 'undefined' | integer(),
  target_user_id => 'undefined' | snowflake_type(),
  temporary => 'undefined' | boolean(),
  unique => 'undefined' | boolean()
}.

-type create_guild_request_channel_item() :: #{
  available_tags => 'undefined' | list(),
  bitrate => 'undefined' | integer(),
  default_auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  default_forum_layout => 'undefined' | forum_layout(),
  default_reaction_emoji => 'undefined' | update_default_reaction_emoji_request(),
  default_sort_order => 'undefined' | thread_sort_order(),
  default_thread_rate_limit_per_user => 'undefined' | integer(),
  id => 'undefined' | snowflake_type(),
  name := binary(),
  nsfw => 'undefined' | boolean(),
  parent_id => 'undefined' | snowflake_type(),
  permission_overwrites => 'undefined' | list(),
  position => 'undefined' | integer(),
  rate_limit_per_user => 'undefined' | integer(),
  rtc_region => 'undefined' | binary(),
  topic => 'undefined' | binary(),
  type => 'undefined' | integer(),
  user_limit => 'undefined' | integer(),
  video_quality_mode => 'undefined' | video_quality_modes()
}.

-type create_guild_request_role_item() :: #{
  color => 'undefined' | integer(),
  hoist => 'undefined' | boolean(),
  id := integer(),
  mentionable => 'undefined' | boolean(),
  name => 'undefined' | binary(),
  permissions => 'undefined' | integer(),
  unicode_emoji => 'undefined' | binary()
}.

-type create_message_interaction_callback_request() :: #{
  data => 'undefined' | incoming_webhook_interaction_request(),
  type := integer()
}.

-type create_message_interaction_callback_response() :: #{
  message := message_response(),
  type := integer()
}.

-type create_or_update_thread_tag_request() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  moderated => 'undefined' | boolean(),
  name := binary()
}.

-type create_private_channel_request() :: #{
  access_tokens => 'undefined' | list(),
  nicks => 'undefined' | any(),
  recipient_id => 'undefined' | snowflake_type()
}.

-type create_text_thread_with_message_request() :: #{
  auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  name := binary(),
  rate_limit_per_user => 'undefined' | integer()
}.

-type create_text_thread_without_message_request() :: #{
  auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  invitable => 'undefined' | boolean(),
  name := binary(),
  rate_limit_per_user => 'undefined' | integer(),
  type => 'undefined' | integer()
}.

-type created_thread_response() :: #{
  applied_tags => 'undefined' | list(),
  bitrate => 'undefined' | integer(),
  flags := integer(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  last_message_id => 'undefined' | snowflake_type(),
  last_pin_timestamp => 'undefined' | binary(),
  member => 'undefined' | thread_member_response(),
  member_count := integer(),
  message_count := integer(),
  name := binary(),
  owner_id := snowflake_type(),
  parent_id => 'undefined' | snowflake_type(),
  permissions => 'undefined' | binary(),
  rate_limit_per_user => 'undefined' | integer(),
  rtc_region => 'undefined' | binary(),
  thread_metadata => 'undefined' | thread_metadata_response(),
  total_message_sent := integer(),
  type := integer(),
  user_limit => 'undefined' | integer(),
  video_quality_mode => 'undefined' | video_quality_modes()
}.

-type default_keyword_list_trigger_metadata() :: #{
  allow_list => 'undefined' | list(),
  presets => 'undefined' | list()
}.

-type default_keyword_list_trigger_metadata_response() :: #{
  allow_list := list(),
  presets := list()
}.

-type default_keyword_list_upsert_request() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name := binary(),
  trigger_metadata := default_keyword_list_trigger_metadata(),
  trigger_type := integer()
}.

-type default_keyword_list_upsert_request_partial() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type => 'undefined' | automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name => 'undefined' | binary(),
  trigger_metadata => 'undefined' | default_keyword_list_trigger_metadata(),
  trigger_type => 'undefined' | integer()
}.

-type default_keyword_rule_response() :: #{
  actions := list(),
  creator_id := snowflake_type(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  trigger_metadata := default_keyword_list_trigger_metadata_response(),
  trigger_type := integer()
}.

-type default_reaction_emoji_response() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary()
}.

-type discord_integration_response() :: #{
  account => 'undefined' | account_response(),
  application := integration_application_response(),
  enabled => 'undefined' | boolean(),
  id := snowflake_type(),
  name => 'undefined' | binary(),
  scopes := list(),
  type := binary(),
  user => 'undefined' | user_response()
}.

-type embedded_activity_instance() :: #{
  application_id := snowflake_type(),
  instance_id := binary(),
  launch_id := binary(),
  location => 'undefined' | private_channel_location(),
  users := list()
}.

-type emoji_response() :: #{
  animated := boolean(),
  available := boolean(),
  id := snowflake_type(),
  managed := boolean(),
  name := binary(),
  require_colons := boolean(),
  roles := list(),
  user => 'undefined' | user_response()
}.

-type entitlement_response() :: #{
  application_id := snowflake_type(),
  consumed => 'undefined' | boolean(),
  deleted := boolean(),
  ends_at => 'undefined' | binary(),
  fulfilled_at => 'undefined' | binary(),
  fulfillment_status => 'undefined' | entitlement_tenant_fulfillment_status_response(),
  guild_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  sku_id := snowflake_type(),
  starts_at => 'undefined' | binary(),
  type := entitlement_types(),
  user_id := snowflake_type()
}.

-type entity_metadata_external() :: #{
  location := binary()
}.

-type entity_metadata_external_response() :: #{
  location := binary()
}.

-type error() :: #{
  code := integer(),
  message := binary()
}.

-type external_connection_integration_response() :: #{
  account => 'undefined' | account_response(),
  enable_emoticons => 'undefined' | boolean(),
  enabled => 'undefined' | boolean(),
  expire_behavior => 'undefined' | integration_expire_behavior_types(),
  expire_grace_period => 'undefined' | integration_expire_grace_period_types(),
  id := binary(),
  name => 'undefined' | binary(),
  revoked => 'undefined' | boolean(),
  role_id => 'undefined' | snowflake_type(),
  subscriber_count => 'undefined' | integer(),
  synced_at => 'undefined' | binary(),
  syncing => 'undefined' | boolean(),
  type := binary(),
  user := user_response()
}.

-type external_scheduled_event_create_request() :: #{
  channel_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_metadata := entity_metadata_external(),
  entity_type := integer(),
  image => 'undefined' | binary(),
  name := binary(),
  privacy_level := guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time := binary()
}.

-type external_scheduled_event_patch_request_partial() :: #{
  channel_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_metadata => 'undefined' | entity_metadata_external(),
  entity_type => 'undefined' | integer(),
  image => 'undefined' | binary(),
  name => 'undefined' | binary(),
  privacy_level => 'undefined' | guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time => 'undefined' | binary(),
  status => 'undefined' | guild_scheduled_event_statuses()
}.

-type external_scheduled_event_response() :: #{
  channel_id => 'undefined' | snowflake_type(),
  creator => 'undefined' | user_response(),
  creator_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_id => 'undefined' | snowflake_type(),
  entity_metadata := entity_metadata_external_response(),
  entity_type := integer(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  image => 'undefined' | binary(),
  name := binary(),
  privacy_level := guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time := binary(),
  status := guild_scheduled_event_statuses(),
  user_count => 'undefined' | integer(),
  user_rsvp => 'undefined' | scheduled_event_user_response()
}.

-type flag_to_channel_action() :: #{
  metadata := flag_to_channel_action_metadata(),
  type := integer()
}.

-type flag_to_channel_action_metadata() :: #{
  channel_id := snowflake_type()
}.

-type flag_to_channel_action_metadata_response() :: #{
  channel_id := snowflake_type()
}.

-type flag_to_channel_action_response() :: #{
  metadata := flag_to_channel_action_metadata_response(),
  type := integer()
}.

-type forum_tag_response() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  id := snowflake_type(),
  moderated := boolean(),
  name := binary()
}.

-type friend_invite_response() :: #{
  channel => 'undefined' | invite_channel_response(),
  code := binary(),
  created_at => 'undefined' | binary(),
  expires_at => 'undefined' | binary(),
  flags => 'undefined' | integer(),
  friends_count => 'undefined' | integer(),
  inviter => 'undefined' | user_response(),
  is_contact => 'undefined' | boolean(),
  max_age => 'undefined' | integer(),
  max_uses => 'undefined' | integer(),
  type => 'undefined' | integer(),
  uses => 'undefined' | integer()
}.

-type gateway_bot_response() :: #{
  session_start_limit := gateway_bot_session_start_limit_response(),
  shards := integer(),
  url := binary()
}.

-type gateway_bot_session_start_limit_response() :: #{
  max_concurrency := integer(),
  remaining := integer(),
  reset_after := integer(),
  total := integer()
}.

-type gateway_response() :: #{
  url := binary()
}.

-type github_author() :: #{
  name := binary(),
  username => 'undefined' | binary()
}.

-type github_check_app() :: #{
  name := binary()
}.

-type github_check_pull_request() :: #{
  number := integer()
}.

-type github_check_run() :: #{
  check_suite := github_check_suite(),
  conclusion => 'undefined' | binary(),
  details_url => 'undefined' | binary(),
  html_url := binary(),
  name := binary(),
  output => 'undefined' | github_check_run_output(),
  pull_requests => 'undefined' | list()
}.

-type github_check_run_output() :: #{
  summary => 'undefined' | binary(),
  title => 'undefined' | binary()
}.

-type github_check_suite() :: #{
  app := github_check_app(),
  conclusion => 'undefined' | binary(),
  head_branch => 'undefined' | binary(),
  head_sha := binary(),
  pull_requests => 'undefined' | list()
}.

-type github_comment() :: #{
  body := binary(),
  commit_id => 'undefined' | binary(),
  html_url := binary(),
  id := integer(),
  user := github_user()
}.

-type github_commit() :: #{
  author := github_author(),
  id := binary(),
  message := binary(),
  url := binary()
}.

-type github_discussion() :: #{
  answer_html_url => 'undefined' | binary(),
  body => 'undefined' | binary(),
  html_url := binary(),
  number := integer(),
  title := binary(),
  user := github_user()
}.

-type github_issue() :: #{
  body => 'undefined' | binary(),
  html_url := binary(),
  id := integer(),
  number := integer(),
  pull_request => 'undefined' | any(),
  title := binary(),
  user := github_user()
}.

-type github_release() :: #{
  author := github_user(),
  html_url := binary(),
  id := integer(),
  tag_name := binary()
}.

-type github_repository() :: #{
  full_name := binary(),
  html_url := binary(),
  id := integer(),
  name := binary()
}.

-type github_review() :: #{
  body => 'undefined' | binary(),
  html_url := binary(),
  state := binary(),
  user := github_user()
}.

-type github_user() :: #{
  avatar_url := binary(),
  html_url := binary(),
  id := integer(),
  login := binary()
}.

-type github_webhook() :: #{
  action => 'undefined' | binary(),
  answer => 'undefined' | github_comment(),
  check_run => 'undefined' | github_check_run(),
  check_suite => 'undefined' | github_check_suite(),
  comment => 'undefined' | github_comment(),
  commits => 'undefined' | list(),
  compare => 'undefined' | binary(),
  discussion => 'undefined' | github_discussion(),
  forced => 'undefined' | boolean(),
  forkee => 'undefined' | github_repository(),
  head_commit => 'undefined' | github_commit(),
  issue => 'undefined' | github_issue(),
  member => 'undefined' | github_user(),
  pull_request => 'undefined' | github_issue(),
  ref => 'undefined' | binary(),
  ref_type => 'undefined' | binary(),
  release => 'undefined' | github_release(),
  repository => 'undefined' | github_repository(),
  review => 'undefined' | github_review(),
  sender := github_user()
}.

-type group_dm_invite_response() :: #{
  approximate_member_count => 'undefined' | integer(),
  channel => 'undefined' | invite_channel_response(),
  code := binary(),
  created_at => 'undefined' | binary(),
  expires_at => 'undefined' | binary(),
  inviter => 'undefined' | user_response(),
  max_age => 'undefined' | integer(),
  type => 'undefined' | integer()
}.

-type guild_audit_log_response() :: #{
  application_commands := list(),
  audit_log_entries := list(),
  auto_moderation_rules := list(),
  guild_scheduled_events := list(),
  integrations := list(),
  threads := list(),
  users := list(),
  webhooks := list()
}.

-type guild_ban_response() :: #{
  reason => 'undefined' | binary(),
  user := user_response()
}.

-type guild_channel_location() :: #{
  channel_id := snowflake_type(),
  guild_id := snowflake_type(),
  id := binary(),
  kind := binary()
}.

-type guild_channel_response() :: #{
  available_tags => 'undefined' | list(),
  bitrate => 'undefined' | integer(),
  default_auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  default_forum_layout => 'undefined' | forum_layout(),
  default_reaction_emoji => 'undefined' | default_reaction_emoji_response(),
  default_sort_order => 'undefined' | thread_sort_order(),
  default_thread_rate_limit_per_user => 'undefined' | integer(),
  flags := integer(),
  guild_id := snowflake_type(),
  hd_streaming_buyer_id => 'undefined' | snowflake_type(),
  hd_streaming_until => 'undefined' | binary(),
  id := snowflake_type(),
  last_message_id => 'undefined' | snowflake_type(),
  last_pin_timestamp => 'undefined' | binary(),
  name := binary(),
  nsfw => 'undefined' | boolean(),
  parent_id => 'undefined' | snowflake_type(),
  permission_overwrites => 'undefined' | list(),
  permissions => 'undefined' | binary(),
  position := integer(),
  rate_limit_per_user => 'undefined' | integer(),
  rtc_region => 'undefined' | binary(),
  topic => 'undefined' | binary(),
  type := integer(),
  user_limit => 'undefined' | integer(),
  video_quality_mode => 'undefined' | video_quality_modes()
}.

-type guild_create_request() :: #{
  afk_channel_id => 'undefined' | snowflake_type(),
  afk_timeout => 'undefined' | afk_timeouts(),
  channels => 'undefined' | list(),
  default_message_notifications => 'undefined' | user_notification_settings(),
  description => 'undefined' | binary(),
  explicit_content_filter => 'undefined' | guild_explicit_content_filter_types(),
  icon => 'undefined' | binary(),
  name := binary(),
  preferred_locale => 'undefined' | available_locales_enum(),
  region => 'undefined' | binary(),
  roles => 'undefined' | list(),
  system_channel_flags => 'undefined' | integer(),
  system_channel_id => 'undefined' | snowflake_type(),
  verification_level => 'undefined' | verification_levels()
}.

-type guild_home_settings_response() :: #{
  enabled := boolean(),
  guild_id := snowflake_type(),
  new_member_actions => 'undefined' | list(),
  resource_channels => 'undefined' | list(),
  welcome_message => 'undefined' | welcome_message_response()
}.

-type guild_incoming_webhook_response() :: #{
  application_id => 'undefined' | snowflake_type(),
  avatar => 'undefined' | binary(),
  channel_id => 'undefined' | snowflake_type(),
  guild_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  token => 'undefined' | binary(),
  type := integer(),
  url => 'undefined' | binary(),
  user => 'undefined' | user_response()
}.

-type guild_invite_response() :: #{
  approximate_member_count => 'undefined' | integer(),
  approximate_presence_count => 'undefined' | integer(),
  channel => 'undefined' | invite_channel_response(),
  code := binary(),
  created_at => 'undefined' | binary(),
  expires_at => 'undefined' | binary(),
  flags => 'undefined' | integer(),
  guild => 'undefined' | invite_guild_response(),
  guild_id => 'undefined' | snowflake_type(),
  guild_scheduled_event => 'undefined' | scheduled_event_response(),
  inviter => 'undefined' | user_response(),
  is_contact => 'undefined' | boolean(),
  is_nickname_changeable => 'undefined' | boolean(),
  max_age => 'undefined' | integer(),
  max_uses => 'undefined' | integer(),
  stage_instance => 'undefined' | invite_stage_instance_response(),
  target_application => 'undefined' | invite_application_response(),
  target_type => 'undefined' | invite_target_types(),
  target_user => 'undefined' | user_response(),
  temporary => 'undefined' | boolean(),
  type => 'undefined' | integer(),
  uses => 'undefined' | integer()
}.

-type guild_m_f_a_level_response() :: #{
  level := guild_m_f_a_level()
}.

-type guild_member_response() :: #{
  avatar => 'undefined' | binary(),
  avatar_decoration_data => 'undefined' | user_avatar_decoration_response(),
  banner => 'undefined' | binary(),
  communication_disabled_until => 'undefined' | binary(),
  deaf := boolean(),
  flags := integer(),
  joined_at := binary(),
  mute := boolean(),
  nick => 'undefined' | binary(),
  pending := boolean(),
  premium_since => 'undefined' | binary(),
  roles := list(),
  user := user_response()
}.

-type guild_onboarding_response() :: #{
  default_channel_ids := list(),
  enabled := boolean(),
  guild_id := snowflake_type(),
  prompts := list()
}.

-type guild_patch_request_partial() :: #{
  afk_channel_id => 'undefined' | snowflake_type(),
  afk_timeout => 'undefined' | afk_timeouts(),
  banner => 'undefined' | binary(),
  default_message_notifications => 'undefined' | user_notification_settings(),
  description => 'undefined' | binary(),
  discovery_splash => 'undefined' | binary(),
  explicit_content_filter => 'undefined' | guild_explicit_content_filter_types(),
  features => 'undefined' | list(),
  home_header => 'undefined' | binary(),
  icon => 'undefined' | binary(),
  name => 'undefined' | binary(),
  owner_id => 'undefined' | snowflake_type(),
  preferred_locale => 'undefined' | available_locales_enum(),
  premium_progress_bar_enabled => 'undefined' | boolean(),
  public_updates_channel_id => 'undefined' | snowflake_type(),
  region => 'undefined' | binary(),
  rules_channel_id => 'undefined' | snowflake_type(),
  safety_alerts_channel_id => 'undefined' | snowflake_type(),
  splash => 'undefined' | binary(),
  system_channel_flags => 'undefined' | integer(),
  system_channel_id => 'undefined' | snowflake_type(),
  verification_level => 'undefined' | verification_levels()
}.

-type guild_preview_response() :: #{
  approximate_member_count := integer(),
  approximate_presence_count := integer(),
  description => 'undefined' | binary(),
  discovery_splash => 'undefined' | binary(),
  emojis := list(),
  features := list(),
  home_header => 'undefined' | binary(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary(),
  splash => 'undefined' | binary(),
  stickers := list()
}.

-type guild_product_purchase_response() :: #{
  listing_id := snowflake_type(),
  product_name := binary()
}.

-type guild_prune_response() :: #{
  pruned => 'undefined' | integer()
}.

-type guild_response() :: #{
  system_channel_id => 'undefined' | snowflake_type(),
  system_channel_flags := integer(),
  icon => 'undefined' | binary(),
  vanity_url_code => 'undefined' | binary(),
  nsfw_level := guild_n_s_f_w_content_level(),
  max_members => 'undefined' | integer(),
  afk_timeout := afk_timeouts(),
  discovery_splash => 'undefined' | binary(),
  max_stage_video_channel_users => 'undefined' | integer(),
  premium_progress_bar_enabled := boolean(),
  afk_channel_id => 'undefined' | snowflake_type(),
  max_video_channel_users => 'undefined' | integer(),
  owner_id := snowflake_type(),
  public_updates_channel_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  description => 'undefined' | binary(),
  default_message_notifications := user_notification_settings(),
  safety_alerts_channel_id => 'undefined' | snowflake_type(),
  preferred_locale := available_locales_enum(),
  splash => 'undefined' | binary(),
  roles := list(),
  application_id => 'undefined' | snowflake_type(),
  widget_enabled := boolean(),
  premium_subscription_count := integer(),
  nsfw := boolean(),
  verification_level := verification_levels(),
  features := list(),
  explicit_content_filter := guild_explicit_content_filter_types(),
  widget_channel_id => 'undefined' | snowflake_type(),
  mfa_level := guild_m_f_a_level(),
  banner => 'undefined' | binary(),
  stickers := list(),
  emojis := list(),
  rules_channel_id => 'undefined' | snowflake_type(),
  home_header => 'undefined' | binary(),
  premium_tier := premium_guild_tiers(),
  max_presences => 'undefined' | integer(),
  region := binary()
}.

-type guild_role_response() :: #{
  color := integer(),
  description => 'undefined' | binary(),
  hoist := boolean(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  managed := boolean(),
  mentionable := boolean(),
  name := binary(),
  permissions := binary(),
  position := integer(),
  tags => 'undefined' | guild_role_tags_response(),
  unicode_emoji => 'undefined' | binary()
}.

-type guild_role_tags_response() :: #{
  available_for_purchase => 'undefined' | any(),
  bot_id => 'undefined' | snowflake_type(),
  guild_connections => 'undefined' | any(),
  integration_id => 'undefined' | snowflake_type(),
  premium_subscriber => 'undefined' | any(),
  subscription_listing_id => 'undefined' | snowflake_type()
}.

-type guild_sticker_response() :: #{
  available := boolean(),
  description => 'undefined' | binary(),
  format_type => 'undefined' | sticker_format_types(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  tags := binary(),
  type := integer(),
  user => 'undefined' | user_response()
}.

-type guild_subscription_integration_response() :: #{
  account => 'undefined' | account_response(),
  enabled => 'undefined' | boolean(),
  id := snowflake_type(),
  name => 'undefined' | binary(),
  type := binary()
}.

-type guild_template_channel_response() :: #{
  available_tags => 'undefined' | list(),
  bitrate := integer(),
  default_auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  default_forum_layout => 'undefined' | forum_layout(),
  default_reaction_emoji => 'undefined' | default_reaction_emoji_response(),
  default_sort_order => 'undefined' | thread_sort_order(),
  default_thread_rate_limit_per_user => 'undefined' | integer(),
  icon_emoji => 'undefined' | icon_emoji_response(),
  id => 'undefined' | integer(),
  name => 'undefined' | binary(),
  nsfw := boolean(),
  parent_id => 'undefined' | snowflake_type(),
  permission_overwrites := list(),
  position => 'undefined' | integer(),
  rate_limit_per_user := integer(),
  template := binary(),
  theme_color => 'undefined' | integer(),
  topic => 'undefined' | binary(),
  type := integer(),
  user_limit := integer()
}.

-type guild_template_channel_tags() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  moderated => 'undefined' | boolean(),
  name := binary()
}.

-type guild_template_response() :: #{
  code := binary(),
  created_at := binary(),
  creator => 'undefined' | user_response(),
  creator_id := snowflake_type(),
  description => 'undefined' | binary(),
  is_dirty => 'undefined' | boolean(),
  name := binary(),
  serialized_source_guild := guild_template_snapshot_response(),
  source_guild_id := snowflake_type(),
  updated_at := binary(),
  usage_count := integer()
}.

-type guild_template_role_response() :: #{
  color := integer(),
  hoist := boolean(),
  icon => 'undefined' | binary(),
  id := integer(),
  mentionable := boolean(),
  name := binary(),
  permissions := binary(),
  unicode_emoji => 'undefined' | binary()
}.

-type guild_template_snapshot_response() :: #{
  afk_channel_id => 'undefined' | snowflake_type(),
  afk_timeout := afk_timeouts(),
  channels := list(),
  default_message_notifications := user_notification_settings(),
  description => 'undefined' | binary(),
  explicit_content_filter := guild_explicit_content_filter_types(),
  name := binary(),
  preferred_locale := available_locales_enum(),
  region => 'undefined' | binary(),
  roles := list(),
  system_channel_flags := integer(),
  system_channel_id => 'undefined' | snowflake_type(),
  verification_level := verification_levels()
}.

-type guild_welcome_channel() :: #{
  channel_id := snowflake_type(),
  description := binary(),
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary()
}.

-type guild_welcome_screen_channel_response() :: #{
  channel_id := snowflake_type(),
  description := binary(),
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary()
}.

-type guild_welcome_screen_response() :: #{
  description => 'undefined' | binary(),
  welcome_channels := list()
}.

-type guild_with_counts_response() :: #{
  approximate_presence_count => 'undefined' | integer(),
  system_channel_id => 'undefined' | snowflake_type(),
  system_channel_flags := integer(),
  icon => 'undefined' | binary(),
  vanity_url_code => 'undefined' | binary(),
  nsfw_level := guild_n_s_f_w_content_level(),
  max_members => 'undefined' | integer(),
  afk_timeout := afk_timeouts(),
  discovery_splash => 'undefined' | binary(),
  max_stage_video_channel_users => 'undefined' | integer(),
  premium_progress_bar_enabled := boolean(),
  afk_channel_id => 'undefined' | snowflake_type(),
  max_video_channel_users => 'undefined' | integer(),
  owner_id := snowflake_type(),
  public_updates_channel_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  description => 'undefined' | binary(),
  default_message_notifications := user_notification_settings(),
  safety_alerts_channel_id => 'undefined' | snowflake_type(),
  approximate_member_count => 'undefined' | integer(),
  preferred_locale := available_locales_enum(),
  splash => 'undefined' | binary(),
  roles := list(),
  application_id => 'undefined' | snowflake_type(),
  widget_enabled := boolean(),
  premium_subscription_count := integer(),
  nsfw := boolean(),
  verification_level := verification_levels(),
  features := list(),
  explicit_content_filter := guild_explicit_content_filter_types(),
  widget_channel_id => 'undefined' | snowflake_type(),
  mfa_level := guild_m_f_a_level(),
  banner => 'undefined' | binary(),
  stickers := list(),
  emojis := list(),
  rules_channel_id => 'undefined' | snowflake_type(),
  home_header => 'undefined' | binary(),
  premium_tier := premium_guild_tiers(),
  max_presences => 'undefined' | integer(),
  region := binary()
}.

-type incoming_webhook_interaction_request() :: #{
  allowed_mentions => 'undefined' | message_allowed_mentions_request(),
  attachments => 'undefined' | list(),
  components => 'undefined' | list(),
  content => 'undefined' | binary(),
  embeds => 'undefined' | list(),
  flags => 'undefined' | integer(),
  poll => 'undefined' | poll_create_request(),
  tts => 'undefined' | boolean()
}.

-type incoming_webhook_request_partial() :: #{
  allowed_mentions => 'undefined' | message_allowed_mentions_request(),
  applied_tags => 'undefined' | list(),
  attachments => 'undefined' | list(),
  avatar_url => 'undefined' | binary(),
  components => 'undefined' | list(),
  content => 'undefined' | binary(),
  embeds => 'undefined' | list(),
  flags => 'undefined' | integer(),
  poll => 'undefined' | poll_create_request(),
  thread_name => 'undefined' | binary(),
  tts => 'undefined' | boolean(),
  username => 'undefined' | binary()
}.

-type incoming_webhook_update_for_interaction_callback_request_partial() :: #{
  allowed_mentions => 'undefined' | message_allowed_mentions_request(),
  attachments => 'undefined' | list(),
  components => 'undefined' | list(),
  content => 'undefined' | binary(),
  embeds => 'undefined' | list(),
  flags => 'undefined' | integer()
}.

-type incoming_webhook_update_request_partial() :: #{
  allowed_mentions => 'undefined' | message_allowed_mentions_request(),
  attachments => 'undefined' | list(),
  components => 'undefined' | list(),
  content => 'undefined' | binary(),
  embeds => 'undefined' | list(),
  flags => 'undefined' | integer(),
  poll => 'undefined' | poll_create_request()
}.

-type inner_errors() :: #{
  errors := list()
}.

-type integration_application_response() :: #{
  bot => 'undefined' | user_response(),
  cover_image => 'undefined' | binary(),
  description := binary(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary(),
  primary_sku_id => 'undefined' | snowflake_type(),
  type => 'undefined' | application_types()
}.

-type interaction_application_command_autocomplete_callback_integer_data() :: #{
  choices => 'undefined' | list()
}.

-type interaction_application_command_autocomplete_callback_number_data() :: #{
  choices => 'undefined' | list()
}.

-type interaction_application_command_autocomplete_callback_string_data() :: #{
  choices => 'undefined' | list()
}.

-type interaction_callback_response() :: #{
  interaction := interaction_response(),
  resource => 'undefined' | launch_activity_interaction_callback_response()
}.

-type interaction_response() :: #{
  channel_id => 'undefined' | snowflake_type(),
  guild_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  response_message_ephemeral => 'undefined' | boolean(),
  response_message_id => 'undefined' | snowflake_type(),
  response_message_loading => 'undefined' | boolean(),
  type := interaction_types()
}.

-type invite_application_response() :: #{
  bot => 'undefined' | user_response(),
  bot_public => 'undefined' | boolean(),
  bot_require_code_grant => 'undefined' | boolean(),
  cover_image => 'undefined' | binary(),
  custom_install_url => 'undefined' | binary(),
  description := binary(),
  flags := integer(),
  guild_id => 'undefined' | snowflake_type(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  install_params => 'undefined' | application_oauth2_install_params_response(),
  integration_types_config => 'undefined' | any(),
  max_participants => 'undefined' | integer(),
  name := binary(),
  primary_sku_id => 'undefined' | snowflake_type(),
  privacy_policy_url => 'undefined' | binary(),
  rpc_origins => 'undefined' | list(),
  slug => 'undefined' | binary(),
  tags => 'undefined' | list(),
  terms_of_service_url => 'undefined' | binary(),
  type => 'undefined' | application_types(),
  verify_key := binary()
}.

-type invite_channel_recipient_response() :: #{
  username := binary()
}.

-type invite_channel_response() :: #{
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name => 'undefined' | binary(),
  recipients => 'undefined' | list(),
  type := channel_types()
}.

-type invite_guild_response() :: #{
  banner => 'undefined' | binary(),
  description => 'undefined' | binary(),
  features := list(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary(),
  nsfw => 'undefined' | boolean(),
  nsfw_level => 'undefined' | guild_n_s_f_w_content_level(),
  premium_subscription_count => 'undefined' | integer(),
  splash => 'undefined' | binary(),
  vanity_url_code => 'undefined' | binary(),
  verification_level => 'undefined' | verification_levels()
}.

-type invite_stage_instance_response() :: #{
  members => 'undefined' | list(),
  participant_count => 'undefined' | integer(),
  speaker_count => 'undefined' | integer(),
  topic := binary()
}.

-type keyword_rule_response() :: #{
  actions := list(),
  creator_id := snowflake_type(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  trigger_metadata := keyword_trigger_metadata_response(),
  trigger_type := integer()
}.

-type keyword_trigger_metadata() :: #{
  allow_list => 'undefined' | list(),
  keyword_filter => 'undefined' | list(),
  regex_patterns => 'undefined' | list()
}.

-type keyword_trigger_metadata_response() :: #{
  allow_list := list(),
  keyword_filter := list(),
  regex_patterns := list()
}.

-type keyword_upsert_request() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name := binary(),
  trigger_metadata => 'undefined' | keyword_trigger_metadata(),
  trigger_type := integer()
}.

-type keyword_upsert_request_partial() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type => 'undefined' | automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name => 'undefined' | binary(),
  trigger_metadata => 'undefined' | keyword_trigger_metadata(),
  trigger_type => 'undefined' | integer()
}.

-type launch_activity_interaction_callback_request() :: #{
  type := integer()
}.

-type launch_activity_interaction_callback_response() :: #{
  type := integer()
}.

-type list_application_emojis_response() :: #{
  items := list()
}.

-type list_guild_soundboard_sounds_response() :: #{
  items := list()
}.

-type mention_spam_rule_response() :: #{
  actions := list(),
  creator_id := snowflake_type(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  trigger_metadata := mention_spam_trigger_metadata_response(),
  trigger_type := integer()
}.

-type mention_spam_trigger_metadata() :: #{
  mention_raid_protection_enabled => 'undefined' | boolean(),
  mention_total_limit := integer()
}.

-type mention_spam_trigger_metadata_response() :: #{
  mention_raid_protection_enabled => 'undefined' | boolean(),
  mention_total_limit := integer()
}.

-type mention_spam_upsert_request() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name := binary(),
  trigger_metadata => 'undefined' | mention_spam_trigger_metadata(),
  trigger_type := integer()
}.

-type mention_spam_upsert_request_partial() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type => 'undefined' | automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name => 'undefined' | binary(),
  trigger_metadata => 'undefined' | mention_spam_trigger_metadata(),
  trigger_type => 'undefined' | integer()
}.

-type mentionable_select_component_for_message_request() :: #{
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type mentionable_select_component_response() :: #{
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  id := integer(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type message_allowed_mentions_request() :: #{
  parse => 'undefined' | list(),
  replied_user => 'undefined' | boolean(),
  roles => 'undefined' | list(),
  users => 'undefined' | list()
}.

-type message_attachment_request() :: #{
  description => 'undefined' | binary(),
  duration_secs => 'undefined' | float(),
  filename => 'undefined' | binary(),
  id := snowflake_type(),
  is_remix => 'undefined' | boolean(),
  title => 'undefined' | binary(),
  waveform => 'undefined' | binary()
}.

-type message_attachment_response() :: #{
  application => 'undefined' | application_response(),
  clip_created_at => 'undefined' | binary(),
  clip_participants => 'undefined' | list(),
  content_type => 'undefined' | binary(),
  description => 'undefined' | binary(),
  duration_secs => 'undefined' | float(),
  ephemeral => 'undefined' | boolean(),
  filename := binary(),
  height => 'undefined' | integer(),
  id := snowflake_type(),
  proxy_url := binary(),
  size := integer(),
  title => 'undefined' | binary(),
  url := binary(),
  waveform => 'undefined' | binary(),
  width => 'undefined' | integer()
}.

-type message_call_response() :: #{
  ended_timestamp => 'undefined' | binary(),
  participants := list()
}.

-type message_component_interaction_metadata_response() :: #{
  authorizing_integration_owners := any(),
  id := snowflake_type(),
  interacted_message_id := snowflake_type(),
  original_response_message_id => 'undefined' | snowflake_type(),
  type := integer(),
  user => 'undefined' | user_response()
}.

-type message_create_request() :: #{
  allowed_mentions => 'undefined' | message_allowed_mentions_request(),
  attachments => 'undefined' | list(),
  components => 'undefined' | list(),
  confetti_potion => 'undefined' | confetti_potion_create_request(),
  content => 'undefined' | binary(),
  embeds => 'undefined' | list(),
  enforce_nonce => 'undefined' | boolean(),
  flags => 'undefined' | integer(),
  message_reference => 'undefined' | message_reference_request(),
  nonce => 'undefined' | binary(),
  poll => 'undefined' | poll_create_request(),
  sticker_ids => 'undefined' | list(),
  tts => 'undefined' | boolean()
}.

-type message_edit_request_partial() :: #{
  allowed_mentions => 'undefined' | message_allowed_mentions_request(),
  attachments => 'undefined' | list(),
  components => 'undefined' | list(),
  content => 'undefined' | binary(),
  embeds => 'undefined' | list(),
  flags => 'undefined' | integer(),
  sticker_ids => 'undefined' | list()
}.

-type message_embed_author_response() :: #{
  icon_url => 'undefined' | binary(),
  name := binary(),
  proxy_icon_url => 'undefined' | binary(),
  url => 'undefined' | binary()
}.

-type message_embed_field_response() :: #{
  inline := boolean(),
  name := binary(),
  value := binary()
}.

-type message_embed_footer_response() :: #{
  icon_url => 'undefined' | binary(),
  proxy_icon_url => 'undefined' | binary(),
  text := binary()
}.

-type message_embed_image_response() :: #{
  flags => 'undefined' | u_int32_type(),
  height => 'undefined' | u_int32_type(),
  placeholder => 'undefined' | binary(),
  placeholder_version => 'undefined' | u_int32_type(),
  proxy_url => 'undefined' | binary(),
  url => 'undefined' | binary(),
  width => 'undefined' | u_int32_type()
}.

-type message_embed_provider_response() :: #{
  name := binary(),
  url => 'undefined' | binary()
}.

-type message_embed_response() :: #{
  author => 'undefined' | message_embed_author_response(),
  color => 'undefined' | integer(),
  description => 'undefined' | binary(),
  fields => 'undefined' | list(),
  footer => 'undefined' | message_embed_footer_response(),
  image => 'undefined' | message_embed_image_response(),
  provider => 'undefined' | message_embed_provider_response(),
  thumbnail => 'undefined' | message_embed_image_response(),
  timestamp => 'undefined' | binary(),
  title => 'undefined' | binary(),
  type := binary(),
  url => 'undefined' | binary(),
  video => 'undefined' | message_embed_video_response()
}.

-type message_embed_video_response() :: #{
  flags => 'undefined' | u_int32_type(),
  height => 'undefined' | u_int32_type(),
  placeholder => 'undefined' | binary(),
  placeholder_version => 'undefined' | u_int32_type(),
  proxy_url => 'undefined' | binary(),
  url => 'undefined' | binary(),
  width => 'undefined' | u_int32_type()
}.

-type message_interaction_response() :: #{
  id := snowflake_type(),
  name := binary(),
  name_localized => 'undefined' | binary(),
  type := interaction_types(),
  user => 'undefined' | user_response()
}.

-type message_mention_channel_response() :: #{
  guild_id := snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  type := channel_types()
}.

-type message_reaction_count_details_response() :: #{
  burst := integer(),
  normal := integer()
}.

-type message_reaction_emoji_response() :: #{
  animated => 'undefined' | boolean(),
  id => 'undefined' | snowflake_type(),
  name => 'undefined' | binary()
}.

-type message_reaction_response() :: #{
  burst_colors := list(),
  count := integer(),
  count_details := message_reaction_count_details_response(),
  emoji := message_reaction_emoji_response(),
  me := boolean(),
  me_burst := boolean()
}.

-type message_reference_request() :: #{
  channel_id => 'undefined' | snowflake_type(),
  fail_if_not_exists => 'undefined' | boolean(),
  guild_id => 'undefined' | snowflake_type(),
  message_id := snowflake_type(),
  type => 'undefined' | message_reference_type()
}.

-type message_reference_response() :: #{
  channel_id := snowflake_type(),
  guild_id => 'undefined' | snowflake_type(),
  message_id => 'undefined' | snowflake_type(),
  type => 'undefined' | message_reference_type()
}.

-type message_response() :: #{
  reactions => 'undefined' | list(),
  interaction_metadata => 'undefined' | message_component_interaction_metadata_response(),
  embeds := list(),
  activity => 'undefined' | message_activity_response(),
  interaction => 'undefined' | message_interaction_response(),
  referenced_message => 'undefined' | basic_message_response(),
  nonce => 'undefined' | binary(),
  mention_everyone := boolean(),
  timestamp := binary(),
  pinned := boolean(),
  mention_roles := list(),
  components := list(),
  resolved => 'undefined' | resolved_objects_response(),
  mention_channels => 'undefined' | list(),
  author := user_response(),
  edited_timestamp => 'undefined' | binary(),
  tts := boolean(),
  mentions := list(),
  channel_id := snowflake_type(),
  purchase_notification => 'undefined' | purchase_notification_response(),
  call => 'undefined' | message_call_response(),
  message_snapshots => 'undefined' | list(),
  id := snowflake_type(),
  webhook_id => 'undefined' | snowflake_type(),
  poll => 'undefined' | poll_response(),
  flags := integer(),
  application_id => 'undefined' | snowflake_type(),
  content := binary(),
  application => 'undefined' | basic_application_response(),
  message_reference => 'undefined' | message_reference_response(),
  position => 'undefined' | integer(),
  type := message_type(),
  attachments := list(),
  thread => 'undefined' | thread_response(),
  stickers => 'undefined' | list(),
  role_subscription_data => 'undefined' | message_role_subscription_data_response(),
  sticker_items => 'undefined' | list()
}.

-type message_role_subscription_data_response() :: #{
  is_renewal := boolean(),
  role_subscription_listing_id := snowflake_type(),
  tier_name := binary(),
  total_months_subscribed := integer()
}.

-type message_snapshot_response() :: #{
  message => 'undefined' | minimal_content_message_response()
}.

-type message_sticker_item_response() :: #{
  format_type := sticker_format_types(),
  id := snowflake_type(),
  name := binary()
}.

-type minimal_content_message_response() :: #{
  attachments := list(),
  components := list(),
  content := binary(),
  edited_timestamp => 'undefined' | binary(),
  embeds := list(),
  flags := integer(),
  mention_roles := list(),
  mentions := list(),
  resolved => 'undefined' | resolved_objects_response(),
  sticker_items => 'undefined' | list(),
  stickers => 'undefined' | list(),
  timestamp := binary(),
  type := message_type()
}.

-type ml_spam_rule_response() :: #{
  actions := list(),
  creator_id := snowflake_type(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  trigger_metadata := ml_spam_trigger_metadata_response(),
  trigger_type := integer()
}.

-type ml_spam_upsert_request() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name := binary(),
  trigger_metadata => 'undefined' | ml_spam_trigger_metadata(),
  trigger_type := integer()
}.

-type ml_spam_upsert_request_partial() :: #{
  actions => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  event_type => 'undefined' | automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  name => 'undefined' | binary(),
  trigger_metadata => 'undefined' | ml_spam_trigger_metadata(),
  trigger_type => 'undefined' | integer()
}.

-type modal_interaction_callback_request() :: #{
  data := modal_interaction_callback_request_data(),
  type := integer()
}.

-type modal_interaction_callback_request_data() :: #{
  components := list(),
  custom_id := binary(),
  title := binary()
}.

-type modal_submit_interaction_metadata_response() :: #{
  authorizing_integration_owners := any(),
  id := snowflake_type(),
  original_response_message_id => 'undefined' | snowflake_type(),
  triggering_interaction_metadata := message_component_interaction_metadata_response(),
  type := integer(),
  user => 'undefined' | user_response()
}.

-type my_guild_response() :: #{
  approximate_member_count => 'undefined' | integer(),
  approximate_presence_count => 'undefined' | integer(),
  banner => 'undefined' | binary(),
  features := list(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary(),
  owner := boolean(),
  permissions := binary()
}.

-type new_member_action_response() :: #{
  action_type := new_member_action_type(),
  channel_id := snowflake_type(),
  description := binary(),
  emoji => 'undefined' | settings_emoji_response(),
  icon => 'undefined' | binary(),
  title := binary()
}.

-type oauth2_get_authorization_response() :: #{
  application := application_response(),
  expires := binary(),
  scopes := list(),
  user => 'undefined' | user_response()
}.

-type oauth2_get_keys() :: #{
  keys := list()
}.

-type oauth2_get_open_id_connect_user_info_response() :: #{
  email => 'undefined' | binary(),
  email_verified => 'undefined' | boolean(),
  locale => 'undefined' | binary(),
  nickname => 'undefined' | binary(),
  picture => 'undefined' | binary(),
  preferred_username => 'undefined' | binary(),
  sub := binary()
}.

-type oauth2_key() :: #{
  alg := binary(),
  e := binary(),
  kid := binary(),
  kty := binary(),
  n := binary(),
  use := binary()
}.

-type onboarding_prompt_option_request() :: #{
  channel_ids => 'undefined' | list(),
  description => 'undefined' | binary(),
  emoji_animated => 'undefined' | boolean(),
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  id => 'undefined' | snowflake_type(),
  role_ids => 'undefined' | list(),
  title := binary()
}.

-type onboarding_prompt_option_response() :: #{
  channel_ids := list(),
  description := binary(),
  emoji := settings_emoji_response(),
  id := snowflake_type(),
  role_ids := list(),
  title := binary()
}.

-type onboarding_prompt_response() :: #{
  id := snowflake_type(),
  in_onboarding := boolean(),
  options := list(),
  required := boolean(),
  single_select := boolean(),
  title := binary(),
  type := onboarding_prompt_type()
}.

-type partial_discord_integration_response() :: #{
  account => 'undefined' | account_response(),
  application_id := snowflake_type(),
  id := snowflake_type(),
  name => 'undefined' | binary(),
  type := binary()
}.

-type partial_external_connection_integration_response() :: #{
  account => 'undefined' | account_response(),
  id := snowflake_type(),
  name => 'undefined' | binary(),
  type := binary()
}.

-type partial_guild_subscription_integration_response() :: #{
  account => 'undefined' | account_response(),
  id := snowflake_type(),
  name => 'undefined' | binary(),
  type := binary()
}.

-type poll_answer_create_request() :: #{
  poll_media := poll_media_create_request()
}.

-type poll_answer_details_response() :: #{
  users => 'undefined' | list()
}.

-type poll_answer_response() :: #{
  answer_id := integer(),
  poll_media := poll_media_response()
}.

-type poll_create_request() :: #{
  allow_multiselect => 'undefined' | boolean(),
  answers := list(),
  duration => 'undefined' | integer(),
  layout_type => 'undefined' | poll_layout_types(),
  question := poll_media()
}.

-type poll_emoji() :: #{
  animated => 'undefined' | boolean(),
  id => 'undefined' | snowflake_type(),
  name => 'undefined' | binary()
}.

-type poll_emoji_create_request() :: #{
  animated => 'undefined' | boolean(),
  id => 'undefined' | snowflake_type(),
  name => 'undefined' | binary()
}.

-type poll_media() :: #{
  emoji => 'undefined' | poll_emoji(),
  text => 'undefined' | binary()
}.

-type poll_media_create_request() :: #{
  emoji => 'undefined' | poll_emoji_create_request(),
  text => 'undefined' | binary()
}.

-type poll_media_response() :: #{
  emoji => 'undefined' | message_reaction_emoji_response(),
  text => 'undefined' | binary()
}.

-type poll_response() :: #{
  allow_multiselect := boolean(),
  answers := list(),
  expiry := binary(),
  layout_type := poll_layout_types(),
  question := poll_media_response(),
  results := poll_results_response()
}.

-type poll_results_entry_response() :: #{
  count := integer(),
  id := integer(),
  me_voted => 'undefined' | boolean()
}.

-type poll_results_response() :: #{
  answer_counts => 'undefined' | list(),
  is_finalized := boolean()
}.

-type pong_interaction_callback_request() :: #{
  type := integer()
}.

-type private_application_response() :: #{
  approximate_guild_count => 'undefined' | integer(),
  approximate_user_install_count := integer(),
  bot => 'undefined' | user_response(),
  bot_public => 'undefined' | boolean(),
  bot_require_code_grant => 'undefined' | boolean(),
  cover_image => 'undefined' | binary(),
  custom_install_url => 'undefined' | binary(),
  description := binary(),
  explicit_content_filter := application_explicit_content_filter_types(),
  flags := integer(),
  guild_id => 'undefined' | snowflake_type(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  install_params => 'undefined' | application_oauth2_install_params_response(),
  integration_types_config => 'undefined' | any(),
  interactions_endpoint_url => 'undefined' | binary(),
  max_participants => 'undefined' | integer(),
  name := binary(),
  owner := user_response(),
  primary_sku_id => 'undefined' | snowflake_type(),
  privacy_policy_url => 'undefined' | binary(),
  redirect_uris := list(),
  role_connections_verification_url => 'undefined' | binary(),
  rpc_origins => 'undefined' | list(),
  slug => 'undefined' | binary(),
  tags => 'undefined' | list(),
  team => 'undefined' | team_response(),
  terms_of_service_url => 'undefined' | binary(),
  type => 'undefined' | application_types(),
  verify_key := binary()
}.

-type private_channel_location() :: #{
  channel_id := snowflake_type(),
  id := binary(),
  kind := binary()
}.

-type private_channel_request_partial() :: #{
  icon => 'undefined' | binary(),
  name => 'undefined' | binary()
}.

-type private_channel_response() :: #{
  flags := integer(),
  id := snowflake_type(),
  last_message_id => 'undefined' | snowflake_type(),
  last_pin_timestamp => 'undefined' | binary(),
  recipients := list(),
  type := integer()
}.

-type private_group_channel_response() :: #{
  application_id => 'undefined' | snowflake_type(),
  flags := integer(),
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  last_message_id => 'undefined' | snowflake_type(),
  last_pin_timestamp => 'undefined' | binary(),
  managed => 'undefined' | boolean(),
  name => 'undefined' | binary(),
  owner_id => 'undefined' | snowflake_type(),
  recipients := list(),
  type := integer()
}.

-type private_guild_member_response() :: #{
  avatar => 'undefined' | binary(),
  avatar_decoration_data => 'undefined' | user_avatar_decoration_response(),
  banner => 'undefined' | binary(),
  communication_disabled_until => 'undefined' | binary(),
  deaf := boolean(),
  flags := integer(),
  joined_at := binary(),
  mute := boolean(),
  nick => 'undefined' | binary(),
  pending := boolean(),
  premium_since => 'undefined' | binary(),
  roles := list(),
  user := user_response()
}.

-type purchase_notification_response() :: #{
  guild_product_purchase => 'undefined' | guild_product_purchase_response(),
  type := purchase_type()
}.

-type quarantine_user_action() :: #{
  metadata => 'undefined' | quarantine_user_action_metadata(),
  type := integer()
}.

-type quarantine_user_action_response() :: #{
  metadata := quarantine_user_action_metadata_response(),
  type := integer()
}.

-type resolved_objects_response() :: #{
  channels := any(),
  members := any(),
  roles := any(),
  users := any()
}.

-type resource_channel_response() :: #{
  channel_id := snowflake_type(),
  description := binary(),
  emoji => 'undefined' | settings_emoji_response(),
  icon => 'undefined' | binary(),
  title := binary()
}.

-type rich_embed() :: #{
  author => 'undefined' | rich_embed_author(),
  color => 'undefined' | integer(),
  description => 'undefined' | binary(),
  fields => 'undefined' | list(),
  footer => 'undefined' | rich_embed_footer(),
  image => 'undefined' | rich_embed_image(),
  provider => 'undefined' | rich_embed_provider(),
  thumbnail => 'undefined' | rich_embed_thumbnail(),
  timestamp => 'undefined' | binary(),
  title => 'undefined' | binary(),
  type => 'undefined' | binary(),
  url => 'undefined' | binary(),
  video => 'undefined' | rich_embed_video()
}.

-type rich_embed_author() :: #{
  icon_url => 'undefined' | binary(),
  name => 'undefined' | binary(),
  url => 'undefined' | binary()
}.

-type rich_embed_field() :: #{
  inline => 'undefined' | boolean(),
  name := binary(),
  value := binary()
}.

-type rich_embed_footer() :: #{
  icon_url => 'undefined' | binary(),
  text => 'undefined' | binary()
}.

-type rich_embed_image() :: #{
  height => 'undefined' | integer(),
  is_animated => 'undefined' | boolean(),
  placeholder => 'undefined' | binary(),
  placeholder_version => 'undefined' | integer(),
  url => 'undefined' | binary(),
  width => 'undefined' | integer()
}.

-type rich_embed_provider() :: #{
  name => 'undefined' | binary(),
  url => 'undefined' | binary()
}.

-type rich_embed_thumbnail() :: #{
  height => 'undefined' | integer(),
  is_animated => 'undefined' | boolean(),
  placeholder => 'undefined' | binary(),
  placeholder_version => 'undefined' | integer(),
  url => 'undefined' | binary(),
  width => 'undefined' | integer()
}.

-type rich_embed_video() :: #{
  height => 'undefined' | integer(),
  is_animated => 'undefined' | boolean(),
  placeholder => 'undefined' | binary(),
  placeholder_version => 'undefined' | integer(),
  url => 'undefined' | binary(),
  width => 'undefined' | integer()
}.

-type role_select_component_for_message_request() :: #{
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type role_select_component_response() :: #{
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  id := integer(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type role_select_default_value() :: #{
  id := snowflake_type(),
  type := binary()
}.

-type role_select_default_value_response() :: #{
  id := snowflake_type(),
  type := binary()
}.

-type scheduled_event_response() :: #{
  channel_id => 'undefined' | snowflake_type(),
  creator => 'undefined' | user_response(),
  creator_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_id => 'undefined' | snowflake_type(),
  entity_type := guild_scheduled_event_entity_types(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  image => 'undefined' | binary(),
  name := binary(),
  privacy_level := guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time := binary(),
  status := guild_scheduled_event_statuses(),
  user_count => 'undefined' | integer(),
  user_rsvp => 'undefined' | scheduled_event_user_response()
}.

-type scheduled_event_user_response() :: #{
  guild_scheduled_event_id := snowflake_type(),
  member => 'undefined' | guild_member_response(),
  user => 'undefined' | user_response(),
  user_id := snowflake_type()
}.

-type settings_emoji_response() :: #{
  animated => 'undefined' | boolean(),
  id => 'undefined' | snowflake_type(),
  name => 'undefined' | binary()
}.

-type slack_webhook() :: #{
  attachments => 'undefined' | list(),
  icon_url => 'undefined' | binary(),
  text => 'undefined' | binary(),
  username => 'undefined' | binary()
}.

-type soundboard_create_request() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  name := binary(),
  sound := binary(),
  volume => 'undefined' | float()
}.

-type soundboard_patch_request_partial() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  name => 'undefined' | binary(),
  volume => 'undefined' | float()
}.

-type soundboard_sound_response() :: #{
  available := boolean(),
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  guild_id => 'undefined' | snowflake_type(),
  name := binary(),
  sound_id := snowflake_type(),
  user => 'undefined' | user_response(),
  volume := float()
}.

-type soundboard_sound_send_request() :: #{
  sound_id := snowflake_type(),
  source_guild_id => 'undefined' | snowflake_type()
}.

-type spam_link_rule_response() :: #{
  actions := list(),
  creator_id := snowflake_type(),
  enabled => 'undefined' | boolean(),
  event_type := automod_event_type(),
  exempt_channels => 'undefined' | list(),
  exempt_roles => 'undefined' | list(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  name := binary(),
  trigger_metadata := spam_link_trigger_metadata_response(),
  trigger_type := integer()
}.

-type stage_instance_response() :: #{
  channel_id := snowflake_type(),
  discoverable_disabled => 'undefined' | boolean(),
  guild_id := snowflake_type(),
  guild_scheduled_event_id => 'undefined' | snowflake_type(),
  id := snowflake_type(),
  privacy_level := stage_instances_privacy_levels(),
  topic := binary()
}.

-type stage_scheduled_event_create_request() :: #{
  channel_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_metadata => 'undefined' | entity_metadata_stage_instance(),
  entity_type := integer(),
  image => 'undefined' | binary(),
  name := binary(),
  privacy_level := guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time := binary()
}.

-type stage_scheduled_event_patch_request_partial() :: #{
  channel_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_metadata => 'undefined' | entity_metadata_stage_instance(),
  entity_type => 'undefined' | integer(),
  image => 'undefined' | binary(),
  name => 'undefined' | binary(),
  privacy_level => 'undefined' | guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time => 'undefined' | binary(),
  status => 'undefined' | guild_scheduled_event_statuses()
}.

-type stage_scheduled_event_response() :: #{
  channel_id => 'undefined' | snowflake_type(),
  creator => 'undefined' | user_response(),
  creator_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_id => 'undefined' | snowflake_type(),
  entity_metadata => 'undefined' | entity_metadata_stage_instance_response(),
  entity_type := integer(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  image => 'undefined' | binary(),
  name := binary(),
  privacy_level := guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time := binary(),
  status := guild_scheduled_event_statuses(),
  user_count => 'undefined' | integer(),
  user_rsvp => 'undefined' | scheduled_event_user_response()
}.

-type standard_sticker_response() :: #{
  description => 'undefined' | binary(),
  format_type => 'undefined' | sticker_format_types(),
  id := snowflake_type(),
  name := binary(),
  pack_id := snowflake_type(),
  sort_value := integer(),
  tags := binary(),
  type := integer()
}.

-type sticker_pack_collection_response() :: #{
  sticker_packs := list()
}.

-type sticker_pack_response() :: #{
  banner_asset_id => 'undefined' | snowflake_type(),
  cover_sticker_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary(),
  sku_id := snowflake_type(),
  stickers := list()
}.

-type string_select_component_for_message_request() :: #{
  custom_id := binary(),
  disabled => 'undefined' | boolean(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  options := list(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type string_select_component_response() :: #{
  custom_id := binary(),
  disabled => 'undefined' | boolean(),
  id := integer(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  options := list(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type string_select_option_for_message_request() :: #{
  default => 'undefined' | boolean(),
  description => 'undefined' | binary(),
  emoji => 'undefined' | component_emoji_for_message_request(),
  label := binary(),
  value := binary()
}.

-type string_select_option_response() :: #{
  default => 'undefined' | boolean(),
  description => 'undefined' | binary(),
  emoji => 'undefined' | component_emoji_response(),
  label := binary(),
  value := binary()
}.

-type team_member_response() :: #{
  membership_state := team_membership_states(),
  team_id := snowflake_type(),
  user := user_response()
}.

-type team_response() :: #{
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  members := list(),
  name := binary(),
  owner_user_id := snowflake_type()
}.

-type text_input_component_for_modal_request() :: #{
  custom_id := binary(),
  label := binary(),
  max_length => 'undefined' | integer(),
  min_length => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  style := text_input_style_types(),
  type := integer(),
  value => 'undefined' | binary()
}.

-type text_input_component_response() :: #{
  custom_id := binary(),
  id := integer(),
  label => 'undefined' | binary(),
  max_length => 'undefined' | integer(),
  min_length => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  required => 'undefined' | boolean(),
  style := text_input_style_types(),
  type := integer(),
  value => 'undefined' | binary()
}.

-type thread_member_response() :: #{
  flags := integer(),
  id := snowflake_type(),
  join_timestamp := binary(),
  member => 'undefined' | guild_member_response(),
  user_id := snowflake_type()
}.

-type thread_metadata_response() :: #{
  archive_timestamp => 'undefined' | binary(),
  archived := boolean(),
  auto_archive_duration := thread_auto_archive_duration(),
  create_timestamp => 'undefined' | binary(),
  invitable => 'undefined' | boolean(),
  locked := boolean()
}.

-type thread_response() :: #{
  applied_tags => 'undefined' | list(),
  bitrate => 'undefined' | integer(),
  flags := integer(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  last_message_id => 'undefined' | snowflake_type(),
  last_pin_timestamp => 'undefined' | binary(),
  member => 'undefined' | thread_member_response(),
  member_count := integer(),
  message_count := integer(),
  name := binary(),
  owner_id := snowflake_type(),
  parent_id => 'undefined' | snowflake_type(),
  permissions => 'undefined' | binary(),
  rate_limit_per_user => 'undefined' | integer(),
  rtc_region => 'undefined' | binary(),
  thread_metadata => 'undefined' | thread_metadata_response(),
  total_message_sent := integer(),
  type := integer(),
  user_limit => 'undefined' | integer(),
  video_quality_mode => 'undefined' | video_quality_modes()
}.

-type threads_response() :: #{
  has_more => 'undefined' | boolean(),
  members := list(),
  threads := list()
}.

-type update_default_reaction_emoji_request() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary()
}.

-type update_guild_channel_request_partial() :: #{
  available_tags => 'undefined' | list(),
  bitrate => 'undefined' | integer(),
  default_auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  default_forum_layout => 'undefined' | forum_layout(),
  default_reaction_emoji => 'undefined' | update_default_reaction_emoji_request(),
  default_sort_order => 'undefined' | thread_sort_order(),
  default_thread_rate_limit_per_user => 'undefined' | integer(),
  flags => 'undefined' | integer(),
  name => 'undefined' | binary(),
  nsfw => 'undefined' | boolean(),
  parent_id => 'undefined' | snowflake_type(),
  permission_overwrites => 'undefined' | list(),
  position => 'undefined' | integer(),
  rate_limit_per_user => 'undefined' | integer(),
  rtc_region => 'undefined' | binary(),
  topic => 'undefined' | binary(),
  type => 'undefined' | integer(),
  user_limit => 'undefined' | integer(),
  video_quality_mode => 'undefined' | video_quality_modes()
}.

-type update_guild_onboarding_request() :: #{
  default_channel_ids => 'undefined' | list(),
  enabled => 'undefined' | boolean(),
  mode => 'undefined' | guild_onboarding_mode(),
  prompts => 'undefined' | list()
}.

-type update_message_interaction_callback_request() :: #{
  data => 'undefined' | incoming_webhook_update_for_interaction_callback_request_partial(),
  type := integer()
}.

-type update_message_interaction_callback_response() :: #{
  message := message_response(),
  type := integer()
}.

-type update_onboarding_prompt_request() :: #{
  id := snowflake_type(),
  in_onboarding => 'undefined' | boolean(),
  options := list(),
  required => 'undefined' | boolean(),
  single_select => 'undefined' | boolean(),
  title := binary(),
  type => 'undefined' | onboarding_prompt_type()
}.

-type update_thread_request_partial() :: #{
  applied_tags => 'undefined' | list(),
  archived => 'undefined' | boolean(),
  auto_archive_duration => 'undefined' | thread_auto_archive_duration(),
  bitrate => 'undefined' | integer(),
  flags => 'undefined' | integer(),
  invitable => 'undefined' | boolean(),
  locked => 'undefined' | boolean(),
  name => 'undefined' | binary(),
  rate_limit_per_user => 'undefined' | integer(),
  rtc_region => 'undefined' | binary(),
  user_limit => 'undefined' | integer(),
  video_quality_mode => 'undefined' | video_quality_modes()
}.

-type update_thread_tag_request() :: #{
  emoji_id => 'undefined' | snowflake_type(),
  emoji_name => 'undefined' | binary(),
  id => 'undefined' | snowflake_type(),
  moderated => 'undefined' | boolean(),
  name := binary()
}.

-type user_avatar_decoration_response() :: #{
  asset := binary(),
  sku_id => 'undefined' | snowflake_type()
}.

-type user_communication_disabled_action() :: #{
  metadata := user_communication_disabled_action_metadata(),
  type := integer()
}.

-type user_communication_disabled_action_metadata() :: #{
  duration_seconds => 'undefined' | integer()
}.

-type user_communication_disabled_action_metadata_response() :: #{
  duration_seconds := integer()
}.

-type user_communication_disabled_action_response() :: #{
  metadata := user_communication_disabled_action_metadata_response(),
  type := integer()
}.

-type user_guild_onboarding_response() :: #{
  default_channel_ids := list(),
  enabled := boolean(),
  guild_id := snowflake_type(),
  prompts := list()
}.

-type user_pii_response() :: #{
  accent_color => 'undefined' | integer(),
  avatar => 'undefined' | binary(),
  avatar_decoration_data => 'undefined' | user_avatar_decoration_response(),
  banner => 'undefined' | binary(),
  bot => 'undefined' | boolean(),
  clan => 'undefined' | user_primary_guild_response(),
  discriminator := binary(),
  email => 'undefined' | binary(),
  flags := int53_type(),
  global_name => 'undefined' | binary(),
  id := snowflake_type(),
  locale := available_locales_enum(),
  mfa_enabled := boolean(),
  premium_type => 'undefined' | premium_types(),
  public_flags := integer(),
  system => 'undefined' | boolean(),
  username := binary(),
  verified => 'undefined' | boolean()
}.

-type user_response() :: #{
  accent_color => 'undefined' | integer(),
  avatar => 'undefined' | binary(),
  avatar_decoration_data => 'undefined' | user_avatar_decoration_response(),
  banner => 'undefined' | binary(),
  bot => 'undefined' | boolean(),
  clan => 'undefined' | user_primary_guild_response(),
  discriminator := binary(),
  flags := int53_type(),
  global_name => 'undefined' | binary(),
  id := snowflake_type(),
  public_flags := integer(),
  system => 'undefined' | boolean(),
  username := binary()
}.

-type user_select_component_for_message_request() :: #{
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type user_select_component_response() :: #{
  custom_id := binary(),
  default_values => 'undefined' | list(),
  disabled => 'undefined' | boolean(),
  id := integer(),
  max_values => 'undefined' | integer(),
  min_values => 'undefined' | integer(),
  placeholder => 'undefined' | binary(),
  type := integer()
}.

-type user_select_default_value() :: #{
  id := snowflake_type(),
  type := binary()
}.

-type user_select_default_value_response() :: #{
  id := snowflake_type(),
  type := binary()
}.

-type vanity_url_error_response() :: #{
  code := integer(),
  message := binary()
}.

-type vanity_url_response() :: #{
  code => 'undefined' | binary(),
  error => 'undefined' | vanity_url_error_response(),
  uses := integer()
}.

-type voice_region_response() :: #{
  custom := boolean(),
  deprecated := boolean(),
  id := binary(),
  name := binary(),
  optimal := boolean()
}.

-type voice_scheduled_event_create_request() :: #{
  channel_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_metadata => 'undefined' | entity_metadata_voice(),
  entity_type := integer(),
  image => 'undefined' | binary(),
  name := binary(),
  privacy_level := guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time := binary()
}.

-type voice_scheduled_event_patch_request_partial() :: #{
  channel_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_metadata => 'undefined' | entity_metadata_voice(),
  entity_type => 'undefined' | integer(),
  image => 'undefined' | binary(),
  name => 'undefined' | binary(),
  privacy_level => 'undefined' | guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time => 'undefined' | binary(),
  status => 'undefined' | guild_scheduled_event_statuses()
}.

-type voice_scheduled_event_response() :: #{
  channel_id => 'undefined' | snowflake_type(),
  creator => 'undefined' | user_response(),
  creator_id => 'undefined' | snowflake_type(),
  description => 'undefined' | binary(),
  entity_id => 'undefined' | snowflake_type(),
  entity_metadata => 'undefined' | entity_metadata_voice_response(),
  entity_type := integer(),
  guild_id := snowflake_type(),
  id := snowflake_type(),
  image => 'undefined' | binary(),
  name := binary(),
  privacy_level := guild_scheduled_event_privacy_levels(),
  scheduled_end_time => 'undefined' | binary(),
  scheduled_start_time := binary(),
  status := guild_scheduled_event_statuses(),
  user_count => 'undefined' | integer(),
  user_rsvp => 'undefined' | scheduled_event_user_response()
}.

-type voice_state_response() :: #{
  channel_id => 'undefined' | snowflake_type(),
  deaf := boolean(),
  guild_id => 'undefined' | snowflake_type(),
  member => 'undefined' | guild_member_response(),
  mute := boolean(),
  request_to_speak_timestamp => 'undefined' | binary(),
  self_deaf := boolean(),
  self_mute := boolean(),
  self_stream => 'undefined' | boolean(),
  self_video := boolean(),
  session_id := binary(),
  suppress := boolean(),
  user_id := snowflake_type()
}.

-type webhook_slack_embed() :: #{
  author_icon => 'undefined' | binary(),
  author_link => 'undefined' | binary(),
  author_name => 'undefined' | binary(),
  color => 'undefined' | binary(),
  fields => 'undefined' | list(),
  footer => 'undefined' | binary(),
  footer_icon => 'undefined' | binary(),
  image_url => 'undefined' | binary(),
  pretext => 'undefined' | binary(),
  text => 'undefined' | binary(),
  thumb_url => 'undefined' | binary(),
  title => 'undefined' | binary(),
  title_link => 'undefined' | binary(),
  ts => 'undefined' | integer()
}.

-type webhook_slack_embed_field() :: #{
  inline => 'undefined' | boolean(),
  name => 'undefined' | binary(),
  value => 'undefined' | binary()
}.

-type webhook_source_channel_response() :: #{
  id := snowflake_type(),
  name := binary()
}.

-type webhook_source_guild_response() :: #{
  icon => 'undefined' | binary(),
  id := snowflake_type(),
  name := binary()
}.

-type welcome_message_response() :: #{
  author_ids := list(),
  message := binary()
}.

-type welcome_screen_patch_request_partial() :: #{
  description => 'undefined' | binary(),
  enabled => 'undefined' | boolean(),
  welcome_channels => 'undefined' | list()
}.

-type widget_activity() :: #{
  name := binary()
}.

-type widget_channel() :: #{
  id := snowflake_type(),
  name := binary(),
  position := integer()
}.

-type widget_member() :: #{
  activity => 'undefined' | widget_activity(),
  avatar => 'undefined' | any(),
  avatar_url := binary(),
  channel_id => 'undefined' | snowflake_type(),
  deaf => 'undefined' | boolean(),
  discriminator := widget_user_discriminator(),
  id := binary(),
  mute => 'undefined' | boolean(),
  self_deaf => 'undefined' | boolean(),
  self_mute => 'undefined' | boolean(),
  status := binary(),
  suppress => 'undefined' | boolean(),
  username := binary()
}.

-type widget_response() :: #{
  channels := list(),
  id := snowflake_type(),
  instant_invite => 'undefined' | binary(),
  members := list(),
  name := binary(),
  presence_count := integer()
}.

-type widget_settings_response() :: #{
  channel_id => 'undefined' | snowflake_type(),
  enabled := boolean()
}.


