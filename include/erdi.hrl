%% Discord Op Codes
-define(OP_DISPATCH, 0).
-define(OP_HEARTBEAT, 1).
-define(OP_IDENTIFY, 2).
-define(OP_PRESENCE_UPDATE, 3).
-define(OP_VOICE_STATE_UPDATE, 4).
-define(OP_RESUME, 6).
-define(OP_RECONNECT, 7).
-define(OP_REQUEST_GUILD_MEMBERS, 8).
-define(OP_INVALID_SESSION, 9).
-define(OP_HELLO, 10).
-define(OP_HEARTBEAT_ACK, 11).
%% Discord keys
-define(OPCODE, <<"op">>).
-define(DATA, <<"d">>).
-define(SEQ, <<"s">>).
-define(TYPE, <<"t">>).
-define(HEARTBEAT_INTERVAL, <<"heartbeat_interval">>).
