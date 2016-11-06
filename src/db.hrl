-define(ID, any()).

-record(t_alias, {
    provider::string(),
    username::string(),
    user_id::?ID
}).

-record(t_user, {
    id::?ID,
    provider::string(),
    username::string(),
    status::string(),
    active::boolean()
}).

-record(t_session, {
    id::?ID,
    user_id::?ID,
    provider::string(),
    token::any()
}).

-record(t_token, {
    user_id::?ID,
    provider::string(),
    token::any()
}).

-define(T_ROOM_PRIVATE, "private").
-define(T_ROOM_PUBLIC, "public").
-define(T_ROOM_DIRECT, "direct").
-define(T_ROOM_BOT, "bot").

-record(t_room, {
    id::?ID,
    name::string(),
    type::string(),
    data = none::any()
}).

-record(t_message, {
    room_id::?ID,
    timestamp::any(),
    id::?ID,
    user_id::?ID,
    data::any()
}).
