
-define(ID, string()).

-record(t_user, {
    id::?ID,
    alias::string(),
    name::string(),
    password::string()
}).

-record(t_alias, {
    alias::string(),
    user_id::?ID
}).

-record(t_session, {
    id::?ID,
    user_id::?ID
}).

-record(t_room, {
    id::?ID,
    name::string(),
    type::group|private|bot,
    data::any()
}).

-record(t_message, {
    room_id::?ID,
    timestamp::any(),
    id::?ID,
    user_id::?ID,
    data::any()
}).
