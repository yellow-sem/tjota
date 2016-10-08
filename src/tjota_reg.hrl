
-define(ID, string()).

-record(t_alias, {
    provider::string(),
    alias::string(),
    user_id::?ID
}).

-record(t_user, {
    id::?ID,
    provider::string(),
    alias::string(),
    name::string(),
    active::boolean()
}).

-record(t_session, {
    id::?ID,
    user_id::?ID,
    data::any()
}).

-record(t_room, {
    id::?ID,
    user_id::?ID,
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
