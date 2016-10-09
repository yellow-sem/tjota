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
    name::string(),
    type::group|private|bot,
    data::any()
}).

-record(t_message, {
    room_id::?ID|[?ID],
    timestamp::any(),
    id::?ID,
    user_id::?ID,
    data::any()
}).
