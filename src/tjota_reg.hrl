
-define(ID, string()).

-record(t_user, {
    alias::string(),
    name::string(),
    password::string(),
    id::?ID
}).

-record(t_session, {
    token::string(),
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
    user_id::?ID,
    data::any()
}).
