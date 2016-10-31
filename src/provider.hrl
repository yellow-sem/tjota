-define(ID, any()).

-record(p_course, {
    id::?ID,
    name::string(),
    category::string(),
    active::boolean(),
    url::string()
}).

-record(p_member, {
    type::student|supervisor,
    name::string(),
    alias::string()
}).

-record(p_assignment, {
    id::?ID,
    name::string(),
    group::string(),
    url::string(),
    deadline::any(),
    status::string()
}).

-record(p_room, {
    id::?ID,
    name::string(),
    type::course|project|other,
    role::student|supervisor
}).
