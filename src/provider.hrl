
-record(p_course, {
    id::any(),
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
    id::any(),
    name::string(),
    group::string(),
    url::string(),
    deadline::any(),
    status::string()
}).

-define(P_ROOM_COURSE, "course").
-define(P_ROOM_PROJECT, "project").
-define(P_ROOM_OTHER, "other").

-define(P_ROLE_STUDENT, "student").
-define(P_ROLE_SUPERVISOR, "supervisor").

-record(p_room, {
    id::any(),
    name::string(),
    type::string(),
    role::string()
}).
