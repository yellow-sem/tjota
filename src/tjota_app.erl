-module(tjota_app).
-behaviour(application).
-export([
    start/2,
    stop/1
]).

-include("tjota_reg.hrl").

start(_StartType, _StartArgs) ->
    tjota_db:bootstrap(),
    User = #t_user{
        id = "7734df5b-3441-415f-9928-fc2217376577",
        alias = "gujdoe",
        name = "John Doe",
        password = "pw",
        active = true
    },
    tjota_db:insert_user(User),
    tjota_db:delete_user(User),
    tjota_sup:start_link().

stop(_State) -> ok.
