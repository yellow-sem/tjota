-module(tjota_app).
-behaviour(application).
-export([
    start/2,
    stop/1
]).

-include("tjota_com.hrl").

start(_StartType, _StartArgs) ->
    %tjota_db:bootstrap(),
    %User = #t_user{
    %    id = "7734df5b-3441-415f-9928-fc2217376577",
    %    provider = "yellow",
    %    alias = "gujdoe",
    %    name = "John Doe",
    %    active = true
    %},
    %tjota_db:insert_user(User),
    %Data = tjota_db:select_user([#t_user{id = "7734df5b-3441-415f-9928-fc2217376577"}]),
    %io:format("~p~n", [Data]),
    tjota_sup:start_link().

stop(_State) -> ok.
