-module(tjota_sup).
-behaviour(supervisor).
-export([
    start_link/0
]).
-export([
    init/1
]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Flags = {one_for_one, 0, 1},
    Children = [
        {
            socket_acceptor_sup,
            {socket_acceptor_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            []
        },
        {
            bot_handler_sup,
            {bot_handler_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            []
        }
    ],
    {ok, {Flags, Children}}.
