-module(socket_acceptor_sup).
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
            socket_acceptor,
            {socket_acceptor, start_link, []},
            permanent,
            infinity,
            worker,
            [socket_acceptor]
        },

        {
            socket_receiver_sup,
            {socket_receiver_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            []
        }
    ],
    {ok, {Flags, Children}}.
