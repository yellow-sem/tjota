-module(socket_tcp_acceptor_sup).
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
        util:child(socket_tcp_acceptor, worker),
        util:child(socket_tcp_receiver_sup, supervisor)
    ],
    {ok, {Flags, Children}}.
