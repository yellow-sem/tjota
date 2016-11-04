-module(socket_acceptor_tcp_sup).
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
        util:child(socket_acceptor_tcp, worker),
        util:child(socket_receiver_tcp_sup, supervisor)
    ],
    {ok, {Flags, Children}}.
