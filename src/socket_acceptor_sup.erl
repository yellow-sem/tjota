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
    Children = [util:child(socket_receiver_event, [], worker),
                util:child(socket_acceptor, [socket_acceptor], worker),
                util:child(socket_receiver_sup, [], supervisor),
                util:child(socket_handler_sup, [], supervisor)],
    {ok, {Flags, Children}}.
