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
        util:child(http, worker),
        util:child(bot_handler_sup, supervisor),
        util:child(socket_handler_sup, supervisor),
        util:child(socket_receiver_event, [], worker, permanent),
        util:child(socket_tcp_acceptor_sup, supervisor)
    ],
    {ok, {Flags, Children}}.
