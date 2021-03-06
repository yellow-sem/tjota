-module(bot_handler_sup).
-behaviour(supervisor).
-export([
    start_link/0
]).
-export([
    init/1
]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Flags = {simple_one_for_one, 0, 1},
    Children = [
        util:child(bot_handler, worker, temporary)
    ],
    {ok, {Flags, Children}}.
