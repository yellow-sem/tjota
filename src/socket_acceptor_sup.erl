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
    Children = [child(socket_receiver_event, [], worker),
                child(socket_acceptor, [socket_acceptor], worker),
                child(socket_receiver_sup, [], supervisor),
                child(socket_handler_sup, [], supervisor)],
    {ok, {Flags, Children}}.


child(Module, Modules, Type) ->
    #{id => Module,
      start => {Module, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => Type,
      modules => Modules}.

