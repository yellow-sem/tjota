-module(util).
-export([
    child/3
]).

child(Module, Modules, Type) ->
    #{id => Module,
      start => {Module, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => Type,
      modules => Modules}.
