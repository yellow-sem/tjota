-module(resource_mqtt_sup).
-behaviour(supervisor).
-export([
    start_link/0
]).
-export([
    start_resource/0,
    start_resource/1,
    stop_resource/1
]).
-export([
    init/1
]).

-include("db.hrl").

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_resource() ->
    [
        start_resource(R)
        || R <- db:select_resource(#t_resource{protocol = ?T_RESOURCE_MQTT})
    ].

start_resource(#t_resource{} = _Resource) -> ok.

stop_resource(#t_resource{} = _Resource) -> ok.

init([]) ->
    Flags = {simple_one_for_one, 0, 1},
    Children = [
        util:child(resource_mqtt, worker, temporary)
    ],
    {ok, {Flags, Children}}.
