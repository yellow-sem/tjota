-module(resource_sup).
-behaviour(supervisor).
-export([
    start_link/0
]).
-export([
    init/1
]).
-export([
    start/1,
    stop/1,
    publish/2,
    start_all/0
]).

-include("db.hrl").

-define(REGISTRY, resource_registry).

start_link() ->
    Sup = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ets:new(?REGISTRY, [set, named_table, public]),
    start_all(),
    Sup.

init([]) ->
    Flags = {one_for_one, 0, 1},
    Children = [
        util:child(resource_event, [], worker, permanent),
        util:child(resource_protocol_mqtt_sup, supervisor)
    ],
    {ok, {Flags, Children}}.

get_protocol_sup(?T_RESOURCE_MQTT) -> resource_protocol_mqtt_sup;
get_protocol_sup(_Protocol) -> none.

start(#t_resource{protocol = Protocol} = Resource) ->
    ProtocolSup = get_protocol_sup(Protocol),
    case ProtocolSup of
        none -> ok;
        _ ->
            {ok, Process} = supervisor:start_child(ProtocolSup, []),
            ets:insert(?REGISTRY, {Resource, Process}),
            ok = gen_server:call(Process, {start, Resource}),
            resource_event:subscribe(Process, Resource)
    end.

stop(#t_resource{protocol = Protocol} = Resource) ->
    ProtocolSup = get_protocol_sup(Protocol),
    case ProtocolSup of
        none -> ok;
        _ ->
            [{Resource, Process}] = ets:lookup(?REGISTRY, Resource),
            resource_event:unsubscribe(Process),
            ok = gen_server:call(Process, stop),
            ets:delete(?REGISTRY, Resource)
    end.

publish(#t_resource{} = Resource, Data) ->
    resource_event:publish(Resource, Data).

start_all() -> [start(R) || R <- db:select_resource()].
