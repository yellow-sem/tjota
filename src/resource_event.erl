-module(resource_event).
-behaviour(gen_event).
-export([
    start_link/0
]).
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-export([
    subscribe/2,
    unsubscribe/1
]).
-export([
    publish/2
]).

-include("db.hrl").

-define(MANAGER_REF, resource_event).

start_link() -> gen_event:start_link({local, ?MANAGER_REF}).

init({new, #t_resource{} = Resource, Receiver}) ->
    {ok, {state, Resource, Receiver}}.

handle_event({publish, #t_resource{} = Resource, Data},
             {state, #t_resource{} = Resource, Receiver}) ->

    Receiver ! {publish, Data},
    {ok, {state, Resource, Receiver}};

handle_event(_, State) -> {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

subscribe(Receiver, Resource) ->
    gen_event:delete_handler(?MANAGER_REF,
                             {resource_event, Receiver},
                             []),
    gen_event:add_handler(?MANAGER_REF,
                          {resource_event, Receiver},
                          {new, Resource, Receiver}).

unsubscribe(Receiver) ->
    gen_event:delete_handler(?MANAGER_REF,
                             {resource_event, Receiver},
                             []).

publish(Resource, Data) ->
    gen_event:notify(?MANAGER_REF, {publish, Resource, Data}).
