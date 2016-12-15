-module(resource_protocol_mqtt).
-behaviour(gen_server).
-export([
    start_link/0
]).
-export([
    topic/2,
    topic/1
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("db.hrl").
-include("data.hrl").

-define(TOPIC_IMPL_INCOMING, "/incoming").
-define(TOPIC_IMPL_OUTGOING, "/outgoing").

start_link() -> gen_server:start_link(?MODULE, default, []).

init(default) -> {ok, new}.

handle_call({start, #t_resource{address = Address} = Resource}, _From, new) ->
    {Host, Port, Path} = data:address(Address),
    {ok, MQTT} = emqttc:start_link([{host, Host},
                                    {port, Port},
                                    {logger, info}]),
    Topic = topic(Path, impl_outgoing),
    emqttc:subscribe(MQTT, Topic),
    {reply, ok, {state, Resource, MQTT, Path}};

handle_call(stop, _From, State) -> {stop, normal, State};

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info({publish, _Topic, Payload},
            {state, #t_resource{room_id = RoomId} = Resource, MQTT, Path}) ->

    MessageOut = #t_message{
        room_id = RoomId,
        timestamp = now,
        id = uuid:get_v4(),
        data = Payload
    },
    {ok, _} = db:insert_message(MessageOut),
    [
        send({identity, I}, ?C_MSG_RECV, data:format(MessageOut))
        || #t_user{id = I} <- db:select_room_user(#t_room{id = RoomId})
    ],
    {noreply, {state, Resource, MQTT, Path}};

handle_info({publish, Data},
            {state, Resource, MQTT, Path}) ->
    Topic = topic(Path, impl_incoming),
    emqttc:publish(MQTT, Topic, unicode:characters_to_binary(Data)),
    {noreply, {state, Resource, MQTT, Path}}.

terminate(_Reason, {state, _Resource, MQTT, _Path}) ->
    emqttc:disconnect(MQTT),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

send(To, Command, Data) -> command_event:send(To, Command, Data).

topic(Path, impl_incoming) -> topic(string:concat(Path, ?TOPIC_IMPL_INCOMING));
topic(Path, impl_outgoing) -> topic(string:concat(Path, ?TOPIC_IMPL_OUTGOING));
topic(_, _) -> undefined.

topic(Path) -> list_to_binary(string:strip(Path, both, $/)).
