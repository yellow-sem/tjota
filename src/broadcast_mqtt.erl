-module(broadcast_mqtt).
-behaviour(gen_server).
-export([
    start_link/0
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

-define(MQTT_HOST, "localhost").
-define(MQTT_PORT, 1883).

-define(TOPIC_FORMAT, "tjota/users/~s/~s").

start_link() -> gen_server:start_link({local, broadcast_mqtt},
                                      ?MODULE, default, []).

init(default) ->
    {ok, MQTT} = emqttc:start_link([{host, ?MQTT_HOST},
                                    {port, ?MQTT_PORT},
                                    {logger, info}]),
    {ok, {state, MQTT}}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({status, #t_user{} = User}, {state, MQTT}) ->
    Topic = topic(User),
    Data = jiffy:encode({[
        {status, encode(User#t_user.status)},
        {connected, true},
        {user, {[
            {id, encode(uuid:uuid_to_string(User#t_user.id))},
            {provider, encode(User#t_user.provider)},
            {username, encode(User#t_user.username)}
        ]}}
    ]}),
    emqttc:publish(MQTT, Topic, Data),
    {noreply, {state, MQTT}};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, {state, MQTT}) ->
    emqttc:disconnect(MQTT),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

topic(#t_user{} = User) ->
    list_to_binary(io_lib:format(?TOPIC_FORMAT,
                                 [User#t_user.provider,
                                  User#t_user.username])).

encode(List) -> unicode:characters_to_binary(List).
