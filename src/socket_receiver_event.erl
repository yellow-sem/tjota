-module(socket_receiver_event).
-behaviour(gen_event).
-export([
    start_link/0
]).
-export([
    send/3,
    subscribe/1
]).
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("socket.hrl").

-define(MANAGER_REF, socket_receiver_event).

start_link() -> gen_event:start_link({local, ?MANAGER_REF}).

send(To, Command, Data) ->
    gen_event:notify(?MANAGER_REF, {send, To, Command, Data}).

subscribe(Client) ->
    gen_event:delete_handler(?MANAGER_REF,
                             {socket_receiver_event, self()},
                             []),
    gen_event:add_handler(?MANAGER_REF,
                          {socket_receiver_event, self()},
                          Client).

init(#s_client{} = Client) -> {ok, Client}.

handle_event({send, {identity, Identity}, Command, Data},
             #s_client{identity = Identity} = Client) ->
    Client#s_client.receiver ! {send, {Command, Data}},
    {ok, Client};

handle_event({send, {receiver, Receiver}, Command, Data},
             #s_client{receiver = Receiver} = Client) ->
    Client#s_client.receiver ! {send, {Command, Data}},
    {ok, Client};

handle_event({send, _To, _Command, _Data}, #s_client{} = Client) ->
    {ok, Client};

handle_event(_, #s_client{} = Client) -> {ok, Client}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
