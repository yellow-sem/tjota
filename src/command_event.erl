-module(command_event).
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
    send/3
]).

-define(MANAGER_REF, command_event).

start_link() -> gen_event:start_link({local, ?MANAGER_REF}).

init({new, Identity, Receiver}) -> {ok, {state, Identity, Receiver}}.

handle_event({send, {identity, Identity}, Command, Data},
             {state, Identity, Receiver}) ->
    Receiver ! {send, {Command, Data}},
    {ok, {state, Identity, Receiver}};

handle_event({send, {receiver, Receiver}, Command, Data},
             {state, Identity, Receiver}) ->
    Receiver ! {send, {Command, Data}},
    {ok, {state, Identity, Receiver}};

handle_event({send, _To, _Command, _Data}, State) ->
    {ok, State};

handle_event(_, State) -> {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

subscribe(Receiver, Identity) ->
    gen_event:delete_handler(?MANAGER_REF,
                             {command_event, Receiver},
                             []),
    gen_event:add_handler(?MANAGER_REF,
                          {command_event, Receiver},
                          {new, Identity, Receiver}).

unsubscribe(Receiver) ->
    gen_event:delete_handler(?MANAGER_REF,
                             {command_event, Receiver},
                             []).

send(To, Command, Data) ->
    gen_event:notify(?MANAGER_REF, {send, To, Command, Data}).
