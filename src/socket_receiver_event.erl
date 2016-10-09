-module(socket_receiver_event).
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

-include("socket_com.hrl").

start_link() -> gen_event:start_link({local, socket_receiver_event}).

init(#s_client{} = Client) -> {ok, Client}.

handle_event({send, User, Command, Content},
             #s_client{user = User} = Client) ->
    Client#s_client.receiver ! {content, {Command, Content}},
    {ok, Client};

handle_event(_, #s_client{} = Client) -> {ok, Client}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
