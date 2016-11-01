-module(bot_handler).
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
-include("data.hrl").

start_link() -> gen_server:start_link(?MODULE, default, []).

init(default) -> {ok, new}.

handle_call({dispatch, Room, Message}, _From, new) ->
    {stop, normal, dispatch(Room, Message), done}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

dispatch(#t_room{} = Room, #t_message{} = MessageIn) ->
    MessageOut = #t_message{
        room_id = Room#t_room.id,
        timestamp = now,
        id = uuid:get_v4(),
        data = MessageIn#t_message.data
    },
    {ok, _} = db:insert_message(MessageOut),
    [
        send({identity, I}, ?C_MSG_RECV, data:format(MessageOut))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    ok.

send(To, Command, Data) -> socket_receiver_event:send(To, Command, Data).
