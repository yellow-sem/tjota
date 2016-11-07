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

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({dispatch, User, Room, Message}, new) ->
    dispatch(User, Room, Message),
    {noreply, done}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

dispatch(#t_user{} = User, #t_room{} = Room,
         #t_message{data = DataIn} = _MessageIn) ->
    [Protocol, Address] = string:tokens(Room#t_room.data, "://"),
    {ok, DataOut} = request(User, Room, Protocol, Address, DataIn),
    MessageOut = #t_message{
        room_id = Room#t_room.id,
        timestamp = now,
        id = uuid:get_v4(),
        data = DataOut
    },
    {ok, _} = db:insert_message(MessageOut),
    [
        send({identity, I}, ?C_MSG_RECV, data:format(MessageOut))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    ok.

request(#t_user{id = Identity} = _User, #t_room{id = RoomId} = _Room,
        "provider", Provider, DataIn) ->
    [#t_token{token = Token}] = db:select_token(#t_token{user_id = Identity,
                                                         provider = Provider}),
    {true, DataOut} = provider:chat_handle(Provider, Token, DataIn,
                                           uuid:uuid_to_string(RoomId)),
    {ok, DataOut};

request(_User, _Room, _Protocol, _Address, _DataIn) -> not_implemented.

send(To, Command, Data) -> command_event:send(To, Command, Data).
