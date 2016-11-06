-module(command_handler).
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

handle_call({identity, Identity}, _From, new) ->
    {reply, ok, {identity, Identity}};

handle_call({handle, Command, Args}, _From, {identity, Identity}) ->
    {stop, normal, handle(Identity, Command, Args), done}.

handle_cast(_Request, Client) -> {noreply, Client}.

handle_info(_Info, Client) -> {noreply, Client}.

terminate(_Reason, _Client) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle(Identity, ?C_SYS_EXIT, []) ->
    {ok, Identity, stop};

handle(_Identity, ?C_AUTH_LOGIN, [Id]) ->
    {session, Session} = db_auth:login(#t_session{id = Id}),
    {
        ok, Session#t_session.user_id,
        uuid:uuid_to_string(Session#t_session.id)
    };

handle(_Identity, ?C_AUTH_LOGIN, [Credential, Password]) ->
    [Username, Provider] = string:tokens(Credential, "@"),
    {true, Token} = provider:identity_login(Provider, Username, Password),
    {session, Session} = db_auth:login(Provider, Username, Token),
    {
        ok, Session#t_session.user_id,
        uuid:uuid_to_string(Session#t_session.id)
    };

handle(undefined, _Command, _Args) -> unauthorized;

handle(Identity, ?C_AUTH_LOGOUT, [Id]) ->
    [Session] = db:select_session(#t_session{id = Id}),
    Identity = Session#t_session.user_id,
    db:delete_session(Session),
    true = provider:identity_logout(Session#t_session.provider,
                                    Session#t_session.token),
    {ok, undefined};

handle(Identity, ?C_AUTH_CHECK, [Credential]) ->
    [Username, Provider] = string:tokens(Credential, "@"),
    [#t_alias{}] = db:select_alias(#t_alias{provider = Provider,
                                            username = Username}),
    {ok, Identity};

handle(Identity, ?C_ROOM_LIST, []) ->
    User = #t_user{id = Identity},
    [
        send({identity, Identity}, ?C_ROOM_SELF, data:format(R))
        || R <- db:select_room(db:select_user_room(User))
    ],
    {ok, Identity};

handle(Identity, ?C_ROOM_LIST, [Id]) ->
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [_] = db:select_user_room(#t_user{id = Identity}, Room),
    [
        send({identity, Identity}, ?C_ROOM_ANY, data:format(Room, U, in))
        || U <- db:select_user(db:select_room_user(Room))
    ],
    {ok, Identity};

handle(Identity, ?C_ROOM_DISCOVER, []) ->
    {ok, Identity};

handle(Identity, ?C_ROOM_CREATE, [Name, Type]) ->
    handle(Identity, ?C_ROOM_CREATE, [Name, Type, none]);

handle(Identity, ?C_ROOM_CREATE, [Name, Type, Data]) ->
    [User] = db:select_user(#t_user{id = Identity}),
    Room = #t_room{
        id = uuid:get_v4(),
        name = Name,
        type = Type,
        data = Data
    },
    {ok, _} = db:insert_room(Room),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Identity}, ?C_ROOM_SELF, data:format(Room)),
    [
        send({identity, I}, ?C_ROOM_ANY, data:format(Room, User, in))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    case {Type, Data} of
        {?T_ROOM_DIRECT, none} -> ok;
        {?T_ROOM_DIRECT, Credential} ->
            handle(Identity, ?C_ROOM_INVITE,
                   [uuid:uuid_to_string(Room#t_room.id), Credential]);
        {_, _} -> ok
    end,
    {ok, Identity, uuid:uuid_to_string(Room#t_room.id)};

handle(Identity, ?C_ROOM_JOIN, [Id]) ->
    [User] = db:select_user(#t_user{id = Identity}),
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [#t_room{type = ?T_ROOM_PUBLIC}] = db:select_room(Room),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Identity}, ?C_ROOM_SELF, data:format(Room)),
    [
        send({identity, I}, ?C_ROOM_ANY, data:format(Room, User, in))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    {ok, Identity};

handle(Identity, ?C_ROOM_INVITE, [Id, Credential]) ->
    [Room] = db:select_room(#t_room{id = uuid:string_to_uuid(Id)}),
    [_] = db:select_user_room(#t_user{id = Identity}, Room),
    [Username, Provider] = string:tokens(Credential, "@"),
    [Alias] = db:select_alias(#t_alias{provider = Provider,
                                       username = Username}),
    [User] = db:select_user(#t_user{id = Alias#t_alias.user_id}),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Alias#t_alias.user_id}, ?C_ROOM_SELF, data:format(Room)),
    [
        send({identity, I}, ?C_ROOM_ANY, data:format(Room, User, in))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    case Room of
        #t_room{type = ?T_ROOM_DIRECT} ->
            Data = uuid:uuid_to_string(User#t_user.id),
            db:update_room(Room#t_room{data = Data}),
            send({identity, Identity}, ?C_STATUS_RECV,
                 data:format(User, User#t_user.status));
        _ -> ok
    end,
    {ok, Identity};

handle(Identity, ?C_ROOM_LEAVE, [Id]) ->
    [User] = db:select_user(#t_user{id = Identity}),
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    {ok, _} = db:sym_update_user_room(User, Room, false),
    [
        send({identity, I}, ?C_ROOM_ANY, data:format(Room, User, out))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    send({identity, Identity}, ?C_ROOM_EXIT, Id),
    {ok, Identity, Id};

handle(Identity, ?C_MSG_SEND, [Id, Data]) ->
    User = #t_user{id = Identity},
    [Room] = db:select_room(#t_room{id = uuid:string_to_uuid(Id)}),
    [_] = db:select_user_room(User, Room),
    Message = #t_message{
        room_id = Room#t_room.id,
        timestamp = now,
        id = uuid:get_v4(),
        user_id = User#t_user.id,
        data = Data
    },
    handle_message(User, Room, Message),
    {ok, Identity};

handle(Identity, ?C_MSG_REQ, [Id]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [_] = db:select_user_room(User, Room),
    [
        send({identity, Identity}, ?C_MSG_RECV, data:format(M))
        || M <- db:select_message(#t_message{room_id = Room#t_room.id})
    ],
    {ok, Identity};

handle(Identity, ?C_LINK_EXTRACT, [Link]) ->
    {ok, Identity, Link};

handle(Identity, ?C_STATUS_SET, [Status]) ->
    [User] = db:select_user(#t_user{id = Identity}),
    db:update_user(User#t_user{status = Status}),
    [
        send({identity, I}, ?C_STATUS_RECV, data:format(User, Status))
        || I <- sets:to_list(sets:from_list(lists:concat([
            [U#t_user.id || U <- db:select_room_user(R)]
            || R <- db:select_room(db:select_user_room(User)),
               R#t_room.type == ?T_ROOM_DIRECT
        ])))
    ],
    {ok, Identity};

handle(Identity, ?C_STATUS_REQ, []) ->
    [User] = db:select_user(#t_user{id = Identity}),
    Status = User#t_user.status,
    [
        send({identity, I}, ?C_STATUS_RECV, data:format(User, Status))
        || I <- sets:to_list(sets:from_list(lists:concat([
            [U#t_user.id || U <- db:select_room_user(R)]
            || R <- db:select_room(db:select_user_room(User)),
               R#t_room.type == ?T_ROOM_DIRECT
        ])))
    ],
    {ok, Identity}.

broadcast_message(Room, Message) ->
    [
        send({identity, I}, ?C_MSG_RECV, data:format(Message))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    ok.

handle_message(#t_user{} = User,
               #t_room{} = Room,
               #t_message{data = [$/ | Payload]} = _Message) ->

    handle_message_command(User, Room, data:tokens(Payload));

handle_message(#t_user{} = User,
               #t_room{type = ?T_ROOM_BOT} = Room,
               #t_message{} = Message) ->

    {ok, _} = db:insert_message(Message),
    ok = broadcast_message(Room, Message),

    {ok, Process} = supervisor:start_child(bot_handler_sup, []),
    (catch gen_server:cast(Process, {dispatch, User, Room, Message}));

handle_message(#t_user{} = _User,
               #t_room{} = Room,
               #t_message{} = Message) ->

    {ok, _} = db:insert_message(Message),
    ok = broadcast_message(Room, Message).

handle_message_command(#t_user{} = User, #t_room{} = Room,
                       [?MC_ROOM_INVITE, Credential]) ->

    {ok, _} = handle(User#t_user.id, ?C_ROOM_INVITE,
                     [uuid:uuid_to_string(Room#t_room.id), Credential]);

handle_message_command(#t_user{} = User, #t_room{} = Room,
                       [?MC_ROOM_LEAVE]) ->

    {ok, _, _} = handle(User#t_user.id, ?C_ROOM_LEAVE,
                        [uuid:uuid_to_string(Room#t_room.id)]);

handle_message_command(#t_user{} = User, #t_room{} = _Room,
                       [?MC_STATUS_SET, Status]) ->

    {ok, _} = handle(User#t_user.id, ?C_STATUS_SET, [Status]).

send(To, Command, Data) -> command_event:send(To, Command, Data).
