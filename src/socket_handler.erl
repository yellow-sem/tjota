-module(socket_handler).
-export([
    handle/3
]).

-include("socket_com.hrl").
-include("db_com.hrl").

-define(C_SYS_EXIT, "sys:exit").

-define(C_AUTH_LOGIN, "auth:login").
-define(C_AUTH_LOGOUT, "auth:logout").

-define(C_ROOM_PING, "room:ping").
-define(C_ROOM_PONG, "room:pong").
-define(C_ROOM_LEAVE, "room:leave").
-define(C_ROOM_INVITE, "room:invite").
-define(C_ROOM_USER, "room:user").

-define(C_MSG_RECV, "msg:recv").
-define(C_MSG_SEND, "msg:send").

-define(C_LINK_EXTRACT, "link:extract").

-define(A_DISCOVER, "discover").
-define(A_CREATE, "create").
-define(A_JOIN, "join").

handle(#s_client{} = Client, ?C_SYS_EXIT, []) ->
    {ok, Client, stop};

handle(#s_client{} = Client, ?C_AUTH_LOGIN, [Id]) ->
    {session, Session} = db_auth:login(#t_session{id = Id}),
    {
        ok, Client#s_client{identity = Session#t_session.user_id},
        uuid:uuid_to_string(Session#t_session.id)
    };

handle(#s_client{} = Client, ?C_AUTH_LOGIN, [Credential, Password]) ->
    [Username, Provider] = string:tokens(Credential, "@"),
    {true, Token} = provider:identity_login(Provider, Username, Password),
    {session, Session} = db_auth:login(Provider, Username, Token),
    {
        ok, Client#s_client{identity = Session#t_session.user_id},
        uuid:uuid_to_string(Session#t_session.id)
    };

handle(#s_client{} = Client, ?C_AUTH_LOGOUT, [Id]) ->
    [Session] = db:select_session(#t_session{id = Id}),
    db:delete_session(Session),
    true = provider:identity_logout(Session#t_session.provider,
                                    Session#t_session.token),
    {ok, Client};

handle(#s_client{identity = Identity} = Client, ?C_ROOM_PING, []) ->
    User = #t_user{id = Identity},
    [
        send({identity, Identity}, ?C_ROOM_PONG, format(Room)) ||
        Room <- db:select_room(db:select_user_room(User))
    ],
    {ok, Client};

handle(#s_client{} = Client,
       ?C_ROOM_PING, [?A_DISCOVER]) ->
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_PING, [?A_CREATE, Name, Type]) ->
    User = #t_user{id = Identity},
    Room = #t_room{
        id = uuid:get_v4(),
        name = Name,
        type = Type
    },
    {ok, _} = db:insert_room(Room),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Identity}, ?C_ROOM_PONG, format(Room)),
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_PING, [?A_JOIN, Id]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [#t_room{type = ?T_ROOM_PUBLIC}] = db:select_room(Room),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Identity}, ?C_ROOM_PONG, format(Room)),
    {ok, Client};

handle(#s_client{identity = Identity} = Client, ?C_ROOM_LEAVE, [Id]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    {ok, _} = db:sym_update_user_room(User, Room, false),
    send({identity, Identity}, ?C_ROOM_LEAVE, Id),
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_INVITE, [Id, Credential]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [_] = db:select_user_room(User, Room),
    [Username, Provider] = string:tokens(Credential, "@"),
    [#t_alias{} = Alias] = db:select_alias(#t_alias{provider = Provider,
                                                    username = Username}),
    {ok, _} = db:sym_insert_user_room(#t_user{id = Alias#t_alias.user_id},
                                      Room, true),
    send({identity, Alias#t_alias.user_id}, ?C_ROOM_INVITE, format(Room)),
    {ok, Client};

handle(#s_client{identity = Identity} = Client, ?C_ROOM_USER, [Id]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [_] = db:select_user_room(User, Room),
    [
        send({identity, Identity}, ?C_ROOM_USER, format(Room, User)) ||
        User <- db:select_user(db:select_room_user(Room))
    ],
    {ok, Client};

handle(#s_client{identity = Identity} = Client, ?C_MSG_SEND, [Id, Data]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    Message = #t_message{
        room_id = Room#t_room.id,
        timestamp = 0,
        id = uuid:get_v4(),
        user_id = User#t_user.id,
        data = Data
    },
    send({identity, Identity}, ?C_MSG_RECV, format(Message)),
    {ok, Client};

handle(#s_client{} = Client, ?C_LINK_EXTRACT, [Link]) ->
    {ok, Client, Link};

handle(#s_client{} = _Client, _Command, _Args) -> not_implemented.

send(To, Command, Data) ->
    gen_event:notify(socket_receiver_event, {send, To, Command, Data}).

format(#t_user{} = User) ->
    io_lib:format("~s ~s@~s", [
        uuid:uuid_to_string(User#t_user.id),
        User#t_user.username,
        User#t_user.provider
    ]);

format(#t_room{} = Room) ->
    io_lib:format("~s '~s' ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        Room#t_room.name,
        Room#t_room.type
    ]);

format(#t_message{} = Message) ->
    io_lib:format("~s ~p ~s '~s'", [
        uuid:uuid_to_string(Message#t_message.room_id),
        Message#t_message.timestamp,
        uuid:uuid_to_string(Message#t_message.user_id),
        Message#t_message.data
    ]).

format(#t_room{} = Room, #t_user{} = User) ->
    io_lib:format("~s ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        format(User)
    ]).
