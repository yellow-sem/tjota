-module(socket_handler).
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

-include("socket_com.hrl").
-include("db_com.hrl").

-define(C_SYS_EXIT, "sys:exit").

-define(C_AUTH_LOGIN, "auth:login").
-define(C_AUTH_LOGOUT, "auth:logout").
-define(C_AUTH_CHECK, "auth:check").

-define(C_ROOM_LIST, "room:list").
-define(C_ROOM_DISCOVER, "room:discover").
-define(C_ROOM_CREATE, "room:create").
-define(C_ROOM_JOIN, "room:join").
-define(C_ROOM_INVITE, "room:invite").
-define(C_ROOM_LEAVE, "room:leave").

-define(C_ROOM_SELF, "room:self").
-define(C_ROOM_ANY, "room:*").

-define(C_MSG_SEND, "msg:send").
-define(C_MSG_REQ, "msg:req").
-define(C_MSG_RECV, "msg:recv").

-define(C_LINK_EXTRACT, "link:extract").

start_link() -> gen_server:start_link(?MODULE, default, []).

init(default) -> {ok, new}.

handle_call({client, Client}, _From, new) -> {reply, ok, Client};

handle_call({handle, Command, Args}, _From, #s_client{} = Client) ->
    {stop, normal, handle(Client, Command, Args), done}.

handle_cast(_Request, Client) -> {noreply, Client}.

handle_info(_Info, Client) -> {noreply, Client}.

terminate(_Reason, _Client) -> ok.

code_change(_OldVsn, #s_client{} = Client, _Extra) -> {ok, Client}.

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

handle(#s_client{identity = undefined}, _Command, _Args) -> unauthorized;

handle(#s_client{} = Client, ?C_AUTH_LOGOUT, [Id]) ->
    [Session] = db:select_session(#t_session{id = Id}),
    db:delete_session(Session),
    true = provider:identity_logout(Session#t_session.provider,
                                    Session#t_session.token),
    {ok, Client#s_client{identity = undefined}};

handle(#s_client{} = Client, ?C_AUTH_CHECK, [Credential]) ->
    [Username, Provider] = string:tokens(Credential, "@"),
    [#t_alias{}] = db:select_alias(#t_alias{provider = Provider,
                                            username = Username}),
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_LIST, []) ->
    User = #t_user{id = Identity},
    [
        send({identity, Identity}, ?C_ROOM_SELF, format(R))
        || R <- db:select_room(db:select_user_room(User))
    ],
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_LIST, [Id]) ->
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [_] = db:select_user_room(#t_user{id = Identity}, Room),
    [
        send({identity, Identity}, ?C_ROOM_ANY, format(Room, U, in))
        || U <- db:select_user(db:select_room_user(Room))
    ],
    {ok, Client};

handle(#s_client{} = Client,
       ?C_ROOM_DISCOVER, []) ->
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_CREATE, [Name, Type]) ->
    [User] = db:select_user(#t_user{id = Identity}),
    Room = #t_room{
        id = uuid:get_v4(),
        name = Name,
        type = Type
    },
    {ok, _} = db:insert_room(Room),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Identity}, ?C_ROOM_SELF, format(Room)),
    [
        send({identity, I}, ?C_ROOM_ANY, format(Room, User, in))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_JOIN, [Id]) ->
    [User] = db:select_user(#t_user{id = Identity}),
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [#t_room{type = ?T_ROOM_PUBLIC}] = db:select_room(Room),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Identity}, ?C_ROOM_SELF, format(Room)),
    [
        send({identity, I}, ?C_ROOM_ANY, format(Room, User, in))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_INVITE, [Id, Credential]) ->
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [_] = db:select_user_room(#t_user{id = Identity}, Room),
    [Username, Provider] = string:tokens(Credential, "@"),
    [#t_alias{} = Alias] = db:select_alias(#t_alias{provider = Provider,
                                                    username = Username}),
    [User] = db:select_user(#t_user{id = Alias#t_alias.user_id}),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Alias#t_alias.user_id}, ?C_ROOM_SELF, format(Room)),
    [
        send({identity, I}, ?C_ROOM_ANY, format(Room, User, in))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    {ok, Client};

handle(#s_client{identity = Identity} = Client,
       ?C_ROOM_LEAVE, [Id]) ->
    [User] = db:select_user(#t_user{id = Identity}),
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    {ok, _} = db:sym_update_user_room(User, Room, false),
    [
        send({identity, I}, ?C_ROOM_ANY, format(Room, User, out))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    {ok, Client};

handle(#s_client{identity = Identity} = Client, ?C_MSG_SEND, [Id, Data]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    Message = #t_message{
        room_id = Room#t_room.id,
        timestamp = now,
        id = uuid:get_v4(),
        user_id = User#t_user.id,
        data = Data
    },
    {ok, _} = db:insert_message(Message),
    [
        send({identity, I}, ?C_MSG_RECV, format(Message))
        || #t_user{id = I} <- db:select_room_user(Room)
    ],
    {ok, Client};

handle(#s_client{identity = Identity} = Client, ?C_MSG_REQ, [Id]) ->
    User = #t_user{id = Identity},
    Room = #t_room{id = uuid:string_to_uuid(Id)},
    [_] = db:select_user_room(User, Room),
    [
        send({identity, Identity}, ?C_MSG_RECV, format(M))
        || M <- db:select_message(#t_message{room_id = Room#t_room.id})
    ],
    {ok, Client};

handle(#s_client{} = Client, ?C_LINK_EXTRACT, [Link]) ->
    {ok, Client, Link}.

send(To, Command, Data) -> socket_receiver_event:send(To, Command, Data).

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

format(#t_room{} = Room, #t_user{} = User, in) ->
    io_lib:format("~s << ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        format(User)
    ]);

format(#t_room{} = Room, #t_user{} = User, out) ->
    io_lib:format("~s >> ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        format(User)
    ]).
