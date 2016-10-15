-module(socket_handler).
-export([
    handle/3
]).

-include("socket_com.hrl").
-include("db_com.hrl").

-define(R_ERROR, "err").
-define(R_SUCCESS, "ok").
-define(R_UNKNOWN, "unknown").

-define(C_SYS_EXIT, "sys:exit").

-define(C_AUTH_LOGIN, "auth:login").
-define(C_AUTH_LOGOUT, "auth:logout").

-define(C_ROOM_LIST, "room:list").
-define(C_ROOM_CREATE, "room:create").
-define(C_ROOM_DISCOVER, "room:discover").
-define(C_ROOM_JOIN, "room:join").
-define(C_ROOM_LEAVE, "room:leave").
-define(C_ROOM_INVITE, "room:invite").

-define(C_MSG_RECV, "msg:recv").
-define(C_MSG_SEND, "msg:send").

-define(C_LINK_EXTRACT, "link:extract").

handle(#s_client{} = Client, ?C_SYS_EXIT, []) ->
    {ok, Client, stop};

handle(#s_client{} = Client, ?C_AUTH_LOGIN, [Id]) ->
    case db_auth:login(#t_session{id = Id}) of
        none -> {ok, Client, {send, self, ?R_ERROR}};
        {session, Session} ->
            {
                ok, Client#s_client{identity = Session#t_session.user_id},
                {send, self, uuid:uuid_to_string(Session#t_session.id)}
            }
    end;

handle(#s_client{} = Client, ?C_AUTH_LOGIN, [Credential, Password]) ->
    [Username, Provider] = string:tokens(Credential, "@"),
    {Success, Token} = provider:identity_login(Provider, Username, Password),
    case Success of
        true ->
            {session, Session} = db_auth:login(Provider, Username, Token),
            {
                ok, Client#s_client{identity = Session#t_session.user_id},
                {send, self, uuid:uuid_to_string(Session#t_session.id)}
            };
        false ->
            {ok, Client, {send, self, ?R_ERROR}}
    end;

handle(#s_client{} = Client, ?C_AUTH_LOGOUT, [Id]) ->
    case db:select_session(#t_session{id = Id}) of
        [Session] ->
            Success = provider:identity_logout(Session#t_session.provider,
                                               Session#t_session.token),
            db:delete_session(Session),

            case Success of
                true -> {
                    ok, Client#s_client{identity = undefined},
                    {send, self, ?R_SUCCESS}
                };
                false -> {
                    ok, Client#s_client{identity = undefined},
                    {send, self, ?R_ERROR}
                }
            end;

        [] ->
            {ok, Client, {send, self, ?R_ERROR}}
    end;

handle(#s_client{identity = Identity} = Client, ?C_ROOM_LIST, []) ->
    User = #t_user{id = Identity},
    [
        send({identity, Identity}, ?C_ROOM_JOIN, format(Room)) ||
        Room <- db:select_room(db:select_user_room(User))
    ],
    {ok, Client, {send, self, ?R_SUCCESS}};

handle(#s_client{identity = Identity} = Client, ?C_ROOM_CREATE, [Name]) ->
    User = #t_user{id = Identity},
    Room = #t_room{
        id = uuid:get_v4(),
        name = Name,
        type = custom
    },
    {ok, _} = db:insert_room(Room),
    {ok, _} = db:sym_insert_user_room(User, Room, true),
    send({identity, Identity}, ?C_ROOM_JOIN, format(Room)),
    {ok, Client, {send, self, ?R_SUCCESS}};

handle(#s_client{} = Client, ?C_ROOM_DISCOVER, []) ->
    {ok, Client, {send, all, "discover"}};

handle(#s_client{} = Client, ?C_ROOM_JOIN, []) ->
    {ok, Client, {send, all, "join"}};

handle(#s_client{} = Client, ?C_ROOM_LEAVE, []) ->
    {ok, Client, {send, all, "leave"}};

handle(#s_client{} = Client, ?C_ROOM_INVITE, []) ->
    {ok, Client, {send, self, "invite"}};

handle(#s_client{} = Client, ?C_MSG_RECV, []) ->
    {ok, Client, none};

handle(#s_client{} = Client, ?C_MSG_SEND, [Message]) ->
    gen_event:notify(socket_receiver_event,
                     {send, {identity, "anotheruser"}, ?C_MSG_RECV, Message}),
    {ok, Client, {send, self, "ok"}};

handle(#s_client{} = Client, ?C_LINK_EXTRACT, [Link]) ->
    {ok, Client, {send, self, Link}};

handle(#s_client{} = Client, _Command, _Args) ->
    {ok, Client, {send, self, ?R_UNKNOWN}}.

send(To, Command, Data) ->
    gen_event:notify(socket_receiver_event, {send, To, Command, Data}).

format(#t_user{} = User) ->
    io_lib:format("~s@~s '~s'", [
        User#t_user.username,
        User#t_user.provider,
        User#t_user.name
    ]);

format(#t_room{} = Room) ->
    io_lib:format("~s '~s' ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        Room#t_room.name,
        Room#t_room.type
    ]).
