-module(socket_handler).
-export([
    handle/3
]).

-include("socket_com.hrl").
-include("db_com.hrl").

-define(C_SYS_EXIT, "sys:exit").

-define(C_AUTH_LOGIN, "auth:login").
-define(C_AUTH_LOGOUT, "auth:logout").

-define(C_ROOM_LIST, "room:list").
-define(C_ROOM_CREATE, "room:create").
-define(C_ROOM_JOIN, "room:join").
-define(C_ROOM_LEAVE, "room:leave").
-define(C_ROOM_INVITE, "room:invite").

-define(C_MSG_RECV, "msg:recv").
-define(C_MSG_SEND, "msg:send").

handle(#s_client{} = Client, ?C_SYS_EXIT, []) ->
    {ok, Client, stop};

handle(#s_client{} = Client, ?C_AUTH_LOGIN, [Identity, Password]) ->
    [Username, Provider] = string:tokens(Identity, "@"),

    Content = io_lib:format("credentials ~s ok", [Username]),

    {ok, Client#s_client{identity = "myident"}, {send, self, Content}};

handle(#s_client{} = Client, ?C_AUTH_LOGOUT, []) ->
    {ok, Client, {send, self, "ok"}};

handle(#s_client{} = Client, ?C_ROOM_LIST, []) ->
    {ok, Client, {send, self, "list"}};

handle(#s_client{} = Client, ?C_ROOM_CREATE, []) ->
    {ok, Client, {send, all, "create"}};

handle(#s_client{} = Client, ?C_ROOM_JOIN, []) ->
    {ok, Client, {send, all, "join"}};

handle(#s_client{} = Client, ?C_ROOM_LEAVE, []) ->
    {ok, Client, {send, all, "leave"}};

handle(#s_client{} = Client, ?C_ROOM_INVITE, []) ->
    {ok, Client, {send, all, "invite"}};

handle(#s_client{} = Client, ?C_MSG_RECV, []) ->
    {ok, Client, {send, self, "messages"}};

handle(#s_client{} = Client, ?C_MSG_SEND, [Message]) ->
    gen_event:notify(socket_receiver_event,
                     {send, "anotheruser", ?C_MSG_RECV, Message}),
    {ok, Client, {send, self, "ok"}};

handle(#s_client{} = Client, _Command, _Args) ->
    {ok, Client, none}.
