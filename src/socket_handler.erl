-module(socket_handler).
-export([
    handle/3
]).

-include("socket_com.hrl").
-include("db_com.hrl").

handle(#s_client{} = Client, "sys:exit", []) ->
    {ok, Client, stop};

handle(#s_client{} = Client, "auth:login", [Credentials]) ->
    Reply = io_lib:format("credentials ~s", [Credentials]),
    {ok, Client, {reply, Reply}};

handle(#s_client{} = Client, "auth:logout", []) ->
    {ok, Client, noreply};

handle(#s_client{} = Client, "room:list", []) ->
    {ok, Client, noreply};

handle(#s_client{} = Client, "room:create", []) ->
    {ok, Client, noreply};

handle(#s_client{} = Client, "room:leave", []) ->
    {ok, Client, noreply};

handle(#s_client{} = Client, "room:invite", []) ->
    {ok, Client, noreply};

handle(#s_client{} = Client, "msg:fetch", []) ->
    {ok, Client, noreply};

handle(#s_client{} = Client, "msg:send", []) ->
    {ok, Client, noreply};

handle(#s_client{} = Client, _Command, _Args) ->
    {ok, Client, noreply}.
