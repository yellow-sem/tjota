-module(socket_tcp_util).

-export([
    acceptor_start/1,
    acceptor_loop/1
]).

-export([
    receiver_start/1,
    receiver_loop/1
]).

-include("socket.hrl").

acceptor_start(Socket) when is_port(Socket) ->
    acceptor_start(#s_handler{socket = Socket, owner = self()});

acceptor_start(#s_handler{} = Handler) ->
    proc_lib:spawn_link(?MODULE, acceptor_loop, [Handler]).

acceptor_loop(#s_handler{} = Handler) ->
    case gen_tcp:accept(Handler#s_handler.socket) of
        {ok, Socket} ->
            Handler#s_handler.owner ! {acceptor, {socket, Socket}},
            acceptor_loop(Handler);

        {error, Reason} ->
            Handler#s_handler.owner ! {acceptor, {error, Reason}}
    end.

receiver_start(Socket) when is_port(Socket) ->
    receiver_start(#s_handler{socket = Socket, owner = self()});

receiver_start(#s_handler{} = Handler) ->
    proc_lib:spawn_link(?MODULE, receiver_loop, [Handler]).

receiver_loop(#s_handler{} = Handler) ->
    case gen_tcp:recv(Handler#s_handler.socket, 0) of
        {ok, Payload} ->
            Handler#s_handler.owner ! {receiver, {payload, Payload}},
            receiver_loop(Handler);

        {error, Reason} ->
            Handler#s_handler.owner ! {receiver, {error, Reason}}
    end.
