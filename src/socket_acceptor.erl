-module(socket_acceptor).
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

-include("socket_rec.hrl").

start_link() -> gen_server:start_link(?MODULE, default, []).

init(default) -> init(#s_server{});

init(#s_server{} = Server) ->
    Address = Server#s_server.address,

    Options = [
        binary,
        {packet, raw},
        {active, false},
        {ip, Address#s_address.host}
    ],

    case gen_tcp:listen(Address#s_address.port, Options) of
        {ok, Socket} ->
            socket_util:acceptor_start(Socket),

            {ok, #s_server{socket = Socket,
                           address = Address,
                           acceptor = self()}};

        {error, Reason} -> {stop, Reason}
    end.

handle_call(_Request, _From, #s_server{} = Server) -> {noreply, Server}.

handle_cast(_Request, #s_server{} = Server) -> {noreply, Server}.

handle_info({acceptor, {socket, Socket}}, #s_server{} = Server) ->
    {ok, Process} = supervisor:start_child(socket_receiver_sup, []),
    gen_tcp:controlling_process(Socket, Process),
    gen_server:cast(Process, {socket, Socket}),
    {noreply, Server};

handle_info({acceptor, {error, Reason}}, #s_server{} = Server) ->
    {stop, Reason, Server}.

terminate(_Reason, #s_server{} = Server) ->
    gen_tcp:close(Server#s_server.socket).

code_change(_OldVsn, #s_server{} = Server, _Extra) -> {ok, Server}.
