-module(socket_tcp_receiver).
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

-include("socket.hrl").
-include("data.hrl").

start_link() -> gen_server:start_link(?MODULE, default, []).

init(default) -> init(#s_client{});

init(#s_client{} = Client) -> {ok, Client}.

handle_call(_Request, _From, #s_client{} = Client) -> {noreply, Client}.

handle_cast({socket, Socket}, #s_client{} = _Client) ->

    {ok, {Host, Port}} = inet:peername(Socket),

    gen_tcp:send(Socket,
                 io_lib:format("start ~w ~n", [Host])),

    socket_tcp_util:receiver_start(Socket),

    {noreply, #s_client{socket = Socket,
                        address = #s_address{host = Host, port = Port},
                        receiver = self()}}.

handle_info({receiver, {payload, Payload}}, #s_client{} = Client) ->

    {request, Command, Id, Args} = data:request_parse(Payload),

    {ok, Process} = supervisor:start_child(command_handler_sup, []),
    ok = gen_server:call(Process, {identity, Client#s_client.identity}),
    Handle = (catch gen_server:call(Process, {handle, Command, Args})),

    Result = case Handle of
        {ok, NewIdentity, stop} -> stop;
        {ok, NewIdentity} -> ok;
        {ok, NewIdentity, Other} -> {ok, Other};
        _ -> NewIdentity = Client#s_client.identity, err
    end,

    NewClient = Client#s_client{identity = NewIdentity},

    if
        Client#s_client.identity =/= NewClient#s_client.identity ->
            command_event:subscribe(NewClient#s_client.receiver,
                                    NewClient#s_client.identity);

        true -> ok
    end,

    case Result of
        stop ->
            send(Client, Command, Id, ?R_SUCCESS),
            {stop, normal, NewClient};

        ok ->
            send(Client, Command, Id, ?R_SUCCESS),
            {noreply, NewClient};

        err ->
            send(Client, Command, Id, ?R_ERROR),
            {noreply, NewClient};

        {ok, Data} ->
            send(Client, Command, Id, Data),
            {noreply, NewClient}
    end;

handle_info({receiver, {error, Reason}}, #s_client{} = Client) ->
    {stop, Reason, Client};

handle_info({send, {Command, Data}}, #s_client{} = Client) ->
    send(Client, Command, any, Data),
    {noreply, Client}.

terminate(_Reason, #s_client{} = Client) ->
    command_event:unsubscribe(Client#s_client.receiver),
    gen_tcp:close(Client#s_client.socket).

code_change(_OldVsn, #s_client{} = Client, _Extra) -> {ok, Client}.

send(#s_client{} = Client, Command, Id, Data) ->
    gen_tcp:send(Client#s_client.socket,
                 data:response_format({response, Command, Id, Data})).
