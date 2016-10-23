-module(socket_receiver).
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

-define(R_SUCCESS, "ok").
-define(R_ERROR, "err").

start_link() -> gen_server:start_link(?MODULE, default, []).

init(default) -> init(#s_client{});

init(#s_client{} = Client) -> {ok, Client}.

handle_call(_Request, _From, #s_client{} = Client) -> {noreply, Client}.

handle_cast({socket, Socket}, #s_client{} = _Client) ->

    {ok, {Host, Port}} = inet:peername(Socket),

    gen_tcp:send(Socket,
                 io_lib:format("start ~w ~n", [Host])),

    socket_util:receiver_start(Socket),

    {noreply, #s_client{socket = Socket,
                        address = #s_address{host = Host, port = Port},
                        receiver = self()}}.

handle_info({receiver, {payload, Payload}}, #s_client{} = Client) ->
    Message = strip(binary_to_list(Payload)),
    {match, Groups} = re:run(Message, "'([^']+)'|([^\s']+)",
                             [{capture, all, list}, global]),
    Tokens = [case Group of [_, _, Token] -> Token; [_, Token] -> Token end
              || Group <- Groups],
    case Tokens of
        [Command, Id|Args] -> ok;
        [Command|Args] -> Id = any;
        _ -> Command = none, Id = any, Args = []
    end,

    {ok, Process} = supervisor:start_child(socket_handler_sup, []),
    ok = gen_server:call(Process, {client, Client}),
    Handle = (catch gen_server:call(Process, {handle, Command, Args})),

    Result = case Handle of
        {ok, NewClient, stop} -> stop;
        {ok, NewClient} -> ok;
        {ok, NewClient, Other} -> {ok, Other};
        _ -> NewClient = Client, err
    end,

    if
        Client =/= NewClient -> client_change(Client, NewClient);
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
    gen_tcp:close(Client#s_client.socket).

code_change(_OldVsn, #s_client{} = Client, _Extra) -> {ok, Client}.

client_change(#s_client{} = OldClient, #s_client{} = NewClient) ->

    if OldClient#s_client.identity =/= NewClient#s_client.identity ->

        gen_event:delete_handler(socket_receiver_event,
                                 {socket_receiver_event, self()},
                                 []),

        gen_event:add_handler(socket_receiver_event,
                              {socket_receiver_event, self()},
                              NewClient)

    end.

send(#s_client{} = Client, Command, Id, Data) ->
    gen_tcp:send(Client#s_client.socket,
                 io_lib:format("~s ~s ~s~n", [Command, Id, Data])).

strip(Content) ->
    re:replace(Content, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).
