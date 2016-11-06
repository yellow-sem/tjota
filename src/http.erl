-module(http).
-export([
    start_link/0
]).
-export([
    loop/1,
    handle/3
]).
-export([
    manager_init/2,
    manager_loop/1
]).

-include("http.hrl").
-include("socket.hrl").
-include("data.hrl").

start_link() ->
    mochiweb_http:start_link([{name, client_access},
                              {loop, {?MODULE, loop, []}},
                              {port, ?HTTP_PORT}]).

loop(Request) ->
    Path = Request:get(path),
    Header = mochiweb_request:get_header_value("Upgrade", Request),

    if
        Header == "websocket" ->
            Handle = fun ?MODULE:handle/3,
            {Start, Reply} = mochiweb_websocket:upgrade_connection(Request,
                                                                   Handle),

            Client = #s_client{socket = Reply,
                               address = websocket},
            Manager = spawn(?MODULE, manager_init, [self(), Client]),
            Start({state, Client, Manager});

        true ->
            Request:ok({_ContentType = "text/plain",
                        _Headers = [],
                        Path})
    end.

handle(Data, State, _) ->
    lists:last([handle(Payload, State)
                || Payload <- string:tokens(Data, "\n")]).

handle(Payload, {state, Client, Manager}) ->
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
            Manager ! {identity, NewIdentity};

        true -> ok
    end,

    case Result of
        stop -> send(Client, Command, Id, ?R_SUCCESS), exit(normal);
        ok -> send(Client, Command, Id, ?R_SUCCESS);
        err -> send(Client, Command, Id, ?R_ERROR);
        {ok, Data} -> send(Client, Command, Id, Data)
    end,

    {state, NewClient, Manager}.

manager_init(Monitor, Client) ->
    monitor(process, Monitor),
    manager_loop(Client#s_client{receiver = self()}).

manager_loop(Client) ->
    receive
        {identity, NewIdentity} ->
            NewClient = Client#s_client{identity = NewIdentity},
            command_event:subscribe(NewClient#s_client.receiver,
                                    NewClient#s_client.identity),
            manager_loop(NewClient);

        {send, {Command, Data}} ->
            send(Client, Command, any, Data),
            manager_loop(Client);

        {'DOWN', _, process, _, _Reason} ->
            command_event:unsubscribe(Client#s_client.receiver),
            ok;

        _ ->
            manager_loop(Client)
    end.

send(Client, Command, Id, Data) ->
    Reply = Client#s_client.socket,
    Reply(data:response_format({response, Command, Id, Data})).
