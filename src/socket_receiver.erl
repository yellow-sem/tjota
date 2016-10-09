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

-include("socket_com.hrl").

-define(TOKEN_SEP, " ").

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
    Tokens = string:tokens(Message, ?TOKEN_SEP),
    case Tokens of
        [Command, Id|Args] -> ok;
        [Command|Args] -> Id = any;
        _ -> Command = none, Id = any, Args = []
    end,

    {ok, NewClient, Result} = socket_handler:handle(Client, Command, Args),

    if
        Client =/= NewClient -> client_change(Client, NewClient);
        true -> ok
    end,

    case Result of
        stop -> {stop, normal, NewClient};

        none -> {noreply, NewClient};

        {send, self, Content} ->
            send(Client, Command, Id, Content),
            {noreply, NewClient};

        {send, all, Content} ->
            Identity = NewClient#s_client.identity,
            gen_event:notify(socket_receiver_event,
                             {send, Identity, Command, Content}),
            {noreply, NewClient}
    end;

handle_info({receiver, {error, Reason}}, #s_client{} = Client) ->
    {stop, Reason, Client};

handle_info({content, {Command, Content}}, #s_client{} = Client) ->
    send(Client, Command, any, Content),
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

send(#s_client{} = Client, Command, Id, Content) ->
    gen_tcp:send(Client#s_client.socket,
                 io_lib:format("~s ~s ~s~n", [Command, Id, Content])).

strip(Content) ->
    re:replace(Content, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).
