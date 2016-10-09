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

    {noreply, #s_client{socket = Socket,
                        address = #s_address{host = Host, port = Port},
                        receiver = socket_util:receiver_start(Socket)}}.

handle_info({receiver, {payload, Payload}}, #s_client{} = Client) ->
    Message = strip(binary_to_list(Payload)),
    [Command|Args] = string:tokens(Message, ?TOKEN_SEP),
    {ok, NewClient, Result} = socket_handler:handle(Client, Command, Args),
    case Result of
        stop -> {stop, normal, NewClient};
        noreply -> {noreply, NewClient};
        {reply, Reply} ->
            gen_tcp:send(Client#s_client.socket,
                         io_lib:format("~s ~s~n", [Command, Reply])),
            {noreply, NewClient}
    end;

handle_info({receiver, {error, Reason}}, #s_client{} = Client) ->
    {stop, Reason, Client}.

terminate(_Reason, #s_client{} = Client) ->
    gen_tcp:close(Client#s_client.socket).

code_change(_OldVsn, #s_client{} = Client, _Extra) -> {ok, Client}.

strip(Content) ->
    re:replace(Content, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).
