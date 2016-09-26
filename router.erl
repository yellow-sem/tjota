-module(router).
-export([start/0, stop/1, route_messages/0]).

start() ->
    spawn(router, route_messages, []).

stop(Pid) ->
    Pid ! shutdown.

route_messages() ->
    receive 
        {send_chat_msg, Addresse, Body} ->
            Addresse ! {recv_chat_msg, Body},
            io:format("Send chat message to: ~p~n", [Addresse]),
            route_messages();
        {rect_chat_msg, Body} ->
            io:format("Received: ~p~n", [Body]);
        shutdown ->
            io:format("Shutting down~n");
        Unexpected ->
            io:format("Unexpected message: ~p~n", [Unexpected]),
            route_messages()
    end.
