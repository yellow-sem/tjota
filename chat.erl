-module(chat).
-export([send_message/3]).


send_message(RouterPid, Addresse, Body) ->
    RouterPid ! {send_chat_msg, Addresse, Body},
    receive
        {recv_chat_msg, Body} ->
            Body
    end.
