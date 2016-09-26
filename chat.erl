-module(chat).
-export([]).


send_message(RouterPid, Addresse, Body) ->
    RouterPid ! {send_chat_msg, Addresse, Body}.
