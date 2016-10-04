%%%-------------------------------------------------------------------
%% @doc tjota public API
%% @end
%%%-------------------------------------------------------------------

-module(hello_handler).
-export([init/2]).

init(Request, State) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Hello Erlang!">>,
                           Request),
    {ok, Req, State}.


