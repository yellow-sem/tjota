-module(chat_handler).
-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         to_json/2,
         from_json/2]).



init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, from_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
      ], Req, State}.

from_json(Request=#{method := <<"PUT">>}, State) ->
    Body1 = <<"{\"status\": \"ok\"}">>,
    {Body1, Request, State}.

to_json(Request=#{method := <<"GET">>}, State) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Request, State}.


