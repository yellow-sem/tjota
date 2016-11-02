-module(provider).
-export([
    identity_login/3,
    identity_logout/2
]).
-export([
    courses/2,
    course_students/3,
    course_supervisors/3,
    course_assignments/3
]).
-export([
    link_extract/3
]).
-export([
    chat_handle/3,
    chat_rooms/2
]).
-export([
    url_encode/1
]).

-include("provider.hrl").

-define(HTTP_OK, 200).

identity_login("yellow", Username, Password) ->
    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(post, {
        "https://api.tjota.online/api/v1/identity/login/", [],
        "application/x-www-form-urlencoded",
        url_encode([{"username", Username}, {"password", Password}])
    }, [], []),

    {Data} = jiffy:decode(Body),

    {lookup(Data, <<"success">>),
     lookup(Data, <<"token">>)};

identity_login(_Provider, _Username, _Password) -> not_implemented.

identity_logout("yellow", Token) ->
    {ok, {_, _, Body}} = httpc:request(post, {
        "https://api.tjota.online/api/v1/identity/logout/",
        [{"Authorization", io_lib:format("Token ~s", [Token])}],
        "application/x-www-form-urlencoded",
        url_encode([])
    }, [], []),

    {Data} = jiffy:decode(Body),

    lookup(Data, <<"success">>);

identity_logout(_Provider, _Token) -> not_implemented.

courses("yellow", Token) ->
    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        "https://api.tjota.online/api/v1/gul/courses/",
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_course{
        id = decode(lookup(Data, <<"id">>)),
        name = decode(lookup(Data, <<"name">>)),
        category = decode(lookup(Data, <<"category">>)),
        active = lookup(Data, <<"active">>),
        url = decode(lookup(Data, <<"url">>))
    } || {Data} <- jiffy:decode(Body)]};

courses(_Provider, _Token) -> not_implemented.

course_students("yellow", Token, CourseId) ->
    Url = io_lib:format(
        "https://api.tjota.online/api/v1/gul/courses/~p/students/",
        [CourseId]
    ),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_member{
        type = student,
        name = decode(lookup(Data, <<"name">>)),
        alias = decode(lookup(Data, <<"alias">>))
    } || {Data} <- jiffy:decode(Body)]};

course_students(_Provider, _Token, _CourseId) -> not_implemented.

course_supervisors("yellow", Token, CourseId) ->
    Url = io_lib:format(
        "https://api.tjota.online/api/v1/gul/courses/~p/supervisors/",
        [CourseId]
    ),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_member{
        type = supervisor,
        name = decode(lookup(Data, <<"name">>)),
        alias = decode(lookup(Data, <<"alias">>))
    } || {Data} <- jiffy:decode(Body)]};

course_supervisors(_Provider, _Token, _CourseId) -> not_implemented.

course_assignments("yellow", Token, CourseId) ->
    Url = io_lib:format(
        "https://api.tjota.online/api/v1/gul/courses/~p/assignments/",
        [CourseId]
    ),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_assignment{
        id = decode(lookup(Data, <<"id">>)),
        name = decode(lookup(Data, <<"name">>)),
        group = decode(lookup(Data, <<"group">>)),
        url = decode(lookup(Data, <<"url">>)),
        deadline = decode(lookup(Data, <<"deadline">>)),
        status = decode(lookup(Data, <<"status">>))
    } || {Data} <- jiffy:decode(Body)]};

course_assignments(_Provider, _Token, _CourseId) -> not_implemented.

link_extract("yellow", _Token, _Link) -> not_implemented;

link_extract(_Provider, _Token, _Link) -> not_implemented.

chat_handle("yellow", _Token, _Data) -> not_implemented;

chat_handle(_Provider, _Token, _Data) -> not_implemented.

chat_rooms("yellow", _Token) -> not_implemented;

chat_rooms(_Provider, _Token) -> not_implemented.

url_encode({K, V}) ->
    io_lib:format("~s=~s", [edoc_lib:escape_uri(K),
                            edoc_lib:escape_uri(V)]);

url_encode(I) -> iolist_to_binary(url_encode(I, "")).

url_encode([], O) -> O;
url_encode([{K, V}|T], "") -> url_encode(T, url_encode({K, V}));
url_encode([{K, V}|T], O) -> url_encode(T, O ++ "&" ++ url_encode({K, V})).

decode(Value) when is_binary(Value) -> unicode:characters_to_list(Value);
decode(Value) -> Value.

lookup(Data, Key) ->
    case proplists:lookup(Key, Data) of
        {_, Value} -> Value;
        none -> undefined
    end.
