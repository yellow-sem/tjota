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
    url_encode/1
]).

identity_login("yellow", Username, Password) ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request(post, {
        "https://api.tjota.online/api/v1/identity/login/", [],
        "application/x-www-form-urlencoded",
        url_encode([{"username", Username}, {"password", Password}])
    }, [], []),

    {Data} = jiffy:decode(Body),

    case proplists:lookup(<<"token">>, Data) of
        {_, Token} -> ok;
        none -> Token = none
    end,

    case proplists:lookup(<<"success">>, Data) of
        {_, Success} -> ok;
        none -> Success = false
    end,

    {Success, Token};

identity_login(_Provider, _Username, _Password) -> not_implemented.

identity_logout("yellow", Token) ->
    {ok, {_, _, Body}} = httpc:request(post, {
        "https://api.tjota.online/api/v1/identity/logout/",
        [{"Authorization", io_lib:format("Token ~s", [Token])}],
        "application/x-www-form-urlencoded",
        url_encode([])
    }, [], []),

    {Data} = jiffy:decode(Body),

    case proplists:lookup(<<"success">>, Data) of
        {_, Success} -> ok;
        none -> Success = false
    end,

    Success;

identity_logout(_Provider, _Token) -> not_implemented.

courses("yellow", Token) -> not_implemented;
courses(_Provider, Token) -> not_implemented.

course_students("yellow", Token, CourseId) -> not_implemented;
course_students(_Provider, _Token, _CourseId) -> not_implemented.

course_supervisors("yellow", Token, CourseId) -> not_implemented;
course_supervisors(_Provider, _Token, _CourseId) -> not_implemented.

course_assignments("yellow", Token, CourseId) -> not_implemented;
course_assignments(_Provider, _Token, _CourseId) -> not_implemented.

url_encode({K, V}) ->
    io_lib:format("~s=~s", [edoc_lib:escape_uri(K),
                            edoc_lib:escape_uri(V)]);

url_encode(I) -> iolist_to_binary(url_encode(I, "")).

url_encode([], O) -> O;
url_encode([{K, V}|T], "") -> url_encode(T, url_encode({K, V}));
url_encode([{K, V}|T], O) -> url_encode(T, O ++ "&" ++ url_encode({K, V})).
