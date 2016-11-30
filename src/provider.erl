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
    extract/3
]).
-export([
    chat_handle/4,
    chat_rooms/2
]).
-export([
    url_encode/1
]).

-include("provider.hrl").

-define(HTTP_OK, 200).

base("yellow") -> "https://api.tjota.online";
base(Domain) -> io_lib:format("https://~s", [Domain]).

%% ------------------------------------------------------------------
%% Identity
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Authenticate with the specified credentials.
%% @end
%% ------------------------------------------------------------------
identity_login(Provider, Username, Password) ->

    % Build URL
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/identity/login/",
        [base(Provider)]
    )),

    % Send request
    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(post, {
        Url, [],
        "application/x-www-form-urlencoded",
        url_encode([{"username", Username}, {"password", Password}])
    }, [], []),

    % Decode response
    {Data} = jiffy:decode(Body),

    % Return tuple of `{Success, Token}` where
    % `Success` is `true` or `false`
    % `Token` is a string token or `none`
    {lookup(Data, <<"success">>),
     lookup(Data, <<"token">>)}.

identity_logout(Provider, Token) ->
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/identity/logout/",
        [base(Provider)]
    )),

    {ok, {_, _, Body}} = httpc:request(post, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}],
        "application/x-www-form-urlencoded",
        url_encode([])
    }, [], []),

    {Data} = jiffy:decode(Body),

    lookup(Data, <<"success">>).

%% ------------------------------------------------------------------
%% Courses
%% ------------------------------------------------------------------

courses(Provider, Token) ->
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/courses/",
        [base(Provider)]
    )),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_course{
        id = decode(lookup(Data, <<"id">>)),
        name = decode(lookup(Data, <<"name">>)),
        category = decode(lookup(Data, <<"category">>)),
        active = lookup(Data, <<"active">>),
        url = decode(lookup(Data, <<"url">>))
    } || {Data} <- jiffy:decode(Body)]}.

course_students(Provider, Token, CourseId) ->
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/courses/~p/students/",
        [base(Provider), CourseId]
    )),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_member{
        type = student,
        name = decode(lookup(Data, <<"name">>)),
        alias = decode(lookup(Data, <<"alias">>))
    } || {Data} <- jiffy:decode(Body)]}.

course_supervisors(Provider, Token, CourseId) ->
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/courses/~p/supervisors/",
        [base(Provider), CourseId]
    )),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_member{
        type = supervisor,
        name = decode(lookup(Data, <<"name">>)),
        alias = decode(lookup(Data, <<"alias">>))
    } || {Data} <- jiffy:decode(Body)]}.

course_assignments(Provider, Token, CourseId) ->
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/courses/~p/assignments/",
        [base(Provider), CourseId]
    )),

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
    } || {Data} <- jiffy:decode(Body)]}.

%% ------------------------------------------------------------------
%% Links
%% ------------------------------------------------------------------

extract(_Provider, _Token, _Url) -> not_implemented.

%% ------------------------------------------------------------------
%% Chat
%% ------------------------------------------------------------------

chat_handle(Provider, Token, Text, RoomId) ->
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/chat/handle/",
        [base(Provider)]
    )),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(post, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}],
        "application/x-www-form-urlencoded",
        url_encode([{"text", Text}, {"room_id", RoomId}])
    }, [], []),

    {Data} = jiffy:decode(Body),

    {true, decode(lookup(Data, <<"text">>))}.

chat_rooms(Provider, Token) ->
    Url = iolist_to_string(io_lib:format(
        "~s/api/v1/chat/rooms/",
        [base(Provider)]
    )),

    {ok, {{_, ?HTTP_OK, _}, _, Body}} = httpc:request(get, {
        Url,
        [{"Authorization", io_lib:format("Token ~s", [Token])}]
    }, [], []),

    {true, [#p_room{
        id = decode(lookup(Data, <<"id">>)),
        name = decode(lookup(Data, <<"name">>)),
        type = decode(lookup(Data, <<"type">>)),
        role = decode(lookup(Data, <<"role">>))
    } || {Data} <- jiffy:decode(Body)]}.

%% ------------------------------------------------------------------
%% Utilities
%% ------------------------------------------------------------------

url_encode({K, V}) ->
    io_lib:format("~s=~s", [edoc_lib:escape_uri(K),
                            edoc_lib:escape_uri(V)]);

url_encode(I) -> iolist_to_binary(url_encode(I, "")).

url_encode([], O) -> O;
url_encode([{K, V}|T], "") -> url_encode(T, url_encode({K, V}));
url_encode([{K, V}|T], O) -> url_encode(T, O ++ "&" ++ url_encode({K, V})).

decode(Value) when is_binary(Value) -> unicode:characters_to_list(Value);
decode(Value) -> Value.

iolist_to_string(Value) ->
    unicode:characters_to_list(erlang:iolist_to_binary(Value)).

lookup(Data, Key) ->
    case proplists:lookup(Key, Data) of
        {_, Value} -> Value;
        none -> undefined
    end.
