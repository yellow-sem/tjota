-module(data).
-export([
    request_parse/1,
    response_format/1
]).
-export([
    format/1,
    format/2,
    format/3
]).
-export([
    strip/1,
    replace/3,
    escape/1,
    unescape/1,
    tokens/1
]).

-include("db.hrl").

request_parse(Payload) when is_binary(Payload) ->
    request_parse(binary_to_list(Payload));

request_parse(Payload) ->
    Tokens = tokens(Payload),
    case Tokens of
        [Command, Id|Args] -> ok;
        [Command|Args] -> Id = any;
        _ -> Command = none, Id = any, Args = []
    end,

    {request, Command, Id, Args}.

response_format({response, Command, Id, Data}) ->
    io_lib:format("~s ~s ~s~n", [Command, Id, Data]).

format(#t_user{} = User) ->
    io_lib:format("~s ~s@~s", [
        uuid:uuid_to_string(User#t_user.id),
        User#t_user.username,
        User#t_user.provider
    ]);

format(system) ->
    io_lib:format("~s ~s", [
        "system",
        "system"
    ]);

format(#t_room{} = Room) ->
    io_lib:format("~s '~s' ~s '~s'", [
        uuid:uuid_to_string(Room#t_room.id),
        Room#t_room.name,
        Room#t_room.type,
        Room#t_room.data
    ]);

format(#t_message{user_id = Identity} = Message) ->
    io_lib:format("~s ~p ~s '~s'", [
        uuid:uuid_to_string(Message#t_message.room_id),
        Message#t_message.timestamp,
        format(get_user(Identity)),
        Message#t_message.data
    ]).

format(#t_user{} = User, Status) ->
    io_lib:format("~s '~s'", [
        format(User),
        Status
    ]).

format(#t_room{} = Room, #t_user{} = User, in) ->
    io_lib:format("~s << ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        format(User)
    ]);

format(#t_room{} = Room, #t_user{} = User, out) ->
    io_lib:format("~s >> ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        format(User)
    ]).

get_user(system) -> system;
get_user(undefined) -> system;
get_user(null) -> system;
get_user(Identity) -> [User] = db:select_user(#t_user{id = Identity}), User.

strip(Content) ->
    re:replace(Content, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

replace(Content, ReplaceWhat, ReplaceWith) ->
    re:replace(Content, ReplaceWhat, ReplaceWith, [global, {return, list}]).

escape(Content) -> replace(Content, "\\\\'", "%quote%").
unescape(Content) -> replace(Content, "%quote%", "'").

tokens(Payload) ->
    Message = escape(strip(Payload)),
    {match, Groups} = re:run(Message, "'([^']+)'|([^\s']+)",
                             [{capture, all, list}, global]),
    Tokens = [case Group of [_, _, Token] -> Token; [_, Token] -> Token end
              || Group <- Groups],
    [unescape(Token) || Token <- Tokens].
