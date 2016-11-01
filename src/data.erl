-module(data).
-export([
    format/1,
    format/3
]).

-include("db.hrl").

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
    io_lib:format("~s '~s' ~s", [
        uuid:uuid_to_string(Room#t_room.id),
        Room#t_room.name,
        Room#t_room.type
    ]);

format(#t_message{user_id = Identity} = Message) ->
    io_lib:format("~s ~p ~s '~s'", [
        uuid:uuid_to_string(Message#t_message.room_id),
        Message#t_message.timestamp,
        format(get_user(Identity)),
        Message#t_message.data
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
