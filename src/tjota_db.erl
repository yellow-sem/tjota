-module(tjota_db).
-export([
    bootstrap/0
]).
-export([
    insert_user/1,
    update_user/1,
    delete_user/1,
    select_user/1
]).
-export([
    insert_room/1,
    update_room/1,
    delete_room/1,
    select_room/1
]).
-export([
    insert_message/1,
    select_message/1
]).
-export([
    sym_insert_user_room/2,
    sym_delete_user_room/2
]).
-export([
    select_user_room/1,
    select_room_user/1
]).

-include("tjota_reg.hrl").

-define(KEYSPACE, "tjota").

-define(TABLE_USER, "user").
-define(TABLE_ROOM, "room").
-define(TABLE_MESSAGE, "message").

-define(TABLE_USER_ROOM, "user_room").
-define(TABLE_ROOM_USER, "room_user").

bootstrap() ->
    create_keyspace(),
    create_table([
        user,
        room,
        message
    ]),
    create_table([
        user_room,
        room_user
    ]).

get_cqerl_client() -> cqerl:get_client({}).

create_keyspace() ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE KEYSPACE IF NOT EXISTS ~s
        WITH replication = {
            'class': 'SimpleStrategy',
            'replication_factor': 1
        }
    ", [?KEYSPACE])).

create_table([]) -> ok;
create_table([H|T]) -> create_table(H), create_table(T);

create_table(user) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            alias TEXT,
            name TEXT,
            password TEXT,
            id UUID,
            PRIMARY KEY (alias)
        )
    ", [?KEYSPACE, ?TABLE_USER]));

create_table(room) -> not_implemented;
create_table(message) -> not_implemented;

create_table(user_room) -> not_implemented;
create_table(room_user) -> not_implemented.

insert_user(#t_user{} = User) -> not_implemented.
update_user(#t_user{} = User) -> not_implemented.
delete_user(#t_user{} = User) -> not_implemented.
select_user(#t_user{} = User) -> not_implemented.

insert_room(#t_room{} = Room) -> not_implemented.
update_room(#t_room{} = Room) -> not_implemented.
delete_room(#t_room{} = Room) -> not_implemented.
select_room(#t_room{} = Room) -> not_implemented.

insert_message(#t_message{} = Message) -> not_implemented.
select_message(#t_message{} = Message) -> not_implemented.

sym_insert_user_room(#t_user{} = User, #t_room{} = Room) ->
    insert_user_room(User, Room),
    insert_room_user(Room, User).

sym_delete_user_room(#t_user{} = User, #t_room{} = Room) ->
    delete_user_room(User, Room),
    delete_room_user(Room, User).

insert_user_room(#t_user{} = User, #t_room{} = Room) -> not_implemented.
delete_user_room(#t_user{} = User, #t_room{} = Room) -> not_implemented.
select_user_room(#t_user{} = User) -> not_implemented.

insert_room_user(#t_room{} = Room, #t_user{} = User) -> not_implemented.
delete_room_user(#t_room{} = Room, #t_user{} = User) -> not_implemented.
select_room_user(#t_user{} = User) -> not_implemented.
