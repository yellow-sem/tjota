-module(tjota_db).
-export([
    bootstrap/0
]).
-export([
    insert_alias/1,
    update_alias/1,
    select_alias/1
]).
-export([
    insert_user/1,
    update_user/1,
    delete_user/1,
    select_user/1
]).
-export([
    insert_session/1,
    delete_session/1,
    select_session/1
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
    sym_insert_user_room/3,
    sym_update_user_room/3,
    sym_delete_user_room/2
]).
-export([
    select_user_room/1,
    select_room_user/1
]).

-include("tjota_reg.hrl").
-include("cqerl.hrl").

-define(KEYSPACE, "tjota").

-define(TABLE_USER, "user").
-define(TABLE_ALIAS, "alias").
-define(TABLE_SESSION, "session").
-define(TABLE_ROOM, "room").
-define(TABLE_MESSAGE, "message").

-define(TABLE_USER_ROOM, "user_room").
-define(TABLE_ROOM_USER, "room_user").

bootstrap() ->
    create_keyspace(),
    create_table([
        user,
        alias,
        session,
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
            id UUID,
            alias TEXT,
            name TEXT,
            password TEXT,
            active BOOLEAN,
            PRIMARY KEY (id)
        )
    ", [?KEYSPACE, ?TABLE_USER]));

create_table(alias) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            alias TEXT,
            user_id UUID,
            PRIMARY KEY (alias)
        )
    ", [?KEYSPACE, ?TABLE_ALIAS]));

create_table(session) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            id UUID,
            user_id UUID,
            PRIMARY KEY (id)
        )
    ", [?KEYSPACE, ?TABLE_SESSION]));

create_table(room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            id UUID,
            name TEXT,
            type TEXT,
            data TEXT,
            PRIMARY KEY (id)
        )
    ", [?KEYSPACE, ?TABLE_ROOM]));

create_table(message) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            room_id UUID,
            timestamp TIMESTAMP,
            id UUID,
            user_id UUID,
            data TEXT,
            PRIMARY KEY (room_id, timestamp, id)
        )
    ", [?KEYSPACE, ?TABLE_MESSAGE]));

create_table(user_room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            user_id UUID,
            room_id UUID,
            PRIMARY KEY (user_id, room_id)
        )
    ", [?KEYSPACE, ?TABLE_USER_ROOM]));

create_table(room_user) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            room_id UUID,
            user_id UUID,
            PRIMARY KEY (room_id, user_id)
        )
    ", [?KEYSPACE, ?TABLE_ROOM_USER])).


% `alias` table

insert_alias(#t_alias{} = Alias) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (alias, user_id)
            VALUES (?, ?)
            IF NOT EXISTS
        ", [?KEYSPACE, ?TABLE_ALIAS]),
        values = [
            {alias, Alias#t_alias.alias},
            {user_id, Alias#t_alias.user_id}
        ]
    }).

update_alias(#t_alias{} = Alias) -> insert_alias(Alias).

select_alias(#t_alias{} = Alias) -> not_implemented.


% `user` table

insert_user(#t_user{} = User) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = insert_alias(#t_alias{
        alias = User#t_user.alias,
        user_id = User#t_user.id
    }),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (id, alias, name, password, active)
            VALUES (?, ?, ?, ?, ?)
        ", [?KEYSPACE, ?TABLE_USER]),
        values = [
            {id, User#t_user.id},
            {alias, User#t_user.alias},
            {name, User#t_user.name},
            {password, User#t_user.password},
            {active, User#t_user.active}
        ]
    }).

update_user(#t_user{} = User) -> insert_user(User).

delete_user(#t_user{} = User) -> update_user(User#t_user{active = false}).

select_user(#t_user{} = User) -> not_implemented.


% `session` table

insert_session(#t_session{} = Session) -> not_implemented.
delete_session(#t_session{} = Session) -> not_implemented.
select_session(#t_session{} = Session) -> not_implemented.


% `room` table

insert_room(#t_room{} = Room) -> not_implemented.
update_room(#t_room{} = Room) -> insert_room(Room).
delete_room(#t_room{} = Room) -> not_implemented.
select_room(#t_room{} = Room) -> not_implemented.


% `message` table

insert_message(#t_message{} = Message) -> not_implemented.
select_message(#t_message{} = Message) -> not_implemented.


% `user_room` `room_user` symmetric shortcuts

sym_insert_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    {ok, _} = insert_user_room(User, Room, Active),
    {ok, _} = insert_room_user(Room, User, Active).

sym_update_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    {ok, _} = update_user_room(User, Room, Active),
    {ok, _} = update_room_user(Room, User, Active).

sym_delete_user_room(#t_user{} = User, #t_room{} = Room) ->
    {ok, _} = delete_user_room(User, Room),
    {ok, _} = delete_room_user(Room, User).


% `user_room` table

insert_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    not_implemented.

update_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    insert_user_room(User, Room, Active).

delete_user_room(#t_user{} = User, #t_room{} = Room) -> not_implemented.

select_user_room(#t_user{} = User) -> not_implemented.


% `room_user` table

insert_room_user(#t_room{} = Room, #t_user{} = User, Active) ->
    not_implemented.

update_room_user(#t_room{} = Room, #t_user{} = User, Active) ->
    insert_room_user(Room, User, Active).

delete_room_user(#t_room{} = Room, #t_user{} = User) -> not_implemented.

select_room_user(#t_room{} = Room) -> not_implemented.
