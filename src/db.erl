-module(db).
-export([
    bootstrap/0
]).
-export([
    insert_alias/1,
    select_alias/1
]).
-export([
    insert_user/1,
    update_user/1,
    select_user/1
]).
-export([
    insert_session/1,
    delete_session/1,
    select_session/1
]).
-export([
    insert_token/1,
    select_token/1
]).
-export([
    insert_room/1,
    update_room/1,
    select_room/1
]).
-export([
    insert_resource/1,
    delete_resource/1,
    select_resource/1
]).
-export([
    insert_message/1,
    select_message/1
]).
-export([
    select_user_room/1,
    select_user_room/2,
    select_room_user/1,
    select_room_user/2
]).
-export([
    sym_insert_user_room/3,
    sym_update_user_room/3
]).

-include("cqerl.hrl").
-include("db.hrl").

-define(KEYSPACE, "tjota").

-define(TABLE_USER, "user").
-define(TABLE_ALIAS, "alias").
-define(TABLE_SESSION, "session").
-define(TABLE_TOKEN, "\"token\"").
-define(TABLE_ROOM, "room").
-define(TABLE_RESOURCE, "resource").
-define(TABLE_MESSAGE, "message").

-define(TABLE_USER_ROOM, "user_room").
-define(TABLE_ROOM_USER, "room_user").

%% ------------------------------------------------------------------
%% Bootstrap
%% ------------------------------------------------------------------

bootstrap() ->
    create_keyspace(),
    create_table([
        user,
        alias,
        session,
        token,
        room,
        resource,
        message
    ]),
    create_table([
        user_room,
        room_user
    ]).

%% ------------------------------------------------------------------
%% Utilities
%% ------------------------------------------------------------------

decode(Value) when is_binary(Value) -> unicode:characters_to_list(Value);
decode(Value) -> Value.

encode(Value) when is_atom(Value) -> Value;
encode(Value) -> unicode:characters_to_binary(Value).

get_cqerl_client() -> cqerl:get_client({}).

%% ------------------------------------------------------------------
%% Tables
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Create database.
%% @end
%% ------------------------------------------------------------------
create_keyspace() ->

    % Obtain CQL client
    {ok, Client} = get_cqerl_client(),

    % Execute `CREATE KEYSPACE` query
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE KEYSPACE IF NOT EXISTS ~s
        WITH replication = {
            'class': 'SimpleStrategy',
            'replication_factor': 1
        }
    ", [?KEYSPACE])).

%% ------------------------------------------------------------------
%% @doc Create the specified list of tables.
%% @end
%% ------------------------------------------------------------------
create_table([]) -> ok;
create_table([H|T]) -> create_table(H), create_table(T);

%% ------------------------------------------------------------------
%% @doc Create the `user` table.
%% @end
%% ------------------------------------------------------------------
create_table(user) ->

    % Obtain CQL client
    {ok, Client} = get_cqerl_client(),

    % Execute `CREATE TABLE` query
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            id UUID,
            provider TEXT,
            username TEXT,
            status TEXT,
            active BOOLEAN,
            PRIMARY KEY (id)
        )
    ", [?KEYSPACE, ?TABLE_USER]));

%% ------------------------------------------------------------------
%% @doc Create the `alias` table.
%% @end
%% ------------------------------------------------------------------
create_table(alias) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            provider TEXT,
            username TEXT,
            user_id UUID,
            PRIMARY KEY ((provider, username))
        )
    ", [?KEYSPACE, ?TABLE_ALIAS]));

%% ------------------------------------------------------------------
%% @doc Create the `session` table.
%% @end
%% ------------------------------------------------------------------
create_table(session) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            id UUID,
            user_id UUID,
            provider TEXT,
            \"token\" TEXT,
            PRIMARY KEY (id)
        )
    ", [?KEYSPACE, ?TABLE_SESSION]));

%% ------------------------------------------------------------------
%% @doc Create the `token` table.
%% @end
%% ------------------------------------------------------------------
create_table(token) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            user_id UUID,
            provider TEXT,
            \"token\" TEXT,
            PRIMARY KEY ((user_id, provider))
        )
    ", [?KEYSPACE, ?TABLE_TOKEN]));

%% ------------------------------------------------------------------
%% @doc Create the `room` table.
%% @end
%% ------------------------------------------------------------------
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

%% ------------------------------------------------------------------
%% @doc Create the `resource` table.
%% @end
%% ------------------------------------------------------------------
create_table(resource) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            protocol TEXT,
            address TEXT,
            room_id UUID,
            PRIMARY KEY (protocol, address, room_id)
        )
    ", [?KEYSPACE, ?TABLE_RESOURCE]));

%% ------------------------------------------------------------------
%% @doc Create the `message` table.
%% @end
%% ------------------------------------------------------------------
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
        ) WITH CLUSTERING ORDER BY (timestamp ASC)
    ", [?KEYSPACE, ?TABLE_MESSAGE]));

%% ------------------------------------------------------------------
%% @doc Create the `user_room` table.
%% @end
%% ------------------------------------------------------------------
create_table(user_room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            user_id UUID,
            room_id UUID,
            active BOOLEAN,
            PRIMARY KEY (user_id, room_id)
        )
    ", [?KEYSPACE, ?TABLE_USER_ROOM]));

%% ------------------------------------------------------------------
%% @doc Create the `room_user` table.
%% @end
%% ------------------------------------------------------------------
create_table(room_user) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, io_lib:format("
        CREATE TABLE IF NOT EXISTS ~s.~s (
            room_id UUID,
            user_id UUID,
            active BOOLEAN,
            PRIMARY KEY (room_id, user_id)
        )
    ", [?KEYSPACE, ?TABLE_ROOM_USER])).

%% ------------------------------------------------------------------
%% Alias
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Insert an alias.
%% @end
%% ------------------------------------------------------------------
insert_alias(#t_alias{} = Alias) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (provider, username, user_id)
            VALUES (?, ?, ?)
            IF NOT EXISTS
        ", [?KEYSPACE, ?TABLE_ALIAS]),
        values = [
            {provider, Alias#t_alias.provider},
            {username, Alias#t_alias.username},
            {user_id, Alias#t_alias.user_id}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Select an alias.
%% @end
%% ------------------------------------------------------------------
select_alias(#t_alias{} = Alias) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE provider = ? AND username = ?
        ", [?KEYSPACE, ?TABLE_ALIAS]),
        values = [
            {provider, Alias#t_alias.provider},
            {username, Alias#t_alias.username}
        ]
    }),
    lists:map(fun map_alias/1, cqerl:all_rows(Result)).

map_alias(Row) ->
    #t_alias{
        provider = decode(proplists:get_value(provider, Row)),
        username = decode(proplists:get_value(username, Row)),
        user_id = proplists:get_value(user_id, Row)
    }.

%% ------------------------------------------------------------------
%% User
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Insert a user.
%% @end
%% ------------------------------------------------------------------
insert_user(#t_user{} = User) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (id, provider, username, status, active)
            VALUES (?, ?, ?, ?, ?)
        ", [?KEYSPACE, ?TABLE_USER]),
        values = [
            {id, User#t_user.id},
            {provider, User#t_user.provider},
            {username, User#t_user.username},
            {status, encode(User#t_user.status)},
            {active, User#t_user.active}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Update a user.
%% @end
%% ------------------------------------------------------------------
update_user(#t_user{} = User) -> insert_user(User).

%% ------------------------------------------------------------------
%% @doc Select a user.
%% @end
%% ------------------------------------------------------------------
select_user(#t_user{} = User) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE id = ?
        ", [?KEYSPACE, ?TABLE_USER]),
        values = [
            {id, User#t_user.id}
        ]
    }),
    lists:map(fun map_user/1, cqerl:all_rows(Result));

%% ------------------------------------------------------------------
%% @doc Select several users.
%% @end
%% ------------------------------------------------------------------
select_user(List) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE id IN ?
        ", [?KEYSPACE, ?TABLE_USER]),
        values = [
            {'in(id)', [User#t_user.id || User <- List]}
        ]
    }),
    lists:map(fun map_user/1, cqerl:all_rows(Result)).

map_user(Row) ->
    #t_user{
        id = proplists:get_value(id, Row),
        provider = decode(proplists:get_value(provider, Row)),
        username = decode(proplists:get_value(username, Row)),
        status = decode(proplists:get_value(status, Row)),
        active = proplists:get_value(active, Row)
    }.

%% ------------------------------------------------------------------
%% Session
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Insert a session.
%% @end
%% ------------------------------------------------------------------
insert_session(#t_session{} = Session) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (id, user_id, provider, \"token\")
            VALUES (?, ?, ?, ?)
        ", [?KEYSPACE, ?TABLE_SESSION]),
        values = [
            {id, Session#t_session.id},
            {user_id, Session#t_session.user_id},
            {provider, Session#t_session.provider},
            {token, Session#t_session.token}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Delete a session.
%% @end
%% ------------------------------------------------------------------
delete_session(#t_session{} = Session) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            DELETE FROM ~s.~s
            WHERE id = ?
        ", [?KEYSPACE, ?TABLE_SESSION]),
        values = [
            {id, Session#t_session.id}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Select a session.
%% @end
%% ------------------------------------------------------------------
select_session(#t_session{} = Session) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE id = ?
        ", [?KEYSPACE, ?TABLE_SESSION]),
        values = [
            {id, Session#t_session.id}
        ]
    }),
    lists:map(fun map_session/1, cqerl:all_rows(Result)).

map_session(Row) ->
    #t_session{
        id = proplists:get_value(id, Row),
        user_id = proplists:get_value(user_id, Row),
        provider = decode(proplists:get_value(provider, Row)),
        token = proplists:get_value(token, Row)
    }.

%% ------------------------------------------------------------------
%% Token
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Insert a token.
%% @end
%% ------------------------------------------------------------------
insert_token(#t_token{} = Token) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (user_id, provider, \"token\")
            VALUES (?, ?, ?)
        ", [?KEYSPACE, ?TABLE_TOKEN]),
        values = [
            {user_id, Token#t_token.user_id},
            {provider, Token#t_token.provider},
            {token, Token#t_token.token}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Select a token.
%% @end
%% ------------------------------------------------------------------
select_token(#t_token{} = Token) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE user_id = ? AND provider = ?
        ", [?KEYSPACE, ?TABLE_TOKEN]),
        values = [
            {user_id, Token#t_token.user_id},
            {provider, Token#t_token.provider}
        ]
    }),
    lists:map(fun map_token/1, cqerl:all_rows(Result)).

map_token(Row) ->
    #t_token{
        user_id = proplists:get_value(user_id, Row),
        provider = decode(proplists:get_value(provider, Row)),
        token = proplists:get_value(token, Row)
    }.

%% ------------------------------------------------------------------
%% Room
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Insert a room.
%% @end
%% ------------------------------------------------------------------
insert_room(#t_room{} = Room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (id, name, type, data)
            VALUES (?, ?, ?, ?)
        ", [?KEYSPACE, ?TABLE_ROOM]),
        values = [
            {id, Room#t_room.id},
            {name, encode(Room#t_room.name)},
            {type, Room#t_room.type},
            {data, Room#t_room.data}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Update a room.
%% @end
%% ------------------------------------------------------------------
update_room(#t_room{} = Room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            UPDATE ~s.~s
            SET name = ?, type = ?, data = ?
            WHERE id = ?
        ", [?KEYSPACE, ?TABLE_ROOM]),
        values = [
            {id, Room#t_room.id},
            {name, encode(Room#t_room.name)},
            {type, Room#t_room.type},
            {data, encode(Room#t_room.data)}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Select a room.
%% @end
%% ------------------------------------------------------------------
select_room(#t_room{} = Room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE id = ?
        ", [?KEYSPACE, ?TABLE_ROOM]),
        values = [
            {id, Room#t_room.id}
        ]
    }),
    lists:map(fun map_room/1, cqerl:all_rows(Result));

%% ------------------------------------------------------------------
%% @doc Select several rooms.
%% @end
%% ------------------------------------------------------------------
select_room(List) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE id IN ?
        ", [?KEYSPACE, ?TABLE_ROOM]),
        values = [
            {'in(id)', [Room#t_room.id || Room <- List]}
        ]
    }),
    lists:map(fun map_room/1, cqerl:all_rows(Result)).

map_room(Row) ->
    #t_room{
        id = proplists:get_value(id, Row),
        name = decode(proplists:get_value(name, Row)),
        type = decode(proplists:get_value(type, Row)),
        data = decode(proplists:get_value(data, Row))
    }.

%% ------------------------------------------------------------------
%% Resource
%% ------------------------------------------------------------------

insert_resource(#t_resource{} = Resource) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (protocol, address, room_id)
            VALUES (?, ?, ?)
        ", [?KEYSPACE, ?TABLE_RESOURCE]),
        values = [
            {protocol, Resource#t_resource.protocol},
            {address, Resource#t_resource.address},
            {room_id, Resource#t_resource.room_id}
        ]
    }).

delete_resource(#t_resource{} = Resource) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            DELETE FROM ~s.~s
            WHERE protocol = ? AND address = ? AND room_id = ?
        ", [?KEYSPACE, ?TABLE_RESOURCE]),
        values = [
            {protocol, Resource#t_resource.protocol},
            {address, Resource#t_resource.address},
            {room_id, Resource#t_resource.room_id}
        ]
    }).

select_resource(#t_resource{} = Resource) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE protocol = ?
        ", [?KEYSPACE, ?TABLE_RESOURCE]),
        values = [
            {protocol, Resource#t_resource.protocol}
        ]
    }),
    lists:map(fun map_resource/1, cqerl:all_rows(Result)).

map_resource(Row) ->
    #t_resource{
        protocol = decode(proplists:get_value(protocol, Row)),
        address = decode(proplists:get_value(address, Row)),
        room_id = proplists:get_value(room_id, Row)
    }.

%% ------------------------------------------------------------------
%% Message
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% @doc Insert a message.
%% @end
%% ------------------------------------------------------------------
insert_message(#t_message{} = Message) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (room_id, timestamp, id, user_id, data)
            VALUES (?, ?, ?, ?, ?)
        ", [?KEYSPACE, ?TABLE_MESSAGE]),
        values = [
            {room_id, Message#t_message.room_id},
            {timestamp, Message#t_message.timestamp},
            {id, Message#t_message.id},
            {user_id, Message#t_message.user_id},
            {data, encode(Message#t_message.data)}
        ]
    }).

%% ------------------------------------------------------------------
%% @doc Select messages.
%% @end
%% ------------------------------------------------------------------
select_message(#t_message{} = Message) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE room_id = ?
            ORDER BY timestamp ASC
        ", [?KEYSPACE, ?TABLE_MESSAGE]),
        values = [
            {room_id, Message#t_message.room_id}
        ]
    }),
    lists:map(fun map_message/1, cqerl:all_rows(Result)).

map_message(Row) ->
    #t_message{
        room_id = proplists:get_value(room_id, Row),
        timestamp = proplists:get_value(timestamp, Row),
        id = proplists:get_value(id, Row),
        user_id = proplists:get_value(user_id, Row),
        data = decode(proplists:get_value(data, Row))
    }.

%% ------------------------------------------------------------------
%% User -> Room
%% ------------------------------------------------------------------

insert_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (user_id, room_id, active)
            VALUES (?, ?, ?)
        ", [?KEYSPACE, ?TABLE_USER_ROOM]),
        values = [
            {user_id, User#t_user.id},
            {room_id, Room#t_room.id},
            {active, Active}
        ]
    }).

update_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            UPDATE ~s.~s
            SET active = ?
            WHERE user_id = ? AND room_id = ?
        ", [?KEYSPACE, ?TABLE_USER_ROOM]),
        values = [
            {user_id, User#t_user.id},
            {room_id, Room#t_room.id},
            {active, Active}
        ]
    }).

select_user_room(#t_user{} = User) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE user_id = ? AND active = TRUE
            ALLOW FILTERING
        ", [?KEYSPACE, ?TABLE_USER_ROOM]),
        values = [
            {user_id, User#t_user.id}
        ]
    }),
    lists:map(fun map_user_room/1, cqerl:all_rows(Result)).

select_user_room(#t_user{} = User, #t_room{} = Room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE user_id = ? AND room_id = ? AND active = TRUE
            ALLOW FILTERING
        ", [?KEYSPACE, ?TABLE_USER_ROOM]),
        values = [
            {user_id, User#t_user.id},
            {room_id, Room#t_room.id}
        ]
    }),
    lists:map(fun map_user_room/1, cqerl:all_rows(Result)).

map_user_room(Row) ->
    #t_room{
        id = proplists:get_value(room_id, Row)
    }.

%% ------------------------------------------------------------------
%% Room -> User
%% ------------------------------------------------------------------

insert_room_user(#t_room{} = Room, #t_user{} = User, Active) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            INSERT INTO ~s.~s (room_id, user_id, active)
            VALUES (?, ?, ?)
        ", [?KEYSPACE, ?TABLE_ROOM_USER]),
        values = [
            {room_id, Room#t_room.id},
            {user_id, User#t_user.id},
            {active, Active}
        ]
    }).

update_room_user(#t_room{} = Room, #t_user{} = User, Active) ->
    {ok, Client} = get_cqerl_client(),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            UPDATE ~s.~s
            SET active = ?
            WHERE room_id = ? AND user_id = ?
        ", [?KEYSPACE, ?TABLE_ROOM_USER]),
        values = [
            {room_id, Room#t_room.id},
            {user_id, User#t_user.id},
            {active, Active}
        ]
    }).

select_room_user(#t_room{} = Room) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE room_id = ? AND active = TRUE
            ALLOW FILTERING
        ", [?KEYSPACE, ?TABLE_ROOM_USER]),
        values = [
            {room_id, Room#t_room.id}
        ]
    }),
    lists:map(fun map_room_user/1, cqerl:all_rows(Result)).

select_room_user(#t_room{} = Room, #t_user{} = User) ->
    {ok, Client} = get_cqerl_client(),
    {ok, Result} = cqerl:run_query(Client, #cql_query{
        statement = io_lib:format("
            SELECT * FROM ~s.~s
            WHERE room_id = ? AND user_id = ? AND active = TRUE
            ALLOW FILTERING
        ", [?KEYSPACE, ?TABLE_ROOM_USER]),
        values = [
            {room_id, Room#t_room.id},
            {user_id, User#t_user.id}
        ]
    }),
    lists:map(fun map_room_user/1, cqerl:all_rows(Result)).

map_room_user(Row) ->
    #t_user{
        id = proplists:get_value(user_id, Row)
    }.

%% ------------------------------------------------------------------
%% Shortcuts
%% ------------------------------------------------------------------

sym_insert_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    {ok, _} = insert_user_room(User, Room, Active),
    {ok, _} = insert_room_user(Room, User, Active).

sym_update_user_room(#t_user{} = User, #t_room{} = Room, Active) ->
    {ok, _} = update_user_room(User, Room, Active),
    {ok, _} = update_room_user(Room, User, Active).
