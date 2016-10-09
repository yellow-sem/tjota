-module(db_auth).
-export([
    login/1,
    login/3,
    logout/2
]).

-include("db_com.hrl").

login(#t_session{id = Id} = _) ->
    case db:select_session(#t_session{id = Id}) of
        [Session] -> {session, Session};
        [] -> none
    end.

login(Provider, Username, Token) ->
    case db:select_alias(#t_alias{provider = Provider,
                                  username = Username}) of
        [Alias] ->
            [User] = db:select_user(#t_user{id = Alias#t_alias.user_id}),
            db:update_user(User#t_user{provider = Provider,
                                       username = Username});

        [] ->
            User = #t_user{
                id = uuid:get_v4(),
                provider = Provider,
                username = Username,
                name = Username,
                active = true
            },

            db:insert_user(User),

            db:insert_alias(#t_alias{
                provider = Provider,
                username = Username,
                user_id = User#t_user.id
            })
    end,

    Session = #t_session{
        id = uuid:get_v4(),
        user_id = User#t_user.id,
        data = Token
    },

    db:insert_session(Session),
    {session, Session}.


logout(_Provider, _Username) -> not_implemented.
