-module(db_auth).
-export([
    login/1,
    login/3
]).

-include("db.hrl").

login(#t_session{id = Id} = _) ->
    [Session] = db:select_session(#t_session{id = Id}),
    {session, Session}.

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
                active = true
            },

            {ok, _} = db:insert_user(User),

            {ok, _} = db:insert_alias(#t_alias{
                provider = Provider,
                username = Username,
                user_id = User#t_user.id
            })
    end,

    Session = #t_session{
        id = uuid:get_v4(),
        user_id = User#t_user.id,
        provider = Provider,
        token = Token
    },

    {ok, _} = db:insert_session(Session),
    {ok, _} = db:insert_token(#t_token{user_id = User#t_user.id,
                                       provider = Provider,
                                       token = Token}),
    {session, Session}.
