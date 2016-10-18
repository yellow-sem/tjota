-define(DEFAULT_HOST, {127, 0, 0, 1}).
-define(DEFAULT_PORT, 4080).

-record(s_address, {
    host = ?DEFAULT_HOST,
    port = ?DEFAULT_PORT
}).

-record(s_handler, {
    socket::port(),
    owner::pid()
}).

-record(s_server, {
    socket::port(),
    address = #s_address{},
    acceptor::pid()
}).

-record(s_client, {
    socket::port(),
    address = #s_address{},
    receiver::pid(),
    identity::any()
}).
