-define(DEFAULT_CONNECT_TIMEOUT, 5000).

-type address() :: inet:socket_address() | inet:hostname().
-type port_number() :: inet:port_number().

-type connect_option() :: {ssl, boolean()} | {timeout, timeout()}.

-type connection() :: pid().
