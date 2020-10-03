-module(aqlc).

-export([connect/2, connect/3, close/1, query/2]).

-include("aqlc.hrl").
-include("aql_pb.hrl").

-spec connect(address(), port_number()) -> {ok, connection()} | {error, term()}.
connect(Address, Port) ->
    connect(Address, Port, []).

-spec connect(address(), port_number(), [connect_option()]) ->
    {ok, connection()} | {error, term()}.
connect(Address, Port, Opts) ->
    aqlc_tcp:start_link(Address, Port, Opts).

-spec close(connection()) -> ok.
close(Connection) ->
    aqlc_tcp:stop(Connection).

-spec query(connection(), iodata()) -> ok | {error, term()}.
query(Connection, Query) ->
    Request = #'Request'{
        type = 'QUERY',
        query = Query
    },
    Message = aql_pb:encode_msg(Request),
    aqlc_tcp:send(Connection, Message).
