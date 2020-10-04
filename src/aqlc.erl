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

-spec query(connection(), iodata()) -> {ok, term()} | {error, term()}.
query(Connection, Query) ->
    Message = aql_pb:encode_msg(#'Request'{type = 'QUERY', query = Query}),
    case aqlc_tcp:send(Connection, Message) of
        {ok, Response} ->
            {ok, aql_pb:decode_msg(Response, 'QueryResponse')};
        Error ->
            Error
    end.
