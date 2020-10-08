-module(aqlc).

-export([connect/2, connect/3, close/1, query/2]).

-include("aqlc.hrl").
-include("aql_pb.hrl").
-include("parser.hrl").

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
    case rewrite_query(Connection, Query) of
        {ok, Message} ->
            case aqlc_tcp:send(Connection, Message) of
                {ok, Response} ->
                    {ok, aql_pb:decode_msg(Response, 'Response')};
                Error ->
                    Error
            end;
        Error = {error, _Reason} ->
            Error
    end.

rewrite_query(Connection, Query) ->
    case parse_query(Query) of
        % Rewrite `CREATE` queries when there are attributes with a default values.
        {ok, [AST = {create, _}]} ->
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(AST)})};

        % Rewrite all `INSERT` queries. Values must be encrypted according to encryption type
        % specified in the `CREATE` query.
        {ok, [{insert, {Table, ?PARSER_WILDCARD, Values}}]} ->
            Metadata = fetch_metadata(Connection),
            EncryptedValues = encrypt_values(Metadata, Table, [], Values),
            Insert = {insert, {Table, ?PARSER_WILDCARD, EncryptedValues}},
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Insert)})};
        {ok, [{insert, {Table, Keys, Values}}]} ->
            Metadata = fetch_metadata(Connection),
            EncryptedValues = encrypt_values(Metadata, Table, Keys, Values),
            Insert = {insert, {Table, Keys, EncryptedValues}},
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Insert)})};

        % Similar to `INSERT`, values present in `UPDATE` queries must be encrypted.
        {ok, [AST = {update, _}]} ->
            _Metadata = fetch_metadata(Connection),
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(AST)})};

        % `SELECT` queries don't actually need to be rewritten, the result however, needs to
        % be decrypted. For this reason a special request is made to the server. The client
        % asks the server for the table metadata, so that it knows how to decrypt the result.
        {ok, [AST = {select, {Table, _Projection, _Where}}]} ->
            {ok,
                aql_pb:encode_msg(#'Request'{
                    type = 'QUERY_AND_METADATA',
                    query = term_to_binary(AST),
                    tables = atom_to_list(Table)
                })};

        {ok, [AST]} ->
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(AST)})};
        {error, Reason, Line} ->
            {error, {Reason, Line}}
    end.

parse_query(Query) ->
    case scanner:string(Query) of
        {ok, Tokens, _} ->
            parser:parse(Tokens);
        Error ->
            Error
    end.

fetch_metadata(_Connection) ->
    [].

encrypt_values(_Metadata, _Table, _Keys, Values) ->
    % TODO: actually get metadata and encrypt values accordingly.
    Values.
