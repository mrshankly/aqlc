-module(aqlc).

-export([connect/2, connect/3, close/1, query/2, query/3]).
% TODO remove
-export([parse_query/1, rewrite_query/3, fetch_metadata/2]).

-include("aqlc.hrl").
-include("aql_pb.hrl").
-include("parser.hrl").
-include("types.hrl").

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
    case parse_query(Query) of
        {ok, [AST]} ->
            Message = aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(AST)}),
            case aqlc_tcp:send(Connection, Message) of
                {ok, Response} ->
                    {ok, aql_pb:decode_msg(Response, 'Response')};
                Error ->
                    Error
            end;
        {error, Reason, Line} ->
            {error, {Reason, Line}}
    end.

-spec query(connection(), iodata(), binary()) -> {ok, term()} | {error, term()}.
query(Connection, Query, Key) ->
    case rewrite_query(Connection, Query, Key) of
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

rewrite_query(Connection, Query, Key) ->
    case parse_query(Query) of
        % Rewrite `CREATE` queries when there are attributes with a default values.
        {ok, [{create, ?T_TABLE(Name, Policy, Cols, FKeys, Indexes, PartitionCol)}]} ->
            EncryptedCols = lists:map(
                fun
                    ({CName, CType, EncryptionType, ?DEFAULT_KEY(Default)}) ->
                        {CName, CType, EncryptionType,
                            encrypt_value(EncryptionType, Default, Key)};
                    (Col) ->
                        Col
                end,
                Cols
            ),

            EncryptedAST =
                {create, ?T_TABLE(Name, Policy, EncryptedCols, FKeys, Indexes, PartitionCol)},
            {ok,
                aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(EncryptedAST)})};
        % Rewrite all `INSERT` queries. Values must be encrypted according to encryption type
        % specified in the `CREATE` query.
        {ok, [{insert, {Table, ?PARSER_WILDCARD, Values}}]} ->
            Metadata = fetch_metadata(Connection, Table),
            EncryptedValues = encrypt_values(Metadata, [], Values, Key),
            Insert = {insert, {Table, ?PARSER_WILDCARD, EncryptedValues}},
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Insert)})};
        {ok, [{insert, {Table, Keys, Values}}]} ->
            Metadata = fetch_metadata(Connection, Table),
            EncryptedValues = encrypt_values(Metadata, Keys, Values, Key),
            Insert = {insert, {Table, Keys, EncryptedValues}},
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Insert)})};
        % Similar to `INSERT`, values present in `UPDATE` queries must be encrypted.
        {ok, [AST = {update, _}]} ->
            _Metadata = fetch_metadata(Connection, ""),
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

fetch_metadata(Connection, Table) ->
    Message = aql_pb:encode_msg(#'Request'{type = 'METADATA', tables = Table}),
    case aqlc_tcp:send(Connection, Message) of
        {ok, RawResponse} ->
            case aql_pb:decode_msg(RawResponse, 'Response') of
                #'Response'{metadata_error = Error} when Error /= <<>> ->
                    {error, binary_to_term(Error)};
                #'Response'{metadata = RawMetadata} ->
                    [{_, Metadata}] = binary_to_term(RawMetadata),

                    #{{names} := Columns} = Metadata,
                    EncryptionMetadata = lists:map(
                        fun(Column) ->
                            #{Column := {_, _, EncryptionType, _}} = Metadata,
                            {Column, EncryptionType}
                        end,
                        Columns
                    ),

                    {ok, EncryptionMetadata}
            end;
        Error ->
            Error
    end.

encrypt_value(encrypted, Value, Key) ->
    aqlc_crypto:probabilistic_encrypt(Value, Key);
encrypt_value(deterministic_encrypted, Value, Key) ->
    aqlc_crypto:deterministic_encrypt(Value, Key);
encrypt_value(plain, Value, _Key) ->
    Value.

encrypt_all([], [], Acc, _Key) ->
    Acc;
encrypt_all([{_, EncryptionType} | Metadata], [Value | Values], Acc, Key) ->
    EncryptedValue = encrypt_value(EncryptionType, Value, Key),
    encrypt_all(Metadata, Values, [EncryptedValue | Acc], Key).

encrypt_some(_Metadata, [], [], Acc, _Key) ->
    Acc;
encrypt_some(Metadata, [Key | Keys], [Value | Values], Acc, Key) ->
    EncryptionType = proplists:get_value(Key),
    EncryptedValue = encrypt_value(EncryptionType, Value, Key),
    encrypt_some(Metadata, Keys, Values, [EncryptedValue | Acc], Key).
    
encrypt_values(Metadata, [], Values, Key) ->
    encrypt_all(Metadata, Values, [], Key);
encrypt_values(Metadata, Keys, Values, Key) ->
    encrypt_some(Metadata, Keys, Values, [], Key).
