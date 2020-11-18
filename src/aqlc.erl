-module(aqlc).

-export([connect/2, connect/3, close/1, query/2, query/3]).
% TODO remove
-export([parse_query/1, rewrite_query/3, fetch_metadata/2, encrypt_where/3]).

-include_lib("kernel/include/logger.hrl").

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
                    decode_response(Response);
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
                    decode_response(Response);
                Error ->
                    Error
            end;
        {ok, Message, Metadata} ->
            case aqlc_tcp:send(Connection, Message) of
                {ok, Response} ->
                    decode_response(Response, Metadata, Key);
                Error ->
                    Error
            end;
        Error = {error, _Reason} ->
            Error
    end.

decode_response(RawResponse) ->
    case aql_pb:decode_msg(RawResponse, 'Response') of
        #'Response'{query_error = Error} when Error /= <<>> ->
            {error, binary_to_term(Error)};
        #'Response'{query = QueryResponse} ->
            {ok, binary_to_term(QueryResponse)}
    end.

decode_response(RawResponse, Metadata, Key) ->
    case aql_pb:decode_msg(RawResponse, 'Response') of
        #'Response'{query_error = Error} when Error /= <<>> ->
            {error, binary_to_term(Error)};
        #'Response'{query = QueryResponse} ->
            [Values] = binary_to_term(QueryResponse),
            {ok, [decrypt_values(Metadata, Values, Key)]}
    end.

%% erlfmt-ignore
rewrite_query(Connection, Query, Key) ->
    case parse_query(Query) of
        % Rewrite `CREATE` queries when there are attributes with a default values.
        {ok, [?CREATE_CLAUSE(?T_TABLE(Name, Policy, Cols, FKeys, Indexes, PartitionCol))]} ->
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
                ?CREATE_CLAUSE(
                    ?T_TABLE(Name, Policy, EncryptedCols, FKeys, Indexes, PartitionCol)
                ),
            {ok,
                aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(EncryptedAST)})};

        % Rewrite all `INSERT` queries. Values must be encrypted according to encryption type
        % specified in the `CREATE` query.
        {ok, [?INSERT_CLAUSE({Table, ?PARSER_WILDCARD, Values})]} ->
            {ok, Metadata} = fetch_metadata(Connection, Table),
            EncryptedValues = encrypt_values(Metadata, [], Values, Key),
            Insert = ?INSERT_CLAUSE({Table, ?PARSER_WILDCARD, EncryptedValues}),
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Insert)})};
        {ok, [?INSERT_CLAUSE({Table, Keys, Values})]} ->
            {ok, Metadata} = fetch_metadata(Connection, Table),
            EncryptedValues = encrypt_values(Metadata, Keys, Values, Key),
            Insert = ?INSERT_CLAUSE({Table, Keys, EncryptedValues}),
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Insert)})};

        % Similar to `INSERT`, values present in `UPDATE` queries must be encrypted.
        {ok, [?UPDATE_CLAUSE({Table, {set, Operations}, Constraint})]} ->
            {ok, Metadata} = fetch_metadata(Connection, Table),
            EncryptedOperations = encrypt_operations(Metadata, Operations, Key),
            [EncryptedConstraint] = encrypt_operations(Metadata, [Constraint], Key),
            Update = ?UPDATE_CLAUSE({Table, {set, EncryptedOperations}, EncryptedConstraint}),
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Update)})};

        % Encrypt where clause of `SELECT` queries, the result also needs to be decrypted.
        % For this reason we return a three element tuple, where the third element is the
        % metadata. This avoid having the request the metadata twice, once for encrypting
        % the where clause, another for decrypting the response.
        {ok, [?SELECT_CLAUSE({Table, Projection, Where})]} ->
            {ok, Metadata} = fetch_metadata(Connection, Table),
            EncryptedWhere = encrypt_where(Metadata, Where, Key),
            Select = ?SELECT_CLAUSE({Table, Projection, EncryptedWhere}),
            {ok, aql_pb:encode_msg(#'Request'{type = 'QUERY', query = term_to_binary(Select)}), Metadata};

        {ok, [AST]} ->
            ?LOG_DEBUG("SKIP QUERY REWRITE: ~p", [AST]),
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

fetch_metadata(Connection, Table) when is_atom(Table) ->
    fetch_metadata(Connection, atom_to_list(Table));
fetch_metadata(Connection, Table) when is_list(Table) ->
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

decrypt_value(encrypted, Value, Key) ->
    binary_to_term(aqlc_crypto:probabilistic_decrypt(Value, Key));
decrypt_value(deterministic_encrypted, Value, Key) ->
    binary_to_term(aqlc_crypto:deterministic_decrypt(Value, Key));
decrypt_value(plain, Value, _Key) ->
    Value.

decrypt_values(Metadata, Values, Key) ->
    lists:map(
        fun(V) ->
            lists:map(
                fun({Column, Value}) ->
                    EncryptionType = proplists:get_value(Column, Metadata),
                    Plaintext = decrypt_value(EncryptionType, Value, Key),
                    {Column, Plaintext}
                end,
                V
            )
        end,
        Values
    ).

encrypt_value(encrypted, Value, Key) ->
    aqlc_crypto:probabilistic_encrypt(term_to_binary(Value), Key);
encrypt_value(deterministic_encrypted, Value, Key) ->
    aqlc_crypto:deterministic_encrypt(term_to_binary(Value), Key);
encrypt_value(plain, Value, _Key) ->
    Value.

encrypt_all([], [], Acc, _Key) ->
    lists:reverse(Acc);
encrypt_all([{_, EncryptionType} | Metadata], [Value | Values], Acc, Key) ->
    EncryptedValue = encrypt_value(EncryptionType, Value, Key),
    encrypt_all(Metadata, Values, [EncryptedValue | Acc], Key).

encrypt_some(_Metadata, [], [], Acc, _Key) ->
    lists:reverse(Acc);
encrypt_some(Metadata, [Column | Columns], [Value | Values], Acc, Key) ->
    EncryptionType = proplists:get_value(Column, Metadata),
    EncryptedValue = encrypt_value(EncryptionType, Value, Key),
    encrypt_some(Metadata, Columns, Values, [EncryptedValue | Acc], Key).

encrypt_values(Metadata, [], Values, Key) ->
    encrypt_all(Metadata, Values, [], Key);
encrypt_values(Metadata, Keys, Values, Key) ->
    encrypt_some(Metadata, Keys, Values, [], Key).

encrypt_operations(Metadata, Operations, Key) when is_list(Operations) ->
    Keys = lists:map(fun({K, _, _}) -> K end, Operations),
    Values = lists:map(fun({_, _, V}) -> V end, Operations),
    EncryptedValues = encrypt_values(Metadata, Keys, Values, Key),
    lists:map(
        fun({{K, Op, _}, EncryptedValue}) -> {K, Op, EncryptedValue} end,
        lists:zip(Operations, EncryptedValues)
    );
encrypt_operations(Metadata, {K, Op, V}, Key) ->
    [EncryptedValue] = encrypt_values(Metadata, [K], [V], Key),
    {K, Op, EncryptedValue};
encrypt_operations(_Metadata, Operations, _Key) ->
    Operations.

encrypt_where(Metadata, Op = {_, _, _}, Key) ->
    encrypt_operations(Metadata, Op, Key);
encrypt_where(Metadata, Ops = [{_, _, _}], Key) ->
    encrypt_operations(Metadata, Ops, Key);
encrypt_where(Metadata, [First = {_, _, _}, AndOr = {_, _}, Second = {_, _, _}], Key) ->
    F = encrypt_operations(Metadata, First, Key),
    S = encrypt_operations(Metadata, Second, Key),
    [F, AndOr, S];
encrypt_where(Metadata, [First = {_, _, _}, AndOr = {_, _}, Rest], Key) when is_list(Rest) ->
    [encrypt_operations(Metadata, First, Key), AndOr, encrypt_where(Metadata, Rest, Key)];
encrypt_where(_Metadata, Where, _Key) ->
    Where.
