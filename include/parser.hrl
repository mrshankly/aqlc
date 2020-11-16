% -----------------------------------------------------------------------------
% --------------------------------- Lexer ------------------------------------
% -----------------------------------------------------------------------------

-define(ADD_WINS, add).
-define(REMOVE_WINS, remove).
-define(NO_CONCURRENCY, noconcurrency).

% Encryption types.
-define(AQL_ENCRYPTED, encrypted).
-define(AQL_DT_ENCRYPTED, deterministic_encrypted).
-define(AQL_OP_ENCRYPTED, order_preserving_encrypted).
-define(AQL_HM_ENCRYPTED, homomorphic_encrypted).

-define(AQL_INTEGER, integer).
-define(AQL_VARCHAR, varchar).
-define(AQL_BOOLEAN, boolean).
-define(AQL_COUNTER_INT, counter_int).

% show
-define(SHOW_TOKEN, show).
-define(SHOW_CLAUSE(TokenChars), {?SHOW_TOKEN, TokenChars}).

-define(TABLES_TOKEN, tables).
-define(TABLES_CLAUSE(TokenChars), {?TABLES_TOKEN, TokenChars}).

% index
-define(INDEX_TOKEN, index).
-define(INDEX_CLAUSE(TokenChars), {?INDEX_TOKEN, TokenChars}).
-define(INDEXES_TOKEN, indexes).
-define(INDEXES_CLAUSE(TokenChars), {?INDEXES_TOKEN, TokenChars}).
% on
-define(ON_TOKEN, on).
-define(ON_KEY(TokenChars), {?ON_TOKEN, TokenChars}).

% select
-define(SELECT_TOKEN, select).
-define(SELECT_CLAUSE(TokenChars), {?SELECT_TOKEN, TokenChars}).
%% from
-define(FROM_TOKEN, from).
-define(FROM_CLAUSE(TokenChars), {?FROM_TOKEN, TokenChars}).

%join
-define(JOIN_TOKEN, join).
-define(JOIN_CLAUSE(TokenChars), {?JOIN_TOKEN, TokenChars}).

% where
-define(WHERE_TOKEN, where).
-define(WHERE_CLAUSE(TokenChars), {?WHERE_TOKEN, TokenChars}).
%% and
-define(CONJUNCTIVE_TOKEN, conjunctive).
-define(CONJUNCTIVE_KEY(TokenChars), {?CONJUNCTIVE_TOKEN, TokenChars}).
%% or
-define(DISJUNCTIVE_TOKEN, disjunctive).
-define(DISJUNCTIVE_KEY(TokenChars), {?DISJUNCTIVE_TOKEN, TokenChars}).

% insert
-define(INSERT_TOKEN, insert).
-define(INSERT_CLAUSE(TokenChars), {?INSERT_TOKEN, TokenChars}).
%% into
-define(INTO_TOKEN, into).
-define(INTO_KEY(TokenChars), {?INTO_TOKEN, TokenChars}).

% create
-define(CREATE_TOKEN, create).
-define(CREATE_CLAUSE(TokenChars), {?CREATE_TOKEN, TokenChars}).
% partitioned
-define(PARTITION_TOKEN, partition).
-define(PARTITION_CLAUSE(TokenChars), {?PARTITION_TOKEN, TokenChars}).
%% table
-define(TABLE_TOKEN, table).
-define(TABLE_KEY(TokenChars), {?TABLE_TOKEN, TokenChars}).
%% values
-define(VALUES_TOKEN, values).
-define(VALUES_CLAUSE(TokenChars), {?VALUES_TOKEN, TokenChars}).
%% no constraint
-define(NO_CONSTRAINT, ignore).
%% primary key constraint
-define(PRIMARY_TOKEN, primary).
-define(PRIMARY_KEY(TokenChars), {?PRIMARY_TOKEN, TokenChars}).
-define(FOREIGN_TOKEN, foreign).
-define(FOREIGN_KEY(TokenChars), {?FOREIGN_TOKEN, TokenChars}).
-define(KEY_TOKEN, key).
-define(KEY_KEY(TokenChars), {?KEY_TOKEN, TokenChars}).
-define(REFERENCES_TOKEN, references).
-define(REFERENCES_KEY(TokenChars), {?REFERENCES_TOKEN, TokenChars}).
-define(CASCADE_TOKEN, cascade).
-define(CASCADE_CLAUSE(TokenChars), {?CASCADE_TOKEN, TokenChars}).
-define(RESTRICT_TOKEN, restrict).

%% check constraint
-define(CHECK_TOKEN, check).
-define(CHECK_KEY(TokenChars), {?CHECK_TOKEN, TokenChars}).
-define(COMPARATOR_KEY(Comparator), {comparator, Comparator}).
-define(GREATER_KEY, ?COMPARATOR_KEY(?PARSER_GREATER)).
-define(GREATEREQ_KEY, ?COMPARATOR_KEY(?PARSER_GEQ)).
-define(LESSER_KEY, ?COMPARATOR_KEY(?PARSER_LESSER)).
-define(LESSEREQ_KEY, ?COMPARATOR_KEY(?PARSER_LEQ)).
%% default value
-define(DEFAULT_TOKEN, default).
-define(DEFAULT_KEY(TokenChars), {?DEFAULT_TOKEN, TokenChars}).
%% attributes
-define(ATTR_TYPE_TOKEN, attribute_type).
-define(ATTR_KEY(AttrType), {?ATTR_TYPE_TOKEN, AttrType}).
-define(ATTR_ENC_TOKEN, attribute_encryption_type).
-define(ATTR_ENC(AttrEncType), {?ATTR_ENC_TOKEN, AttrEncType}).
-define(NO_ENCRYPTION, plain).
%% table policies
%-define(TABLE_POLICY_TOKEN, table_policy).
%-define(TABLE_POLICY_KEY(Crp), {?TABLE_POLICY_TOKEN, Crp}).
%-define(DEP_POLICY_TOKEN, dep_policy).
%-define(DEP_POLICY_KEY(Crp), {?DEP_POLICY_TOKEN, Crp}).
-define(CRP_TOKEN, crp).
-define(CRP_KEY(Crp), {?CRP_TOKEN, Crp}).
-define(NO_CONCURRENCY_KEY, {?CRP_TOKEN, noconcurrency}).

% update
-define(UPDATE_TOKEN, update).
-define(UPDATE_CLAUSE(TokenChars), {?UPDATE_TOKEN, TokenChars}).
%% set
-define(SET_TOKEN, set).
-define(SET_CLAUSE(TokenChars), {?SET_TOKEN, TokenChars}).
%%% set ops
-define(SET_OP_T(Token, TChars), {Token, TChars}).
-define(ASSIGN_TOKEN, assign).
-define(ASSIGN_OP(TokenChars), ?SET_OP_T(?ASSIGN_TOKEN, TokenChars)).
-define(INCREMENT_TOKEN, increment).
-define(INCREMENT_OP(TokenChars), ?SET_OP_T(?INCREMENT_TOKEN, TokenChars)).
-define(DECREMENT_TOKEN, decrement).
-define(DECREMENT_OP(TokenChars), ?SET_OP_T(?DECREMENT_TOKEN, TokenChars)).

% delete
-define(DELETE_TOKEN, delete).
-define(DELETE_CLAUSE(TokenChars), {?DELETE_TOKEN, TokenChars}).

% begin transaction
-define(BEGIN_TOKEN, 'begin').
-define(BEGIN_CLAUSE(TokenChars), {?BEGIN_TOKEN, TokenChars}).
% commit transaction
-define(COMMIT_TOKEN, commit).
-define(COMMIT_CLAUSE(TokenChars), {?COMMIT_TOKEN, TokenChars}).
% rollback transaction
-define(ROLLBACK_TOKEN, rollback).
-define(ROLLBACK_CLAUSE(TokenChars), {?ROLLBACK_TOKEN, TokenChars}).
% transaction
-define(TRANSACTION_TOKEN, transaction).
-define(TRANSACTION_KEY(TokenChars), {?TRANSACTION_TOKEN, TokenChars}).

% quit program
-define(QUIT_TOKEN, quit).
-define(QUIT_CLAUSE(TokenChars), {?QUIT_TOKEN, TokenChars}).

%terms
-define(PARSER_BOOLEAN_TOKEN, boolean).
-define(PARSER_ATOM_TOKEN, atom_value).
-define(PARSER_STRING_TOKEN, string).
-define(PARSER_NUMBER_TOKEN, number).
-define(PARSER_TYPE(Type, Value), {Type, Value}).
-define(PARSER_BOOLEAN(Boolean), ?PARSER_TYPE(?PARSER_BOOLEAN_TOKEN, Boolean)).
-define(PARSER_ATOM(Atom), ?PARSER_TYPE(?PARSER_ATOM_TOKEN, Atom)).
-define(PARSER_STRING(String), ?PARSER_TYPE(?PARSER_STRING_TOKEN, String)).
-define(PARSER_NUMBER(Number), ?PARSER_TYPE(?PARSER_NUMBER_TOKEN, Number)).
-define(is_parser(Parser), is_tuple(Parser) andalso tuple_size(Parser) =:= 2).
-define(is_parser_type(Parser, Type), ?is_parser(Parser) andalso element(1, Parser) =:= Type).

% extras
-define(PARSER_EQUALITY, {equality, ignore}).
-define(PARSER_NEQ, {notequality, ignore}).
-define(PARSER_GREATER, {greater, ignore}).
-define(PARSER_LESSER, {lesser, ignore}).
-define(PARSER_GEQ, {greatereq, ignore}).
-define(PARSER_LEQ, {lessereq, ignore}).
-define(PARSER_PLUS, {plus, ignore}).
-define(PARSER_MINUS, {minus, ignore}).
-define(PARSER_WILDCARD, {wildcard, ignore}).

-define(PARSER_SLIST, {start_list, ignore}).
-define(PARSER_ELIST, {end_list, ignore}).
-define(PARSER_SEP, {sep, ignore}).
-define(PARSER_SCOLON, {semi_colon, ignore}).
