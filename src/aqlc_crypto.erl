-module(aqlc_crypto).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    generate_key/0,

    probabilistic_encrypt/2,
    probabilistic_decrypt/2,

    deterministic_encrypt/2,
    deterministic_decrypt/2,

    ope_encrypt/2,
    ope_decrypt/2,

    paillier_encrypt/2,
    paillier_decrypt/2
]).

-type key() :: {binary(), binary(), paillier:keypair()}.

-define(CIPHER, aes_128_cfb128).
-define(KEY_BYTES, 16).
-define(IV_BYTES, 16).
-define(DETERMINISTIC_IV, <<0:(?IV_BYTES * 8)>>).

-spec generate_key() -> key().
generate_key() ->
    {
        crypto:strong_rand_bytes(?KEY_BYTES),
        base64:encode(crypto:strong_rand_bytes(?KEY_BYTES)),
        paillier:keypair(2048)
    }.

-spec probabilistic_encrypt(Plaintext :: iodata(), Key :: key()) -> binary().
probabilistic_encrypt(Plaintext, {Key, _, _}) ->
    IV = crypto:strong_rand_bytes(?IV_BYTES),
    Ciphertext = crypto:crypto_one_time(?CIPHER, Key, IV, Plaintext, true),
    <<IV/binary, Ciphertext/binary>>.

-spec probabilistic_decrypt(Data :: iodata(), Key :: key()) -> binary().
probabilistic_decrypt(Data, {Key, _, _}) ->
    <<IV:?IV_BYTES/binary, Ciphertext/binary>> = Data,
    crypto:crypto_one_time(?CIPHER, Key, IV, Ciphertext, false).

-spec deterministic_encrypt(Plaintext :: iodata(), Key :: key()) -> binary().
deterministic_encrypt(Plaintext, {Key, _, _}) ->
    crypto:crypto_one_time(?CIPHER, Key, ?DETERMINISTIC_IV, Plaintext, true).

-spec deterministic_decrypt(Ciphertext :: iodata(), Key :: key()) -> binary().
deterministic_decrypt(Ciphertext, {Key, _, _}) ->
    crypto:crypto_one_time(?CIPHER, Key, ?DETERMINISTIC_IV, Ciphertext, false).

-spec ope_encrypt(Plaintext :: number(), Key :: key()) -> number().
ope_encrypt(Plaintext, {_, Key, _}) ->
    RawCiphertext = os:cmd(io_lib:format("ope -e '~s' '~B'~n", [Key, Plaintext])),
    {Ciphertext, _} = string:to_integer(RawCiphertext),
    Ciphertext.

-spec ope_decrypt(Ciphertext :: number(), Key :: key()) -> number().
ope_decrypt(Ciphertext, {_, Key, _}) ->
    RawPlaintext = os:cmd(io_lib:format("ope -d '~s' '~B'~n", [Key, Ciphertext])),
    {Plaintext, _} = string:to_integer(RawPlaintext),
    Plaintext.

paillier_encrypt(Plaintext, {_, _, {PublicKey, _PrivateKey}}) ->
    RawCiphertext = paillier:encrypt(PublicKey, Plaintext),
    binary:decode_unsigned(RawCiphertext).

paillier_decrypt(Ciphertext, {_, _, {_PublicKey, PrivateKey}}) ->
    RawCiphertext = binary:encode_unsigned(Ciphertext),
    paillier:decrypt(PrivateKey, RawCiphertext).

-ifdef(TEST).

deterministic_test() ->
    Key = generate_key(),
    Foo = deterministic_encrypt(<<"foo">>, Key),
    ?assertEqual(deterministic_encrypt(<<"foo">>, Key), Foo),
    ?assertEqual(deterministic_decrypt(Foo, Key), <<"foo">>).

probabilistic_test() ->
    Key = generate_key(),
    Foo = probabilistic_encrypt(<<"foo">>, Key),
    ?assertNotEqual(probabilistic_encrypt(<<"foo">>, Key), Foo),
    ?assertEqual(probabilistic_decrypt(Foo, Key), <<"foo">>).

ope_test() ->
    Key = generate_key(),
    lists:foreach(
        fun(X) ->
            ENC_X = ope_encrypt(X, Key),
            ENC_XP1 = ope_encrypt(X + 1, Key),
            % Should be deterministic.
            ?assertEqual(ope_encrypt(X, Key), ENC_X),
            % Should preserve the order.
            ?assertEqual(X < X + 1, ENC_X < ENC_XP1),
            % Should decrypt ok.
            ?assertEqual(ope_decrypt(ENC_X, Key), X),
            ?assertEqual(ope_decrypt(ENC_XP1, Key), X + 1)
        end,
        lists:seq(1, 25)
    ).

paillier_test() ->
    Key = generate_key(),
    {_, _, {{_, _, _, NSquared}, _}} = Key,

    One = paillier_encrypt(1, Key),
    Two = paillier_encrypt(2, Key),
    Sum = (One * Two) rem binary:decode_unsigned(NSquared),
    ?assertEqual(3, paillier_decrypt(Sum, Key)).

-endif.
