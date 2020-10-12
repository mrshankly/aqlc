-module(aqlc_crypto).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    generate_key/0,

    probabilistic_encrypt/2,
    probabilistic_decrypt/2,

    deterministic_encrypt/2,
    deterministic_decrypt/2
]).

-define(CIPHER, aes_128_cfb128).
-define(KEY_BYTES, 16).
-define(IV_BYTES, 16).
-define(DETERMINISTIC_IV, <<0:(?IV_BYTES * 8)>>).

-spec generate_key() -> binary().
generate_key() ->
    crypto:strong_rand_bytes(?KEY_BYTES).

-spec probabilistic_encrypt(Plaintext :: iodata(), Key :: binary()) -> binary().
probabilistic_encrypt(Plaintext, Key) ->
    IV = crypto:strong_rand_bytes(?IV_BYTES),
    Ciphertext = crypto:crypto_one_time(?CIPHER, Key, IV, Plaintext, true),
    <<IV/binary, Ciphertext/binary>>.

-spec probabilistic_decrypt(Data :: iodata(), Key :: binary()) -> binary().
probabilistic_decrypt(Data, Key) ->
    <<IV:?IV_BYTES/binary, Ciphertext/binary>> = Data,
    crypto:crypto_one_time(?CIPHER, Key, IV, Ciphertext, false).

-spec deterministic_encrypt(Plaintext :: iodata(), Key :: binary()) -> binary().
deterministic_encrypt(Plaintext, Key) ->
    crypto:crypto_one_time(?CIPHER, Key, ?DETERMINISTIC_IV, Plaintext, true).

-spec deterministic_decrypt(Ciphertext :: iodata(), Key :: binary()) -> binary().
deterministic_decrypt(Ciphertext, Key) ->
    crypto:crypto_one_time(?CIPHER, Key, ?DETERMINISTIC_IV, Ciphertext, false).

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

-endif.
