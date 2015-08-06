-module(matasano_bytes_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

% following examples taken from wikipedia
% https://en.wikipedia.org/wiki/Base64
binary_base64_set() ->
    [ { "TWFu", "Man" },
      { "YW55IGNhcm5hbCBwbGVhc3VyZS4=", "any carnal pleasure." },
      { "YW55IGNhcm5hbCBwbGVhc3VyZQ==", "any carnal pleasure" },
      { "YW55IGNhcm5hbCBwbGVhc3Vy", "any carnal pleasur" },
      { "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFz" ++
        "b24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhl" ++
        "ciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRo" ++
        "YXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29u" ++
        "dGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25v" ++
        "d2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55" ++
        "IGNhcm5hbCBwbGVhc3VyZS4=",
        "Man is distinguished, not only by his reason, but by thi" ++
        "s singular passion from other animals, which is a lust o" ++
        "f the mind, that by a perseverance of delight in the con" ++
        "tinued and indefatigable generation of knowledge, exceed" ++
        "s the short vehemence of any carnal pleasure." }
    ].

binary_to_base64_test_() ->
    lists:map(fun({X, Y}) ->
                ?_assertEqual({base64, binary:list_to_bin(X)},
                    matasano_bytes:binary_to_base64(binary:list_to_bin(Y)))
        end, binary_base64_set()).

base64_to_binary_test_() ->
    lists:map(fun({X, Y}) ->
                ?_assertEqual(binary:list_to_bin(Y),
                    matasano_bytes:base64_to_binary(
                        {base64, binary:list_to_bin(X)}))
        end, binary_base64_set()).
