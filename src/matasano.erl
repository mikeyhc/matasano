-module(matasano).

-export([ % set 1
          set_1/0,
          challenge_1_1/0,
          challenge_1_2/0,
          challenge_1_3/0,
          challenge_1_4/0,
          challenge_1_5/0
        ]).


%% API

set_1_list() ->
    [ {1, fun challenge_1_1/0},
      {2, fun challenge_1_2/0},
      {3, fun challenge_1_3/0},
      {4, fun challenge_1_4/0},
      {5, fun challenge_1_5/0}
    ].

set_1() ->
    RunFun = fun({I, F}) ->
            io:format("Set 1 - Challenge ~w - output~n", [I]),
            R = F(),
            io:format("~n", []),
            {I, R}
    end,
    PrintFun = fun({I, R}) ->
            io:format("  Challenge ~w: ~s~n", [I, passed(R)])
    end,
    L = lists:map(RunFun, set_1_list()),
    io:format("~nSet 1~n"),
    lists:foreach(PrintFun, L).

challenge_1_1() ->
    Hex = <<"49276d206b696c6c696e6720796f757220627261696e206c696b6520",
            "6120706f69736f6e6f7573206d757368726f6f6d">>,
    Base64 = <<"SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzI",
               "G11c2hyb29t">>,
    Plain = <<"I'm killing your brain like a poisonous mushroom">>,
    HexObj = matasano_bytes:new_hex(Hex),
    ABase64 = matasano_bytes:base64_binary(
        matasano_bytes:hex_to_base64(HexObj)),
    APlain = matasano_bytes:hex_to_binary(HexObj),
    io:format("input hex:       ~s~n", [Hex]),
    io:format("expected base64: ~s~n", [Base64]),
    io:format("expected plain:  ~s~n", [Plain]),
    io:format("actual base64:   ~s~n", [ABase64]),
    io:format("actual plain:    ~s~n", [APlain]),
    Base64 =:= ABase64 andalso Plain =:= APlain.

challenge_1_2() ->
    Hex1 = matasano_bytes:new_hex(<<"1c0111001f010100061a024b53535009181c">>),
    Hex2 = matasano_bytes:new_hex(<<"686974207468652062756c6c277320657965">>),
    Hex3 = matasano_bytes:new_hex(<<"746865206b696420646f6e277420706c6179">>),
    Plain = <<"the kid don't play">>,
    AHex3 = matasano_bytes:hex_hex_fixed_xor(Hex1, Hex2),
    APlain = matasano_bytes:hex_to_binary(AHex3),
    io:format("input hex 1:    ~s~n", [matasano_bytes:hex_binary(Hex1)]),
    io:format("input hex 2:    ~s~n", [matasano_bytes:hex_binary(Hex2)]),
    io:format("expected hex:   ~s~n", [matasano_bytes:hex_binary(Hex3)]),
    io:format("expected plain: ~s~n", [Plain]),
    io:format("actual hex:     ~s~n", [matasano_bytes:hex_binary(AHex3)]),
    io:format("actual plain:   ~s~n", [APlain]),
    Hex3 =:= AHex3 andalso Plain =:= APlain.

challenge_1_3() ->
    Hex = matasano_bytes:new_hex(<<"1b37373331363f78151b7f2b783431333"
                                   "d78397828372d363c78373e783a393b37"
                                   "36">>),
    OutChar = 88,
    OutBinary = <<"Cooking MC's like a pound of bacon">>,
    {AChar, ABinary} = matasano_guess:hex_single_byte_xor(Hex),
    io:format("input hex:       ~s~n", [matasano_bytes:hex_binary(Hex)]),
    io:format("expected output: {~w, ~s}~n", [OutChar, OutBinary]),
    io:format("actual output:   {~w, ~s}~n", [AChar, ABinary]),
    OutChar =:= AChar andalso OutBinary =:= ABinary.

challenge_1_4() ->
    Line = 157,
    Byte = 53,
    Binary = <<"Now that the party is jumping\n">>,
    File = "data/s1c4.txt",
    {ok, InString} = file:read_file(File),
    NewLineSplit = fun(X, Acc) ->
                           Hd = hd(Acc), Tl = tl(Acc),
                           if X =:= 10 -> [<<>>|Acc];
                              true     -> [<<Hd/binary, X>>|Tl]
                           end
                   end,
    Strings = matasano_bytes:binary_foldl(NewLineSplit, [<<>>], InString),
    HexStrings = lists:map(fun matasano_bytes:new_hex/1, Strings),
    {ALine, AByte, ABinary} =
        matasano_guess:hex_detect_single_byte_xor(HexStrings),
    io:format("input file:      ~w~n", [File]),
    io:format("expected output: {~w, ~w, <<\"~s\">>}~n",
              [Line, Byte, Binary]),
    io:format("actual output:   {~w, ~w, <<\"~s\">>}~n",
              [ALine, AByte, ABinary]),
    Line =:= ALine andalso Byte =:= AByte andalso Binary =:= ABinary.

challenge_1_5() ->
    Plain = <<"Burning 'em, if you ain't quick and nimble\n"
              "I go crazy when I hear a cymbal">>,
    Key = <<"ICE">>,
    Hex = <<"0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c"
            "2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b"
            "2027630c692b20283165286326302e27282f">>,
    AHex = matasano_bytes:binary_to_hex(
             matasano_bytes:binary_binary_repeating_xor(Plain, Key)),
    io:format("input binary:    ~s~n", [Plain]),
    io:format("input key:       ~s~n", [Key]),
    io:format("expected output: ~s~n", [Hex]),
    io:format("actual output:   ~w~n", [matasano_bytes:hex_binary(AHex)]),
    Hex =:= matasano_bytes:hex_binary(AHex).

%% Helper functions

passed(true) -> "Passed";
passed(false) -> "Failed".
