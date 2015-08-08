-module(matasano_guess).
-export([hex_single_byte_xor/1]).

letter_scores() ->
    ValueList = [ 8.16,  1.49,  2.78,  4.25, 12.70,  2.23,  2.02,
                  6.09,  6.97,  0.15,  0.77,  4.03,  2.41,  6.75,
                  7.51,  1.93,  0.10,  5.99,  6.33,  9.06,  2.76,
                  0.98,  2.36,  0.15,  1.97,  0.07 ],
    [{32, 13.00}|lists:zip(lists:seq(97,122), ValueList)].

hex_single_byte_xor(HexText) ->
    CipherText = matasano_bytes:hex_to_binary(HexText),
    HighestFun = fun(Byte, Acc={_, _, AS}) ->
                         {_, T, S} = score_text(CipherText, Byte),
                         if S > AS -> {Byte, T, S};
                            true   -> Acc
                         end
                 end,
    {Byte, Text, _Score} = lists:foldl(HighestFun,
                                       score_text(CipherText, 0),
                                       lists:seq(1, 255)),
    {Byte, Text}.

score_text(CipherText, Byte) ->
    XORFun = fun(X) -> X bxor Byte end,
    ConvertedText = matasano_bytes:binary_map(XORFun, CipherText),
    {Byte, ConvertedText, score_text(ConvertedText)}.

score_text(Text) ->
    AddScore = fun(X, Acc) -> Acc + get_score(X) end,
    matasano_bytes:binary_foldl(AddScore, 0,
                                matasano_bytes:binary_map(fun to_lower/1,
                                                          Text)).

get_score(X) ->
    Scores = letter_scores(),
    case proplists:is_defined(X, Scores) of
        true -> proplists:get_value(X, Scores);
        false -> 0.0
    end.

to_lower(X) when X >= 65 andalso X =< 90 -> X + 32;
to_lower(X) -> X.
