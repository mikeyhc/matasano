-module(matasano_bytes).

-export_type([ hex/0,
               base64/0,
               bytestring/0
             ]).

-compile([export_all]).
%-export([ new_hex/1,
%          new_base64/1,
%          hex_to_base64/1,
%          base64_to_hex/1
%        ]).

-opaque hex()        :: {hex, binary()}.
-opaque base64()     :: {base64, binary()}.
-opaque bytestring() :: hex() | base64().

% create a hex type with the given binary string (no conversion)
-spec new_hex(binary()) -> hex().
new_hex(S) when is_binary(S) -> {hex, S}.

% create a base64 type with the given binary string (no conversion)
-spec new_base64(binary()) -> base64().
new_base64(S) when is_binary(S) -> {base64, S}.

% convert a string binary to a base64 type
-spec binary_to_base64(binary()) -> binary().
binary_to_base64(S) -> {base64, binary_to_base64(S, <<>>)}.

% convert a hex type to a base64 type
-spec hex_to_base64(hex() | binary()) -> base64().
hex_to_base64({hex, _String}) -> throw(not_implemented).

% convert a base64 type to a hex type
-spec base64_to_hex(base64()) -> hex().
base64_to_hex({base64, _String}) -> throw(not_implemented).

%% Helper Methods

% convert a single byte into its value in the base64 table
-spec to_base64_char(byte()) -> byte().
to_base64_char(B) when B >= 0 andalso B =< 25 -> B + 65;
to_base64_char(B) when B >= 26 andalso B =< 51 -> B + 71;
to_base64_char(B) when B >= 52 andalso B =< 61 -> B - 4;
to_base64_char(62) -> 43;
to_base64_char(63) -> 47.

% map F over the binary B
-spec binary_map(fun((byte()) -> byte()), binary()) -> binary().
binary_map(F, B) ->  binary:list_to_bin(lists:map(F, binary:bin_to_list(B))).

%% convert a binary string into its base64 representation
-spec binary_to_base64(binary(), binary()) -> binary().
binary_to_base64(<<>>, Acc) -> lists:map(to_base64_char, Acc);
binary_to_base64(S= <<_:8>>, Acc) ->
    Last = bytes_to_base64(S),
    <<Acc/binary, Last/binary, "==">>;
binary_to_base64(S= <<_:16>>, Acc) ->
    Last = bytes_to_base64(S),
    <<Acc/binary, Last/binary, "=">>;
binary_to_base64(<<H:24, T/binary>>, Acc) ->
    Base64 = bytes_to_base64(<<H:24>>),
    binary_to_base64(T, <<Acc/binary, Base64/binary>>).

% convert a string of bytes (at most 3) to their base64 representation.
% no tail = padding is provided.
-spec bytes_to_base64(<<_:8>>) -> <<_:16>>;
                     (<<_:16>>) -> <<_:24>>;
                     (<<_:24>>) -> <<_:32>>.
bytes_to_base64(<<A:8>>) ->
    <<S:6, T:2, U:4, _:4>> = <<A, 0>>,
    binary_map(fun to_base64_char/1, <<S:8, T:4, U:4>>);
bytes_to_base64(<<A:16>>) ->
    <<S:6, T:2, U:4, V:4, W:2, _:6>> = <<A:16, 0>>,
    binary_map(fun to_base64_char/1, <<S:8, T:4, U:4, V:6, W:2>>);
bytes_to_base64(A) ->
    <<S:6, T:2, U:4, V:4, W:2, X:6>> = A,
    binary_map(fun to_base64_char/1, <<S:8, T:4, U:4, V:6, W:2, X:8>>).
