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

%% New API

% create a hex type with the given binary string (no conversion)
-spec new_hex(binary()) -> hex().
new_hex(S) when is_binary(S) -> {hex, S}.

% create a base64 type with the given binary string (no conversion)
-spec new_base64(binary()) -> base64().
new_base64(S) when is_binary(S) -> {base64, S}.

%% Conversion API

% convert a string binary to a base64 type
-spec binary_to_base64(binary()) -> binary().
binary_to_base64(S) -> {base64, binary_to_base64(S, <<>>)}.

% convert a string binary to a hex type
-spec binary_to_hex(binary()) -> binary().
binary_to_hex(S) ->
    Fun = fun(A, X) ->
            B = byte_to_hex(X),
            <<A/binary, B>>
    end,
    {hex, binary_foldl(Fun, <<>>, S)}.

%% convert base64 type to a binary string
-spec base64_to_binary(base64()) -> binary().
base64_to_binary({base64, S}) -> base64_to_binary(S, <<>>).

% convert a base64 type to a hex type
-spec base64_to_hex(base64()) -> hex().
base64_to_hex({base64, _String}) -> throw(not_implemented).

% convert a hex type to a base64 type
-spec hex_to_base64(hex() | binary()) -> base64().
hex_to_base64({hex, _String}) -> throw(not_implemented).

%% Helper Methods

% convert a single byte into its value in the base64 table
-spec to_base64_char(byte()) -> byte().
to_base64_char(B) when B >= 0 andalso B =< 25 -> B + 65;
to_base64_char(B) when B >= 26 andalso B =< 51 -> B + 71;
to_base64_char(B) when B >= 52 andalso B =< 61 -> B - 4;
to_base64_char(62) -> 43;
to_base64_char(63) -> 47.

% convert a base64 value to its original byte
-spec from_base64_char(byte()) -> byte().
from_base64_char(C) when C >= 65 andalso C =< 90 -> C - 65;
from_base64_char(C) when C >= 97 andalso C =< 122 -> C - 71;
from_base64_char(C) when C >= 48 andalso C =< 57 -> C + 4;
from_base64_char(43) -> 62;
from_base64_char(63) -> 47.

% convert a hex byte to its a printable representation
to_hex_char(X) when X >= 0 andalso X =< 9 -> X + 48;
to_hex_char(X) when X >= 10 andalso X =< 15 -> X + 55.

% convert a byte to a printable hex representation
byte_to_hex(<<A:4, B:4>>) ->
    HA = to_hex_char(A),
    HB = to_hex_char(B),
    <<HA, HB>>.

% map F over the binary B
-spec binary_map(fun((byte()) -> byte()), binary()) -> binary().
binary_map(F, B) ->  binary:list_to_bin(lists:map(F, binary:bin_to_list(B))).

% fold from the left using F over the binary B starting with accumulator A
-spec binary_foldl(fun((A, byte()) -> A), A, binary()) -> A.
binary_foldl(F, A, B) -> lists:foldl(F, A, binary:bin_to_list(B)).

% convert a binary string into its base64 representation
-spec binary_to_base64(binary(), binary()) -> binary().
binary_to_base64(<<>>, Acc) -> Acc;
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

% conver a base64 string to its binary representation
-spec base64_to_binary(binary(), binary()) -> binary().
base64_to_binary(<<>>, Acc) -> Acc;
base64_to_binary(<<H:32, T/binary>>, Acc) ->
    BH = base64_to_bytes(<<H:32>>),
    base64_to_binary(T, <<Acc/binary, BH/binary>>).

% convert 4 base64 encoded bytes into at most 3 decimal values
-spec base64_to_bytes(<<_:32>>) -> <<_:8>> | <<_:16>> | <<_:24>>.
base64_to_bytes(<<A, B, 61, 61>>) ->
    <<_:2, S:6, _:2, T:2, _:4>> = binary_map(fun from_base64_char/1, <<A, B>>),
    <<S:6, T:2>>;
base64_to_bytes(<<A, B, C, 61>>) ->
    DS = binary_map(fun from_base64_char/1, <<A, B, C>>),
    <<_:2, S:6, _:2, T:2, U:4, _:2, V:4, _:2>> = DS,
    <<S:6, T:2, U:4, V:4>>;
base64_to_bytes(Str) ->
    DS = binary_map(fun from_base64_char/1, Str),
    <<_:2, S:6, _:2, T:2, U:4, _:2, V:4, W:2, _:2, X:6>> = DS,
    <<S:6, T:2, U:4, V:4, W:2, X:6>>.
