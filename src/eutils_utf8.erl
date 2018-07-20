-module(eutils_utf8).
-export([cut/3]).

-spec cut(binary(), integer(), integer()) -> binary().
cut(Binary, Start, Limit) when is_binary(Binary) ->
    Part0 = binary:part(Binary, Start, Limit),
    Part1 = fix(Part0),
    binary:copy(Part1).


%% INTERNALS

fix(Value) when is_binary(Value) ->
    case unicode:characters_to_binary(Value) of
        {error, Encoded, _RestData} -> Encoded;
        {incomplete, Encoded, _RestBinary} -> Encoded;
        Encoded -> Encoded
    end.