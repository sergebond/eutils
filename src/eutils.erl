-module(eutils).
-author("srg").

%% CONVERTERS
-export([
  to_bin/1,
  to_str/1,
  to_list/1,
  to_int/1,
  to_int/2,
  to_float/1,
  to_atom/1,
  to_boolean/1,
  to_lower/1
]).

%% MISC
-export([
  get_value/2,
  get_value/3,
  set_value/3,
  delete_key/2,
  delete_keys/2
]).

%% BINARIES
-export([
  bjoin/1,
  bjoin/2
]).

%% JSON
-export([
  to_json/1,
  from_json/1,
  from_json/2
]).

%% HTTP
-export([
  compose_body_encode/1,
  urlencode/1,
  join_form/1,
  x_www_form_urlencoded/1
]).

-export([hexstring/1]).

-export([
  gen_rand_id/1,
  get_unixtime/0,
  get_timestamp_in_millisec/0
]).

-export([
  get_random_string/1,
  get_random_string/2,
  pick_random/1,
  randint/2
]).

%%------------------TYPE CONVERSION-------------------------------------------------------------------------------------
%% @doc universal converter to binary
-spec to_bin(binary()|list()|integer()|atom()|float()) -> binary().
to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X);
to_bin(X) when is_integer(X) -> integer_to_binary(X);
to_bin(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_bin(X) when is_float(X) -> float_to_binary(X, [{decimals, 4}]).


to_str(X) -> to_list(X).
%% @doc universal converter to string(list)
-spec to_list(binary()|list()|integer()|atom()|float()) -> list().
to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_float(X) -> float_to_list(X,[{decimals, 4}]).

%% @doc universal converter to integer
-spec to_int(binary()|list()|integer()|atom()) -> integer().
to_int(X) when is_integer(X) -> X;
to_int(X) when is_binary(X) -> binary_to_integer(X);
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_float(X) -> round(X);
to_int(X) when is_atom(X) -> list_to_integer(atom_to_list(X)).

-spec to_int(term(), term()) -> integer()|term().
to_int(X, Default) ->
  try
    to_int(X)
  catch
    _:_ ->
      Default
  end.

%% @doc universal converter to float
-spec to_float(binary()|list()|float()) -> float().
to_float(X) when is_float(X) -> X;
to_float(X) when is_binary(X) -> binary_to_float(X);
to_float(X) when is_list(X) -> list_to_float(X).

%% @doc universal converter to atom
-spec to_atom(binary()|list()|float()) -> float().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> binary_to_atom(list_to_binary(X), utf8).

to_boolean(X) when X =:= <<"true">> orelse X =:= <<"false">> -> binary_to_atom( X , utf8);
to_boolean(X) when X =:= true orelse X =:= false ->  X;
to_boolean(X) when  X =:= "true" orelse X =:= "false" -> binary_to_atom(list_to_binary(X), utf8).

%%  MISC
%%______________________________________________________________________________________________________________________
get_value(Key, List)->
  get_value(Key, List, undefined).
get_value(Key, List, Default)->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    _        -> Default
  end.

set_value(Key, Value, PropList) ->
  lists:keystore(Key, 1, PropList, {Key, Value}).

delete_key(Key, PropList) ->
  lists:keydelete(Key, 1, PropList).

delete_keys(Keys, PropList) ->
  lists:foldl(
    fun(Key, List) ->
        lists:keydelete(Key, 1, List)
    end, PropList, Keys).

%%  BINARIES
%%______________________________________________________________________________________________________________________
-spec bjoin(List :: list(binary())) -> binary().
bjoin(List) -> iolist_to_binary(List).

bjoin([], _) -> <<>>;
bjoin([H|T], Separator) ->
  List = [H| [ [Separator, X] || X <- T ] ],
  iolist_to_binary(List).

%%----------------------------------------------------------------------
%%                        JSON ENCODE/DECODE
%%----------------------------------------------------------------------

% Term to json convertor -> binary()
to_json({ok, Data}) when is_binary(Data)->
  Data;
to_json(Data) when is_binary(Data)->
  Data;

% jsx
to_json(Data)->
  to_json_run(jiffy, Data).

% jiffy
to_json_run(jiffy, Data)->
  NewData = jiffy_encode_params(Data),
  try jiffy:encode( NewData ) of
    JSON    -> JSON
  catch _E:_Desc ->
%%    lager:error("[JSON] Error jiffy:encode ~p~n~p", [ NewData, erlang:get_stacktrace() ]) ,
    <<"error json encode">>
  end.

from_json( Text ) -> from_json( Text, <<"error_json_decode">> ).
from_json(Text, Default)->
  from_json_run(jiffy, Text, Default).


% jiffy
from_json_run(jiffy, null, Default)->
  Default;

from_json_run(jiffy, Text, Default)->
  try jiffy:decode( Text ) of
    Data  ->
      jiffy_decode_params(Data)
  catch _E:_Desc  ->
%%    lager:error("[JSON] Error jiffy:decode ~p ~n~p", [ Text, erlang:get_stacktrace() ]),
    Default
  end.

% encode
jiffy_encode_params(List = [{_, _} | _]) when is_list(List)->
  Res =
    lists:map(fun(Value) ->
      case Value of
        {Key, Val} -> {Key, jiffy_encode_param(Val)};
        _          -> jiffy_encode_params(Value)
      end
              end, List),
  {Res};

jiffy_encode_params(List) when is_list(List)->
  lists:map(fun(Value)-> jiffy_encode_params(Value) end, List);

jiffy_encode_params(Params)-> Params.


% [{},{},{}]
jiffy_encode_param(Val = [{_, _} | _])->
  jiffy_encode_params(Val);

% [[{},{},{}],[{},{},{}],[{},{},{}]]
jiffy_encode_param(Val) when is_list(Val)->
  lists:map(fun(Value)-> jiffy_encode_params(Value) end, Val);

jiffy_encode_param(Val)->
  Val.

% decode
jiffy_decode_params({List = [{_, _} | _]}) when is_list(List)->
  lists:map(fun(Value)->
    case Value of
      {Key, Val} -> {Key, jiffy_decode_param(Val)};
      _          -> jiffy_decode_params(Value)
    end
            end, List);

jiffy_decode_params(List) when is_list(List)->
  lists:map(fun(Value)-> jiffy_decode_params(Value) end, List);

jiffy_decode_params(Params)->
  Params.

% [{},{},{}]
jiffy_decode_param({Val = [{_, _} | _]})->
  jiffy_decode_params({Val});

% [[{},{},{}],[{},{},{}],[{},{},{}]]
jiffy_decode_param(Val) when is_list(Val)->
  lists:map(fun(Value)-> jiffy_decode_params(Value) end, Val);

jiffy_decode_param(Val)->
  Val.

%%  HTTP_UTILS
%%______________________________________________________________________________________________________________________

compose_body_encode(Args) ->
  lists:concat(
    lists:foldl(
      fun(Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
      [],
      [eutils:to_str(K) ++ "=" ++ http_uri:encode(eutils:to_str(V)) || {K, V} <- Args]
    )
  ).

join_form(Form) ->
  UrlPars  = [ << (urlencode(to_bin(K)))/binary, <<"=">>/binary, (urlencode(to_bin(V)))/binary >>||{K,V} <- Form ],
  bjoin(UrlPars, <<"&">>).

-spec urlencode(B) -> B when B::binary().
urlencode(B) ->
  urlencode(B, <<>>).

urlencode(<< $!, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $! >>);
urlencode(<< $$, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $$ >>);
urlencode(<< $&, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $& >>);
urlencode(<< $', Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $' >>);
urlencode(<< $(, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $( >>);
urlencode(<< $), Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $) >>);
urlencode(<< $*, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $* >>);
urlencode(<< $+, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $+ >>);
urlencode(<< $,, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $, >>);
urlencode(<< $-, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $- >>);
urlencode(<< $., Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $. >>);
urlencode(<< $0, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $0 >>);
urlencode(<< $1, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $1 >>);
urlencode(<< $2, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $2 >>);
urlencode(<< $3, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $3 >>);
urlencode(<< $4, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $4 >>);
urlencode(<< $5, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $5 >>);
urlencode(<< $6, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $6 >>);
urlencode(<< $7, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $7 >>);
urlencode(<< $8, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $8 >>);
urlencode(<< $9, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $9 >>);
urlencode(<< $:, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $: >>);
urlencode(<< $;, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $; >>);
urlencode(<< $=, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $= >>);
urlencode(<< $@, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $@ >>);
urlencode(<< $A, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $A >>);
urlencode(<< $B, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $B >>);
urlencode(<< $C, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $C >>);
urlencode(<< $D, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $D >>);
urlencode(<< $E, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $E >>);
urlencode(<< $F, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $F >>);
urlencode(<< $G, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $G >>);
urlencode(<< $H, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $H >>);
urlencode(<< $I, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $I >>);
urlencode(<< $J, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $J >>);
urlencode(<< $K, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $K >>);
urlencode(<< $L, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $L >>);
urlencode(<< $M, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $M >>);
urlencode(<< $N, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $N >>);
urlencode(<< $O, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $O >>);
urlencode(<< $P, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $P >>);
urlencode(<< $Q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Q >>);
urlencode(<< $R, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $R >>);
urlencode(<< $S, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $S >>);
urlencode(<< $T, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $T >>);
urlencode(<< $U, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $U >>);
urlencode(<< $V, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $V >>);
urlencode(<< $W, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $W >>);
urlencode(<< $X, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $X >>);
urlencode(<< $Y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Y >>);
urlencode(<< $Z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Z >>);
urlencode(<< $_, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $_ >>);
urlencode(<< $a, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $a >>);
urlencode(<< $b, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $b >>);
urlencode(<< $c, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $c >>);
urlencode(<< $d, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $d >>);
urlencode(<< $e, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $e >>);
urlencode(<< $f, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $f >>);
urlencode(<< $g, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $g >>);
urlencode(<< $h, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $h >>);
urlencode(<< $i, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $i >>);
urlencode(<< $j, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $j >>);
urlencode(<< $k, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $k >>);
urlencode(<< $l, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $l >>);
urlencode(<< $m, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $m >>);
urlencode(<< $n, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $n >>);
urlencode(<< $o, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $o >>);
urlencode(<< $p, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $p >>);
urlencode(<< $q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $q >>);
urlencode(<< $r, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $r >>);
urlencode(<< $s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $s >>);
urlencode(<< $t, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $t >>);
urlencode(<< $u, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $u >>);
urlencode(<< $v, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $v >>);
urlencode(<< $w, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $w >>);
urlencode(<< $x, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $x >>);
urlencode(<< $y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $y >>);
urlencode(<< $z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $z >>);
urlencode(<< $~, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $~ >>);
urlencode(<< C, Rest/bits >>, Acc) ->
  H = hex(C bsr 4),
  L = hex(C band 16#0f),
  urlencode(Rest, << Acc/bits, $%, H, L >>);
urlencode(<<>>, Acc) ->
  Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.


-spec x_www_form_urlencoded(binary()) -> list({binary(), binary() | true}).
x_www_form_urlencoded(<<>>) ->
  [];
x_www_form_urlencoded(Qs) ->
  Tokens = binary:split(Qs, <<"&">>, [global, trim]),
  [case binary:split(Token, <<"=">>) of
     [Token] -> {urldecode(Token), true};
     [Name, Value] -> {urldecode(Name), urldecode(Value)}
   end || Token <- Tokens].


%% @doc Decode a URL encoded binary.
%% @equiv urldecode(Bin, crash)
-spec urldecode(binary()) -> binary().
urldecode(Bin) when is_binary(Bin) ->
  urldecode(Bin, <<>>, crash).


-spec urldecode(binary(), binary(), crash | skip) -> binary().
urldecode(<<$%, H, L, Rest/binary>>, Acc, OnError) ->
  G = unhex(H),
  M = unhex(L),
  if	G =:= error; M =:= error ->
    case OnError of skip -> ok; crash -> erlang:error(badarg) end,
    urldecode(<<H, L, Rest/binary>>, <<Acc/binary, $%>>, OnError);
    true ->
      urldecode(Rest, <<Acc/binary, (G bsl 4 bor M)>>, OnError)
  end;
urldecode(<<$%, Rest/binary>>, Acc, OnError) ->
  case OnError of skip -> ok; crash -> erlang:error(badarg) end,
  urldecode(Rest, <<Acc/binary, $%>>, OnError);
urldecode(<<$+, Rest/binary>>, Acc, OnError) ->
  urldecode(Rest, <<Acc/binary, $ >>, OnError);
urldecode(<<C, Rest/binary>>, Acc, OnError) ->
  urldecode(Rest, <<Acc/binary, C>>, OnError);
urldecode(<<>>, Acc, _OnError) ->
  Acc.

-spec unhex(byte()) -> byte() | error.
unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(_) -> error.


hexstring(String) when is_list(String) ->
  lists:flatten(
    lists:map(fun(X) -> io_lib:format("~2.16.0b", [X]) end, String));

hexstring(Binary) when is_binary(Binary) ->
  hexstring(to_list(Binary)).


-spec get_unixtime() -> integer().
get_unixtime() ->
  {Mega, Secs, _} = os:timestamp(),
  Timestamp = Mega*1000000 + Secs,
  Timestamp.

-spec get_timestamp_in_millisec() -> integer().
get_timestamp_in_millisec() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% RANDOM ______________________________________________________________________________________________________________
gen_rand_id(Len) ->
  base64:encode( crypto:strong_rand_bytes(Len) ).

get_random_string(Length) ->
  AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890",
  get_random_string(Length, AllowedChars).

get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [pick_random(AllowedChars) | Acc] end, [], lists:seq(1, Length)).

pick_random(List) when is_list(List) ->
  lists:nth(rand:uniform(length(List)), List).

randint(A, B) -> %% Get random int beetween
  rand:uniform(B - A) + A.

%% STRING_______________________________________________________________________________________________________________

to_lower(S) when is_binary(S) ->
  unicode:characters_to_binary(
    to_lower(
      unicode:characters_to_list(S)
    )
  );
to_lower(S) when is_list(S) ->
  string:to_lower([char_to_lower(C) || C <- S]);
to_lower(C) when is_integer(C) ->
  char_to_lower(C).

char_to_lower($А) -> $а;
char_to_lower($Б) -> $б;
char_to_lower($В) -> $в;
char_to_lower($Г) -> $г;
char_to_lower($Ґ) -> $ґ;
char_to_lower($Д) -> $д;
char_to_lower($Е) -> $е;
char_to_lower($Є) -> $є;
char_to_lower($Ж) -> $ж;
char_to_lower($З) -> $з;
char_to_lower($И) -> $и;
char_to_lower($І) -> $і;
char_to_lower($Ї) -> $ї;
char_to_lower($Й) -> $й;
char_to_lower($К) -> $к;
char_to_lower($Л) -> $л;
char_to_lower($М) -> $м;
char_to_lower($Н) -> $н;
char_to_lower($О) -> $о;
char_to_lower($П) -> $п;
char_to_lower($Р) -> $р;
char_to_lower($С) -> $с;
char_to_lower($Т) -> $т;
char_to_lower($У) -> $у;
char_to_lower($Ф) -> $ф;
char_to_lower($Х) -> $х;
char_to_lower($Ц) -> $ц;
char_to_lower($Ч) -> $ч;
char_to_lower($Ш) -> $ш;
char_to_lower($Щ) -> $щ;
char_to_lower($Ь) -> $ь;
char_to_lower($Ю) -> $ю;
char_to_lower($Я) -> $я;

char_to_lower($Ё) -> $ё;
char_to_lower($Ъ) -> $ъ;
char_to_lower($Ы) -> $ы;
char_to_lower($Э) -> $э;

char_to_lower(Ch) -> Ch.
