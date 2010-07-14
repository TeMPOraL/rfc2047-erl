%%%---------------------------------------------------------------------
%%% Description of module ucs_encode
%%%---------------------------------------------------------------------
%%% Useful lecture:
%%% http://www.faqs.org/rfcs/rfc822.html  (RFC822)
%%% http://www.faqs.org/rfcs/rfc1341.html (RFC1341)
%%% http://www.faqs.org/rfcs/rfc1342.html (RFC1342)
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% decode(Encoded)
%%%   Returns a list of UCS-4 Code Points, with each number representing
%%%   a single code point. Encoded should be a binary representing
%%%   text data following rules described in RFC1342. This function will
%%%   decode Q-encoded and BCD-encoded strings and unify all charsets
%%%   into single UCS-4 representation.
%%%---------------------------------------------------------------------
%%% decode2iolist(Encoded)
%%%   Returns an iolist of UCS-4 Code Points, with each number representing
%%%   a single code point. Encoded should be a binary representing
%%%   text data following rules described in RFC1342. This function will
%%%   decode Q-encoded and BCD-encoded strings and unify all charsets
%%%   into single UCS-4 representation.
%%%---------------------------------------------------------------------

-module(rfc1342).
-created('12.07.2010').
-created_by('jacek.zlydach@erlang-solutions.com').
-export([decode/1, decode2iolist/1]).

%% Unit testing
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------
%% Public interface
%%----------------------------------------------------------------------
-spec(decode/1 :: (binary()) -> string()).
decode(Encoded) ->
    lists:flatten(decode2iolist(Encoded)).

-spec(decode2iolist/1 :: (binary()) -> iolist()).
decode2iolist(Encoded) ->
    lists:map(fun convert_string_part_to_ucs/1,
                split_string_to_conversion_segments(Encoded)).

%%----------------------------------------------------------------------
%% Private implementation
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Function: split_string_to_conversion_segments/1
%% Purpose:  Splits binary ASCII-7 - encoded string to segments that
%%           will or won't need conversions from RFC1342 encoding to
%%           UCS.
%% Args:     Binary - a binary ASCII-7 string.
%% Returns:  A list of already UCS-encoded strings and tuples
%%           {charset, encoding, string to convert} needing conversion.
%%----------------------------------------------------------------------
split_string_to_conversion_segments(Binary) ->
     split_string_to_conv_segments_tail(Binary, [[]]).

split_string_to_conv_segments_tail(<<>>, Acc) ->
    lists:reverse(Acc);
split_string_to_conv_segments_tail(<<Char1, Char2, Rest/binary>>, [AccH|Acc])
  when Char1 == $=, Char2 == $? ->
    Result = isolate_next_convertable_substring(Rest),
    case Result of
	{Convertable, RestOfstring} ->
	    split_string_to_conv_segments_tail(RestOfstring, [[], Convertable, AccH | Acc]);
	not_found ->
	    split_string_to_conv_segments_tail(Rest, [(AccH ++ [Char1, Char2]) | Acc])
		%% FIXME
		%% We can drop our search at this point
    end;
split_string_to_conv_segments_tail(<<Char, Rest/binary>>, [AccH|Acc]) ->
    split_string_to_conv_segments_tail(Rest, [(AccH ++ [Char]) | Acc]).

%%----------------------------------------------------------------------
%% Function: isolate_next_convertable_substring/1
%% Purpose:  Extracts a single fragment of text that should undergo
%%           conversion to UCS.
%% Args:     BinaryString - a binary ASCII-7 string.
%% Returns:  {{charset, encoding, string}, rest of binary string} |
%%           not_found
%%----------------------------------------------------------------------
isolate_next_convertable_substring(BinaryString) ->
    isolate_next_convertable_substring_tail(BinaryString,
					    {[], undefined, undefined},
					    0).
%% Limit of 75 chars enforced by RFC 1342 (including initial =? delimiter)
isolate_next_convertable_substring_tail(_, _, 73) -> not_found;
isolate_next_convertable_substring_tail(<<>>, _, _) -> not_found;
%% Process first part
isolate_next_convertable_substring_tail(<<Char, Rest/binary>>,
					{Str, undefined, undefined},
					Count) when Char == $? ->
    isolate_next_convertable_substring_tail(Rest, {Str, [], undefined}, Count+1);
isolate_next_convertable_substring_tail(<<Char, Rest/binary>>,
					{Str, undefined, undefined},
					Count) ->
    isolate_next_convertable_substring_tail(Rest, {Str ++ [Char], undefined, undefined}, Count+1);
%% Process second part
isolate_next_convertable_substring_tail(<<Char, Rest/binary>>,
					{PrevPart, Str, undefined},
					Count) when Char == $? ->
    isolate_next_convertable_substring_tail(Rest, {PrevPart, Str, []}, Count + 1);

isolate_next_convertable_substring_tail(<<Char, Rest/binary>>,
					{PrevPart, Str, undefined},
					Count) ->
    isolate_next_convertable_substring_tail(Rest, {PrevPart, Str ++ [Char], undefined}, Count + 1);

%%FIXME handle Count < 71 (or sth like that) condition to drop out invalid entries
isolate_next_convertable_substring_tail(<<Char1, Char2, Rest/binary>>, Result, _Count)
  when Char1 == $?, Char2 == $= ->
    {Result, Rest};

isolate_next_convertable_substring_tail(<<Char1, _Rest/binary>>, _Result, _Count)
  when Char1 == $? ->
    not_found;

isolate_next_convertable_substring_tail(<<Char, Rest/binary>>,
					{FirstPart, SecondPart, Str},
					Count) ->
    isolate_next_convertable_substring_tail(Rest,
					    {FirstPart, SecondPart, Str ++ [Char]},
					    Count + 1).

%%----------------------------------------------------------------------
%% Function: convert_string_parts_to_ucs/1
%% Purpose:  Converts found tuples {charset, encoding, string} to lists
%%           of UCS Code Points.
%% Args:     Binary - a binary ASCII-7 string.
%% Returns:  A list of numbers (UCS Code Points), or error code
%%           propagated from convert_string_parts_to_ucs/2.
%%----------------------------------------------------------------------

%% Phase one - decode BASE64 or Q-encoding
%  BASE-64
convert_string_part_to_ucs({Charset, "B", Text}) ->
    convert_decoded_string_to_ucs(Charset, base64:decode(Text));
convert_string_part_to_ucs({Charset, "b", Text}) ->
    convert_decoded_string_to_ucs(Charset, base64:decode(Text));
%  Q-string
convert_string_part_to_ucs({Charset, "Q", Text}) ->
    convert_decoded_string_to_ucs(Charset, decode_qstring(Text));
convert_string_part_to_ucs({Charset, "q", Text}) ->
    convert_decoded_string_to_ucs(Charset, decode_qstring(Text));

convert_string_part_to_ucs(SomethingElse) ->
    SomethingElse.

%%----------------------------------------------------------------------
%% Function: convert_decoded_string_to_ucs/2
%% Purpose:  Performs the real conversion from given charset to UCS
%% Args:     Charset - a string describing charset of the Text argument
%%           (ie. "ISO-8859-2"). Note that it's passed directly to
%%           iconv.
%%           Text - binary or list containing text in specified Charset.
%% Returns:  A list of numbers (UCS Code Points) |
%%           {error, conversion} - on failed conversion
%%           {error, charset} - if failed to create charset converter.
%%----------------------------------------------------------------------
convert_decoded_string_to_ucs(Charset, Text) ->
    Result = iconv:open("UCS-4", Charset),
    case Result of
	{ok, Handler} ->
	    ConvResult = iconv:conv(Handler, Text),
	    case ConvResult of
		{ok, ConvertedText} ->
		    iconv:close(Handler),
		    binary_to_4byte_list(ConvertedText);
		_ -> {error, conversion}
	    end;
	_ -> {error, charset}
    end.

%%----------------------------------------------------------------------
%% Function: decode_qstring/1
%% Purpose:  Converts q-string to normal string.
%% Args:     Text - list of characters to be decoded.
%% Returns:  Decoded string.
%%----------------------------------------------------------------------
decode_qstring(Text) ->
    decode_qstring_tail(Text, []).

decode_qstring_tail([], Acc) ->
    lists:reverse(Acc);
decode_qstring_tail([$\=, Hi, Lo | Rest], Acc) ->
    decode_qstring_tail(Rest, [hex_chars_2_dec(Hi, Lo) | Acc]);
decode_qstring_tail([H | Rest], Acc) when H == $_ ->
    decode_qstring_tail(Rest, [16#20 | Acc]);
decode_qstring_tail([H | Rest], Acc) ->
    decode_qstring_tail(Rest, [H | Acc]).

%%----------------------------------------------------------------------
%% Function: hex_chars_2_dec/2
%% Purpose:  Converts a two digit hexadecimal number given as ASCII
%%           characters to an integer.
%% Args:     Hi, Lo - characters representing hexadecimal digits.
%% Returns:  Number represented by passed arguments.
%%----------------------------------------------------------------------
hex_chars_2_dec(Hi, Lo) ->
    16*hex_digit_2_dec(Hi) + hex_digit_2_dec(Lo).

hex_digit_2_dec(Digit) when Digit >= $a ->
    (Digit - $a) + 10;
hex_digit_2_dec(Digit) when Digit >= $A ->
    (Digit - $A) + 10;
hex_digit_2_dec(Digit) when Digit >= $0 ->
    Digit - $0.

%%----------------------------------------------------------------------
%% Function: binary_to_4byte_list/1
%% Purpose:  Converts a binary to list of integers, but treating every
%%           4 bytes as a single number.
%% Args:     Binary - binary to be converted. Must be 4-byte aligned -
%%           its size in bytes must be divisible by 4.
%% Returns:  List of integers, or
%%           {error, 'Binary not aligned to 4 bytes'}
%%----------------------------------------------------------------------
binary_to_4byte_list(Binary) ->
    binary_to_4byte_list_tail(Binary, []).

binary_to_4byte_list_tail(<<>>, Acu) ->
    lists:reverse(Acu);
binary_to_4byte_list_tail(<<Number:32, Rest/binary>>, Acu) ->
    binary_to_4byte_list_tail(Rest, [Number | Acu]);
binary_to_4byte_list_tail(<<_Rest/binary>>, _Acu) ->
    {error, 'Binary not aligned to 4 bytes'}.


%%----------------------------------------------------------------------
%% Unit tests
%% HIC SUNT DRACONES
%%----------------------------------------------------------------------
split_string_to_conversion_segments_test() ->
    ?assert(length(split_string_to_conversion_segments(<<"Now =?ISO-8859-1?Q?I am become Death?=,? ? ? the destroyer of =?ISO-8859-1?Q?worlds?=.">>)) == 5),
    %% RFC1342 says that a valid encoded word may contain only four question marks (including delimiters).
    ?assert(length(split_string_to_conversion_segments(<<"=?CODING?= ??? ?=HORROR">>)) == 1),
    %% RFC1342 limits the length of an encoded word to 75 characters (including encoding anc charset).
    ?assert(length(split_string_to_conversion_segments(<<"=?ISO-8859-1?Q?Any_sufficiently_complicated_C_or_Fortran_program_contains_an_ad_hoc,_informally-specified,_bug-ridden,_slow_implementation_of_half_of_Common_Lisp.?=_-_Greenspun's_Tenth_Rule_of_Programming">>)) == 1).

%% TODO write tests imposing limits of 75 characters per encoded word, space separation, no invalid characters inside encoded words, ect.

decode_qstring_test() ->
    "Now I am become Death, the destroyer of worlds." = 
	decode_qstring("Now=20I=20am_become_=44eath,_the destroyer of=20worlds.").
hex_chars_2_dec_test() ->
    ok. %% TODO
hex_digit_2_dec_test() ->
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] =
	lists:map(fun hex_digit_2_dec/1,
		  [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F]),
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] =
	lists:map(fun hex_digit_2_dec/1,
		  [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f]).
%% NOTE - are lowercase hex letters allowed?

binary_to_4byte_list_test() ->
    {error, 'Binary not aligned to 4 bytes'} = binary_to_4byte_list(<<1>>),
    {error, 'Binary not aligned to 4 bytes'} = binary_to_4byte_list(<<1,2,3,4,5>>).
