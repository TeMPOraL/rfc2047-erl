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
%%% convert_to_ucs_encoding(BinaryString)
%%%   Returns a list of UCS-4 Code Points, with each number representing
%%%   a single code point. BinaryString should be a binary representing
%%%   text data following rules described in RFC1342. This function will
%%%   decode Q-encoded and BCD-encoded strings and unify all charsets
%%%   into single UCS-4 representation.
%%%---------------------------------------------------------------------

-module(rfc1342).
-created('12.07.2010').
-created_by('jacek.zlydach@erlang-solutions.com').
-export([convert_to_ucs_encoding/1]).

%%----------------------------------------------------------------------
%% Public interface
%%----------------------------------------------------------------------
convert_to_ucs_encoding(BinaryString) ->
    lists:flatten(lists:map(fun convert_string_part_to_ucs/1,
			    split_string_to_conversion_segments(BinaryString))).

%%----------------------------------------------------------------------
%% Private implementation
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Function: split_string_to_conversion_segments/1
%% Purpose:  Splits binary ASCII-7 - encoded string to segments that will
%%           or won't need conversions from RFC1342 encoding to UCS.
%% Args:     Binary - a binary ASCII-7 string.
%% Returns:  A list of numbers (UCS Code Points) or optionally tuples
%%           {charset, encoding, string to convert}.
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

decode_qstring_tail([], Acc) -> Acc;
decode_qstring_tail([$\=, Hi, Lo | Rest], Acc) ->
    decode_qstring_tail(Rest, Acc ++ [hex_chars_2_dec(Hi, Lo)]);
decode_qstring_tail([H | Rest], Acc) when H == $_ ->
    decode_qstring_tail(Rest, Acc ++ [16#20]);
decode_qstring_tail([H | Rest], Acc) ->
    decode_qstring_tail(Rest, Acc ++ [H]).

%%----------------------------------------------------------------------
%% Function: hex_chars_2_dec/2
%% Purpose:  Converts a two digit hexadecimal number given as ASCII
%%           characters to an integer.
%% Args:     Hi, Lo - characters representing hexadecimal digits.
%% Returns:  Number represented by passed arguments.
%%----------------------------------------------------------------------
hex_chars_2_dec(Hi, Lo) ->
    16*hex_digit_2_dec(Hi) + hex_digit_2_dec(Lo).

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
    Acu;
binary_to_4byte_list_tail(<<Number:32, Rest/binary>>, Acu) ->
    binary_to_4byte_list_tail(Rest, Acu ++ [Number]);
binary_to_4byte_list_tail(<<_Rest/binary>>, _Acu) ->
    {error, 'Binary not aligned to 4 bytes'}.
