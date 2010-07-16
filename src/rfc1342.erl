%%%---------------------------------------------------------------------
%%% RFC1342 Decoder
%%% @author Jacek Zlydach <jacek.zlydach@erlang-solutions.com>
%%% @todo Krzysiek - sign yourself here :)
%%%---------------------------------------------------------------------
%%% @doc 
%%% RFC1342 Decoder - a simple tool to decode strings conforming to
%%% RFC1342 that can be found in e-mail headers.
%%%
%%% The <em>primary goal is to make this library interpret strings as close
%%% to the way RFC1342 describes it as possible</em> - including those tricky
%%% edge cases. Therefore, unless otherwise noted, every case when
%%% this library interprets a string one way, and RFC1342 says
%%% something else, it should be considered as a bug.
%%%
%%% This library uses <a href="http://github.com/Vagabond/erlang-iconv/">erlang-iconv</a>
%%% for charset conversions. Therefore, you need to have it installed on
%%% your system before using `RFC1342 Decoder'. Moreover, you should have
%%% `iconv' started (ie. by calling `iconv:start/0') before calling any
%%% function from this library.
%%% @end
%%%
%%% Useful lecture:
%%% @reference <a href="http://www.faqs.org/rfcs/rfc822.html">RFC822</a>
%%% @reference <a href="http://www.faqs.org/rfcs/rfc1341.html">RFC1341</a>
%%% @reference <a href="http://www.faqs.org/rfcs/rfc1342.html">RFC1342</a>
%%% @end
%%%---------------------------------------------------------------------
%%% Known issues
%%%   * 75 character limit for code word may not be precisely checked
%%%     (off-by-one error, ect.)</li>
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% decode(Encoded)
%%% decode2iolist(Encoded)
%%%---------------------------------------------------------------------

-module(rfc1342).
-created('12.07.2010').
-created_by('jacek.zlydach@erlang-solutions.com').
-export([decode/1]).%, decode2iolist/1]).

%% Constants
-define(RFC1342_MAX_CODESTRING_LENGTH, 73).
-define(RFC2047_MAX_CODESTRING_LENGTH_WITHOUT_INITIAL_DELIMITERS, 73).
-define(TEST, true).
%% Unit testing
%% For unit testing in the shell you may want to compile this module with:
%%   c(rfc1342, [{d, 'TEST'}])
%% to introduce TEST macro and export all functions, so that
%% they are visible to rfc1342_tests module.
-ifdef(TEST).
-compile(export_all).
-endif.

%%----------------------------------------------------------------------
%% Public interface
%%----------------------------------------------------------------------

%%% decode(Encoded)
%%% @spec decode(Encoded::binary()) -> string()
%%% @doc
%%%   Returns a list of UCS-4 Code Points, with each number representing
%%%   a single code point. `Encoded' should be a binary representing
%%%   text data following rules described in RFC1342. This function will
%%%   decode Q-encoded and BCD-encoded strings and unify all charsets
%%%   into single UCS-4 representation.
%%% @end
%-spec(decode/1 :: (binary()) -> string()).
%decode(Encoded) ->
%    lists:flatten(decode2iolist(Encoded)).

%%% decode2iolist(Encoded)
%%% @spec decode2iolist(Encoded::binary()) -> iolist()
%%% @doc
%%%   Returns an iolist of UCS-4 Code Points, with each number representing
%%%   a single code point. `Encoded' should be a binary representing
%%%   text data following rules described in RFC1342. This function will
%%%   decode Q-encoded and BCD-encoded strings and unify all charsets
%%%   into single UCS-4 representation.
%%% @end
%% -spec(decode2iolist/1 :: (binary()) -> iolist()).
%% decode2iolist(Encoded) ->
%%     lists:map(fun convert_string_part_to_ucs/1,
%%                 split_string_to_conversion_segments(Encoded)).

%%----------------------------------------------------------------------
%% Public interface v2.0
%%----------------------------------------------------------------------
decode(Encoded) ->
    decode(Encoded, normal).
decode(Encoded, structured_field) ->
    lists:flatten(apply_display_rules(decode_encoded_words(parse_input(Encoded, comments))));
decode(Encoded, normal) ->
    lists:flatten(apply_display_rules(decode_encoded_words(parse_input(Encoded, normal)))).

%%----------------------------------------------------------------------
%% Private implementation v2.0
%%----------------------------------------------------------------------

%% NOTE
%% Way to optimize: we could make parser fire up a "parse top-level encoded words"
%% on just-parsed comment, thereby saving another pass.

%% apply_display_rules/1 should alter decoded token list to reflect display
%% rules of RFC2047 - remove spaces between adjacent encoded words. Also, this
%% function strips input tree out of token type tags, so that it may be simply flattened
%% in order to get a final string.
apply_display_rules(Tree) ->
    apply_display_rules(Tree, []).

apply_display_rules([], Acu) ->
    lists:reverse(Acu);
apply_display_rules([{decoded_word, A}, {linear_whitespace, _}, {decoded_word, B} | Rest],
		   Acu) ->
    apply_display_rules([{decoded_word, B} | Rest], [A | Acu]);
apply_display_rules([{comment, What} | Rest], Acu) ->
    apply_display_rules(Rest, [$), apply_display_rules(What, []), $( | Acu]);
apply_display_rules([{_, What} | Rest], Acu) ->
    apply_display_rules(Rest, [What | Acu]).

%% decode_encoded_words/1 should 
decode_encoded_words(List) ->
    lists:map(fun tree_decode_elements/1,
	      List).

%% parse_input/2 should convert binary string into list of tokens
parse_input(InputBinary, Mode) ->
    parse_input(InputBinary, Mode, false, []).

parse_input(<<>>, _, _, TokenAccu) ->
    lists:reverse(TokenAccu);

% In comment mode, reading {text, ...} token will break on encountering $( or $).
% In normal mode, both $( and $) will be treated as normal text.
% Recognize comments in comment mode.

parse_input(<<Char, Rest/binary>>, comments, InComment, TokenAccu) when Char == $(->
    {Token, RemainingBinary} = parse_input(Rest, comments, true, []),
    parse_input(RemainingBinary, comments, InComment, [Token | TokenAccu]);

% Finish comments in comment mode
parse_input(<<Char, Rest/binary>>, comments, true, TokenAccu) when Char == $) ->
    {{comment, lists:reverse(TokenAccu)}, Rest};

% Watch for potential encoded words
parse_input(Binary = <<Char, Char2, Rest/binary>>, CommentMode, InComment, TokenAccu) when
      Char  == $=,
      Char2 == $? ->
    %% try_scan_encoded_word/3 will return either {encoded_word, ...} or will backtrack
    %% by returning not_found, in which case we continue with reading it as text
    Result = try_scan_encoded_word(Rest, CommentMode, InComment),
    case Result of
	{Token, RestOfBinary} ->
	    parse_input(RestOfBinary, CommentMode, InComment, [Token | TokenAccu]);
	not_found ->
	    {Token, RestOfBinary} = try_scan_text(Binary, CommentMode, InComment),
	    parse_input(RestOfBinary, CommentMode, InComment, [Token | TokenAccu])
    end;

% Choose between linearwhitespace
parse_input(Binary, CommentMode, InComment, TokenAccu) ->
     case is_linear_whitespace_next(Binary) of
	true ->
	    {Token, RestOfBinary} = try_scan_linear_whitespace(Binary);
	_ ->
	    {Token, RestOfBinary} = try_scan_text(Binary, CommentMode, InComment)
    end,
    parse_input(RestOfBinary, CommentMode, InComment, [Token | TokenAccu]).

%%----------------------------------------------------------------------
%% Private implementation
%%----------------------------------------------------------------------

%% (15.07.2010) Will finish that tomorrow...

%% KALENDARZ - AGA PO 26-TYM

tree_decode_elements({comment, CommentData}) ->
    {comment, lists:map(fun tree_decode_elements/1,
	      CommentData)};
tree_decode_elements({_, {Charset, Encoding, Word}} = {encoded_word, EncodedWord}) ->
    case (catch convert_string_part_to_ucs(EncodedWord)) of
	{_, _} -> {text, "=?" ++ Charset ++ "?" ++ Encoding ++ "?" ++ Word ++ "?="};
	Result -> {decoded_word, Result}
    end;
%    {decoded_word, convert_string_part_to_ucs(EncodedWord)};
tree_decode_elements(Rest) ->
    Rest.

try_scan_linear_whitespace(Binary) ->
    try_scan_linear_whitespace(Binary, []).

try_scan_linear_whitespace(Binary, Buffer) ->
    case is_linear_whitespace_next(Binary) of
	true ->
	    {WS, Rest} = grab_linear_whitespace(Binary),
	    try_scan_linear_whitespace(Rest, [WS | Buffer]);
	false ->
	    {{linear_whitespace, lists:reverse(Buffer)}, Binary}
    end.


try_scan_text(InputBinary, Mode, InComment) ->
    try_scan_text(InputBinary, Mode, InComment, []).

% end of binary == end of processing
try_scan_text(<<>>, _, _, Accu) ->
    {{text, lists:reverse(Accu)}, <<>>};

% handle comments in comment mode
try_scan_text(<<Char, Rest/binary>>, comments, _ , Accu) when Char == $( ->
    {{text, lists:reverse(Accu)}, <<Char, Rest/binary>>};

try_scan_text(<<Char, Rest/binary>>, comments, true , Accu) when Char == $) ->
    {{text, lists:reverse(Accu)}, <<Char, Rest/binary>>};

% linear whitespaces are handled elsewhere; otherwise read stuff
try_scan_text(Binary = <<Char, Rest/binary>>, Mode, InComment, Accu) ->
    case is_linear_whitespace_next(Binary) of
	true -> {{text, lists:reverse(Accu)}, Binary};
	_ -> try_scan_text(Rest, Mode, InComment, [Char | Accu])
    end.

try_scan_encoded_word(Binary, CommentMode, InComment) ->
    try_scan_encoded_word(Binary,
			  CommentMode,
			  InComment,
			  {[], undefined, undefined},
			  0).

% NOTE Legal (final) whitespaces will be checked along with ?=

% Length limit.
try_scan_encoded_word(_, _, _, _, ?RFC2047_MAX_CODESTRING_LENGTH_WITHOUT_INITIAL_DELIMITERS) ->
    not_found;
% Fail if end-of-data reached before completion of encoded word.
try_scan_encoded_word(<<>>, _, _, _, _) ->
    not_found;

% Process first part (charset)
try_scan_encoded_word(<<Char, Rest/binary>>, CM, InC, {Str, undefined, undefined}, Count)
  when Char == $? ->
    try_scan_encoded_word(Rest, CM, InC, {lists:reverse(Str), [], undefined}, Count+1);
try_scan_encoded_word(Binary = <<Char, Rest/binary>>,
		      CM,
		      InC,
		      {Str, undefined, undefined},
		      Count) ->
    case is_linear_whitespace_next(Binary) of
	true ->
	    not_found;
	_ ->
	    try_scan_encoded_word(Rest, CM, InC, {[Char|Str], undefined, undefined}, Count+1)
    end;

% Process second part (encoding)
try_scan_encoded_word(<<Char, Rest/binary>>, CM, InC, {PrevStr, Str, undefined}, Count)
  when Char == $? ->
    try_scan_encoded_word(Rest, CM, InC, {PrevStr, lists:reverse(Str), []}, Count+1);
try_scan_encoded_word(Binary = <<Char, Rest/binary>>,
		      CM,
		      InC,
		      {PrevStr, Str, undefined},
		      Count) ->
    case is_linear_whitespace_next(Binary) of
	true ->
	    not_found;
	_ ->
	    try_scan_encoded_word(Rest, CM, InC, {PrevStr, [Char|Str], undefined}, Count+1)
    end;
% Process third part (encoded word)
% a) comment-ending
% b) linear-whitespace-ending
% c) legal char addition
try_scan_encoded_word(<<Char, Char2, Char3, Rest/binary>>,
		      comments,
		      true,
		      {FirstPart, SecondPart, Str},
		      _Count) 
  when Char  == $?,
       Char2 == $=,
       Char3 == $) ->
    {{encoded_word, {FirstPart, SecondPart, lists:reverse(Str)}}, <<Char3, Rest/binary>>};
try_scan_encoded_word(<<Char, Char2, Rest/binary>>,
		     _CM,
		     _InC,
		     {FirstPart, SecondPart, Str},
		     _Count)
  when Char  == $?,
       Char2 == $= ->
    case is_linear_whitespace_next(Rest) of
	true ->
	    {{encoded_word, {FirstPart, SecondPart, lists:reverse(Str)}}, Rest};
	_ ->
	    case size(Rest) of
		0 -> {{encoded_word, {FirstPart, SecondPart, lists:reverse(Str)}}, Rest};
		_ -> not_found
	    end
    end;
try_scan_encoded_word(Binary = <<Char, Rest/binary>>,
		      CM,
		      InC,
		      {FirstPart, SecondPart, Str},
		      Count) ->
    case is_linear_whitespace_next(Binary) of
	true ->
	    not_found;
	false ->
	    try_scan_encoded_word(Rest,
				  CM,
				  InC,
				  {FirstPart, SecondPart, [Char | Str]},
				  Count + 1)
    end.
    

is_linear_whitespace_next(<<16#20, _/binary>>) ->
    true;
is_linear_whitespace_next(<<$\t, _/binary>>) ->
    true;
is_linear_whitespace_next(<<13, 10, _/binary>>) ->
    true;
is_linear_whitespace_next(_) ->
    false.

grab_linear_whitespace(<<16#20, Rest/binary>>) ->
    {16#20, Rest};
grab_linear_whitespace(<<$\t, Rest/binary>>) ->
    {$\t, Rest};
grab_linear_whitespace(<<13, 10, Rest/binary>>) ->
    {[13, 10], Rest}.

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
%% @todo Optimize this function to use cons and list:reverse instead of
%%       appending.
isolate_next_convertable_substring(BinaryString) ->
    isolate_next_convertable_substring_tail(BinaryString,
					    {[], undefined, undefined},
					    0).
%% Limit of 75 chars enforced by RFC 1342 (including initial =? delimiter)
isolate_next_convertable_substring_tail(_, _, ?RFC1342_MAX_CODESTRING_LENGTH) -> not_found;
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

% handle removal of space after encoded word
isolate_next_convertable_substring_tail(<<Char1, Char2, Char3, Rest/binary>>, Result, Count)
  when Char1 == $?,
       Char2 == $=,
       Char3 == 16#20,
       Count =< (?RFC1342_MAX_CODESTRING_LENGTH-2) % <- 2 is because we capture 2 chars
       ->
    {Result, Rest};

% handle removal of newline after encoded word
isolate_next_convertable_substring_tail(<<Char1, Char2, Char3, Char4, Rest/binary>>, Result, Count)
  when Char1 == $?,
       Char2 == $=,
       Char3 == $\r,
       Char4 == $\n,
       Count =< (?RFC1342_MAX_CODESTRING_LENGTH-2) % <- 2 is because we capture 2 chars
       ->
    {Result, Rest};

isolate_next_convertable_substring_tail(<<Char1, Char2, Rest/binary>>, Result, Count)
  when Char1 == $?,
       Char2 == $=,
       Count =< (?RFC1342_MAX_CODESTRING_LENGTH-2) % <- 2 is because we capture 2 chars
       ->
    {Result, Rest};

isolate_next_convertable_substring_tail(<<Char1, _Rest/binary>>, _Result, _Count)
  when Char1 == $? ->
    not_found;

isolate_next_convertable_substring_tail(<<Char1, _Rest/binary>>, _Result, _Count)
  when Char1 == 16#20 ->
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
    convert_decoded_string_to_ucs(Charset, decode_qstring(Text)).

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
%% @todo make this function die if invalid character is specified
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

