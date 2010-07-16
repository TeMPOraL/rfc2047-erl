%%% Copyright (c) 2010, Erlang Solutions Ltd.
%%% All rights reserved.

%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * Neither the name of the Erlang Solutions Ltd. nor the
%%%       names of its contributors may be used to endorse or promote products
%%%       derived from this software without specific prior written permission.

%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL ERLANG SOLUTIONS LTD. BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



%%%---------------------------------------------------------------------
%%% RFC2047 Decoder
%%% @author Jacek Zlydach <jacek.zlydach@erlang-solutions.com>
%%% @author Krzysztof Goj <krzysztof.goj@erlang-solutions.com>
%%%---------------------------------------------------------------------
%%% @doc 
%%% RFC2047 Decoder - a simple tool to decode strings conforming to
%%% RFC2047 that can be found in e-mail headers and convert them to
%%% unified UCS-4 (Unicode) representation.
%%%
%%% The <em>primary goal is to make this library interpret strings as close
%%% to the way RFC2047 describes it as possible</em> - including those tricky
%%% edge cases. Therefore, unless otherwise noted, every case when
%%% this library interprets a string one way, and RFC2047 says
%%% something else, it should be considered as a bug.
%%%
%%% This library uses <a href="http://github.com/Vagabond/erlang-iconv/">erlang-iconv</a>
%%% for charset conversions. Therefore, you need to have it installed on
%%% your system before using `RFC2047 Decoder'. Moreover, you should have
%%% `iconv' started (ie. by calling `iconv:start/0') before calling any
%%% function from this library.
%%%
%%% All functions operate on header field body, passed as binary string.
%%% Because some headers (called "structured fields" in RFC 2047) have additional
%%% processing rules regarding encoded words, you should supply information whether
%%% input should be decoded as `structured-field' or normal text. See decode/2 for details.
%%%
%%% The output of this library is a list (or ioList) of UCS-4 code points.
%%%
%%% <h2>Usage example</h2>
%%%
%%% A typical `From:' header may contain a following address:
%%% `From: =?ISO-8859-2?Q?Andr=E9?= Pirard <PIRARD@vml.ulg.ac.be>'.
%%% Because `From:' is a `structured-field', an example decoding process would look like:
%%% ```
%%% > rfc2047:decode(<<"=?ISO-8859-2?Q?Andr=E9?= Pirard <PIRARD@vml.ulg.ac.be>">>, structured_field).
%%% "André Pirard <PIRARD@vml.ulg.ac.be>"
%%% '''
%%%
%%% For information whether a given header is `structured-field' or normal, see RFC 822.
%%% @end
%%%
%%% Useful lecture:
%%% @reference <a href="http://www.faqs.org/rfcs/rfc822.html">RFC822</a>
%%% @reference <a href="http://www.faqs.org/rfcs/rfc1341.html">RFC1341</a>
%%% @reference <a href="http://www.faqs.org/rfcs/rfc1342.html">RFC1342</a>
%%% @reference <a href="http://www.faqs.org/rfcs/rfc2047.html">RFC2047</a>
%%% @reference <a href="http://en.wikipedia.org/wiki/Unicode">Unicode (@ Wikipedia)</a>
%%% @reference <a href="http://en.wikipedia.org/wiki/UTF-32/UCS-4">UCS-4 (@ Wikipedia)</a>
%%% @end
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% decode/1, decode/2
%%% decode2iolist/1, decode2iolist/2
%%%---------------------------------------------------------------------

-module(rfc2047).
-created('12.07.2010').
-created_by('jacek.zlydach@erlang-solutions.com').
-export([decode/1, decode/2, decode2iolist/1, decode2iolist/2]).

%% Constants
-define(RFC2047_MAX_CODESTRING_LENGTH_WITHOUT_INITIAL_DELIMITERS, 73).

%% Unit testing
%% For unit testing in the shell you may want to compile this module with:
%%   c(rfc1342, [{d, 'TEST'}])
%% to introduce TEST macro and export all functions, so that
%% they are visible to rfc1342_tests module.

-ifdef(TEST).
-compile(export_all).
-endif.

%%----------------------------------------------------------------------
%% Public interface v2.0
%%----------------------------------------------------------------------

%%% decode(Encoded)
%%% @spec decode(Encoded::binary()) -> string()
%%% @doc
%%%   Returns a list of UCS-4 Code Points, with each number representing
%%%   a single code point.
%%%
%%%   Equivalent to: `decode(Encoded, normal)'.
%%%   @see decode/2.
%%% @end
-spec(decode/1 :: (binary()) -> string()).
decode(Encoded) ->
    decode(Encoded, normal).

%%% decode(Encoded, Mode)
%%% @spec decode(Encoded::binary(), Mode::atom()) -> string()
%%% @doc
%%%   Returns a list of UCS-4 Code Points, with each number representing
%%%   a single code point. `Encoded' should be a binary representing
%%%   text data following rules described in RFC2047. This function will
%%%   decode Q-encoded and B-encoded strings and unify all charsets
%%%   into single UCS-4 representation.
%%%
%%%   `Mode' should take one of two values:
%%%    <ul>
%%%         <li>`normal' - interpret input as normal text (like in `Subject' header)</li>
%%%         <li>`structured_field' - interpret input as structured field, which can contain comments in parentheses.</li>
%%%    </ul>
%%% @end
-spec(decode/2 :: (binary(), atom()) -> string()).
decode(Encoded, Mode) ->
    lists:flatten(decode2iolist(Encoded, Mode)).

%%% decode2iolist(Encoded)
%%% @spec decode2iolist(Encoded::binary()) -> iolist()
%%% @doc
%%%   Returns an iolist of UCS-4 Code Points, with each number representing
%%%   a single code point.
%%%
%%%   Equivalent to `decode2iolist(Encoded, normal)'.
%%%   @see decode2iolist/2.
%%% @end
-spec(decode2iolist/1 :: (binary()) -> iolist()).
decode2iolist(Encoded) ->
    decode2iolist(Encoded, normal).

%%% decode2iolist(Encoded, Mode)
%%% @spec decode2iolist(Encoded::binary(), Mode::atom()) -> iolist()
%%% @doc
%%%   Returns an iolist of UCS-4 Code Points, with each number representing
%%%   a single code point. `Encoded' should be a binary representing
%%%   text data following rules described in RFC2047. This function will
%%%   decode Q-encoded and B-encoded strings and unify all charsets
%%%   into single UCS-4 representation.
%%%
%%%   For description of `Mode' parameter and allowed values, see decode/2.
%%%   @see decode/2.
%%% @end
-spec(decode2iolist/2 :: (binary(), atom()) -> iolist()).
decode2iolist(Encoded, normal) ->
    apply_display_rules(decode_encoded_words(parse_input(Encoded, normal)));
decode2iolist(Encoded, structured_field) ->
    apply_display_rules(decode_encoded_words(parse_input(Encoded, comments))).

%%----------------------------------------------------------------------
%% Private implementation v2.0
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Function: apply_display_rules/1
%% Purpose:  apply_display_rules/1 should alter decoded token list to
%%           reflect display rules defined in RFC2047 - removing
%%           linear-whitespaces between adjacent encoded words. Also,
%%           this function strips input tree out of token type tags, so
%%           that it may be simply flattened in order to get a final
%%           string.
%% Args:     Tree - a syntax tree from parse_input/2.
%% Returns:  A list of UCS-encoded lists to be flattened or treated as
%%           an iolist.
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function: parse_input/2
%% Purpose:  Parses input binary string into a syntax tree.
%% Args:     BinaryString - a binary ASCII-7 string.
%%           Mode - eiter 'normal' or 'comments' - depending on whether
%%           we should treat the text as normal or structured-field
%%           (the second one allows using comments in parentheses).
%% Returns:  Syntax tree.
%%----------------------------------------------------------------------
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

% Choose between linear-whitespace and text
parse_input(Binary, CommentMode, InComment, TokenAccu) ->
     case is_linear_whitespace_next(Binary) of
	true ->
	    {Token, RestOfBinary} = try_scan_linear_whitespace(Binary);
	_ ->
	    {Token, RestOfBinary} = try_scan_text(Binary, CommentMode, InComment)
    end,
    parse_input(RestOfBinary, CommentMode, InComment, [Token | TokenAccu]).

%%----------------------------------------------------------------------
%% Function: tree_decode_elements/1
%% Purpose:  Walks through the tree generated by parse_input and decodes
%%           every encoded-word it meets, replacing {encoded_word, Encoded}
%%           with {decoded_word, Decoded}.
%% Args:     BinaryString - a binary ASCII-7 string.
%% Returns:  {linear_whitespace, LinearWhitespace}
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function: try_scan_linear_whitespace/1
%% Purpose:  Attempts to scan a linear-whitespace from passed binary.
%%           linear-whitespace may consist of one or more SPACE, TAB or
%%           CRLF characters.
%% Args:     BinaryString - a binary ASCII-7 string,
%% Returns:  {linear_whitespace, LinearWhitespace}
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function: try_scan_text/3
%% Purpose:  Attempts to scan a piece of plain-text (a word) from passed binary.
%% Args:     BinaryString - a binary ASCII-7 string,
%%           CommentMode - see parse_input/2
%%           InComment - are we inside a comment?
%% Returns:  {text, Text}
%%----------------------------------------------------------------------
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

%%----------------------------------------------------------------------
%% Function: try_scan_encoded_word/3
%% Purpose:  Attempts to scan an encoded-word from passed binary.
%% Args:     BinaryString - a binary ASCII-7 string,
%%           CommentMode - see parse_input/2
%%           InComment - are we inside a comment?
%% Returns:  {{encoded_word, {Charset, Encoding, Encoded Text}}, RestOfBinary}
%%           | atom not_found.
%%----------------------------------------------------------------------
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
    

%%----------------------------------------------------------------------
%% Function: grab_linear_whitespace/1
%% Purpose:  Checks if next char (or group of characters) in passed binary
%%           form a linear-whitespace. To be used along with
%%           grab_linear_whitespace/1.
%% Args:     BinaryString - a binary ASCII-7 string.
%% Returns:  true or false, depending on whether the next char is a
%%           linear-whitespace.
%%----------------------------------------------------------------------
is_linear_whitespace_next(<<16#20, _/binary>>) ->
    true;
is_linear_whitespace_next(<<$\t, _/binary>>) ->
    true;
is_linear_whitespace_next(<<13, 10, _/binary>>) ->
    true;
is_linear_whitespace_next(_) ->
    false.

%%----------------------------------------------------------------------
%% Function: grab_linear_whitespace/1
%% Purpose:  Extract and return a single linear-whitespace from a binary.
%% Args:     BinaryString - a binary ASCII-7 string.
%% Returns:  {Whitespace, Rest of binary after extracting whitespace}.
%%           Whitespace may be list, if it contains more than 1 character
%%           (like CLRF defined in RFC822).
%%----------------------------------------------------------------------
grab_linear_whitespace(<<16#20, Rest/binary>>) ->
    {16#20, Rest};
grab_linear_whitespace(<<$\t, Rest/binary>>) ->
    {$\t, Rest};
grab_linear_whitespace(<<13, 10, Rest/binary>>) ->
    {[13, 10], Rest}.

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

