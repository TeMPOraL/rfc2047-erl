%%% Unit tests for public interface of rfc1342.erl
%%% @author Jacek Zlydach <jacek.zlydach@erlang-solutions.com>
%%% @hidden
%%%

-module(rfc1342_tests).
-import(rfc1342, [split_string_to_conversion_segments/1,
		  decode_qstring/1,
		  hex_digit_2_dec/1,
		  binary_to_4byte_list/1]).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------
%% Unit tests
%% HIC SUNT DRACONES
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Unit tests :: Public interface
%%----------------------------------------------------------------------
basic_test_() ->
    [{"Identity transformation - nothing should happen",
      ?_assert(rfc1342:decode(<<"You live and learn. At any rate, you live.">>) == "You live and learn. At any rate, you live.")},
     {"Q decoding.",
      ?_assert(rfc1342:decode(<<"It is a mistake to think =?ISO-8859-1?Q?you_can_solve_any_major_problems?= just with potatoes.">>) == "It is a mistake to think you can solve any major problems just with potatoes.")},
     {"B (BASE64) decoding.",
      ?_assert(rfc1342:decode(<<"=?ISO-8859-1?B?Tm90aGluZyBzaG9ja3MgbWUuIEknbSBhIHNjaWVudGlzdC4=?=">>) == "Nothing shocks me. I'm a scientist.")}].

invalid_character_test_() ->
    [{"RFC2047 forbids SPACE chars in encoded words",
      ?_assert(rfc1342:decode(<<"Humans are not proud =?ISO-8859-1?Q?of their ancestors, and rarely invite them round?= to dinner.">>) == "Humans are not proud =?ISO-8859-1?Q?of their ancestors, and rarely invite them round?= to dinner.")},
     {"In Q-encoding, = are allowed only when they are used to form a hexadecimal "
      "representation of a character.",
     ?_assert(rfc1342:decode(<<"=?ISO-8859-1?Q?Who w=atches the watchers=3F?=">>) == "=?ISO-8859-1?Q?Who w=atches the watchers=3F?=")}].

failed_b_conversion_test_() ->
    [{"Improperly B-encoded word should be ignored and displayed as-is.",
      ?_assert(rfc1342:decode(<<"=?ISO-8859-1?B?123?=">>) == "=?ISO-8859-1?B?123?=")}].

unknowns_test_() ->
    [{"In case of unknown encoding type, we're allowed to ignore the entire encoded word.",
      ?_assert(rfc1342:decode(<<"=?UTF-8?H?TheSalmonOfDoubt?=">>) == "=?UTF-8?H?TheSalmonOfDoubt?=")},
     {"In case of unknown charset type, we're allowed to ignore the entire encoded word.",
      ?_assert(rfc1342:decode(<<"=?UTF-9.5?Q?The_Salmon_of_Doubt?=">>) == "=?UTF-9.5?Q?The_Salmon_of_Doubt?=")}].

character_conversion_in_q_encoding_test_() ->
    [{"=XX sequence should be converted to a character with 0xXX ASCII code, if "
      "XX form a valid hexadecimal number.",
      ?_assert(rfc1342:decode(<<"=?ISO-8859-1?Q?=48=41=43=4b=20=54=48=45=20=50=4C=41=4e=45=54?=">>) == "HACK THE PLANET")}].

%% This one will tell all 8-bit values :)
hex_chars_in_q_encoding_test() ->
    A = [$1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $A, $B, $C, $D, $E, $F],
    V = [ {[$=, X, Y], rfc1342:hex_chars_2_dec(X,Y)} || X <- A, Y <- A],
    lists:map(fun ({Text, Value}) ->
		      [Value] = rfc1342:decode(list_to_binary("=?iso-8859-1?q?" ++ Text ++ "?=")) end,
	      V).

encoded_word_separation_test_() ->
    [{"\"an 'encoded-word' (...) MUST be separated from any adjacent 'encoded-word' or 'text' by 'linear-white-space'\" - RFC2047, page 7",
      [?_assert(rfc1342:decode(<<"The ships =?ISO-8859-1?Q?hung_in_the_sky_?=in much the same way that bricks don't.">>) == "The ships =?ISO-8859-1?Q?hung_in_the_sky_?=in much the same way that bricks don't."),
       ?_assert(rfc1342:decode(<<"The ships=?ISO-8859-1?Q?_hung_in_the_sky?= in much the same way that bricks don't.">>) == "The ships=?ISO-8859-1?Q?_hung_in_the_sky?= in much the same way that bricks don't."),
       ?_assert(rfc1342:decode(<<"The ships\t=?ISO-8859-1?Q?hung_in_the_sky?= in much the same way that bricks don't.">>) == "The ships\thung in the sky in much the same way that bricks don't."),
       ?_assert(rfc1342:decode(<<"The ships =?ISO-8859-1?Q?hung_in_the_sky?==?ISO-8859-2?Q?_in_much_the_same_way?= that bricks don't.">>) == "The ships =?ISO-8859-1?Q?hung_in_the_sky?==?ISO-8859-2?Q?_in_much_the_same_way?= that bricks don't.")]}].



%%%% FIX THOSE TESTS!; they're mostly fixed now; however, probably incomplete.
space_and_newline_handling_test_() ->
    [{"A single space between encoded words should be discarded.",
      ?_assert(rfc1342:decode(<<"Nothing =?ISO-8859-1?Q?is_as_far_aw?= =?ISO-8859-1?Q?ay_as_one_minute?= ago.">>) == "Nothing is as far away as one minute ago.")},
     {"Multiple spaces and horizontal tabs between encoded words should be discarded.",
      ?_assert(rfc1342:decode(<<"Nothing =?ISO-8859-1?Q?is_as_far_aw?=   \t    =?ISO-8859-1?Q?ay_as_one_minute?= ago.">>) == "Nothing is as far away as one minute ago.")},
     {"A single newline between encoded words should be discarded.",
      ?_assert(rfc1342:decode(<<"Nothing =?ISO-8859-1?Q?is_as_far_aw?=\r\n=?ISO-8859-1?Q?ay_as_one_minute?= ago.">>) == "Nothing is as far away as one minute ago.")},
     {"Multiple newlines after an encoded word should be discarded.",
      ?_assert(rfc1342:decode(<<"Nothing =?ISO-8859-1?Q?is_as_far_aw?=\r\n\r\n\r\n\r\n=?ISO-8859-1?Q?ay_as_one_minute?= ago.">>) == "Nothing is as far away as one minute ago.")},
     {"A single space between encoded word and normal word should NOT be discarded.",
      ?_assert(rfc1342:decode(<<"Nothing =?ISO-8859-1?Q?is_as_far_away?= as one minute ago.">>) == "Nothing is as far away as one minute ago.")},
% NOT VALID IN RFC2047, TO THE BEST OF MY KNOWLEDGE
%     {"A single space at the end of the encoded word should always be discarded",
%     ?_assert(rfc1342:decode(<<"Humanity is acquiring all the right technology for =?ISO-8859-1?Q?all_the_wrong_reasons.?= ">>) == "Humanity is acquiring all the right technology for all the wrong reasons.")},
%     {"A single newline at the end of the encoded word should be discarded unless"
%      "the word is the last token in the text.",
%      [?_assert(rfc1342:decode(<<"Humanity is acquiring all the right technology for =?ISO-8859-1?Q?all_the_wrong?=\r\n reasons.">>) == "Humanity is acquiring all the right technology for all the wrong reasons."),
%       ?_assert(rfc1342:decode(<<"Humanity is acquiring all the right technology for =?ISO-8859-1?Q?all the wrong reasons.?=\r\n">>) == "Humanity is acquiring all the right technology for all the wrong reasons.\r\n")]}].
     {"", ?_assert(true)}].

encoded_chars_test_() ->
    [{"In Q-encoded representation, _ is always converted to 0x20, regardless "
      "of used charset.",
      [?_assert(rfc1342:decode(<<"=?ISO-8859-1?Q?_?=">>) == rfc1342:decode(<<"=?ISO-8859-1?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?ISO-8859-2?Q?_?=">>) == rfc1342:decode(<<"=?ISO-8859-2?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?ISO-8859-8?Q?_?=">>) == rfc1342:decode(<<"=?ISO-8859-8?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?US-ASCII?Q?_?=">>) == rfc1342:decode(<<"=?US-ASCII?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?UTF-8?Q?_?=">>) == rfc1342:decode(<<"=?UTF-8?Q?=20?=">>))]},
     {"Well, those 0x20 should be seen Erlang-side too.",
      [?_assert([16#20] == rfc1342:decode(<<"=?ISO-8859-1?Q?_?=">>)),
       ?_assert([16#20] == rfc1342:decode(<<"=?ISO-8859-2?Q?_?=">>)),
       ?_assert([16#20] == rfc1342:decode(<<"=?ISO-8859-8?Q?_?=">>)),
       ?_assert([16#20] == rfc1342:decode(<<"=?US-ASCII?Q?_?=">>)),
       ?_assert([16#20] == rfc1342:decode(<<"=?UTF-8?Q?_?=">>))]}].

charset_case_sensitivity_test_() ->
    [{"Charset names should be treated as case-insensitive.",
      [?_assert(rfc1342:decode(<<"=?ISO-8859-2?B?VGhlIEd1aWRlIGlzIGRlZmluaXRpdmUu?=">>) == rfc1342:decode(<<"=?iso-8859-2?B?VGhlIEd1aWRlIGlzIGRlZmluaXRpdmUu?=">>)),
       ?_assert(rfc1342:decode(<<"=?iSo-8859-2?B?VGhlIEd1aWRlIGlzIGRlZmluaXRpdmUu?=">>) == rfc1342:decode(<<"=?IsO-8859-2?B?VGhlIEd1aWRlIGlzIGRlZmluaXRpdmUu?=">>)),
       ?_assert(rfc1342:decode(<<"=?ISO-8859-1?Q?Reality_is_frequently_inaccurate.?=">>) == rfc1342:decode(<<"=?iso-8859-1?Q?Reality_is_frequently_inaccurate.?=">>)),
       ?_assert(rfc1342:decode(<<"=?iSo-8859-1?Q?Reality_is_frequently_inaccurate.?=">>) == rfc1342:decode(<<"=?IsO-8859-1?Q?Reality_is_frequently_inaccurate.?=">>)),
       ?_assert(rfc1342:decode(<<"=?utf-8?Q?Reality_is_frequently_inaccurate.?=">>) == rfc1342:decode(<<"=?UTF-8?Q?Reality_is_frequently_inaccurate.?=">>)),
       ?_assert(rfc1342:decode(<<"=?UtF-8?Q?Reality_is_frequently_inaccurate.?=">>) == rfc1342:decode(<<"=?uTf-8?Q?Reality_is_frequently_inaccurate.?=">>))
       %% TODO add some tests for crazy charsets if we find some
      ]}].

encoding_case_sensitivity_test_() ->
    [{"Encoding names should be treated as case-insensitive.",
      [?_assert(rfc1342:decode(<<"=?ISO-8859-2?Q?They_obstinately_persisted_in_their_absence.?=">>) == rfc1342:decode(<<"=?ISO-8859-2?q?They_obstinately_persisted_in_their_absence.?=">>)),
       ?_assert(rfc1342:decode(<<"=?ISO-8859-2?B?QW55dGhpbmcgdGhhdCBoYXBwZW5zLCBoYXBwZW5zLg==?=">>) == rfc1342:decode(<<"=?ISO-8859-2?b?QW55dGhpbmcgdGhhdCBoYXBwZW5zLCBoYXBwZW5zLg==?=">>))]}].

total_case_sensitivity_test_() ->
    [{"Both encoding and charset should be treated as case-insensitive.",
      [?_assert(rfc1342:decode(<<"=?iSo-8859-1?Q?They_obstinately_persisted_in_their_absence.?=">>) == rfc1342:decode(<<"=?iSo-8859-1?q?They_obstinately_persisted_in_their_absence.?=">>)),
       ?_assert(rfc1342:decode(<<"=?utf-8?B?QW55dGhpbmcgdGhhdCBoYXBwZW5zLCBoYXBwZW5zLg==?=">>) == rfc1342:decode(<<"=?UTf-8?b?QW55dGhpbmcgdGhhdCBoYXBwZW5zLCBoYXBwZW5zLg==?=">>))]}].

base64_encoding_test_() ->
    [{"RFC2047 states, that B-encoding is identical to BASE64 encoding defined by RFC2045. Erlang's base64 module should conform to that (and further ones) RFC.",
      ?_assert(rfc1342:decode(list_to_binary("=?iso-8859-1?b?" ++ binary_to_list(base64:encode("Don't panic!")) ++ "?=")) == binary_to_list(base64:decode(base64:encode("Don't panic!"))))}].

%% Tests from page 12 and 13 of RFC2047
comment_mode_off_test_() ->
    [?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?=)">>) == "(=?ISO-8859-1?Q?a?=)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?= b)">>) == "(=?ISO-8859-1?Q?a?= b)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=)">>) == "(=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=)">>) == "(=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?=\r\n    =?ISO-8859-1?Q?b?=)">>) == "(=?ISO-8859-1?Q?a?=\r\n    =?ISO-8859-1?Q?b?=)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a_b?=)">>) == "(=?ISO-8859-1?Q?a_b?=)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?= =?ISO-8859-2?Q?_b?=)">>) == "(=?ISO-8859-1?Q?a?= =?ISO-8859-2?Q?_b?=)")].

%% TODO add comment mode to public interface
comment_mode_on_test_() ->
    [?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?=)">>, structured_field) == "(a)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?= b)">>, structured_field) == "(a b)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=)">>, structured_field) == "(ab)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=)">>, structured_field) == "(ab)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?=\r\n    =?ISO-8859-1?Q?b?=)">>, structured_field) == "(ab)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a_b?=)">>, structured_field) == "(a b)"),
     ?_assert(rfc1342:decode(<<"(=?ISO-8859-1?Q?a?= =?ISO-8859-2?Q?_b?=)">>, structured_field) == "(a b)")].

%%----------------------------------------------------------------------
%% Unit tests :: Private functions
%%----------------------------------------------------------------------
split_string_to_conversion_segments_test() ->
    ?assert(length(split_string_to_conversion_segments(<<"Now =?ISO-8859-1?Q?I_am_become_Death?=,? ? ? the destroyer of =?ISO-8859-1?Q?worlds?=.">>)) == 5),
    %% RFC1342 says that a valid encoded word may contain only four question marks (including delimiters).
    ?assert(length(split_string_to_conversion_segments(<<"=?CODING?= ??? ?=HORROR">>)) == 1),
    %% RFC1342 limits the length of an encoded word to 75 characters (including encoding anc charset).
    ?assert(length(split_string_to_conversion_segments(<<"=?ISO-8859-1?Q?Any_sufficiently_complicated_C_or_Fortran_program_contains_an_ad_hoc,_informally-specified,_bug-ridden,_slow_implementation_of_half_of_Common_Lisp.?=_-_Greenspun's_Tenth_Rule_of_Programming">>)) == 1),
    ?assert(length(split_string_to_conversion_segments(<<"=?ISO-8859-1?Q?-Time_is_an_illusion._Lunchtime_doubly_so._-_Douglas_Adams-?=">>)) == 1),
    ?assert(length(split_string_to_conversion_segments(<<"=?ISO-8859-1?Q?-Time_is_an_illusion._Lunchtime_doubly_so._-_Douglas_Adams?=">>)) == 3).

%% TODO write tests imposing space separation, no invalid characters inside encoded words, ect.

decode_qstring_test() ->
    "Now I am become Death, the destroyer of worlds." = 
	decode_qstring("Now=20I=20am_become_=44eath,_the_destroyer_of=20worlds.").
hex_chars_2_dec_test() ->
    ok. %% TODO
hex_digit_2_dec_test() ->
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] =
	lists:map(fun rfc1342:hex_digit_2_dec/1,
		  [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F]),
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] =
	lists:map(fun rfc1342:hex_digit_2_dec/1,
		  [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f]).
%% NOTE - are lowercase hex letters allowed?

binary_to_4byte_list_test() ->
    {error, 'Binary not aligned to 4 bytes'} = binary_to_4byte_list(<<1>>),
    {error, 'Binary not aligned to 4 bytes'} = binary_to_4byte_list(<<1,2,3,4,5>>).

%%% People quoted in unit tests:
%%% - Douglas Adams
%%% - J. Robert Oppenheimer
%%% - Jim Bishop
%%% - Harrison Ford (as Indiana Jones)
%%% - Philip Greenspun
