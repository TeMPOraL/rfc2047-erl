%%% Unit tests for public interface of rfc1342.erl
%%% @author Jacek Zlydach <jacek.zlydach@erlang-solutions.com>
%%% @hidden
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
    [?_assert(rfc1342:decode(<<"You live and learn. At any rate, you live.">>) == "You live and learn. At any rate, you live."),
     ?_assert(rfc1342:decode(<<"It is a mistake to think =?ISO-8859-1?Q?you_can_solve_any_major_problems?= just with potatoes.">>) == "It is a mistake to think you can solve any major problems just with potatoes.")].

invalid_character_test_() ->
    [{"RFC1342 forbids SPACE chars in encoded words",
      ?_assert(rfc1342:decode(<<"Humans are not proud =?ISO-8859-1?Q?of their ancestors, and rarely invite them round?= to dinner.">>) == "Humans are not proud =?ISO-8859-1?Q?of their ancestors, and rarely invite them round?= to dinner.")}].

encoded_word_concatenation_test_() ->
    [{"A single space after an encoded word should be discarded.",
      ?_assert(rfc1342:decode(<<"Nothing =?ISO-8859-1?Q?is_as_far_aw?= =?ISO-8859-1?Q?ay_as_one minute?=  ago.">>) == "Nothing is as far away as one minute ago.")},
     {"A single newline after an encoded word should be discarded.",
      ?_assert(rfc1342:decode(<<"Nothing =?ISO-8859-1?Q?is_as_far_aw?=\r\n=?ISO-8859-1?Q?ay_as_one_minute?=  ago.">>) == "Nothing is as far away as one minute ago.")}].

encoded_chars_test_() ->
    [{"In Quoted-Printable representation, _ is always converted to 0x20, regardless "
      "of used charset.",
      [?_assert(rfc1342:decode(<<"=?ISO-8859-1?Q?_?=">>) == rfc1342:decode(<<"=?ISO-8859-1?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?ISO-8859-2?Q?_?=">>) == rfc1342:decode(<<"=?ISO-8859-2?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?ISO-8859-8?Q?_?=">>) == rfc1342:decode(<<"=?ISO-8859-8?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?US-ASCII?Q?_?=">>) == rfc1342:decode(<<"=?US-ASCII?Q?=20?=">>)),
       ?_assert(rfc1342:decode(<<"=?UTF-8?Q?_?=">>) == rfc1342:decode(<<"=?UTF-8?Q?=20?=">>))]}].

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
	decode_qstring("Now=20I=20am_become_=44eath,_the destroyer of=20worlds.").
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
