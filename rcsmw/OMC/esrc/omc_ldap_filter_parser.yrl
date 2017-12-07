Header
"%% some header stuff here"
"%% no more"
"%attributedescription vs valueencoding"
"%RFC4512 vs RFC4511"
"%% Adapted from ABNF grammar found in RFC4515"
"%%".

Nonterminals
singlefilter filter filtercomp item filterlist and or not simple present attr
assertionvalue assertionvaluelist filtertype
substring .

Terminals '(' ')' '&' '|' '!' equal lesser greater approx asterisk keystring
keyvalue hex oid newline single keyvalueasterisk.

Rootsymbol singlefilter.

%ensure that (type=*) resolves to present instead of substring
%as it should according to RFC
Nonassoc 100 substring.
Nonassoc 200 present.

singlefilter -> filter newline : {ok, '$1'}.
singlefilter -> filter : {ok, '$1'}.
filter -> '(' filtercomp ')' : '$2'.
filter -> '(' '&' ')' :
    eldap:'and'([]). %supported! according to something found on the internet
filtercomp -> and :
    eldap:'and'('$1').
filtercomp -> or :
    eldap:'or'('$1').
filtercomp -> not :
    eldap:'not'('$1').
filtercomp -> item: '$1'.
and -> '&' filterlist: '$2'.
or -> '|' filterlist: '$2'.
not -> '!' filter: '$2'.
filterlist -> filter: ['$1'].
filterlist -> filter filterlist : ['$1' | '$2'].
item -> simple : '$1'.
item -> present : '$1'.
item -> substring : '$1'.
% item -> extensible : '$1'.          %HERE FIXME (not supported by OTP:eldap)

simple -> attr filtertype assertionvaluelist :
    eldap:'$2'('$1','$3').
present -> attr equal asterisk :
    eldap:present('$1').
substring -> attr equal keyvalueasterisk :
    mk_ss_filter('$1', '$3').

filtertype -> equal : 'equalityMatch'.
filtertype -> lesser : 'lessOrEqual'.
filtertype -> greater : 'greaterOrEqual'.
filtertype -> approx : 'approxMatch'.

attr -> keystring : value_of('$1').
attr -> oid : value_of('$1').

%the assertionvalue has wider definition than attr, thus a lot of stuff
%parsing to other tokens may be included
assertionvalue -> keystring : value_of('$1').
assertionvalue -> keyvalue : value_of('$1').
assertionvalue -> hex : value_of('$1').
assertionvalue -> oid : value_of('$1').
assertionvalue -> equal : "=".
assertionvalue -> lesser : "<=".
assertionvalue -> greater : ">=".
assertionvalue -> approx : "~=".
assertionvalue -> '&' : "&".
assertionvalue -> '|' : "|".
assertionvalue -> '!' : "!".
assertionvalue -> single : value_of('$1').
assertionvaluelist -> assertionvalue : '$1'.
assertionvaluelist -> assertionvalue assertionvaluelist : concat('$1', '$2').

Erlang code.

value_of({_, _, V}) -> V.

concat(H ,T) when is_list(H), is_list(T) ->
    H ++ T;
concat([H] ,T) when is_list(T) ->
    [H | T];
concat(H ,T) ->
    [H | T].

% word* -> {initial,word}
% *word -> {final,word}
% *word* -> {any,word}
% word*more -> {initial, word},{final,more}
% *word*more -> {any, word},{final,more}
% first*word*more -> {initial,first},{any,word},{final,more}
% first*word*wordN*more -> {initial,first},{any,word},{any, wordN},{final,more}

mk_ss_filter(Attribute, {keyvalueasterisk, _, List_with_asterisk}) ->
    Parsed = divide_with_asterisk(List_with_asterisk),
    Filter =build_filter(Parsed),
    eldap:substrings(Attribute, Filter).

%ensuring that the List is nonempty prevents accepting a filter containing
%multiple consecutive asterisks, e.g. "word**"
build_filter(['*', [_|_] = List]) ->
    [{final, List}];
build_filter([[_|_] = List, '*']) ->
    [{initial, List}];
build_filter([[_|_] = List, '*' | Rest]) ->
    [{initial, List} | build_filter(['*' | Rest])];
build_filter(['*', [_|_] = List, '*' | Rest]) ->
    [{any, List} |  build_filter(['*' | Rest])];
build_filter(['*']) ->
    [].

divide_with_asterisk([]) -> [];
divide_with_asterisk([$*]) ->
	['*'];
divide_with_asterisk([$* | T]) ->
	{Rest, Word} = not_asterisk(T, []),
	['*', Word | divide_with_asterisk(Rest)];
divide_with_asterisk(List) ->
	{Rest, Word} = not_asterisk(List, []),
	[Word | divide_with_asterisk(Rest)].

not_asterisk([$* | _] = Rest, Acc) -> {Rest, lists:reverse(Acc)};
not_asterisk([H | T], Acc) ->  not_asterisk(T, [H | Acc]);
not_asterisk([], Acc) ->  {[], lists:reverse(Acc)}.

