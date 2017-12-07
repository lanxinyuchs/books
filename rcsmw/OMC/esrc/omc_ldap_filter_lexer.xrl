%% This i an input file for the Lexical analyzer generator: leex
%% leex:file("ldap_filter_lexer.xrl") creates the file: "ldap_filter_lexer.erl"
%% to be compiled into the module: ldap_filter_lexer.beam
%% This is used to parse an ldap filter according to RFC4515 (which contains
%% an ABNF grammar).
%% Multibyte characters ans filters of the extended type are NOT supported.
%%
%RFC4512
%attributedescription = descr / numericoid
%	descr = keystring
%	numericoid = number 1*( DOT number )

%	keystring = leadkeychar *keychar
%	leadkeychar = ALPHA
%	keychar = ALPHA / DIGIT / HYPHEN
%	number  = DIGIT / ( LDIGIT 1*DIGIT )
%	ALPHA   = %x41-5A / %x61-7A   ; "A"-"Z" / "a"-"z"
%	DIGIT   = %x30 / LDIGIT       ; "0"-"9"
%	LDIGIT  = %x31-39             ; "1"-"9"

%RFC4515
%	assertionvalue = valueencoding
%	; The <valueencoding> rule is used to encode an <AssertionValue>
%	; from Section 4.1.6 of [RFC4511].
%	valueencoding  = 0*(normal / escaped)
%	normal         = UTF1SUBSET / UTFMB
%	escaped        = ESC HEX HEX
%	UTF1SUBSET     = %x01-27 / %x2B-5B / %x5D-7F
%                          ; UTF1SUBSET excludes 0x00 (NUL), LPAREN,
%                          ; RPAREN, ASTERISK, and ESC.

Definitions.

ALPHA = [A-Za-z]
HEXCHAR = [0-9a-fA-F]
DIGIT = [0-9]
LDIGIT = [1-9]
%UTF1SUBSET = [\x01-\x27\x2b-\x5b\x5d-\x7f]
%UTFMB = %not implemented%

% almost like UTF1SUBSET but the without &*!|=><~\n, these are instead included
% in the definitions done for yecc (not \n). Also see 'single' below
MYSUBSET = [\x01-\x09\x0b-\x20\x22-\x25\x27\x2b-\x3b\x3f-\x5b\x5d-\x7b\x7d\x7f]
%as above but includes also the asterisk ('*': 0x2a), used to directly
%find assertionvalues that builds a ubstring filter
MYSUBAST = [\x01-\x09\x0b-\x20\x22-\x25\x27\x2a-\x3b\x3f-\x5b\x5d-\x7b\x7d\x7f]

Rules.

\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
\& : {token, {'&', TokenLine}}.
\| : {token, {'|', TokenLine}}.
\! : {token, {'!', TokenLine}}.

=  : {token, {equal, TokenLine}}.
>= : {token, {greater, TokenLine}}.
<= : {token, {lesser, TokenLine}}.
~= : {token, {approx, TokenLine}}.

\* : {token, {asterisk, TokenLine}}.

[><~] : {token, {single, TokenLine, TokenChars}}. %excluded from MYSUBSET

{DIGIT} : {token, {oid, TokenLine, TokenChars}}.
{LDIGIT}{DIGIT}* : {token, {oid, TokenLine, TokenChars}}.
{LDIGIT}{DIGIT}*(\.{LDIGIT}{DIGIT}*)* : {token, {oid, TokenLine, TokenChars}}.

{ALPHA}({ALPHA}|{DIGIT}|-)* : {token, {keystring, TokenLine, TokenChars}}.
{MYSUBSET}* : {token, {keyvalue, TokenLine, TokenChars}}.
{MYSUBAST}* : {token, {keyvalueasterisk, TokenLine, TokenChars}}.

\\{HEXCHAR}{HEXCHAR} :
	{token, {hex, TokenLine, list_to_integer(tl(TokenChars), 16)}}.
\\ : {error, "valid sequency would be: ESC HEX HEX"}. %lexer loops unless...

\\"\\" : {token, {keyvalue, TokenLine, get_username()}}.

\n : {token, {newline, TokenLine}}.

#.*\n : skip_token.


Erlang code.

get_username() ->
    get(flexible_filter_uid).  %fusk
    %%omc_ldap_server:get_user(self()).
