%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaNtpUtil.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R4A/R5A/R7A/R10A/1

%%% @doc ==Module title==
%%% Utility functions called from comsaNtpServer
%%% @end

-module(comsaNtpUtil).

-vsn('/main/R2A/R4A/R5A/R7A/R10A/1').
-date('2017-07-03').
-author('enekdav').

%%-compile([export_all]).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
%%% 
%%% The information in this document is the property of Ericsson.
%%% 
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev      Date        Name        What
%%% -----    ---------   --------    ------------------------
%%% Rx       2014-02-05  etxlg       Created
%%% R2A/2    2014-02-16  etxlg       Created
%%% R5A/1    2015-12-18  etxlg       IPv6
%%% R7A/1    2016-09-23  etxpejn     Added functions with Fd, IoDevice 
%%% R10A/1  2017-07-03   enekdav     COMSA trap notifications design helper functions
%%% ----------------------------------------------------------

%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([reassemble/2]).
-export([mk_query_packet/2]).
-export([decode_variables/1]).
-export([string_to_ip/1]).
-export([mk_ds/0]).
-export([us_to_print_string/1]).
-export([decode_and_print_trap_packets/2, decode_and_print_trap_packets/3]).
-export([decode_packet/3, decode_packet/4, get_code/1]).
-export([decode_and_print_variables/2, decode_and_print_variables/3]).
-export([is_clock_step/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc 
%%%
%%%
%%% ===Arguments===
%%% 

mk_ds() ->
    mk_ds(os:timestamp()).

%take a number (large) of microseconds and return a printable approximation
us_to_print_string(Us) ->
    us_to_ps(Us).

decode_and_print_trap_packets(_, {0, _}) ->
    {done, "no packets"};
decode_and_print_trap_packets(Lvl, {Num, Packets}) ->
    io:format("There are ~b packets to decode~n", [Num]),
    do_decode_and_print_trap_packets(Lvl, undefined, queue:to_list(Packets)).

decode_and_print_trap_packets(_, {0, _}, Fd) ->
    io:format(Fd, "There are no packets to decode~n", []);
decode_and_print_trap_packets(Lvl, {Num, Packets}, Fd) ->
    io:format(Fd, "There are ~b packets to decode~n", [Num]),
    do_decode_and_print_trap_packets(Lvl, undefined, queue:to_list(Packets), Fd).

is_clock_step({_, 7, 0, B,  _}) ->
    is_clock_step(B);
is_clock_step(B) when is_integer(B) ->
    is_clock_step(<<B:16>>);
is_clock_step(<<_:12, 12:4>>) ->
    true;
is_clock_step(_) ->
    false.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%Mbit x, Offset 0, reassembly ongoing -> First packet during reassembly
%an error - restart with new reassembly starting from this packet
reassemble(Reass,
	<<_:10, _:1, _:5, _Seq:16, _Status:16, _Ass_id:16, 0:16,
		_Count:16, _/binary>> = Data) when Reass =/= undefined ->
    debug("ERROR: restarting reassembly"),
    reassemble(undefined, Data);
%Mbit 0, Offset 0, no ongoing reassembly -> Single packet
reassemble(undefined,
	<<_:10, 0:1, Op_code:5, Seq:16, Status:16,
	Ass_id:16, 0:16, Count:16, Data/binary>>)->
    {done, {Seq, Op_code, Ass_id, Status, binary:part(Data, 0, Count)}};
%Mbit 1, Offset 0, NO ongoing reassembly -> First of multiple packets
reassemble(undefined,
	<<_:10, 1:1, _:5, Seq:16, _Status:16,
	_Ass_id:16, 0:16, Count:16, Data/binary>>)->
    {more, {Seq, Count, binary:part(Data, 0, Count)}};
%Mbit x, Offset =/= 0, NO ongoing reassembly -> middle or last packet -> skip
%happes when the queue starts to flowover
reassemble(undefined,
	<<Li:2, V:3, Mode:3, R:1, E:1, M:1, Op:5, Seq:16, Status:16, Ass_id:16,
	Offset:16, Count:16, Rest/binary>> = Data) when Offset =/= 0 ->
    debug("reassemble ERROR: skipping packet"),
    debug("Reassembly undefined"),
    debug("Total size of packet: ~b", [size(Data)]),
    debug("Leap ind: ~p", [Li]),
    debug("Version: ~p", [V]),
    debug("Mode: ~p", [Mode]),
    debug("Respond bit: ~p", [R]),
    debug("Error bit: ~p", [E]),
    debug("More bit: ~p", [M]),
    debug("Op code: ~p", [Op]),
    debug("Seq: ~p", [Seq]),
    debug("Status: ~p", [Status]),
    debug("Association id: ~p", [Ass_id]),
    debug("Offset: ~p", [Offset]),
    debug("Count: ~p", [Count]),
    debug("Rest: ~p", [Rest]),
    debug("size(Rest): ~p", [size(Rest)]),
    error;
%Mbit 1, ongoing reassembly, and matching Seq/Offset -> More and more packets
reassemble({Seq, Offset, Sofar},
	<<_:10, 1:1, _:5, Seq:16, _Status:16,
	_Ass_id:16, Offset:16, Count:16, More/binary>>)->
    {more, {Seq, Offset + Count,
		<<Sofar/binary, (binary:part(More, 0, Count))/binary>>}};
%More bit 0, ongoing reassembly, and matching Seq -> Last packet
reassemble({Seq, Offset, Sofar},
	<<_:10, 0:1, Op_code:5, Seq:16, Status:16,
	Ass_id:16, Offset:16, Count:16, Last/binary>>)->
    {done, {Seq, Op_code, Ass_id, Status,
		<<Sofar/binary, (binary:part(Last, 0, Count))/binary>>}};
reassemble(undefined, 
	<<Li:2, V:3, Mode:3, R:1, E:1, M:1, Op:5, Seq:16, Status:16,
	Ass_id:16, Offset:16, Count:16, Rest/binary>> = Data) ->
    debug("Reassembly undefined"),
    debug("Total size of packet: ~b", [size(Data)]),
    debug("Leap ind: ~p", [Li]),
    debug("Version: ~p", [V]),
    debug("Mode: ~p", [Mode]),
    debug("Respond bit: ~p", [R]),
    debug("Error bit: ~p", [E]),
    debug("More bit: ~p", [M]),
    debug("Op code: ~p", [Op]),
    debug("Seq: ~p", [Seq]),
    debug("Status: ~p", [Status]),
    debug("Association id: ~p", [Ass_id]),
    debug("Offset: ~p", [Offset]),
    debug("Count: ~p", [Count]),
    debug("Rest: ~p", [Rest]),
    debug("size(Rest): ~p", [size(Rest)]),
    error;
reassemble(undefined, _) ->
    debug("Reassembly undefined NO MATCH"),
    error;
reassemble(_Reassembly, _) ->
    debug("Reassembly NO MATCH - reset assembly acc"),
    error.

decode_packet(N, _, _) when N < 2 -> ok;
decode_packet(_, Ts, {Seq, 7, 0, Status, _Data}) ->
    io:format("~p~nNTP TRAP#:~2b ~s System, Status word: ~b~n",
		[?MODULE, Seq, mk_ds(Ts), Status]),
    decode_system_status_word(<<Status:16>>);
decode_packet(_, Ts, {Seq, 7, Ass_id, Status, _Data}) ->
    io:format("~p~nNTP TRAP#:~2b ~s Association#: ~b, Status word: ~b~n",
		[?MODULE, Seq, mk_ds(Ts), Ass_id, Status]),
    decode_peer_status_word(<<Ass_id:16, Status:16>>);
decode_packet(_, Ts, {Seq, 1, 0, Status, Data}) ->
    io:format("~p~nNTP RESPONSE#:~2b ~s System, Status word: ~b~n",
		[?MODULE, Seq, mk_ds(Ts), Status]),
    decode_system_status_word(<<Status:16>>),
    [ decode_peer(<<P:32>>) || <<P:32>> <= Data ];
decode_packet(_, Ts, {Seq, 1, Ass_id, Status, _Data}) ->
    io:format("~p~nNTP RESPONSE#:~2b ~s Association#: ~b, Status word: ~b~n",
		[?MODULE, Seq, mk_ds(Ts), Ass_id, Status]),
    decode_peer_status_word(<<Ass_id:16, Status:16>>).

decode_packet(N, _, _,_) when N < 2 -> ok;
decode_packet(_, Ts, {Seq, 7, 0, Status, _Data}, Fd) ->
    io:format(Fd, "~p~nNTP TRAP#:~2b ~s System, Status word: ~b~n",
	      [?MODULE, Seq, mk_ds(Ts), Status]),
    decode_system_status_word(<<Status:16>>, Fd);
decode_packet(_, Ts, {Seq, 7, Ass_id, Status, _Data}, Fd) ->
    io:format(Fd, "~p~nNTP TRAP#:~2b ~s Association#: ~b, Status word: ~b~n",
	      [?MODULE, Seq, mk_ds(Ts), Ass_id, Status]),
    decode_peer_status_word(<<Ass_id:16, Status:16>>, Fd);
decode_packet(_, Ts, {Seq, 1, 0, Status, Data}, Fd) ->
    io:format(Fd, "~p~nNTP RESPONSE#:~2b ~s System, Status word: ~b~n",
	      [?MODULE, Seq, mk_ds(Ts), Status]),
    decode_system_status_word(<<Status:16>>, Fd),
    [ decode_peer(<<P:32>>, Fd) || <<P:32>> <= Data ];
decode_packet(_, Ts, {Seq, 1, Ass_id, Status, _Data}, Fd) ->
    io:format(Fd, "~p~nNTP RESPONSE#:~2b ~s Association#: ~b, Status word: ~b~n",
	      [?MODULE, Seq, mk_ds(Ts), Ass_id, Status]),
    decode_peer_status_word(<<Ass_id:16, Status:16>>, Fd).

do_decode_and_print_trap_packets(_, undefined, []) ->
    {done, ok};
do_decode_and_print_trap_packets(_, _, []) ->
    {done, "reassambly ongoing"};
do_decode_and_print_trap_packets(Verbosity, Reass,  [{Ts, P} | Packets]) ->
    case reassemble(Reass, P) of
	{more, New_reass} ->
	    do_decode_and_print_trap_packets(Verbosity, New_reass,  Packets);
	{done, Trap} ->
            decode_packet(Verbosity, Ts, Trap),
            decode_and_print_variables(Verbosity, Trap),
	    do_decode_and_print_trap_packets(Verbosity, undefined,  Packets);
	error ->
	    io:format("Trap packet(~s) reassembly failed~n", [mk_ds(Ts)]),
	    do_decode_and_print_trap_packets(Verbosity, undefined,  Packets)
    end.
do_decode_and_print_trap_packets(_, undefined, [], _Fd) ->
    {done, ok};
do_decode_and_print_trap_packets(_, _, [], _Fd) ->
    {done, "reassambly ongoing"};
do_decode_and_print_trap_packets(Verbosity, Reass,  [{Ts, P} | Packets], Fd) ->
    case reassemble(Reass, P) of
	{more, New_reass} ->
	    do_decode_and_print_trap_packets(Verbosity, New_reass, Packets, Fd);
	{done, Trap} ->
            decode_packet(Verbosity, Ts, Trap, Fd),
            decode_and_print_variables(Verbosity, Trap, Fd),
	    do_decode_and_print_trap_packets(Verbosity, undefined,  Packets, Fd);
	error ->
	    io:format(Fd, "Trap packet(~s) reassembly failed~n", [mk_ds(Ts)]),
	    do_decode_and_print_trap_packets(Verbosity, undefined,  Packets, Fd)
    end.

decode_system_status_word(<<Li:2, Cs:6, Ec:4, Le:4>>) ->
    io:format(
        "    Leap indicator: (~b) ~s~n"
        "    Clock source: (~b) ~s~n"
        "    System event counter: ~b~n"
        "    Last system event: (~b) ~s~n",
        [Li, decode_li(Li), Cs, decode_cs(Cs), Ec, Le, dec_system_event(Le)]).
decode_system_status_word(<<Li:2, Cs:6, Ec:4, Le:4>>, Fd) ->
    io:format(Fd,
        "    Leap indicator: (~b) ~s~n"
        "    Clock source: (~b) ~s~n"
        "    System event counter: ~b~n"
        "    Last system event: (~b) ~s~n",
        [Li, decode_li(Li), Cs, decode_cs(Cs), Ec, Le, dec_system_event(Le)]).

decode_peer_status_word(<<_:16, Peer_stat:5, Peer_sel:3, Pec:4, Ple:4>>) ->
    io:format(
        "    Status(~b): ~s~n"
        "    Selection(~b): ~s~n"
        "    Event counter: ~b~n"
        "    Event(~b): ~s~n",
        [Peer_stat, decode_ps(<<Peer_stat:5>>), Peer_sel,
                decode_psel(Peer_sel), Pec, Ple, dec_peer_event(Ple)]).
decode_peer_status_word(<<_:16, Peer_stat:5, Peer_sel:3, Pec:4, Ple:4>>, Fd) ->
    io:format(Fd,
        "    Status(~b): ~s~n"
        "    Selection(~b): ~s~n"
        "    Event counter: ~b~n"
        "    Event(~b): ~s~n",
        [Peer_stat, decode_ps(<<Peer_stat:5>>), Peer_sel,
                decode_psel(Peer_sel), Pec, Ple, dec_peer_event(Ple)]).
decode_peer(<<Association:16, Peer_status:5, Peer_sel:3, Pec:4, Ple:4>>) ->
    io:format(
        "Peer association: ~b~n"
        "    Status: (~b) ~s~n"
        "    Selection: (~b) ~s~n"
        "    Event counter: ~b~n"
        "    Event: (~b) ~s~n",
        [Association, Peer_status, decode_ps(<<Peer_status:5>>), Peer_sel,
                decode_psel(Peer_sel), Pec, Ple, dec_peer_event(Ple)]);
decode_peer(What) ->
    io:format("decode error, decode_peer: size: ~b data: ~p~n", [size(What), What]).
decode_peer(<<Association:16, Peer_status:5, Peer_sel:3, Pec:4, Ple:4>>, Fd) ->
    io:format(Fd,
        "Peer association: ~b~n"
        "    Status: (~b) ~s~n"
        "    Selection: (~b) ~s~n"
        "    Event counter: ~b~n"
        "    Event: (~b) ~s~n",
        [Association, Peer_status, decode_ps(<<Peer_status:5>>), Peer_sel,
                decode_psel(Peer_sel), Pec, Ple, dec_peer_event(Ple)]);
decode_peer(What, Fd) ->
    io:format(Fd, "decode error, decode_peer: size: ~b data: ~p~n", [size(What), What]).

decode_li(0) -> "normal synced";
decode_li(1) -> "leap, add sec";
decode_li(2) -> "leap, del sec";
decode_li(3) -> "never synced".

decode_cs(0) -> "not yet synced";
decode_cs(1) -> "PPS";
decode_cs(2) -> "VLF/LF radio";
decode_cs(3) -> "MF/HF radio";
decode_cs(4) -> "VHF/UHF radio";
decode_cs(5) -> "local timecode";
decode_cs(6) -> "NTP";
decode_cs(7) -> "other";
decode_cs(8) -> "eyeball and wristwatch";
decode_cs(9) -> "telephone modem";
decode_cs(_) -> "ERROR: reserved".

dec_system_event(0) -> "unspecified";
dec_system_event(1) -> "frequency file not available";
dec_system_event(2) -> "frequency set from frequency file";
dec_system_event(3) -> "spike detected";
dec_system_event(4) -> "initial frequencey training mode";
dec_system_event(5) -> "clock synchronized";
dec_system_event(6) -> "program restart";
dec_system_event(7) -> "clock error more than 600s";
dec_system_event(8) -> "no system peer";
dec_system_event(9) -> "leap second armed from file or autokey";
dec_system_event(10) -> "leap second disarmed";
dec_system_event(11) -> "leap event";
dec_system_event(12) -> "clock stepped";
dec_system_event(13) -> "kernel information message";
dec_system_event(14) -> "leapsecond values update from file";
dec_system_event(15) -> "new NIST leapseconds file needed".

decode_ps(<<Con:1, Ae:1, Aokay:1, Reachable:1, Bcast:1>>) ->
    io_lib:format(
        "Persistent: ~b, Auth_ok: ~b, Auth_enable: ~b, Reach: ~b "
        "Bcast: ~b", [Con, Ae, Aokay, Reachable, Bcast]).

decode_psel(0) -> "  discarded as not valid";
decode_psel(1) -> "x discarded by intersection algorithm";
decode_psel(2) -> ". discarded by table overflow";
decode_psel(3) -> "- discarded by cluster algorithm";
decode_psel(4) -> "+ included by combine algorithm";
decode_psel(5) -> "# backup";
decode_psel(6) -> "* system peer";
decode_psel(7) -> "o PPS peer".

dec_peer_event(0) -> "ERROR: not valid";
dec_peer_event(1) -> "association mobilized";
dec_peer_event(2) -> "association demobilized";
dec_peer_event(3) -> "server unreachable";
dec_peer_event(4) -> "server reachable";
dec_peer_event(5) -> "association restart";
dec_peer_event(6) -> "no server found (ntpdate mode)";
dec_peer_event(7) -> "rate exceeded";
dec_peer_event(8) -> "access denied";
dec_peer_event(9) -> "leap armed from server";
dec_peer_event(10) -> "become system peer";
dec_peer_event(11) -> "see clock status word";
dec_peer_event(12) -> "authentication failure";
dec_peer_event(13) -> "popcorn spike suppressor";
dec_peer_event(14) -> "entering interleave mode";
dec_peer_event(15) -> "interleave error".

mk_query_packet(Seq, Ass_id) ->
    Qp = <<
	0:2, %Leap indicator
	4:3, %Version (rfc says 3 but 4 works too)
	6:3, %Mode
	0:1, %Response bit: Cmd
	0:1, %Error bit
	0:1, %More bit: 0 - last fragment
	1:5, %Operation code: 1 - read status
	Seq:16, %Sequence number
	0:16, %Status word
	Ass_id:16, %Association id
	0:16, %Offset (used when fragmenting)
	0:16  %Length of data field
	>>,
    0 = bit_size(Qp) rem 32,
    Qp.

decode_and_print_variables(Lvl, _) when Lvl < 3 -> ok;
decode_and_print_variables(_, {_, 1, 0, _, _Vars}) ->
    ok; %%no variables in readstatus association 0
decode_and_print_variables(_, {_, _, _, _, Vars}) ->
    io:format("~p~n", [decode_variables(Vars)]);
decode_and_print_variables(_, Bin) ->
    io:format("~p~n", [decode_variables(Bin)]).

decode_and_print_variables(_, {_, 1, 0, _, _Vars}, _Fd) ->
    ok; %%no variables in readstatus association 0
decode_and_print_variables(_, {_, _, _, _, Vars}, Fd) ->
    io:format(Fd, "~p~n", [decode_variables(Vars)]);
decode_and_print_variables(_, Bin, Fd) ->
    io:format(Fd, "~p~n", [decode_variables(Bin)]).

decode_variables(Bin) ->
    d_var(Bin, {<<>>, <<>>}, [], name, outside).

d_var(<<C:8, Bin/binary>>, Acc, Found, T, outside) when
	%C =:= $\r; C =:= $\n; C =:= $\s ->
	C =:= $\r; C =:= $\n ->
    d_var(Bin, Acc, Found, T, outside);
d_var(<<"\s", Bin/binary>>, Acc, Found, name, outside) ->
    d_var(Bin, Acc, Found, name, outside);
d_var(<<"=", Bin/binary>>, Acc, Found, name, outside) ->
    d_var(Bin, Acc, Found, value, outside);
d_var(<<",", Bin/binary>>, Acc, Found, value, outside) ->
    d_var(Bin, {<<>>, <<>>}, [Acc | Found], name, outside);
d_var(<<"\"", Bin/binary>>, Acc, Found, value, outside) ->
    d_var(Bin, Acc, Found, value, inside);
d_var(<<"\"", Bin/binary>>, Acc, Found, value, inside) ->
    d_var(Bin, Acc, Found, value, outside);
d_var(<<C, Bin/binary>>, {Name, <<>>}, Found, name, outside) ->
    d_var(Bin, {<<Name/binary, C:8>>, <<>>}, Found, name, outside);
d_var(<<C, Bin/binary>>, {Name, Value}, Found, value, Side) ->
    d_var(Bin, {Name, <<Value/binary, C:8>>}, Found, value, Side);
d_var(<<>>, {<<>>, <<>>}, Found, _, _) ->
    Found;
d_var(<<>>, Acc, Found, _, _) ->
    [Acc | Found].

mk_ds(Ts) ->
    {_,_,Micro} = Ts,
    {{Y, M, D}, {Hour,Minute,Second}} = calendar:now_to_universal_time(Ts),
    io_lib:format("~4b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b.~6..0b",
    [Y, M, D, Hour, Minute, Second, Micro]).

string_to_ip(Ip_bin) when is_binary(Ip_bin) ->
    string_to_ip(binary_to_list(Ip_bin));
string_to_ip(Ip_bin) when is_list(Ip_bin) ->
    string_to_ip(inet:parse_address(Ip_bin));
string_to_ip({ok, Ip_tuple}) ->
    Ip_tuple;
string_to_ip(Whatever_wrong) ->
    error(badarg, [Whatever_wrong]).

us_to_ps(0) -> "no difference";
us_to_ps(Us) when Us < 0 ->
    "about " ++ ustops(abs(Us)) ++ " behind";
us_to_ps(Us) when Us > 0 ->
    "about " ++ ustops(Us) ++ " ahead".

ustops(Us) when Us > 1000000 * 60 * 60 * 24 * 30 * 12->
    integer_to_list(Us div (1000000 * 60 * 60 * 24 * 30 * 12)) ++ " years(s)";
ustops(Us) when Us > 1000000 * 60 * 60 * 24 * 30 ->
    integer_to_list(Us div (1000000 * 60 * 60 * 24 * 30)) ++ " months(s)";
ustops(Us) when Us > 1000000 * 60 * 60 * 24 ->
    integer_to_list(Us div (1000000 * 60 * 60 * 24)) ++ " day(s)";
ustops(Us) when Us > 1000000 * 60 * 60 ->
    integer_to_list(Us div (1000000 * 60 * 60)) ++ " hour(s)";
ustops(Us) when Us > 1000000 * 60 ->
    integer_to_list(Us div (1000000 * 60)) ++ " minute(s)";
ustops(Us) when Us > 1000000 ->
    integer_to_list(Us div 1000000) ++ " second(s)";
ustops(Us) when Us > 1000 ->
    integer_to_list(Us div 1000) ++ " millisecond(s)";
ustops(Us) ->
    integer_to_list(Us) ++ " microsecond(s)".

%%% ----------------------------------------------------------
%%% Used by CCI
get_code({_Seq, 7, 0, Status, _Data}) ->
    get_system_status_word(<<Status:16>>);
get_code({_Seq, 7, Ass_id, Status, _Data}) ->
    get_peer_status_word(<<Ass_id:16, Status:16>>).

get_system_status_word(<<_Li:2, _Cs:6, _Ec:4, Le:4>>) ->
    dec_system_event(Le).
get_peer_status_word(<<_:16, _Peer_stat:5, _Peer_sel:3, _Pec:4, Ple:4>>) ->
    dec_peer_event(Ple).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
debug(Format) ->
    debug(Format, []).

debug(Format, Params) ->
    io:format("dbg ~p:" ++ Format ++ "~n", [?MODULE | Params]).
