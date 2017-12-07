%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_ms_user_change.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R4A/1

%%% @doc ==Header==
%%% @end

-module(omc_ms_user_change).
-vsn('/main/R2A/R4A/1').
-date('2015-09-11').
-author('etxasta').

-include("omc.hrl").

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-01-29   etxlg     Created. What a hack - bleah
%%% R2A/2      2014-01-30   etxlg     edoc trouble
%%% R2A/3      2014-03-28   etxlg     use omc_ldap_server:ldap_lookup/1
%%% R2A/4      2014-04-09   etxlg     keep track of terminal row/col
%%% R2A/5      2014-06-16   etxlg     keep track of login sessions
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([ms_user_change_capable/2]).
-export([ms_user_change_capable/3]).
-export([ms_user_change/8]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc
%%% Used by SSH and TLS to check if the current user has capability (role-based)
%%% to initiate a change to a different user.
%%% @end
%%% ----------------------------------------------------------

ms_user_change_capable(Type, Roles) ->
    ms_user_change_capable(Type, Roles, []).
ms_user_change_capable(Type, Roles, Arg_list) ->
    %debug("T: ~p, R: ~p~n", [Type, Roles]),
    u_cap(Type, lists:member(?MS_USER_ROLE, Roles), Arg_list).

%%% ----------------------------------------------------------
%%% @doc
%%% Used by SSH and TLS to check for the new user during MS initiated
%%% user change.
%%% @end
%%% ----------------------------------------------------------
ms_user_change(Type, Transport, User_scan, Data, Port, Peer_ip, User, Roles) ->
% Returns: {User_scan, Is_user_cap, Port, User, Roles}
    if
	Type =:= cli, User_scan#user_scan.count > 200 ->
	    Msg = Transport ++ ": cli: user: " ++ User ++ 
		", No ms_useridentity communicated",
	    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
 	    {#user_scan{}, false, Port, User, Roles};
	Type =:= cli ->
	    Count = User_scan#user_scan.count + size(Data),
	    %debug("Count: ~b", [Count]),
	    do_ms_user_change(Type, Transport,
			User_scan#user_scan{count = Count}, Data, Port,
			Peer_ip, User, Roles);
	true ->
	    do_ms_user_change(Type, Transport, User_scan, Data, Port,
				Peer_ip, User, Roles)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%io:format("TO:~p~n", [binary_to_list(Data)]),

u_cap(_, false, _) ->
    {false, #user_scan{}};
u_cap(cli, true, Arg_list) ->
    {true, #user_scan{state = scan_for_config, acc = <<>>,
		      arg_list = Arg_list}};
u_cap(netconf, true, _) ->
    {true, #user_scan{acc = <<>>,
		      pattern1 = binary:compile_pattern(<<"]]>]]>">>)}}.

do_ms_user_change(netconf, Transport, #user_scan{acc = Acc,
					     pattern1 = Pattern} = User_scan,
			Data, undefined, Peer_ip, User, Roles) ->

    case check_for_hello(<<Acc/binary, Data/binary>>, Pattern) of
	{more, New_acc} ->
 	    {User_scan#user_scan{acc = New_acc}, true, undefined, User, Roles};
	{not_found, Complete_hello} ->
	    Msg = Transport ++ ": netconf: user: " ++ User ++ 
		", No ms_useridentity communicated",
	    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
	    New_port = omc_lib:run_portprog(User, netconf),
            true = port_command(New_port, Complete_hello),
 	    {#user_scan{}, false, New_port, User, Roles};
	too_long ->
	    Msg = Transport ++ ": netconf: user: " ++ User ++ 
		", No ms_useridentity communicated",
	    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
	    exit(normal);
	{ok, Filtered_hello, New_user} ->
	    case omc_ldap_server:ldap_lookup(New_user) of
		{true, New_roles} ->
		    Filtered_r= lists:delete(?MS_USER_ROLE, New_roles),
		    Msg = "LDAP: lookup for user: " ++ New_user ++ ", Roles: "
			++ omc_lib:roles_to_string(Filtered_r),
		    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
		    ok = omc_server:store_authorization(New_user, Filtered_r),
		    omc_server:store_session(New_user, netconf, ssh,
					     Filtered_r),
		    New_port = omc_lib:run_portprog(New_user, netconf),
		    true = port_command(New_port, Filtered_hello),
		    {#user_scan{}, false, New_port, New_user, Filtered_r};
		{false, _Reason} ->
		    Msg = "LDAP: lookup failure for user: " ++ New_user,
		    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
		    exit(normal)
	    end
    end;
do_ms_user_change(cli, _, #user_scan{state = scan_for_config,
				    arg_list = Arg_list} = User_scan,
		  Data, Port, Peer_ip, User, Roles) ->
    case User_scan#user_scan.last_from of
	<<"(config)>">> ->
	    %debug("MATCHED config"),
	    New_acc = binary:replace(Data, [<<"\t">>, <<"\n">>, <<"\r">>],
				<<>>, [global]),
	    %debug("First ACC: |~s|~n Data: ~p",
	%	[binary_to_list(New_acc), Data]),
	    case check_oneliner(Data) of
		{found, New_user} ->
		    {User_scan#user_scan{acc = <<>>,
					user = New_user,
					state = scan_for_commit},
			true, Port, User, Roles};
		notfound ->
		    {User_scan#user_scan{acc = New_acc, state = scan_for_user},
			true, Port, User, Roles}
	    end;
	_ ->
	    %debug("scan_for_config  Data: ~p Last_from: ~p",
	%	[Data, User_scan#user_scan.last_from]),
	    case check_multiliner(Data) of
		{found, New_user} ->
		    swap_cli_program(Port, Peer_ip, New_user, Arg_list);
		notfound ->
		    {User_scan, true, Port, User, Roles}
	    end
    end;
do_ms_user_change(cli, _, #user_scan{state = scan_for_user,
				acc = Acc} = User_scan,
				Data, Port, _Peer_ip, User, Roles) ->
    New_acc = <<Acc/binary, (binary:replace(Data,
					[<<"\t">>, <<"\n">>, <<"\r">>],
					<<>>, [global]))/binary >>,
    Last_from = re:replace(User_scan#user_scan.last_from,
				"\b.", "", [global, {return, binary}]),
    case binary:match(Last_from, ?MS_USER_SET_PREFIX) of
	nomatch ->
	    %debug("scan_for_user-nomatch, Acc: ~p, Data: ~p New_acc: ~p"
	%	"Last_from: ~p",
	%	[Acc, Data, New_acc, Last_from]),
	    {User_scan#user_scan{acc = New_acc}, true, Port, User, Roles};
	_Match ->
	    %debug("scan_for_user-MATCH: ~p, Acc: ~p, Data: ~p New_acc: ~p",
	%	[_Match, Acc, Data, New_acc]),
	    {User_scan#user_scan{state = accumulate_user, acc = Data},
		true, Port, User, Roles}
    end;
do_ms_user_change(cli, _, #user_scan{state = accumulate_user,
				acc = Acc} = User_scan,
				Data, Port, _Peer_ip, User, Roles) ->
    case User_scan#user_scan.last_from of
	<<"(config)>">> ->
	    %debug("USER: ~p", [Acc]),
	    New_user = binary_to_list(
			binary:replace(Acc,
					[<<"\t">>, <<"\n">>, <<"\r">>,
						<<"\"">>, <<"\s">>],
                                        <<>>, [global])),
	    %debug("REALLY USER: ~s", [New_user]),
	    {User_scan#user_scan{state = scan_for_commit,
				acc = Data,
				user = New_user},
		true, Port, User, Roles};
	_ ->
	    New_acc = <<Acc/binary, Data/binary>>,
	    {User_scan#user_scan{acc = New_acc}, true, Port, User, Roles}
    end;
do_ms_user_change(cli, Transport, #user_scan{state = scan_for_commit,
				user = New_user,
				acc = Acc, arg_list = Arg_list} = User_scan,
				<<"\r">>, Port, Peer_ip, User, Roles) ->
    case assume_commit(Acc) of
	skip ->
	    %debug("COMMIT SKIP"),
	    {User_scan, true, Port, User, Roles};
	true ->
	    %debug("COMMIT ASUMED"),
		swap_cli_program(Port, Peer_ip, New_user, Arg_list);
	false ->
	    Msg = Transport ++ ": cli: user: " ++ User ++ 
		", No ms_useridentity communicated",
	    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
	    {#user_scan{}, false, Port, User, Roles}
    end;
do_ms_user_change(cli, _, #user_scan{state = scan_for_commit,
				acc = Acc} = User_scan,
				Data, Port, _Peer_ip, User, Roles) ->
    New_acc = <<Acc/binary, Data/binary>>,
    {User_scan#user_scan{acc = New_acc}, true, Port, User, Roles}.
%;
%do_ms_user_change(cli, _, #user_scan{acc = Acc} = User_scan,
%				Data, Port, _Peer_ip, User, Roles) ->
%    New_acc = <<Acc/binary, (binary:replace(Data,
%					[<<"\t">>, <<"\n">>, <<"\r">>],
%					<<>>, [global]))/binary >>,
%    debug("WARNING clause to be removed"),
%    debug("New ACC: |~s|~n", [binary_to_list(New_acc)]),
%    debug("Last FRM: |~p|~n", [User_scan#user_scan.last_from]),
%    {User_scan#user_scan{acc = New_acc}, true, Port, User, Roles}.

check_multiliner(Data) ->
    %debug("check_multiliner"),
    Regex = "configure\r" ++ binary_to_list(?MS_USER_SET_PREFIX) ++
	"\"*([^\".]+)\"*\rcommit\r",
    check_line(Data, Regex).

check_oneliner(Data) ->
    %debug("check_oneliner"),
    Regex = binary_to_list(?MS_USER_SET_PREFIX) ++
	"\"*([^\".]+)\"*\r",
    check_line(Data, Regex).

check_line(Data, Regex) ->
    case re:run(Data, Regex, [{capture, all_but_first, list}]) of
	{match, [New_user]} ->
	    {found, New_user};
	_ ->
	    notfound
    end.

swap_cli_program(Port, Peer_ip, New_user, Arg_list) ->
    %debug("swap_cli_program"),
    flush_and_forward_portdata(Port, <<>>),
    true = port_close(Port), %receive the exit here
    case omc_ldap_server:ldap_lookup(New_user) of
	{true, New_roles} ->
	    Filtered_r= lists:delete(?MS_USER_ROLE, New_roles),
	    Msg = "LDAP: lookup for user: " ++ New_user ++ ", Roles: "
		++ omc_lib:roles_to_string(Filtered_r),
	    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
	    ok = omc_server:store_authorization(New_user, Filtered_r),
	    omc_server:store_session(New_user, cli, ssh, Filtered_r),
	    New_port = omc_lib:run_portprog(New_user, cli, Arg_list),
	    discard_motd(New_port),
	    {#user_scan{}, false, New_port, New_user, Filtered_r};
	{false, _Reason} ->
	    Msg = "LDAP: lookup failure for user: " ++ New_user,
	    omc_lib:sec_log(omc_lib:ip_to_string(Peer_ip), Msg),
	    exit(normal)
    end.

flush_and_forward_portdata(Port, Sofar) ->
    receive
	{Port, {data, Data}} ->
	    %debug("Flushed: ~p", [Data]),
	    New_data = <<Sofar/binary, Data/binary>>,
	    case binary:last(Data) of
		$> ->
		    self() ! {flushed_data_to_user, New_data};
		_ ->
		    flush_and_forward_portdata(Port, New_data)
	    end
    after
	2000 ->
	    sysInitI:error_msg(
		"~p: Timeout flushing CLISS after "
		"ms_useridentity change~n", [?MODULE]),
	    exit(normal)
    end.

discard_motd(Port) ->
    receive
	{Port, {data, Data}} ->
	    %debug("Discarded: ~p", [Data]),
	    case binary:last(Data) of
		$> -> ok;
		_ -> discard_motd(Port)
	    end
    after
	2000 ->
	    sysInitI:error_msg(
		"~p: Timeout waiting for new CLISS to start after "
		"ms_useridentity change~n", [?MODULE]),
	    exit(normal)
    end.

assume_commit(<<>>) -> skip;
assume_commit(<<"c">>) -> true;
assume_commit(<<"co">>) -> true;
assume_commit(<<"com">>) -> true;
assume_commit(<<"comm">>) -> true;
assume_commit(<<"commi">>) -> true;
assume_commit(<<"commit">>) -> true;
assume_commit(<<"c\t">>) -> true;
assume_commit(<<"co\t">>) -> true;
assume_commit(<<"com\t">>) -> true;
assume_commit(<<"comm\t">>) -> true;
assume_commit(<<"commi\t">>) -> true;
assume_commit(<<"commit\t">>) -> true;
assume_commit(_Acc) ->
    %debug("asume_commit -> false Acc: ~p", [_Acc]),
    false.

check_for_hello(Hello_sofar, _) when size(Hello_sofar) > 400 ->
    too_long;
check_for_hello(Hello_sofar, Pattern) ->
    case binary:split(Hello_sofar, Pattern) of
	[_] ->
	    {more, Hello_sofar};
	[Xml_hello | _] ->
	    %debug("Found  ~p", [binary_to_list(Xml_hello)]),
	    parse_netconf_caps(Xml_hello)
    end.

parse_netconf_caps(Xml_hello) ->
    case xmerl_sax_parser:stream(Xml_hello, [{event_fun, fun sax_event/3},
					{continuation_state, not_found}]) of
	{ok, {ok, User}, _} ->
	    {ok, <<Xml_hello/binary, <<"]]>]]>">>/binary >>, User};
	_ ->
	    {not_found, <<Xml_hello/binary, <<"]]>]]>">>/binary >>}
    end.

sax_event({characters, Chars}, _, State) ->
    case lists:prefix(binary_to_list(?MS_USER_CAP_URN), Chars) of
	true ->
	    {ok, lists:sublist(Chars, size(?MS_USER_CAP_URN) + 1,
			length(Chars))};
	false ->
	    State
    end;
sax_event(_Event, _Location, State) -> 
    State.

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

%debug(Format) ->
%    debug(Format, []).

%debug(Format, Params) ->
%    io:format("dbg ~p:" ++ Format ++ "~n", [?MODULE | Params]).

