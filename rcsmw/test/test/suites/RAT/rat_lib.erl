-module(rat_lib).
%%% ----------------------------------------------------------
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
%%% R1A/1      2014-05-12 eransbn     Created
%%% R3A/2      2015-04-07 etxkols     Avoid ERROR REPORT in CT console
%%% R4A/1      2016-01-27 etxkols     Added send_netconf_filebinary/2
%%% R10A/1     2016-02-26 etxkols     Update due to changed behaviour in
%%%                                   xmerl-1.3.13.
%%%                                   That version is used in new git env.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([check_if_vc_board/0, 
	 reinstall/2,
	 send_netconf_xml/2, 
	 send_netconf_filebinary/2,
	 netconf_open/2
]).



-define(is_whitespace(C), C=:=?space ; C=:=?cr ; C=:=?lf ; C=:=?tab).
-define(space, 32).
-define(cr,    13).
-define(lf,    10).
-define(tab,   9).
%%Read xml file and convert it to simple xml.
send_netconf_xml(Client,File) ->
    ct:log("Reading file: ~p",[File]),
    {ok, FileBinary} = file:read_file(File),
    send_netconf_filebinary(Client,FileBinary).

send_netconf_filebinary(Client,FileBinary) ->
    BinaryList = binary:split(FileBinary, <<"]]>]]>">>, [global]),
    ParsList = parse_nc_to_simple(BinaryList, []),
    ParsList2 =  take_away_first(ParsList), %%take away hello

    {ok,_Reason} = netconf_open(Client,[]),
    lists:foreach(fun(Msg) ->Reason = ct_netconfc:send(Client, Msg),
			     case Reason of

				 {error, _} ->
				     ct:fail(Reason);
				 {_,_,[{'rpc-error',[],_}]} ->
				     ct:fail(Reason);
				 _->
				     ok
			     end
		  end, ParsList2),
    %%Check that the session is closed
    poll_session(Client, 10,[]).

take_away_first([_H|Rest])->
    Rest.

parse_nc_to_simple([], Acc) ->
    lists:reverse(Acc);
parse_nc_to_simple([<<>> |Rest], Acc) ->
    parse_nc_to_simple(Rest, Acc);
parse_nc_to_simple([Binary |Rest], Acc) ->
    case remove_ws(Binary) of
	<<>> ->
	    parse_nc_to_simple(Rest, Acc);
	NewBinary ->
	    case xmerl_sax_parser:stream(NewBinary, [{event_fun, fun nc_simple_dom:event/3},
						     {event_state, nc_simple_dom:initial_state()}]) of
		%% {ok, State, <<>>} ->
		%%     [E] = nc_simple_dom:get_dom(State),
		%%     parse_nc_to_simple(Rest, [E |Acc]);
		{ok, State, _} ->
		    [E] = nc_simple_dom:get_dom(State),
		    parse_nc_to_simple(Rest, [E |Acc]);
		R ->
		    parse_nc_to_simple(Rest, [R |Acc])
	    end
    end.

%%----------------------------------------
%% Internal func
%%----------------------------------------
poll_session(_Client, TimePoll, Result) when TimePoll =:=0->
    ct:fail("~p",[Result]);
poll_session(Client, TimePoll, Result)when TimePoll /= 0 ->
    timer:sleep(1000),
    case  ct_netconfc:get_session_id(Client) of
	{error, _} -> ok;
	Result ->
	    poll_session(Client,TimePoll -1, Result)
    end.

remove_ws(<<C, Rest/binary>>) when ?is_whitespace(C) ->
    remove_ws(Rest);
remove_ws(Binary) ->
    Binary.

%% Reinstall the rbs and check when its done.
reinstall(Rpc, Nc1)->
    case check_if_vc_board() of
	"yes" -> ct:log("This is secure board skip reinstall"),
		 ok;
	_ ->
	    ct:pal("Reinstall"),
	    {ok, ErlNode} = rct_rpc:get_erlnode(Rpc),
	    rct_rpc:call(Rpc,sysNetloader,coli_reinstall,[asdf],10000),
	    net_kernel:disconnect(ErlNode),
	    %%poll ManagedElement
	    Result = poll_reinstall(Nc1),
            net_kernel:connect(ErlNode),
	    Result
	end.
poll_reinstall(Nc1)->
    Timer = 15000,
    timer:sleep(Timer),
    ct:pal("Wait ~p s before checking if node is up",[Timer/1000]),
    Response = netconf_open(nc1,[]),
    case Response of
	{ok,_} ->  case  ct_netconfc:get_config(
			   Nc1,running,
			   {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			    [{managedElementId,[],["1"]}]}) of
		       {ok,_}-> ct:pal("Restart done"),
				ok = ct_netconfc:close_session(Nc1, 10000),
				ok;
		       _-> ok = ct_netconfc:close_session(Nc1, 10000),
			   poll_reinstall(Nc1)
		   end;
	_-> poll_reinstall(Nc1)
    end.
netconf_open(Session, Param)->
    Return=
	case check_if_vc_board()  of
	    "yes" ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param]);
	    _ -> ct_netconfc:open(Session,Param)
	    end,
    Return.

check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.
