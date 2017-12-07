%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaTestLib.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R9A/R10A/1
%%%
%%% @doc Module for common function used with LMA test suites.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(lmaTestLib).

%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date        Name        What
%%% -----      ---------   --------    ------------------------
%%% R2A/1      2014-05-06  etxpejn     Moved from RCT to LMA
%%% R3A/4      2015-02-28  etxkols     Preparation for 2 labs
%%% R4A/5      2015-06-15  etxmlar     Netconf support for vc card
%%% R4A/7      2015-09-23  etxpejn     Added wait_for_install_complete
%%% R5A/3      2015-06-21  etxkols     Added get_test_dir/0
%%% R7A/1      2016-09-16  etxpejn     Changed sleep in check_data_from_cli to 400ms
%%% R7A/2      2016-11-03  etxpejn     Added wait_for_lm_state_via_netconf & 
%%%                                    wait_for_eu_via_netconf
%%% R9A/1      2017-01-31  ekurnik     Added test support for installKeyFile over FTPES
%%% R10A/1     2017-06-01  etxpejn     Added activate_eu
%%% ----------------------------------------------------------

-export([activate_eu/0,
	 activate_grace_period/2,
	 changing_fingerprint_from_cli/1,
	 changing_fingerprint/1,
	 check_data_from_cli/3,
	 check_fingerprint_from_netconf/1,
	 check_fingerprint_from_netconf/0,
	 check_if_fingerprint_updateable/0,
	 connect_and_subscribe_capcity_over_lihi/2,
	 connect_and_subscribe_capcity_over_lihi/3,
	 connect_and_subscribe_feature_over_lihi/1,
	 extract_element/2,
	 find_mo_from_cli/5,
	 find_mo_from_cli_init/5,
	 install_lkf/0,
     install_lkf/1,
	 install_lkf/2,
     install_lkf/3,
     install_lkf/7,
	 change_feature_state/1,
	 change_feature_state/2,
	 unsubscribe_capcity_over_lihi/2,
	 unsubscribe_feature_over_lihi/2,
	 unsubscribe_feature_over_lihi/3,
	 poll_reinstall/1,
	 wait_for_netconf_started/0,
	 wait_for_install_complete/0,
	 get_test_dir/0,
	 wait_for_lm_state_via_netconf/2,
	 wait_for_eu_via_netconf/3
	]).

-include("lihi.hrl").


activate_grace_period(ServerRef, CXC) ->
    ct:pal("Check if GP is activated or not for " ++ CXC),
    case find_mo_from_cli("FmAlarm=", ?FM_MO, 
			  ["minorType=9175051", "source=\"ManagedElement=1,SystemFunctions=1,Lm=1,"
			   "CapacityState="++CXC++",GracePeriod="++CXC], 1, ok) of
	{_, ok} ->
	    ct:pal("GP has already been activated, don't send LcciCapacityLicenseGpActivatedFwd"),
	    %% GP has already been activated, don't send antoher.
	    do_nada;
	{_, nok} ->
	    ct:pal("Send LcciCapacityLicenseGpActivatedFwd for " ++ CXC),
	    {ok, capacity_license_gp_activated} = 
		rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseGpActivatedFwd, 
				     {ServerRef, ?ClientRef, CXC}),
	    ct:pal("Receive LcciCapacityLicenseChangeInd"),
	    {ok, {?LcciCapacityLicenseChangeInd, ?ClientRef, ?LicenseState_Enabled, 
		  ?GP_monitoring_shall_NOT_be_done_0, _ActivationThreshold, _, ?NoLimit_active_1}} = 
		rct_proxy:receive_proxy(),
	    ct:pal("Check that the GP alarm is visible"),
	    {_, ok} = 
		find_mo_from_cli("FmAlarm=", ?FM_MO, 
				 ["minorType=9175051", "source=\"ManagedElement=1,"
				  "SystemFunctions=1,Lm=1,CapacityState="++CXC++",GracePeriod="
				  ++CXC, "activeSeverity=MINOR"], ?NO_OF_TRIES, ok),
	    ct:pal("Check that the GP MO is activated for " ++ CXC),
	    {_, ok} = find_mo_from_cli("GracePeriod="++CXC, ?LM_MO ++",CapacityState=" ++CXC, 
				       ["gracePeriodId=\""++CXC,"gracePeriodState=ACTIVATED"], 
				       ?NO_OF_TRIES, ok)
    end,
    ok.

changing_fingerprint_from_cli(Fingerprint) ->
    rct_cli:send(cli,"configure"),
    rct_cli:send(cli,"ManagedElement=1,SystemFunctions=1,Lm=1"),
    rct_cli:send(cli,"fingerprint =" ++ Fingerprint),
    rct_cli:send(cli,"commit"), 
    rct_cli:send(cli, "top").

changing_fingerprint(Fingerprint) ->
    %% Set fingerprint
    A = {'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',[],
	   [{systemFunctionsId,[],["1"]},
	    {'Lm',
	     [{xmlns,"urn:com:ericsson:ecim:LM"}],
	     [{lmId,[],["1"]},
	      {fingerprint,[],[Fingerprint]}
	     ]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    ct_netconfc:edit_config(nc1, running, A),
    ct_netconfc:close_session(nc1).

activate_eu() ->
    EU =
	{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',
	   [{systemFunctionsId,[],["1"]},
	    {'Lm',
	     [{lmId,[],["1"]},
	      {'EmergencyUnlock',
	       [{emergencyUnlockId,[],["1"]},
		{'activate', [], []}
	       ]}]}]}]},

    {ok,_} = ct_netconfc:open(nc1,[]),
    ct_netconfc:action(nc1, EU),
    ct_netconfc:close_session(nc1).
    

%%
%% This function is to be used to check if a MO contains whanted attributes. 
%%
check_data_from_cli([], _Mo, _NoOfTries) ->
    ok;
check_data_from_cli([WantedString | RestWantedList], Mo, NoOfTries) ->
    {ok, RecievedData} = rct_cli:send(cli, "show verbose "++Mo, print),
    case check_data(WantedString, RecievedData) of
	ok ->
	    check_data_from_cli(RestWantedList, Mo, NoOfTries);
	nok ->
	    case NoOfTries of
		0 ->
		    nok;
		_Else ->
		    ct:pal("Did not find ~p, trying again in 400 msec", [WantedString]),
		    timer:sleep(400),
		    check_data_from_cli([WantedString] ++ RestWantedList, Mo, NoOfTries -1)
	    end
    end.

check_data(WantedString, RecievedData) ->
    Options = [global, %% check all rows
               {capture, all, binary}],
    case re:run(RecievedData, WantedString, Options) of
	{match, _} ->
	    ok;
	nomatch ->
	    nok
    end.

check_report_progress_from_netconf() ->
    ct:pal("Check the report progress from Netconf"),
    %%{ok,_} = ct_netconfc:open(nc1, []),
    netconf_open(nc1, []), 
    {ok, R} = ct_netconfc:get(nc1, {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				    [{managedElementId,[],["1"]},
				     {'SystemFunctions',
				      [{systemFunctionsId,[],["1"]},
				       {'Lm',
					[{lmId,[],["1"]},
					 {'KeyFileManagement',
					  [{keyFileManagementId,[],["1"]}]}
					]}]}]}),
    ok = ct_netconfc:close_session(nc1),
    extract_element(reportProgress, R).

wait_for_lm_state_via_netconf(_State, 0) ->
    nok;
wait_for_lm_state_via_netconf(State, No) ->
    LmState = {'ManagedElement',
	       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId,[],["1"]},
		{'SystemFunctions',
		 [{systemFunctionsId,[],["1"]},
		  {'Lm',
		   [{xmlns,"urn:com:ericsson:ecim:LM"}],
		   [{lmId,[],["1"]},
		    {lmState,[],[]}
		   ]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, C} = ct_netconfc:get(nc1, LmState),
    ct_netconfc:close_session(nc1),
    {ok,{_,_,R}} = extract_element(lmState, C),
    ct:pal("R: ~p", [R]),

    case R of
	 [State] ->
	    ok;
	_Else ->
	    timer:sleep(400),
	    wait_for_lm_state_via_netconf(State, No-1)
    end.

wait_for_eu_via_netconf(_Attribute, _Value, 0) ->
    nok;
wait_for_eu_via_netconf(Attribute, Value, No) ->
    EU = {'ManagedElement',
	  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'SystemFunctions',
	    [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{xmlns,"urn:com:ericsson:ecim:LM"}],
	      [{lmId,[],["1"]},
	       {'EmergencyUnlock',
		[],
		[{emergencyUnlockId,[],["1"]},
		 {Attribute,[],[]}
		]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, C} = ct_netconfc:get(nc1, EU),
    ct_netconfc:close_session(nc1),
    {ok,{_,_,R}} = extract_element(Attribute, C),
    ct:pal("R: ~p", [R]),

    case R of
	[Value] ->
	    ok;
	_Else ->
	    timer:sleep(400),
	    wait_for_eu_via_netconf(Attribute, Value, No-1)
    end.



check_if_fingerprint_updateable() ->
    ct:pal("Check from CLI that the fingerprint is updateable"),
    check_data_from_cli(["fingerprintUpdateable=true"], 
			"ManagedElement=1,SystemFunctions=1,Lm=1", 0).

check_fingerprint_from_netconf() ->
    ct:pal("Check the fingerprint from Netconf"),
    {ok,_} = ct_netconfc:open(nc1, []),

    {ok, R} = ct_netconfc:get(nc1, {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				    [{managedElementId,[],["1"]},
				     {'SystemFunctions',
				      [{systemFunctionsId,[],["1"]},
				       {'Lm',
					[{lmId,[],["1"]}]}]}]}),
    ok = ct_netconfc:close_session(nc1),
    {ok,{fingerprint,[],Fingerprint}} = extract_element(fingerprint, R),
    Fingerprint.

check_fingerprint_from_netconf(Fingerprint) ->
    ct:pal("Check that the fingerprint has been changed to " ++ Fingerprint),
    Fingerprint = check_fingerprint_from_netconf(),
    ok.

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.


connect_and_subscribe_capcity_over_lihi(CXC, GP_Option) ->
    connect_and_subscribe_capcity_over_lihi(CXC, GP_Option, ?ClientRef).

connect_and_subscribe_capcity_over_lihi(CXC, GP_Option, Client) ->    
    ProtocolVersion = 1,
    ct:pal("Send LcciConnToServerReq to GLMS over LIHI with protocol version: ~p", 
	   [ProtocolVersion]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LcciConnToServerReq, 
						   {ProtocolVersion, Client}),
    ct:pal("Wait for LcciConnToServerCfm from GLMS"),
    {ok, {?LcciConnToServerCfm, ProtocolVersion, ServerRef}} = rct_proxy:receive_proxy(),
    ct:pal("Connection to GLMS established over LIHI"),

    ct:pal("Send LcciCapacityLicenseSubscribeReq to GLMS over LIHI"),
    {ok, capacity_license_subscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseSubscribeReq, 
			     {ServerRef, Client, CXC, "Name of the capacity", 
			      "Description of the capacity", "No of cups of coffee", 
			      GP_Option, ?SW_CAPACITYTYPE}),
    ct:pal("Wait for LccipacityLicenseSubscribeCfm from GLMS"),
    {ok, {?LcciCapacityLicenseSubscribeCfm, 1, Client, LicenseState, 
	  GP_Avail, _ActivationThreshold, CapacityValue, NoLimit}} = rct_proxy:receive_proxy(),
    {ServerRef, LicenseState, GP_Avail, _ActivationThreshold, CapacityValue, NoLimit}.

unsubscribe_capcity_over_lihi(CXC, ServerRef) ->
    unsubscribe_capcity_over_lihi(CXC, ServerRef, ?ClientRef).

unsubscribe_capcity_over_lihi(CXC, ServerRef, Client) ->
    ct:pal("Send LcciCapacityLicenseUnsubscribeReq to GLMS over LIHI for " ++ CXC),
    {ok, capacity_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LcciCapacityLicenseUnsubscribeReq, 
			     {ServerRef, Client, CXC}),
    ct:pal("Wait for LcciCapacityLicenseUnsubscribeCfm from GLMS"),
    {ok, {_LcciCapacityLicenseUnsubscribeCfm, Client}} = rct_proxy:receive_proxy(),
    ok.

connect_and_subscribe_feature_over_lihi(CXC) ->
    ProtocolVersion = 1,
    ct:pal("Send LfciConnToServerReq to GLMS over LIHI with protocol version: ~p", 
	   [ProtocolVersion]),
    {ok,conn_to_server_req} = rct_proxy:send_proxy(node1, lihi1, ?LfciConnToServerReq, 
						   {ProtocolVersion, ?ClientRef}),

    ct:pal("Wait for LfciConnToServerCfm from GLMS"),
    {ok, {?LfciConnToServerCfm, ProtocolVersion, ServerRef}} = rct_proxy:receive_proxy(),
     
    %% The license user subscribes for license controlled features
    ct:pal("Send LfciFeatureLicenseSubscribeReq to GLMS over LIHI"),
    {ok, feature_license_subscribe_req} = 
	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseSubscribeReq, 
			     {ServerRef, ?ClientRef,CXC, "Name of the feature", 
			      "Description of the feature"}),
    
    ct:pal("Wait for LfciFeatureLicenseSubscribeCfm from GLMS"),
    {ok, {?LfciFeatureLicenseSubscribeCfm, ?ClientRef}} = rct_proxy:receive_proxy(),
    {ok, ServerRef}.

unsubscribe_feature_over_lihi(CXC, ServerRef) ->
    unsubscribe_feature_over_lihi(CXC, ServerRef, ?ClientRef).

unsubscribe_feature_over_lihi(CXC, ServerRef, Client) ->
    {ok, feature_license_unsubscribe_req} = 
    	rct_proxy:send_proxy(node1, lihi1, ?LfciFeatureLicenseUnsubscribeReq, 
    			     {ServerRef, Client, CXC}),
    ct:pal("Wait for LfciFeatureLicenseUnsubscribeCfm from GLMS"),
    {ok, {?LfciFeatureLicenseUnsubscribeCfm, Client}} = rct_proxy:receive_proxy().

find_mo_from_cli_init(PrefixMo, ParentMo, WantedAttribute, NoOfTries, WantedAnswer) ->
    MOs = find_data_from_cli(PrefixMo, ParentMo),
    ct:pal("Checking these MOs: ~p", [MOs]),
    find_mo(PrefixMo, ParentMo, MOs, WantedAttribute, NoOfTries, WantedAnswer).

find_mo_from_cli(PrefixMo, ParentMo, WantedAttribute, NoOfTries, WantedAnswer) ->
    timer:sleep(1000),
    MOs = find_data_from_cli(PrefixMo, ParentMo),
    ct:pal("Checking these MOs: ~p", [MOs]),
    find_mo(PrefixMo, ParentMo, MOs, WantedAttribute, NoOfTries, WantedAnswer).

find_mo(PrefixMo, ParentMo, [], AttrList, NoOfTries, WantedAnswer) ->
    case NoOfTries of
	0 ->
	    ct:pal("MO not found with attribute: ~p", [AttrList]),
	    {no_mo, nok};
	_Else ->
	    find_mo_from_cli(PrefixMo, ParentMo, AttrList, NoOfTries-1, WantedAnswer)
    end;
find_mo(PrefixMo, ParentMo, [MO | RestMo], AttrList, NoOfTries, WantedAnswer) ->
    LDN = ParentMo ++ "," ++ MO,
    case check_data_from_cli(AttrList, LDN, 0) of
	WantedAnswer ->
	    {LDN, WantedAnswer};
	_ ->
	    find_mo(PrefixMo, ParentMo, RestMo, AttrList, NoOfTries, WantedAnswer)
    end.

install_lkf() ->
    install_lkf(sftp).

install_lkf(sftp) ->
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    TEST_DIR = get_test_dir(),
    {ok,_} = file:copy(TEST_DIR++?LKF, ?PROJ_DIR++JenkinsNode++"/"++?LKF),
    install_lkf(?PROJ_DIR, ?LKF);
install_lkf(ftpes) ->
    %% connect to remote server via ftpes and transfer file
    {ok, Data} = file:read_file(get_test_dir() ++ ?LKF),
    {ok, CurrentDir} = rct_ftpes_client:pwd(),
    rct_ftpes_client:write_file(filename:join(CurrentDir, ?LKF), Data),
    install_lkf(ftpes, CurrentDir, ?LKF).

install_lkf(Dir, Lkf) ->
    install_lkf(sftp, Dir, Lkf).

install_lkf(sftp, Dir, Lkf) ->
    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(rct_oamap_ipv:sftp_server_iptype()),
    install_lkf(sftp, Dir, Lkf, JenkinsNode, SftpHost, Username, Password);
install_lkf(ftpes, Dir, Lkf) ->
    FtpesHost = ftpes_test_lib:get_ftpes_test_server_address(ipv4),
    Username = ftpes_test_lib:get_ftpes_test_server_username(),
    Password = ftpes_test_lib:get_ftpes_test_server_password(),
    install_lkf(ftpes, Dir, Lkf, "", FtpesHost, Username, Password).
    
install_lkf(Protocol, Dir, Lkf, JenkinsNode, Host, Username, Password) 
  when Protocol =:= sftp orelse Protocol =:= ftpes ->
    rct_cli:send(cli, ?KEYFILE_MGMT_MO),
    %% Make sure that no other install is ongoing
    ActionId = 
	case wait_for_install_complete() of
	    ok ->
		{ok, Answer} = rct_cli:send(cli,"installKeyFile \"" 
                        ++ atom_to_list(Protocol) ++ "://"++Username++"@"
					    ++Host++Dir++JenkinsNode++"/"++Lkf++"\""++" "
					    ++Password),
		get_action_id(string:tokens(Answer, "\r\n "), Password);
	    nok ->
		0
	end,
    rct_cli:send(cli, "top"),
    ActionId.

wait_for_install_complete()->
    case check_report_progress_from_netconf() of
	{ok,{_,_,_ReportProgress}} ->
	    case check_data_from_cli(["state=FINISHED"], ?KEYFILE_MGMT_MO, ?NO_OF_TRIES_XL) of
		ok ->
		    ok;
		nok ->
		    ct:pal("Not able to tell if install is ongoing or not!"),
		    nok
	    end;
	not_found ->
	    ok
    end.

change_feature_state(Key) ->
    case check_data_from_cli(["featureState=ACTIVATED"], Key, 0) of
	ok ->
	    ct:pal("Changing state to DEACTIVATED"),
	    change_feature_state(Key, "DEACTIVATED"); 
	nok ->
	    ct:pal("Changing state to ACTIVATED"),
	    change_feature_state(Key, "ACTIVATED")
    end.

change_feature_state(Key, State) ->
    rct_cli:send(cli,"configure"),
    rct_cli:send(cli, Key),
    rct_cli:send(cli,"featureState="++State),
    rct_cli:send(cli,"commit"), 
    rct_cli:send(cli, "top"),
    State.

% Return 
get_test_dir() ->
    {RCS_TOP, GIT_TEST_PATH, CC_TEST_PATH} = ?GIT_TEST_DIR,
    case os:getenv(RCS_TOP) of
	false ->
	    ct:fail("Could not resolve environment variable $RCS_TOP");
	Base ->
	    case string:str(Base, "/vobs") of
		0 -> % GIT
		    filename:join(Base,GIT_TEST_PATH)++"/";
		_ -> % CC
		    filename:join(Base,CC_TEST_PATH)++"/"
	    end
    end.
    

%% Internal functions
%%--------------------------------------------------------------------
%% @doc 
%% Text <br/>
%% @end
%%--------------------------------------------------------------------
find_data_from_cli(Prefix, MO) ->
    {ok, RecievedData} = rct_cli:send(cli, "show "++MO, print),
    Data = string:tokens(RecievedData, "\""),
    Data2 = string:tokens(lists:flatten(Data), "\r\n "),
    find_mo(Prefix, Data2, []).
    
find_mo(_Prefix, [], MOs) ->
    MOs;
find_mo(Prefix, [Element | RestElement], MOs) ->
    case lists:prefix(Prefix, Element) of
	true ->
	    find_mo(Prefix, RestElement, MOs ++ [Element]);
	false ->
	    find_mo(Prefix, RestElement, MOs)
    end.

get_action_id([], _Password) ->
    ct:pal("No action Id found");
get_action_id([Password | RestList], Password) ->
    [ActionId] = lists:sublist(RestList, 1, 1),
    ActionId;
get_action_id([_Element | RestList], Password) ->
    get_action_id(RestList, Password).




%% %% Return sim,not_sec_card or sec_card
check_kdu()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    case ct:get_config({list_to_atom(Hw),secure_board}) of
	"yes" ->
	    sec_card;
	_Other ->
	    not_sec_card
    end.

netconf_open(Session, Param)->
    case check_kdu()  of
	TARGET when TARGET == sim;
		    TARGET == not_sec_card -> 
	    ct_netconfc:open(Session,Param);
	sec_card ->  
	    ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param])
    end.


poll_reinstall(Nc1)->
    Timer = 10000,
    timer:sleep(Timer),
    ct:pal("Wait ~p s before checking if node is up",[Timer/1000]),
    case ct_netconfc:open(Nc1, []) of
	{ok,_} ->  
	    case  ct_netconfc:get_config(
		    Nc1,running,
		    {'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		     [{managedElementId,[],["1"]}]}) of
		{ok,_}-> 
		    ct:pal("Reinstall done"),
		    ok = ct_netconfc:close_session(Nc1, 10000),
		    ok;
		_-> 
		    ct_netconfc:close_session(Nc1, 10000),
		    poll_reinstall(Nc1)
	    end;
	_-> 
	    poll_reinstall(Nc1)
    end.


wait_for_netconf_started() ->
    wait_for_netconf_started(150000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");
wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
    	{ok,_} ->
	    ct:pal("netconf open - ok.",[]);
	{error,{ssh,could_not_connect_to_server,econnrefused}} ->
	    timer:sleep(250),
 	    wait_for_netconf_started(Timeout - 250);
	_Other ->
 	    timer:sleep(250),
	    wait_for_netconf_started(Timeout - 250)
    end.
