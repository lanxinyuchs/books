%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	fetch_esi_vc_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R6A/1
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end


-module(fetch_esi_vc_SUITE).
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2014-10-10 eransbn     Created
%%% R6A/1      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 transfer_esi_unpacked/1]).

-define(SFTP_HOST, "10.68.200.11").
-define(USER, "mauve").
-define(PSWD, "dilbert").
-define(SFTP_URL, "sftp://"++?USER++"@"++?SFTP_HOST).
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_netconf, {nc1, man_auth} },
                 {cth_conn_log,[]},
		 {rct_rpc, rpc_1}


		]}].


%% @hidden
init_per_suite(Config) ->
    poll_connection(nc1, 180),
    crypto:start(),
    ssh:start(),
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    [{meId, MeId}|Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [transfer_esi_unpacked].




%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a transfer ESI log from SUT to a sftp server<br/>
%% This TC transfer ESI log from SUT to a sftp server using netconf.<br/>
%% The log file will be stored .../log_private/ and unpacked, that will be created when TC starts.<br/>
%% TC will check progressreport using netconf and also check that correct file is transfered correct.
%%
%% @spec transfer_esi_unpacked(Config) -> ok
%% @end
%%--------------------------------------------------------------------
transfer_esi_unpacked(Config)->
    MeId = proplists:get_value(meId, Config),
    EsiLogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++EsiLogPath), % else permission.
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [?SFTP_URL++EsiLogPath]},
		     {password, [], [?PSWD]}]}
		  ]}]}]},
    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'LogM',
			  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			  [{logMId,[],["1"]},
			   {progressReport,[],[]}
			  ]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[{timeout, 5000}, {user, "SysAdminTest"}, {password, "SysAdminTest"}]),
    %% trig the action transfer_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
	{ok, A} ->
	    Return = extract_element(returnValue, A),
	    ct:pal("transfer_esi ~p~n",[Return]);
	Error ->
	    ct:pal("~p~n",[Error]),
	    ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),
    timer:sleep(500),
    case wait_for_progress(progressReport, ProgressFilter) of
	{["SUCCESS"], [EsiLogName]} ->
	    ct:pal("transfer_esi: SUCCESS~n",[]),
	    check_esi_log_transfered(EsiLogName, EsiLogPath, true);
	Result ->
	    ct:pal("transfer_esi: ~p~n",[Result]),
	    ct:fail(Result)
    end,
    ct:pal("Test case complete~n",[]).



%% @hidden
%% Will check that esi log is tranfered to expected path and also the size of the file.
%% If the size is to small, then maybe it does not consist on anything!
check_esi_log_transfered(EsiLogName, EsiLogPath, UnpackEsiFile) ->
    {ok,ChPid,_ChRef} =
	ssh_sftp:start_channel(?SFTP_HOST,
			       [{user, ?USER},
				{password, ?PSWD},
				{silently_accept_hosts, true},
				{timeout, 180000}]),

    {ok, DirData} = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
    % DirData = lists of strings from the directory.
    Pred= fun(Str) ->
		  if Str == EsiLogName ->
			  true;
		     true ->
			  false
		  end
	  end,

    case lists:any(Pred,DirData) of
	true ->
	    ct:pal("### Esi log file: ~p, exist in \n path : ~p \n",
		   [EsiLogName, EsiLogPath]),
	    timer:sleep(100),
	    case UnpackEsiFile of
		true -> os:cmd("tar -xf " ++ EsiLogPath ++ EsiLogName ++" --gunzip --directory=" ++ EsiLogPath),
			io:format("<a href=\"~s\">~s</a>",
				  [EsiLogPath, "Esi unpacked dir"]),
			io:format("<a href=\"~s\">~s</a>",
				  [filename:join(EsiLogPath,EsiLogName), EsiLogName]);
		_-> io:format("<a href=\"~s\">~s</a>",
			      [filename:join(EsiLogPath,EsiLogName), EsiLogName])
	    end;
	false ->
	    ct:fail(" Could not find the ESI log file, on sftp server.")
    end,

    {ok, FileInfo} =
	ssh_sftp:read_file_info(ChPid, EsiLogPath++EsiLogName, 3000),
    %ct:pal("### Recieved FileInfo: ~p", [FileInfo]),

    Size = lists:nth(2, tuple_to_list(FileInfo)),
    %{file_info, Size, _, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,

    if Size > 10000 ->
	    true;
       true  ->
	    ct:pal("### Size of the esi tar file is: ~p. ~n "
		   "It is smaller than expected. ~n "
		   "Unpack the file and ckeck that it look OK. \n",[Size]),
	    ct:fail("Size of the esi log file is to small! check if it "
		    "looks ok after unpack!.")
    end,

    ssh_sftp:stop_channel(ChPid),

    ok.

%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------

wait_for_progress(Attribute, ProgressFilter) ->
    {ok, _} = ct_netconfc:open(nc1, [{timeout, 5000}, {user, "SystemAdministrator"}, {password, "SystemAdministrator"}]),
    {ok, A} = ct_netconfc:get(nc1, ProgressFilter),
    ct_netconfc:close_session(nc1),
    {ok, Report} = extract_element(Attribute, A),
    {ok, State} = extract_element(state, [Report]),
    timer:sleep(1000),
    case State of
	{state, _, ["FINISHED"]} ->
	    ct:log("~p~n",[Report]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} =
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, ["CANCELLED"]} ->
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} =
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, [Current]} ->
	    ct:log("State: ~s~n",[Current]),
	    wait_for_progress(Attribute, ProgressFilter)
    end.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

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

%%%--------------------------------------------------------------------
%%Node = target
%%Time in second
%%%--------------------------------------------------------------------
poll_connection(Node, Time) ->
    ct:pal("Trying to connect to the target for ~p seconds",[Time]),
    poll_connection(Node, Time, []).
poll_connection(_Node, Time, Reason) when Time =:= 0 ->
    ct:fail(Reason);
poll_connection(Node, Time, _Reason) ->
    timer:sleep(1000),
    case  ct_netconfc:open(Node,[{timeout, 5000}, {user, "SystemAdministrator"}, {password, "SystemAdministrator"}]) of
	{ok,_} -> ct_netconfc:close_session(Node),
		  ok;
	Result-> poll_connection(Node, Time-1, Result)
    end.



