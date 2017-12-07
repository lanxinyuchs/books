%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_max_allowed_ups_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R5A/1
%%% 
%%% @doc == Test max allowed created upgradepackages. ==
%%% <br/><br/>
%%% @end

-module(swm_max_allowed_ups_SUITE).
-vsn('/main/R4A/R5A/1').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R5A/2      2015-11-30 etxivri     Created due to Max 3 UPs allowed, default.
%%% R5A/1      2015-11-30 etxivri     Update error str check.
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([create_max_allowed_up/1,
	 fail_to_create_up/1,
	 ok_create_one_more_up_than_allowed/1,
	 step_up_revision/1,
	 disable_max_up_check/1,
	 enable_max_up_check/1,
	 remove_ups/1
	]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NrOfAllowedUps, 3).

-define(NC_Session, nc1).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_upgrade,ug1},
		 {cth_conn_log, []},
		 {rct_core,[]},
		 {rct_coli, {coli, [manual_connect]}},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"
					  ],
					  [
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].

%% @hidden
init_per_suite(Config) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session, ug1),
    MeId = swm_test_lib:get_me_id(?NC_Session),
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
     [{meId, MeId},
      {ugPath, UGPath}| Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, _Reason}  ->
    	    ct:pal("Test failed, start cleanup.",[]),
    	    enable_max_up_check(Config)
    end,
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [create_max_allowed_up,
     fail_to_create_up,
     disable_max_up_check,
     ok_create_one_more_up_than_allowed,
     enable_max_up_check,
     step_up_revision,
     fail_to_create_up,
     remove_ups
    ].


%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
create_max_allowed_up(Config) ->
    From_Label = swm_test_lib:get_sw_version(?NC_Session),
    ct:pal("Active version before upgrade starts: ~p~n",[From_Label]),
    MeId = proplists:get_value(meId, Config),
    UGPath=  proplists:get_value(ugPath, Config),

    %% Note! one UP exist from install.
    NrOfCreate = lists:seq(1, ?NrOfAllowedUps-1),
    lists:foreach(fun(X) ->
			  MeId = proplists:get_value(meId, Config),
			  UGPath=  proplists:get_value(ugPath, Config),

			  ct:pal("## Creat Nr: ~p", [X]),
			  swm_test_lib:ug_create_match_result(?NC_Session,
							      "SUCCESS",
							      ?SftpHost,
							      ?SftpUser,
							      ?SftpPassword,
							      UGPath,
							      MeId),
			  
			  LatestCreatedLabel = get_latest_up(),
			  ct:pal("Latest Created Label:~n~p~n",
				 [LatestCreatedLabel]),
			  timer:sleep(5000),
			  step_up_rev(LatestCreatedLabel)
		  end, NrOfCreate),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
fail_to_create_up(Config) ->
    %%%% Revision is stepped uped in create_max_allowed_up.

    MeId = proplists:get_value(meId, Config),
    UGPath=  proplists:get_value(ugPath, Config),

    Uri = "sftp://"++?SftpUser++"@"++?SftpHost++UGPath,

    Action =  {'ManagedElement', 
	       [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	       [{managedElementId, [], [MeId]},
		{'SystemFunctions',
		 [{systemFunctionsId,[],["1"]},
		  {'SwM',
		   [],
		   [{swMId,[],["1"]},
		    {createUpgradePackage, [], 
		     [{uri, [Uri]},
		      {password, [?SftpPassword]}]
		     }]}]}]},
    {error, Reply} = netconf(action, [nc1, Action]),
    ct:log("Reply: ~p, ",[Reply]),

   
    %% {'error-message',[{xmlns,"urn:ietf:params:xml:ns:netconf:base:1.0"},
    %% 		      {'xml:lang',"en"}],
    %%  ["Request could not be performed - resource not available, [Remove an upgrade package before creating another one]"]}
    %% 	= lists:last(Reply),

        {'error-message',[_,
		      _],
     [ErrInfo]}
	= lists:last(Reply),

    ct:log("ErrInfo: ~p, ",[ErrInfo]),
    %% ErrInfo = "Request could not be performed - resource not available, [Remove an upgrade package before creating another one".
    ErrStr = "Request could not be performed - resource not available, ",
    case re:run(ErrInfo, ErrStr) of
	{match, _} ->
	    ok;
	nomatch ->
	    ct:fail("Tc fail due to expected error str not rcsvd.")
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
ok_create_one_more_up_than_allowed(Config) ->
    %%%% Revision is stepped uped in create_max_allowed_up.

    MeId = proplists:get_value(meId, Config),
    UGPath=  proplists:get_value(ugPath, Config),
    swm_test_lib:ug_create_match_result(?NC_Session,
					"SUCCESS",
					?SftpHost,
					?SftpUser,
					?SftpPassword,
					UGPath,
					MeId).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
disable_max_up_check(_Config) ->
    ct:pal("### disable_max_up_check, to allowe create more UPs !",[]),
    ok = rct_coli:connect(coli),
    timer:sleep(1000),
    {ok,_} = rct_coli:send(coli,"/misc/authlevel BasebandSupportExpert"), 
    timer:sleep(1000),
    {ok,_} = rct_coli:send(coli,"/labonly/rcs/disable-max-up-check"),
    timer:sleep(1000),
    rct_coli:disconnect(coli),
    timer:sleep(1000).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
enable_max_up_check(_Config) ->
    ct:pal("### enable_max_up_check !",[]),
    ok = rct_coli:connect(coli),
    timer:sleep(1000),
    {ok,_} = rct_coli:send(coli,"/misc/authlevel BasebandSupportExpert"), 
    timer:sleep(1000),
    {ok,_} = rct_coli:send(coli,"/labonly/rcs/enable-max-up-check"),
    timer:sleep(1000),
    rct_coli:disconnect(coli),
    timer:sleep(1000).

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
step_up_revision(_Config) ->
    LatestCreatedLabel = get_latest_up(),
    ct:pal("Latest Created Label:~n~p~n",
    	   [LatestCreatedLabel]),
    timer:sleep(5000),
    step_up_rev(LatestCreatedLabel),
    ok.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
remove_ups(_Config) ->
    %% Remove all UPs. no check.
    UPList = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UPList:~n~p~n",[UPList]),

    lists:foreach(fun(Label) ->
			 ct:pal("Remove upgrade package: : ~s",[Label]),
			 swm_test_lib:remove_upgrade_package(?NC_Session, Label),
			 timer:sleep(5000)
		 end, UPList).

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------
step_up_rev(PrevLabel) ->
     swm_test_lib:modify_cxs(PrevLabel, 1).
    
get_latest_up() ->
    UPs = swm_test_lib:get_ups(?NC_Session),
    ct:pal("UPs:~n~p~n",[UPs]),

    ToLabel = swm_test_lib:get_highest_label(UPs),
    ct:pal("ToLabel:~n~p~n",[ToLabel]),
    ToLabel.

netconf(F, A) ->
    swm_test_lib:check_nc_session(nc1),
    Res = apply(ct_netconfc, F, A),
    ct_netconfc:close_session(nc1),
    Res.
