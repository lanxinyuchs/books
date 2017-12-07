%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	decupling_rev_algorithm_global_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R6A/1
%%%
%%% @doc == HW-SW-DEC, verify HW revision selection algorithm. When HW only match in Global. ==
%%%
%%%
%%% @end

-module(decupling_rev_algorithm_global_SUITE).
-author('etxivri').
-vsn('/main/R5A/R6A/1').
-date('2016-08-29').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R5A/1      2016-03-04 etxivri     Created
%%% R5A/3      2016-03-14 etxivri     Add more test in special and ordinary rev.
%%% R5A/4      2016-03-16 etxivri     Add more test in special rev.
%%% R6A/1      2016-08-29 etxivri     Update check of Preliminar Product Vers.
%%%                                   P-ver match open range and closed range.
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 groups/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 
	 %% Ordinary Product Versions
	 test_1_1_1/1,  
	 test_1_1_2/1,  
	 test_1_1_3/1, 
	 %% no match
	 test_1_1_10/1,
	 test_1_1_11/1,

	 %% Ordinary Product Versions, using Verification R-state
	 test_1_2_1/1,  
	 test_1_2_2/1,  
	 test_1_2_3/1,
	 test_1_2_4/1,
	 %% no match
	 test_1_2_10/1,

	 %% Special Product Versions, using numerical suffix
	 test_2_1_1/1, 
	 test_2_1_2/1, 
	 test_2_1_3/1,  
	 %% no match
	 test_2_1_10/1, 

	 %% Special Product Versions, using alphabetic suffix
	 test_2_2_1/1,
	 test_2_2_2/1, 
	 test_2_2_3/1,  
	 %% no match
	 test_2_2_10/1,
	 test_2_2_11/1,
	 test_2_2_12/1, 

	 %% Preliminary Product Versions
	 test_3_1_1/1,
	 test_3_2_1/1, 
	 test_3_2_2/1, 
	 %% no match
	 test_3_3_1/1,

	 %% 10. Misc, 
	 %% Nya TCs som kollar max allowed sufix ex, ZZZZ, 9999
	 test_10_1_1/1,  %% -> CXP list
	 test_10_1_2/1,  %% -> CXP list
	 test_10_1_3/1,  %% -> error
	 test_10_1_4/1,  %% -> error
	 test_10_1_5/1,  %% -> error
	 test_10_1_6/1,  %% -> error
	 %% open Rev-state
	 test_10_2_1/1,  %% -> CXP list
	 test_10_2_2/1,  %% -> CXP list
	 %% closed Rev-state
	 test_10_3_1/1,  %% -> CXP list
	 test_10_3_2/1,  %% -> CXP list
	 %% Special rev, letter combination
	 test_10_4_1/1,  %% -> error
	 test_10_4_2/1,  %% -> error
	 %% Verification Rev
	 test_10_5_1/1,  %% -> error
	 test_10_5_2/1,   %% -> error
	 test_10_5_3/1
	]).

%% This is needed, otherwise error TC will result in ERROR in ct-shell.
-define(RPC, rct_rcsTestServer:ct_hook(rct_rpc)). 
%% -define(RPC, rpc).

-define(XML_PATH_1, "/proj/rcs-tmp/hw_sw_dec/xml_for_algorithm_test/rev_algorithm/new_1/").

%%%%
%% Globla UP xml.
%%%%
-define(GLOBAL_XML_1, "GlobalUP_1_rev_check-up.xml").
-define(GLOBAL_XML_MAX_REV_RANGE, "GlobalUP_1_max_rev_range-up.xml").
-define(GLOBAL_XML_NOK_1, "GlobalUP_nok_1-up.xml").
-define(GLOBAL_XML_NOK_2, "GlobalUP_nok_2-up.xml").
-define(GLOBAL_XML_NOK_3, "GlobalUP_nok_3-up.xml").
-define(GLOBAL_XML_NOK_4, "GlobalUP_nok_4-up.xml").


%% HW id.
%%%%
%% Ordinary product Version
%%%%
-define(REV_MATCH_EXACT_1, {"KDU111111/1","R30A"}).
-define(REV_MATCH_OPEN_RANGE_1, {"KDU111111/1","R103A"}).
-define(REV_MATCH_OPEN_RANGE_1_B, {"KDU111112/1","R103AA"}).
-define(REV_MATCH_CLOSED_RANGE_1, {"KDU111111/1","R99B"}).
-define(REV_MATCH_CLOSED_RANGE_1_B, {"KDU111113/1","R99BB"}).
%% No match
-define(REV_NO_MATCH_1, {"KDU111111/1","R100B"}).
-define(REV_ONLY_DIGITS_NO_MATCH, {"KDU111111/1","R30"}).

%%%%
%% Ordinary Product Versions, using Verification R-state
%%%%
-define(REV_VER_STATE_EXACT_MATCH_1, {"KDU111111/1","R30A02"}).
-define(REV_VER_STATE_MATCH_OPEN_RANGE_1, {"KDU111111/1","R310B03"}).
-define(REV_VER_STATE_MATCH_CLOSED_RANGE_1, {"KDU111111/1","R55B01"}).
-define(REV_ONLY_DIGITS_MATCH, {"KDU111111/1","R40"}).
%% No match
-define(REV_VER_STATE_NOT_MATCH_1, {"KDU111111/1","R100B01"}).

%%%%
%% Special products Versions, using numerical suffix.
%%%%
-define(REV_MATCH_EXACT_2, {"KDU111111/1","R3A/5"}).
-define(REV_MATCH_OPEN_RANGE_2, {"KDU111111/1","R3A/99"}).
%% -define(REV_MATCH_OPEN_RANGE_2, {"KDU111111/1","R3A/100"}).
-define(REV_MATCH_CLOSED_RANGE_2, {"KDU111111/1","R3A/77"}).
%% No match
-define(SPEC_REV_NOT_MATCH_2, {"KDU111111/1","R3A/97"}).
%%%%
%% Special products Versions, using alphabetic suffix
%%%%
-define(REV_MATCH_EXACT_3, {"KDU111111/1","R3A/B"}).
-define(REV_MATCH_EXACT_3_B, {"KDU111111/1","R3A"}).
-define(REV_MATCH_EXACT_3_C, {"KDU111111/4","R3A/B"}).
-define(REV_MATCH_EXACT_3_D, {"KDU511111/41","R3B/A"}).
%% -define(REV_MATCH_OPEN_RANGE_3, {"KDU111111/1","R3A/Z"}).
-define(REV_MATCH_OPEN_RANGE_3, {"KDU111111/2","R3A/Z"}).
-define(REV_MATCH_OPEN_RANGE_3_B, {"KDU111111/2","R30A"}).
-define(REV_MATCH_OPEN_RANGE_3_C, {"KDU111111/5","R30A/Z"}).
-define(REV_MATCH_OPEN_RANGE_3_D, {"KDU611111/6","R30C/B"}).
%% -define(REV_MATCH_CLOSED_RANGE_3, {"KDU111111/1","R3A/E"}).
-define(REV_MATCH_CLOSED_RANGE_3, {"KDU111111/3","R3A/E"}).
-define(REV_MATCH_CLOSED_RANGE_3_B, {"KDU111111/3","R3A"}).
-define(REV_MATCH_CLOSED_RANGE_3_C, {"KDU111111/6","R3A/E"}).
-define(REV_MATCH_CLOSED_RANGE_3_D, {"KDU711111/7","R25C/A"}).
%% No match
-define(SPEC_REV_NOT_MATCH_3, {"KDU111111/1","R3A/G"}).
-define(SPEC_REV_NOT_MATCH_4, {"KDU111111/1","R3A/I-"}).

%%%%
%% Preliminary Product Version
%%%%
-define(PREL_REV_MATCH_EXACT, {"KDU111111/1","P33ZZ"}).
-define(PREL_REV_MATCH_1, {"KDU222222/2","P4BB"}). 
-define(PREL_REV_MATCH_2, {"KDU222222/3","P5CC"}).

%% %% No match
%% -define(PREL_REV_NOT_MATCH_1, {"KDU222222/2","P1A"}). 
%% -define(PREL_REV_NOT_MATCH_2, {"KDU222222/3","P1A"}).
-define(PREL_REV_NOT_MATCH_1, {"KDU222222/3","P6A"}).

%% New TC error cases when it shall not match.
-define(REV_NOT_RECOGNIZE, {"KDU555555/1","R1A"}). %% No letter after digit


%% Usecase Error
-define(UC_ERR_2(Reason), {uc_error, Reason}).

%% Error causes.
-define(ERROR_CAUSE_1, "BoardType not supported by neither HalUP nor GlobalUP").
-define(ERROR_CAUSE_2, "Invalid revision format").
%% -define(ERROR_CAUSE_2, "The letters I, O, P, Q, R and W must not be used").
-define(ERROR_CAUSE_3, "Invalid revision range").


-define(RESULT_1, 
	[{{"TEST-1A","CXP111111_1A","R1A"},
	      {global,"TEST_1A_CXP111111.cxp"}},
            {{"TEST-1B","CXP111111_1B","R1B"},
             {global,"TEST_1B_CXP111111.cxp"}}] 
       ).
-define(RESULT_2, 
	[{{"TEST-2A","CXP222222_2A","R2A"},
	      {global,"TEST_2A_CXP222222.cxp"}},
	     {{"TEST-2B","CXP222222_2B","R2B"},
	      {global,"TEST_2B_CXP222222.cxp"}}]
       ).
-define(RESULT_3, 
	[{{"TEST-3A","CXP333333_3A","R3A"},
	      {global,"TEST_3A_CXP333333.cxp"}},
	     {{"TEST-3B","CXP333333_3B","R3B"},
	      {global,"TEST_3B_CXP333333.cxp"}}]
       ).
-define(RESULT_4, 
	[{{"TEST-4A","CXP444444_4A","R4A"},
	      {global,"TEST_4A_CXP444444.cxp"}},
	     {{"TEST-4B","CXP444444_4B","R4B"},
	      {global,"TEST_4B_CXP444444.cxp"}}]
       ).
-define(RESULT_5, 
	[{{"TEST-5A","CXP555555_5A","R5A"},
	      {global,"TEST_5A_CXP555555.cxp"}},
	     {{"TEST-5B","CXP555555_5B","R5B"},
	      {global,"TEST_5B_CXP555555.cxp"}}]
       ).
-define(RESULT_6, 
	[{{"TEST-6A","CXP666666_6A","R6A"},
	      {global,"TEST_6A_CXP666666.cxp"}},
	     {{"TEST-6B","CXP666666_6B","R6B"},
	      {global,"TEST_6B_CXP666666.cxp"}}]
       ).
-define(RESULT_7, 
	[{{"TEST-7A","CXP777777_7A","R7A"},
	      {global,"TEST_7A_CXP777777.cxp"}},
	     {{"TEST-7B","CXP777777_7B","R7B"},
	      {global,"TEST_7B_CXP777777.cxp"}}]
       ).
-define(RESULT_8, 
	[{{"TEST-8A","CXP888888_8A","R8A"},
	      {global,"TEST_8A_CXP888888.cxp"}},
	     {{"TEST-8B","CXP888888_8B","R8B"},
	      {global,"TEST_8B_CXP888888.cxp"}}] 
       ).
-define(RESULT_9,
	[{{"TEST-9A","CXP999999_9A","R9A"},
	      {global,"TEST_9A_CXP999999.cxp"}},
	     {{"TEST-9B","CXP999999_9B","R9B"},
	      {global,"TEST_9B_CXP999999.cxp"}}]
       ).
-define(RESULT_10,
	[{{"TEST-10A","CXP101010_10A","R10A"},
	      {global,"TEST_10A_CXP101010.cxp"}},
	     {{"TEST-10B","CXP101010_10B","R10B"},
	      {global,"TEST_10B_CXP101010.cxp"}}]
       ).
-define(RESULT_11,
	[{{"TEST-20A","CXP202020_20A","R20A"},
	      {global,"TEST_20A_CXP202020.cxp"}},
	     {{"TEST-20B","CXP202020_20B","R20B"},
	      {global,"TEST_20B_CXP202020.cxp"}}]
       ).


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [
		 %% {rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_consserv,cs1},
		 {rct_rs232,console},
		 {rct_power,node},
		 {rct_netconf, nc1},
		 {cth_conn_log, []},
		 {rct_logging, {all,
				[{erlang,
				  {["ERROR REPORT","CRASH REPORT"],
				   [?ERROR_CAUSE_1,
				    ?ERROR_CAUSE_2,
				    ?ERROR_CAUSE_3
				   ]}
				 }]}},
		 {rct_rpc, ?RPC},
		 {rct_core,[]},
		 {rct_cli, {cli, [manual_connect]}},
		 %% {rct_scp, scp}
		 {rct_scp, [{1, scp}]}
		]}].

%% @hidden
init_per_suite(Config) ->
    NewConfig = rct_rcsTestServer:start(Config),
    ct:log("### init_per_suite, Config: ~n~p", [NewConfig]),
    %% NewConfig.

    TmpDir = rct_rpc:call(?RPC, sysEnv, tmp_dir, [],5000,print),
    Tmp_Dir = TmpDir++"/",
    ct:pal("# TempDir : ~p", [Tmp_Dir]),
    SimOrTarget = os:getenv("SIM_OR_TARGET"),
    ct:pal("# SIM_OR_TARGET : ~p", [SimOrTarget]),
    [{tmp_dir, Tmp_Dir},
     {sim_or_target, SimOrTarget} | NewConfig].
    %% Config.
%% @hidden
end_per_suite(Config) ->
    rct_rcsTestServer:stop(Config),
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("## ~p ##", [TestCase]),
    cleanup_tmp_dir(Config),
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    cleanup_tmp_dir(Config),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p", [Reason]),
	    ok
	    %% export_ai_log(Config),
	    %% export_esi(Config)
    end,
    ok.


cleanup_tmp_dir(Config) ->
    decupling_lib:
	cleanup_tmp_dir(Config, ?RPC, console).

%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{group_match,[],[
		      test_1_1_1,
		      test_1_1_2,
		      test_1_1_3,
		      
		      test_1_2_1,  
		      test_1_2_2, 
		      test_1_2_3,
		      test_1_2_4,
		      
		      test_2_1_1,
		      test_2_1_2,
		      test_2_1_3,

		      test_2_2_1,
		      test_2_2_2,
		      test_2_2_3,
		      
		      test_3_1_1,
		      test_3_2_1, 
		      test_3_2_2,

		      test_10_1_1,
		      test_10_1_2,
		      test_10_2_1,
		      test_10_2_2,
		      test_10_3_1,
		      test_10_3_2,
		      test_10_5_3
	 ]},

     {group_no_match,[],[
			 test_1_1_10,
			 test_1_1_11,
			 test_1_2_10,
			 test_2_1_10,
			 test_2_2_10,
			 test_2_2_11,
			 test_2_2_12,
			 test_3_3_1,
			 test_10_1_3,
			 test_10_1_4,
			 test_10_1_5,
			 test_10_1_6,
			 test_10_4_1,
			 test_10_4_2,
			 test_10_5_1,
			 test_10_5_2 
			]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [	 
	 test_1_1_1,
	 test_1_1_2,
	 test_1_1_3,
	 test_1_1_10,
	 test_1_1_11,

	 test_1_2_1,
	 test_1_2_2,
	 test_1_2_3,
	 test_1_2_4,
	 test_1_2_10,

	 test_2_1_1,
	 test_2_1_2,
	 test_2_1_3,
	 test_2_1_10,

	 test_2_2_1,
	 test_2_2_2,
	 test_2_2_3,
	 test_2_2_10,
	 test_2_2_11,
	 test_2_2_12,

	 test_3_1_1,
	 test_3_2_1,
	 test_3_2_2,

	 test_10_1_1,
	 test_10_1_2,
	 test_10_1_3,
	 test_10_1_4,
	 test_10_1_5,
	 test_10_1_6,
	 test_10_2_1,
	 test_10_2_2,
	 test_10_3_1,
	 test_10_3_2,
	 test_10_4_1,
	 test_10_4_2,
	 test_10_5_1,
	 test_10_5_2
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 1. Ordinary Product Versions. %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 1_1. Ordinary
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev match an explicit R-state in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_1
%% @spec test_1_1_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_EXACT_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev match an open ended range in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_2
%% @spec test_1_1_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    ct:pal("A"),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_OPEN_RANGE_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_2, CxpList),

    ct:pal("B"),
    CxpListB = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_OPEN_RANGE_1_B),
    ct:pal("Reply B: ~p ", [CxpListB]),
    decupling_lib:match_cxplist(?RESULT_11, CxpListB).


%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev match an closed range in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_3
%% @spec test_1_1_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    ct:pal("A"),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_CLOSED_RANGE_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_3, CxpList),
    

    ct:pal("B"),
    CxpListB = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_CLOSED_RANGE_1_B),
    ct:pal("Reply B: ~p ", [CxpListB]),
    decupling_lib:match_cxplist(?RESULT_11, CxpListB).


%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev NOT match an revision range in XML.
%% - error due to no rev match
%% @spec test_1_1_10(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_10(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_NO_MATCH_1),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev with only digit in XML, shall not be supported.
%% - Result:  error due to no rev match
%% @spec test_1_1_11(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_11(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_ONLY_DIGITS_NO_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.
    %% decupling_lib:match_cxplist(?RESULT_1, CxpList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 1_2. Match verification R-state
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% An Verification Rev state shall match an explicit R-state in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_1
%% @spec test_1_2_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_2_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    %% scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_VER_STATE_EXACT_MATCH_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% An Verification Rev state match an open ended range in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_2
%% @spec test_1_2_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_2_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_VER_STATE_MATCH_OPEN_RANGE_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_2, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% An Verification Rev state match an closed range in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_3
%% @spec test_1_2_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_2_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_VER_STATE_MATCH_CLOSED_RANGE_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_3, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev with only digit match an range in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_3
%% @spec test_1_2_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_2_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_ONLY_DIGITS_MATCH),
    decupling_lib:match_cxplist(?RESULT_3, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Verification Rev NOT match an revision range in XML.
%% - error due to no rev match
%% @spec test_1_2_10(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_2_10(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_VER_STATE_NOT_MATCH_1),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 2. Special Product Versions. %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 2_1. Using numerical suffix
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev match an explicit R-state in XML. Using numerical suffix.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_4
%% @spec test_2_1_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_1_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    ct:pal("A. HW Spcial rev match special rev in global, explicit R-state."),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_EXACT_2),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_4, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev match an open ended range in XML. Using numerical suffix.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_5
%% @spec test_2_1_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_1_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_OPEN_RANGE_2),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_5, CxpList).
    

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev match an closed range in XML. Using numerical suffix.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_6
%% @spec test_2_1_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_1_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_CLOSED_RANGE_2),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_6, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev NOT match an revision range in XML. Using numerical suffix. 
%% - error due to no rev match
%% @spec test_2_1_10(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_1_10(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?SPEC_REV_NOT_MATCH_2),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 2_2. Using alphabetic suffix
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev match an explicit R-state in XML. Using alphabetic suffix.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_7
%% @spec test_2_2_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_2_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    ct:pal("A. HW Spcial rev match special rev in global, explicit R-state."),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_EXACT_3),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_7, CxpList),

    ct:pal("B. HW ordinary rev match special rev in global, explicit R-state."),
    CxpListB = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_EXACT_3_B),
    ct:pal("Reply B: ~p ", [CxpListB]),
    decupling_lib:match_cxplist(?RESULT_7, CxpListB),

    ct:pal("C. HW special rev match ordinary rev in global, explicit R-state."),
    CxpListC = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_EXACT_3_C),
    ct:pal("Reply C: ~p ", [CxpListC]),
    decupling_lib:match_cxplist(?RESULT_1, CxpListC),

    ct:pal("D. HW special rev match ordinary rev in global, explicit R-state as a range."),
    CxpListD = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_EXACT_3_D),
    ct:pal("Reply D: ~p ", [CxpListD]),
    decupling_lib:match_cxplist(?RESULT_7, CxpListD).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev match an open ended range in XML. Using alphabetic suffix.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_8
%% @spec test_2_2_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_2_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    ct:pal("A. HW Spcial rev match special rev in global, open range."),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_OPEN_RANGE_3),
    ct:pal("Reply : ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_8, CxpList),
    
    ct:pal("B. HW ordinary rev match special rev in global, open range."),
    CxpListB = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_OPEN_RANGE_3_B),
    ct:pal("Reply B: ~p ", [CxpListB]),
    decupling_lib:match_cxplist(?RESULT_8, CxpListB),

    ct:pal("C. HW special rev match ordinary rev in global, open range."),
    CxpListC = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_OPEN_RANGE_3_C),
    ct:pal("Reply C: ~p ", [CxpListC]),
    decupling_lib:match_cxplist(?RESULT_2, CxpListC),
    
    ct:pal("D. HW special rev match ordinary rev in global, open range."),
    CxpListD = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_OPEN_RANGE_3_D),
    ct:pal("Reply D: ~p ", [CxpListD]),
    decupling_lib:match_cxplist(?RESULT_8, CxpListD).
    

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev match an closed range in XML. Using alphabetic suffix.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_9
%% @spec test_2_2_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_2_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    ct:pal("A. HW Spcial rev match special rev in global, closed range"),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_CLOSED_RANGE_3),
    ct:pal("Reply : ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_9, CxpList),

    ct:pal("B. HW ordinary rev match special rev in global, closed range"),
    CxpListB = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_CLOSED_RANGE_3_B),
    ct:pal("Reply B: ~p ", [CxpListB]),
    decupling_lib:match_cxplist(?RESULT_9, CxpListB),
    
    ct:pal("C. HW special rev match  ordinary rev in global, closed range"),
    CxpListC = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_CLOSED_RANGE_3_C),
    ct:pal("Reply C: ~p ", [CxpListC]),
    decupling_lib:match_cxplist(?RESULT_3, CxpListC),

    ct:pal("D. HW special rev match  ordinary rev in global, closed range"),
    CxpListD = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_CLOSED_RANGE_3_D),
    ct:pal("Reply D: ~p ", [CxpListD]),
    decupling_lib:match_cxplist(?RESULT_9, CxpListD).
    
    

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Special Rev NOT match an revision range in XML. Using alphabetic suffix.
%% - error due to no rev match
%% @spec test_2_2_10(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_2_10(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?SPEC_REV_NOT_MATCH_3),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Use of function when Alphabetic letters not Allowed in XML.
%% - error due to no rev match
%% @spec test_2_2_11(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_2_11(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?SPEC_REV_NOT_MATCH_4),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Alphabetic letters not Allowed in XML.
%% - error due to no rev match
%% @spec test_2_2_12(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_2_12(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NOK_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?REV_MATCH_EXACT_1),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 3. Preliminar Product Version. %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 3_1. Ordinary
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev match an explicit P-state in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_10
%% @spec test_3_1_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_3_1_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?PREL_REV_MATCH_EXACT),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_10, CxpList).

%%%% 3_2. No match
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Preliminary Rev match an open range in XML.
%% - Result: This will match an open range.
%% @spec test_3_2_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_3_2_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?PREL_REV_MATCH_1),
    decupling_lib:match_cxplist(?RESULT_11, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Preliminary Rev match an closed range in XML.
%% - Result: This will match an range closed range.
%% @spec test_3_2_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_3_2_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?PREL_REV_MATCH_2),
    decupling_lib:match_cxplist(?RESULT_11, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Preliminary Rev not match an closed range in XML.
%% - Result: This will NOT an match an closed range.
%% @spec test_3_3_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_3_3_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?PREL_REV_NOT_MATCH_1),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.


%%%%%%%%%%%%%%%%%%%%
%%%% 10. Misc.  %%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_1. Max allowed Specific R-state range.
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev using max allowed alpabetic letters match an explicit rev in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_1
%% @spec test_10_1_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    MAX_SPECIFIC_1 = {"KDU111111/1","R99ZZZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  MAX_SPECIFIC_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev using max allowed digits match an explicit rev in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_2
%% @spec test_10_1_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    MAX_SPECIFIC_2 = {"KDU111111/1","R9999ZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  MAX_SPECIFIC_2),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_2, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Use function when  Rev exceed max allowed alpabetic letters.
%% - Reslt: error
%% @spec test_10_1_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    NOK_MAX_SPECIFIC_1 = {"KDU111111/1","R9ZZZZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  NOK_MAX_SPECIFIC_1),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Use function when Rev using over max allowed digits.
%% - Reslt: error
%% @spec test_10_1_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    NOK_MAX_SPECIFIC_2 = {"KDU111111/1","R99999Z"},
    CxpList = run_mfa_to_get_cxps(Config,
				  NOK_MAX_SPECIFIC_2),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev exceed max allowed alpabetic letters in XML.
%% - Reslt: error
%% @spec test_10_1_5(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_5(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NOK_2),

    NOK_MAX_SPECIFIC_1 = {"KDU111111/1","R99ZZZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  NOK_MAX_SPECIFIC_1),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev using exceed max allowed digits in XML.
%% - Reslt: error
%% @spec test_10_1_6(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_6(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NOK_2),

    NOK_MAX_SPECIFIC_2 = {"KDU111111/1","R9999ZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  NOK_MAX_SPECIFIC_2),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.


%%%% 10_2. Max allowed open ended R-state range.
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev using max allowed alpabetic letters match an open ended rev in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_1
%% @spec test_10_2_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_2_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    MAX_OPEN_1 = {"KDU111111/2","R99ZZZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  MAX_OPEN_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev using max allowed digits match an open ended rev in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_2
%% @spec test_10_2_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_2_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    MAX_OPEN_2 = {"KDU111111/3","R9999ZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  MAX_OPEN_2),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_2, CxpList).

%%%% 10_3. Max allowed open ended R-state range.
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev using max allowed alpabetic letters match an closed rev in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_1
%% @spec test_10_3_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_3_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    MAX_CLOSED_1 = {"KDU111111/4","R99ZZZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  MAX_CLOSED_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Rev using max allowed digits match an closed rev in XML.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_2
%% @spec test_10_3_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_3_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_MAX_REV_RANGE),

    MAX_CLOSED_1 = {"KDU111111/5","R9999ZZ"},
    CxpList = run_mfa_to_get_cxps(Config,
				  MAX_CLOSED_1),
    ct:pal("Reply: ~p ", [CxpList]),
    decupling_lib:match_cxplist(?RESULT_2, CxpList).

%%%% 10_4. Letter comination.
%% Vowels (A, E, U, Y) are forbidden in letter position 2 and 3
%% for R-states with 3 and 4 letters"
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Vowels (A, E, U, Y) are forbidden in letter position 2 and 3 
%% for R-states with 3 and 4 letters.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RESULT_1
%% @spec test_10_4_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_4_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    LETTER_CHECK_1 = {"KDU111111/1","R40AYA"},
    CxpList1 = run_mfa_to_get_cxps(Config,
				  LETTER_CHECK_1),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList1,

    LETTER_CHECK_2 = {"KDU111111/1","R40BAE"},
    CxpList2 = run_mfa_to_get_cxps(Config,
				  LETTER_CHECK_2),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList2,

    LETTER_CHECK_3 = {"KDU111111/1","R40AAUA"},
    CxpList3 = run_mfa_to_get_cxps(Config,
				  LETTER_CHECK_3),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList3,

    LETTER_CHECK_4 = {"KDU111111/1","R40ZEAZ"},
    CxpList4 = run_mfa_to_get_cxps(Config,
				  LETTER_CHECK_4),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList4.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Not allowed letter combination in XML.
%% - Reslt: error
%% @spec test_10_4_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_4_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NOK_3),

    HW_1 = {"KDU111111/1","R30ZZZZ"},
    CxpList1 = run_mfa_to_get_cxps(Config,
				  HW_1),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList1,
    
    HW_2 = {"KDU111111/2","R30ZZZZ"},
    CxpList2 = run_mfa_to_get_cxps(Config,
				  HW_2),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList2,
    
    HW_3 = {"KDU111111/3","R30ZZZZ"},
    CxpList3 = run_mfa_to_get_cxps(Config,
				  HW_3),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList3,
    
    HW_4 = {"KDU111111/4","R30ZZZZ"},
    CxpList4 = run_mfa_to_get_cxps(Config,
				  HW_4),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList4.

%%%% 10_5. Verification Range
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Use of Verification Rev that is not allowed.
%% - 
%% @spec test_10_5_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_5_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    ct:pal(" # Test 1"),
    HW_1 = {"KDU111111/1","R3A1"},
    CxpList1 = run_mfa_to_get_cxps(Config,
				   HW_1),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList1,

    ct:pal(" # Test 2"),
    HW_2 = {"KDU111111/1","R3A9999"},
    CxpList2 = run_mfa_to_get_cxps(Config,
				   HW_2),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList2.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Verification Rev NOT allowed in XML.
%% - Invalid revision format due to it must consist of two digits.
%% @spec test_10_5_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_5_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NOK_4),

    ct:pal(" # Test 1"),
    HW_1 = {"KDU111111/3","R30A1"},
    CxpList1 = run_mfa_to_get_cxps(Config,
				   HW_1),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList1.


%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. 
%% KDU match.
%% Verification Rev allowed as a range in XML.
%% - Success
%% @spec test_10_5_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_5_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NOK_4),

    ct:pal(" # Test 2"),
    HW_2 = {"KDU111111/1","R30A10"},
    CxpList2 = run_mfa_to_get_cxps(Config,
    				   HW_2),
    decupling_lib:match_cxplist(?RESULT_8, CxpList2),

    ct:pal(" # Test 3"),
    HW_3 = {"KDU111111/2","R30A10"},
    CxpList3 = run_mfa_to_get_cxps(Config,
    				   HW_3),
    decupling_lib:match_cxplist(?RESULT_8, CxpList3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fix_string(Str, Separator) ->
%%     string:tokens(Str, Separator).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% scp_xml
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scp_xml(Config, FROM_XML_PATH, XML_FILE) ->
    decupling_lib:
	scp_xml(Config, FROM_XML_PATH, XML_FILE, ?RPC, scp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% run_mfa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_mfa_to_get_cxps(Config, HWid) ->
    decupling_lib:
	run_mfa_to_get_cxps(Config, HWid).
