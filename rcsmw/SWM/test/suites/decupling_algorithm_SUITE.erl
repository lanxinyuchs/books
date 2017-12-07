%% coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	decupling_algorithm_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/3
%%%
%%% @doc == HW-SW-DEC, verify LMC selection algorithm rule. ==
%%%
%%%
%%% @end

-module(decupling_algorithm_SUITE).
-author('etxivri').
-vsn('/main/R5A/3').
-date('2016-04-14').

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
%%% R5A/1      2016-02-09 etxivri     Created, This test the LMC selection 
%%%                                   algorithm. This test use /tmp dir put
%%%                                   the differents tests Global and Hal XMLs.
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
	 
	 %% Test algorithm when HW match. 
	 test_1_1/1,  %% -> CXP list
	 test_1_2/1,  %% -> CXP list
	 test_1_3/1,  %% -> CXP list
	 test_1_4/1,  %% -> CXP list

	 %% TCs using hwcategory BASEBAND to get RADIO CXPs.
	 %% Note! not affect HAL, check is only in Global.
	 test_1_1_1/1,   %% -> CXP list
	 test_1_1_2/1,   %% -> CXP list
	 test_1_1_3/1,   %% -> CXP list
	 test_1_1_4/1,   %% -> CXP list
	 test_1_1_5/1,   %% -> CXP list
	 test_1_1_6/1,   %% -> CXP list
	 test_1_1_7/1,   %% -> CXP list
	 test_1_1_8/1,   %% -> CXP list
	 test_1_1_10/1,   %% -> error


	 %% Test algorithm when HW NOT match.
	 test_2_1/1,  %% -> error
	 test_2_2/1,  %% -> error

	 %% Test algorithm when needed XML not exist
	 test_3_1/1,  %% -> error
	 test_3_2/1,  %% -> error

	 %% Test algorithm for Index
	 test_4_1/1,  %% -> error
	 test_4_2/1,  %% -> CXP list
	 test_4_3/1,  %% -> CXP list
	 test_4_4/1,  %% -> CXP list
	 test_4_5/1,  %% -> error
	 test_4_6/1,  %% -> error

	 %% Test algorithm for revision, when kdu prodnumber match
	 test_5_1/1,  %% -> error
	 test_5_2/1,  %% -> CXP list
	 test_5_3/1,  %% -> CXP list
 	 %% test_5_4/1,  %% -> This test is obsolete
 	 test_5_5/1,  %% -> error
 	 test_5_6/1,  %% -> error
 	 test_5_7/1,  %% -> error

	 %% TCs using several XMLs
	 test_6_1/1,  %% -> CXP list
	 test_6_2/1,  %% -> CXP list
	 test_6_3/1,  %% -> error
	 test_6_4/1,  %% -> error

	 %% TCs using old legacy XMLs.
	 test_7_1/1,   %% -> CXP list
	 test_7_2/1,   %% -> CXP list

	 %% 10. Misc.
	 %% 10_1. CXP missmatch
	 test_10_1_1/1,  %% -> error
	 test_10_1_2/1,  %% -> error
	 %% 10_2. Boardlist is empty
	 test_10_2_1/1,  %% -> CXP list
	 test_10_2_2/1,  %% -> CXP list
	 test_10_2_3/1,  %% -> error
	 %% 10_3. combination of Index and Boardlists not exist 
	 test_10_3_1/1,  %% -> error
	 test_10_3_2/1,  %% -> error
	 test_10_3_3/1,  %% -> CXP list
	 test_10_3_4/1,  %% -> CXP list
	 test_10_3_5/1,  %% -> CXP list
	 %% 10_4. Mal format or not correct syntax in XML
	 test_10_4_1/1,  %% -> error
	 test_10_4_2/1,  %% -> error
	 test_10_4_3/1,  %% -> error
	 test_10_4_4/1,  %% -> error
	 %% 10_5. Multiple of HW match.
	 test_10_5_1/1,  %% -> error
	 test_10_5_2/1,  %% -> error
	 %% 10_6. No products exist
	 test_10_6_1/1, %% -> error
	 test_10_6_2/1, %% -> error
	 test_10_6_3/1, %% -> CXP list
	 test_10_6_4/1, %% -> error
	 %% 10_7. Wrong use of function.
	 test_10_7_1/1, %% -> error
	 test_10_7_2/1, %% -> error
	 test_10_7_3/1 %% -> error
	]).

%% This is needed, otherwise error TC will result in ERROR in ct-shell.
-define(RPC, rct_rcsTestServer:ct_hook(rct_rpc)). 
%% -define(RPC, rpc).

-define(XML_PATH_1, "/proj/rcs-tmp/hw_sw_dec/xml_for_algorithm_test/selection_algorithm/new_1/").


%% HW id.
-define(HW_ID_MATCH, {"KDU123456/99","R31A"}).
-define(HW_ID_MATCH_2, {"KDU123456/99","R2B"}).
-define(HW_MATCH_ONLY_HAL, {"KDU123457/88","R31A"}).
-define(HW_MATCH_ONLY_GLOB, {"KDU123456/77","R4"}).
-define(KDU_MATCH_ONLY_GLOB_TO_HIGH_REV, {"KDU123456/77","R41B"}). 
-define(HW_ID_NOT_MATCH, {"KDU654321/1","R3A"}).
-define(KDU_OK_REV_OK_HAL, {"KDU111222/100","R3A"}).
-define(KDU_OK_REV_OK_GLOBAL, {"KDU111222/100","R6A"}).
-define(HW_OK_HALCXP_NOK, {"KDU444444/1","R5A"}).
-define(HW_NOK_HALCXP_NOK, {"KDU555555/1","R2A"}).

%% -define(CHECK_COMPAT_REV_GLOB, {"KDU123456/77","R40CD/r"}).
-define(CHECK_COMPAT_REV_HAL, {"KDU123457/88","R31R_1o"}).
-define(CHECK_COMPAT_REV_MATCH_HAL_GLOB, {"KDU123456/99","R40C.*j"}).
-define(CHECK_REV_LENGTH_NOK_HAL_GLOB, {"KDU123456/99","R40C*/_j"}).
-define(KDU_REV_NOT_A_STR, {"KDU123456/99", r31a}).


%% Globla UP xml.
-define(GLOBAL_XML_1, "GlobalUP_1-up.xml").
-define(GLOBAL_XML_2, "GlobalUP_2-up.xml").
-define(GLOBAL_XML_3, "GlobalUP_3-up.xml").
-define(GLOBAL_XML_NO_HW, "GlobalUP_4-up.xml").
-define(GLOBAL_XML_CHECK_INDEX, "GlobalUP_5-up.xml").

-define(NOK_GLOBAL_XML_1, "NokGlobal_1-up.xml").
-define(NOK_GLOBAL_XML_2, "NokGlobal_2-up.xml").
-define(NOK_GLOBAL_XML_3, "NokGlobal_3-up.xml").
-define(NOK_GLOBAL_XML_4, "NokGlobal_4-up.xml").
-define(NOK_GLOBAL_XML_5, "NokGlobal_5-up.xml").
-define(NOK_GLOBAL_XML_6, "NokGlobal_6-up.xml").


%% test hwcategory
-define(GLOBAL_HWCAT_1, "GlobalUP_hwcat_1-up.xml").
-define(GLOBAL_HWCAT_2, "GlobalUP_hwcat_2-up.xml").
-define(GLOBAL_HWCAT_NOK_1, "GlobalUP_hwcat_NOK-up.xml").
-define(HAL_HWCAT_1, "Hal_hwcat_1-hal.xml").


%% HAL SWP xml.
-define(HAL_XML_1, "HalUP_1-hal.xml").
-define(HAL_XML_2_INDEX_MISSMATCH, "HalUP_2-hal.xml").
-define(HAL_XML_3_REV_MISSMATCH, "HalUP_3-hal.xml").
-define(HAL_XML_4_INDEX_MISSMATCH, "HalUP_4-hal.xml").
-define(HAL_XML_5_CHECK_INDEX, "HalUP_5-hal.xml").
-define(NOK_HAL_XML_1, "NOK_1-hal.xml").
-define(NOK_HAL_XML_2, "NOK_2-hal.xml").
-define(NOK_HAL_XML_3, "NOK_3-hal.xml").
-define(NOK_HAL_XML_4, "NOK_4-hal.xml").


%% Legacy UP xml.
-define(LEGACY_XML_1, "cxs101549_5-up.xml").


%% Usecase Error
-define(UC_ERR_2(Reason), {uc_error, Reason}).

%% Error causes.
-define(ERROR_CAUSE_1, "No GlobalUP found").
-define(ERROR_CAUSE_2, "BoardType not supported by neither HalUP nor GlobalUP").
-define(ERROR_CAUSE_3, "More than one GlobalUP found").
-define(ERROR_CAUSE_4, "Duplicate index in Hal UP").
%% -define(ERROR_CAUSE_5, "Product not found in Global UP").
-define(ERROR_CAUSE_5, "Product not found in selected boardLists in Global UP").
-define(ERROR_CAUSE_6, "Missing hwSwCompatibility Index in Hal UP file").
-define(ERROR_CAUSE_7, "Missing hwSwCompatibility Index in Global UP").
-define(ERROR_CAUSE_8, "Mandatory information missing").
-define(ERROR_CAUSE_9, "BoardType defined multiple times").
-define(ERROR_CAUSE_10, "Empty product list in hal UP").
-define(ERROR_CAUSE_11, "Empty product list in global UP").
-define(ERROR_CAUSE_12, "Revision length out of range").
-define(ERROR_CAUSE_13, "Multiple hwSwCompatibility Index found in Global UP file").
-define(ERROR_CAUSE_14, "Multiple hwSwCompatibility Index found in Hal UP file").
-define(ERROR_CAUSE_15, "Parsing fault").
%% -define(ERROR_CAUSE_16, "More than one BoardType specified for Baseband").
-define(ERROR_CAUSE_17, "Revision is not a string").
-define(ERROR_CAUSE_18, "Unrecognized argument").
-define(ERROR_CAUSE_19, "No product list found in Global UP").
-define(ERROR_CAUSE_20, "Invalid revision range").
-define(ERROR_CAUSE_21, "Invalid revision format").


%% Result to match when only Global exist and HW is match
%% CXPs from Global/boardlist shall be used.
-define(RES_TEST_1, 
	 [{{"DUMMY","CXP111111_2","R60D"},
	   {global,"BL_DUMMY_CXP111111_2-R60D.cxp"}},
	  {{"RRU22F1LMC","CXP9026642_1Z2","R60N"},
	   {global,"BL_g2_rru22f1_app-CXP9026642_1Z2-R1N.cxp"}},
	  {{"COBRA-A10","CXP102188_2","R60A02"},
	   {global,"BL_COBRA_CXP102188_2.cxp"}},
	  {{"RCS-DUS2","CXP9031275_3","R60K12"},
	   {global,"BL_RCS-DUS2_CXP9031275_3.cxp"}},
	  {{"APC-ARM","CXP9024886_3","R60CJ"},
	   {global,"BL_APC-ARM_CXP9024886_3-R22CJ.cxp"}},
	  {{"FRUM","CXP9024280_4","R60A"},
	  {global,"BL_FRUM_CXP9024280_4-R33A.cxp"}},
	  {{"TAIPAN","CXP102172_2","R60Z"},
	   {global,"TAIPAN_CXP102172_2.cxp"}},
	  {{"NOTINHAL","CXP9024280_99","R60D"},
	   {global,"BL_NOTINHAL_CXP9024280_99-R60D.cxp"}}]
       ).

%% Result to match when HAL and Global exist, 
%% Index and HW match both in HAL and Global.
%% CXPs from Hal and Global/boardlist shall be used.
-define(RES_TEST_2, 
	 [{{"COBRA-A10","CXP102188_2","R60A02"},
	   {global,"BL_COBRA_CXP102188_2.cxp"}},
	  {{"APC-ARM","CXP9024886_3","R10A"},
	   {hal,"APC-ARM_CXP9024886_3-R10A.cxp"}},
	  {{"RRU22F1LMC","CXP9026642_1Z2","R60N"},
	   {global,"BL_g2_rru22f1_app-CXP9026642_1Z2-R1N.cxp"}},
	  {{"RCS-DUS2","CXP9031275_3","R10B"},
	   {hal,"RCS-DUS2_CXP9031275_3.cxp"}},
	  {{"FRUM","CXP9024280_4","R60A"},
	   {global,"BL_FRUM_CXP9024280_4-R33A.cxp"}},
	  {{"TAIPAN","CXP102172_2","R60Z"},
	   {global,"TAIPAN_CXP102172_2.cxp"}}]
       ).

%% Result to match when HAL and Global exist, 
%% Rev and HW match in HAL, But HW Not match in Global.
%% CXPs from Hal and Global/contentinfo shall be used.
-define(RES_TEST_3, 
	 [{{"COBRA-A10","CXP102188_2","R500N"},
	   {global,"CI_COBRA_CXP102188_2.cxp"}},
	  {{"APC-ARM","CXP9024886_3","R10A"},
	   {hal,"APC-ARM_CXP9024886_3-R10A.cxp"}},
	  {{"RRU22F1LMC","CXP9026642_1Z2","R500A"},
	   {global,"gCI_2_rru22f1_app-CXP9026642_1Z2-R111N.cxp"}},
	  {{"RCS-DUS2","CXP9031275_3","R10B"},
	   {hal,"RCS-DUS2_CXP9031275_3.cxp"}},
	  {{"FRUM","CXP9024280_4","R500YY"},
	   {global,"CI_FRUM_CXP9024280_4-R555A.cxp"}},
	  {{"TAIPAN","CXP102172_2","R500ZZ"},
	   {global,"CI_TAIPAN_CXP102172_2.cxp"}}]
	).


%% Result to match when HAL and Global exist, 
%% Rev match in HAL and Global, But HW Not match in HAL.
%% CXPs from Global/boardlist shall be used.
-define(RES_TEST_4, 
	 [{{"DUMMY_1","CXP111111_1","R01D"},
	   {global,"DUMMY_CXP111111_1.cxp"}},
	  {{"DUMMY_2","CXP111111_2","R02D"},
	   {global,"DUMMY_CXP111111_2.cxp"}},
	  {{"DUMMY_3","CXP111111_3","R03D"},
	   {global,"DUMMY_CXP111111_3.cxp"}}]
       ).


%% %% Result when old legacy UP is used
%% Result an old legacy UP xml is used. 
%% No index, no HW exist.
%% CXPs from Global/contentlist shall be used.
-define(RES_LEGACY_1, 
	 [{{"COBRA","CXP102171_1","R21B01"},
	   {global,"COBRA_CXP102171_1.cxp"}},
	  {{"COBRA-A10","CXP102188_2","R10B01"},
	   {global,"COBRA_CXP102188_2.cxp"}},
	  {{"DUMMY-ARM","CXP9021691_3","R5C13"},
	   {global,"DUMMY-ARM_CXP9021691_3.cxp"}},
	  {{"RCS-DUS2","CXP9031275_3","R5C13"},
	   {global,"RCS-DUS2_CXP9031275_3.cxp"}}]
       ).

%% hwcategory check get RADIO CXPs.
%% Only, Global is used
-define(RES_HWCAT_CHECK_GLOB_1,
	[{{"FRUM","CXP9024280_4","R60A"},
	  {global,"BL_FRUM_CXP9024280_4-R33A.cxp"}},
	 {{"RCS-DUS2","CXP9031275_3","R60K12"},
	  {global,"BL_RCS-DUS2_CXP9031275_3.cxp"}},
	 {{"DUMMY","CXP111111_2","R60D"},
	  {global,"BL_DUMMY_CXP111111_2-R60D.cxp"}},
	 {{"RU11","CXP111111_11","R11A"},{global,"RU11_CXP111111.cxp"}},
	 {{"RU22","CXP222222_22","R22A"},{global,"RU22_CXP222222.cxp"}},
	 {{"RU33","CXP333333_33","R33D"},{global,"RU33_CXP333333.cxp"}},
	 {{"RU44","CXP444444_44","R44E"},{global,"RU23_CXP444444.cxp"}}]
       ).

%% Global and HAL is used, 
%% Global CXPs from boardtype
-define(RES_HWCAT_CHECK_GLOB_HAL_1,
	[{{"FRUM","CXP9024280_4","R60A"},
	  {hal,"BL_FRUM_CXP9024280_4-R33A.cxp"}},
	 {{"RCS-DUS2","CXP9031275_3","R60K12"},
	  {global,"BL_RCS-DUS2_CXP9031275_3.cxp"}},
	 {{"DUMMY","CXP111111_2","R60D"},
	  {hal,"BL_DUMMY_CXP111111_2-R60D.cxp"}},
	 {{"RU11","CXP111111_11","R11A"},{global,"RU11_CXP111111.cxp"}},
	 {{"RU22","CXP222222_22","R22A"},{global,"RU22_CXP222222.cxp"}},
	 {{"RU33","CXP333333_33","R33D"},{global,"RU33_CXP333333.cxp"}},
	 {{"RU44","CXP444444_44","R44E"},{global,"RU23_CXP444444.cxp"}}]
       ).

%% Global and HAL is used, 
%% Global CXPs from boardtype and contentinfo
-define(RES_HWCAT_CHECK_GLOB_HAL_2,
	[{{"FRUM","CXP9024280_4","R60A"},
	  {hal,"BL_FRUM_CXP9024280_4-R33A.cxp"}},
	 {{"RCS-DUS2","CXP9031275_3","R500U"},
	  {global,"CI_RCS-DUS2_CXP9031275_3.cxp"}},
	 {{"DUMMY","CXP111111_2","R60D"},
	  {hal,"BL_DUMMY_CXP111111_2-R60D.cxp"}},
	 {{"RU11","CXP111111_11","R11A"},{global,"RU11_CXP111111.cxp"}},
	 {{"RU22","CXP222222_22","R22A"},{global,"RU22_CXP222222.cxp"}},
	 {{"RU33","CXP333333_33","R33D"},{global,"RU33_CXP333333.cxp"}},
	 {{"RU44","CXP444444_44","R44E"},{global,"RU23_CXP444444.cxp"}},
	 {{"RU55","CXP444444_55","R500AA"},{global,"CI_RU55_CXP555555.cxp"}}]
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
				    ?ERROR_CAUSE_3,
				    ?ERROR_CAUSE_4,
				    ?ERROR_CAUSE_5,
				    ?ERROR_CAUSE_6,
				    ?ERROR_CAUSE_7,
				    ?ERROR_CAUSE_8,
				    ?ERROR_CAUSE_9,
				    ?ERROR_CAUSE_10,
				    ?ERROR_CAUSE_11,
				    ?ERROR_CAUSE_12,
				    ?ERROR_CAUSE_13,
				    ?ERROR_CAUSE_14,
				    "whitespace_required_between_attributes",
				    %% ?ERROR_CAUSE_15,
				    %% ?ERROR_CAUSE_16,
				    ?ERROR_CAUSE_17,
				    ?ERROR_CAUSE_18,
				    ?ERROR_CAUSE_19,
				    ?ERROR_CAUSE_20,
				    ?ERROR_CAUSE_21
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
    [
     {group_match,[],[
		      test_1_1,
		      test_1_2,
		      test_1_3,
		      test_1_4,
		      
		      test_1_1_1,
		      test_1_1_2,
		      test_1_1_3,
		      test_1_1_4,
		      test_1_1_5,
		      test_1_1_6,
		      test_1_1_7,
		      test_1_1_8,

		      test_4_2,
		      test_4_3,
		      test_4_4,

		      test_5_2,
		      test_5_3,

		      test_6_1,
		      test_6_2,
		      
		      test_7_1,
		      test_7_2,

		      test_10_2_1,
		      test_10_2_2,
		      test_10_3_3,
		      test_10_3_4,
		      test_10_3_5,
		      test_10_6_3
		 ]},

     {group_no_match,[],[
		      test_1_1_10,

		      test_2_1,
		      test_2_2,

		      test_3_1,
		      test_3_2,
		      
		      test_4_1,

		      test_4_5,
		      test_4_6,
		      
		      test_5_1,
		      
		      test_5_5,
		      test_5_6,
		      test_5_7,
		      
		      test_6_3,
		      test_6_4,
		      
		      test_10_1_1,
		      test_10_1_2,
		      test_10_2_3,
		      test_10_3_1,
		      test_10_3_2,
		      test_10_4_1,
		      test_10_4_2,
		      test_10_4_3,
		      test_10_4_4,
		      test_10_5_1,
		      test_10_5_2,
		      test_10_6_1,
		      test_10_6_2,
		      test_10_6_4,
		      test_10_7_1,
		      test_10_7_2,
		      test_10_7_3
		     ]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [	 
	 test_1_1,
	 test_1_2,
	 test_1_3,
	 test_1_4,

	 test_1_1_1,
	 test_1_1_2,
	 test_1_1_3,
	 test_1_1_4,
	 test_1_1_5,
	 test_1_1_6,
	 test_1_1_7,
	 test_1_1_8,
	 test_1_1_10,

	 test_2_1,
	 test_2_2,

	 test_3_1,
	 test_3_2,

	 test_4_1,
	 test_4_2,
	 test_4_3,
	 test_4_4,
	 test_4_5,
	 test_4_6,

	 test_5_1,
	 test_5_2,
	 test_5_3,
	 %% test_5_4, Obsolete
	 test_5_5,
	 test_5_6,
	 test_5_7,

	 test_6_1,
	 test_6_2,
	 test_6_3,
	 test_6_4,

	 test_7_1,
	 test_7_2,

	 test_10_1_1,
	 test_10_1_2,
	 test_10_2_1,
	 test_10_2_2,
	 test_10_2_3,
	 test_10_3_1,
	 test_10_3_2,
	 test_10_3_3,
	 test_10_3_4,
	 test_10_3_5,
	 test_10_4_1,
	 test_10_4_2,
	 test_10_4_3,
	 test_10_4_4,
	 test_10_5_1,
	 test_10_5_2,
	 test_10_6_1,
	 test_10_6_2,
	 test_10_6_3,
	 test_10_6_4,
	 test_10_7_1,
	 test_10_7_2,
	 test_10_7_3
    ].
	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 1. Test algorithm when HW match. %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist. Hw Id match.
%% - CXPs from Global - boardType is used. 
%% - Expected CXPs = ?RES_TEST_1
%% @spec test_1_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    %% ?RES_TEST_1 = CxpList.
    ok = match_cxplist(?RES_TEST_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in both HAL and Global match.
%% Hw Id match in both HAL and Global.
%% - CXPs from HAL with revison will be used.
%% - CXPs from HAL with * will use CXPs from Global - boardType is used.
%% -- CXPs in HAL with * shall exist in  Global - boardType.
%% - Expected CXPs = ?RES_TEST_2
%% @spec test_1_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_2, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in both HAL and Global match.
%% Hw Id match only in HAL. KDU missmatch in Global
%% - CXPs from HAL with revison will be used.
%% - CXPs from HAL with * will use CXPs from Global - contentinfo is used.
%% -- CXPs in HAL with * shall exist in  Global - contentinfo.
%% - Expected CXPs = ?RES_TEST_3
%% @spec test_1_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_MATCH_ONLY_HAL),
    match_cxplist(?RES_TEST_3, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in both HAL and Global match.
%% Hw Id match only in Global.
%% - CXPs from Global - boardType shall be used.
%% - Expected CXPs = ?RES_TEST_4
%% @spec test_1_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_MATCH_ONLY_GLOB),
    match_cxplist(?RES_TEST_4, CxpList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 1_1. hwcategory BASEBAND get RADIO lmc  %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% Only Global exist
%% index exist.
%% HW match and exist in hwcategory BASEBAND.
%% Two hwcategory="RADIO" exist with lmc.
%% - CXPs from boardtype and RADIO shall be used.
%% @spec test_1_1_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ct:pal("CxpList: ~p", [CxpList]),
    match_cxplist(?RES_HWCAT_CHECK_GLOB_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in both HAL and Global match.
%% Hw Id match in both HAL and Global.
%% HW match and exist in hwcategory BASEBAND in Global.
%% hwcategory="RADIO" exist with lmc in Global.
%% RU CXPs exist in HAL with wildcards
%% - CXPs from HAL with revison will be used.
%% - CXPs from HAL with * will use CXPs from Global - boardType is used.
%% - RU CXPs from RADIO shall be used.
%% - Expected CXPs = ?RES_TEST_2
%% @spec test_1_1_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_HWCAT_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ct:pal("CxpList: ~p", [CxpList]),
    match_cxplist(?RES_HWCAT_CHECK_GLOB_HAL_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in both HAL and Global match.
%% Hw Id match only in HAL. KDU missmatch in Global
%% HW match and exist in hwcategory BASEBAND in HAL.
%% hwcategory="RADIO" exist with lmc in Global.
%% RU CXPs exist in HAL with wildcards
%% - CXPs from HAL with revison will be used.
%% - CXPs from HAL with * will use CXPs from Global - boardType or contentinfo.
%% - RU CXPs from RADIO shall be used.
%% - Expected CXPs = ?RES_TEST_2
%% @spec test_1_1_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_HWCAT_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_MATCH_ONLY_HAL),
    ct:pal("CxpList: ~p", [CxpList]),
    match_cxplist(?RES_HWCAT_CHECK_GLOB_HAL_2, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in both HAL and Global match.
%% Hw Id match only in Global. KDU missmatch in HAL
%% HW match and exist in hwcategory BASEBAND in Global.
%% hwcategory="RADIO" exist with lmc in Global.
%% - CXPs from Global - boardType is used.
%% - RU CXPs from RADIO shall be used.
%% - Expected CXPs = ?RES_TEST_2
%% @spec test_1_1_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_1_1_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_HWCAT_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_MATCH_ONLY_GLOB),
    match_cxplist(?RES_HWCAT_CHECK_GLOB_1, CxpList).

%% Global exist. hw match BASEBAND-T
test_1_1_5(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_1),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH_2),
    match_cxplist(?RES_TEST_4, CxpList).

%% KRC match
test_1_1_6(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_1),
    RU_HW = {"KRC444444/1","R2B"},
    CxpList = run_mfa_to_get_cxps(Config,
    				  RU_HW),
    RES_RU_MATCH = 
	[{{"RU44","CXP444444_44","R44E"},{global,"RU23_CXP444444.cxp"}},
	 {{"RU33","CXP333333_33","R33D"},{global,"RU33_CXP333333.cxp"}},
	 {{"RU22","CXP222222_22","R21A"},{global,"RU22_CXP222222.cxp"}},
	 {{"RU11","CXP111111_11","R9A"},{global,"RU11_CXP111111.cxp"}}],
	match_cxplist(RES_RU_MATCH, CxpList).


%% KDU and one KRC
%% swmBoardList:products({{"KDU137624/1","R1A"},
%% [{ "KRC11866/2", "R1A"}]}, {all, ”/tmp”}).
test_1_1_7(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_2),
    RU_HW_1 = {"KRC111111/1","R2B"},
    CxpList = run_mfa_to_get_cxps(Config,
    				  {?HW_ID_MATCH, [RU_HW_1]}),
    RES_RU_MATCH = 
	[{{"FRUM","CXP9024280_4","R60A"},
	  {global,"BL_FRUM_CXP9024280_4-R33A.cxp"}},
	 {{"RCS-DUS2","CXP9031275_3","R60K12"},
	  {global,"BL_RCS-DUS2_CXP9031275_3.cxp"}},
	 {{"DUMMY","CXP111111_2","R60D"},
	  {global,"BL_DUMMY_CXP111111_2-R60D.cxp"}},
	 {{"RU11","CXP111111_11","R11A"},{global,"RU11_CXP111111.cxp"}},
	 {{"RU22","CXP222222_22","R22A"},{global,"RU22_CXP222222.cxp"}}],
	match_cxplist(RES_RU_MATCH, CxpList).


%% KDU and two KRC
%% swmBoardList:products({{"KDU123456/99","R31A"},
%% [{"KRC111111/1","R2B"}, {"KRC444444/1","R2B"}]}, {all, ”/tmp”}).
test_1_1_8(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_2),
    RU_HW_1 = {"KRC111111/1","R2B"},
    RU_HW_2 = {"KRC444444/1","R2B"},
    CxpList = run_mfa_to_get_cxps(Config,
    				  {?HW_ID_MATCH, [RU_HW_1, RU_HW_2]}),
    RES_RU_MATCH = 
	[{{"FRUM","CXP9024280_4","R60A"},
	  {global,"BL_FRUM_CXP9024280_4-R33A.cxp"}},
	 {{"RCS-DUS2","CXP9031275_3","R60K12"},
	  {global,"BL_RCS-DUS2_CXP9031275_3.cxp"}},
	 {{"DUMMY","CXP111111_2","R60D"},
	  {global,"BL_DUMMY_CXP111111_2-R60D.cxp"}},
	 {{"RU11","CXP111111_11","R11A"},{global,"RU11_CXP111111.cxp"}},
	 {{"RU22","CXP222222_22","R22A"},{global,"RU22_CXP222222.cxp"}},
	 {{"RU33","CXP333333_33","R33D"},{global,"RU33_CXP333333.cxp"}},
	 {{"RU44","CXP444444_44","R44E"},{global,"RU23_CXP444444.cxp"}}],
	match_cxplist(RES_RU_MATCH, CxpList).


%% No cxp exist in RADIO
test_1_1_10(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_HWCAT_NOK_1),
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH_2),
    ?UC_ERR_2(?ERROR_CAUSE_11) = CxpList.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 2. Test algorithm when HW NOT match. %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global match.
%% Hw Id Not exist in neither in HAL and Global.
%% - No CXP shall be used
%% @spec test_2_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_NOT_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global UP exist.
%% Hw Id Not in Global.
%% - No CXP shall be used
%% @spec test_2_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_2_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_NOT_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 3. Test algorithm when needed XML not exist. %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% Only HAL exist.
%% Hw Id match in HAL.
%% - No CXP shall be used
%% @spec test_3_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_3_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% No HAL, No Global exist.
%% - No CXP shall be used
%% @spec test_3_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_3_2(Config) ->
    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_1) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 4. Test algorithm for Index  %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global Not match.
%% Hw Id match only in HAL.
%% - No CXP shall be used
%% @spec test_4_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_4_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_2_INDEX_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_MATCH_ONLY_HAL),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global missmatch.
%% Hw Id match only in Global.
%% - CXPs from Global - boardtype shall be used.
%% @spec test_4_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_4_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_2),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_2_INDEX_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_MATCH_ONLY_GLOB),
    match_cxplist(?RES_TEST_4, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global missmatch on capital letter.
%% R19A in HAL, R19AA in Global.
%% Hw Id match both im HAL and Global.
%% - CXPs from Global - boardtype shall be used.
%% @spec test_4_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_4_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_2),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_2_INDEX_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_1, CxpList).
%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global missmatch on capital letter.
%% R19AA in HAL, R19A in Global.
%% Hw Id match both im HAL and Global.
%% - CXPs from Global - boardtype shall be used.
%% @spec test_4_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_4_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_3),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_4_INDEX_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Mltiple Index exist in Global.
%% Hw Id match both im HAL and Global.
%% - error
%% @spec test_4_5(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_4_5(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_4),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_13) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Mltiple Index exist in HAL.
%% Hw Id match both im HAL and Global.
%% - error
%% @spec test_4_6(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_4_6(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_4),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_14) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 5. Test algorithm for revision, when kdu prodnumber match %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global match.
%% KDU match only in Global, but revision is to high.
%% Note! revision check is to first numbers, everything after shall be backward
%%       compatible, ie R40B is compatible to R40A.
%% - No CXP shall be used.
%% @spec test_5_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_5_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?KDU_MATCH_ONLY_GLOB_TO_HIGH_REV),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global match.
%% KDU match both HAL and Global, but revision match only in HAL.
%% - CXPs from HAL with revison will be used.
%% - CXPs in HAL with * shall exist in  Global - contentinfo.
%% - Expected CXPs = ?RES_TEST_3
%% @spec test_5_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_5_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_3_REV_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?KDU_OK_REV_OK_HAL),
    match_cxplist(?RES_TEST_3, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% HAL and Global UP exist. 
%% Index in HAL and Global match.
%% KDU match both HAL and Global, but revision match only in Global.
%% - CXPs from Global - boardType is used.
%% @spec test_5_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_5_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_3_REV_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?KDU_OK_REV_OK_GLOBAL),
    match_cxplist(?RES_TEST_4, CxpList).

%%%%%% This test is not valid.
%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Only Global UP exist. 
%% %% Index exist.
%% %% KDU match only in Global, but revision shall be backwardcompatible.
%% %% - CXPs from Gloabal - boardType is used.
%% %% @spec test_5_4(Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% test_5_4(Config) ->
%%     scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

%%     CxpList = run_mfa_to_get_cxps(Config,
%%     				  ?CHECK_COMPAT_REV_GLOB),
%%     ?RES_TEST_4 = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Hal and Global UP exist. 
%% Index match.
%% KDU match, check that complex revision not match in HAL.
%% - Result: - error due to invalid format
%% @spec test_5_5(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_5_5(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?CHECK_COMPAT_REV_HAL),
    %% ?RES_TEST_3 = CxpList.
    ?UC_ERR_2(?ERROR_CAUSE_21) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Hal and Global UP exist. 
%% Index match.
%% KDU match, check that a complex revision not match in HAL and Global.
%% - Result: - error due to invalid format
%% @spec test_5_6(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_5_6(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?CHECK_COMPAT_REV_MATCH_HAL_GLOB),
    %% ?RES_TEST_2 = CxpList.
    ?UC_ERR_2(?ERROR_CAUSE_21) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Hal and Global UP exist. 
%% Index match.
%% KDU match, check when revision length is out of range in HAL and Global.
%% - Result: - error due to revision length is out of range.
%% @spec test_5_7(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_5_7(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?CHECK_REV_LENGTH_NOK_HAL_GLOB),
    ?UC_ERR_2(?ERROR_CAUSE_12) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 6. TCs using several XMLs %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% 3 HAL UPs exist. One Global Exist.
%% Index in Global match one of the HAL.
%% HW match all HAL UPs and the Global.
%% - CXPs from HAL with revison will be used.
%% - CXPs from HAL with * will use CXPs from Global - boardType is used.
%% -- CXPs in HAL with * shall exist in  Global - boardType.
%% - Expected CXPs = ?RES_TEST_2
%% @spec test_6_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_6_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_2_INDEX_MISSMATCH),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_4_INDEX_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_2, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% 3 HAL UPs exist. One Global Exist 
%% Index in Global match one of the HAL.
%% HW match only one of the HAL UPs.
%% - CXPs from HAL with revison will be used.
%% - CXPs from HAL with * will use CXPs from Global - contentinfo is used.
%% -- CXPs in HAL with * shall exist in  Global - contentinfo.
%% - Expected CXPs = ?RES_TEST_3
%% @spec test_6_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_6_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_2_INDEX_MISSMATCH),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_4_INDEX_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_MATCH_ONLY_HAL),
    match_cxplist(?RES_TEST_3, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Two Global UPs exist. 
%% Different Index.
%% HW match in both Global UPs
%% - No CXPs shall be used.
%% @spec test_6_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_6_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_2),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_3) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Two HAL exist. One Global UP exist.
%% Same Index in all XMLs.
%% - No CXPs shall be used.
%% @spec test_6_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_6_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_3_REV_MISSMATCH),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_4) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 7. Legacy UP xml %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% Only an old legacy UP exist.
%% No index exist.
%% No HW exist.
%% - CXPs from contentlist shall be used.
%% @spec test_7_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_7_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?LEGACY_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_LEGACY_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% Hal exist and old legacy UP exist.
%% Index and HW exist in HAL.
%% No index exist and No HW exist in old legacy up.
%% - CXPs from contentlist shall be used.
%% @spec test_7_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_7_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?LEGACY_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_LEGACY_1, CxpList).


%%%%%%%%%%%%%%%%%%
%%%% 10. Misc %%%%
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_1. CXP missmatch
%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index in HAL and Global match.
%% Hw match both HAL and Global.
%% Expexted CXP is missing in Global - boardtype.
%% - error due to exp CXP not found.
%% @spec test_10_1_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_OK_HALCXP_NOK),
    ?UC_ERR_2(?ERROR_CAUSE_5) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index in HAL and Global match.
%% Hw match only HAL.
%% Expected CXP is missing in Global - contentlist.
%% - error due to exp CXP not found.
%% @spec test_10_1_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_1_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_NOK_HALCXP_NOK),
    ?UC_ERR_2(?ERROR_CAUSE_5) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_2. Boardlist is empty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index in HAL and Global match.
%% Hw match only HAL. 
%% No HW exist in Global. boardlist is empty in Global.
%% Result: - CXPs from HAL with fixed revison will be used.
%%         - CXPs in HAL with * shall exist in  Global - contentinfo.
%%         - ?RES_TEST_3
%% @spec test_10_2_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_2_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NO_HW),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_3, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index in HAL and Global match.
%% Hw match only Global. boardlist is empty in HAL.
%% Result: - CXPs from Global - boardType is used. 
%%         - ?RES_TEST_1
%% @spec test_10_2_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_2_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index in HAL and Global match.
%% boardlist is empty in HAL and Global
%% Result: - ERROR_CAUSE_2
%%        
%% @spec test_10_2_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_2_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_NO_HW),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_2) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_3. combination of Index and Boardlists not exist 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% No index exist in HAL, no boardlists in HAL. (an legacy UP in HAL).
%% Hw match only Global.
%% Result: - error
%% @spec test_10_3_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_3_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_2),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_6) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index exist in HAL. Index Not exist Global.
%% Result: - error
%% @spec test_10_3_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_3_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_3),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_7) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global exist.
%% Index exist with complex format.
%% HW match.
%% Result: - CXPs from Global - boardtype shall be used.
%% @spec test_10_3_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_3_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_CHECK_INDEX),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index exist with complex format in Global.
%% Index in HAL is not complex. It will not match the Global Index.
%% HW match.
%% Result: - CXPs from Global - boardtype shall be used.
%% @spec test_10_3_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_3_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_CHECK_INDEX),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_1, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Complex Index match Global and HAL.
%% HW match.
%% Result: - CXPs from HAL with fixed revison will be used.
%%         - CXPs in HAL with * shall exist in  Global - boardtype.
%% @spec test_10_3_5(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_3_5(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_CHECK_INDEX),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_5_CHECK_INDEX),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    match_cxplist(?RES_TEST_2, CxpList).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_4. Mal format XML
%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index in global is malformed, missing value index.
%% Result: - error     
%% @spec test_10_4_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_4_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_8) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index and HW match.
%% Index in global is malformed, missing space between productNumber and revision.
%% Result: - error     
%% @spec test_10_4_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_4_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_5),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_15) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global exist.
%% Index exist HW match in Global.
%% No contentinfo or boartlist with products for HW. 
%% Result: - error     
%% @spec test_10_4_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_4_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_6),

    CxpList = run_mfa_to_get_cxps(Config,
				  ?HW_MATCH_ONLY_HAL),
    ?UC_ERR_2(?ERROR_CAUSE_19) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% Only Global exist.
%% Index exist HW match in Global.
%% Invalid range exist in matched HW revision. 
%% Result: - error     
%% @spec test_10_4_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_4_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_6),

    CxpList = run_mfa_to_get_cxps(Config,
				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_20) = CxpList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_5. Multiple of HW match.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index match.
%% Hultiples of HW match in Global. 
%% Result: - error
%% @spec test_10_5_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_5_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_2),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_9) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index match.
%% Hultiples of HW match in same HAL. 
%% Result: - error
%%         
%% @spec test_10_5_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_5_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_3),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH),
    ?UC_ERR_2(?ERROR_CAUSE_9) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_6. No products exist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index match.
%% HW match in both HAL and Global. 
%% No product exist in HAL. 
%% Result: - error due to empty product list in HAL.
%%         
%% @spec test_10_6_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_6_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_3),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_OK_HALCXP_NOK),
    ?UC_ERR_2(?ERROR_CAUSE_10) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index match.
%% HW match in both HAL and Global. 
%% No products exist in Global. 
%% Result: - error due to empty product list in global.
%%         
%% @spec test_10_6_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_6_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?NOK_GLOBAL_XML_2),
    scp_xml(Config, ?XML_PATH_1, ?HAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?HW_ID_MATCH_2),
    ?UC_ERR_2(?ERROR_CAUSE_11) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index match.
%% HW match only Global. 
%% No product exist in HAL. 
%% Result: - CXP list from global - boardlist
%%         - RES_TEST_4
%%         
%% @spec test_10_6_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_6_3(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_3),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?KDU_OK_REV_OK_GLOBAL),
    match_cxplist(?RES_TEST_4, CxpList).

%%--------------------------------------------------------------------
%% @doc
%% One HAL exist. One Global exist.
%% Index match.
%% HW match oly HAL. 
%% No product exist in HAL. 
%% Result: - error due to empty product list in HAL.
%% @spec test_10_6_4(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_6_4(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    scp_xml(Config, ?XML_PATH_1, ?NOK_HAL_XML_3),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?KDU_OK_REV_OK_HAL),
    ?UC_ERR_2(?ERROR_CAUSE_10) = CxpList.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 10_7. Wrong use of function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% One Global exist.
%% Use of two HwIds in function call shall not work.
%% Result: - error
%%         
%% @spec test_10_7_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_7_1(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),
    FaltyArgHW = [{"KDU123456/99","R30A"}, {"KDU123456/99","R31A"}],
    CxpList = run_mfa_to_get_cxps(Config,
    				  FaltyArgHW),
    %% ?UC_ERR_2(?ERROR_CAUSE_16) = CxpList.
    {throw,?UC_ERR_2(?ERROR_CAUSE_18)} = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% One Global exist.
%% Index exist.
%% Kdu and Rev exist.
%% User use function with revision is not an string, shall not work.
%% Result: - error
%%         
%% @spec test_10_7_2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_7_2(Config) ->
    scp_xml(Config, ?XML_PATH_1, ?GLOBAL_XML_1),

    CxpList = run_mfa_to_get_cxps(Config,
    				  ?KDU_REV_NOT_A_STR),
    ?UC_ERR_2(?ERROR_CAUSE_17) = CxpList.

%%--------------------------------------------------------------------
%% @doc
%% No hal, No Global exist.
%% User use function with faulty argument.
%% Result: - error
%%         
%% @spec test_10_7_3(Config) -> ok
%% @end
%%--------------------------------------------------------------------
test_10_7_3(Config) ->
    FaltyArg = rr,
    CxpList = run_mfa_to_get_cxps(Config,
    				  FaltyArg),
    {throw,?UC_ERR_2(?ERROR_CAUSE_18)} = CxpList.


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% match rcvd cxp list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
match_cxplist(ExpCxpList, RcvdCxpList) ->
    decupling_lib:match_cxplist(ExpCxpList, RcvdCxpList).
