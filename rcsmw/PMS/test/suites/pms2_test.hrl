%%% --------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% --------------------------------------------------------

-include("pms2_flex_test.hrl").


%%=========================================================
%% Rop file handling
%%=========================================================
-define(SINGLE_ROP, 1).
-define(MULTI_ROP,  2).


%%=========================================================
%% 
%%=========================================================
-define(LOW,    1).
-define(MEDIUM, 2).
-define(HIGH,   3).

%%=========================================================
%% RP and GP timers
%%=========================================================
-define(TEN_SECONDS, 1).
-define(THIRTY_SECONDS, 2).
-define(ONE_MIN, 3).
-define(FIVE_MIN, 4).
-define(FIFTEEN_MIN, 5).
-define(THIRTY_MIN, 6).
-define(ONE_HOUR, 7).
-define(TWELVE_HOUR, 8).
-define(ONE_DAY, 9).

-define(GP_10_SEC, 10).
-define(GP_30_SEC, 30).
-define(GP_1_MIN,  60).
-define(GP_5_MIN,  300).
-define(GP_15_MIN, 900).

%%=========================================================
%% Final flag
%%=========================================================
-define(FF_TRUE,  true).
-define(FF_FALSE, false).

%%=========================================================
%% Job state
%%=========================================================
-define(ACTIVE, 1).
-define(STOPPED, 2).

%%=========================================================
%% Default PMI2 initialize callbacks
%% {subscribe, rop, show_counters}
%%=========================================================
-define(CB_DEF, {true, true, true}).

%%=========================================================
%% Default PMI2 initialize counter map
%%=========================================================
-define(COUNTER_MAP_DEF,  "fake").
-define(COUNTER_MAP_FLEX, "flexFake").

%%=========================================================
%% PM Groups used in test suites
%%=========================================================
-define(Group1, "Group1").
-define(Group2, "Group2").
-define(ROPGroup1, "ROPGroup1").
-define(ROPGroup2, "ROPGroup2").
-define(ROPGroupMult, "ROPGroupMult").
-define(ROPGroupNonExist, "ROPGroupNonExist").



%%=========================================================
%% Measurement Types used in test suites
%%=========================================================
-define(Type1, "Type1").
-define(Type2, "Type2").
-define(Type3, "Type3").

-define(ROPType1Sum, "ROPType1Sum").
-define(ROPType2Avg, "ROPType2Avg").
-define(ROPType3Min, "ROPType3Min").
-define(ROPType4Max, "ROPType4Max").
-define(ROPType5LU,  "ROPType5LU").

-define(ROPType6Sum, "ROPType6Sum").
-define(ROPType7Avg, "ROPType7Avg").
-define(ROPType8Min, "ROPType8Min").
-define(ROPType9Max, "ROPType9Max").
-define(ROPType10LU, "ROPType10LU").

-define(ROPType11SumMult, "ROPType11SumMult").
-define(ROPType12AvgMult, "ROPType12AvgMult").
-define(ROPType13MinMult, "ROPType13MinMult").
-define(ROPType14MaxMult, "ROPType14MaxMult").
-define(ROPType15LUMult, "ROPType15LUMult").

-define(ROPTypeNonExist, "ROPTypeNonExist").

%%=========================================================
%% Alias definitions for the groups and types
%%=========================================================
-define(GRP1_ALIAS,       1).
-define(GRP1_TYPE1_ALIAS, 11).

-define(GRP2_ALIAS,       2).
-define(GRP2_TYPE2_ALIAS, 21).
-define(GRP2_TYPE3_ALIAS, 22).

-define(ROPGRP1_ALIAS, 3).
-define(ROPGRP1_SUMTYPE_ALIAS, 31).
-define(ROPGRP1_AVGTYPE_ALIAS, 32).
-define(ROPGRP1_MINTYPE_ALIAS, 33).
-define(ROPGRP1_MAXTYPE_ALIAS, 34).
-define(ROPGRP1_LUTYPE_ALIAS,  35).

-define(ROPGRP2_ALIAS, 4).
-define(ROPGRP2_SUMTYPE_ALIAS, 41).
-define(ROPGRP2_AVGTYPE_ALIAS, 42).
-define(ROPGRP2_MINTYPE_ALIAS, 43).
-define(ROPGRP2_MAXTYPE_ALIAS, 44).
-define(ROPGRP2_LUTYPE_ALIAS,  45).

-define(ROPGRP_MULT_ALIAS, 5).
-define(ROPGRP_MULT_SUMTYPE_ALIAS, 51).
-define(ROPGRP_MULT_AVGTYPE_ALIAS, 52).
-define(ROPGRP_MULT_MINTYPE_ALIAS, 53).
-define(ROPGRP_MULT_MAXTYPE_ALIAS, 54).
-define(ROPGRP_MULT_LUTYPE_ALIAS,  55).


%%=========================================================
%% Alias definitions for the groups and types
%%=========================================================
-define(GRP_ALIAS(Grp), 
	case Grp of
	    ?Group1           -> ?GRP1_ALIAS;
	    ?Group2           -> ?GRP2_ALIAS;
	    ?ROPGroup1        -> ?ROPGRP1_ALIAS;
	    ?ROPGroup2        -> ?ROPGRP2_ALIAS;
	    ?ROPGroupMult     -> ?ROPGRP_MULT_ALIAS;
	    ?ROPGroupNonExist -> 12345678;
	    ?FlexGroup1           -> ?FLEX_GRP1_ALIAS;
	    ?FlexGroup2           -> ?FLEX_GRP2_ALIAS;
	    ?FlexROPGroup1        -> ?FLEX_ROPGRP1_ALIAS;
	    ?FlexROPGroup2        -> ?FLEX_ROPGRP2_ALIAS;
	    ?FlexROPGroupMult     -> ?FLEX_ROPGRP_MULT_ALIAS;
	    ?FlexROPGroupNonExist -> 123456789
	end).

-define(MT_ALIAS(MR), 
	case MR of
	    ?Type1 -> ?GRP1_TYPE1_ALIAS;
	    ?Type2 -> ?GRP2_TYPE2_ALIAS;
	    ?Type3 -> ?GRP2_TYPE3_ALIAS;
	    
	    ?ROPType1Sum -> ?ROPGRP1_SUMTYPE_ALIAS;
	    ?ROPType2Avg -> ?ROPGRP1_AVGTYPE_ALIAS;
	    ?ROPType3Min -> ?ROPGRP1_MINTYPE_ALIAS;
	    ?ROPType4Max -> ?ROPGRP1_MAXTYPE_ALIAS;
	    ?ROPType5LU  -> ?ROPGRP1_LUTYPE_ALIAS;

	    ?ROPType6Sum -> ?ROPGRP2_SUMTYPE_ALIAS;
	    ?ROPType7Avg -> ?ROPGRP2_AVGTYPE_ALIAS;
	    ?ROPType8Min -> ?ROPGRP2_MINTYPE_ALIAS;
	    ?ROPType9Max -> ?ROPGRP2_MAXTYPE_ALIAS;
	    ?ROPType10LU -> ?ROPGRP2_LUTYPE_ALIAS;

	    ?ROPType11SumMult -> ?ROPGRP_MULT_SUMTYPE_ALIAS;
	    ?ROPType12AvgMult -> ?ROPGRP_MULT_AVGTYPE_ALIAS;
	    ?ROPType13MinMult -> ?ROPGRP_MULT_MINTYPE_ALIAS;
	    ?ROPType14MaxMult -> ?ROPGRP_MULT_MAXTYPE_ALIAS;
	    ?ROPType15LUMult  -> ?ROPGRP_MULT_LUTYPE_ALIAS;

	    ?ROPTypeNonExist  -> 87654321;

	    ?FlexType1 -> ?FLEX_GRP1_TYPE1_ALIAS;
	    ?FlexType2 -> ?FLEX_GRP2_TYPE2_ALIAS;
	    ?FlexType3 -> ?FLEX_GRP2_TYPE3_ALIAS;
	    
	    ?FlexROPType1Sum -> ?FLEX_ROPGRP1_SUMTYPE_ALIAS;
	    ?FlexROPType2Avg -> ?FLEX_ROPGRP1_AVGTYPE_ALIAS;
	    ?FlexROPType3Min -> ?FLEX_ROPGRP1_MINTYPE_ALIAS;
	    ?FlexROPType4Max -> ?FLEX_ROPGRP1_MAXTYPE_ALIAS;
	    ?FlexROPType5LU  -> ?FLEX_ROPGRP1_LUTYPE_ALIAS;

	    ?FlexROPType1SumFilter1 -> ?FLEX_ROPGRP1_SUMTYPE_FILTER_1_ALIAS;
	    ?FlexROPType2AvgFilter1 -> ?FLEX_ROPGRP1_AVGTYPE_FILTER_1_ALIAS;
	    ?FlexROPType3MinFilter1 -> ?FLEX_ROPGRP1_MINTYPE_FILTER_1_ALIAS;
	    ?FlexROPType4MaxFilter1 -> ?FLEX_ROPGRP1_MAXTYPE_FILTER_1_ALIAS;
	    ?FlexROPType5LUFilter1  -> ?FLEX_ROPGRP1_LUTYPE_FILTER_1_ALIAS;

	    ?FlexROPType1SumFilter2 -> ?FLEX_ROPGRP1_SUMTYPE_FILTER_2_ALIAS;
	    ?FlexROPType2AvgFilter2 -> ?FLEX_ROPGRP1_AVGTYPE_FILTER_2_ALIAS;
	    ?FlexROPType3MinFilter2 -> ?FLEX_ROPGRP1_MINTYPE_FILTER_2_ALIAS;
	    ?FlexROPType4MaxFilter2 -> ?FLEX_ROPGRP1_MAXTYPE_FILTER_2_ALIAS;
	    ?FlexROPType5LUFilter2  -> ?FLEX_ROPGRP1_LUTYPE_FILTER_2_ALIAS;

	    ?FlexROPType1SumFilter3 -> ?FLEX_ROPGRP1_SUMTYPE_FILTER_3_ALIAS;
	    ?FlexROPType2AvgFilter3 -> ?FLEX_ROPGRP1_AVGTYPE_FILTER_3_ALIAS;
	    ?FlexROPType3MinFilter3 -> ?FLEX_ROPGRP1_MINTYPE_FILTER_3_ALIAS;
	    ?FlexROPType4MaxFilter3 -> ?FLEX_ROPGRP1_MAXTYPE_FILTER_3_ALIAS;
	    ?FlexROPType5LUFilter3  -> ?FLEX_ROPGRP1_LUTYPE_FILTER_3_ALIAS;

	    ?FlexROPType6Sum -> ?FLEX_ROPGRP2_SUMTYPE_ALIAS;
	    ?FlexROPType7Avg -> ?FLEX_ROPGRP2_AVGTYPE_ALIAS;
	    ?FlexROPType8Min -> ?FLEX_ROPGRP2_MINTYPE_ALIAS;
	    ?FlexROPType9Max -> ?FLEX_ROPGRP2_MAXTYPE_ALIAS;
	    ?FlexROPType10LU -> ?FLEX_ROPGRP2_LUTYPE_ALIAS;

	    ?FlexROPType6SumFilter1 -> ?FLEX_ROPGRP2_SUMTYPE_FILTER1_ALIAS;
	    ?FlexROPType6SumFilter2 -> ?FLEX_ROPGRP2_SUMTYPE_FILTER2_ALIAS;
	    ?FlexROPType8MinFilter1 -> ?FLEX_ROPGRP2_MINTYPE_FILTER1_ALIAS;
	    ?FlexROPType8MinFilter2 -> ?FLEX_ROPGRP2_MINTYPE_FILTER2_ALIAS;

	    ?FlexROPType11SumMult -> ?FLEX_ROPGRP_MULT_SUMTYPE_ALIAS;
	    ?FlexROPType12AvgMult -> ?FLEX_ROPGRP_MULT_AVGTYPE_ALIAS;
	    ?FlexROPType13MinMult -> ?FLEX_ROPGRP_MULT_MINTYPE_ALIAS;
	    ?FlexROPType14MaxMult -> ?FLEX_ROPGRP_MULT_MAXTYPE_ALIAS;
	    ?FlexROPType15LUMult  -> ?FLEX_ROPGRP_MULT_LUTYPE_ALIAS;

	    ?FlexROPTypeNonExist  -> 987654321
	end).


%%=========================================================
%% All MeasurementTypes for a Group
%%=========================================================
-define(ALL_MR(Grp),
	case Grp of
	    ?Group1    -> [?Type1];
	    ?Group2    -> [?Type2, ?Type3];
	    ?ROPGroup1 -> [?ROPType1Sum, 
			   ?ROPType2Avg, 
			   ?ROPType3Min, 
			   ?ROPType4Max, 
			   ?ROPType5LU];
	    ?ROPGroup2 -> [?ROPType6Sum,
			   ?ROPType7Avg,
			   ?ROPType8Min,
			   ?ROPType9Max,
			   ?ROPType10LU];
	    ?ROPGroupMult -> [?ROPType11SumMult,
			      ?ROPType12AvgMult,
			      ?ROPType13MinMult,
			      ?ROPType14MaxMult,
			      ?ROPType15LUMult];
	    ?ROPGroupNonExist -> [?ROPTypeNonExist]
	end).

		       
		   



%%=========================================================
%% Counter map default definitions
%%=========================================================
-define(CM_GRP_DEF, [{"Group1", ?GRP1_ALIAS, [{"Type1", ?GRP1_TYPE1_ALIAS}]},
		     {"Group2", ?GRP2_ALIAS, [{"Type2", ?GRP2_TYPE2_ALIAS}]},
		     {"Group2", ?GRP2_ALIAS, [{"Type3", ?GRP2_TYPE3_ALIAS}]}]).

-define(CM_GRP_1, [{"Group1", ?GRP1_ALIAS, [{"Type1", ?GRP1_TYPE1_ALIAS}]}]).

-define(CM_GRP_2, [{"Group2", ?GRP2_ALIAS, [{"Type2", ?GRP2_TYPE2_ALIAS}]},
		   {"Group2", ?GRP2_ALIAS, [{"Type3", ?GRP2_TYPE3_ALIAS}]}]).


-define(ROP_GRP_1, [{?ROPGroup1, ?ROPGRP1_ALIAS, 
		     [{?ROPType1Sum, ?ROPGRP1_SUMTYPE_ALIAS}]},
		    {?ROPGroup1, ?ROPGRP1_ALIAS, 
		     [{?ROPType2Avg, ?ROPGRP1_AVGTYPE_ALIAS}]},
		    {?ROPGroup1, ?ROPGRP1_ALIAS, 
		     [{?ROPType3Min, ?ROPGRP1_MINTYPE_ALIAS}]},
		    {?ROPGroup1, ?ROPGRP1_ALIAS, 
		     [{?ROPType4Max, ?ROPGRP1_MAXTYPE_ALIAS}]},
		    {?ROPGroup1, ?ROPGRP1_ALIAS, 
		     [{?ROPType5LU, ?ROPGRP1_LUTYPE_ALIAS}]}]).

-define(ROP_GRP_2, [{?ROPGroup2, ?ROPGRP2_ALIAS, 
		     [{?ROPType6Sum, ?ROPGRP2_SUMTYPE_ALIAS}]},
		    {?ROPGroup2, ?ROPGRP2_ALIAS, 
		     [{?ROPType7Avg, ?ROPGRP2_AVGTYPE_ALIAS}]},
		    {?ROPGroup2, ?ROPGRP2_ALIAS, 
		     [{?ROPType8Min, ?ROPGRP2_MINTYPE_ALIAS}]},
		    {?ROPGroup2, ?ROPGRP2_ALIAS, 
		     [{?ROPType9Max, ?ROPGRP2_MAXTYPE_ALIAS}]},
		    {?ROPGroup2, ?ROPGRP2_ALIAS, 
		     [{?ROPType10LU, ?ROPGRP2_LUTYPE_ALIAS}]}]).

-define(ROP_GRP_MULT, [{?ROPGroupMult, ?ROPGRP_MULT_ALIAS, 
			[{?ROPType11SumMult, ?ROPGRP_MULT_SUMTYPE_ALIAS}]},
		       {?ROPGroupMult, ?ROPGRP_MULT_ALIAS, 
			[{?ROPType12AvgMult, ?ROPGRP_MULT_AVGTYPE_ALIAS}]},
		       {?ROPGroupMult, ?ROPGRP_MULT_ALIAS, 
			[{?ROPType13MinMult, ?ROPGRP_MULT_MINTYPE_ALIAS}]},
		       {?ROPGroupMult, ?ROPGRP_MULT_ALIAS, 
			[{?ROPType14MaxMult, ?ROPGRP_MULT_MAXTYPE_ALIAS}]},
		       {?ROPGroupMult, ?ROPGRP_MULT_ALIAS, 
			[{?ROPType15LUMult, ?ROPGRP_MULT_LUTYPE_ALIAS}]}]).


%%=========================================================
%% Default expected lists for creating a job using
%% Group1, Group2 jobs
%%=========================================================

-define(OPTS_SUBSCR_GROUP_1,
	[{gp, 10}, {spec, [{?GRP1_ALIAS, [?GRP1_TYPE1_ALIAS]}]}]).

-define(OPTS_SUBSCR_GROUP_2,
	[{gp, 10}, 
	 {spec, [{?GRP2_ALIAS, [?GRP2_TYPE2_ALIAS,
				?GRP2_TYPE3_ALIAS]}]}]).


-define(EXP_SUBSCR_GROUP_1,
	[{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_1},
	 pmi2ReportRop]).

-define(EXP_SUBSCR_GROUP_2,
	[{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, ?OPTS_SUBSCR_GROUP_2},
	 pmi2ReportRop]).


%%=========================================================
%% Default expected lists for creating a job using
%% ROPGroup1, ROPGroup2 
%%=========================================================

-define(OPTS_SUBSCR_ROPGROUP_1,
	[{gp, 10}, {spec, [{?ROPGRP1_ALIAS, [?ROPGRP1_SUMTYPE_ALIAS,
					     ?ROPGRP1_AVGTYPE_ALIAS,
					     ?ROPGRP1_MINTYPE_ALIAS,
					     ?ROPGRP1_MAXTYPE_ALIAS,
					     ?ROPGRP1_LUTYPE_ALIAS]}]}]).

-define(OPTS_SUBSCR_ROPGROUP_2,
	[{gp, 10}, {spec, [{?ROPGRP2_ALIAS, [?ROPGRP2_SUMTYPE_ALIAS,
					     ?ROPGRP2_AVGTYPE_ALIAS,
					     ?ROPGRP2_MINTYPE_ALIAS,
					     ?ROPGRP2_MAXTYPE_ALIAS,
					     ?ROPGRP2_LUTYPE_ALIAS]}]}]).

-define(OPTS_SUBSCR_ROPGROUP_MULTI,
	[{gp, 10}, {spec, [{?ROPGRP_MULT_ALIAS, 
			    [?ROPGRP_MULT_SUMTYPE_ALIAS,
			     ?ROPGRP_MULT_AVGTYPE_ALIAS,
			     ?ROPGRP_MULT_MINTYPE_ALIAS,
			     ?ROPGRP_MULT_MAXTYPE_ALIAS,
			     ?ROPGRP_MULT_LUTYPE_ALIAS]}]}]).

-define(EXP_SUBSCR_ROPGROUP_1,
	[{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_1},
	 pmi2ReportRop]).

-define(EXP_SUBSCR_ROPGROUP_2,
	[{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_2},
	 pmi2ReportRop]).

-define(EXP_SUBSCR_ROPGROUP_MULT,
	[{pmi2ReportRop, {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, ?OPTS_SUBSCR_ROPGROUP_MULT},
	 pmi2ReportRop]).


%%=========================================================
%% Default expected lists for deleting a job 
%%=========================================================
-define(EXP_FINAL,  
	[{pmi2ReportRop,    {repeat, wait_until_subscribe}, []},
	 {pmi2SubscribeRop, {repeat, 1}, [{gp, 10}, {spec, []}]}]).


%%=========================================================
%% Used for creating default group definitions when 
%% creating a job 
%%=========================================================
-define(JOB_GROUP_1, job_group_1).
-define(JOB_GROUP_2, job_group_2).
-define(JOB_NO_SR,   job_group_1_no_sr).
-define(JOB_NO_MR,   job_no_mr).

-define(JOB_FLEX_GROUP_1, job_flex_group_1).
-define(JOB_FLEX_GROUP_2, job_flex_group_2).
-define(JOB_FLEX_NO_SR,   job_flex_group_1_no_sr).
-define(JOB_FLEX_NO_MR,   job_flex_no_mr).

-define(A2L(A), atom_to_list(A)).

%%=========================================================
%% Create default LDNs for faked MOs
%% used when replying measurement data. 
%%=========================================================
-define(LDN_DEF_1(App), ["ManagedElement=1," ++ ?A2L(App) ++ "=1,Mo=one"]).
-define(LDN_DEF_2(App), ["ManagedElement=1," ++ ?A2L(App) ++ "=1,Mo=two"]).
-define(LDN_DEF_3(App), ["ManagedElement=1," ++ ?A2L(App) ++ "=1,Mo=three"]).

-define(LDN_DEF(App), lists:append([?LDN_DEF_1(App),
				    ?LDN_DEF_2(App),
				    ?LDN_DEF_3(App)])).


%%=========================================================
%% Default job attributes.
%%=========================================================
-define(DEF_JOB_ATTRS, [{"granularityPeriod", ?TEN_SECONDS},
			{"reportingPeriod",   ?TEN_SECONDS},
			{"currentJobState",   ?ACTIVE}]).

-define(JOB_ATTR_RP30, [{"granularityPeriod", ?THIRTY_SECONDS},
			{"reportingPeriod",   ?THIRTY_SECONDS},
			{"currentJobState",   ?ACTIVE}]).


%%=========================================================
%% predefined jobs
%%=========================================================
-define(PREDEF_JOB, "PredefJob").

-define(PREDEF_JOB_PROCS, 2).

%%=========================================================
%% NetConf attribute operations
%%=========================================================
-define(DELETE_ATTR, delete_attribute).
