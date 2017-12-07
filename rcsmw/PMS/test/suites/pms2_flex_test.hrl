%%% --------------------------------------------------------
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
%%% --------------------------------------------------------



%%=========================================================
%% PM Groups used in test suites
%%=========================================================
-define(FlexGroup1, "PmFlexGroup1").
-define(FlexGroup2, "PmFlexGroup2").
-define(FlexROPGroup1, "PmFlexROPGroup1").
-define(FlexROPGroup2, "PmFlexROPGroup2").
-define(FlexROPGroupMult, "PmFlexROPGroupMult").
-define(FlexROPGroupNonExist, "PmFlexROPGroupNonExist").

%%=========================================================
%% Measurement Types used in test suites
%%=========================================================
-define(FlexType1, "PmFlexType1").
-define(FlexType2, "PmFlexType2").
-define(FlexType3, "PmFlexType3").

-define(FlexROPType1Sum, "PmFlexROPType1Sum").
-define(FlexROPType2Avg, "PmFlexROPType2Avg").
-define(FlexROPType3Min, "PmFlexROPType3Min").
-define(FlexROPType4Max, "PmFlexROPType4Max").
-define(FlexROPType5LU,  "PmFlexROPType5LU").

-define(FlexROPType1SumFilter1, "PmFlexROPType1SumFilter1").
-define(FlexROPType2AvgFilter1, "PmFlexROPType2AvgFilter1").
-define(FlexROPType3MinFilter1, "PmFlexROPType3MinFilter1").
-define(FlexROPType4MaxFilter1, "PmFlexROPType4MaxFilter1").
-define(FlexROPType5LUFilter1,  "PmFlexROPType5LUFilter1").

-define(FlexROPType1SumFilter2, "PmFlexROPType1SumFilter2").
-define(FlexROPType2AvgFilter2, "PmFlexROPType2AvgFilter2").
-define(FlexROPType3MinFilter2, "PmFlexROPType3MinFilter2").
-define(FlexROPType4MaxFilter2, "PmFlexROPType4MaxFilter2").
-define(FlexROPType5LUFilter2,  "PmFlexROPType5LUFilter2").

-define(FlexROPType1SumFilter3, "PmFlexROPType1SumFilter3").
-define(FlexROPType2AvgFilter3, "PmFlexROPType2AvgFilter3").
-define(FlexROPType3MinFilter3, "PmFlexROPType3MinFilter3").
-define(FlexROPType4MaxFilter3, "PmFlexROPType4MaxFilter3").
-define(FlexROPType5LUFilter3,  "PmFlexROPType5LUFilter3").

-define(FlexROPType6Sum, "PmFlexROPType6Sum").
-define(FlexROPType7Avg, "PmFlexROPType7Avg").
-define(FlexROPType8Min, "PmFlexROPType8Min").
-define(FlexROPType9Max, "PmFlexROPType9Max").
-define(FlexROPType10LU, "PmFlexROPType10LU").

-define(FlexROPType6SumFilter1, "PmFlexROPType6SumFilter1").
-define(FlexROPType6SumFilter2, "PmFlexROPType6SumFilter2").
-define(FlexROPType8MinFilter1, "PmFlexROPType8MinFilter1").
-define(FlexROPType8MinFilter2, "PmFlexROPType8MinFilter2").

-define(FlexROPType11SumMult, "PmFlexROPType11SumMult").
-define(FlexROPType12AvgMult, "PmFlexROPType12AvgMult").
-define(FlexROPType13MinMult, "PmFlexROPType13MinMult").
-define(FlexROPType14MaxMult, "PmFlexROPType14MaxMult").
-define(FlexROPType15LUMult,  "PmFlexROPType15LUMult").

-define(FlexROPTypeNonExist, "PmFlexROPTypeNonExist").

%%=========================================================
%% Alias definitions for the groups and types
%%=========================================================
-define(FLEX_GRP1_ALIAS,       101).
-define(FLEX_GRP1_TYPE1_ALIAS, 111).

-define(FLEX_GRP2_ALIAS,       102).
-define(FLEX_GRP2_TYPE2_ALIAS, 121).
-define(FLEX_GRP2_TYPE3_ALIAS, 122).

-define(FLEX_ROPGRP1_ALIAS, 103).
-define(FLEX_ROPGRP1_SUMTYPE_ALIAS, 131).
-define(FLEX_ROPGRP1_AVGTYPE_ALIAS, 132).
-define(FLEX_ROPGRP1_MINTYPE_ALIAS, 133).
-define(FLEX_ROPGRP1_MAXTYPE_ALIAS, 134).
-define(FLEX_ROPGRP1_LUTYPE_ALIAS,  135).

-define(FLEX_ROPGRP1_SUMTYPE_FILTER_1_ALIAS, 231).
-define(FLEX_ROPGRP1_AVGTYPE_FILTER_1_ALIAS, 232).
-define(FLEX_ROPGRP1_MINTYPE_FILTER_1_ALIAS, 233).
-define(FLEX_ROPGRP1_MAXTYPE_FILTER_1_ALIAS, 234).
-define(FLEX_ROPGRP1_LUTYPE_FILTER_1_ALIAS,  235).

-define(FLEX_ROPGRP1_SUMTYPE_FILTER_2_ALIAS, 331).
-define(FLEX_ROPGRP1_AVGTYPE_FILTER_2_ALIAS, 332).
-define(FLEX_ROPGRP1_MINTYPE_FILTER_2_ALIAS, 333).
-define(FLEX_ROPGRP1_MAXTYPE_FILTER_2_ALIAS, 334).
-define(FLEX_ROPGRP1_LUTYPE_FILTER_2_ALIAS,  335).

-define(FLEX_ROPGRP1_SUMTYPE_FILTER_3_ALIAS, 431).
-define(FLEX_ROPGRP1_AVGTYPE_FILTER_3_ALIAS, 432).
-define(FLEX_ROPGRP1_MINTYPE_FILTER_3_ALIAS, 433).
-define(FLEX_ROPGRP1_MAXTYPE_FILTER_3_ALIAS, 434).
-define(FLEX_ROPGRP1_LUTYPE_FILTER_3_ALIAS,  435).






-define(FLEX_ROPGRP2_ALIAS, 104).
-define(FLEX_ROPGRP2_SUMTYPE_ALIAS, 141).
-define(FLEX_ROPGRP2_AVGTYPE_ALIAS, 142).
-define(FLEX_ROPGRP2_MINTYPE_ALIAS, 143).
-define(FLEX_ROPGRP2_MAXTYPE_ALIAS, 144).
-define(FLEX_ROPGRP2_LUTYPE_ALIAS,  145).

-define(FLEX_ROPGRP2_SUMTYPE_FILTER1_ALIAS, 1411).
-define(FLEX_ROPGRP2_SUMTYPE_FILTER2_ALIAS, 1412).
-define(FLEX_ROPGRP2_MINTYPE_FILTER1_ALIAS, 1431).
-define(FLEX_ROPGRP2_MINTYPE_FILTER2_ALIAS, 1432).

-define(FLEX_ROPGRP_MULT_ALIAS, 105).
-define(FLEX_ROPGRP_MULT_SUMTYPE_ALIAS, 151).
-define(FLEX_ROPGRP_MULT_AVGTYPE_ALIAS, 152).
-define(FLEX_ROPGRP_MULT_MINTYPE_ALIAS, 153).
-define(FLEX_ROPGRP_MULT_MAXTYPE_ALIAS, 154).
-define(FLEX_ROPGRP_MULT_LUTYPE_ALIAS,  155).

%%=========================================================
%% Alias definitions for the groups and types
%%=========================================================
-define(FLEX_GRP_ALIAS(Grp), 
	case Grp of
	    ?FlexGroup1           -> ?FLEX_GRP1_ALIAS;
	    ?FlexGroup2           -> ?FLEX_GRP2_ALIAS;
	    ?FlexROPGroup1        -> ?FLEX_ROPGRP1_ALIAS;
	    ?FlexROPGroup2        -> ?FLEX_ROPGRP2_ALIAS;
	    ?FlexROPGroupMult     -> ?FLEX_ROPGRP_MULT_ALIAS;
	    ?FlexROPGroupNonExist -> 123456789
	end).

-define(FLEX_MT_ALIAS(MT), 
	case MT of
	    ?FlexType1 -> ?FLEX_GRP1_TYPE1_ALIAS;
	    ?FlexType2 -> ?FLEX_GRP2_TYPE2_ALIAS;
	    ?FlexType3 -> ?FLEX_GRP2_TYPE3_ALIAS;
	    
	    ?FlexROPType1Sum -> ?FLEX_ROPGRP1_SUMTYPE_ALIAS;
	    ?FlexROPType2Avg -> ?FLEX_ROPGRP1_AVGTYPE_ALIAS;
	    ?FlexROPType3Min -> ?FLEX_ROPGRP1_MINTYPE_ALIAS;
	    ?FlexROPType4Max -> ?FLEX_ROPGRP1_MAXTYPE_ALIAS;
	    ?FlexROPType5LU  -> ?FLEX_ROPGRP1_LUTYPE_ALIAS;

	    ?FlexROPType6Sum -> ?FLEX_ROPGRP2_SUMTYPE_ALIAS;
	    ?FlexROPType7Avg -> ?FLEX_ROPGRP2_AVGTYPE_ALIAS;
	    ?FlexROPType8Min -> ?FLEX_ROPGRP2_MINTYPE_ALIAS;
	    ?FlexROPType9Max -> ?FLEX_ROPGRP2_MAXTYPE_ALIAS;
	    ?FlexROPType10LU -> ?FLEX_ROPGRP2_LUTYPE_ALIAS;

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
-define(ALL_FLEX_MR(Grp),
	case Grp of
	    ?FlexGroup1    -> [?FlexType1];
	    ?FlexGroup2    -> [?FlexType2, ?FlexType3];
	    ?FlexROPGroup1 -> [?FlexROPType1Sum, 
			       ?FlexROPType2Avg, 
			       ?FlexROPType3Min, 
			       ?FlexROPType4Max, 
			       ?FlexROPType5LU];
	    ?FlexROPGroup2 -> [?FlexROPType6Sum,
			       ?FlexROPType7Avg,
			       ?FlexROPType8Min,
			       ?FlexROPType9Max,
			       ?FlexROPType10LU];
	    ?FlexROPGroupMult -> [?FlexROPType11SumMult,
				  ?FlexROPType12AvgMult,
				  ?FlexROPType13MinMult,
				  ?FlexROPType14MaxMult,
				  ?FlexROPType15LUMult];
	    ?FlexROPGroupNonExist -> [?FlexROPTypeNonExist]
	end).

		       
		   



%%=========================================================
%% Counter map default definitions
%%=========================================================
-define(CM_FLEX_GRP_1, [{"PmFlexGroup1", ?FLEX_GRP1_ALIAS,
			 [{"PmFlexType1", ?FLEX_GRP1_TYPE1_ALIAS}]}]).

-define(CM_FLEX_GRP_2, [{"PmFlexGroup2", ?FLEX_GRP2_ALIAS, 
			 [{"PmFlexType2", ?FLEX_GRP2_TYPE2_ALIAS}]},
			{"PmFlexGroup2", ?FLEX_GRP2_ALIAS, 
			 [{"PmFlexType3", ?FLEX_GRP2_TYPE3_ALIAS}]}]).

-define(FLEX_ROP_GRP_1, [{?FlexROPGroup1, ?FLEX_ROPGRP1_ALIAS, 
			  [{?FlexROPType1Sum, ?FLEX_ROPGRP1_SUMTYPE_ALIAS}]},
			 {?FlexROPGroup1, ?FLEX_ROPGRP1_ALIAS, 
			  [{?FlexROPType2Avg, ?FLEX_ROPGRP1_AVGTYPE_ALIAS}]},
			 {?FlexROPGroup1, ?FLEX_ROPGRP1_ALIAS, 
			  [{?FlexROPType3Min, ?FLEX_ROPGRP1_MINTYPE_ALIAS}]},
			 {?FlexROPGroup1, ?FLEX_ROPGRP1_ALIAS, 
			  [{?FlexROPType4Max, ?FLEX_ROPGRP1_MAXTYPE_ALIAS}]},
			 {?FlexROPGroup1, ?FLEX_ROPGRP1_ALIAS, 
			  [{?FlexROPType5LU, ?FLEX_ROPGRP1_LUTYPE_ALIAS}]}]).


