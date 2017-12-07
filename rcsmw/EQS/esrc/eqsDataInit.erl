%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	eqsDataInit.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R8A/1
%%%
%%% @doc == Initialization of EQS ==
%%% This module initializes EQS through the various callbacks to SYS etc

-module(eqsDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R8A/1').
-date('2016-12-28').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-10-03   etxbjca     Created
%%% R4A/1      2015-05-18   etxpejn     Added init_board
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R8A/1      2016-12-28   uabesvi     Removed MMI log if cloud
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init_data/0,
	 instPhParallel_init_board/0]).
-export([children/0, activate/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data() ->
    ok = cec:register(<<"PRI">>, eqs_pri_service),
    ok = cec:register(<<"VII">>, eqs_vii_service),
    ok = cec:register(<<"BUTI">>, eqs_buti_service).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_board() ->
    %% No MMI in cloud
    case sysEnv:rcs_mode_2() of
	vrcs ->
	    ok;
	_ ->
	    OptList = [{maxSize, 1}, {rotatingSegments, 3}, {public, false}],
	    ok = logI:create_log("MMILog", OptList)
    end.

activate() ->
    eqs_mmi_service:activate(),
    eqs_pri_service:activate(),
    eqs_vii_service:activate(),
    eqs_buti_service:activate(),
    ok.

children() ->
    {ok, [{eqs_mmi_service, {eqs_mmi_service, start_link, []},
	   permanent,
	   1000,
	   worker,
	   [eqs_mmi_service]},

	  {eqs_pri_service, {eqs_pri_service, start_link, []},
	   permanent,
	   1000,
	   worker,
	   [eqs_pri_service]},

	  {eqs_vii_service, {eqs_vii_service, start_link, []},
	   permanent,
	   1000,
	   worker,
	   [eqs_vii_service]},

	  {eqs_buti_service, {eqs_buti_service, start_link, []},
	   permanent,
	   1000,
	   worker,
	   [eqs_buti_service]}]}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
