%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lihDataInit.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R5A/1
%%%
%%% @doc ==Initialization of LIH==

-module(lihDataInit).
-vsn('/main/R1A/R5A/1').
-author('etxberb').
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%% Rev     Date       Name     What
%% -----   ---------  -------  ------------------------
%% R1A/8   2012-05-02 etxpeno  Added spec()
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([instPhParallel_init/1,
	 instPhParallel_init_data/0]).
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
-spec instPhParallel_init(DbNodes::[node()]) -> 'ok'.
instPhParallel_init(DbNodes) ->
    ok = lih_db:init_tables(DbNodes).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
-spec instPhParallel_init_data() -> 'ok'.
instPhParallel_init_data() ->
    ok = lih_db:init_data().

-spec activate() -> 'ok'.
activate() ->
    lih_lici_server:activate(),
    lih_lihi_server:activate(),

    ok.

-spec children() -> {'ok', [supervisor:child_spec()]}.
children() ->
    {ok, [{lih_lici_server, {lih_lici_server, start_link, []},
	   permanent, 1000, worker, [lih_lici_server]},
	  {lih_lihi_server, {lih_lihi_server, start_link, []},
	   permanent, 1000, worker, [lih_lihi_server]}]}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
