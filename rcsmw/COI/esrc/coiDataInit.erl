%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coiDataInit.erl %
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%% @author etxberb
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R4A/R5A/1

%%% @doc Data initialization functions used at system installation and
%%%      software upgrade.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coiDataInit).
%%-behaviour(rcmMnesia). % This ensures that the mandatory callbacks are present
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-01-07').
-author('etxberb').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% ------  ---------- -------  ------------------------------------------------
%% R3A/1   2014-12-10 etxbjca  Created
%% R3A/2   2015-01-09 etxberb  First version for delivery.
%% R3A/4   2015-01-13 etxberb  Moved calls from comsaDataInit.
%% R3A/6   2015-01-15 etxberb  Temporary disabled calls during move of modules.
%% R3A/7   2015-01-15 etxberb  Removed temporary disabling.
%% R3A/8   2015-02-13 etxberb  Added coiAlarm.
%% R4A/1   2015-09-03 etxberb  Removed call to coiMim:post_init.
%% R4A/2   2015-10-21 etxberb  Added call to coiMib:init_tables & post_init.
%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% Optional functions that will be called if implemented when this module is
%%% specified as a 'sysApp' 'initmodule' in the .appSrc:
-export([activate/0,
	 children/0]).

%%%-export([init_tables/0,
-export([instPhParallel_init/1,
	 instPhParallel_post_init/0]).

%%% Optional application upgrade function
%%%-export([upgrade_init/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###
%%                        :: ProcessId used internally by the supervisor.
%%                           No direct relation to the registered name.
-type child_Id()          :: term().

%%                        :: The function call used to start the child process.
-type child_StartMFA()    :: {M :: module(), F :: atom(), A :: [term()]}.

%%                        :: When to restart the child.
-type child_RestartType() :: permanent | transient | temporary.

%%                        :: How to terminate the child.
%%                           Integer indicates a time in milliseconds that the
%%                           supervisor waits for an exit (return) signal. At
%%                           timeout, the child is unconditionally terminated.
-type child_Shutdown()    :: brutal_kill | pos_integer() | infinity.

%%                        :: Type.
-type child_ProcessType() :: worker | supervisor.

%%                        :: Modules containing a 'code_change' function.
-type child_Modules()     :: [module()] | dynamic.

%%                 :: See also http://erlang.org/doc/man/supervisor.html.
-type child_spec() :: {child_Id(),
		       child_StartMFA(),
		       child_RestartType(),
		       child_Shutdown(),
		       child_ProcessType(),
		       child_Modules()}.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.1.1 'sysApp' Call-Back Functions
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This is a poor man's version of a StartPhase, originating from sysApp.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec activate() ->
    ok.
%%% ###=====================================================================###
activate() ->
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Start child processes in the supervisor tree needed for COI.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec children() ->
    {ok, list(child_spec())}.
%%% ###=====================================================================###
children() ->
    ChildSpecs =
	coiServer:child_specs() ++
	coiAlarm:child_specs(),
    {ok, ChildSpecs}.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Create the Mnesia tables needed for COI.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec instPhParallel_init(DbNodes :: list()) ->
    ok.
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    coiMim:init_tables(DbNodes),
    coiMib:init_tables(DbNodes),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Register things for COI that require init_data phase to have been done.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec instPhParallel_post_init() ->
    ok.
%%% ###=====================================================================###
instPhParallel_post_init()->
    coiMim:post_init(),
    coiMib:post_init(),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% 'cinnamon_bun'doc Application code change for COI.
%%%
%%% 'cinnamon_bun'end
%%% ----------------------------------------------------------
%%%-spec upgrade_init(Type       :: atom() ,
%%%                   OldVersion :: string()) ->
%%%    ok.
%%% ###=====================================================================###
%%%upgrade_init(Type, OldVersion)->
%%%    merge_data_from_old_node(),
%%%    ok.

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
