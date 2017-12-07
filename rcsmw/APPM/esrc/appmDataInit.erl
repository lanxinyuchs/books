%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmDataInit.erl %
%%% Author:	etxbjca
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(appmDataInit).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/3').
-date('2017-11-25').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-01-04   etxbjca     Created
%%% R1A/7      2012-03-05   etxarnu     Added call to appmSever:activate
%%% R1A/10     2012-04-13   etxpeno     Dialyzer fixes
%%% R1A/12     2012-08-10   etxarnu     Added appm_lmi stuff
%%% R1A/13     2012-08-20   etxarnu     Changed records due to LMHI
%%% R2A/1      2012-11-28   etxarnu     Register to CEC
%%% R2A/2      2013-03-06   etxpeno     Added appmPghServer to childspec
%%% R2A/3      2013-03-06   etxarnu     Added appmPgroupData do mnesia
%%% R2A/6      2013-04-19   etxarnu     Added appm_tables/0
%%% R2A/7      2013-04-29   etxarnu     Added logI:register_esi_dir for
%%%                                     rcs/applicationlogs
%%% R2A/8      2014-01-31   etxberb     Added appmNodeTable.
%%% R2A/10     2014-02-06   etxberb     Added swmI:copy_old_table in init/1.
%%% R2A/11     2014-02-18   etxberb     Added swmI:copy_upgrWindow_table in
%%%                                     init/1.
%%% R2A/12     2014-03-12   etxarnu     Added restart_strategy/0
%%% R2A/13     2014-04-03   etxberb     Moved swmI:copy_upgrWindow_table to
%%%                                     appmServer.
%%% R2A/14     2014-04-11   etxpejn     Added appmHbServer
%%% R2A/15     2014-04-16   etxarnu     Added call to appmEsi:init_data().
%%% R3A/1      2014-09-22   etxjotj     Moved log registration to init_data
%%% R3A/4      2015-01-21   etxpeno     New function: init_board/0
%% ----    ---------- -------  ------------------------------------------------
%% R4A/1   2015-04-24 etxpejn  Added #appmDataTablewith key = esi_local
%% R4A/2   2015-07-08 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%% R4A/4   2015-10-09 etxarnu  Enable warm restart for TCU
%% R4A/5   2015-10-12 etxarnu  Disable warm restart for TCU
%% R4A/6   2015-10-23 etxarnu  Enable warm restart for TCU again
%% R4A/8   2015-11-17 etxarnu  Enable warm restart for DUS
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%% R5A/2   2016-02-03 etxarnu  HU54336 (part of): Clear restart_info at upgrade
%% R5A/3   2016-02-03 etxarnu  set_revert_state(undefined) at upgrade
%% R5A/4   2016-02-25 etxarnu  HU60044: Register upg_callback in SWM.
%% R6A/1   2016-07-05 etxberb  Added appmHwData & appmHwData_ug.
%% R7A/1   2016-09-16 etxarnu  Disable warm restart for vrcs
%% R7A/2   2016-10-03 uabesvi  HV26316 Timeout in ESI CB
%% R7A/3   2016-10-17 etxarnu  WP6081 : Added #appmDataTable with key = rollback_esi
%% R10A/1  2017-05-29 etxarnu  Added #appmDataTable with key = restart_cold
%% R11A/1  2017-09-01 etxarnu  Removed add_symlinks in init_board on regular
%% ----    ---------- -------  ------------------------------------------------
%% R12A/1  2017-10-23 etxarnu  Initialize appmDataTable with pgroup_cb
%% R12A/2  2017-11-08 etxarnu  Initialize appmDataTable with cold_cb
%% R12A/3  2017-11-25 etxarnu  Use appmServer:disable_pg_restart/2 for reason
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init/1,
	 instPhParallel_post_init/0,
	 instPhParallel_init_data/0,
	 instPhParallel_init_board/0]).
-export([children/0,
	 activate/0]).
-export([restart_strategy/0]).

-export([appm_dir/0]).

-export([appm_tables/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("appm.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    init_tables(DbNodes),
    case swmI:is_upgrade_ongoing() of
	false ->
	    ok;
	true ->
	    swmI:copy_old_table(appmNodeTable),
	    mnesia:dirty_delete({appmNodeTable, {restart_info, node()} }),
	    appmServer:set_revert_state(undefined)
    end,
    ok.

instPhParallel_post_init() ->
    swmI:register_upg_callback(appmI),
    LMs   = appmAppData:get_esi_apps() ++ appmAppData:get_esi_local_apps(),
    Times = [appmAppData:get_max_esi_cb_time(LM) || LM <- LMs],
    Time  = [Time || Time <- Times, is_integer(Time)],
    case Time of
	[] -> appmEsi:init_data();
	_  -> appmEsi:init_data(lists:max(Time))
    end,
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_board() ->
    init_board(sysEnv:role()),
    ok.

restart_strategy() ->
     {ok, {one_for_one, 0, 1}}.

children() ->
    %% appmPghServer must start before appmServer
    {ok, [{appmPghServer, {appmPghServer, start_link, []},
	   permanent, 1000, worker, [appmPghServer]},
	  {appmHbServer, {appmHbServer, start_link, []},
	   permanent, 1000, worker, [appmHbServer]},
	  {appmServer, {appmServer, start_link, []},
	   permanent, 1000, worker, [appmServer]},
	  {appm_lmi_service, {appm_lmi_service, start_link, []},
	   permanent, 1000, worker, [appm_lmi_service]}
	 ]}.

activate()->
    appmServer:activate(),
    ok.


appm_tables() ->
    [appmNodeTable,
     appmDataTable,
     appmLmData,
     appmBoardData,
     appmHwData,
     appmPgroupData].

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% -type init_tables()->                                   %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Create the Mnesia tables needed for APPM.
%%% ----------------------------------------------------------
init_tables(DbNodes)->
    {atomic, ok} =
	clhI:mnesia_create_table(appmNodeTable,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmNodeTable)} |
				  add_clh_option(appmNodeTable)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmDataTable,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmDataTable)} |
				  add_clh_option(appmDataTable)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmLmData,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmLmData)} |
				  add_clh_option(appmLmData)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmBoardData,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmBoardData)} |
				  add_clh_option(appmBoardData)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmHwData,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmHwData)} |
				  add_clh_option(appmHwData)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmPgroupData,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmPgroupData)} |
				  add_clh_option(appmPgroupData)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmLmData_ug,
				 [{type, set},
				  {ram_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmLmData_ug)} |
				  add_clh_option(appmLmData_ug)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmBoardData_ug,
				 [{type, set},
				  {ram_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmBoardData_ug)} |
				  add_clh_option(appmBoardData_ug)]),
    {atomic, ok} =
	clhI:mnesia_create_table(appmHwData_ug,
				 [{type, set},
				  {ram_copies, DbNodes},
				  {attributes,
				   record_info(fields, appmHwData_ug)} |
				  add_clh_option(appmHwData_ug)]),
    ok.

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type init_data()->                                     %#
%%%     ok                                                  %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Initiate Mnesia data tables for APPM.
%%% ----------------------------------------------------------
instPhParallel_init_data()->
    logI:register_esi_dir(filename:join([sysEnv:rcs_dir(), "applicationlogs"])),
    F = fun(Tag) ->
		mnesia:dirty_write(#appmDataTable{key = Tag,
						  value = []}) end,
    lists:map(F,?MP_TAGS),
    
    mnesia:dirty_write(#appmDataTable{key = pgroup_cb,
				      value = []}),
    mnesia:dirty_write(#appmDataTable{key = warm_cb,
				      value = []}),
    mnesia:dirty_write(#appmDataTable{key = cold_cb,
				      value = []}),

    swmI:register_appdata_receiver("appm", appmI),
    file:make_dir(appm_dir()),
    ok = cec:register(<<"LMI">>, appm_lmi_service),
    case sysEnv:vrcs() of
	true ->
	    appmServer:disable_warm_restart(0),
	    appmLib:log(info, ?MODULE_STRING ++
			    ": Disabled warm restart for VRCS",[]);
	false ->
	    appmServer:enable_warm_restart(0),
	    appmLib:log(info, ?MODULE_STRING ++
			    ": Enabled warm restart for both DUS and TCU",[])
    end,
    appmServer:disable_pg_restart(0,"at install"), % Default value currently
    ok.



appm_dir() ->
    filename:join([sysEnv:home_dir(), "appm"]).

init_board(_) ->
    ok.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
