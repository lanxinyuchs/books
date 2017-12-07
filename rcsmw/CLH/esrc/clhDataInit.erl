%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	clhDataInit.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R2A/R3A/R4A/R5A/2
%%%
%%% @doc == Initialization of CLH ==
%%% This module initializes CLH through the various callbacks to SYS etc

-module(clhDataInit).
-vsn('/main/R2A/R3A/R4A/R5A/2').
-date('2016-01-11').
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-11-13 etxpeno     Created
%%% R3A/1      2015-02-03 etxpejn     Added clhMpConf & clhMpState in init/1
%%% R3A/2      2015-02-06 etxpejn     Added upgrade support
%%% R4A/1      2015-04-17 etxberb     Updated according to changed interfaces.
%%% R4A/2      2015-06-05 etxberb     Changed FruId_undefined to FruId_default.
%%% R4A/3      2015-07-06 etxberb     Added install_begin/0 & install_end/0.
%% R4A/4   2015-07-07 etxberb  Added call to clh_csi_service:install_begin/0.
%% R4A/5   2015-07-09 etxberb  Bug correction - now always doing 'cec:register'.
%% R4A/6   2015-08-11 etxberb  TR HT98071.
%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%% R5A/2   2016-01-11 etxberb  Added old installation phase functions for
%%                             backwards compatibility reasons (explicit calls
%%                             from $RDE_TOP/tools/mkcpi/mkcpi.escript)
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhSeqBeg_begin/0,
	 instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhSeqEnd_end/0]).
-export([install_begin/0,
	 init/1,
	 install_end/0]).
-export([children/0, activate/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("clhTables.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ###########################################################################
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
instPhSeqBeg_begin() ->
    clh_db:install_begin(),
    DbNodes = mnesia:system_info(running_db_nodes),
    {atomic, ok} =
        mnesia:create_table(clhMpConf,
			    [{attributes, record_info(fields, clhMpConf)},
			     {type, ordered_set},
			     {disc_copies, DbNodes}]),
    {atomic, ok} =
        mnesia:create_table(clhMpState,
			    [{attributes, record_info(fields, clhMpState)},
			     {type, ordered_set},
			     {disc_copies, DbNodes}]),
    {atomic, ok} =
        mnesia:create_table(clhTableVersion,
			    [{attributes, record_info(fields, clhTableVersion)},
			     {type, ordered_set},
			     {disc_copies, DbNodes}]),
    clh_csi_service:install_begin(),
    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
install_begin() ->
    instPhSeqBeg_begin().

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ###########################################################################
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    ok = clh_db:init_tables(DbNodes).

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(DbNodes) ->
    instPhParallel_init(DbNodes).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ###########################################################################
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
instPhParallel_init_data() ->
    setTableVersions(),
    case swmI:is_upgrade_ongoing() of
	true ->
	    try
		ok = copyOldTables(getOldTableVersions())
	    catch
		_ErrClass : _ErrReason ->
		    nok
	    end;
	false ->
	    MpId = 1,
	    ok = mnesia:dirty_write(#clhMpConf{mpId = MpId,
					       fruId = ?FruId_default(MpId),
					       erlNode =
					       clhI:make_erlang_node(MpId),
					       mpRole = core,
					       coreRank = primary}),
	    ok = mnesia:dirty_write(#clhMpState{mpId = MpId,
						coreState = active})
    end,
    ok = cec:register(<<"CSI">>, clh_csi_service).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ###########################################################################
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
instPhSeqEnd_end() ->
    clh_db:install_end().

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
install_end() ->
    instPhSeqEnd_end().

%%% ###########################################################################
%%% @doc Start phase.
%%%
%%% @end
%%% ###=====================================================================###
activate() ->
    clh_csi_service:activate(),
    ok.

children() ->
    {ok, [{clh_csi_service, {clh_csi_service, start_link, []},
           permanent,
           1000,
           worker,
           [clh_csi_service]}]}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%% convertCoreRoleToCoreRank
%%%
%%% ###=====================================================================###
convertCoreRoleToCoreRank(CoreRole) when CoreRole == primary orelse
					 CoreRole == secondary ->
    CoreRole;
convertCoreRoleToCoreRank(_) ->
    undefined.

%%% ###########################################################################
%%% convertCoreRoleToMpRole
%%%
%%% ###=====================================================================###
convertCoreRoleToMpRole(CoreRole) when CoreRole == primary orelse
				       CoreRole == secondary ->
    core;
convertCoreRoleToMpRole(_) ->
    regular.

%%% ###########################################################################
%%% convertRoleToCoreState
%%%
%%% ###=====================================================================###
convertRoleToCoreState(Role) when Role == active orelse
				  Role == standby ->
    Role;
convertRoleToCoreState(_) ->
    undefined.

%%% ###########################################################################
%%% convertOldTable
%%%
%%% ###=====================================================================###
convertOldTable([{clhMpConf, MpId, CoreRole, FruId} | Tail],
		undefined = OldVersion) ->
    NewFruId =
	case FruId of
	    undefined ->
		?FruId_default(MpId);
	    _ ->
		FruId
	end,
    ok = mnesia:write(#clhMpConf{mpId = MpId,
				 fruId = NewFruId,
				 erlNode = clhI:make_erlang_node(MpId),
				 mpRole = convertCoreRoleToMpRole(CoreRole),
				 coreRank=convertCoreRoleToCoreRank(CoreRole)}),
    convertOldTable(Tail, OldVersion);
convertOldTable([{clhMpState, MpId, Role, OpState, _MpState} | Tail],
		undefined = OldVersion) ->
    ok = mnesia:write(#clhMpState{mpId = MpId,
				  opState = OpState,
				  coreState = convertRoleToCoreState(Role)}),
    convertOldTable(Tail, OldVersion);
convertOldTable([], _) ->
    ok.

%%% ###########################################################################
%%% copyOldTables
%%%
%%% ###=====================================================================###
copyOldTables([#clhTableVersion{table = clhMpConf = Table,
				version = ?TblVersion_clhMpConf} | Tail]) ->
    swmI:copy_old_table(Table),
    copyOldTables(Tail);
copyOldTables([#clhTableVersion{table = clhMpState = Table,
				version = ?TblVersion_clhMpState} | Tail]) ->
    swmI:copy_old_table(Table),
    copyOldTables(Tail);
copyOldTables([#clhTableVersion{table = Table,
				version = OldVersion} | Tail]) ->
    convertOldTable(swmI:all_objects(Table), OldVersion),
    copyOldTables(Tail);
copyOldTables([]) ->
    ok.

%%% ###########################################################################
%%% getOldTableVersions
%%%
%%% ###=====================================================================###
getOldTableVersions() ->
    try
	swmI:all_objects(clhTableVersion)
    catch
	_ : _ ->
	    %% Default version = undefined:
	    [#clhTableVersion{table = clhMpConf},
	     #clhTableVersion{table = clhMpState}]
    end.

%%% ###########################################################################
%%% setTableVersions
%%%
%%% ###=====================================================================###
setTableVersions() ->
    ok = mnesia:dirty_write(#clhTableVersion{table = clhMpConf,
					     version = ?TblVersion_clhMpConf}),
    ok = mnesia:dirty_write(#clhTableVersion{table = clhMpState,
					     version = ?TblVersion_clhMpState}).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
