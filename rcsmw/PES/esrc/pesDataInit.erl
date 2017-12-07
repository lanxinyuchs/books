%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesDataInit.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R4A/R5A/4
%%%
%%% @doc ==Initialization of PM Event management==
%%% This module contains the necessary initialization of PM event.
%%% @end
%%% ----------------------------------------------------------
-module(pesDataInit).
-vsn('/main/R3A/R4A/R5A/4').
-date('2016-02-29').
-author('eolaand').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev     Date       Name     What
%%% -----   -------    -------  ------------------------
%%% R3A/1   2014-10-22 etxbjca  Created
%%% R3A/2   2014-10-27 etxjotj  First version
%%% R4A/4   2015-09-08 uabesvi  error_logger -> sysInitI
%%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R5A/2   2016-01-11 etxberb  Added old installation phase functions for
%%%                             backwards compatibility reasons (explicit calls
%%%                             from $RDE_TOP/tools/mkcpi/mkcpi.escript)
%%% R5A/3   2016-01-26 uabesvi  Moved upgrade to post init 
%%% R5A/4   2016-02-29 eolaand  Moved SFTP registration from server to post_init
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0]).
-export([init/1]).
-export([children/0,
	 activate/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsPMEventM.hrl").
-include("pes.hrl").

-define(PM_EVENT, #pmEventM{pmEventMId = {"1","1","1"}}).

-define(MNESIA_SET_DISC(__Attrs, __Nodes), [{disc_copies, __Nodes},
					    {type,        set},
					    {attributes,  __Attrs}]).
-define(MNESIA_BAG(__Attrs), [{type,        bag},
			      {attributes,  __Attrs}]).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type init_tables()->                                   %#
%%%     ok                                                  %#
%%% Input:                                             
%%% Output: 
%%% Exceptions: 
%%% Description: Create the Mnesia tables needed for XXX.
%%% ----------------------------------------------------------
instPhParallel_init(DbNodes)->
    DiscTabs = [{pmEventM,              i_f(?pmEventM_types)},
		{eventType,             i_f(?eventType_types)},
		{eventJob,              i_f(?eventJob_types)},
		{eventGroup,            i_f(?eventGroup_types)},
		{streamingCapabilities, i_f(?streamingCapabilities_types)},
		{filePullCapabilities,  i_f(?filePullCapabilities_types)},
		{eventCapabilities,     i_f(?eventCapabilities_types)},
		{eventFilterType,       i_f(?eventFilterType_types)},
		{eventProducer,         i_f(?eventProducer_types)},
		{pesGroupAliases,       record_info(fields, pesGroupAliases)},
		{pesTypeAliases,        record_info(fields, pesTypeAliases)},
		{pesAppRegPid,          record_info(fields, pesAppRegPid)},
		{pesEnv,                record_info(fields, pesEnv)}
	       ],
    [pesDb:create_table(Name, ?MNESIA_SET_DISC(Fields, DbNodes)) || 
	{Name, Fields} <- DiscTabs],

    Bags = [{pesAppRegType, record_info(fields, pesAppRegType)}],
    [pmsDb:create_table(Name, ?MNESIA_BAG(Fields)) || {Name, Fields} <- Bags],

    pesDb:pes_env_set(appdata_created, []),

    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(DbNodes) ->
    instPhParallel_init(DbNodes).

i_f(Types) ->
    [Field || {Field, _} <- Types].


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
%%% Description: Initiate Mnesia data tables for XXX.
%%% ----------------------------------------------------------
instPhParallel_init_data()->
    swmI:register_appdata_receiver("pmEvent",         pesAppData),
    swmI:register_appdata_receiver("pmEventProducer", pesAppData),
    swmI:register_appdata_receiver("pmEventFilter",   pesAppData),
    swmI:register_appdata_receiver("pmEventAlias",    pesAppData),
    swmI:register_appdata_receiver("pmEventJob",      pesAppData),


    %%================================================================
    %% create PmEventM
    %%================================================================
    pesDb:pm_event_set(?PM_EVENT),

    %%================================================================
    %% inhibit other than 15 min RP/GP
    %%================================================================
    pesDb:pes_env_set(reporting_period, legacy),

    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% -type post_init()->                                     %#
%%%     ok                                                  %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Register things for XXX that require init_data phase
%%%              to have been done.
%%% ----------------------------------------------------------
instPhParallel_post_init()->
    %%================================================================
    %% Reformat records, if needed.
    %%================================================================
    pesDataUpgrade:upgrade(swmI:is_upgrade_ongoing()),

    ProdCXP = mnesia:dirty_match_object(#pesEnv{key = {'_', cxp_info}, 
						value = '_'}),
    lists:foreach(fun(#pesEnv{key = {ProducerId, _}, 
			      value = {CxpProdId, CxpVersion}}) ->
			  AppTmpDir = 
			      get_producer_app_tmp(CxpProdId, CxpVersion),
			  {ok, FPC} = pesDb:file_pull_cap_get(ProducerId),
			  [sysFi:register_sftp_ram_dir(Dir, AppTmpDir,
						       undefined) 
			   || #filePullCapabilities{outputDirectory = Dir} 
				  <- FPC]
			  %% TmpDirKey = {ProducerId, app_tmp_dir},
			  %% pesDb:pes_env_set(TmpDirKey, AppTmpDir)
		  end, ProdCXP),
    comsaI:register_subscriptions(
      "RcsPMEventM", [{"PmEventM",   pmEventM},
		      {"EventType",  eventType},
		      {"EventJob",   eventJob},
		      {"EventGroup", eventGroup},
		      {"EventCapabilities", eventCapatbilities},
		      {"EventFilterType",   eventFilterType},
		      {"EventProducer",     eventProducer}]),
    comsaLib:register_callback(["ManagedElement","SystemFunctions","PmEventM"],
			       pesModel),
    ok.

activate() ->
    appmI:register_warm_cb(pesI),  
    cec:register("PEI",  pesSession),
    ok.

children() ->
    {ok, [{pesServer, {pesServer, start, []},
	   permanent, 1000, worker, [pesServer]},
	  {pesAppRegistry, {pesAppRegistry, start, []},
	   permanent, 1000, worker, [pesAppRegistry]}
	 ]}.




%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%internal_function1(One, Two)->
%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
get_producer_app_tmp(CxpProdId, CxpVersion) ->
    case swmI:get_cxp_path(CxpProdId, CxpVersion) of
    	{ok, CxpPath} ->
    	    AppTmp = appmI:get_rcs_env("APP_TMP", CxpPath),
    	    AppTmp =/= undefined orelse
    		sysInitI:error_msg("~p: No APP_TMP dir found for ~s~n",
    				       [?MODULE, CxpPath]),
    	    AppTmp;
    	Error ->
    	    sysInitI:error_msg("~p: Failed to get cxp path from SWM~n~p~n",
    				   [?MODULE, Error]),
    	    undefined
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%even_more_internal_function1(One, Two)->
%   nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
