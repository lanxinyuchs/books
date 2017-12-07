%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	alhDataInit.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R4A/R5A/R11A/1
%%%
%%% @doc == Initialization of ALH ==
%%% This module initializes ALH through the various callbacks to SYS etc

-module(alhDataInit).
-vsn('/main/R2A/R4A/R5A/R11A/1').
-date('2017-10-02').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% R2A/1      2012-12-03 eolaand     Created
%%% R2A/2      2012-12-07 eolaand     Add mnesia table
%%% R2A/6      2014-02-05 etxberb     Added swmI:copy_old_table in init/1.
%%% R2A/7      2014-02-18 etxberb     Added swmI:copy_upgrWindow_table and
%%%                                   alh_service:delete_oldest_objs in init/1.
%%% R2A/9      2014-04-09 etxberb     Moved swmI:copy_upgrWindow_table to
%%%                                   alh_service.erl.
%%% R4A/    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R5A/    ---------- -------  ------------------------------------------------
%%% R5A/1   2015-12-11 ekurnik  Added alh_cec_service and alh_itc_service
%%%                             support.
%%% R5A/2   2015-12-29 etomist  Removed alh_cec_service and alh_itc_service
%%% R5A/3   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R11A/1  2017-10-01 etxberb  SP531: Added check in instPhParallel_init_data.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0]).
-export([children/0, activate/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%%===================================================================
%%% Include files
%%%===================================================================
-include("alh_service.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
-spec instPhParallel_init(DbNodes::[atom()]) -> ok. 
instPhParallel_init(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(?MNESIA_TAB,
				 [{disc_copies, DbNodes},
				  {attributes, record_info(fields,?MNESIA_TAB)},
				  {type, ordered_set}]),
    case swmI:is_upgrade_ongoing() of
	false ->
	    ok;
	true ->
	    swmI:copy_old_table(?MNESIA_TAB)
    end,
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
-spec instPhParallel_init_data() -> ok.
instPhParallel_init_data() ->
    case mnesia:dirty_read(?MNESIA_TAB, 0) of
	[] ->
	    Created = alh_service:get_log_created(),
	    mnesia:dirty_write(#?MNESIA_TAB{logNo = 0, logRec = Created});
	_ ->
	    ok
    end.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
-spec instPhParallel_post_init() -> ok.
instPhParallel_post_init() ->
    ok = logI:register_esi_dir(alh_service:default_log_dir()),
    ok = logI:register_esi_cb(alhI),
    ok = cec:register(<<"AVLI">>, alh_service).

-spec activate() -> ok.
activate() ->
    alh_service:activate(),
    ok.

-spec children() -> {ok, list()}.
children() ->
    {ok, [{alh_service, {alh_service, start_link, []},
	   permanent,
	   1000,
	   worker,
	   [alh_service]}]}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
