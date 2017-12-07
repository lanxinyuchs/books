%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysDataInit.erl %
%%% @author etxlg
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R11A/2
%%%
%%% @doc ==Initialization of SYS==

-module(sysDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R11A/2').
-date('2017-10-06').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%% R1A/1      2012-01-26 etxjotj     Created
%%% R1A/3      2012-06-18 etxlg       Added sysNetloader -server
%%% R1A/4      2012-07-11 etxjotj     Removed sys_error_logger
%%% R2A/8      2013-08-30 etxarnu     Register rcs/comte to logEsi
%%% R2A/9      2014-01-22 etxlg       sysSsh -stuff -> OMC
%%% R2A/10     2014-02-07 etxarnu     Register rcs/dumps to logEsi
%%% R2A/11     2014-08-13 etxtory     Removed sysNetloader call
%%% RaA/1      2014-09-11 etxpejn     Added sysFi
%%% R3A/2      2014-11-25 etxberb     Added sysUtil:activate().
%%% R3A/3      2015-01-14 erarafo     Launching sysFi with start_link
%%% R3A/4      2015-01-29 etxarnu     Register rcs/networkloader to logEsi
%%%                                   New sysVariables table created
%%% R3A/5      2015-03-11 etxtory     Removed called to sysWeb
%%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-04-17 etxpejn  Merge from R3 branch
%%% R4A/6   2015-05-19 etxpejn  Added init_board
%%% R4A/7   2015-06-09 etxarnu  Removed sysDhcpd in children
%%% R4A/8   2015-06-22 etxarnu  Added /rcs/sasl to esi
%%% R4A/9   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/10  2015-07-21 etxjotj  Disk space check
%%% R4A/11  2015-08-13 etxtory  Removed sysTftpd from children (opens port without IP)
%%% R4A/12  2015-09-13 etxarnu  Added sysTftpd to children again
%%% R4A/13  2015-09-13 etxarnu  Removed sysTftpd to children again
%%% R4A/14  2015-09-07 etxtory  ESI dirs updated
%%% R4A/16  2015-09-14 etxarnu  Bug fix in children()
%%% R4A/17  2015-09-21 etxarnu  temporarily removed sysItcLnh fomr childspec
%%%                             on core
%%% R4A/18  2015-09-23 etxpeno  re-added sysItcLnh to childspec on core
%%% R4A/19  2015-09-28 etxtory  Creating /rcs/erlang_disk dir
%%% R4A/20  2015-09-29 eolaand  Add call to sysFi:data_init()
%%% R4A/21  2015-09-30 etxpeno  Correction when create /rcs/erlang_disk dir
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2015-11-20 etxpeno  Add call to appmI:register_warm_cb/1
%%% R5A/2   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R5A/3   2016-02-02 etxarnu  Corrected erlang_disk dir for sim
%%% R5A/4   2016-02-08 etxtory  Corrected above
%%% R5A/5   2016-04-25 etxtory  Register sys as ESI-dir; used by sysServer
%%% R6A/1   2016-05-26 etxarnu  Added sysTftpd in children for vrcs
%%% R8A/1   2016-12-13 uabesvi  Moved register_esi_dir for comte to comsaDataInit
%%% ----    ---------- -------  ------------------------------------------------
%%% R11A/1  2017-08-28 etxpeno  add call to sysOei:init_data/0
%%% R11A/2  2017-10-06 etxlg    add call to sysXcm:init_data/0
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_init_board/0,
	 instPhParallel_post_init/0]).
-export([children/0, restart_strategy/0]).
-export([activate/0]).

-include("sys.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([add_clh_option/1]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(sysVariables,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, sysVariables)} |
				  add_clh_option(sysVariables)]),
    ok = sysOei:init(DbNodes),
    ok = sysWeb:init(DbNodes),
    ok = sysFi:init_tables(DbNodes),
    ok = sysServer:datainit(DbNodes).

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
instPhParallel_init_data() ->
    ok = sysWeb:init_data(),
    ok = sysXte:init_data(),
    ok = sysFi:init_data(),
    ok = sysOei:init_data(),
    ok = sysXcm:init_data(),
    ok = logI:register_esi_dir(
	   filename:join([sysEnv:rcs_dir(), "dumps"])),
    ok = logI:register_esi_dir(
	   filename:join([sysEnv:rcs_dir(), "networkloader"])),
    ok = logI:register_esi_dir(
	   filename:join([sysEnv:rcs_dir(), "erlang_disk"])),
    ok = logI:register_esi_dir(
	   filename:join([sysEnv:rcs_dir(), "sys"])),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_board() ->
    ok = sysXte:init_board(),

    EDir = filename:join([sysEnv:rcs_dir(), "erlang_disk", "dummy"]),
    ok = filelib:ensure_dir(EDir),

    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_post_init() ->
    ok = sysOei:register_cec(),
    ok = sysTmmiServer:register_cec(),
    ok = appmI:register_warm_cb(sysItcLnh).

restart_strategy() ->
    {ok, {one_for_one, 0, 1}}.

children() ->
    Role = clhI:mp_role(),
    IsDus = sysEnv:is_dus(),
    CoreSpec=[
	      {sysServer, {sysServer, start_link, []},
	       permanent, 5000, worker, [sysServer]},
	      {sysNetloader, {sysNetloader, start_link, []},
	       permanent, 1000, worker, [sysNetloader]},
	      {sysFi, {sysFi, start_link, []},
	       permanent, 1000, worker, [sysFi]},
	      {sysNetloaderTls, {sysNetloaderTls, start_link, []},
	       permanent, 1000, worker, [sysNetloaderTls]},
	      {sysTftpd, {sysTftpd, start_link, []},
	       permanent, 1000, worker, [sysTftpd]},
	      {sysDhcpd, {sysDhcpd, start_link, []},
	       permanent, 1000, worker, [sysDhcpd]},
	      {sysItcLnh, {sysItcLnh, start_link, []},
	       permanent, 1000, worker, [sysItcLnh]},
	      {sysTmmiServer, {sysTmmiServer, start_link, []},
	       permanent, 1000, worker, [sysTmmiServer]}
	     ],
    TcuSpec=[
	     {sysServer, {sysServer, start_link, []},
	      permanent, 5000, worker, [sysServer]},
	     {sysNetloader, {sysNetloader, start_link, []},
	      permanent, 1000, worker, [sysNetloader]},
	     {sysFi, {sysFi, start_link, []},
	      permanent, 1000, worker, [sysFi]}
	    ],
    RegularSpec=[
		 {sysServer, {sysServer, start_link, []},
		  permanent, 5000, worker, [sysServer]},
		 {sysNetloader, {sysNetloader, start_link, []},
		  permanent, 1000, worker, [sysNetloader]},
		 {sysFi, {sysFi, start_link, []},
		  permanent, 1000, worker, [sysFi]},
		 {sysItcLnh, {sysItcLnh, start_link, []},
		  permanent, 1000, worker, [sysItcLnh]},
		 {sysTmmiServer, {sysTmmiServer, start_link, []},
		  permanent, 1000, worker, [sysTmmiServer]}
		],
    {ok, case IsDus of
	     true when Role == core -> CoreSpec;
	     true -> RegularSpec;
	     false -> TcuSpec ++ if_vrcs(Role)
	 end }.


activate() ->
    sysTftpd:activate(),
    sysUtil:activate(),
    sysServer:activate().

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
if_vrcs(core) ->
    case sysEnv:vrcs() of
	true ->
	    [{sysTftpd, {sysTftpd, start_link, []},
	       permanent, 1000, worker, [sysTftpd]}];
	_ -> []
    end;
if_vrcs(_) -> [].
