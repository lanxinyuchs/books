%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logEsi.erl %
%%% Author:	etxjotj
%%% Description:
%%%
%%% @copyright Ericsson AB 2012-2017
%%% @doc
%%% This module controls the generation of ESI files.
%%% 
%%% == Database ==
%%%
%%% It uses a DB 'logEsi' to store information about applications
%%% that have registered a callback to be invoked at ESI,
%%% the same table contains also information what directories
%%% are to be included in the ESI.
%%% 
%%% == ESI files ==
%%% 
%%% When the exportEsi action is invoked in ME=1,SF=1,LogM=1
%%% one ESI file is generated for the DU, and one ESI per RU.
%%% 
%%% The DU ESI is entirely controlled by LOG block, while the ESI RU
%%% uses LOG_ESI ITC interface towards CAT, refer to csrc/log_esi.sig
%%% 
%%% The signal flow for an RU ESI is as follows:
%%% 
%%% <pre>
%%%    OPERATOR     RCS                                               CAT
%%% 
%%%                   &lt;----- LOG_ESI_EXT_CONN_ESTABLISH_UREQ -----------
%%%    -- exportEsi --&gt;
%%%                   ------ LOG_ESI_EXT_GET_BOARDS_RREQ ---------------&gt;
%%%                   &lt;----- LOG_ESI_EXT_GET_BOARDS_RCFM ----------------
%%% 
%%%            ------ loop over all RUs
%%%            |
%%%            |      ------ LOG_ESI_EXT_GET_BOARD_INFO_RREQ -----------&gt;
%%%            |      &lt;----- LOG_ESI_EXT_GET_BOARD_INFO_[RCFM | RREJ] ---
%%%            |
%%%            |      transfer the file to requested destination
%%%            |
%%%            |      ------ LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ ----&gt;
%%%            |
%%%            ------ loop
%%% 
%%% </pre>
%%% 
%%% A request to generate an ESI file can also come from AIC,
%%% when going down to NetworkLoader, and also from VNFM in the cloud
%%% when an upgrade failed.
%%% 
%%% == Module sequence ==
%%% 
%%% To generate a sequence diagram from an ESI generataion
%%% with two faked RUs (the CAT stub code is found in logDbg)
%%% sysProfiler can be used.
%%% 
%%% Do the following in the Erlang shell:
%%% 
%%% load the modules to be included in the sequence:
%%% <pre>
%%% l(logEsi).
%%% l(logEsiLib).
%%% l(logGenerateEsi).
%%% l(logGenerateEsiExt).
%%% l(logEsiExtServer).
%%% </pre>
%%% 
%%% start the profiler
%%% <pre>
%%% sysProfiler:start().
%%% sysProfiler:trace(logEsi, l).
%%% sysProfiler:trace(logEsiLib, l).
%%% sysProfiler:trace(logGenerateEsi, l).
%%% sysProfiler:trace(logGenerateEsiExt, l).
%%% sysProfiler:trace(logEsiExtServer, l).
%%% sysProfiler:trace(start).
%%% </pre>
%%% 
%%% start the CAT simulator by invoking
%%% <pre>
%%% logDbg:itc_start(justus).
%%% </pre>
%%% 
%%% to expertEsi run this test suite (not from Erlang shell)
%%% <pre>
%%% $RCT_TOP/test/bin/rct_run.sh -sim uabesvi_tmp -dir /vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/LOG/LOG_CNX9012616/test/suites -suite log_esi_SUITE.erl -case transfer_esi
%%% </pre>
%%% 
%%% generate the results
%%% <pre>
%%% sysProfiler:trace(stop).
%%% sysProfiler:analyse().
%%% 
%%% sysProfiler:print(1500).
%%% 
%%% sysProfiler:seq(). 
%%% sysProfiler:sort(). 
%%% sysProfiler:count(). 
%%% 
%%% sysProfiler:stop().
%%% </pre>
%%% 
%%% Below is a basic sequence chart for exportEsi action.
%%% The sequence is started from logServer wich spawns a process
%%% to handler the ESI generatation.
%%% 
%%% <pre>
%%% -- PREPARE
%%% logEsi:transfer_esi()
%%%   logEsi:transfer_esi_uri()
%%%     ftpI:start_channel_with_alt()
%%%   logEsi:transfer_esi_channel()
%%%   logEsi:set_esi_ongoing()
%%%   logEsi:transfer_an_esi()
%%%     logEsi:do_transfer_esi()
%%%       -- CHECK DISK SPACE ETC
%%%       -- START GENERATING DU ESI
%%%       logGenerateEsi:generate_esi()
%%%       -- INVOKE ALL CALLBACKS
%%%       logGenerateEsi:do_generate_esi_cb()
%%%         -- CHECK WHAT DIRECTORIES ARE TO BE INCLUDED
%%%         logEsiLib:limit_dirs() 
%%%         logEsiLib:pack_and_compress_dirs()
%%%       logEsi:do_export_esi()
%%%       logEsi:dee_upload_file()
%%%       -- UPLOAD THE FILE
%%%       logEsi:open_remote_file()
%%%       logEsi:upload_file()
%%%       -- INVOKE POST CALLBACK FUNCTIONS
%%%       logEsi:do_generate_esi_post()
%%% 
%%%   -- LOOPING OVER EXTERNAL BOARDS, I.E. RUs
%%%   logEsi:transfer_esi_ext()
%%%     logEsiExtServer;get_ext_boards()
%%%       logEsi:transfer_esi_ext_loop()
%%%       logEsi:transfer_an_esi()
%%%       logEsi:do_transfer_esi()
%%%          -- CHECK DISK SPACE ETC
%%%          -- START GENERATING DU ESI
%%%         logGenerateEsiExt;generate_esi_ext()
%%%         logEsiExtServer:get_ext_board_info()
%%%         logGenerateEsiExt:generate_esi()
%%%           logGenerateEsiExt,generate_file()
%%%             logEsiLib:pack_and_compress_dirs()
%%%           -- UPLOAD THE FILE
%%%           logEsi:open_remote_file()
%%%           logEsi:dee_upload_file()
%%%           logEsi:dee_pack_and_export()
%%%         logEsiExtServer:ext_esi_ready()
%%%       -- INVOKE POST CALLBACK FUNCTIONS
%%%       logEsi:do_generate_esi_post()
%%% 
%%% </pre>
%%% 
%%% 
%%% @end
%%% ----------------------------------------------------------
-module(logEsi).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/5').
-date('2017-11-20').
-author('uabesvi').
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
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2014-10-29 etxpeno     HT16841 add check of URI
%%% R3A/2      2014-11-24 etxtory     Added get_reg_esi_dirs
%%% R3A/3      2015-01-30 etxjotj     HT42962 Clear esi directory
%%% R3A/4      2015-02-25 etxarnu     Added rcs/erlang to get_reg_esi_dirs
%%% R3A/5      2015-03-04 etxarnu     Made tar ignore some more errors
%%% R3A/6      2015-03-05 etxjotj     Improved esi log printouts
%%% R3A/7      2015-04-02 etxjotj     HT63111 Reset logM PR
%%% R3A/8      2015-04-16 etxjotj     Clarified timeout response
%%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-04-17 etxpejn  Added support for ESI on dual DU
%%% R4A/2   2015-04-20 etxpejn  Added generate_esi_on_all_mps/0
%%% R4A/3   2015-04-21 etxpejn  Added throw when error during ESI
%%% R4A/4   2015-04-22 etxpejn  Corrected resultInfo when one ESI failes.
%%% R4A/6   2015-07-01 etxarnu  Don't crash if ssh_sftp:write fails
%%% R4A/7   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/8   2015-07-21 etxjotj  Added is_esi_ongoing
%%% R4A/9   2015-08-05 etxtory  Handle badrpc for generate_esi
%%%                             Added disk space check
%%% R4A/10  2015-08-27 etxasta  Added esi encrypt (code not active)
%%% R4A/11  2015-08-27 etxtory  Added granularity
%%% R4A/12  2015-08-28 etxasta  Added esi encrypt test licence check
%%%                             (code not active)
%%% R4A/13  2015-09-01 etxtory  logEsi:generate_esi fix
%%% R4A/15  2015-09-14 etxlg    Alternate connection
%%% R4A/16  2015-09-15 etxtory  Handling shortage of RAM
%%% R4A/17  2015-09-21 etxtory  /tmp not working for simulated env
%%% R4A/22  2015-10-09 etxjotj  Removed warning msg. Added progress reporting.
%%% R4A/23  2015-10-12 etxpeno  fix typo in is_esi_ongoing/0
%%% R4A/24  2015-10-13 etxarnu  changed error to warning in get_size
%%% R4A/25  2015-10-18 etxtory  Only follow symlinks for /rcs/erlang
%%% R4A/26  2015-10-19 etxtory  Corrected warning_msg
%%% R4A/27  2015-10-20 etxtory  RAM-usage in esi.log when not enough RAM
%%% R4A/28  2015-10-26 etxarnu  Don't check disk size in sim environment
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2015-11-02 etxpeno  Support for ESI in cluster
%%% R5A/2   2016-02-04 etxpeno  check the sftp connection before generating ESI
%%% R5A/3   2016-02-15 etxpejn  Changed call to module lmaI
%%% R5A/4   2016-02-19 etxtory  tar and gz in one go (HU58936)
%%% R5A/5   2016-03-09 erarafo  Adjusted --transform patterns
%%% R5A/6   2016-04-15 etxpejn  Added code for encryption of ESI
%%% R5A/7   2016-04-26 etxpejn  Removed code for encryption of ESI :)
%%% R6A/1   2016-06-07 etxpejn  Added encryption of ESI
%%% R6A/1   2016-07-14 etxarnu  Added --dereference to tar commands
%%% R6A/2   2016-08-23 etxpejn  HV19012, added check in get_free_tmp
%%% ----------------------------------------------------------
%%% R7A/1   2016-05-27 etxpeno  WindRiver 8 workaround in get_free_tmp()
%%%                             More investigation is needed in this area
%%% R7A/2   2016-06-09 etxpeno  Added encryption of ESI
%%% R7A/3   2016-08-25 etxpeno  Added updates from R6A/2-3
%%% R7A/4   2016-10-03 uabesvi  HV26316 timeout in ESI cb
%%% R7A/6   2016-10-13 etxarnu  WP6081 : Added generate_rollback_esi
%%% ----------------------------------------------------------
%%% R8A/2   2016-11-24 uabesvi  Encrypted logs
%%% R8A/5   2016-12-13 erarafo  Hidden creation of mnesia dumps on /tmp 
%%% R8A/6   2017-01-03 uabesvi  Avli log moved to /tmp/log/alh from /rcs/alh
%%% R8A/7   2017-01-16 eivmiha  switched ssh_sftp and sysSftp to ftpI
%%% ----------------------------------------------------------
%%% R9A/1   2017-01-31 emarnek  Added support for ftpes
%%% R9A/3   2017-03-28 etxarnu  Removed --dereference to not duplicate dspdumps
%%% R9A/4   2017-04-11 uabesvi  Added granularity 'static'
%%% R9A/5   2017-04-11 etxtory  Windriver 8 command free changed
%%%                             esi name changed for vrcs (du1 not so nice)
%%% ----------------------------------------------------------
%%% R10A/2   2017-05-16 etxjotj  Use sysEnv:rcs_mode_2
%%% R10A/3-9 2017-05-16 uabesvi  Cleanup of the code, no changes in the old code
%%%                              Prep for ext board esi
%%% ----------------------------------------------------------
%%% R11A/1   2017-08-30 uabesvi  RU ESI additions
%%% R11A/3   2017-10-03 uabesvi  RU ESI fix wrong no of args in do_transfer_esi
%%%                              when calling generate_esi in multi DU
%%% ----------------------------------------------------------
%%% R12A/1   2017-10-26 uabesvi  fail handling updated
%%% R12A/3   2017-10-31 uabesvi  generate RU ESI only on active core 
%%% R12A/4   2017-11-08 uabesvi  HW37113 ESI during warm restarts
%%% R12A/6   2017-11-20 uabesvi  removde 'export complete' from add info
%%% ---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ---------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% ESI registration
-export([register_esi_cb/1, 
	 register_esi_cb/2, 
	 unregister_esi_cb/1,
	 register_esi_dir/1, 
	 unregister_esi_dir/1,
	 get_reg_esi_dirs/0]).

%% Initialize data (installation)
-export([init_tables/1]).

%% Misc functions
-export([is_esi_ongoing/0]).

%% Operator action COM-CLI/exportEsi
-export([transfer_esi/3, 
	 transfer_esi/4]).

%% called from logWeb (only in cloud when an upgrade has failed)
-export([generate_esi/2]).

%% Called by AIC when going back to NL at AI failure.
-export([generate_esi/0]).

%% Called by SWM when before rollback (upgrade or failsafe).
-export([generate_rollback_esi/0]).

%% Must be called at active core (du1)
%% Called by APPM when starting a rollback
-export([generate_esi_on_all_mps/0]).

%% Called by AIC for emergency access
-export([generate_esi/3]).
-export([pack_esi/3]).

%% Test function(s)
-export([get_free_tmp/0]).
-export([get_size_dirs/1]).
-export([get_size/1]).
-export([get_chunks/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_cb_fun/2, generate_cb_fun/3]).
-export([generate_esi_ext/3]).
-export([handle_fail/1]).

%% for testing
-export([transfer_esi_ext/4]).

-export([get_fru_type/0]).

-include("log.hrl").
-include("RcsLogM.hrl").
-include_lib("kernel/include/file.hrl").

-define(MAX_UPLOAD_TIME, 300000).
-define(MAX_WRITE_TIME,   60000).

-define(MIN_TMP_SIZE_MB, 100).

%% Default ESI callback timeout = 10 minutes
-define(DEFAULT_ESI_CB_TIMEOUT,  600).

%% Print a warning in erlang log after this time in seconds
-define(WARNING_ESI_CB_TIME,  150). 

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%============================================================================
%% @doc
%% register esi callback function
%% @end
%%============================================================================

register_esi_cb(Module) ->
    register_esi_cb(Module, ?DEFAULT_ESI_CB_TIMEOUT).

register_esi_cb(Module, Timeout) 
  when is_atom(Module) andalso 
       is_integer(Timeout) ->
    F = fun() ->
		[{logEsi, cb, Callbacks}] =  mnesia:wread({logEsi, cb}),
		Modules = [M || {M, _} <- Callbacks],
		%% HS91155
		case lists:member(Module, Modules) of
		    true ->
			ok;
		    false ->
			mnesia:write({logEsi, 
				      cb, 
				      [{Module, Timeout * 1000} | Callbacks]})
		end
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok;
register_esi_cb(Mod, TO) ->
    Error = logLib:choose(is_atom(Mod), 
			  {not_an_integer, TO}, 
			  {not_an_atom,    Mod}),
    sysInitI:error_report([{mfa, {?MODULE, register_esi_cb, [Mod, TO]}},
			   Error,
			   erlang:get_stacktrace()]).


%%============================================================================
%% @doc unregister esi callback functions
%%
%% 
%% @end
%%============================================================================

unregister_esi_cb(Module) ->
    F = fun() ->
		[{logEsi, cb, Modules}] = mnesia:wread({logEsi, cb}),
		NewModules = lists:delete(Module, Modules),
		mnesia:write({logEsi, cb, NewModules})
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.


%%============================================================================
%% @doc register esi directory
%%
%% 
%% @end
%%============================================================================
register_esi_dir(Dir) when is_list(Dir) ->
    F = fun() ->
		[{logEsi, dir, Dirs}] =  mnesia:wread({logEsi, dir}),
		%% HS91155
		case lists:member(Dir, Dirs) of
		    true ->
			ok;
		    false ->
			mnesia:write({logEsi, dir, [Dir|Dirs]})
		end
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok;
register_esi_dir(Any) ->
    sysInitI:error_report([{mfa, {?MODULE, register_esi_dir, [Any]}},
			   {not_a_string, Any},
			   erlang:get_stacktrace()]).


%%============================================================================
%% @doc unregister ESI directory
%%
%% 
%% @end
%%============================================================================
unregister_esi_dir(Dir) ->
    F = fun() ->
		[{logEsi, dir, Dirs}] = mnesia:wread({logEsi, dir}),
		NewDirs = lists:delete(Dir, Dirs),
		mnesia:write({logEsi, dir, NewDirs})
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.


%%============================================================================
%% @doc get registered ESI directories
%%
%% 
%% @end
%%============================================================================
get_reg_esi_dirs() ->
    ErlangDir = filename:join(sysEnv:rcs_dir(), "erlang"),
    [{logEsi, dir, Dirs}] = mnesia:dirty_read({logEsi, dir}),
    [ErlangDir|Dirs].



%%============================================================================
%% @doc transfer ESI
%%
%% - check the validity of the URL  
%% - start channel to remote node
%% - generate ESI for the vRC/DU
%% - generate ESIs for the external boards
%% 
%% ESI for each board will generate unique file.
%% The files are transferred separately to the remote ftp server..
%% 
%% All of the ESIs must be successful to mark 
%% the exportEsi action to be successful.
%%  
%% @end
%%============================================================================
transfer_esi(Url, Password, ActionId) ->
    transfer_esi(Url, Password, _Granularity = undefined, ActionId).

transfer_esi(Url, Password, Granularity, ActionId) ->
    init_progress(ActionId),
    transfer_esi_uri(logServer:get_warm_restart_state(),
		     parse_uri(ftpI:parse_uri(Url)),
		     Password, 
		     Granularity, 
		     ActionId).


%%-----------------------------------------------------------
%% Check that the remote URL is valid
%%-----------------------------------------------------------
transfer_esi_uri(false,
		 {ok, {Proto, User, Host, Port, _RemoteDir} = ProtoData},
		 Password, 
		 Granularity, 
		 ActionId) ->
    transfer_esi_channel(logServer:get_warm_restart_state(),
			 start_channel(Proto, Host, Port, User, Password),
			 Granularity, 
			 ActionId, 
			 ProtoData);
transfer_esi_uri(false, {error, Error}, _Password, _Granularity, _ActionId) ->
    handle_fail(Error),
    ProgressFin = [{state, ?ActionStateType_FINISHED}],
    logEsiLib:update_progress(ProgressFin);
transfer_esi_uri(true, _Uri, _Password, _Granularity, _ActionId) ->
    handle_fail("Warm restart ongoing."),
    ProgressFin = [{state, ?ActionStateType_FINISHED}],
    logEsiLib:update_progress(ProgressFin).


%%-----------------------------------------------------------
%% Check that the channel could be started
%% Start generating the ESI for the DUs and
%% then generate ESIs for the possible RU borads.
%%-----------------------------------------------------------
transfer_esi_channel(false,
		     {ok, FtpPid, CRef},
		     Granularity, 
		     _ActionId, 
		     {Proto, _User, _Host, _Port, _RemoteDir} = ProtoData) ->
    set_esi_ongoing(),
    
    MpId = clhI:mp_id(),

    AddInfo1 = "Checking " ++ atom_to_list(Proto) ++ " server",
    logEsiLib:update_progress([{additionalInfo, AddInfo1}]),
    
    AddInfo2 = "Generating ESI for MpId: " ++ integer_to_list(MpId),
    logEsiLib:update_progress([{additionalInfo, AddInfo2}]),
    
    %% Generate ESI for DU
    transfer_an_esi({logGenerateEsi, generate_esi},
		    MpId,
		    Granularity, 
		    FtpPid,
		    ProtoData),
    %% Generate ESI for RUs
    transfer_esi_ext(get_fru_type(),
		     Granularity, 
		     FtpPid,
		     ProtoData),
    
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    ProgressFin  = [{result,              ?ActionResultType_SUCCESS},
		    {state,               ?ActionStateType_FINISHED},
		    {timeActionCompleted, CompleteTime}],
    logEsiLib:update_progress(ProgressFin),
    
    case get(localPath) of
	{path, LocalPath} ->
	    AllNodes = clhI:erlang_nodes(all),
	    Dir      = filename:dirname(LocalPath),
	    rpc:multicall(AllNodes, os, cmd, [["rm -rf ", Dir]]);
	undefined ->
	    ok
    end,
    
    stop_channel(FtpPid, CRef, Proto),
    remove_esi_ongoing(),
    ok;
transfer_esi_channel(false,
		     {error, _Error},
		     _Granularity, 
		     _ActionId, 
		     _ProtoData) ->
    ProgressInfo = "The action could not be completed. "
	"Could not start FTP channel.",
    handle_fail(ProgressInfo),
    ProgressFin = [{state, ?ActionStateType_FINISHED}],
    logEsiLib:update_progress(ProgressFin);
transfer_esi_channel(true, _Channel, _Gran, _ActionId, _ProtoData) ->
    handle_fail("Warm restart ongoing."),
    ProgressFin = [{state, ?ActionStateType_FINISHED}],
    logEsiLib:update_progress(ProgressFin).




%%-----------------------------------------------------------
%% Generate and transfer an ESI
%%
%% If an error occurs, mark that the action exprotEsi 
%% is failed, but continue to generate ESIs for the 
%% remaining boards.
%% All ESIs must be successful to set the ESI result to 
%%-----------------------------------------------------------
transfer_an_esi(EsiType, Node, Granularity, FtpPid, ProtoData) ->
    try
	ok = do_transfer_esi(EsiType, 
			     Node,
			     Granularity, 
			     FtpPid,
			     ProtoData),
	AddInfo = "Export SUCCESS " ++ tea_get_node_as_string(Node),
	logEsiLib:update_progress([{additionalInfo, AddInfo}])
    catch
	throw:{fail, ProgressInfo} ->
	    handle_fail(ProgressInfo);
	throw:_ ->
	    ProgressInfo = "The action could not be completed",
	    handle_fail(ProgressInfo);
	Type:Reason ->
	    sysInitI:error_report([{Type, Reason}, erlang:get_stacktrace()]),
	    Size = get_free_tmp(),
	    handle_fail(tea_get_prog_info(Size))
    end,
    ok.

%% DU
tea_get_node_as_string(Node) when is_integer(Node) ->
    "MpId: " ++ integer_to_list(Node);
%% RU
tea_get_node_as_string({BoardId, _}) ->
    "RuId: " ++ BoardId.

tea_get_prog_info(Size) when Size < ?MIN_TMP_SIZE_MB ->
    "There is only " ++ integer_to_list(Size) ++ 
	"M free RAM or /tmp disk. "
	"Perform a cold restart to clear space for "
	"the ESI collection or contact the next "
	"level of support";
tea_get_prog_info(_) ->
    "Failed to collect ESI, please try again".    


%%-----------------------------------------------------------
%% Loop over the external boards 
%% 
%% ESI for external boards is only generated on the active core node
%%-----------------------------------------------------------
transfer_esi_ext(central, Granularity, FtpPid, ProtoData) ->
    tee(logEsiExtServer:get_ext_boards(),
	Granularity, 
	FtpPid,
	ProtoData);
transfer_esi_ext(_, _, _, _) ->
    ok.

tee({ok, Boards, Mbox}, Granularity, FtpPid, ProtoData) ->
    transfer_esi_ext_loop(logServer:get_warm_restart_state(),
			  Boards,
			  Mbox,
			  Granularity, 
			  FtpPid,
			  ProtoData);
tee(_, _, _, _) ->
    ProgressInfo = "Could not collect RU board identities",
    handle_fail(ProgressInfo).



transfer_esi_ext_loop(true, _Boards, _Mbox, _Granularity, _FtpPid, _ProtoData) -> 
    handle_fail("Warm restart ongoing."),
    ProgressFin = [{state, ?ActionStateType_FINISHED}],
    logEsiLib:update_progress(ProgressFin);
transfer_esi_ext_loop(false, [], _Mbox, _Granularity, _FtpPid, _ProtoData) -> 
    ok;
%%transfer_esi_ext([{{BoardNr, _BoardRev}, Mbox} | T], 
transfer_esi_ext_loop(false,
		      [BoardNr | T],
		      Mbox,
		      Granularity, 
		      FtpPid,
		      ProtoData) -> 
    AddInfo = "Generating ESI for RuId: " ++ BoardNr,
    logEsiLib:update_progress([{additionalInfo, AddInfo}]),
    transfer_an_esi({logGenerateEsiExt, generate_esi_ext}, 
		    {BoardNr, Mbox}, 
		    Granularity, 
		    FtpPid,
		    ProtoData),
    transfer_esi_ext_loop(logServer:get_warm_restart_state(),
			  T, 
			  Mbox, 
			  Granularity, 
			  FtpPid, 
			  ProtoData).


%%============================================================================
%% @doc generate esi on all MPs
%%
%% 
%% @end
%%============================================================================
generate_esi_on_all_mps() ->
    AllNodes = clhI:erlang_nodes(all),
    rpc:multicall(AllNodes, ?MODULE, generate_esi, []).

generate_esi() ->
    generate_esi(_Granularity = undefined, _Mode = complete).

generate_esi(Granularity, Mode) ->
    MpId = clhI:mp_id(),
    generate_esi(MpId, Granularity, Mode).

generate_esi(MpId, Granularity, Mode) ->
    EsiDir = logEsiLib:make_esi_dir(),
    try logGenerateEsi:generate_esi(EsiDir, MpId, Granularity, Mode)
    catch T:E ->
	    cmd(["rm -rf ", EsiDir]),
	    erlang:T(E)
    after
	case get(fd) of
	    Fd when is_pid(Fd) -> file:close(Fd);
	    undefined          -> ok
	end
    end.


generate_esi_ext(RuData, Granularity, Mode) ->
    EsiDir = logEsiLib:make_esi_dir(),
    try logGenerateEsiExt:generate_esi_ext(EsiDir, RuData, Granularity, Mode)
    catch T:E ->
	    cmd(["rm -rf ", EsiDir]),
	    erlang:T(E)
    after
	case get(fd) of
	    Fd when is_pid(Fd) -> file:close(Fd);
	    undefined          -> ok
	end
    end.



generate_rollback_esi() ->
    try do_generate_rollback_esi()
    catch T:E ->
	    erlang:T(E)
    after
	case get(fd) of
	    Fd when is_pid(Fd) -> file:close(Fd);
	    undefined          -> ok
	end
    end.


%%========================================================================
%% pack and export ESI
%%========================================================================
pack_esi(Chunk, No, EsiFileBase) ->
    EsiDir = logEsiLib:make_esi_dir(),
    EsiFile = filename:join(EsiDir, EsiFileBase),
    logEsiLib:pack_and_compress_dirs(Chunk, 
				     EsiFile, 
				     _LogFile = [], 
				     _InclErlDir = false),
    NewFn = EsiFile ++ "." ++ integer_to_list(No),
    file:rename(EsiFile, NewFn),
    NewFn.


%%============================================================================
%% @doc check if is ESI ongoing
%%
%% 
%% @end
%%============================================================================
is_esi_ongoing() ->
    Fun = fun() -> mnesia:read({logRamVariables, esi_ongoing}) end,
    {atomic, true} == mnesia:transaction(Fun).


%%============================================================================
%% @doc initialize tables
%%
%% 
%% @end
%%============================================================================
init_tables(DbNodes) ->
    Opts = [{type,        set},
	    {disc_copies, DbNodes},
	    {attributes,  [key, value]} |
	    logDataInit:add_clh_option(logEsi)],
    {atomic, ok} = clhI:mnesia_create_table(logEsi, Opts),

    mnesia:dirty_write({logEsi, cb,  []}),
    mnesia:dirty_write({logEsi, dir, []}).



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%=======================================================================
%% 
%%=======================================================================
%% This is for testing error cases.
%% do_transfer_esi({EsiMod, EsiFunc}, 
%% 		{"berit", _}, 
%% 		Granularity, 
%% 		FtpPid,
%% 		{Proto, _User, _Host, _Port, RemoteDir}) ->
%%     throw({fail, "berit failed"});
do_transfer_esi({EsiMod, EsiFunc}, 
		Node, 
		Granularity, 
		FtpPid,
		{Proto, _User, _Host, _Port, RemoteDir}) ->
    
    logEsiLib:update_progress([{additionalInfo, "Generating information"}]),

    %% Information: 
    %% For WR6 dus32 cannot allocate all ram and trying using ram less 
    %% than 200Mb causes node to reboot. Due to this 250MB was used as
    %% a safety margin.
    %% WR8 is not having this problem for dus32 so this safety margin
    %% is now removed.
    FreeTmpSize = get_free_tmp(),

    case FreeTmpSize of
	Size when Size < ?MIN_TMP_SIZE_MB ->
	    Msg1 = "There is only " ++
		integer_to_list(Size) ++
		"M free RAM or /tmp disk. "
		"Perform a cold restart to clear space for the ESI collection.",
	    throw({fail, Msg1});
	_ ->
	    ok
    end,
    
    %% Check if the active core MP has enough disk space to generate an ESI.
    %% The other MPs are best effort but should work since there are less data
    %% there.
    case logEsiLib:check_disk_space() of
	ok ->
	    ok;
	{nok, RcsSize} ->
	    Msg2 = "Generating ESI failed due to disk space shortage. "
		"Remove backups or old upgrade packages to make room. "
		"100M is needed and " ++ RcsSize ++ "M is available.",
	    throw({fail, Msg2})
    end,
    
    case Granularity of
	large ->
	    logEsiLib:update_progress([{additionalInfo, "Large ESI"}]);
	undefined ->
	    logEsiLib:update_progress([{additionalInfo, "Default large ESI"}]);
	small ->
	    logEsiLib:update_progress([{additionalInfo, "Small ESI"}]);
	static ->
	    logEsiLib:update_progress([{additionalInfo, "Static ESI"}]);
	_ ->
	    ok
    end,

    AllNodes = clhI:erlang_nodes(all),
    %% Node is only valid for RU ESI generation
    Args = logLib:choose(EsiMod == logGenerateEsi,
			 [Granularity, any],
			 [Node, Granularity, any]),
    {ResAllL, _BadNodes} = rpc:multicall(AllNodes, 
				      EsiMod, 
				      EsiFunc,
				      Args),

    ResL = dte_check_errors(ResAllL),

    logEsiLib:update_progress([{additionalInfo, "Transferring files"}]),
    ProgressLimits = calc_progress_limit(length(ResL)),

    BaseNames = lists:flatmap(
		  fun({A, ProgressLimit}) -> do_export_esi(FtpPid, 
							   Proto,
							   RemoteDir, 
							   A, 
							   ProgressLimit)
		  end, 
		  lists:zip(ResL, ProgressLimits)),

    %% Remove esi-dir created in generate_esi
    RcsEsiDir = sysEnv:rcs_dir() ++ "/esi/",
    os:cmd("rm -rf " ++ RcsEsiDir),

    %% Execute esi-post callbacks
    do_generate_esi_post(EsiFunc),
    
    ProgressFin = [{resultInfo,          string:join(BaseNames, "; ")},
		   {progressPercentage,  100}],
    logEsiLib:update_progress(ProgressFin),
    ok.

dte_check_errors(ResAllL) ->
    Fun = fun({error, _} = Res, {Ok, Error}) -> {Ok, [Res |  Error]};
	     (Res,              {Ok, Error}) -> {[Res | Ok], Error}
	  end,
    {ResL, ErrL} = lists:foldl(Fun, {[], []}, ResAllL),
    [logEsiLib:update_progress([{additionalInfo, Info}]) || {_, Info} <- ErrL],
    ResL.

%%=======================================================================
%% 
%%=======================================================================
do_export_esi(FtpPid, 
	      Proto,
	      RemoteDir, 
	      {Node, Type, EsiFile},
	      ProgressLimit)
  when Type =:= ?ESI_VSN_LIMITED;
       Type =:= ?ESI_VSN_FULL ->
    %% ESI is limited or full but no chunks.
    Basename = dee_upload_file(FtpPid, 
			       Proto,
			       Node, 
			       EsiFile, 
			       RemoteDir, 
			       ProgressLimit),
    [Basename];
do_export_esi(FtpPid, 
	      Proto,
	      RemoteDir, 
	      {Node, ?ESI_VSN_CHUNK, EsiFile, Chunks},
	      ProgressLimit) ->
    %% ESI is not complete; export first chunk (EsiFile)
    NewEsiFile = EsiFile ++ ".1",
    file:rename(EsiFile, NewEsiFile),
    [ProgressLimitH | ProgressLimitT] = calc_progress_limit(ProgressLimit,
							    1 + length(Chunks)),
    BaseName    = dee_upload_file(FtpPid, 
				  Proto,
				  Node, 
				  NewEsiFile, 
				  RemoteDir, 
				  ProgressLimitH),
    EsiFileBase = filename:basename(EsiFile),
    BaseNames   = dee_pack_and_export(Chunks, 
				      ProgressLimitT, 
				      _No = 2, 
				      Node,
				      EsiFileBase, 
				      FtpPid, 
				      Proto,
				      RemoteDir,
				      _Acc = []),
    [BaseName|BaseNames].

dee_upload_file(FtpPid, Proto, Node, LocalPath, RemoteDir, ProgressLimit) ->
    put(localPath, {path, LocalPath}),
    dee_uf(FtpPid, Proto, RemoteDir, Node, LocalPath, ProgressLimit),
    rpc:call(Node, os, cmd, [["rm -rf ", filename:dirname(LocalPath)]]),
    filename:basename(LocalPath).

dee_uf(FtpPid, Proto, RemoteDir, Node, LocalPath, ProgressLimit) ->
    RemotePath = filename:join(RemoteDir, filename:basename(LocalPath)),
    %% Open the remote file
    {ok, Handle} = open_remote_file(Proto, FtpPid, RemotePath),
    put(handle, Handle),

    upload_file(Proto, FtpPid, Handle, Node, LocalPath, ProgressLimit),

    %% Cleanup
    ftpI:close(Proto, FtpPid, Handle).


dee_pack_and_export([Chunk | Chunks], 
		    [ProgressLimit | ProgressLimits], 
		    No, 
		    Node,
		    EsiFileBase, 
		    FtpPid, 
		    Proto,
		    RemoteDir, 
		    Acc) ->
    EsiDir = logEsiLib:make_esi_dir(),
    EsiFile = filename:join(EsiDir, EsiFileBase),
    logEsiLib:pack_and_compress_dirs(Chunk, 
				     EsiFile, 
				     _LogFile = [], 
				     _InclErlDir = false),
    NewFn = EsiFile ++ "." ++ integer_to_list(No),
    file:rename(EsiFile, NewFn),
    BaseName = dee_upload_file(FtpPid, 
			       Proto,
			       Node, 
			       NewFn, 
			       RemoteDir, 
			       ProgressLimit),
    dee_pack_and_export(Chunks, 
			ProgressLimits, 
			No + 1, 
			Node, 
			EsiFileBase, 
			FtpPid,
			Proto,
			RemoteDir, 
			[BaseName | Acc]);
dee_pack_and_export([], [], _No, _Node, _Base, _FtpPid, _, _RemDir, Acc) ->
    Acc.


%%=======================================================================
%% 
%%=======================================================================
do_generate_rollback_esi() ->
    TS       = comsaI:iso_time(os:timestamp(), basic),
    Root     = sysEnv:rcs_root(),
    RbEsiDir = filename:join([Root, "rcs", "erlang_disk", "rollback"]),

    LogFileName   = "rollback_esi.log",
    LogFile       = filename:join(RbEsiDir, LogFileName),
    RbEsiFileName = "rollback_esi.tar.gz",
    RbEsiFile     = filename:join(RbEsiDir, RbEsiFileName),
    ok            = filelib:ensure_dir(LogFile),
    {ok, LogFd}   = file:open(LogFile, [write]),
    put(fd, LogFd),
    io:format(LogFd, "Generating Rollback ESI : ~s~n~n", [TS]),
    %%=======================================================
    %% Invoke Rollback ESI callbacks
    %% print a warning if the callbacks take very long time 
    %%=======================================================
    {ok, Ref} = timer:apply_after(?WARNING_ESI_CB_TIME * 1000, 
				  logEsiLib, 
				  print_esi_time_warning,
				  []),
    case do_generate_rollback_esi_cb(LogFd) of
	{ok,Results} ->
	    file:close(LogFd),
	    erase(fd),
	    Paths = [" "++Path ++" " || {_,_,Path} <- lists:flatten(Results)],
	    sysInitI:info_msg("Creating rollback ESI tar file, result  ~p, ~n",
		      [cmd("tar --ignore-failed-read  "
			   ++ " -cvzf "
			   ++ RbEsiFile ++ " " ++ Paths ++ LogFile)]);
	Error ->
	    sysInitI:info_msg("Failed to create rollback ESI tar file, "
			      "result  ~p, ~n",[Error])
    end,
    timer:cancel(Ref),
    ok.

%%=======================================================================
%% Invoke rollback_esi callbacks 
%%=======================================================================
do_generate_rollback_esi_cb(LogFd) ->    
    %% Run callbacks
    [{logEsi, cb, Callbacks}] = mnesia:dirty_read({logEsi, cb}),

    AllModules = [M || {M, _} <- Callbacks],
    Modules = [X || X <- AllModules, 
		    lists:member({generate_rollback_esi,0},
				 apply(X, module_info, [exports]))],
    io:format(LogFd, 
	      "~n~p generate_rollback_esi callback modules = ~p~n", 
	      [time(), Modules]),
    TimeOut = lists:max([T || {_, T} <- Callbacks]),
    Funs    = [{?MODULE, 
		generate_cb_fun, 
		[Module, generate_rollback_esi, LogFd]} || Module <- Modules],
    Opts    = [{timeout, TimeOut}, {results_order, chrono_reverse}],
    Result  = sysUtil:parallel_call(Funs, Opts),
    io:format(LogFd,
	      "~n~p generate_rollback_esi callback results = ~p~n", 
	      [time(), Result]),
    Result.

%%=======================================================================
%% Invoke esi-post callbacks
%%=======================================================================
do_generate_esi_post(generate_esi) ->
    [{logEsi, cb, Callbacks}] = mnesia:dirty_read({logEsi, cb}),
    {Funs, MaxTimeout} = 
	lists:foldl(
	  fun({Module, Timeout}, {MfaAcc, T} = Acc) ->
		  Exported = apply(Module, module_info, [exports]),
		  case lists:member({generate_esi_post, 0}, Exported) of
		      false ->
			  Acc;
		      true ->
			  {[{?MODULE,
			     generate_cb_fun,
			     [Module, generate_esi_post]}
			   | MfaAcc],
			   max(Timeout, T)}
		  end
	  end,
	  {[], 0},
	  Callbacks),
    Opts = [{timeout, MaxTimeout}, {results_order, chrono_reverse}],
    _Result = sysUtil:parallel_call(Funs, Opts),
    % sysInitI:info_msg("~n~p generate_esi callback results = ~p~n", 
    %                   [time(), Result]),
    ok;
do_generate_esi_post(generate_esi_ext) ->
    [{logEsi, cb, Callbacks}] = mnesia:dirty_read({logEsi, cb}),
    {Funs, MaxTimeout} = 
	lists:foldl(
	  fun({Module, Timeout}, {MfaAcc, T} = Acc) ->
		  Exported = apply(Module, module_info, [exports]),
		  case lists:member({generate_esi_post, 0}, Exported) of
		      false ->
			  Acc;
		      true ->
			  {[{?MODULE,
			     generate_cb_fun,
			     [Module, generate_esi_post]}
			   | MfaAcc],
			   max(Timeout, T)}
		  end
	  end,
	  {[], 0},
	  Callbacks),
    Opts = [{timeout, MaxTimeout}, {results_order, chrono_reverse}],
    _Result = sysUtil:parallel_call(Funs, Opts),
    % sysInitI:info_msg("~n~p generate_esi callback results = ~p~n", 
    %                   [time(), Result]),
    ok.



%%=======================================================================
%% Invoke esi callback fun without logging
%%=======================================================================
generate_cb_fun(Module, Func) ->
    case lists:member({Func, 0}, apply(Module, module_info, [exports])) of
	true ->
	    try apply(Module, Func, []) of
		Result ->
		    Result
	    catch T:E ->
		    ST = erlang:get_stacktrace(),
		    sysInitI:error_report([{T,E}, ST])
	    end;
	false ->
	    ok
    end.

%%=======================================================================
%% Invoke esi callback fun
%%=======================================================================
generate_cb_fun(Module, Func, LogFd) ->
    case lists:member({Func, 0}, apply(Module, module_info, [exports])) of
	true ->
	    io:format(LogFd,
		      "~n~p Calling ~w:~w()~n",
		      [time(), Module, Func]),
	    try apply(Module, Func, []) of
		Result ->
		    io:format(LogFd,
			      "~n~p Result ~w:~w() = ~p~n",
			      [time(), Module, Func, Result]),
		    Result
	    catch T:E ->
		    ST = erlang:get_stacktrace(),
		    io:format(LogFd,
			      "~n~p Error ~w:~w() = ~p~n~p~n", 
			      [time(), Module, Func, {T,E}, ST]),
		    sysInitI:error_report([{T,E}, ST])
	    end;
	false ->
	    io:format(LogFd,
		      "~n~p Callback function not exported ~w:~w()~n",
		      [time(), Module, Func]),
	    ok
    end.
    



%%========================================================================
%% channel handling
%%========================================================================
%%--------------------------------------------
%% start_channel
%%--------------------------------------------
start_channel(Proto, Host, Port, User, Passwd) ->
    Result = ftpI:start_channel_with_alt(Proto, 
					 Host, 
					 Port, 
					 User, 
					 Passwd,
					 [{timeout, ?MAX_UPLOAD_TIME}]),
    sc_rc(Result, Proto).


sc_rc({ok, SP, C}, _) ->
    put(channelPid,    SP),
    put(connectionRef, C),
    {ok, SP, C};
sc_rc({error, Error}, Proto) ->
    Info = sc_info(Error, Proto),
    logEsiLib:update_progress([{resultInfo, Info}]),
    {error, Error}.


sc_info("timeout", _) ->
    "Cannot establish a connection to remote server";
sc_info(List, _) when is_list(List)->
    List;
sc_info(etimedout, _) ->
    "Cannot establish a connection to remote server";
sc_info(ehost, _) ->
    "Cannot establish a connection to remote server";
sc_info(nxdomain, _) ->
    "Domain name not found";
sc_info(no_tls, _) ->
    "TLS parameters are not valid";
sc_info(Error, Proto) ->
    ftpI:format_error(Proto, Error).


%%--------------------------------------------
%% stop_channel
%%--------------------------------------------
stop_channel(FtpPid, undefined, ftpes) -> 
    ftpI:stop_channel(ftpes, FtpPid, undefined);
stop_channel(_, undefined, sftp) ->
    ok;
stop_channel(FtpPid, CRef, Proto) ->
    ftpI:stop_channel(Proto, FtpPid, CRef).



%%========================================================================
%% file handling
%%========================================================================
open_remote_file(Proto, Pid, RemotePath) ->
    case ftpI:open(Proto, Pid, RemotePath, [write]) of
	{ok, Handle} -> 
	    {ok, Handle};
	{error, Reason} ->
	    Info = orf_info(Reason, Proto),
	    logEsiLib:update_progress([{resultInfo, Info}]),
	    throw(error)
    end.

orf_info(no_such_file, _) -> "No such file or directory";
orf_info(ehost,        _) -> "Cannot establish a connection to remote server";
orf_info(reconf_error, _) -> "Client is stopped";
orf_info(no_proc,      _) -> "Client doesn't exists";
orf_info(Reason,   Proto) -> ftpI:format_error(Proto, Reason).
    


upload_file(Proto, Pid, Handle, Node, LocalPath, ProgressLimit) ->
    TotalSize = do_file_size(Node, LocalPath),
    AddInfo   = "Transferring " ++ filename:basename(LocalPath),
    logEsiLib:update_progress([{additionalInfo, AddInfo}]),
    case do_file_open(Node, LocalPath) of
	{ok, Fd, OpenPid} ->
	    do_upload_file(Proto,
			   Fd, 
			   Pid, 
			   Handle, 
			   0, 
			   TotalSize, 
			   ProgressLimit,
			   LocalPath,
			   file:read(Fd, 65536)),
	    do_file_close(Fd, OpenPid);
	{error, Reason} ->
	    File = filename:basename(LocalPath),
	    Msg  = File ++ " " ++ file:format_error(Reason),
	    logEsiLib:update_progress([{resultInfo, Msg}])
    end.

do_file_size(Node, LocalPath) ->
    rpc:call(Node, filelib, file_size, [LocalPath]).

do_file_open(Node, LocalPath) when Node == node() ->
    case file:open(LocalPath, [read, raw, binary]) of
	{ok, Fd} ->
	    Pid = undefined,
	    {ok, Fd, Pid};
	{error, Reason} ->
	    {error, Reason}
    end;
do_file_open(Node, LocalPath) ->
    Self = self(),
    Ref = make_ref(),
    F = fun() ->
		Res = {Ref, file:open(LocalPath, [read, binary])},
		Self ! Res,
		receive 
		    close -> 
			ok
		end
	end,
    Pid = spawn_link(Node, F),

    R = receive {Ref, X} -> X end,

    case R of
	{ok, Fd} ->
	    {ok, Fd, Pid};
	{error, Reason} ->
	    Pid!close,
	    {error, Reason}
    end.

do_file_close(Fd, Pid) ->
    R = file:close(Fd),
    case is_pid(Pid) of
	true  -> Pid ! close;
	false -> ok
    end,
    R.


%%========================================================================
%% 
%%========================================================================
do_upload_file(Proto, 
	       Fd, 
	       Pid, 
	       Handle, 
	       AccuSize, 
	       TotalSize, 
	       ProgressLimit,
	       LocalPath,
	       {ok, Data}) ->
    garbage_collect(),
    NewSize = size(Data),
    NewAccuSize = AccuSize + NewSize,
    case ftpI:write(Proto, Pid, Handle, Data, ?MAX_WRITE_TIME) of
	ok ->
	    Progress = calc_progress(AccuSize, TotalSize, ProgressLimit),
	    logEsiLib:update_progress([{progressPercentage, Progress}]),
	    do_upload_file(Proto, 
			   Fd, 
			   Pid, 
			   Handle, 
			   NewAccuSize, 
			   TotalSize,
			   ProgressLimit, 
			   LocalPath,
			   file:read(Fd, 65536));
	Error ->
	    Reason = ftpI:format_error(Proto, Error),
	    error_msg("ftpI:write failed with reason ~p~n"
		      "AccuSize=  ~p, NewSize=  ~p, TotalSize=  ~p~n",
		      [Reason, AccuSize, NewSize, TotalSize]),
	    error_msg("Failed to transfer ~p~n", [LocalPath]),
	    {error, write_failed}
    end;
do_upload_file(_, _, _, _, _, _, _, _, eof) ->
    ok;
do_upload_file(_, _, _, _, _, _, _, _, {error, Reason}) ->
    logEsiLib:update_progress([{resultInfo, file:format_error(Reason)}]),
    erlang:error(Reason).

%%========================================================================
%% handle_fail
%%========================================================================
handle_fail(ProgressInfo) ->
    PI = "FAILURE: " ++ ProgressInfo,
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    logEsiLib:update_progress([{result, ?ActionResultType_FAILURE},
			       {progressPercentage, 100},
			       {additionalInfo, PI},
			       {timeActionCompleted, CompleteTime}]).





%%==========================================================
%% Test function
%%==========================================================
get_chunks(Granularity) ->
    TmpLogFile = filename:join([sysEnv:tmp_dir(), "get_chunks"]),
    {ok, LogFd} = file:open(TmpLogFile, [write]),
    logEsiLib:print_granularity(LogFd, Granularity),
    [{logEsi, dir, Dirs}] = mnesia:dirty_read({logEsi, dir}),
    AllDirs = logEsiLib:limit_dirs(LogFd, Granularity, Dirs),
    R = logEsiLib:get_chunks(AllDirs, LogFd, 0, [], []),
    file:close(LogFd),
    file:delete(TmpLogFile),
    R.



%%=======================================================================
%% 
%%=======================================================================
calc_progress_limit(Count) ->
    calc_progress_limit({0.0, 1.0}, Count).

calc_progress_limit(_, 0) ->
    [];
calc_progress_limit({BeginProgress, EndProgress},
		    Count)
  when is_float(BeginProgress),
       is_float(EndProgress) ->
    Step = (EndProgress - BeginProgress) / Count,
    [{BeginProgress + (N - 1) * Step, BeginProgress + N * Step} ||
	N <- lists:seq(1, Count)].

calc_progress(Size, TotalSize, {BeginProgress, EndProgress}) ->
    SizeProgress = Size/TotalSize,
    trunc(100 * (BeginProgress + SizeProgress * (EndProgress - BeginProgress))).





%%=======================================================================
%% 
%%=======================================================================
parse_uri({ok, {sftp, User, Host, Port, RemoteDir, _Query}}) ->
    {ok, {sftp, User, Host, Port, RemoteDir}};
parse_uri({ok, {ftpes, User, Host, Port, RemoteDir, _Query}}) ->
    {ok, {ftpes, User, Host, Port, RemoteDir}};
parse_uri(_) ->
    {error, "Failed to parse the URI"}.





%%========================================================================
%% get_free_tmp() -> integer()
%% 
%% Information regarding free in WR6:
%% tmpfs is using real RAM but not counted in free.
%% This means that free really returns a incorrect value and the
%% real "free" value is as follows:
%% buffers/cache:free - "the used size of /tmp".
%% 
%% The above has been solved in WR8 free and the new column
%% available is giving a correct value (also including tmpfs).
%% See: http://www.linuxatemyram.com/ for more information.
%%========================================================================
get_free_tmp() ->
    logEsiLib:get_free_tmp(sysEnv:target()).


%%========================================================================
%% call to os:cmd
%%========================================================================
cmd(Cmd) ->
    os:cmd(Cmd).

%%========================================================================
%% Test function
%%========================================================================
get_size_dirs(Granularity) ->
    TmpLogFile  = filename:join([sysEnv:tmp_dir(), "get_size_dirs"]),
    {ok, LogFd} = file:open(TmpLogFile, [write]),
    [{logEsi, dir, Dirs}] = mnesia:dirty_read({logEsi, dir}),
    AllDirs = logEsiLib:limit_dirs(LogFd, Granularity, Dirs),
    file:close(LogFd),
    file:delete(TmpLogFile),
    logEsiLib:get_size_dirs(AllDirs, 0) / 1024.    


get_size(Dir) ->
    Res = os:cmd("du -sL " ++ Dir),
    case string:tokens(Res, "\t") of
	[Size | _] ->
	    %% Size is in kB
	    try list_to_integer(Size) of
		S -> {ok, S}
	    catch error:badarg ->
		    filter_unreadable(Res, Dir)
	    end;
	[] ->
	    warning_msg("Directory size did not return:~n~s~p~n",
			["du -sL ", Dir]),
	    nok
    end.

%% {ok, Size} | nok
filter_unreadable(DuStr, Dir) ->
    NlToks = string:tokens(DuStr, "\n"),
    filter_unreadable(NlToks, "Permission denied", Dir).

filter_unreadable([Tok | T], Expect, Dir) ->
    case re:run(Tok, Expect) of
	{match, _} ->
	    warning_msg("Directory size failed for ~p~n~s~n",
			[Dir, Tok]),
	    filter_unreadable(T, Expect, Dir);
	nomatch ->
	    case string:tokens(Tok, "\t") of
		[Size | _] ->
		    try list_to_integer(Size) of
			S -> {ok, S}
		    catch error:badarg ->
			    warning_msg("Directory size failed for ~p~n~s~n",
					[Dir, Tok]),
			    nok
		    end
	    end
    end;
filter_unreadable([], _Expect, _Dir) ->
    %% Should not happen
    nok.



init_progress(ActionId) ->
    %% Intiate progress report
    StartTime = comsaI:iso_time(os:timestamp(), extended),
    AddInfo   = "Checking URL",
    Progress  = [{actionName,          "Export ESI"},
		 {additionalInfoClear, AddInfo},
		 {progressPercentage,  0},
		 {result,              ?ActionResultType_NOT_AVAILABLE},
		 {resultInfo,          ""},
		 {state,               ?ActionStateType_RUNNING},
		 {actionId,            ActionId},
		 {timeActionStarted,   StartTime},
		 {timeActionCompleted, undefined}],
    logEsiLib:update_progress(Progress).


set_esi_ongoing() ->
    Fun = fun() -> mnesia:write({logRamVariables, esi_ongoing, true}) end,
    {atomic, ok} = mnesia:transaction(Fun).

remove_esi_ongoing() ->
    Fun = fun() -> mnesia:delete({logRamVariables, esi_ongoing}) end,
    {atomic, _} = mnesia:transaction(Fun).
    




get_fru_type() ->
    case clhI:fru_id() of
	""  -> central;
	Fid -> gft(clhI:mp_id(Fid))
    end.

gft(undefined) ->
    fruacc;
gft(MpId) ->
    case {clhI:mp_role(MpId), clhI:core_state(MpId)} of
	{core, active} -> central;
	_              -> local
    end.   
		   


%%% ----------------------------------------------------------
%%% sysInitI handling
%%% ----------------------------------------------------------
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: " ++ Format, [?MODULE | Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format, [?MODULE | Args]).




    %% NOTE: EsiFile now contains .gz; code below needs to be updated
    %% ESI encrypt code start:
    %% Do not remove; will soon be activated
    %case lmaI:encrypt_esi_log() of
    %    false -> %% No esi encryption
    %        EsiFile".gz";
    %    true -> %% Esi encryption
    %        PubKey = "test.txt",
    %        UserId = "test10",
    %        os:cmd(["gpg --import ", PubKey,
    %                "cd ", EsiDir,
    %                ";gpg --output ", EsiFile, ".gz.gpg",
    %                " --recipient ", UserId,
    %                " --encrypt ", EsiFile, ".gz"]),
    %        file:delete(EsiFile++".gz"),
    %        EsiFile++".gpg"
    %end.
    %% ESI encrypt code end:


