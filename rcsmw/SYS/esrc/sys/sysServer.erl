%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysServer.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/2

%%% @doc ==General purpose server==
%%% This module implements a general purpose server.

-module(sysServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/2').
-date('2017-08-16').
-author('etxpejn').
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
%%% R3A/1      2015-01-20 etxberb     Added get_group_leader/0.
%%% R3A/2      2015-03-11 etxtory     Mv update_server to omc
%%% R4A/1      2015-07-21 etxjotj     Basic disk space check
%%% R4A/2      2015-07-21 etxjotj     Disk check cont'd
%%% R4A/3      2015-07-21 etxjotj     Disk check cont'd
%%% R4A/7      2015-08-05 etxtory     function_clause in make_callback_list
%%%                                   Send alarm for fast increase
%%%                                   Update alarm when passing a "limit"
%%% R4A/10     2015-09-27 etxtory     Added disk-reserveration
%%% R4A/11     2015-11-16 etxtory     Added cleaning for 90%
%%% R4A/12     2015-11-17 etxtory     Disk check only on target
%%% R4A/13     2015-11-27 etxarnu     TR HU38977: add check of number of used fd (ports)
%%% R4A/14     2015-11-27 etxtory     HU37484 (part): do garbage collect regularly
%%% R4A/15     2015-11-29 etxarnu     Removed temporary port_info.txt file
%%% R5A/1      2016-01-07 etxpeno     Using erlang:monotonic_time to measure time
%%% R5A/2      2016-04-18 etxtory     Memory usage monitor
%%% R7A/1      2016-05-19 etxarnu     get_free_mem updated for wr8
%%% R8A/1      2016-11-10 etxjotj     Added debug printout for disk alarm
%%% R8A/2      2016-11-25 etxjotj     Disk alarm limits raised
%%%                                   Process load debug function
%%% R8A/3      2017-01-13 etxjotj     Disk alarm limits raised again
%%% R9A/1-3    2017-03-05 etxarnu     In VRCS, restart lttng if oap_ns changes
%%% R9A/4      2017-03-05 etxarnu     Bug in restart_lttng
%%% R9A/5      2017-03-06 etxarnu     Bug in restart_lttng
%%% R9A/6      2017-03-14 etxarnu     Now call restart_lttng(NewNs)
%%% R9A/7      2017-03-20 etxjotj     Memory check failed fix
%%% R9A/8      2017-04-12 etxtory     WR8 free adaption
%%% R10A/1     2017-05-03 ekrebak     HV78044
%%% R10A/2     2017-06-05 ejinfeg     Added a new folder "/rcs/fi/" to ORDER
%%% R10A/3     2017-06-15 etxpeno     add checking number of processes
%%% R10A/4     2017-06-19 etxarnu     Use ee_oamnetns to set lttng namespace if it exists
%%% R10A/5     2017-06-19 etxarnu     Changed printout when ee_oamnetns is used
%%% R11A/1     2017-08-16 etxpejn     Disk alarm limits set back to default since HSI is enabled
%%% R11A/2     2017-08-16 etxpejn     Added try to swmI call
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).
-export([datainit/1]).
-export([activate/0]).
-export([register_file_owner/2, unregister_file_owner/1]).
-export([get_group_leader/0]).
-export([clean_disk/1]).

-export([reserve_disk/3,
	 unreserve_disk/1,
	 disk_reservation/0,
	 get_free_disk/0]).

-export([updated_oam_ip_data/1]).

%% Debug interface
-export([run_clean_disk/1]).
-export([remove_all/1]).
-export([handle_check_ports/1]).
-export([handle_check_mem/1]).
-export([handle_check_processes/0]).
-export([get_free_mem/0]).

-export([process_load/0, process_load/1]).

-export([file_descriptor_status/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-define(SERVER, ?MODULE).

%% Memory Limits
-define(LOW_LIMIT, 80).   
-define(MEDIUM_LIMIT, 85).
-define(HIGH_LIMIT, 90). 
-define(REMOVE_MARGIN, 3).

%% Process Limit
-define(PROCESS_LIMIT, 10). %% Percent of the maximum number of processes

%% file descriptor Limits
-define(FD_ZERO,          0). 
-define(FD_LIMIT_LOW,    80). 
-define(FD_LIMIT_MEDIUM, 90).
-define(FD_LIMIT_HIGH,   95).

%% Safety margin for /rcs disk usage (30720KB=30MB)
-define(DISK_MARGIN_KB, 30720).

%% Disk clean
-define(ROP_DIR, "/rcs/sftp/rop").
-define(ORDER, ["/rcs/applicationlogs/",
		"/rcs/comte/",
		"/rcs/fi/",
		"/rcs/saf_log/",
		"/rcs/log/",
		"/rcs/bootlogs/"]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Record definitions
-record(state, {disk_check_timer,
		free_ram_mb = 0,
		oam_ns,
	        fd_status = ?FD_ZERO}).

%% include
-include_lib("kernel/include/file.hrl").
-include("SysDiskCheck.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc return the group leader
%%% @end
%%% ----------------------------------------------------------
get_group_leader() ->
    gen_server:call(?SERVER, {?MODULE, get_group_leader}).

%%% ----------------------------------------------------------
%%% @doc Register a file owner
%%% All rcs contents must have a file owner registered here
%%% @end
%%% ----------------------------------------------------------
register_file_owner(File, Module) ->
    case
	mnesia:transaction(
	  fun() ->
		  mnesia:write(#sysDiskCheckCb{file=File,
					       module=Module})
	  end) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%% ----------------------------------------------------------
%%% @doc Unregister a file owner
%%% Be sure to register another file owner
%%% @end
%%% ----------------------------------------------------------
unregister_file_owner(File) ->
    case mnesia:transaction(
	   fun() ->
		   mnesia:delete({sysDiskCheckCb, File})
	   end) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%% ----------------------------------------------------------
%%% @doc updated OAM IP data
%%% Callback from OOT
%%% @end
%%% ----------------------------------------------------------
updated_oam_ip_data(OamIpData) ->
    gen_server:cast(?SERVER, {update_oam_ip_data, OamIpData}).

%%% ----------------------------------------------------------
%%% @doc Start the sysServer
%%% Supervisor interface
%%% @end
%%% ----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% ----------------------------------------------------------
%%% @doc Datainit
%%% Datainit
%%% @end
%%% ----------------------------------------------------------
datainit(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(
	  sysDiskReserve,
	  [{type, set},
	   {disc_copies, DbNodes},
	   {attributes, record_info(fields, sysDiskReserve)}]),

    {atomic, ok} =
	clhI:mnesia_create_table(
	  sysDiskCheckCb,
	  [{type, set},
	   {disc_copies, DbNodes},
	   {attributes, record_info(fields, sysDiskCheckCb)}]),

    SysOwnerships = [{"applicationlogs", ?MODULE},
		     {"bootlogs", ?MODULE},
		     {"dumps", ?MODULE},
		     {"ee-htmd", ?MODULE},
		     {"ee-rhpmem", ?MODULE},
		     {"ee-testmgr", ?MODULE},
		     {"erlang", ?MODULE},
		     {"persistent", ?MODULE}],
    [mnesia:dirty_write(#sysDiskCheckCb{file=File, module=Module})
     || {File, Module} <- SysOwnerships],

    ok.

%%% ----------------------------------------------------------
%%% @doc reserve_disk/3 -> ok
%%% Dir::string(), Module::atom(), KbDisk::atom()
%%% Reserve disk space for /rcs.
%%% Called at installation (DataInit).
%%% @end
%%% ----------------------------------------------------------
reserve_disk(Dir, Module, KbDisk) ->
    ok = mnesia:dirty_write(#sysDiskReserve{
			       dir = Dir,
			       module = Module,
			       diskspace = KbDisk}).

%%% ----------------------------------------------------------
%%% @doc unreserve_disk(Dir) -> ok
%%% Unreserve disk space for /rcs.
%%% @end
%%% ----------------------------------------------------------
unreserve_disk(Dir) ->
    mnesia:dirty_delete(sysDiskReserve, Dir),
    ok.

%%% ----------------------------------------------------------
%%% @doc disk_reservation() -> returns disk reservation
%%% @end
%%% ----------------------------------------------------------
disk_reservation() ->
    ets:tab2list(sysDiskReserve).

%%% ----------------------------------------------------------
%%% @doc get_free_disk() -> returns free disk in Kb
%%% 30MB/30720KB in safety margin
%%% @end
%%% ----------------------------------------------------------
get_free_disk() ->
    DfRes = os:cmd(["df -kP ", sysEnv:rcs_dir()]),
    Values = hd(lists:reverse(string:tokens(DfRes, "\n"))),
    %% [Device, Blocks, Used, Available, UsePercent, Mounted]
    [_, _, _, AvailableKbStr, _, _] = string:tokens(Values, "\t "),
    AvailableKbInt = list_to_integer(AvailableKbStr),
    ReservedKb = calc_reserved_disk(),
    AvailableKbInt - ReservedKb - ?DISK_MARGIN_KB.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(Args) ->
    ok = gen_server:cast(self(), {initialize, Args}),
    {ok, initializing}.

initialize(_Args) ->
    %% Setup and start the disk check when HSI has removed unused LM
    {ok, _TimeRef} = timer:send_after(1000, setup_disk_space_check),
    Dir = filename:join([sysEnv:rcs_dir(), "sys"]),
    filelib:ensure_dir(Dir ++ "/x"),
    {noreply, #state{}}.
    
activate() ->
    case sysEnv:vrcs() of
	false ->
	    ok;
	true ->
	    gen_server:cast(?SERVER, activate)
    end.

handle_call({?MODULE, get_group_leader}, _From, State) ->
    {reply, erlang:group_leader(), State};
handle_call({?MODULE, file_descriptor_status}, _From, State) ->
    Status = handle_file_descriptor_status(),
    {reply, Status, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({initialize, Args}, initializing) ->
    initialize(Args);
handle_cast(activate, State) ->
    ootI:register_cfg_upd_cb(fun updated_oam_ip_data/1 ),
    {noreply,  State};
handle_cast({update_oam_ip_data, OamIpData}, State=#state{oam_ns=Ns}) ->
    NewNs=get_data(oap_namespace, OamIpData),
    case NewNs of
	Ns ->
	    ok;
	NewNs->
	    restart_lttng(NewNs)
    end,
    {noreply, State#state{oam_ns=NewNs}};

 handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_disk_space, #state{fd_status = FdStatus} = State) ->
    case sysEnv:rcs_mode() of
	target ->
	    handle_check_disk_space();
	_ ->
	    ok
    end,
    handle_check_ports(900),
    handle_check_processes(),
    do_garbage_collect(),
    NewFreeRam  = handle_check_mem(State#state.free_ram_mb),
    NewFdStatus = handle_file_descriptors(FdStatus),
    {noreply, State#state{free_ram_mb = NewFreeRam,
			  fd_status   = NewFdStatus}};

handle_info(setup_disk_space_check, State) ->
    HSI = 
	try 
	    swmI:is_hsi_active()
	catch
	    _:_ ->
		not_applicable
	end,
	
    case HSI of
	false ->
	    %% SWM on not ready with the HSI clean up 
	    {ok, _TimeRef} = timer:send_after(30000, setup_disk_space_check),
	    {noreply, State};
	_Else ->
	    %% setup regular disk checks
	    {ok, Files} = file:list_dir(sysEnv:rcs_dir()),
	    Unclaimed =
		[File || File <- Files,
			 case mnesia:dirty_read({sysDiskCheckCb, File}) of
			     [] -> true;
			     _ -> false
			 end],
	    case Unclaimed of
		[] ->
		    ok;
		Unclaimed ->
		    warning_msg("No owner registered for ~p~n"
				"Use sysServer:register_file_owner/2 ~n",
				[Unclaimed])
	    end,
	    {ok, DiskCheckTimer} = timer:send_interval(5 * 60000, check_disk_space),
	    {noreply, State#state{disk_check_timer = DiskCheckTimer}}
    end;
      
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------
%%% @doc Clean the disk
%%% Clean erlang directory
%%% @end
%%% ----------------------------------------------------------
clean_disk(major) ->
    %% Currently not used
    clean_disk(minor);
clean_disk(minor) ->
    {ok, Files} = file:list_dir(filename:join(sysEnv:rcs_dir(), "erlang")),
    [file:delete(filename:join([sysEnv:rcs_dir(), "erlang", File]))||
	File <- Files,
	case File of
	    "erlang.log."++_ -> false;
	    _ -> true
	end],
    os:cmd(["rm -rf ", filename:join([sysEnv:rcs_dir(), "db", "*"])]),
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           handle_check_disk_space()
%%% Input: -
%%% Output: ok
%%% Exceptions:
%%% Description: Check the /rcs disk and act on high usage
%%% ----------------------------------------------------------
handle_check_disk_space() ->
    Res = os:cmd(["df -hP ", sysEnv:rcs_dir()]),
    [_, Data] = string:tokens(Res, "\n"),
    [_FS, _Total, _Used, Free, Percent|_] = string:tokens(Data, " "),
    case io_lib:fread("~d", Percent) of
	{ok, [P], _} when P >= ?HIGH_LIMIT ->
	    send_alarm(P, Free, _Limit = high),
	    run_clean_disk(major);
	{ok, [P], _} when P >= ?MEDIUM_LIMIT ->
	    send_alarm(P, Free, _Limit = medium),
	    run_clean_disk(minor);
	{ok, [P], _} when P >= ?LOW_LIMIT ->
	    send_alarm(P, Free, _Limit = low);
	{ok, [P], _} when P < ?LOW_LIMIT ->
	    clear_alarm();
	_ ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% #           clean_disk(Severity)
%%% Input: Severity: minor|major
%%% Output: ok
%%% Exceptions:
%%% Description:
%%% minor - RCS cleans own log
%%% major - RCS removes files until reaching below LOW_LIMIT
%%% ----------------------------------------------------------
run_clean_disk(minor) ->
    CallbackList = make_callback_list(ets:tab2list(sysDiskCheckCb)),
    [try Module:clean_disk(minor)
     catch T:E ->
	     sysInitI:error_report(
	       [{mfa, {Module, clean_disk, [minor]}},
		{owner_for, filename:join(sysEnv:rcs_dir(), lists:sort(Files))},
		{T, E}])
     end || {Module, Files} <- CallbackList],
    ok;

%% Note:
%% df Used + df Available it NOT equal to df Total
%% df Use% = df Used / (df Used + df Available)
run_clean_disk(major) ->
    Res = os:cmd(["df -k ",sysEnv:rcs_dir()]),
    [_, Data] = string:tokens(Res, "\n"),
    [_FS, _Total, Used, Free, _Percent|_] = string:tokens(Data, " "),
    UsedKb = list_to_integer(Used),
    FreeKb = list_to_integer(Free),
    BetterTotal = UsedKb + FreeKb,
    WantedKb = round(BetterTotal * (?LOW_LIMIT-?REMOVE_MARGIN)/100),
    RemoveKb = UsedKb - WantedKb,
    info_msg("cleaning disk (start) - used=~pKb, free=~pKb; removing ~pKb~n", [UsedKb, FreeKb, RemoveKb]),
    RemoveResult = remove_closed(RemoveKb),
    info_msg("cleaning disk (done) - ~p~n", [RemoveResult]).

%% 1) Remove files which are not opened and are larger than 2048KB (2MB)
%%    for all directories except for ?ROP_DIR.
remove_closed(RemoveKb) when RemoveKb =< 0 ->
    ok;
remove_closed(RemoveKb) ->
    case do_remove_closed(?ORDER, _AllowedSizeKb = 2048, RemoveKb) of
	ready ->
	    ready;
	{not_ready, LeftToRemoveKb} ->
	    remove_rop(LeftToRemoveKb)
    end.

%% 2) Remove all files below ?ROP_DIR (not opened files) if
%%    exceeding reservation. Handles sub-dirs below ?ROP_DIR.
remove_rop(LeftToRemoveKb) ->
    case do_remove_rop(LeftToRemoveKb) of
	ready ->
	    ready;
	{not_ready, LeftKb} ->
	    remove_all(LeftKb)
    end.

%% 3) Remove all files (not opened files).
%%    Should not come here!
remove_all(LeftToRemoveKb) ->
    do_remove_closed(?ORDER, _AllowedSizeKb = 0, LeftToRemoveKb).

%% ready | {not_ready, LeftToRemovedKb}
do_remove_closed([Dir | T], AllowedSizeKb, LeftToRemoveKb) ->
    Res = os:cmd("du -k " ++ Dir ++ " | tail -1 | awk '{print $1}'"),
    [Size|_] = string:tokens(Res, "\n"),
    try list_to_integer(Size) of
	SizeKb when SizeKb > AllowedSizeKb ->
	    case remove_files(Dir, AllowedSizeKb, LeftToRemoveKb) of
		ready ->
		    ready;
		{not_ready, SubDirs, LeftKb} ->
		    do_remove_closed(lists:append(SubDirs, T), AllowedSizeKb, LeftKb)
	    end;
	_SizeKb ->
	    do_remove_closed(T, AllowedSizeKb, LeftToRemoveKb)
    catch Type:Error ->
	    warning_msg("Can not read size of directory ~p~n~p~n", [Dir, {Type, Error}]),
	    do_remove_closed(T, AllowedSizeKb, LeftToRemoveKb)
    end;
do_remove_closed([], _AllowedSizeKb, LeftToRemoveKb) when LeftToRemoveKb > 0 ->
    {not_ready, LeftToRemoveKb};
do_remove_closed([], _AllowedSizeKb, _LeftToRemoveKb) ->
    ready.

%% ready | {not_ready, LeftToRemoveKb}
do_remove_rop(LeftToRemoveKb) ->
    RopReservedKb =
    case mnesia:dirty_read(sysDiskReserve, ?ROP_DIR) of
	[Obj] ->
	    Obj#sysDiskReserve.diskspace;
	_ ->
	    0
    end,
    Res = os:cmd("du -k " ++ ?ROP_DIR ++ " | tail -1 | awk '{print $1}'"),
    [Size|_] = string:tokens(Res, "\n"),
    try list_to_integer(Size) of
	SizeKb when SizeKb > RopReservedKb ->
	    warning_msg("ROP files are exceeding reservation ~p~n", [{SizeKb, RopReservedKb}]),
	    do_remove_rop([?ROP_DIR], LeftToRemoveKb);
	_SizeKb ->
	    {not_ready, LeftToRemoveKb}
    catch Type:Error ->
	    warning_msg("Can not read size of directory ~p~n~p~n", [?ROP_DIR, {Type, Error}]),
	    {not_ready, LeftToRemoveKb}
    end.

do_remove_rop([Dir | T], LeftToRemoveKb) ->
    case remove_files(Dir, _AllowedSizeKb = 0, LeftToRemoveKb) of
	ready ->
	    ready;
	{not_ready, SubDirs, LeftKb} ->
	    do_remove_rop(lists:append(SubDirs, T), LeftKb)
    end;
do_remove_rop([], LeftToRemoveKb) when LeftToRemoveKb > 0 ->
    {not_ready, LeftToRemoveKb};
do_remove_rop([], _LeftToRemoveKb) ->
    ready.

%% ready | {not_ready, SubDirs, LeftToRemoveKb}
remove_files(Dir, AllowedSizeKb, LeftToRemoveKb) ->
    case file:list_dir(Dir) of
	{ok, Fs} ->
	    do_remove_files(Fs, Dir, AllowedSizeKb, _SubDirs = [], LeftToRemoveKb);
	{error, Reason} ->
	    warning_msg("remove_files(~p) error - Reason ~p~n", [Dir, Reason]),
	    {nok, [], LeftToRemoveKb}
    end.

do_remove_files(_Files, _Dir, _AllowedSizeKb, _SubDirs, LeftToRemoveKb)
  when LeftToRemoveKb =< 0 ->
    ready;
do_remove_files([File | T], Dir, AllowedSizeKb, SubDirs, LeftToRemoveKb) ->
    Fn = filename:join([Dir, File]),
    case filelib:is_dir(Fn) of
	true ->
	    do_remove_files(T, Dir, AllowedSizeKb, [Fn | SubDirs], LeftToRemoveKb);
	false ->
	    NewLeftToRemoveKb = remove_file(Dir, File, lists:reverse(File), AllowedSizeKb, LeftToRemoveKb),
	    do_remove_files(T, Dir, AllowedSizeKb, SubDirs, NewLeftToRemoveKb)
    end;
do_remove_files([], _Dir, _AllowedSizeKb, SubDirs, LeftToRemoveKb) ->
    {not_ready, SubDirs, LeftToRemoveKb}.

%% Do not remove:
%% log index (.idx) - /rcs/log/..
%% size (.siz) files - /rcs/log/..
remove_file(_Dir, _File, "xdi."++_ , _AllowedSizeKb, LeftToRemoveKb) ->
    LeftToRemoveKb;
remove_file(_Dir, _File, "zis."++_ , _AllowedSizeKb, LeftToRemoveKb) ->
    LeftToRemoveKb;
remove_file(Dir, File, _RevFile, AllowedSizeKb, LeftToRemoveKb) ->
    Fn = filename:join([Dir, File]),
    case os:cmd("sudo fuser " ++ Fn) of
	[] ->
	    %% Not used; remove
	    remove_file(Fn, AllowedSizeKb, LeftToRemoveKb);
	_ ->
	    %% Used or "No such file or directory"; don't remove
	    LeftToRemoveKb
    end.

%% Don't remove symlinks
remove_file(Fn, AllowedSizeKb, LeftToRemoveKb) ->
    case file:read_link_info(Fn) of
	{ok, FI} when FI#file_info.type =:= regular,
		      FI#file_info.size >= 0 ->
	    RmKb = round(FI#file_info.size/1024),
	    if
		RmKb >= AllowedSizeKb ->
		    case file:delete(Fn) of
			ok ->
			    io:format("~w: clean disk (delete) - ok ~p~n", [?MODULE, Fn]),
			    LeftToRemoveKb - RmKb;
			{error, Reason} ->
			    io:format("~w: clean disk (delete) - nok ~p: ~p~n", [?MODULE, Fn, Reason]),
			    LeftToRemoveKb
		    end;
		true ->
		    LeftToRemoveKb
	    end;

	{ok, _FI} ->
	    LeftToRemoveKb;

	{error, Reason} ->
	    io:format("~w: clean disk (delete) - nok ~p: ~p~n", [?MODULE, Fn, Reason]),
	    LeftToRemoveKb
    end.

%%% ----------------------------------------------------------
%%% #           handle_check_ports()
%%% Input: -
%%% Output: ok
%%% Exceptions:
%%% Description: Check if number of ports used goes above limit
%%% ----------------------------------------------------------
handle_check_ports(Limit) ->
    PortCount = erlang:system_info(port_count) ,
    if
	PortCount > Limit ->
	    FileNameTmp=filename:join( [sysEnv:tmp_dir(), "port_info.txt"]),
	    FileName=filename:join( [sysEnv:rcs_dir(), "erlang", "port_info.txt.gz"]),
	    sysUtil:print_port_info(FileNameTmp),
	    Cmd="gzip --stdout "++ FileNameTmp ++" > " ++ FileName,
	    os:cmd(Cmd),
	    error_msg("Port count exceeds limit (~p) :  ~p~n"
		      "Stored info in ~p~n", [Limit,PortCount,FileName]),
	    file:delete(FileNameTmp);
	true -> ok
    end.

%%% ----------------------------------------------------------
%%% #           handle_check_processes()
%%% Input: -
%%% Output: ok
%%% Exceptions:
%%% Description: Check if number of processes used goes above limit
%%% ----------------------------------------------------------
handle_check_processes() ->
    Count = erlang:system_info(process_count),
    Limit = ?PROCESS_LIMIT * erlang:system_info(process_limit) / 100,
    handle_check_processes(Count, Limit).

handle_check_processes(Count, Limit) when Count > Limit ->
    info_msg("Number of processes (~p) exceeds limit (~p)~n", [Count, Limit]),
    ok;
handle_check_processes(_, _) ->
    ok.


%%% ----------------------------------------------------------
%%% #           handle_file_descriptor_status()
%%% Input: -
%%% Output: 
%%% Exceptions:
%%% Description: Print file descriptor status
%%% ----------------------------------------------------------
handle_file_descriptor_status() ->
    FdStr = os:cmd("cat /proc/sys/fs/file-nr"),
    hfds(string:tokens(FdStr, " \t\n")).

hfds([Current, _, Max]) ->
    Procent = (list_to_integer(Current)*100) div list_to_integer(Max),
    {Current, Max, integer_to_list(Procent)};
hfds(_) ->
    "Could not find file descriptor usage~n".




%%% ----------------------------------------------------------
%%% #           handle_file_descriptors(FdStatus)
%%% Input: -
%%% Output: FdStatus
%%% Exceptions:
%%% Description: Check if number of file descriptors used goes above limit
%%% ----------------------------------------------------------
handle_file_descriptors(FdStatus) ->
    FdStr = os:cmd("cat /proc/sys/fs/file-nr"),
    handle_file_descriptors(string:tokens(FdStr, " \t\n"), FdStatus).


handle_file_descriptors([Current, _, Max], FdStatus) ->
    handle_fd((list_to_integer(Current)*100) div list_to_integer(Max),
	      FdStatus);
handle_file_descriptors(_, FdStatus) ->
    FdStatus.



handle_fd(Count, FdStatus) 
  when Count >= ?FD_LIMIT_HIGH andalso
       FdStatus < ?FD_LIMIT_HIGH ->
    error_msg("Number of used file descriptors "
	      "exceeds medium threshold (~p%).~n"
	      "Current level = ~p%.~n",
	      [?FD_LIMIT_HIGH, Count]),
    ?FD_LIMIT_HIGH;
handle_fd(Count, FdStatus) 
  when Count >= ?FD_LIMIT_MEDIUM andalso
       FdStatus < ?FD_LIMIT_MEDIUM ->
    error_msg("Number of used file descriptors "
	      "exceeds medium threshold (~p%).~n"
	      "Current level = ~p%.~n",
	      [?FD_LIMIT_MEDIUM, Count]),
    ?FD_LIMIT_MEDIUM;
handle_fd(Count, FdStatus) 
  when Count < ?FD_LIMIT_LOW andalso
       FdStatus > ?FD_ZERO ->
    info_msg("Number of used file descriptors "
	     "is now below the low threshold (~p%).~n"
	      "Current level = ~p%.~n",
	     [?FD_LIMIT_LOW, Count]),
    ?FD_ZERO;
handle_fd(_Count, FdStatus) ->
    FdStatus.


%%% ----------------------------------------------------------
%%% #           do_garbage_collect
%%% Input: -
%%% Output: -
%%% Exceptions: -
%%% Description: Performs GC on all processes
%%% ----------------------------------------------------------
do_garbage_collect() ->
    MT1 = erlang:memory(total),
    Time1 = erlang:monotonic_time(micro_seconds),
    [erlang:garbage_collect(Pid) || Pid <- erlang:processes()],
    Time2 = erlang:monotonic_time(micro_seconds),
    MT2 = erlang:memory(total),
    TimeDiff = (Time2-Time1) / 1000000, %% secs
    MTDiff = round((MT1 - MT2) / (1024*1024) * 10) / 10, %% Mb
    if (TimeDiff > 2) or (MTDiff > 25) ->
	    info_msg("GC took ~psecs and GC away ~pMb~n", [TimeDiff, MTDiff]);
       true ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% #           make_callback_list(Objs)
%%% Input: Objs:[#sysDiskCheckCb{}]
%%% Output: ok
%%% Exceptions:
%%% Description: Callback list keyed on module
%%% ----------------------------------------------------------
make_callback_list(Objs) ->
    make_callback_list(Objs, []).

make_callback_list([Obj | T], List) ->
    File = Obj#sysDiskCheckCb.file,
    Module = Obj#sysDiskCheckCb.module,
    case lists:keysearch(Module, 1, List) of
	{value, {Module, Files}} ->
	    case lists:member(File, Files) of
		true ->
		    make_callback_list(T, List);
		false ->
		    NewFiles = [File|Files],
		    NewList =
			lists:keyreplace(Module, 1, List, {Module, NewFiles}),
		    make_callback_list(T, NewList)
	    end;
	false ->
	    make_callback_list(T, [{Module, [File]} | List])
    end;
make_callback_list([], List) ->
    List.

%%% ----------------------------------------------------------
%%% #           send_alarm(Percentage, Free, CurrentLimit)
%%% Input: Percentage::integer() - Share of the disk that is used
%%%        Free::string() - Free disk space in human readable format
%%%        CurrentLimit::atom() - low | medium | high
%%% Output: ok
%%% Exceptions:
%%% Description: Raise the ArchiveDiskAlmostFull alarm
%%% ----------------------------------------------------------
send_alarm(Percentage, Free, CurrentLimit) ->
    case send_or_update_alarm(Percentage, CurrentLimit) of
	true ->
	    Cmd = "du -mH "++sysEnv:rcs_dir(),
	    info_msg("~s~n~s~n",[Cmd, os:cmd(Cmd)]),
	    Dn = [<<"ManagedElement=1">>,<<"SystemFunctions=1">>],
	    Fmt = "The archive disk is ~w% full. ~s available",
	    Msg = lists:flatten(io_lib:format(Fmt, [Percentage, Free])),
	    comsaI:send_alarm('ArchiveDiskAlmostFull', warning, Dn, Msg),
	    ok;
	false ->
	    ok
    end.

send_or_update_alarm(Percentage, CurrentLimit) ->
    case catch comsaI:get_alarms('ArchiveDiskAlmostFull') of
	[] ->
	    true;
	[AlarmData] ->
	    AlarmPercentage = extract_percentage(AlarmData, Percentage),
	    case get_limit(AlarmPercentage) of
		CurrentLimit ->
		    %% No change of limit; do not update alarm
		    false;
		_ ->
		    %% Limit has changed; update alarm
		    true
	    end;
	 Other ->
	    %% Should not happen
	    error_msg("Unexpected return from comsaI:get_alarms ~n~p~n", [Other]),
	    false
    end.

extract_percentage(AlarmData, Percentage) ->
    case lists:keyfind(additionalText, 1, AlarmData) of
	false ->
	    %% Should not happen
	    Percentage;
	{additionalText, S1} ->
	    [S2 | _ ] = string:tokens(S1, "%"),
	    S3 = string:substr(S2, string:len(S2) - 2),
	    list_to_integer(string:strip(S3))
    end.

get_limit(Percentage) when Percentage >= ?HIGH_LIMIT -> high;
get_limit(Percentage) when Percentage >= ?MEDIUM_LIMIT -> medium;
get_limit(Percentage) when Percentage >= ?LOW_LIMIT -> low;
get_limit(_Percentage) -> low.

%%% ----------------------------------------------------------
%%% #           clear_alarm()
%%% Input: -
%%% Output: ok
%%% Exceptions:
%%% Description: Cease the DiskAlmostFull alarm
%%% ----------------------------------------------------------
clear_alarm() ->
    case comsaI:get_alarms('ArchiveDiskAlmostFull') of
	[] ->
	    ok;
	_ ->
	    Dn = [<<"ManagedElement=1">>,<<"SystemFunctions=1">>],
	    comsaI:clear_alarm('ArchiveDiskAlmostFull', Dn),
	    ok
    end.

%%% ----------------------------------------------------------
%%% Disk space handling
%%% ----------------------------------------------------------
calc_reserved_disk() ->
    AllKeys = mnesia:dirty_all_keys(sysDiskReserve),
    calc_reserved_disk(AllKeys, _Acc = 0).

calc_reserved_disk([Key | T], Acc) ->
    case catch mnesia:dirty_read(sysDiskReserve, Key) of
	[Obj] ->
	    Kb = get_disk_space(Obj),
	    calc_reserved_disk(T, Acc + Kb);
	Other ->
	    %% Should not happen
	    error_msg("calc_reserved_disk - missing object Key ~p~n~p~n",
		      [Key, Other]),
	    calc_reserved_disk(T, Acc)
    end;
calc_reserved_disk([], Acc) ->
    Acc.

%% Returns reserved_disk_space - already_used_disk_space
get_disk_space(Obj) ->
    try begin
	    true = filelib:is_dir(Obj#sysDiskReserve.dir),
	    Cmd = "du -k " ++ Obj#sysDiskReserve.dir ++
		"| tail -1 | awk '{ print $1}'",
	    R = os:cmd(Cmd),
	    [UsedKbStr] = string:tokens(R, "\n"),
	    UsedKbInt = list_to_integer(UsedKbStr),
	    LeftInt = Obj#sysDiskReserve.diskspace - UsedKbInt,
	    case LeftInt of
		LeftInt when LeftInt < 0 ->
		    error_msg("~p uses more disk (~pKb) than reserved~n~p~n",
			      [Obj#sysDiskReserve.module,
			       LeftInt,
			       {Obj#sysDiskReserve.dir,
				Obj#sysDiskReserve.diskspace}]),
		    0;
		LeftInt ->
		    LeftInt
	    end
	end
    catch Type:Error ->
	    error_msg("get_disk_space failed ~p~n", [{Type, Error}]),
	    0
    end.

%% ----------------------------------------------------------
%%% #           handle_check_mem(PrevFreeSize)
%%% Input: PrevFreeSize::int() - MB
%%% Output: NewFreeSize::int() - MB
%%% Exceptions: -
%%% Description: Checks memory usage
%%% ----------------------------------------------------------
handle_check_mem(PrevFreeSize) ->
    case catch get_free_mem() of
	{FreeSize, AllTmpFs} ->
	    handle_check_mem(PrevFreeSize, FreeSize, AllTmpFs);
	Other ->
	    error_msg("Checking memory failed~n~p~n", [Other]),
	    0
    end.

handle_check_mem(PrevFreeSize, FreeSize, AllTmpFs) ->
    if
	PrevFreeSize =:= 0 ->
	    %% Initial value taken 5mins after start-up (first time the timer fires).
	    log_mem_usage(PrevFreeSize, FreeSize, AllTmpFs),
	    FreeSize;

	(PrevFreeSize > 300) and (FreeSize < 300) ->
	    %% Log at this fixed limit.
	    %% The idea is to log the transition of the limit.
	    warning_msg("Passing 300Mb free RAM~n", []),
	    log_mem_usage(PrevFreeSize, FreeSize, AllTmpFs),
	    FreeSize;

	(PrevFreeSize * 1.05) =< FreeSize ->
	    %% Less memory is used; increase is more than 5%
	    %% Increase (or same) of available free sizeDr
	    %% - return new value (FreeSize)
	    FreeSize;

	PrevFreeSize =< FreeSize ->
	    %% Less memory is used; increase is less than 5%
	    %% - return (keep) previous value (PrevFreeSize)
	    PrevFreeSize;

	(0.05 * PrevFreeSize) >= (PrevFreeSize - FreeSize) ->
	    %% More memory is used but doesn't exceed 5% more usage
	    %% - return (keep) previous value (PrevFreeSize)
	    PrevFreeSize;

	(0.05 * PrevFreeSize) < (PrevFreeSize - FreeSize) ->
	    %% More memory is used and exceeds 5% more usage
	    %% - return new value (FreeSize)
	    %% - log information
	    log_mem_usage(PrevFreeSize, FreeSize, AllTmpFs),
	    FreeSize;

	true ->
	    %% Should/can not come here.
	    error_msg("Memory check failure ~n", []),
	    PrevFreeSize
    end.

%%% ----------------------------------------------------------
%%% Information regarding free in WR6:
%%% tmpfs is using real RAM but not counted in free.
%%% This means that free really returns a incorrect value and the
%%% real "free" value is as follows:
%%% buffers/cache:free - "the used size of /tmp".
%%%
%%% The above has been solved in WR8 free and the new column
%%% available is giving a correct value (also including tmpfs).
%%% See: http://www.linuxatemyram.com/ for more information.
%%% ----------------------------------------------------------
get_free_mem() ->
    %% WindRiver 8 version of free
    FreeRes = os:cmd("free -m | grep \"Mem:\" | awk '{print $7}'"),
    [Available] = string:tokens(FreeRes, " \n"),
    FreeSizeMb = list_to_integer(Available),

    %% Get "Used" ($3) of all tmpfs (-m = MiB)
    %% During df execution some partitions may disappear
    %% Example: "df: '/tmp/cupmnt': No such file or directory"
    %% In these cases we want the last line of the df command, which
    %% will be the sum of the used memory
    DfRes = os:cmd("df -m | grep tmpfs | awk '{s+=$3} END {print s}'"),
    AllTmpFsStr = lists:last(string:tokens(DfRes, "\n")),
    AllTmpFsMb = list_to_integer(AllTmpFsStr),


    {FreeSizeMb, AllTmpFsMb}.

log_mem_usage(PrevFreeSize, FreeSize, AllTmpFs) ->
    TS = comsaI:iso_time(os:timestamp(), basic),
    Filename = "mem_info" ++ "." ++ TS,
    SysDir = filename:join([sysEnv:rcs_dir(), "sys"]),
    TmpFile = filename:join(["/tmp", Filename]),

    limit_mem_data_storage(SysDir, "mem_info*"),

    Free = os:cmd("free -m"),
    Top = os:cmd("top -b -n 1"),
    {ok, Fd} = file:open(TmpFile, [write]),

    io:format(Fd, "Memory usage information generated ~p~n~n", [TS]),

    io:format(Fd, "Previous total free size: ~pM~n", [PrevFreeSize]),
    io:format(Fd, "Current total free size: ~pM~n", [FreeSize]),
    io:format(Fd, "tmpfs usage: ~pM~n~n", [AllTmpFs]),

    io:format(Fd, "# Linux command: free -m~n", []),
    io:format(Fd, "~s~n~n", [Free]),

    io:format(Fd, "# Linux command: top -b -n 1~n", []),
    io:format(Fd, "~s~n", [Top]),

    file:close(Fd),
    os:cmd("gzip " ++ TmpFile ++ ";mv " ++ TmpFile ++ ".gz" ++ " " ++ SysDir).

limit_mem_data_storage(Dir, FileWild) ->
    Wild = filename:join([Dir, FileWild]),
    R = os:cmd("ls -1 " ++ Wild ++ " | wc -l"),
    [NoStr | _] = string:tokens(R, "\n"),
    case catch list_to_integer(NoStr) of
	No when is_integer(No) ->
	    if
		No < 15 ->
		    ok;
		true ->
		    RmNo = No - 15,
		    RmNoStr = integer_to_list(RmNo),
		    os:cmd("rm `ls -t1 " ++ Wild ++ " | tail -" ++ RmNoStr ++ "`")
	    end;
	_ ->
	    %% No entry created yet
	    ok
    end.

get_data(Type, TupleList) ->
    case lists:keyfind(Type, 1, TupleList) of
        {Type, Data} -> Data;
        _ -> undefined
    end.

%%% ----------------------------------------------------------
%%% Erlang shell printouts
%%% ----------------------------------------------------------
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% @doc Print currently used file descriptors
%%% @end
%%% ----------------------------------------------------------
file_descriptor_status() ->
    gen_server:call(?SERVER, {?MODULE, file_descriptor_status}).

%%% ----------------------------------------------------------
%%% @doc List the 15 processes with the highest number of reductions
%%% @end
%%% ----------------------------------------------------------
process_load() ->
    process_load(15).

%%% ----------------------------------------------------------
%%% @doc List the N processes with the highest number of reductions
%%% @end
%%% ----------------------------------------------------------
-spec process_load(N::integer()) -> ok.

process_load(N) ->
    A = [{X, element(2, process_info(X, reductions))}||X<-processes()],
    erlang:yield(),
    timer:sleep(2000),
    erlang:yield(),
    B = [{X, element(2, process_info(X, reductions))}||X<-processes()],
    Diffs =
	[begin
	     {_, ARed} = lists:keyfind(Pid, 1, A),
	      {Pid, Reductions-ARed}
	 end || {Pid, Reductions} <- B],
    process_load_print(lists:reverse(lists:keysort(2, Diffs)), N).

process_load_print(PData, N) ->
    io:format("Reductions      | Process~n"
	      "======================================================~n"++
		  do_process_load_print(PData, N),[]).

do_process_load_print(_, N) when N =< 0 ->
    [];
do_process_load_print([{_, 0}|_], _) ->
    io_lib:format("All other processes are idle~n",[]);
do_process_load_print([{Pid, Reductions}|PData], N) ->
    RegName =
	case process_info(Pid, registered_name) of
	    [] -> Pid;
	    {_,Name} -> Name;
	    _ -> Pid
	end,
    case self() of
	Pid ->
	    [io_lib:format("~15w | ~p (~w) (this process)~n",
			   [Reductions, RegName, Pid])|
	     do_process_load_print(PData, N-1)];
	_ ->
	    [io_lib:format("~15w | ~p (~w)~n",[Reductions, RegName, Pid])|
	     do_process_load_print(PData, N-1)]
    end.

-define(EE_OAMNETNS, "/usr/local/bin/ee_oamnetns").

restart_lttng(NsT) ->
    case sysEnv:vrcs() of
	false ->
	    ok;
	true ->
	    Ns = b2l(NsT),
	    case file:read_file_info(?EE_OAMNETNS) of
		{ok,_} ->
		    
		    case Ns of
			X when X==""; X==undefined   ->
			    Res=os:cmd(?EE_OAMNETNS),
			    sysInitI:info_msg(
			      "~p: lttng updated by ~p in default namespace~nResult=~p~n",
			      [?MODULE,?EE_OAMNETNS,Res]);
			Ns ->
			    Res=os:cmd(?EE_OAMNETNS ++ " " ++ Ns),
			    sysInitI:info_msg(
			      "~p: lttng updated by ~p  in OaM namespace: ~p~nResult=~p~n",
			      [?MODULE, ?EE_OAMNETNS,Ns,Res])
		    end;
		{error,_} -> %old way 
		    os:cmd("/etc/init.d/lttng.sh stop"),
		    case Ns of
			X when X==""; X==undefined   ->
			    Res=os:cmd("/etc/init.d/lttng.sh start"),
			    sysInitI:info_msg(
			      "~p: lttng restarted in default namespace~nResult=~p~n",
			      [?MODULE,Res]);
			Ns ->
			    Res=os:cmd(
				  "ip netns exec "++Ns++" /etc/init.d/lttng.sh start"),
			    sysInitI:info_msg(
			      "~p: lttng restarted in OaM namespace: ~p~nResult=~p~n",
			      [?MODULE, Ns,Res])
		    end
	    end
    end.

b2l(B) when is_binary(B) ->
    binary_to_list(B);
b2l(L)  ->
    L.
