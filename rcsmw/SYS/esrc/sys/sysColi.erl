%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysColi.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1

%%% @doc ==COLI module for sys==
-module(sysColi).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-11-20').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-04-25 etxpejn     Created
%%% R2A/2      2014-04-25 etxberb     Added os_df/1 & os_who/1.
%%% R2A/3      2014-05-06 etxpejn     Added sys_date
%%% R2A/4      2014-05-08 etxpejn     Added os_meminfo & os_cpuinfo
%%% R2A/5      2014-05-13 etxpejn     Added allocated_disc_space & free_up_disc_space
%%% R2A/6      2014-06-03 etxberb     Changed sys_uptime to os_uptime and
%%%                                   sys_date to os_date.
%%% R2A/8      2014-06-11 etxberb     Removed os_uptime, os_who and os_df.
%%% R2A/9      2014-06-27 etxberb     Added board_escalation/1, board_restart/1,
%%%                                   sys_discspace/1, sys_factoryreset/1.
%%% R2A/12     2014-07-08 etxberb     Changed factoryreset to sw.
%%% R2A/13     2014-07-24 etxarnu     TR HS78250: Added dnsadd/1,dnsdel/1
%%% R2A/14     2014-07-24 etxarnu     TR HS78250: Added dnsshow and content to dnsadd/dnsdel
%%% R2A/15     2014-07-24 etxarnu     TR HS78250: Corrected dnsshow
%%% R2A/16     2014-07-24 etxarnu     TR HS78250: Check for valid IP address
%%% R2A/17     2014-07-24 etxarnu     TR HS78250: Fix to handle \n correctly in dnsshow
%%% R2A/18     2014-07-25 etxarnu     Removed extra newlines in dns commands
%%% R2A/22     2014-09-03 etxberb     Removed deprecated functions.
%%% R3A/1      2014-09-26 etxarnu     Added -t option to board_restart (cold with test)
%%% R3A/2      2015-01-08 etxlg       Removed all COLI DNS stuff
%%% R3A/3      2015-02-05 etxarnu     Added board_restart options cold/warm/test
%%% R3A/4      2015-04-15 etxarnu     Bug fix in extract_pgm_recentRestart
%%% R3A/5      2015-05-22 etxarnu     Bug fix in board_escalation -p
%%% R4A/2      2015-07-09 etxjotj     HT91333 Extended discspace -c to remove 
%%%                                   SWM files too
%%% R4A/3      2015-07-10 etxarnu     Bug fix
%%% R5A/1      2015-11-13 etxarnu     added sys_mwreport
%%% R5A/2      2015-11-16 etxarnu     Improved sys_mwreport
%%% R5A/3      2016-02-29 etxarnu     Corrected board/restart to show warm restart
%%% R7A/1      2016-09-08 etxarnu     Added diagm_exportdump 
%%% R7A/2      2016-09-09 etxarnu     Improved error printouts
%%% R7A/3      2016-09-13 etxarnu     Added listbb and exportbb to exportdump
%%%                                   Encrypt exported files if secure board.
%%% R7A/4      2016-10-13 etxberb     Added options -ibu & -up in sys_sw/1.
%%% R7A/5      2016-10-30 etxarnu     Added extracted llog to pmd exportdumps
%%% R7A/6      2016-11-07 etxarnu     Use llog.1 if llog is missing in exportdumps
%%% ----------------------------------------------------------
%%% R9A/1      2017-01-23 eivmiha     switched ssh_sftp and sysSftp to ftpI
%%% R9A/2      2016-12-29 etxarnu     Added sysm/sd coli commands
%%% R9A/3      2017-02-02 estjako     Export dumps for ftpes
%%% R9A/5      2017-02-28 etxarnu     Added pg_restart for coli command
%%% R9A/6      2017-04-10 etxarnu     Added board_restart(["clear"]) ->
%%% R9A/7      2017-04-10 uabesvi     Added /diagm/showhostname
%%% R10A/1     2017-06-19 etxtory     Added -l option on discspace
%%% R10A/2     2017-07-04 uabesvi     Added fd_status
%%% R10A/3     2017-07-08 etomist     HV97965
%%% ----------------------------------------------------------
%%% R11A/1     2017-08-10 edamkon     HW19480
%%% R11A/2     2017-09-19 etxjotj     HV73597 Add coli to read kernel usage
%%% ----------------------------------------------------------
%%% R12A/1     2017-11-17 etomist     Erlang process info COLI added
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


%%% COLI commands
-export([board_escalation/1]).
-export([pg_restart/1]).
-export([board_restart/1]).
-export([os_cpuinfo/1]).
-export([os_date/1]).
-export([os_meminfo/1]).
-export([sys_discspace/1]).
-export([sys_sd/1]).
-export([sys_sw/1]).
-export([sys_mwreport/1]).
-export([diagm_exportdump/1]).
-export([show_hostname/1]).
-export([fd_status/1]).
-export([kmemavail/1]).
-export([erl_proc_info/1]).

-define(AVLI_CAUSE_ManualCOLI, "ManualCOLI").

-compile(nowarn_unused_vars).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% #---------------------------------------------------------
board_escalation([]) ->
    board_escalation(["-p"]);
board_escalation([Arg]) when Arg == "-d" orelse Arg == "false" ->
    sysInitApp:disable_escalation(),
    board_escalation(["-p"]);
board_escalation([Arg]) when Arg == "-e" orelse Arg == "true" ->
    sysInitApp:enable_escalation(),
    board_escalation(["-p"]);
board_escalation(["-c", "cnt"]) ->
    appmServer:reset_restart_list(),
    io:format("Restart counters are reset.~n", []),
    io:format("The Escalation procedure is deactivated.~n~n", []),
    board_escalation(["-p"]);
board_escalation(["cnt"]) ->
    Pgms = appmServer:get_info(pgms),
    PgmGrps = appmServer:get_info(grps),
    ColumnWidths1 = [32, 12, 10, 12, 12, 16],
    H1 = ["PROGRAM",
	  "GROUP MEMBER",
	  "ESCALATION",
	  "MAX RESTARTS",
	  "MAX TIMEOUT",
	  "RECENT RESTARTS"],
    R1 = extract_pgms_info(Pgms),
    ColumnWidths2 = [12, 10, 12, 12, 16],
    H2 = ["GROUP",
	  "ESCALATION",
	  "MAX RESTARTS",
	  "MAX TIMEOUT",
	  "RECENT RESTARTS"],
    R2 = extract_pgmGrps_info(PgmGrps),
    print_rows([H1 | R1], ColumnWidths1),
    io:format("~n"),
    print_rows([H2 | R2], ColumnWidths2),
    io:format("~n");
board_escalation(["rt", Arg]) ->
    try list_to_integer(Arg) of
	Value ->
	    appmServer:set_rollback_time(Value),
	    board_escalation(["rt"])
    catch
	_ : _ ->
	    io:format("argument error~n~n"),
	    exit(error)
    end;
board_escalation(["rt"]) ->
    io:format("Escalation Rollback timeout value = ~p seconds~n~n",
	      [appmServer:get_revert_timeout()]);
board_escalation(["-p", Arg]) ->
    board_escalation([Arg]);
board_escalation(["-p"]) ->
    Enabled = sysInitApp:is_escalation_enabled(),
    {Activated, Next} = case appmServer:get_esc_rest_type(undefined) of
			    undefined -> {false,cold};
			    Other -> {true,Other}
			end,
			    
    Revert = appmServer:get_revert_state(),
    ColumnWidths = [8, 10, 16, 20],
    H = ["ENABLED", "ACTIVATED", "NEXT RANK", "NEXT REVERT ACTION"],
    R = [atom_to_list(Enabled),
	 atom_to_list(Activated),
	 atom_to_list(Next),
	 atom_to_list(Revert)],
    print_rows([H, R], ColumnWidths),
    io:format("~n");
board_escalation(_) ->
    io:format("argument error~n~n").

%%% #---------------------------------------------------------
%%% Code for /diagm/exportdump
%%% #---------------------------------------------------------
-define(DUMPS,filename:join([sysEnv:rcs_dir(),"dumps"])).
-define(PMDS,filename:join([?DUMPS,"pmd"])).
-define(APPDUMP,filename:join([?DUMPS,"appdump"])).

diagm_exportdump([]) ->
    io:format("Write 'help exportdump' for usage~n~n");
diagm_exportdump(["list"]) ->
    case file:list_dir(?PMDS) of
	{ok,DirList} ->
	    [
	     io:format("~s   ~s ~n",[X,File]) ||
		X <- lists:sort(DirList),
		File <- [get_file(filename:join(?PMDS,X))]],
	    ok;
	{error,enoent} ->
	    io:format("No dumps found~n")
    end;
diagm_exportdump(["listbb"]) ->
    diagm_exportdump(["listbb","*"]) ;
diagm_exportdump(["listbb"|WC]) ->
    case file:list_dir(?APPDUMP) of
	{ok,FileList} ->
	    Files=filelib:wildcard(filename:join(?APPDUMP,WC)),

	    [io:format("~s ~n",[filename:basename(X)]) ||
		X <- lists:sort(Files)],
	    ok;
	{error,enoent} ->
	    io:format("No BB dumps found~n")
    end;
diagm_exportdump(["export",Url,Password | Dirs]) ->
    case ftpI:parse_uri(Url) of
        {ok, {Proto, User, Host, Port, RemoteDir, _Query}} ->

	    %% First check if it's possible to open the 
	    %% ssh connection and sftp channel
	    {ok, _, _} = start_channel(Proto, Host, Port, User, Password),
	    stop_channel(Proto),

	    %% OK, so lets start the transfer
	    %% First open the ssh connection and sftp channel

	    {ok, SshPid, _CRef} = start_channel(Proto, Host, Port, User, Password),
	    Files = get_files(Dirs),
	    [do_upload_pmd(Proto, SshPid, RemoteDir,File) || File <- Files],
	    stop_channel(Proto),
	    ok;
	{error,Str} ->
	    io:format( "~s~n",[Str])
    end;
diagm_exportdump(["exportbb",Url,Password | WCList]) ->
    case ftpI:parse_uri(Url) of
        {ok, {Proto, User, Host, Port, RemoteDir, _Query}} ->


	    %% First check if it's possible to open the 
	    %% ssh connection and sftp channel
	    {ok, _, _} = start_channel(Proto, Host, Port, User, Password),
	    stop_channel(Proto),

	    %% OK, so lets start the transfer
	    %% First open the ssh connection and sftp channel

	    {ok, SshPid, _CRef} = start_channel(Proto, Host, Port, User, Password),
	    Files = get_bb_files(WCList),
	    [do_upload_pmd(Proto, SshPid, RemoteDir,File) || File <- Files],
	    stop_channel(Proto),
	    ok;
	{error,Str} ->
	    io:format( "~s~n",[Str])
    end;
diagm_exportdump(_) ->
    io:format("argument error~n~n").

show_hostname(_) ->
   {ok, Hostname} = inet:gethostname(),
    io:format("~p~n", [Hostname]).

fd_status(_) ->
    {Current, Max, Procent} = sysServer:file_descriptor_status(),
    io:format("File descriptor status:\n"
	      " Currently used : ~s\n" ++
	      " Maximum value  : ~s\n" ++
	      " Procent used   : ~s\n",
	      [Current, Max, Procent]).


get_file(Dir) ->
    case file:list_dir(Dir) of
	{ok,[File]} ->
	    filename:basename(File);
	_ -> "No file found"
    end.

get_files(Dirs) ->
    get_files(Dirs,[]).
get_files([Dir|Rest],Acc) -> 
    case file:list_dir(filename:join(?PMDS,Dir)) of
	{ok,[File]} ->
	    get_files(Rest,[filename:join([?PMDS,Dir,File])|Acc]);
	_ -> 
	    get_files(Rest,Acc)
    end;
get_files([],Acc) ->
    Acc.

get_bb_files(WCs) ->
    get_bb_files(WCs,[]).
get_bb_files([WC|Rest],Acc) ->
    Files=filelib:wildcard(filename:join(?APPDUMP,WC)),
    get_bb_files(Rest,Rest++Files);
get_bb_files([],Acc) ->
    Acc.

add_llog_if_pmd("/rcs/dumps/pmd"++_=File, EncFile) ->
    case cmdres("tar tf "++File++" |grep llog") of
	{0,Res} ->
	    BaseEnc = filename:basename(EncFile),
	    NewFile = BaseEnc ++"_with_llog.tgz",
	    TmpDir = "/tmp/exportdump/",
	    AbsFile = TmpDir++NewFile,
	    filelib:ensure_dir(AbsFile),
	    {ok,OldDir} = file:get_cwd(),
	    file:set_cwd(TmpDir),
	    cmdres("rm -rf *"),
	    LlogPath = lists:last(lists:reverse(string:tokens(Res, "\n"))),
	    case cmdres("tar  xf " ++
				File ++
				" --strip-components=1 " ++
				LlogPath) of
		{0,[]} ->
		    case file:list_dir(".") of
			{ok,["llog"]} ->
			    ok;
			{ok,[Fn]} ->
			    %% sometimes only llog.1 exists
			    file:rename(Fn,"llog");
			_ ->
			    %should not happen...
			    ok
		    end,
		    EncDir = filename:dirname(EncFile),
		    cmdres("tar czf " ++
			       AbsFile ++
			       " llog " ++
			       " -C " ++ EncDir ++ " " ++
			       BaseEnc),
		    file:set_cwd(OldDir),
		    AbsFile;
		_ ->
		    file:set_cwd(OldDir),
		    EncFile
	    end;
	_ ->
	    EncFile
    end;
add_llog_if_pmd(_,EncFile) -> EncFile.

do_upload_pmd(Proto, SshPid, RemoteDir,File) ->
    EncFile = encrypt_file_if_needed(File),
    NewFile = add_llog_if_pmd(File,EncFile),
    FileBase = filename:basename(NewFile),
    RemotePath = filename:join([RemoteDir,FileBase]),
    io:format("Uploading file   ~s~n",[FileBase]),
    case ftpI:put_file(Proto, SshPid, NewFile, RemotePath) of
	ok ->
	    ok;
	{error, file, Reason} ->
	    io:format("Error reading file  ~s, Reason ~p~n",
		      [NewFile, Reason]);
	{error, ssh_sftp, Reason} ->
	    io:format("Error uploading file to  ~s, Reason  ~p~n",
		      [RemotePath, Reason]);
    {error, ftpes, Reason} ->
        io:format("Error uploading file to  ~s, Reason  ~p~n",
              [RemotePath, Reason])
    end,
    case NewFile of
    	File ->
    	    ok;
    	_ ->
    	    file:delete(NewFile)
    end,
    ok.

%%% Copied from logEsi.erl
encrypt_file_if_needed(File) ->
    Encrypt = lmaI:encrypt_esi_log(),
    case Encrypt of
	false ->
	    File;
	true ->
	    Base = filename:basename(File),
	    TmpFile = filename:join([sysEnv:rcs_root(),"tmp",Base ++ ".gpg"]),
	    PubKey = code:priv_dir(log) ++ "/gpg/public.key",
	    {0, R} = cmdres("gpg --import " ++ PubKey),
	    UserId = get_user_from_key(string:tokens(R, " ")),
	    cmdres("gpg --import " ++ PubKey
		   ++ " ; cat " ++ File 
		   ++ "| gpg "
		   ++ "--trust-model always --encrypt "
		   ++ "--output "
		   ++ TmpFile
		   ++ " --recipient " ++ UserId),
	    TmpFile
    end.

get_user_from_key([R | Rest]) ->
    case string:str(R, "@") of
	0 ->
	    get_user_from_key(Rest);
	_ ->
	    %% The string contains the user
	    %% Remove unwanted caracters
	    string:strip(string:strip(string:strip(R, both, $<), both, $\"), both, $> )
    end.

cmdres(Cmd) ->
    CmdR = Cmd++" ; echo -n \"Res=$?\"",
    Res = os:cmd(CmdR),
    Code = lists:last(string:tokens(Res, "\n=")),
    Rev = lists:reverse(Res),
    Result =
	case string:str(Rev, "\n") of
	    0 -> "";
	    Pos -> lists:reverse(string:sub_string(Rev, Pos))
	end,
    {list_to_integer(Code), Result}.

-define(MAX_UPLOAD_TIME, 5000).

start_channel(Proto, Host, Port, User, Passwd) ->
    case ftpI:start_channel_with_alt(Proto, Host, Port, User, Passwd,
					[{timeout, ?MAX_UPLOAD_TIME}]) of
	{ok, SP, C} ->
	    put(channelPid, SP),
	    put(connectionRef, C),
	    {ok, SP, C};
	{error, E1} ->
	    io:format("~p~n",[ E1]),
	    {error,E1}
    end.

stop_channel(sftp) ->
    SshPid = erase(channelPid),
    CRef = erase(connectionRef),
    stop_channel(SshPid, CRef);
stop_channel(ftpes) ->
    Pid = erase(channelPid),
    case Pid of
      undefined -> ok;
       _-> ftpI:stop_channel(ftpes, Pid)
    end.

stop_channel(undefined, _) -> ok;
stop_channel(_, undefined) -> ok;
stop_channel(SshPid, CRef) -> ftpI:stop_channel(sftp, SshPid, CRef).

%%% #---------------------------------------------------------
pg_restart([]) ->
    pg_restart(["-l"]);
pg_restart(Arg) when Arg == ["-e"] ->
    sysInitI:info_report(["COLI: pg_restart -e"]),
    appmServer:enable_pg_restart(coli);
pg_restart(Arg) when Arg == ["-d"] ->
    sysInitI:info_report(["COLI: pg_restart -d"]),
    appmServer:disable_pg_restart(coli);
pg_restart(Arg) when Arg == ["-l"] ->
    sysInitI:info_report(["COLI: pg_restart -l"]),
    PGState = 
	case appmServer:is_pg_restart_enabled() of
	    true -> "enabled";
	    false -> "disabled"
	end,
    Info2 = "PG restart is " ++ PGState,
    io:format("~n~s~n~n", [Info2]),
   PgmGrps = appmServer:get_info(grps),
    ColumnWidths2 = [12, 10, 12, 12, 16],
    H2 = ["GROUP",
	  "ESCALATION",
	  "MAX RESTARTS",
	  "MAX TIMEOUT",
	  "RECENT RESTARTS"],
    R2 = extract_pgmGrps_info(PgmGrps),
    print_rows([H2 | R2], ColumnWidths2),
    io:format("~n");
pg_restart([Arg])  ->
    sysInitI:info_report(["COLI: restart  ~p"],[Arg]),
    Res= appmServer:restart_pg(Arg),
    io:format("~s~n~n", [Res]).

%%% #---------------------------------------------------------
board_restart([]) ->
    board_restart(["-p"]);
board_restart(Arg) when Arg == ["-ew"] ->
    sysInitI:info_report(["COLI: restart -ew"]),
    appmServer:enable_warm_restart(coli);
board_restart(Arg) when Arg == ["-dw"] ->
    sysInitI:info_report(["COLI: restart -dw"]),
    appmServer:disable_warm_restart(coli);
board_restart(Arg) when Arg == ["-w"] orelse Arg == warm->
    sysInitI:info_report(["COLI: restart -w"]),
    appmI:restart_node(warm, ?AVLI_CAUSE_ManualCOLI);
board_restart(Arg )  when Arg == ["-c"] orelse Arg == cold ->
    sysInitI:info_report(["COLI: restart -c"]),
    appmI:restart_node(cold, ?AVLI_CAUSE_ManualCOLI);
board_restart(Arg) when Arg == ["-t"] orelse Arg == test ->
    sysInitI:info_report(["COLI: restart -t"]),
    appmI:restart_node(cold_with_test, ?AVLI_CAUSE_ManualCOLI);
board_restart(["-p"]) ->
    Rank = appmServer:latest_restart(),
    Info = "Latest restart was: Rank=" ++ Rank,		     
    io:format("~s~n~n", [Info]),

    WState = 
	case appmServer:is_warm_restart_enabled() of
	    true -> "enabled";
	    false -> "disabled"
	end,
    Info2 = "Warm restart is " ++ WState,
    io:format("~s~n~n", [Info2]);

board_restart(["clear"]) ->
    sysInitI:info_report(["COLI: restart clear"]),
    sysInitApp:reset_cold_cnt();

board_restart(_) ->
    io:format("argument error~n~n").

%%% #---------------------------------------------------------
os_cpuinfo(_) ->
    io:format("~s~n", [os:cmd("cat /proc/cpuinfo")]).

%%% #---------------------------------------------------------
os_date(_) ->
    io:format("~s~n", [os:cmd("date")]).

%%% #---------------------------------------------------------
os_meminfo(_) ->
    io:format("~s~n", [os:cmd("cat /proc/meminfo")]).

%%% #---------------------------------------------------------
sys_discspace([]) ->
    sys_discspace(["-p"]);
sys_discspace(["-p"]) ->
    Info = lists:nth(11, string:tokens(os:cmd("df " ++ sysEnv:rcs_dir()), " ")),
    io:format("~s~n", [Info]);
sys_discspace(["-c"]) ->
    RcsDir = sysEnv:rcs_dir(),
    io:format("Allocated disc space: ~s~n", 
	      [lists:nth(11, string:tokens(os:cmd("df " ++ RcsDir), " "))]),
    io:format("Removing files ...~n"),
    os:cmd("\\rm -rf " ++ RcsDir ++ "/applicationlogs/*/*"),
    os:cmd("\\rm -rf " ++ RcsDir ++ "/networkloader/esi*.gz*"),
    os:cmd("\\rm -rf " ++ RcsDir ++ "/erlang_disk/erl_crash_dump.*"),
    swmI:clean_files(),
    io:format("Allocated disc space is now: ~s~n", 
	      [lists:nth(11, string:tokens(os:cmd("df " ++ RcsDir), " "))]);
sys_discspace(["-l"]) ->
    RcsDir = sysEnv:rcs_dir(),
    io:format("Listing files in the archive ~n~s~n", 
	      [os:cmd("ls -lRhS --ignore=dumps " ++ RcsDir)]);
sys_discspace(_) ->
    io:format("argument error~n~n").
 
%%% #---------------------------------------------------------
sys_sd(["stop"]) ->
    comsaServDiscServer:coli_stop();
sys_sd(["start"]) ->
    comsaServDiscServer:coli_start();
sys_sd(["state"]) ->
    {ok,Res}=comsaServDiscServer:coli_state(),
    io:format("~s~n",[Res]);
sys_sd(_) ->
    io:format("argument error~n~n").

%%% #---------------------------------------------------------
sys_sw(["-fr"]) ->
    sysNetloader:coli_factory_reset(?AVLI_CAUSE_ManualCOLI);
sys_sw(["-r", "current"]) ->
    sysNetloader:coli_reinstall(?AVLI_CAUSE_ManualCOLI);
sys_sw(["-r"]) ->
    sys_sw(["-r", "current"]);
sys_sw(_) ->
    io:format("argument error~n~n").

%%% #---------------------------------------------------------
sys_mwreport(_) ->
    ErlDir = filename:join([sysEnv:rcs_dir(), "erlang"]),
    ErlDiskDir = filename:join([ sysEnv:rcs_dir(), "erlang_disk"]),
    Err =
	case get_errors(ErlDiskDir)  of
	    "" ->
		case get_errors(ErlDir)  of
		    "" ->
			"0";
		    E ->
			E
		end;
	    E ->
		E
	end,

    io:format("~s~n",[Err]).

get_errors(Dir) ->
    case os:cmd("ls "  ++ Dir) of
	    "" -> "";
	_ ->
	    Cmd = "egrep  'CRASH REPORT|ERROR REPORT' "
		++ Dir ++ "/* | wc -l",
	    Errors = os:cmd(Cmd),
	    [Err]=string:tokens(Errors,"\n"),
	    Err
    end.


kmemavail(_) ->
    Res =os:cmd("kmemavail"),
    io:format("~s~n",[Res]).

erl_proc_info(_) ->
    F = 
    fun(P) ->
        Info = erlang:process_info(P, 
            [registered_name, message_queue_len, memory, status, reductions]),
        case Info of
            undefined -> 
                io:format("~15s~50s~20s~20s~10s~25s~n",
                    [pid_to_list(P), "N/A", "N/A", "N/A", "N/A", "N/A"]);
            _ ->
                {_, N} = lists:keyfind(registered_name, 1, Info),
                Name = 
                case N of
                    [] -> "";
                    _ -> atom_to_list(N)
                end,

                {_, QueueLen} = lists:keyfind(message_queue_len, 1, Info),
                {_, Memory} = lists:keyfind(memory, 1, Info),
                {_, Status} = lists:keyfind(status, 1, Info),
                {_, Reductions} = lists:keyfind(reductions, 1, Info),

                io:format("~15s~50s~20s~20s~10s~25s~n",
                    [pid_to_list(P), Name, atom_to_list(Status),
                     integer_to_list(Reductions), integer_to_list(QueueLen),
                     integer_to_list(Memory)])
        end
    end,

    io:format("~15s~50s~20s~20s~10s~25s~n",
        ["PID", "Name", "Status", "Reductions", "Queued", "Memory [bytes]"]),
    io:format("~140.0.=s~n", ["="]),
    lists:foreach(F, erlang:processes()),
    io:format("~140.0.=s~n~n", ["="]).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% #---------------------------------------------------------
extract_pgms_info([{{Name, _}, Params} | Tail]) ->
    RecentRestarts = extract_pgm_recentRestart(Params),
    ParamsValues =
	keysfind([grp, escalation, maxRestarts, maxTimeout], 1, Params) ++
	[RecentRestarts],
    [[Name | ParamsValues] | extract_pgms_info(Tail)];
extract_pgms_info([_ | Tail]) ->
    [["unknown program info"] |  extract_pgms_info(Tail)];
extract_pgms_info([]) ->
    [].

%%% #---------------------------------------------------------
extract_pgm_recentRestart(Params) ->
    {_, RestartList} = lists:keyfind(restartList, 1, Params),
    {_, MaxTimeout} = lists:keyfind(maxTimeout, 1, Params),
    RecentRestarts =
	appmServer:recent_restarts(undefined, MaxTimeout , RestartList),
    integer_to_list(RecentRestarts).

%%% #---------------------------------------------------------
extract_pgmGrps_info([{Name, Params} | Tail]) ->
    RecentRestarts = extract_pgm_recentRestart(Params),
    ParamsValues =
	keysfind([escalation, maxRestarts, maxTimeout], 1, Params) ++
	[RecentRestarts],
    [[Name | ParamsValues] | extract_pgmGrps_info(Tail)];
extract_pgmGrps_info([_ | Tail]) ->
    [["unknown group info"] |  extract_pgmGrps_info(Tail)];
extract_pgmGrps_info([]) ->
    [].

%%% #---------------------------------------------------------
keysfind([Tag | Tail], N, Params) ->
    {_, Value} = lists:keyfind(Tag, N, Params),
    [value_to_list(Value) | keysfind(Tail, N, Params)];
keysfind([], _, _) ->
    [].

%%% #---------------------------------------------------------
make_row([H], _, _) ->
    %% Last column doesn't need fillings.
    [H];
make_row([H | Tail], ColumnWidths, FillChar) ->
    case ColumnWidths of
	[ColumnWidth | Tail2] ->
	    ok;
	[] ->
	    ColumnWidth = 16,
	    Tail2 = [],
	    sysInitI:error_report([{?MODULE, make_row},
				       missing_column_width,
				       {text, H}])
    end,
    try lists:duplicate(ColumnWidth - length(H), FillChar) of
	ColumnFill ->
	    [H ++ ColumnFill ++ " " | make_row(Tail, Tail2, FillChar)]
    catch
	_ : _ ->
	    sysInitI:warning_report([{?MODULE, make_row},
					 text_wider_than_column_width,
					 {text, H},
					 {column_width, ColumnWidth}]),
	    [lists:sublist(H, ColumnWidth) ++ " " |
	     make_row(Tail, Tail2, FillChar)]
    end;
make_row([], _, _) ->
    [].

%%% #---------------------------------------------------------
print_rows(Rows, ColumnWidths) ->
    FillChar = $ ,
    FormatedRows = [make_row(Row, ColumnWidths, FillChar) || Row <- Rows],
    [io:format("~s~n", [FormatedRow]) || FormatedRow <- FormatedRows].

%%% #---------------------------------------------------------
value_to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
value_to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
value_to_list(Value) when is_list(Value) ->
    try io_lib:format("~s", [Value]),
	Value
    catch
	_ : _ ->
	    "unknown"
    end;
value_to_list(_) ->
    "unknown".


