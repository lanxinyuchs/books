#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa /home/sirpa/dev_patches

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	make_release.escript %
%%% Author:	etxjotj
%%% Description:
%%%
%%% This script creates the necessary erlang release files which are
%%% necessary in order to start an erlang application system. Each
%%% file is created in its own function here.
%%%
%%% All files are stored in the "releases" dir. The "releases" dir is created
%%% on the current working directory from where the script is executed.
%%% The calling shell script is responible for it being called from the
%%% correct directory.
%%%
%%% The following names are used consistently throughout the script:
%%%
%%% System:string() - The system name is generated from the CXS name and id
%%% Version:string() - The system version is the CXS R-state
%%% CodeRootDir:string() - Path to the current software dir
%%% OtpRootDir:string() - Path to the otp dir
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------

-module(make_release).
-mode(compile).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/R12A/7').
-date('2017-12-04').

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
%%% -----      ---------- --------    ------------------------
%%% Removed old history
%%% ----------------------------------------------------------
%%% R4A/1      2015-06-01 etxtory     Removed http
%%% R4A/2      2015-06-25 etxarnu     Merge use soft link for FALLBACK.BUP
%%% R4A/3      2015-07-10 etxpeno     IMM timeout extended
%%% R4A/4      2015-07-20 etxjotj     Support for generating scripts at preload
%%% R4A/6      2015-08-13 etxtory     Removed sasl log
%%% R4A/7      2015-09-08 etxarnu     Added extra_db_nodes for regular
%%% R4A/13     2015-10-01 etxjotj     Support for compressed backups
%%% R4A/15     2016-02-22 etxpeno     set the restart_policy in safs to {one_for_one, 0, 1}
%%% ----------------------------------------------------------
%%% R5A/1      2015-10-07 etxjotj     Mnesia on tmp
%%% R5A/2      2016-02-15 etxarnu     Handle arch i686 for VRCS
%%% R5A/3      2016-02-23 etxpeno     set the restart_policy in safs to {one_for_one, 0, 1}
%%% R5A/5      2016-04-18 ehsake      Don't expose RBS name in index.html, HU72500
%%% R6A/3      2016-05-27 etxarnu     extra_db_nodes for vrcs
%%% R7A/1      2016-08-15 etxarnu     added support for aarch64 architecture
%%% R7A/2      2016-09-05 etxarnu     OTP-19: changed random: -> rand:
%%% R7A/3      2016-10-26 etxpeno     Support for IMM related dumps at warm restart
%%% R8A/2      2016-12-19 eolaand     Add /vnf directory in cloud environment
%%% R9A/2      2017-03-24 etomist     HV73396
%%% R9A/3      2017-03-24 etxberb     Added ensure_home/0 & 2.
%%% R9A/4      2017-04-04 ekurnik     Added ftpes to port_conf file
%%% ----------------------------------------------------------
%%%            2016-09-15 etxaldu     remove CXC dependency from paths
%%%            2016-11-29 etxarnu     Updated for 64bit prod struct
%%% ----------------------------------------------------------
%%%            2017-09-06 etxarnu     Updated for sim32 prod struct
%%% R11A/1     2017-09-21 etxarnu     Fixes for rcs-sim
%%%                                   and merged the above from git.
%%% R11A/2     2017-09-21 etxarnu     Bug fix
%%% ----------------------------------------------------------
%%% R12A/1     2017-11-08 etxberb     Added vRCS_init_config/0.
%%% R12A/2     2017-11-09 etxberb     Changed make_release_fun to
%%%                                   make_release_actions
%%% R12A/4     2017-11-20 etxarnu     Removed CXC parts of paths
%%% R12A/5     2017-11-30 etxpeno     Increase imm_cb_timeout to 60000
%%% R12A/6     2017-12-04 etxarnu     Handle port.conf for vrcs as for target
%%% R12A/7     2017-12-04 etxarnu     bug fix in above change.
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% In escripts main/1 is always called first

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include_lib("xmerl/include/xmerl.hrl").
-define(START_RESERVED_PORTRANGE, 27000).
-define(END_RESERVED_PORTRANGE, 27999).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type main(System, Version, Path, OtpPath)->            %#
%%%     ok | error().                                       %#
%%% Input: System:string() - System release name
%%%        Version:string() - System release version
%%%        Path:string() - Code root directory
%%%        OtpPath:string() - Otp root directory
%%% Output: -
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
main([]) -> ok;  %to test for compilation errors

main([Path, OtpPath, LogFile]) ->
   {ok, HomeDir} = file:get_cwd(),
    main([Path, OtpPath, HomeDir, LogFile]);


main([Path, OtpPath, HomeDir, LogFile]) ->
    put(home_dir, HomeDir),
    put(log_file, LogFile),

    ModVsn = proplists:get_value(vsn, ?MODULE:module_info(attributes)),
    SwmDir = swm_dir(),
    info_msg("make_release (~s)  entered /hacked version/~n"
	     "working dir: ~s~n"
	     "    OTP dir: ~s~n"
	     "   home dir: ~s~n"
	     "    swm dir: ~s~n"
	     "    logfile: ~s~n",
	     [atom_to_list(hd(ModVsn)), Path, OtpPath, HomeDir, SwmDir, LogFile]),

    ensure_home(),
    CmdSwm = "ls -la " ++ swm_dir(),
    info_msg("~s~n~s~n", [CmdSwm, os:cmd(CmdSwm)]),
    CmdSwmIC = "ls -la " ++ init_config_dir(),
    info_msg("~s~n~s~n", [CmdSwmIC, os:cmd(CmdSwmIC)]),
    %% If there is a "restore" directory in the home directory it is
    %% assumed to contain a backup, and the contents of the backup
    %% shall be used
    case restore_type() of
	noRestore ->
	    MR_complete = filename:join(home_dir(), "make_release_complete"),
	    case filelib:is_file(MR_complete) of
		true ->
		    info_msg("The release is complete. Ending run.~n"),
		    ok;
		false ->
		    make_release(Path, OtpPath)
	    end;
	Type ->
	    make_release_from_backup(Type)
    end;

main([Path, OtpPath, HomeDir, "upgrade", LogFile]) ->
    put(home_dir, HomeDir),
    put(log_file, LogFile),

    ModVsn = proplists:get_value(vsn, ?MODULE:module_info(attributes)),
    info_msg("make_release (~s) upgrade entered~n", [ atom_to_list(hd(ModVsn))]),
    make_release_at_upgrade(Path, OtpPath).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
make_release(Path, OtpPath) ->
    [_, {vsn, MakeReleaseVersion}|_] = make_release:module_info(attributes),
    MRV = atom_to_list(hd(MakeReleaseVersion)),

    AppRoot = normalize_path(Path),
    OtpRoot = normalize_path(OtpPath),
    {System, Version} = read_cxs_data(AppRoot),
    make_start_data(System, Version),
    make_rel_file(System, Version, AppRoot, OtpRoot, MRV),
    make_start_script(System, Version, AppRoot, OtpRoot),
    make_RELEASES(System, Version, AppRoot, OtpRoot),

    case (simulated =:= rcs_mode()) and (is_vrcs() == false)
	and filelib:is_file(filename:join(home_dir(), "upgrade_init")) of
	true ->
	    info_msg("upgrade (simulated): copy previous port config", []),
	    copy_port_conf(Version);
	_ ->
	    make_port_conf(Version)
    end,

    make_sys_config(Version),
    info_msg("make_release ready ~n"),
    halt(0,[{flush,false}]). %workaround for beam hanging, etxarnu 131218

make_release_from_backup(_Type) ->
    %% So at this point, we should see a home directory that looks as if
    %% it is an initial install, that is the directories should be
    %% /home/sirpa/restore
    %% /home/sirpa/software
    %% /home/sirpa/start

    BuDir = filename:join(home_dir(), "restore"),
    BuPathGz = filename:join([BuDir, "mnesia_backup.gz"]),
    BuPath =
	case filelib:is_file(BuPathGz) of
	    true ->
		info_msg("Installing compressed backup~n"),
		TmpPath = filename:join(tmp_dir(), "mnesia_backup"),
		os:cmd(["gunzip -c ", BuPathGz, " > ", TmpPath]),
		TmpPath;
	    false ->
		info_msg("Installing backup~n"),
		filename:join([BuDir, "mnesia_backup"])
	end,
    cmd(["mkdir -p ", mnesia_dir()]),
    cmd(["ln -s ",BuPath, " ", filename:join(mnesia_dir(), "FALLBACK.BUP")]),
    [begin
	 Path = filename:join(BuDir, File),
	 cmd(["cd ", home_dir(), " ; tar xfz ", Path])
     end||File<-["bin.tgz", "releases.tgz"]],
    cmd(["touch ", home_dir(), "/install_complete"]).

tmp_dir() ->
    filename:join(rcs_root(), "tmp").

mnesia_dir() ->
    filename:join(tmp_dir(),"mnesia").

swm_dir() ->
    filename:join(rcs_dir(), "swm").

init_config_dir() ->
    filename:join(swm_dir(), "init_config").

init_restore_file() ->
    %% Must be same as 'swmLib:init_restore_file/0'
    filename:join(init_config_dir(), "restore_info").

%%% ----------------------------------------------------------
%%% #           restore_type()
%%% Input: -
%%% Output: disk | noRestore
%%% Exceptions:
%%% Description: Determines the restore type
%%% ----------------------------------------------------------
restore_type() ->
    vRCS_init_config(),
    RestorePath = filename:join(home_dir(), "restore"),
    case filelib:is_dir(RestorePath) of
	true ->
	    disk;
	false ->
	    noRestore
    end.

%%% ----------------------------------------------------------
vRCS_init_config() ->
    %% For vRCS, the home directory cannot be prepared in advance. Instead, we
    %% must check for initial configuration in:
    %% <File name determined by 'swmLib:init_restore_file/0'>
    %% /rcs/swm/init_config/restore_info
    %% Actions to be executed by make_release is
    %% #{make_release_actions => list(#{})}
    try
	vRCS_init_config(file:read_file(init_restore_file()))
    catch
	EC : ER ->
	    Stacktrace = erlang:get_stacktrace(),
	    error_logger:error_report([{EC, ER} | Stacktrace])
    end.

vRCS_init_config({ok, Bin}) when is_binary(Bin) ->
    case binary_to_term(Bin) of
	#{make_release_actions := Actions} ->
	    info_msg(init_restore_file() ++ " found~n", []),
	    vRCS_init_config_actions(Actions);
	_ ->
	    info_msg(init_restore_file() ++
		     " found, but no make_release_actions~n", [])
    end;
vRCS_init_config(_) ->
    ok.

%%% ----------------------------------------------------------
vRCS_init_config_actions([Action | Tail]) ->
    vRCS_init_config_action(Action),
    vRCS_init_config_actions(Tail);
vRCS_init_config_actions([]) ->
    ok.

%%% ----------------------------------------------------------
vRCS_init_config_action(#{mfa := {M, F, A}} = Action) ->
    try
	info_msg(maps:get(logInfo, Action), [])
    catch
	error : {badkey, logInfo} ->
	    ok;
	ExcC : ExcR ->
	    Stacktrace = erlang:get_stacktrace(),
	    info_msg("Bad logInfo:~n~p~n~p~n", [{ExcC, ExcR}, Stacktrace])
    end,
    try
	Expected = maps:get(expectedMfaResult, Action),
	Expected = apply(M, F, A)
    catch
	error : {badkey, expectedMfaResult} ->
	    apply(M, F, A)
    end;
vRCS_init_config_action(Action) ->
    info_msg("Unrecognized action:~n~p~n", [Action]).

%%% ----------------------------------------------------------
ensure_home() ->
    ensure_home(filelib:is_dir(home_dir()), 0).

ensure_home(true, Cnt) ->
    info_msg("Ensured ~s after ~p millisec~n", [home_dir(), Cnt * 50]),
    Cmd = "ls -la " ++ home_dir(),
    info_msg("~s~n~s~n", [Cmd, os:cmd(Cmd)]),
    ok;
ensure_home(false, Cnt) when Cnt < 600 ->
    timer:sleep(50),
    ensure_home(filelib:is_dir(home_dir()), Cnt + 1);
ensure_home(false, Cnt) ->
    error_logger:error_report([{"No home dir", home_dir()},
			       {"after (millisec)", Cnt * 50}]).

%%% ----------------------------------------------------------
%%% #           make_release_at_upgrade
%%% Input: -
%%% Output: -
%%% Exceptions:
%%% Description: This is for soft erlang upgrade. Not implemented in full yet.
%%% ----------------------------------------------------------
make_release_at_upgrade(Path, OtpPath) ->
    [_, {vsn, MakeReleaseVersion}|_] = make_release:module_info(attributes),
    MRV = atom_to_list(hd(MakeReleaseVersion)),

    AppRoot = normalize_path(Path),
    OtpRoot = normalize_path(OtpPath),
    {System, Version} = read_cxs_data(AppRoot),
    make_start_data(System, Version),
    {_RelPath, _AppPaths, _OtpAppPaths} =
	make_rel_file(System, Version, AppRoot, OtpRoot, MRV),
    make_start_script(System, Version, AppRoot, OtpRoot),
    _ReleasesPath = copy_RELEASES(),
    copy_port_conf(Version),
    make_sys_config(Version),

    %% OldSysVsn = make_appup(AppPaths, ReleasesPath),
    %% make_relup(System, Version, OldSysVsn, MRV),
    %% AppDirs = make_app_dirs(AppPaths++OtpAppPaths),
    %% cmd("cp -R "++releases_vsn_dir(Version)++" "++
    %% 	    releases_vsn_dir_other(Version)),
    %% CompletePath = filename:join(home_dir(), "make_release_complete"),
    %% {ok, Fd} = file:open(CompletePath, [write]),
    %% io:format(Fd, "{relpath, ~p}.~n",[RelPath]),
    %% io:format(Fd, "{appdirs, ~p}.~n",[AppDirs]),
    %% file:close(Fd).
    cmd(["touch ", home_dir(), "/make_release_complete"]).

%%% ----------------------------------------------------------
%%% #           normalize_path(Path
%%% Input: Path:string() - A file path, relative or absolute
%%% Output:
%%% Exceptions:
%%% Description: Make relative paths absolute, which means expand ~ to the
%%%              user's home directory, and relative paths relative to the
%%%              current working directory
%%% ----------------------------------------------------------
normalize_path(Path) ->
    case hd(Path) of
	$/ ->
	    Path;
	$~ ->
	    case tl(Path) of
		[] ->
		    os:getenv("HOME");
		[$/|Tail] ->
		    filename:join(os:getenv("HOME"), Tail);
		Other ->
		    Split = filename:split(Other),
		    Base = filename:dirname(os:getenv("HOME")),
		    filename:join([Base|Split])
	    end;
	_ ->
	    {ok, Cwd} = file:get_cwd(),
	    filename:join(Cwd, Path)
    end.

%%% ----------------------------------------------------------
%%% #           read_cxs_data(CodeRootDir)
%%% Input: CodeRootDir:string() - Path to the current $HOME/software dir
%%% Output:
%%% Exceptions:
%%% Description: Reads the cxs info associated with the current upgrade
%%%              package, i.e. the package unpacked in the "software" dir
%%%              and extract a system name and version from that
%%% ----------------------------------------------------------
read_cxs_data(CodeRootDir) ->
    info_msg("read_cxs_data (~s)~n", [CodeRootDir]),
    Pattern = filename:join(CodeRootDir, "*-up.xml"),
    [CxsMetaPath] = case filelib:wildcard(Pattern) of
			[] ->
			    Pattern2 = filename:join(CodeRootDir, "cxs*.xml"),
			    filelib:wildcard(Pattern2);
			Result -> Result
		    end,

    %% Read metadata to find UP name
    {CxsConfigurationE, []} = xmerl_scan:file(CxsMetaPath),

    CxsProductE = find_element(product, CxsConfigurationE),
    CxsName = find_attribute(name, CxsProductE),
    CxsProdId = find_attribute(id, CxsProductE),
    CxsVersion = find_attribute(version, CxsProductE),
    info_msg("read_cxs_data returns  (~p)~n", [{CxsName++"_"++CxsProdId, CxsVersion} ]),
    {CxsName++"_"++CxsProdId, CxsVersion}.

%%% ----------------------------------------------------------
%%% #           make_start_data(System, Version)
%%% Input: System:string() - System name
%%%        Version:string() - System version
%%% Output:
%%% Exceptions:
%%% Description: Write down the system and version data from the
%%%              current upgrade package to be used in start_rcs.sh
%%% ----------------------------------------------------------
make_start_data(System, Version) ->
    Path = filename:join(releases_dir(), "start_data"),
    info_msg("Creating ~p~n",[Path]),
    ok = filelib:ensure_dir(Path),
    {ok, Fd} = file:open(Path, [write]),
    io:format(Fd, "RCS_SYSTEM=~s~n",[System]),
    io:format(Fd, "RCS_VERSION=~s~n",[Version]),
    ok = file:close(Fd).

%%% ----------------------------------------------------------
%%% #           make_rel_file(System, Version)
%%% Input: System:string(), Version:string()
%%% Output: ok
%%% Exceptions:
%%% Description: Build .rel file according to SASL User guide 4.4.1 We
%%%              choose to include included applications here,
%%%              because we want to sort the applications in start
%%%              order, and included applications sorted after their
%%%              top applications.
%%% ----------------------------------------------------------
make_rel_file(System, Version, CodeRootDir, OtpRootDir, MRV) ->

    RelFile = System++"-"++Version++".rel",
    info_msg("Creating ~p~n",[RelFile]),

    %% Find all .app files
    SpecialAppFilePaths = find_app_files(CodeRootDir),
    SpecialApps = [list_to_atom(filename:basename(F, ".app")) ||
		      F <- SpecialAppFilePaths],

    OtpAppFilePaths = find_otp_app_files(filename:join(OtpRootDir, "lib")),
    OtpApps = [list_to_atom(filename:basename(F, ".app")) ||
		  F <- OtpAppFilePaths],

    SpecialAppItems = [make_app_item(AppFilePath, SpecialApps++OtpApps) ||
			  AppFilePath<-SpecialAppFilePaths],

    OtpPaths = [{list_to_atom(filename:basename(F, ".app")),F} ||
		   F <- OtpAppFilePaths],
    AppItems = SpecialAppItems ++ select_otp_apps(SpecialAppFilePaths,
						  OtpPaths),
    StartOrder = start_order(),
    Apps = make_start_order(StartOrder, AppItems),
    EVsn = get_erts_version(OtpRootDir),
    Release = {release, {System, Version}, {erts, EVsn}, Apps},

    VsnDir = releases_vsn_dir(Version),

    RelFilePath = filename:join(VsnDir, RelFile),

    os:cmd("rm -rf "++VsnDir),
    ok = filelib:ensure_dir(RelFilePath),

    {ok, FD} = file:open(RelFilePath, [write]),
    ok=io:format(FD, "%%% Generated by make_release.escript@@~s~n~n",[MRV]),
    ok=io:format(FD, "~p.~n", [Release]),
    file:close(FD),
    info_msg("~p created.~n",[RelFilePath]),
    {RelFilePath, SpecialAppFilePaths, OtpAppFilePaths}.

%%% ----------------------------------------------------------
%%% #           find_app_files/1, find_otp_app_files/1
%%% Input: AppRoot:string()
%%% Output: [Path:string()] - A list of paths to app files
%%% Exceptions:
%%% Description: Searches a file structure for .app files. This function
%%%              presumes a certain fil structure
%%% ----------------------------------------------------------
find_app_files(AppRoot) ->
    Pattern = filename:join([AppRoot, "*CXP*", "*", "*", "ebin", "*.app"]),
    filelib:wildcard(Pattern).

find_otp_app_files(AppRoot) ->
    SimPattern = filename:join([AppRoot, "*-0", "ebin", "*.app"]),
    case filelib:wildcard(SimPattern) of
	[] -> % Not Sim
	    Pattern = filename:join([AppRoot, "*", "ebin", "*.app"]),
            info_msg("NOT sim, found apps: ~p~n", [filelib:wildcard(Pattern)]),
	    filelib:wildcard(Pattern);
	SimApps ->
            info_msg("found sim apps: ~p~n", [SimApps]),
	    SimApps
    end.

%%% ----------------------------------------------------------
%%% #          make_app_item(AppFilePath, AllApps)
%%% Input: AppFilePath:string(), AllApps:[string()] - paths to appfiles
%%% Output: [{AppName, Version, IncludedApps, Applications}]
%%%         AppName:atom() - name of the application
%%%         Version:string() - version of the application
%%%         IncludedApps:[atom()] - applications for which this app is parent
%%%         Applications:[atom()] - apps which must be started before this app
%%% Exceptions:
%%% Description: Read the relevant app information
%%% ----------------------------------------------------------
make_app_item(AppFilePath, AllApps) ->
    case file:consult(AppFilePath) of
        {ok, [{application, AppName, Data}]} ->
            case is_match(AppFilePath, atom_to_list(AppName)) of
                true -> ok;
		false ->
                    erlang:error(
		      {error, {faulty_app_name_or_path, AppName, AppFilePath}})
            end,
            Version =  proplists:get_value(vsn, Data),
	    %% case lists:keysearch(vsn, 1, Data) of
	    %%     {value, {vsn, Vsn}} -> Vsn;
	    %%     false -> erlang:error(no_vsn_tag, [AppFilePath])
	    %% end,
	    IncludedApplications =
		proplists:get_value(included_applications, Data, []),
            %% Sort out those included application names which is not
            %% part of this configuration
            IncludedApplications2 = [A || A <- IncludedApplications,
                                          lists:member(A, AllApps)],
	    Applications = proplists:get_value(applications, Data, []),
            {AppName, Version, IncludedApplications2, Applications};
        {error, Reason} ->
	    error_logger:error_report([{mfa, {file, consult, AppFilePath}},
				       {error, Reason}]),
            erlang:error({error, Reason}, [AppFilePath])
    end.

%%% ----------------------------------------------------------
%%% #          select_otp_apps(Paths, OtpApps)
%%% Input: Paths:[string()] - List of paths to .app files
%%%        OtpApps:[{AppName:atom(), AppPath:string()}] -
%%%             name of the application and path to the .app file
%%% Output: [{AppName, Version, IncludedApps, Applications}]
%%%         AppName:atom() - name of the application
%%%         Version:string() - version of the application
%%%         IncludedApps:[atom()] - applications for which this app is parent
%%%         Applications:[atom()] - apps which must be started before this app
%%% Exceptions:
%%% Description: For all appfiles, select among apps listed in the application
%%%              field, any otp apps that's listed in the OtpApps list
%%% ----------------------------------------------------------
select_otp_apps([], _) -> [];
select_otp_apps([Path|SpecialApps], OtpApps) ->
    OtpItems = select_otp_apps(SpecialApps, OtpApps),

    {ok, [{application, _, Data}]} = file:consult(Path),
    Applications = proplists:get_value(applications, Data, []),
    lists:foldl(fun(App, Accu) -> select_otp_apps(App, Accu, OtpApps) end,
		      OtpItems, Applications).

%% Description: If an application as listed a dependency to Application
%%%             it is checked if this item is part of the OtpItems list
%%%             or otherwise it is picked from OtpApps list if present
select_otp_apps(Application, OtpItems, OtpApps) ->
    case lists:keymember(Application, 1, OtpItems) of
	true ->
	    OtpItems;
	false ->
	    case lists:keysearch(Application, 1, OtpApps) of
		{value, {_, Path}} ->
		    {ok, [{application, _, Data}]} = file:consult(Path),
		    Applications = proplists:get_value(applications, Data, []),
		    NewOtpItems = [make_app_item(Path, OtpApps)|OtpItems],
		    lists:foldl(fun(App, Accu) ->
					select_otp_apps(App, Accu, OtpApps)
				end,
				NewOtpItems, Applications);
		false ->
%%%		    warning_msg("Could not find ~p~n",[Application]),
		    OtpItems
	    end
    end.

start_order() ->
    [kernel, stdlib, sasl, sysinit, mnesia, sysdb].

%%% ----------------------------------------------------------
%%% #           make_start_order(OrderedApps, Items)
%%% Input: OrderedApps:[atom()]
%%%        Items:[{AppName:atom(), Vsn:string(), Inc:[atom()], Apps:[atom()]}]
%%% Output: {AppName:atom(), Vsn:string(), Inc:[atom()]}
%%% Description: Given a list of application names, sort the release app list
%%%              so that the given applications names are sorted in order,
%%%              and included apps sorted immediately after their main
%%%              application
%%% ----------------------------------------------------------
make_start_order(OrderedApps, Items) ->
    ets:new(appitems, [public, set, named_table, {keypos, 1}]),
    [ets:insert(appitems, Item)||Item<-Items],
    First = make_start_order(OrderedApps),
    Second = secondary_start_order(),
    ets:delete(appitems),
    First++Second.

make_start_order([App|T]) ->
    case ets:lookup(appitems, App) of
	[{App, Vsn, Inc, Apps}] ->
	    ets:delete(appitems, App),
	    PrecedingApps = make_start_order(Apps),
	    PrecedingApps ++ [{App,Vsn,Inc}] ++ make_start_order(Inc++T);
	[] ->
	    make_start_order(T)
    end;
make_start_order([]) -> [].

secondary_start_order() ->
    {MasterApps, SimpleApps} =
        lists:foldl(fun({_, _, [], _}=AppItem, {MasterA, SimpleA}) ->
                            {MasterA, SimpleA++[AppItem]};
                       (AppItem, {MasterA, SimpleA}) ->
                            {MasterA++[AppItem], SimpleA}
                    end, {[], []}, ets:tab2list(appitems)),
    case MasterApps of
	[] -> % No more master apps: end the recursion
	    [begin
		 ets:delete(appitems, AppName),
		 {AppName, Vsn, Inc}
	     end||{AppName, Vsn, Inc, _}<-SimpleApps];
        MasterApps ->
            %% The internal order of the master apps is not important
            %% because than they would also be specified in the start
            %% order
            make_start_order([element(1,Item)||Item<-MasterApps])++
		secondary_start_order()
    end.

%%% ----------------------------------------------------------
%%% #           get_erts_version(OtpRoot)
%%% Input: OtpRoot:string()
%%% Output: string() - i.e. "5.3.2.1"
%%% Exceptions:
%%% Description: Find the version of erts given in a OTP software file
%%%              structure.
%%%              We assume there is only one erts directory, and that it is
%%%              named for example "erts-5.3.2.1"
%%% ----------------------------------------------------------
get_erts_version(OtpRoot) ->
    {ok, Files} = file:list_dir(OtpRoot),
    case catch [case X of
		    "0" -> throw(sim);
		    _ -> X
		end|| "erts-" ++ X <- Files] of
	sim ->
	    {ok, Bin} = file:read_file(filename:join([OtpRoot, "bin", "erl"])),
	    Text = binary_to_list(Bin),
	    case re:run(Text, "BINDIR.*erts-[0-9]+(\.[0-9]+)*") of
		nomatch ->
		    erlang:error({no_erts_found, Bin});
		{match, [{Start, Length}|_]} ->
		    BinDirStr = string:substr(Text, Start+1, Length),
		    {match, [{Vstart,Vlength}|_]} =
			re:run(BinDirStr,"[0-9]+(\.[0-9]+)*"),
		    string:substr(BinDirStr, Vstart+1, Vlength)

	    end;
        [Vsn] ->
            Vsn;
        _ -> erlang:error({no_erts_found, Files})
    end.

is_match(String, RegExp) ->
    case re:run(String, RegExp) of
        {match, _ }  -> true;
        nomatch -> false
    end.

%%% ----------------------------------------------------------
%%% #           make_start_script(Version)
%%% Input: Version:string() - e.g. "R1A"
%%% Output: ok
%%% Exceptions:
%%% Description: Creates an erlang start/boot script
%%% ----------------------------------------------------------
make_start_script(System, Version, CodeRootDir, OtpRootDir) ->
    info_msg("Creating boot script~n"),

    VsnDir = releases_vsn_dir(Version),
    {ok, Cwd} = file:get_cwd(),
    info_msg("Setting current working dir to ~p (~p)~n",
             [VsnDir, file:set_cwd(VsnDir)]),
    %% Make sure cwd is restored
    %% This can be done better

    try do_make_start_script(System, Version, CodeRootDir, OtpRootDir)
    catch Any:Reason ->
            erlang:error({make_start_script_failed, {Any,Reason}}, [Version])
    after
	info_msg("Setting current working dir to ~p (~p)~n",
		 [Cwd, file:set_cwd(Cwd)])
    end.

do_make_start_script(System, Version, CodeRootDir, OtpRootDir) ->

    Variables =
        {variables,
         [{"CODE_ROOT", CodeRootDir},
          {"OTP_ROOT", OtpRootDir}]},

    OtpPattern = filename:join([OtpRootDir, "lib", "*", "ebin"]),
    CodePattern = filename:join([CodeRootDir, "*CXP*","*", "*", "ebin"]),

    %% Find duplicate app files and modules. This check is performed in
    %% systools, but we want to point out the fault in a better way
    %% This is for faultsearching only
    %%    Apps = filelib:wildcard(filename:join(OtpPattern, "*.app"))++
    %%    filelib:wildcard(filename:join(CodePattern, "*.app")),
    %%    find_double_apps(Apps),

    %% Now continue with systools

    Path = {path, [OtpPattern, CodePattern]},

    %% no_module_tests means that we won't check if the compiled file
    %% is older then the source code
    %%   Opts = [Variables, Path, no_module_tests],
    %% Fix of HL18065
    Opts = [Path, no_module_tests, silent, Variables],

    Name = System++"-"++Version,

    case systools:make_script(Name, Opts) of
        {ok,_,_} ->
            ok;
        {error,Func,Reason} ->
            throw({error,Func,Reason})
    end,
    %% End of fix of HL18065
    VsnDir = releases_vsn_dir(Version),
    OldName = filename:join(VsnDir, Name++".boot"),
    NewName = filename:join(VsnDir, "start.boot"),
    cmd("cp "++OldName++" "++NewName),

    ok.

%%% ----------------------------------------------------------
%%% #           make_RELEASES
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
make_RELEASES(System, Version, CodeRootDir, OtpRootDir) ->
    VsnDir = releases_vsn_dir(Version),
    RelDir = releases_dir(),
    RelPath = filename:join(RelDir, "RELEASES"),
    info_msg("Creating ~p~n",[RelPath]),

    RelFile = filename:join(VsnDir, System++"-"++Version++".rel"),
    %% Find all .app files
    SpecialAppFilePaths = find_app_files(CodeRootDir),
    OtpAppFilePaths = find_otp_app_files(filename:join(OtpRootDir, "lib")),

    Paths =
	[begin
	     {ok, [{application, AppName, Data}]} = file:consult(Path),
	     Vsn = proplists:get_value(vsn, Data),
	     {AppName, Vsn, triple_dir(Path)}
	 end
	 ||Path<-SpecialAppFilePaths++OtpAppFilePaths],

    ok= release_handler:create_RELEASES(CodeRootDir, RelDir, RelFile, Paths),
    RelPath.

triple_dir(X) ->
    filename:dirname(filename:dirname(filename:dirname(X))).

copy_RELEASES() ->
    Source = filename:join(releases_dir_other(), "RELEASES"),
    Dest = filename:join(releases_dir(), "RELEASES"),
    cmd("cp "++Source++" "++Dest),
    Dest.

%%% ----------------------------------------------------------
%%% #           make_port_conf(Version)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%%  The port.conf file is used to coordinate all ports used by the
%%%  RCS middleware. Since it is supposed to run in simulated
%%%  environment in the HUB - where there are many users, it is
%%%  necessary to have dynamically assigned ports instead of fixed
%%%  ports, which is used for target.
%%%
%%% ----------------------------------------------------------
make_port_conf(Version) ->

    PortConfPath = filename:join(releases_vsn_dir(Version), "port.conf"),

    %% Patching means that the user can supply an (incomplete) port.conf
    %% file in dev patches, and those ports will be used when generating the
    %% real port.conf
    PatchPortConf = filename:join([home_dir(), "dev_patches", "port.conf"]),
    PatchedPorts =
	case filelib:is_file(PatchPortConf) of
	    true ->
		{ok, PP} = file:consult(PatchPortConf),
		PP;
	    false ->
		[]
	end,

    %% Various ports
    %%
    %% NOTE: The snmpMgrFake port is reserved for the test application (the
    %% DUMMY-* CXP). On the simulator higher ports may be used (11002 through
    %% 11010) if multiple simulator instances are running.
    %%
    Ports = [{snmpAgent,161},    % snmp agent
         {snmpMgrRem,27000}, % snmp manager (remote, for testing)
         {netconf,830},      % external netconf port, 830 ackording to RFC!
         {cli,2023},         % external cli port
         {sftp,2024},        % external sftp port
         {coli,4192},        % external coli port
         {log,2302},         % internal log service
         {sysLogClient,2303},
         {comPort,2003},     % internal com port
         {comtePort,2002},   % internal comte port
         {netconf_tls,6513}, % external netconf on TLS (RFC5539)
         {usel,8888},        % User Space Erlang Linx (for sim only)
         {cli_tls,9830},     % external cli on TLS
         {coli_tls,9831},     % external coli on TLS
         {linxgws,22001},    %  Linx GW
         {comNetconfPort,9977}, % internal netconf port
         {comCliPort,9889},  % internal cli port
         {comAuthPort,9988}, % internal com port
         {cecPort,9803},
         {imm_ct, 10005},
         {saf_log,10004},
         {ntf,10003},
         {imm_oi,10002},
         {imm_om,10001},
         {https, 443},        % https: emergency access, models and web IWD
         {https_login, 8443}, % EMGUI (secure login using user-cert)
         {pghd, 56789},
         {snmpMgrFake, 11001},
         {http, 80},          % external default http port HV58854
         {cmp, 829},         % external default cmp port HV58854
         {ftpes, 9921}],

    {ok, Fd} = file:open(PortConfPath, [write]),
    write_port_conf(Fd, rcs_mode(), Ports, PatchedPorts),
    file:close(Fd),

    info_msg("Created port.conf: ~p~n",[PortConfPath]).

write_port_conf(Fd, Mode, Ports, Patched) ->
    case {Mode, is_vrcs()} of
	{simulated, false} ->
	    %% There is a slight risk that ports will become used during
	    %% the time of this call and the actual opening of the port.
	    %% We ignore that for now. jotj 2011-07-12
	    FreePorts = read_ports(["/proc/net/tcp", "/proc/net/tcp6"]),
	    Base = case os:getenv("RCSSIM_BASE_PORT") of
		       false ->
			   rand:uniform(740)*50+2000;
		       SimBase ->
			   list_to_integer(SimBase)
		   end,

	    write_port_conf_simulated(Fd, Ports, Patched, FreePorts, Base);
	_ ->
	    write_port_conf_target(Fd, Ports, Patched)
    end.

is_vrcs() ->
    os:getenv("BT") == "VM".

write_port_conf_target(Fd, [{Name, DefaultNumber}|Rest], Patched) ->
    Number = proplists:get_value(Name, Patched, DefaultNumber),
    io:format(Fd, "~p.~n",[{Name, Number}]),
    write_port_conf_target(Fd, Rest, Patched);
write_port_conf_target(_, [], _) ->
    ok.

write_port_conf_simulated(_, [], _, _, _) ->
    ok;
write_port_conf_simulated(Fd, [{Name, _}|Rest], Patched, FreePorts, Base) ->
    Number = proplists:get_value(Name, Patched,
				 NewPort = find_next(Base, FreePorts)),
    io:format(Fd, "~p.~n",[{Name, Number}]),
    write_port_conf_simulated(Fd, Rest, Patched, FreePorts, NewPort).

read_ports([Path|Paths]) ->
    P =
        case file:open(Path, [read]) of
            {ok, Fd} ->
                io:get_line(Fd, ""), % throw away header
                Ports = read_ports(Fd, io:get_line(Fd, "")),
                file:close(Fd),
                Ports;
            {error, Reason} ->
		error_logger:error_report([{mfa, {file, open, [Path, [read]]}},
					   {error, Reason}]),
		erlang:error(Reason, [[Path|Paths]])
        end,
    P++read_ports(Paths);
read_ports([]) -> [].

read_ports(_, eof) ->
    [];
read_ports(Fd, String) ->
    [_, Source|_] = string:tokens(String, " "),
    {_, P} = decode(Source),
    [P|read_ports(Fd, io:get_line(Fd, ""))].

decode(String) ->
    [Adr, Port] = string:tokens(String, ":"),
    {hex_to_ip(Adr), hex_to_int(Port)}.

hex_to_ip([A,B]) ->
    [hex_to_int([A,B])];
hex_to_ip([A,B|Tail]) ->
    hex_to_ip(Tail)++[hex_to_int([A,B])].

hex_to_int(Chars) ->
    lists:foldl(fun(X, A) when X>=$0, X=<$9->
			(X-$0)+A*16;
		   (X, A) when X>=$A, X=<$F ->
			(X-$A)+10+A*16
		end, 0, Chars).

find_next(Port, Ports) when Port>65535 ->
    erlang:error(no_ports_available, [Port, Ports]);

find_next(Port, Ports) ->
    %% Ports 27000-27999 are open in firewall for snmptraps from target.
    %% So hands off for simulated env, otherwise port collisions may occur
    %% on terminal server when running test environment.
    case (Port >= ?START_RESERVED_PORTRANGE - 1) and (Port =< ?END_RESERVED_PORTRANGE - 1) of
	true ->
	    find_next(?END_RESERVED_PORTRANGE, Ports);
	false ->
	    case lists:member(Port+1, Ports) of
		true ->
		    find_next(Port+1, Ports);
		false ->
		    Port+1
	    end
    end.

copy_port_conf(Version) ->
    Pattern = filename:join([releases_dir_other(), "*", "port.conf"]),
    info_msg("Port conf ~p~n",[Pattern]),
    [Source] = filelib:wildcard(Pattern),
    Dest = filename:join([releases_vsn_dir(Version), "port.conf"]),
    os:cmd("cp "++Source++" "++Dest),
    info_msg("Copied port.conf~n").

%%% ----------------------------------------------------------
%%% #           make_sys_config(Version)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%%  sys.config is used to override the erlang application environment
%%%  variable settings to suit the local installation.
%%% ----------------------------------------------------------

make_sys_config(Version) ->
    SysConfig = filename:join(releases_vsn_dir(Version), "sys.config"),
    info_msg("Creating ~p~n",[SysConfig]),
    case file:open(SysConfig, [write]) of
	{ok, Fd} ->
	    Configs = [config(App, Version) ||
			  App <- [kernel, mnesia, sasl, comte, safs, inets,
				  eitc]],
	    ok = io:format(Fd, "~p.~n", [Configs]),
	    file:close(Fd),
	    info_msg("sys.config complete~n");
	{error, Reason} ->
	    error_logger:error_report(
	      [{mfa, {file, open, [SysConfig, [write]]}},
	       {error, Reason}]),
	    erlang:error(Reason, [Version])
    end.

config(kernel, _) ->
    Nodes = [node()],
    {kernel, [%{sync_nodes_timeout, 5001},
	      {start_disk_log, true},
	      {start_timer, true},
	      {start_os, true},
	      {dist_auto_connect, once},
	      {start_dist_ac, false},
	      {sync_nodes_optional, Nodes},
	      {net_ticktime, 8},
	      {net_setuptime, 10},
	      {error_logger_format_depth, 30},
	      {shutdown_func, {sys_app, shutdown_func}}] ++
	 case os:cmd("cup -c >/dev/null 2>&1;echo -n $?") of
	     "0" -> %the board has VC -> secure
		 [{inet_dist_use_interface, {127,0,0,1}}];
	     _ ->
		 []
	 end
    };

config(mnesia, _) ->
    MCoreDir = filename:join(rcs_dir(), "db/mnesia_cores"),
    filelib:ensure_dir(MCoreDir ++ "/"),
    MnesiaDir = mnesia_dir(),
    erlang:display(home_dir()),
    Extra = case os:getenv("MPID") of
		X when X == false ; X == "1" ; X == "2" ->
		    [];
		_ ->
		    case is_vrcs() of
			true ->
			    [{extra_db_nodes, [root@vrcs_du1]}];
			_ ->
			    [{extra_db_nodes, [sirpa@du1]}]
		    end
	    end,
    {mnesia, [{core_dir, MCoreDir},
	      {dump_log_write_threshold, 800},
	      {dump_log_time_threshold, 180000},
              {no_table_loaders, 4},    % Defaults to 2
	      {dir, MnesiaDir}] ++ Extra};

config(sasl, _Vsn) ->
    %% No sasl log file
    ReleasesDir = releases_dir(),
    {sasl,
     [{sasl_error_logger, false},
      {releases_dir, ReleasesDir}]};

config(comte, Vsn) ->
    ConfigDir = releases_vsn_dir(Vsn),
    ComteDir = filename:join(ConfigDir, "comte"),
    filelib:ensure_dir(filename:join(ComteDir, "x")),

    ComteLogDir = filename:join(vnf_dir(), "comte"),
    filelib:ensure_dir(filename:join(ComteLogDir, "x")),

    PatchDir = filename:join( [home_dir(), "dev_patches"]),

    CxpDirP = filename:join( [home_dir(), "software", "*"]),

    [ComP] = filelib:wildcard(
	       filename:join(
		 [CxpDirP, "*", "com-*", "priv"])),
    ComPatched = filelib:wildcard(
		   filename:join(
		     [PatchDir, "*", "com-*", "priv"])),
    Arch = os:cmd("arch"),
    TgtDir =
	case Arch of
	    "i686\n"  ->
		case sim32() of
		    true ->
			"tgt_i686";
		    false ->
			"tgt_i686_32"
		end;
	    "x86_64\n"  ->
		"tgt_x86_64";

	    Arch when Arch == "armv7l\n" orelse  Arch == "aarch64\n" ->
		case os:getenv("ARMDIR") of
		    false ->
			"tgt_arm";
		    Dir ->
			"tgt_"++Dir
		end

	end,
    ComTop = case ComPatched of
		 [] ->
		     filename:join([ComP,TgtDir]);
		 [ComPatch] ->
		     filename:join([ComPatch,TgtDir])
	     end,

    [ComteTop] = filelib:wildcard(
		   filename:join(
		     [CxpDirP, "*", "comte-*"])),
    ComtePrivDir = filename:join( [ComteTop, "priv"]),

    ComteLibDir = case Arch of
		      "i686\n" ->
			  case sim32() of
			      true ->
				  filename:join([ComtePrivDir, "tgt_i686", "lib32"]);
			      false ->
				  filename:join([ComtePrivDir, "tgt_i686_32", "lib32"])
			  end;
		      "x86_64\n" ->
			  filename:join([ComtePrivDir, "tgt_x86_64", "lib64"]);
		      Arch when Arch == "armv7l\n" orelse Arch ==  "aarch64\n" ->
			  filename:join([ComtePrivDir, arm_dir(), "lib32"])
		  end,
    ComteLib = case file:read_file_info(
		      filename:join( [PatchDir, "libComtE.so"])) of
		   {ok,_} ->
		       filename:join( [PatchDir, "libComtE.so"]);
		   _ ->
		       filename:join([ComteLibDir , "libComtE.so"])
	       end,
    ComteCfgFile =  case file:read_file_info(
			   filename:join( [PatchDir, "libComtE.cfg"])) of
			{ok,_} ->
			    filename:join( [PatchDir, "libComtE.cfg"]);
			_ ->
			    filename:join([ComtePrivDir , "libComtE.cfg"])
		    end,

%%%  The following is a fix until comte allows you to specify the libComtE.cfg file separately
    ComteLibNew = filename:join([ConfigDir,"libComtE.so"]),
    file:make_symlink(ComteLib, ComteLibNew),
    file:make_symlink(ComteCfgFile, filename:join([ConfigDir,"libComtE.cfg"])),
%%%

    StartScript = filename:join(ComteDir, "start_com"),
    Ports = begin
		PortConfPath = filename:join(ConfigDir, "port.conf"),
		{ok, Data} = file:consult(PortConfPath),
		{value, {_, ComPort}} = lists:keysearch(comPort, 1, Data),
		{value, {_, ComtePort}} = lists:keysearch(comtePort, 1, Data),
		{value, {_, ComNetconfPort}} =
		    lists:keysearch(comNetconfPort, 1, Data),
		{value, {_, ComCliPort}} = lists:keysearch(comCliPort, 1, Data),
		[{com_port, ComPort},
		 {comte_port, ComtePort},
		 {netconf_port, ComNetconfPort},
		 {cli_port, ComCliPort}]
	    end,

    SysConfigItems =
	[{callback_registrar, filename:join(ComteDir, "comte_registrar.dets")},
	 {com_top, ComTop},
	 {com_start_mw, true}, %% ???,
	 %% COM will be started by gmf
	 {start_com_prog, StartScript},
	 {start_com, false},
	 {start_com_timeout, 120000},
	 {healthcheck_interval, 30000},
	 {healthcheck_timeout, 25000},
	 {comte_com_timeout, 20000},
	 {max_restarts, 3},
	 {max_restarts_time_limit, 300},
	 {com_conf_dir, ComteDir},
	 {com_run_dir, ComteLogDir},
	 {com_conf_wildcard, "lib*.cfg"},
	 {comte_lib, ComteLibNew },  % for comsaDataInit to read
	 {pidfile_dir, ComteDir},
	 {replicated_list, comte_default_replicated_list},
	 {object_impl, comte_default_object_implementer}, %old comte 131217
	 {oi_register, comte_default_oi_register}, %new comte 131217
	 {transaction_server, comsaTransactionServer},
	 {crypto, comsaPass},
	 {access_mgmt, comsaRoles},
	 {log_write, logComteCbWrite},
	 {template_dir,""},
	 {pm, pmsShowCountersI},
	 {com_initial_ha_mode_timeout,10000},
	 {com_ip, "localhost"},
	 {comte_ip, "localhost"}] ++
	Ports,

    {comte, SysConfigItems};

config(safs, Vsn) ->
    ConfigDir = releases_vsn_dir(Vsn),
    PortConfPath = filename:join(ConfigDir, "port.conf"),
    {ok, Data} = file:consult(PortConfPath),
    {value, {_, ImmOiPort}} = lists:keysearch(imm_oi, 1, Data),
    {value, {_, ImmOmPort}} = lists:keysearch(imm_om, 1, Data),
    {value, {_, ImmCtPort}} = lists:keysearch(imm_ct, 1, Data),
    {value, {_, NtfPort}} = lists:keysearch(ntf, 1, Data),
    {value, {_, LogPort}} = lists:keysearch(saf_log, 1, Data),
    LogConf = filename:join(ConfigDir, "saf_log.cfg"),
    LogRoot = filename:join(vnf_dir(), "saf_log"),
    LocalHost = "127.0.0.1",
    %% Put the IMM related dumps in /rcs/comte/ directory
    InfoDumpDir = filename:join(vnf_dir(), "comte"),

    {safs, [{imm_oi_port, {LocalHost, ImmOiPort}},
	    {imm_om_port, {LocalHost, ImmOmPort}},
	    {imm_ct_port, {LocalHost, ImmCtPort}},
	    {ntf_port, {LocalHost, NtfPort}},
	    {log_port, {LocalHost, LogPort}},
	    {log_cfg, LogConf},
	    {log_root, LogRoot},
	    {imm_ct_cb, gmfImmUgIcti},
	    {imm_sync_cb, {comsaSyncServer, sync}},
	    {imm_ccb_completed_cb_timeout, 20000},
	    {imm_ccb_apply_cb_timeout, 60000},
	    {imm_ccb_abort_cb_timeout, 10000},
	    {imm_cb_timeout, 60000},
	    {imm_clean_rt_on_oi_finalize, true},
	    {restart_policy, {one_for_one, 0, 1}},
	    {info_dump_dir, InfoDumpDir}
	   ]
    };
config(inets, Vsn) ->
    ConfigDir     = releases_vsn_dir(Vsn),
    ServerRoot    = filename:join([ConfigDir, "www", "server_root"]),
    DocRoot       = filename:join([ConfigDir, "www", "document_root"]),
    SecServerRoot = filename:join([ConfigDir, "www_sec", "server_root"]),
    SecDocRoot    = filename:join([ConfigDir, "www_sec", "document_root"]),

    %% RCS_ROOT/rcs/document_root & RCS_ROOT/rcs/server_root
    os:cmd("mkdir -p " ++ ServerRoot),
    os:cmd("mkdir -p " ++ DocRoot),
    os:cmd("mkdir -p " ++ SecServerRoot),
    os:cmd("mkdir -p " ++ SecDocRoot),
    CxpDirP = filename:join([home_dir(), "software", "*"]),
    SysP = filename:join([CxpDirP, "*",
			  "sys-*", "priv", "images",
			  "favicon.ico"]),
    [FavIconPath] = filelib:wildcard(SysP),
    file:copy(FavIconPath, filename:join(DocRoot, "favicon.ico")),
    IndexPath = filename:join(DocRoot, "index.html"),

    {ok, Fd2} = file:open(IndexPath, [write]),
    io:format(Fd2, "<html><head><title></title></head>~n"
	      "<body></body>~n"
	      "</html>~n",[]),
    file:close(Fd2),
    {inets, []};

config(eitc, _Vsn) ->
    PatchDir = filename:join([home_dir(), "dev_patches"]),

    CxpDirP = filename:join([home_dir(), "software", "*"]),

    case filelib:wildcard(
	   filename:join([CxpDirP, "*", "eitc-*"])) of
	[TopDir] ->

	    PrivDir = filename:join([TopDir, "priv"]),
	    LibDir = case os:cmd("arch") of
			 "i686\n"  ->
			     case sim32() of
				 true ->
				     filename:join([PrivDir, "tgt_i686", "lib32"]);
				 false ->
				     filename:join([PrivDir, "tgt_i686_32", "lib32"])
			     end;
			 "x86_64\n"->
			     case is_vrcs() of
				 true ->
				     filename:join([PrivDir, "tgt_x86_64", "lib64"]);
				 _ ->

				     filename:join([PrivDir, "tgt_i686", "lib32"])
			     end;
			 Arch when Arch == "armv7l\n" orelse  Arch == "aarch64\n" ->
			     filename:join([PrivDir, arm_dir(), "lib32"])
		     end,

	    {eitc, [{driver_path, [PatchDir, LibDir]}]};
	[] ->
	    {eitc, []}
    end.


%%% UPGRADE RELATED

%%% ----------------------------------------------------------
%%% #           make_appup(AppFiles)
%%% Input: AppFiles:[string()] - A list of paths to app files
%%% Output:
%%% Exceptions:
%%% Description: Creates appup files for all applications except otp
%%% ----------------------------------------------------------
%% make_appup(AppFiles, ReleasesPath) ->
%%     try do_make_appup(AppFiles, ReleasesPath)
%%     catch Type:Reason ->
%% 	    erlang:Type(Reason, [AppFiles])
%%     end.

%% do_make_appup(AppFilePaths, ReleasesPath) ->
%%     info_msg("Making appup scripts~n",[]),
%%     cmd("rm -f /tmp/*.appup"),
%%     {ok, [Releases]} = file:consult(ReleasesPath),
%%     {value, {release, _System, Version, _Erts, Apps, _State}} =
%% 	lists:keysearch(permanent, 6, Releases),

%%     %% Files =
%%     [make_appup_file(AppFilePath, Apps)||
%% 	AppFilePath<-AppFilePaths, not is_appup_file(AppFilePath)],
%%     %% info_msg("All appup files created~n~p~n",[Files]),
%%     Version.

%% is_appup_file(AppFilePath) ->
%%     AppupFilePath = AppFilePath++"up",
%%     filelib:is_file(AppupFilePath).

%% make_appup_file(AppFilePath, CurrentApps) ->
%%     %% info_msg("make_appup_file(~p, CurrentApps)~n",[AppFilePath]),
%%     AppDir = filename:dirname(AppFilePath),
%%     AppFile = filename:basename(AppFilePath),
%%     App = filename:basename(AppFile, ".app"),

%%     AppName = list_to_atom(App),
%%     OldAppVsn =
%% 	case lists:keysearch(AppName, 1, CurrentApps) of
%% 	    {value, {AppName, OAV, _}} -> OAV;
%% 	    false -> new_app
%% 	end,

%%     {match, [{Start, Length}|_]} = re:run(AppFilePath, App++"\-.*\/ebin"),
%%     AppAndVsn = string:substr(AppFilePath, Start+1, Length-5),
%%     [App, NewAppVsn] = string:tokens(AppAndVsn, "-"),

%%     case OldAppVsn of
%% 	new_app ->
%% 	    ok;
%% 	NewAppVsn -> %% Same version. Nothing to do in the upgrade
%% 	    make_empty_appup(AppFilePath, OldAppVsn, NewAppVsn);
%% 	_ ->
%% 	    UgInstrPath = filename:join(AppDir, App++".upg"),
%% 	    case file:consult(UgInstrPath) of
%% 		{error, enoent} when OldAppVsn == NewAppVsn ->
%% 		    ok;
%% 		{error, enoent} ->
%% 		    UpgData = make_default_upgdata(AppFilePath),
%% 		    do_make_appup_file(
%% 		      AppFilePath, OldAppVsn, NewAppVsn, UpgData);
%% 		{ok, [{NewAppVsn, UpgData, _}]} ->
%% 		    do_make_appup_file(
%% 		      AppFilePath, OldAppVsn, NewAppVsn, UpgData)
%% 	    end
%%     end.

%% make_default_upgdata(AppFilePath) ->
%%     {ok, [{application, _, Info}]} = file:consult(AppFilePath),
%%     {value, {modules, Mods}} = lists:keysearch(modules, 1, Info),
%%     [{other, {swm_default, Mods}}].

%% make_empty_appup(AppFilePath, OldAppVsn, NewAppVsn) ->
%%     AppFile = filename:basename(AppFilePath),
%%     Instructions =
%% 	[{apply, {io, format, ["Upgrading ~s~n",[AppFile]]}},
%% 	 {apply, {io, format, ["No instructions for ~s~n",[AppFile]]}}],
%%     Appup = {NewAppVsn, [{OldAppVsn, Instructions}], []},
%%     create_appup(AppFilePath, Appup).


%% do_make_appup_file(AppFilePath, OldAppVsn, NewAppVsn,  UpgData) ->
%%     AppFile = filename:basename(AppFilePath),

%%     %% Check for the correct OldAppVsn
%%     case lists:keysearch(OldAppVsn, 1, UpgData) of
%% 	{value, {OldAppVsn, {swm_default, ModuleOrder}}} ->
%% 	    Msg = "Executing default generated appup...",
%% 	    UpgInstr =
%% 		[{apply, {io, format, ["~s~n",[Msg]]}}|
%% 		 make_appup_instructions(
%% 		   ModuleOrder, {OldAppVsn, NewAppVsn})],
%% 	    upg_direct(AppFilePath, NewAppVsn, UpgInstr);
%% 	{value, {OldAppVsn, UpgI}} ->
%% 	    upg_direct(AppFilePath, NewAppVsn, UpgI);
%% 	false ->
%% 	    %% Search for the 'other' version
%% 	    case lists:keysearch(other, 1, UpgData) of
%% 		{value, {other, {swm_default, ModuleOrder}}} ->
%% 		    Msg = "Executing default generated appup...",
%% 		    UpgInstr =
%% 			[{apply, {io, format, ["~s~n",[Msg]]}}|
%% 			 make_appup_instructions(
%% 			   ModuleOrder, {OldAppVsn, NewAppVsn})],
%% 		    upg_direct(AppFilePath,  NewAppVsn, UpgInstr);
%% 		{value, {other, UpgI}} ->
%% 		    upg_direct(AppFilePath, NewAppVsn, UpgI);
%% 		false ->
%% 		    erlang:error({no_upgrade_script_for, AppFile, OldAppVsn},
%% 				 [AppFile, OldAppVsn, NewAppVsn, UpgData])
%% 	    end
%%     end.

%% make_appup_instructions([Module|Order], AppVsns) when is_atom(Module)->
%% 						%    Name = atom_to_list(Module)++".beam",

%% 						%    Timeout = 30000,
%%     Nodes = all_nodes(),

%%     [{update, Module, {advanced, AppVsns}, soft_purge, soft_purge, []},
%%      {apply, {swmReleaseMgr, upgrade_module, [Module, AppVsns]}},
%%      {sync_nodes, next_patch, Nodes}|
%%      %% {suspend, [{Module, Timeout}]},
%%      %% {apply, {PatchModule, load_patch, [FilePath]}},
%%      %% {sync_nodes, load_patch, Nodes},
%%      %% {code_change, [{Module, Extra}]},
%%      %% {apply, {swmPatch, do_code_change, [Module, OldVsn]}},
%%      %% {resume, [Module]},
%%      %% {sync_nodes, next_patch, Nodes}|
%%      make_appup_instructions(Order, AppVsns)];

%% %%% make_appup_instructions([],  AppVsns) -> [].
%% make_appup_instructions([], _) -> [].


%% upg_direct(AppFilePath, NewAppVsn, UpgInstr) ->
%%     AppFile = filename:basename(AppFilePath),
%%     Instructions =
%% 	[{apply, {io, format, ["Upgrading ~s~n",[AppFile]]}}|UpgInstr]++
%% 	[{sync_nodes, next_app, all_nodes()},
%% 	 {apply, {io, format, ["Upgrade of ~s complete~n",[AppFile]]}}],
%%     Appup = {NewAppVsn, [{<<".*">>, Instructions}],[]},
%%     create_appup(AppFilePath, Appup).


%% create_appup(AppFilePath, Appup) ->
%% 						%   info_msg("create_appup(~p, ~p)~n",[AppFilePath, Appup]),
%%     AppupPath = AppFilePath++"up",
%%     {ok, Fd} = file:open(AppupPath, [write]),
%%     io:format(Fd, "%%% Autogenerated appupfile~n",[]),
%% 						%    io:format(Fd, "%%% Made by make_release ~w~n", [get(make_release_vsn)]),
%%     io:format(Fd, "~p.", [Appup]),
%%     ok = file:close(Fd),
%%     %% cmd("cat "++AppupPath),
%%     AppupPath.

cmd(Cmd) ->
    info_msg("~s~n~s~n",[Cmd, os:cmd(Cmd)]).

%% all_nodes() ->
%%     [].

%% make_relup(System, NewVsn, OldVsn, MRV) ->
%%     General = filename:join([rcs_root(), "home*", "*", "software", "*", "*",
%% 			     "*", "ebin"]),
%%     Otp = filename:join([rcs_root(), "home*", "*", "software", "*", "*",
%% 			 "*", "priv", "pkg", "*", "*", "ebin"]),
%%     OtpSim = filename:join([rcs_root(), "..", "OTP", "*", "*", "ebin"]),
%%     Path = [General, Otp, OtpSim,
%% 	    releases_vsn_dir_other(OldVsn),
%% 	    releases_vsn_dir(NewVsn)],

%%     Name = System++"-"++NewVsn,
%%     DownTo = [],
%%     Descr = {created, calendar:local_time(), {make_release_vsn, MRV}},
%%     UpFrom = [{System++"-"++OldVsn, Descr}],
%%     case systools:make_relup(Name, UpFrom, DownTo, [{path, Path}, silent]) of

%% 	{ok, Relup, _, []}  ->
%% 	    %% {Vsn, [{UpFromVsn, Descr, Instructions}], []} = filter(Relup),
%% 	    {Vsn, [{UpFromVsn, Descr, Instructions}], []} = Relup,
%% 	    NewInstructions =
%% 		Instructions++
%% 		[{apply, {swmServer, activation_complete, []}}],
%% 	    FinalRelup = {Vsn, [{UpFromVsn, Descr, NewInstructions}], []},
%% 	    %% io:format("~p~nWarning = ~p~n",[FinalRelup, Warning]),
%% 	    VsnDir = releases_vsn_dir(NewVsn),
%% 	    FilePath = filename:join(VsnDir, "relup"),
%% 	    {ok, FD} = file:open(FilePath, [write]),
%% 	    io:format(FD, "~p.~n",[FinalRelup]),
%% 	    file:close(FD),
%% 	    io:format("~p created.~n",[FilePath]);
%% 	{ok, _, Module, Warning}  ->
%% 	    io:format("~s~n",[Module:format_warning(Warning)]),
%% 	    erlang:error({systools, Warning});

%% 	{error, Module, Error} ->
%% 	    io:format("~s~n",[Module:format_error(Error)]),
%% 	    {error, Error}
%%     end.

%% Input: [Paths] - Paths to app files
%% Output: [{App, Vsn, Dir}]

%% make_app_dirs([AppPath|AppPaths]) ->
%%     {ok, [{application, App, Data}]} = file:consult(AppPath),
%%     {value, {vsn, Vsn}} = lists:keysearch(vsn, 1, Data),
%%     Dir = triple_dir(AppPath),
%%     [{App, Vsn, Dir}|make_app_dirs(AppPaths)];
%% make_app_dirs([]) -> [].

%%% ----------------------------------------------------------
%%% #           releases_dir()
%%% Input:
%%% Output: Path:string()
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
releases_dir() ->
    filename:join([home_dir(), "releases"]).

releases_dir_other() ->
    filename:join([home_dir_other(), "releases"]).

releases_vsn_dir(Version) ->
    filename:join([home_dir(), "releases", Version]).

%% releases_vsn_dir_other(Version) ->
%%     filename:join([releases_dir_other(), Version]).

%%% ----------------------------------------------------------
%%% #           home_dir()
%%% Input:
%%% Output: Path:string()
%%% Exceptions:
%%% Description: Path to the user's home directory
%%% ----------------------------------------------------------
home_dir() ->
    get(home_dir).

home_dir_other() ->
    case get(home_dir_other) of
	undefined ->
	    Other = home_dir_other(rcs_mode()),
	    put(home_dir_other, Other),
	    Other;
	OtherDir ->
	    OtherDir
    end.

home_dir_other(simulated) ->
    HomeDir = home_dir(),
    RcsRoot = rcs_root(),
    Pattern = RcsRoot++"/home.*/",
    {match, [{Start, Length}]} = re:run(HomeDir, Pattern),
    Stem = string:substr(HomeDir, Start+1, Length-1),
    case lists:reverse(Stem) of
	[$2|ShortStem] ->
	    ShortStem = string:substr(HomeDir, Start+1, Length-2),
	    filename:join(ShortStem, os:getenv("USER"));
	_ ->
	    filename:join(Stem++"2", os:getenv("USER"))
    end;
home_dir_other(_) ->
    case home_dir() of
	"/rcs/swm/home1/sirpa" ->
	    "/rcs/swm/home2/sirpa";
	"/rcs/swm/home2/sirpa" ->
	    "/rcs/swm/home1/sirpa"
    end.

rcs_root() ->
    case os:getenv("RCS_ROOT") of
	false ->
	    "";
	Root ->
	    Root
    end.

%%% ----------------------------------------------------------
%%% #           rcs_dir()
%%% Input:
%%% Output: Path:string()
%%% Exceptions:
%%% Description: Path to the rcs dir which is used for files that are supposed
%%%              to survive an upgrade
%%% ----------------------------------------------------------
rcs_dir() ->
    filename:join(os:getenv("RCS_ROOT"), "rcs").

%%% ----------------------------------------------------------
%%% #           vnf_dir()
%%% Input:
%%% Output: Path:string()
%%% Exceptions:
%%% Description: Path to the vnf dir if cloud, otherwise same as rcs_dir()
%%%
%%% ----------------------------------------------------------
vnf_dir() ->
    case is_vrcs() of
	true ->
	    filename:join(os:getenv("RCS_ROOT"), "vnf");
	_ ->
	    rcs_dir()
    end.

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------
find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
        lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------
find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

%%% ----------------------------------------------------------
%%% #           rcs_mode()
%%% Input:
%%% Output: target | simulated
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
rcs_mode() ->
    case os:getenv("RCS_MODE") of
	"target" -> target;
	"simulated" -> simulated;
	"sim32" -> sim32;
	_ ->
	    info_msg("RCS MODE not set. Target assumed~n"),
	    target
    end.

sim32() ->
    rcs_mode()  == sim32.


arm_dir() ->
    case os:getenv("ARMDIR") of
	false ->
	    "tgt_arm";
	Dir ->
	    "tgt_"++Dir
    end.

info_msg(Format) ->
    info_msg(Format, []).

info_msg(Format, Args) ->
    case get(log_file) of
	undefined ->
	    error_logger:info_msg("~w: "++Format, [?MODULE|Args]);
	LogFile ->
	    TS = timestamp(os:timestamp()),
	    Str = lists:flatten(
		    io_lib:format("~s ~w: "++Format, [TS,?MODULE|Args])),
	    file:write_file(LogFile, Str, [append])
    end.

timestamp(Now) ->
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [YY, MM, DD, Hour, Min, Sec]).
