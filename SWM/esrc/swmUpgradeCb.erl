%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmUpgradeCb.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R8A/R9A/R11A/1
%%%
%%% @doc ==Upgrade callback functions for software management==
%%% This module contains trigger functions for upgrade triggers

-module(swmUpgradeCb).
-vsn('/main/R3A/R4A/R5A/R8A/R9A/R11A/1').
-date('2017-09-05').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R3A/1      2015-02-06 etxjotj     Created
%%% R5A/1      2016-02-02 etxjotj     New cup call 
%%% R8A/1      2017-01-09 etxjotj     Log and check for failed cup commands
%%% R8A/2      2017-01-10 etxpejn     No check for confirm on sim
%%% R8A/3      2017-01-11 etxpejn     No CUP call on sim
%%% R9A/1      2017-02-21 etxjotj     Add sudo for all cup commands
%%% R11A/1     2017-09-05 etxjotj     Use rcs_mode_2
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([verify_precondition/0,
	 verify_upgrade/0,
	 activate_start/0,
	 preload/0,
	 activate/0,
	 confirm/0]).

-export([make_releases/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Make necessary test for verify precondition
%%% 1. Disk space check
%%% @end
%%% ----------------------------------------------------------

verify_precondition() ->

    case sysEnv:rcs_mode_2() of
	vrcs -> 
	    ok;
	_ ->
	    swmOs:cmdreslog("sudo cup --upgrade verifyPreconditions"),
	    Memory = lists:sum([mnesia:table_info(T, memory)||
				   T<-mnesia:system_info(tables)]),
	    NeededSize = Memory+100000, %% Guestimate for other backup content
	    FreeDisk = swmLib:get_free_disk(),
	    info_msg("Database size: ~w~n",[NeededSize]),
	    info_msg("Free disk:  ~w bytes~n",[FreeDisk]),

	    case FreeDisk of
		FreeDisk when FreeDisk > NeededSize ->
		    ok;
		FreeDisk ->
		    {ok, "There is disk space shortage. Remove backups or "
		     "old upgrade packages to make room. "++
			 integer_to_list(NeededSize div 1024)++" k needed. "++
			 integer_to_list(FreeDisk div 1024)++" k available"}
	    end
    end.

verify_upgrade() ->
    case sysEnv:rcs_mode_2() of
   	target ->
	    {0,_} = swmOs:cmdreslog("sudo cup --upgrade verifyUpgrade");
	_ ->
	    do_nada
    end,
    ok.

activate_start() ->
    case sysEnv:rcs_mode_2() of
   	target ->
	    {0,_} = swmOs:cmdreslog("sudo cup --upgrade activateStart");
	_ ->
	    do_nada
    end,
    ok.

preload() ->
    case sysEnv:rcs_mode_2() of
   	target ->
	    {0,_} = swmOs:cmdreslog("sudo cup --upgrade preload");
	_ ->
	    do_nada
    end,   
    ok.

activate() ->
    case sysEnv:rcs_mode_2() of
   	target ->
	    {0,_} = swmOs:cmdreslog("sudo cup --upgrade activate");
	_ ->
	    do_nada
    end,   
    ok.
	
confirm() ->
    case sysEnv:rcs_mode_2() of
   	target ->
	    {0,_} = swmOs:cmdreslog("sudo cup --upgrade confirm");
	_ ->
	    do_nada
    end,
    ok.

%%% ----------------------------------------------------------
%%% @doc Prepare the "releases" directory on the new home partition
%%% @end
%%% ----------------------------------------------------------


make_releases(OtherHomeDir) ->
    try do_make_releases(OtherHomeDir) 
    catch T:E ->
	    %% Failure to produce files is not considered an error,
	    %% since the normal install procedure will kick in later.
	    error_logger:warning_report(
	      [{mfa, {swmUpgradeCb, do_make_releases, [OtherHomeDir]}},
	       {T,E},
	       erlang:get_stacktrace()]),
	    info_msg("Upgrade halted!~n"),
	    receive
		ok -> ok
	    end
    end.

do_make_releases(OtherHomeDir) ->

    SwVersionDir = swmLib:software_dir(OtherHomeDir),

    %% Locate the make_release.escript
    UgPatches = filename:join(swmLib:swm_dir(), "ug_patches"),
    MrDevPattern = filename:join(UgPatches, "make_release.escript"),
    filelib:ensure_dir(MrDevPattern),
    [MakeRelease] =
	case filelib:wildcard(MrDevPattern) of
	    [] ->
		MrPattern = filename:join(
			      [SwVersionDir, "*CXP*", "*CXC*",
			       "sys-*", "priv", "bin",
			       "make_release.escript"]),
		filelib:wildcard(MrPattern);
	    Dev ->
		Dev
	end,

    %% Find new OTP

    OtpPattern = filename:join([SwVersionDir, "*CXP*", "*CXC*","otp-*",
				"priv", "*"]),

    [OtpDir] = filelib:wildcard(OtpPattern),

    UgLogPath = filename:join([sysEnv:rcs_dir(), "erlang", "upgrade.log"]),

    %% Setup correct OTP
    Cmd = "export OTP_ROOT="++OtpDir++"\n"++
	"export PATH=${OTP_ROOT}/bin:${PATH}\n"++
	MakeRelease++" "++SwVersionDir++" "++OtpDir++" "++OtherHomeDir++
	" upgrade "++UgLogPath++" >> "++UgLogPath++"\n",
    ExePath = filename:join(OtherHomeDir, "run_make_rel"),
    ok = file:write_file(ExePath, list_to_binary(Cmd)),
    file:delete(UgLogPath),
    cmd("chmod u+x "++ExePath++" && "++ExePath),


    %% Check if the make_release.escript program finished. There
    %% should be a data file
    UgCompletePath = filename:join(OtherHomeDir, "make_release_complete"),
    case filelib:is_file(UgCompletePath) of
	true -> 
	    os:cmd(["cd ",OtherHomeDir," ; tar cfz /rcs/swm/test.tgz ",OtherHomeDir]),
	    info_msg("Make release is complete~n"),
	    file:delete(UgCompletePath),
	    ok;
	false ->
	    Res2 = os:cmd(["cat ",UgLogPath]),
	    error_msg("Make release failed~n~s~n",[Res2]),
	    erlang:error(make_release_did_not_finish,[OtherHomeDir])
    end.
	

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           cmd(Cmd)
%%% Input: Cmd::string() - A unix shell command
%%% Output: string()
%%% Exceptions:
%%% Description: os:cmd wrapper with printout
%%% ----------------------------------------------------------
cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), Res = os:cmd(Cmd)]),
    Res.

%% This clause is currently not used.
info_msg(Format) ->
   info_msg(Format, []).
info_msg(Format, Args) ->
   error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

%% These clauses are currently not used.
%% warning_msg(Format) ->
%%    warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%% These clauses are currently not used.
%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    error_logger:error_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
