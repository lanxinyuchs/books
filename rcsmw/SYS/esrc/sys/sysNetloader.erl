%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysNetloader.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R11A/1

%%% @doc ==Network loader==
%%% This module handles some misc functions for NL. 

-module(sysNetloader).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R11A/1').
-date('2017-09-05').
-author('etxjotj').
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
%%% R1A/1      2012-06-18 etxlg       Created
%%% R1A/2      2012-07-12 etxlg       Utils for NL interaction
%%% R1A/3      2012-09-17 etxarnu     Increased INIT_TIME to 3 sec
%%% R1A/4      2012-09-19 etxlg       Retry load of config
%%% R2A/1      2013-03-18 etxlg       Hardcode fw_env.config for now
%%% R2A/2      2013-03-18 etxjotj     Fixes for netconf
%%% R2A/3      2013-03-19 etxjotj     Loading of initial conf
%%% R2A/5      2013-03-21 etxlg       get_active_instance runs w/o server
%%% R2A/6      2013-03-21 etxjotj     Bugfix in enable_env_support
%%% R2A/7      2013-04-16 etxjotj     Execute enable_env_support at init
%%% R2A/8      2013-04-16 etxjotj     Previous fix should only run on target
%%% R2A/9      2013-04-17 etxjotj     Fixed problem with 2nd reboot on target
%%% R2A/10     2013-05-14 etxarnu     Added arm support
%%% R2A/11     2013-05-27 etxlg       Limited use of sudo
%%% R2A/16     2013-10-02 etxlg       No env support on the TCU/ARM
%%% R2A/17     2013-10-09 etxtory     Added COLI: factory_reset
%%% R2A/18     2013-10-23 etxarnu     Added COLI: coli_reinstall
%%% R2A/19     2014-01-17 etxlg       Prepare for sysSsh ->omc_ssh
%%% R2A/20     2014-01-19 etxlg       omc_ssh_cli -> omc_server
%%% R2A/21     2014-01-22 etxlg       completed above
%%% R2A/23     2014-02-24 erarafo     info messages made more informative
%%% R2A/24     2014-03-12 etxjotj     Use swmOs instead of swmServer
%%% R2A/25     2014-04-29 etxjotj     Redirected get_active_instance for tcu03
%%% R2A/26     2014-05-13 etxderb     factory reset for arm
%%% R2A/27     2014-05-14 etxderb     coli_reinstall relayed to aicServer
%%%                                   read_initial_config relayed to aicServer
%%% R2A/28     2014-08-13 etxtory     Removed activate/0, read_initial_config/0
%%%				      Removed AI code (moved to aicServer)
%%% R2A/29     2014-09-03 etxberb     Changed 'init:reboot()' to
%%%                                   'appmI:restart_node'.
%%% ----------------------------------------------------------
%%% R3A/1      2015-03-02 etxtory     Call AIC to clean logs for factoru reset
%%% R3A/2      2015-03-23 etxderb     Remove boot links at factory reset
%%% R3A/3      2015-04-15 etxderb     Incresed timout for revert_to_netloader
%%% R3A/4      2015-05-08 etxderb     revert_to_netloader/1 and use of Arg
%%% ----------------------------------------------------------
%%% R4A/1      2015-09-14 etxderb     revert_to_netloader incresed TO (10min)
%%% ----------------------------------------------------------
%%% R5A/1      2015-11-17 etxpejn     Changed to restart_own_piu in coli_factory_reset
%%%                                   due to dual DU
%%% ----------------------------------------------------------
%%% R11A/1     2017-09-05 etxjotj     Removed obsolete get_active_instance
%%%                                   Current implementation is in SWM
%%% ----------------------------------------------------------

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).
-export([revert_to_netloader/1,
         revert_to_netloader/0,
	 revert_to_netloader_test/0]).

%% Exported for COLI
-export([coli_factory_reset/1]).
-export([coli_reinstall/1]).

-include_lib("xmerl/include/xmerl.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(st,
	{
	  env_support_ok = false
	}).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the sysNetloader server
%%% @end
%%% ----------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
revert_to_netloader(Grade) when Grade == soft; Grade == hard ->
    gen_server:call(?MODULE, {revert_to_netloader, {nl, Grade}}, 600000).
    
revert_to_netloader() ->
    revert_to_netloader(soft).
revert_to_netloader_test() ->
    gen_server:call(?MODULE, {revert_to_netloader, {test, dummy}}).


%%% ----------------------------------------------------------
%%% @doc COLI-cmd interface: reset the DU to factory default.
%%  That is, revert it back to NL (Network Loader).
%%% @end
%%% ----------------------------------------------------------
coli_factory_reset(Arg) ->
    coli_factory_reset(sysEnv:target(), Arg).

if_then_else(true, A,  _) -> A;
if_then_else(_,    _,  B) -> B.

coli_factory_reset(_Target = true, Arg) ->
    Grade = if_then_else(Arg == hard, hard, soft),
    case catch revert_to_netloader(Grade) of
	ok ->
	    %% Revert went fine, call AIC to clean up and then reboot
	    catch aicI:clean_up(Grade), %% Catch due to aicI impl later
	    io:format("Factory reset successful, rebooting ~n", []),
	    appmI:restart_own_piu(cold, false, term_to_string(Arg)),
	    exit(ok);
	{error, Msg} ->
	    %% Revert failed or not allowed
	    error_msg("~p~n", [Msg]),
	    io:format("~p~n", [Msg]),
	    exit(error);
	{'EXIT', Reason} ->
	    Msg = "Command could not be executed, internal error",
	    error_msg("~p~nReason ~p~n", [Msg, Reason]),
	    io:format("~p~n", [Msg]),
	    exit(error)
    end;
coli_factory_reset(_Target = false, _Arg) ->
    Msg = "Command can only be executed in a target environment",
    io:format("~p~n", [Msg]),
    exit(error).

%%% ----------------------------------------------------------
%%% @doc COLI-cmd interface: reinstall the configuration.
%%  That is, revert it back to as after install
%%% @end
%%% ----------------------------------------------------------
coli_reinstall(Arg) ->
    aicServer:coli_reinstall(term_to_string(Arg)).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc init - gen_server interface
%%% @end
%%% ----------------------------------------------------------
init(_Args) ->
    %% Force setup
    %% this is available only on the dus/duw boards,
    %% cheat by matching on the powerpc architecture
    case sysEnv:architecture() of
	{"powerpc", _} ->
	    proc_lib:spawn(fun() -> enable_env_support(false) end);
	_ ->
	    ok
    end,
    {ok, #st{}}.

%%% ----------------------------------------------------------
%%% @doc handle_call - gen_server interface
%%% @end
%%% ----------------------------------------------------------
handle_call(get_active_instance, _From, State) ->
    {Reply, New_active} = handle_get_active_instance(State#st.env_support_ok),
    {reply, Reply, State#st{env_support_ok = New_active}};

handle_call({revert_to_netloader, {Type, Grade}}, 
            _From, 
            #st{env_support_ok= Es_ok} = State) ->

    case sysEnv:architecture() of
	{"powerpc", _} ->
	    enable_env_support(Es_ok),
	    do_revert_to_netloader(Type),
	    {reply, ok, State#st{env_support_ok = true}};
	{"arm",_} ->
            ok = sysRhai:setbootptr(nl),
            catch swmI:reset_boot(Grade), %% Remove the configured/fallback/upgrade
                                    %% links
            info_msg("Reboot will activate the network-loader.~n"),
	    {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%% ----------------------------------------------------------
%%% @doc handle_cast - gen_server interface
%%% @end
%%% ----------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%% ----------------------------------------------------------
%%% @doc handle_info - gen_server interface
%%% @end
%%% ----------------------------------------------------------
handle_info(_Msg, State) ->
    {noreply, State}.

%%% ----------------------------------------------------------
%%% @doc terminate - gen_server interface
%%% @end
%%% ----------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc code_change - gen_server interface
%%% @end
%%% ----------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Function(s) for get active instance
%%% ----------------------------------------------------------
handle_get_active_instance(Active) ->
    case sysEnv:architecture() of
	{"powerpc", _} ->
	    enable_env_support(Active),
	    % Env = os:cmd("sudo /var/tmp/fw_printenv rcs_boot_mode"),
	    % fw_env.sh in /addons/bin is enabled in sudoers
	    Env = os:cmd("cd /tmp; sudo fw_env.sh -i -r rcs_boot_mode"),
	    %% We don't have to look for Usage: ... any more, and
	    %% we want to capture any unexpected output from fw_env.sh
	    Instance =
		case Env of
		    "rcs_boot_mode=b"++NumberStr -> NumberStr--"\n";
		    _ ->
			erlang:error({unexpected_output, Env}, [active])
		end,
	    {list_to_integer(Instance), true};
	_ ->
    	    info_msg("'Get active instance' is currently only supported on powerpc based targets.~n"),
	    {not_supported, Active}
    end.

%%% ----------------------------------------------------------
%%% Function(s) for enable environment support
%%% ----------------------------------------------------------
enable_env_support(true) -> ok;
enable_env_support(false) ->
    enable_env_support(file:read_link("/var/tmp/fw_setenv"));
enable_env_support({ok, Path}) ->
    case lists:prefix(code:priv_dir(sys), Path) of
	true ->
	    ok; %match succesful read_link_info;
	false ->
	    file:delete("/var/tmp/fw_env.config"),
	    file:delete("/var/tmp/fw_printenv"),
	    file:delete("/var/tmp/fw_setenv"),
	    enable_env_support({error, enoent})
    end;
enable_env_support({error, enoent}) -> %match unsuccesful read_link_info
    Mtd = read_proc_file("/proc/mtd"),
    Lines = string:tokens(Mtd, [$\n]),
    {Dev1, _Size1, Sector_size1} = find_name_in_mtd("\"env-0\"", Lines),
    {Dev2, _Size2, Sector_size2} = find_name_in_mtd("\"env-1\"", Lines),
    ok = file:write_file("/var/tmp/fw_env.config",
	["#Config for U-boot utilities, generated from sysNetloader\n"
	"#Note that the size parameter is hardcoded equal to sector_size\n"
	"#device offset size sector_size\n",
	Dev1, " 0x0 ", Sector_size1, " ", Sector_size1, "\n",
	Dev2, " 0x0 ", Sector_size2, " ", Sector_size2, "\n"]),
    % should have this symlink in the OS already
    % link is now added with the -i switch to fw_env.sh
    % os:cmd("sudo ln -s /var/tmp/fw_env.config /etc/fw_env.config"),
    BinDir = sysEnv:target_bin_dir(),
    Cmd = filename:join([code:priv_dir(sys),BinDir, "fw_printenv_ful_bin"]),
    ok = file:make_symlink(Cmd, "/var/tmp/fw_printenv"),
    ok = file:make_symlink(Cmd, "/var/tmp/fw_setenv").

read_proc_file(File) ->
    {ok, Io} = file:open(File, [read, raw]),
    {ok, Content} = file:read(Io, 1024), %file(mtd) is 582bytes at the moment
    file:close(Io),
    Content.

find_name_in_mtd(_, []) ->
    not_found;
find_name_in_mtd(Name, [H|T]) ->
    [Dev, Size, Sector_size, Mtd_name] = string:tokens(H, [$\s]),
    if
	Mtd_name =:= Name ->
		{"/dev/" ++ Dev -- ":", "0x" ++ Size, "0x" ++ Sector_size};
	true -> find_name_in_mtd(Name, T)
    end.

%%% ----------------------------------------------------------
%%% Function(s) for revert to netloader
%%% Type: nl | test
%%% ----------------------------------------------------------
%Type is either nl|test
do_revert_to_netloader(Type) ->
    case Type of
	nl ->
	    os:cmd("cd /tmp; sudo fw_env.sh -i -w rcs_boot_mode -v nl"),
	    info_msg("Reboot will activate the network-loader.~n");
	test ->
	    os:cmd("cd /tmp; sudo fw_env.sh -i -w rcs_boot_mode -v test"),
	    info_msg("Reboot will activate the network-loader environment and launch shell on /dev/console~n")
    end.

%%% ----------------------------------------------------------
%%% Info and error message 
%%% ----------------------------------------------------------
info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% #---------------------------------------------------------
term_to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
term_to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
term_to_string(Value) when is_list(Value) ->
    try io_lib:format("~s", [Value]),
	Value
    catch
	_ : _ ->
	    "unknown"
    end;
term_to_string(_) ->
    "unknown".
