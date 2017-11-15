%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @copyright Ericsson AB 2016
%%% @doc
%%% Runs lmbench tests.
%%%
%%% Execution environment test run my EE Jenkins and MW Jenkins.
%%% @end
%%% ----------------------------------------------------------
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
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-module(lmbench_SUITE).
-compile([export_all]). % edoc is not generated when -compile
-include_lib("common_test/include/ct.hrl").
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
         groups/0,
	 all/0]).

-define(LMBENCH_PATH_TARGET, "/opt/benchmark/os/wr-lmbench/").
-define(KNOWN_ERRORS,sysloglist).
%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_core, rct_logging
%% @end
%%--------------------------------------------------------------------
suite() -> 
    IGNORELIST=string:tokens(ct:get_config({?MODULE,?KNOWN_ERRORS}),","),
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_scp,scpn1},
		 {rct_consserv,cs},
		 {rct_rs232,rs232},
		 {rct_core, []},
		 {rct_logging, {[erlang,syslog], 
				[{syslog,{["ERROR", "error", "Error","HWLOG:"],
					  IGNORELIST}}],
                                [get_all] }},
		 {cth_conn_log,[]}
                 ]}].

init_per_suite(Config) ->
    Config.
end_per_suite(_Config) ->
    ok.
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, _Config) ->
    console_cleanup,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs LMBench testcases.
%% @end
%%--------------------------------------------------------------------


all() -> 
        [{group, tcu04}, {group, dus52}].

groups() ->
           [{tcu04,[],[prepare_lmbench, run_lmbench, fetch_results]},
           {dus52,[],[prepare_lmbench, run_lmbench, fetch_results]}].

%%--------------------------------------------------------------------
%% @doc
%% Send CTRL-C to kill any process left by earlier failing testcase
%% @end
%%--------------------------------------------------------------------

console_cleanup() ->
    timer:sleep(500),
    {ok,_} = ct_telnet:cmd(rs232,"\^c\^c\n"),
    ct_telnet:cmd(rs232,"\n").

%%--------------------------------------------------------------------
%% @doc
%% Wait for du1 login prompt arrives in the consol printouts
%% @end
%%--------------------------------------------------------------------

wait_for_login_prompt() ->
    case ct_telnet:expect(rs232, ".*login", [{timeout,120000}, no_prompt_check]) of
	{ok, _} -> 
	   ok;
	_ ->
	    ct:comment("Login prompt distorted initially"),
            ct_telnet:send(rs232,"\n"),
            case ct_telnet:expect(rs232, ".*login", [{timeout,500}, no_prompt_check]) of
	    {ok, _} ->
	           ok;
	     _ ->
	    ct:fail("No login prompt within max timeout after restart.")
         end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Runs command in the host environment
%% @end
%%--------------------------------------------------------------------
execute_os_cmd(Cmd) ->
	ct:pal("Executing : ~p",[Cmd]),
	Result = os:cmd(Cmd),
        io:format(Result).

%%--------------------------------------------------------------------
%% @doc
%% Make boot partition read/writeable (by first installing overlay FS) 
%% @end
%%--------------------------------------------------------------------

make_fs_rw() ->
    ok = rct_rs232:login(rs232),
    ct_telnet:send(rs232, "install_overlayfs"),
    case ct_telnet:expect(rs232,["patch enabled", "root@.*"],[sequence, {timeout, 60000}]) of
        {ok,_} ->
            ok;
        {error,_} ->
            ct:fail("install_overlayfs failed")
    end,
    ct_telnet:send(rs232, "cup --reboot"),
    ct_telnet:expect(rs232,"Restarting system",[{timeout,80000},no_prompt_check]),
    wait_for_login_prompt(),
    ok = rct_rs232:login(rs232),
    ct_telnet:send(rs232, "mount mount -o remount,rw /opt/rcs_ee/mounts/boot"),
    case ct_telnet:expect(rs232, "root@.*",[{timeout,10000},no_prompt_check]) of
        {ok,_} -> 
            ok;
        {_,_} ->
            ct:fail("Failed to read/write mount boot partition")
    end.

%%--------------------------------------------------------------------
%% @doc
%% Copy LMBench tests to target (and untar it).
%% @end
%%--------------------------------------------------------------------

copy_lmbench() ->
      ct:pal("Copying LMBench tests to target"),
      rct_scp:to_target(scpn1, ""++os:getenv("PWD")++"/testbin/test-arm.tgz", "/", 20000),
      ok = rct_rs232:login(rs232),     
      {ok,_} = ct_telnet:cmd(rs232, "cd /; tar -xf test-arm.tgz; rm test-arm.tgz", 40000).

%%--------------------------------------------------------------------
%% @doc
%% Copy modified pgh_start script from repo and reboot target. 
%% @end
%%--------------------------------------------------------------------

copy_pghstart_reboot() ->
    ct:pal("Copying modified pgh_start.xml to target"),
    rct_scp:to_target(scpn1, ""++os:getenv("PWD")++"/scripts/lmbench_pgh-cfg.xml", "/etc/default/pghd/pgh-cfg.xml", 10000),
    ok = rct_rs232:login(rs232),
    ct_telnet:send(rs232, "cup --reboot"),
    ct_telnet:expect(rs232,"Restarting system",[{timeout,80000},no_prompt_check]),
    wait_for_login_prompt(),
    ok = rct_rs232:login(rs232).

%%--------------------------------------------------------------------
%% @doc
%% Loop whilst running tests ends when 'TEST END 123456' is seen
%% @end
%%--------------------------------------------------------------------

wait_loop() -> 
    ct:timetrap({minutes,45}),		
    case ct_telnet:expect(rs232, [{test_end, "Task is over"}],[sequence,{halt,[{testend,"TEST END 123456"}]},{timeout,2700000}]) of		
        {ok,_} ->		
            wait_loop();		
        {error,{testend,_}} ->		
            ct_telnet:cmd(rs232, "logger LM bench tests finished"),
            ok;		
        _ ->
            ct:fail("LMBench tests took too long (>45 minutes)")
    end.


%%--------------------------------------------------------------------
%% @doc
%% Perform all steps required in preparation for running tests
%% @end
%%--------------------------------------------------------------------

prepare_lmbench(_) ->
     make_fs_rw(),
     copy_lmbench(),
     copy_pghstart_reboot().

%%--------------------------------------------------------------------
%% @doc
%% Run LMBench tests
%% @end
%%--------------------------------------------------------------------

run_lmbench(_) ->
    ct:pal("Start running LMBench tests. Takes about 30 minutes"),
    ok = rct_rs232:login(rs232),
    {ok,_} = ct_telnet:cmd(rs232, "cd "++?LMBENCH_PATH_TARGET),
    ct_telnet:send(rs232, "ls -l"),
    case ct_telnet:expect(rs232, "wr-lmbench-test.sh",[{timeout,5000},no_prompt_check]) of
        {ok,_} -> 
            ok;
        {_,_} ->
            ct:fail("Failed to find wr-lmbench-test.sh")
    end,
    {ok,_} = ct_telnet:cmd(rs232, "logger Starting LM bench tests"),
    {ok,_} = ct_telnet:cmd(rs232, "echo 'TEST END 123456' > endcommand.txt"),
    ct_telnet:send(rs232, "./wr-lmbench-test.sh; cat endcommand.txt"),
    wait_loop().

%%--------------------------------------------------------------------
%% @doc
%% Fetch results (tar and SCP to host)
%% @end
%%--------------------------------------------------------------------

fetch_results(_) ->
    Cwd = os:getenv("PWD"),
    ct:pal("Tar results and fetch to host at: ~s", [Cwd]),
    ok = rct_rs232:login(rs232),
    {ok,_} = ct_telnet:cmd(rs232, "mkdir results"),
    {ok,_} = ct_telnet:cmd(rs232, "./dealt_log.sh logs/*/ results/"),
    {ok,_} = ct_telnet:cmd(rs232, "tar -pczf "++?LMBENCH_PATH_TARGET++"lmbench-results.tar.gz results"),
    rct_scp:from_target(scpn1, ""++?LMBENCH_PATH_TARGET++"lmbench-results.tar.gz", Cwd, 10000),
    file:set_cwd(Cwd),    
%    execute_os_cmd("tar -xvf lmbench-results.tar.gz"),
    execute_os_cmd("ls -l results").


