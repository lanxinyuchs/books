%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:testbox_SUITE.erl %
%%% @author Niclas Bengtsson <niklas.x.bengtsson@ericsson.com>
%%% @copyright Ericsson AB 2013
%%% @version /main/R1A/R2A/0, checkout by etxkols in etxkols_rcs
%%% @doc
%%% Runs execution environment tests.
%%%
%%% Execution environment test run my EE Jenkins and MW Jenking.
%%% @end

-module(tlm_SUITE).
-include_lib("common_test/include/ct.hrl").
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
-compile([export_all]). % edoc is not generated when -compile
-export([all/0]).
-define(DEFAULT_TIMEOUT, 60000).
%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_power, rct_consserv, rct_rs232
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
%		 {rct_power,pow},
%		 {rct_consserv,cs},
		 {rct_rs232,{rs232,[{connect_retries, 10}]}},
		{cth_conn_log,[]}]}].

init_per_suite(Config) ->	
    Config.
end_per_suite(_Config) ->	
    ok.
init_per_testcase(_TestCase, Config) ->	
    Config.
end_per_testcase(_TestCase, _Config) ->	
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Runs ase testcases.
%% @end
%%--------------------------------------------------------------------
all() ->
    [dmesg].

groups() ->
    [{tlm,[],[tlmlogin,%rcsstart,
			  board_state,modules_check,rpmcheck,
	      kernel_warnings,kernel_call_trace,dmesg]}].
%syslog_error disabled until the errors are fixed

tlmlogin(_) ->	
    rct_rs232:login(rs232),
    rct_rs232:login(rs232),
%%     ct_telnet:cmd(rs232,"\n\n"),
    rct_rs232:login(rs232),
    rct_rs232:login(rs232),
	    timer:sleep(500),
%%     ct_telnet:cmd(rs232,"\n\n"),
    rct_rs232:login(rs232),
    rct_rs232:login(rs232),
	    timer:sleep(500),
%%     ct_telnet:cmd(rs232,"\n\n"),
    rct_rs232:login(rs232),
%    ct_telnet:cmd(rs232,"\n\n"),
	    timer:sleep(500),
     ok = rct_rs232:login(rs232).

%%--------------------------------------------------------------------
%% @doc
%% Send CTRL-C to kill any process left by earlier failing testcase
%% @end
%%--------------------------------------------------------------------
console_cleanup() ->	
    timer:sleep(500),
    ct_telnet:cmd(rs232,"\^c\^c\n"),
    ct_telnet:cmd(rs232,"\n").

%% not supported currently
rcsstart(_) ->	
    ok = rct_rs232:login(rs232),
    ct_telnet:send(rs232,"/etc/init.d/rcs-start"),
    {ok,_} = ct_telnet:expect(rs232,["root@"],[{timeout,90000}]),
    wait_for_rcs_start_loop(20),
    ct_telnet:cmd(rs232, "dmesg -D"), % Disable kernel printing to console
    ct_telnet:cmd(rs232,"lsmod").


wait_for_rcs_start_loop(0) ->
    ct_telnet:cmd(rs232, "ps -Al"),
    ct_telnet:cmd(rs232, "lsmod"),
    ct:fail("RCS start failed");

wait_for_rcs_start_loop(N) ->    
    case ct_telnet:cmd(rs232, "lsmod | grep 'ncp'") of
        {ok,[ _Prompt]} ->
	 % Sometimes only one element is returned
	    wait_for_rcs_start_loop(N-1);
	{ok,[_Cmd, _Prompt]} -> % If grep does not match, list of 2 elements is returned
	    wait_for_rcs_start_loop(N-1);
	{ok,_Match} ->  % If grep match, list of >2 elements is returned
	    ok;
	{error, _Reason} ->
	    timer:sleep(1000),
	    wait_for_rcs_start_loop(N-1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check modules
%% @end
%%--------------------------------------------------------------------
modules_check(_) ->	
    ok = rct_rs232:login(rs232),
    case ct_telnet:cmd(rs232, "dmesg | grep 'disagrees about version of symbol'") of
        {ok,[ _Prompt]} ->
	 % Sometimes only one element is returned
	    ok;
	{ok,[_Cmd, _Prompt]} -> % If grep does not match, list of 2 elements is returned
	    ok;
	{ok,Match} ->  % If grep match, list of >2 elements is returned
	    ct:fail(Match);
	{error, Reason} ->
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Run dmesg after tests
%% @end
%%--------------------------------------------------------------------
dmesg(_) ->
    timer:sleep(1000),
    ok = rct_rs232:login(rs232),
    ct_telnet:cmd(rs232, "ls /home/sirpa/software"),    
    ct_telnet:cmd(rs232, "ls /home/sirpa/software/*"),
    ct_telnet:cmd(rs232, "find /home/sirpa/software/ | grep githash | xargs --verbose -l1 cat" , ?DEFAULT_TIMEOUT),
    {ok,_} =
    ct_telnet:cmd(rs232, "dmesg" , ?DEFAULT_TIMEOUT),    
    ct_telnet:cmd(rs232, "echo dmesgEND"),    
    ct_telnet:cmd(rs232, "cat /var/log/syslog" , ?DEFAULT_TIMEOUT),
    ct_telnet:cmd(rs232, "echo END").

board_state_loop(0) ->
    ct:fail("Not OPERATIONAL");

board_state_loop(N) ->
    case ct_telnet:cmd(rs232, "cat /sys/rbs-fn/rbs-sys/board_state") of
        {ok,[_Cmd, "OPERATIONAL" ,_Prompt]} ->
	    ok;
	{ok, _} ->  % If grep match, list of >2 elements is returned
	    board_state_loop(N-1);
        {error, _} ->
	    board_state_loop(N-1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Checks that cat "/sys/rbs-fn/rbs-sys/board_state" is OPERATIONAL
%% @end
%%--------------------------------------------------------------------
board_state(_) ->	
%%     console_cleanup(),
    board_state_loop(5).

%%--------------------------------------------------------------------
%% @doc
%% Checks for kernel warnings/errors
%% @end
%%--------------------------------------------------------------------
kernel_warnings(_) ->
    ok = rct_rs232:login(rs232),
    case ct_telnet:cmd(rs232, "grep -A50 'cut here' /var/log/syslog") of
        {ok,[ _Prompt]} ->
	 % Sometimes only one element is returned
	    ok;
        {ok,[_Cmd, _Prompt]} ->
	 % If grep does not match, list of 2 elements is returned
	    ok;
	{ok, Match} ->  % If grep match, list of >2 elements is returned
            ct:fail("Kernel warnings/errors in /var/log/syslog" ++ Match);
        {error, Reason} ->
            ct:fail(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks for Call Trace
%% @end
%%--------------------------------------------------------------------
kernel_call_trace(_) ->
    ok = rct_rs232:login(rs232),
    case ct_telnet:cmd(rs232, "grep 'Call Trace:' /var/log/syslog") of
        {ok,[ _Prompt]} ->
	 % Sometimes only one element is returned
	    ok;
        {ok,[_Cmd, _Prompt]} ->
	 % If grep does not match, list of 2 elements is returned
	    ok;
	{ok, Match} ->  % If grep match, list of >2 elements is returned
	    ct_telnet:cmd(rs232, "grep -A40 -B40 'Call Trace:' /var/log/syslog"),
            ct:fail("Kernel Call Trace in /var/log/syslog" ++ Match);
        {error, Reason} ->
            ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Checks for errors in syslog
%% @end
%%--------------------------------------------------------------------
syslog_error(_) ->
    ok = rct_rs232:login(rs232),
    case ct_telnet:cmd(rs232, "grep -i 'error' /var/log/syslog | grep -v 'pca953x: probe'") of
	{ok,[ _Prompt]} ->
	 % Sometimes only one element is returned
	    ok;
	{ok,[_Cmd, _Prompt]} ->
	 % If grep does not match, list of 2 elements is returned
	    ok;
	{ok, Match} ->  % If grep match, list of >2 elements is returned
            ct:fail("Error in /var/log/syslog" ++ Match);
        {error, Reason} ->
            ct:fail(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks the rpm dependencies
%% @end
%%--------------------------------------------------------------------
rpmcheck(_) ->
    ok = rct_rs232:login(rs232),
    ct_telnet:cmd(rs232,"rm /var/lib/rpm/__db.001"),
    ct_telnet:send(rs232,"rpm -Va"),
    {ok,_} = ct_telnet:expect(rs232,["root@"],[{timeout,90000}]),
    ct_telnet:cmd(rs232,"echo END").
