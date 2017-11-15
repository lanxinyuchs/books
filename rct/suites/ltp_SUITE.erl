%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:ltp_SUITE.erl %
%%% @copyright Ericsson AB 2014
%%% @doc
%%% Runs ltp tests.
%%%
%%% @end

-module(ltp_SUITE).
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
-export([all/0,
	 power/1,	 
	 dus_ltp/1,
	 arm_ltp/1]).
%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_power, rct_consserv, rct_rs232
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_power,pow},
		 {rct_scp,scpn1},
		 {rct_consserv,cs},
		 {rct_rs232,rs232},
		 {rct_core, []},
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
%% Runs power testcases.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [power].

groups() ->
    [{dus41,[],[power,dus_ltp]},
     {tcu03,[],[power,arm_ltp]}].

%%--------------------------------------------------------------------
%% @doc
%% Send CTRL-C to kill any process left by earlier failing testcase
%% @end
%%--------------------------------------------------------------------
console_cleanup() ->
    timer:sleep(500),
    ct_telnet:send(rs232,"\^c\^c\n"),
    ct_telnet:cmd(rs232,"\n").

%%--------------------------------------------------------------------
%% @doc
%% Check boot
%% @end
%%--------------------------------------------------------------------
checkBoot() ->
    {ok,_} = ct_telnet:expect(rs232, [{dus, "3:rd Stage Boot Loader"},
				     {tcu, "Ericsson Version: 3/CXC1736593"}]
			      ,[{timeout,10000},no_prompt_check]),
    {ok,_} = ct_telnet:expect(rs232,["Booting kernel"],[{timeout,90000},no_prompt_check]),
    case ct_telnet:expect(rs232, [{login, ".*login:"}],
                          [{halt,[{watchdog,
                                   "2:nd Stage Boot Loader"},
				  {watchdogtcu, "Ericsson Version: 2/CXC1736593"}]},
                           {timeout,90000}]) of
        {ok,_} ->
	    ok;
        {error,{watchdog,_}} ->
            ct:fail("Watchdog reset");
        {error,{watchdogtcu,_}} ->
            ct:fail("Watchdog reset");
        _ ->
	    ct_telnet:send(rs232,"\^c\^c\n"),
	    ct_telnet:cmd(rs232,"\n"),
            ct:fail("Timeout in reboot")
    end.

setTime() ->
    {Mega, Secs, _} = now(),
    Timestamp = Mega*1000000 + Secs,
    {ok,_} = ct_telnet:cmd(rs232,"date --set=@" ++ integer_to_list(Timestamp)).

%%--------------------------------------------------------------------
%% @doc
%% Power cycle board.
%% @end
%%--------------------------------------------------------------------
power(_) ->
    ok = rct_power:off(pow),
    timer:sleep(10000),
    ok = rct_power:on(pow),
    ok = checkBoot(),
    rct_rs232:login(rs232),
    timer:sleep(500),
    ok = rct_rs232:login(rs232),
    {ok,_} = setTime().

dus_ltp_loop() ->
    ct:timetrap({minutes,45}),
    case ct_telnet:expect(rs232, [{test_end, "analysis=exit"}],
			  [sequence,
			   {halt,[{testend,
				   "TEST END 123456"}]},
			   {timeout,1800000}]) of
        {ok,_} ->
	    dus_ltp_loop();
        {error,{testend,_}} ->
            ok;
	_ ->
	    ct:fail("LTP timeout")
    end.

dus_ltp_posix_loop() ->
    ct:timetrap({minutes,45}),
    case ct_telnet:expect(rs232, [{test_start, "Entering directory"},
				  {test_end, "Leaving directory"}],
			  [sequence,
			   {halt,[{testend,
				   "TEST END 123456"}]},
			   {timeout,1800000}]) of
        {ok,_} ->
	    dus_ltp_posix_loop();
        {error,{testend,_}} ->
            ok;
	_ ->
	    ct:fail("LTP timeout")
    end.
%%--------------------------------------------------------------------
%% @doc
%% ltp tests dus
%% @end
%%--------------------------------------------------------------------
dus_ltp(_) ->
    timer:sleep(5000),
    ok = rct_rs232:login(rs232),
    {ok,_} = setTime(),
    rct_scp:to_target(scpn1, "/proj/rcs-tmp/windriver-rpm/ppc476/make-3.82-r2.ppc476.rpm", "/root/", 20000),
    rct_scp:to_target(scpn1, "/proj/rcs-tmp/windriver-rpm/ppc476/ltp-git-r8.ppc476.rpm", "/root/", 20000),
    rct_scp:to_target(scpn1, "/proj/rcs-tmp/windriver-rpm/ppc476/ltp-testsuite-git-r8.ppc476.rpm", "/root/", 20000),
    {ok,_} = ct_telnet:cmd(rs232,"rm /var/lib/rpm/__db.001"),
    ct_telnet:send(rs232,"rpm -ivh /root/make-3.82-r2.ppc476.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    ct_telnet:send(rs232,"rpm -ivh --nodeps /root/ltp-git-r8.ppc476.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    ct_telnet:send(rs232,"rpm -ivh --nodeps /root/ltp-testsuite-git-r8.ppc476.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    {ok,_} = ct_telnet:cmd(rs232,"echo 'TEST END 123456' > endcommand.txt"),
    ct_telnet:send(rs232,"/opt/ltp/wrLinux_ltp/wr-runltp | tee ltp-tee.log ; cat endcommand.txt"),
    dus_ltp_loop(),
    ct_telnet:send(rs232,"ls"),
    timer:sleep(5000),
    MakePath = os:getenv("PWD"),
    rct_scp:from_target(scpn1, "/opt/ltp/wrLinux_ltp/results/LTP.log", MakePath, 20000).


dus_ltp_posix(_) ->
    timer:sleep(5000),
    ok = rct_rs232:login(rs232),
    {ok,_} = ct_telnet:cmd(rs232,"date -s 'Jan 25 13:42:23 CET 2014'"),
    {ok,_} = ct_telnet:cmd(rs232,"rm /var/lib/rpm/__db.001"),
    ct_telnet:send(rs232,"rpm -ivh /root/make-3.82-r2.ppc476.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    ct_telnet:send(rs232,"rpm -ivh --nodeps /root/ltp-posix-git-r1.ppc476.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    {ok,_} = ct_telnet:cmd(rs232,"cd /opt/open_posix_testsuite"),
    ct_telnet:send(rs232,"make generate-makefiles"),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,900000}]),
    {ok,_} = ct_telnet:cmd(rs232,"cd conformance"),
    {ok,_} = ct_telnet:cmd(rs232,"echo 'TEST END 123456' > endcommand.txt"),
    ct_telnet:send(rs232,"make all test | tee ltp-tee.log ; cat endcommand.txt"),
    dus_ltp_posix_loop(),
    ct_telnet:send(rs232,"ls").

%%--------------------------------------------------------------------
%% @doc
%% ltp tests arm
%% @end
%%--------------------------------------------------------------------
arm_ltp(_) ->
    timer:sleep(5000),
    ok = rct_rs232:login(rs232),
    {ok,_} = setTime(),
    rct_scp:to_target(scpn1, "/proj/rcs-tmp/windriver-rpm/armv7a_vfp_neon/make-3.82-r2.armv7a_vfp_neon.rpm", "/root/", 20000),
    rct_scp:to_target(scpn1, "/proj/rcs-tmp/windriver-rpm/armv7a_vfp_neon/ltp-git-r8.armv7a_vfp_neon.rpm", "/root/", 20000),
    rct_scp:to_target(scpn1, "/proj/rcs-tmp/windriver-rpm/armv7a_vfp_neon/ltp-testsuite-git-r8.armv7a_vfp_neon.rpm", "/root/", 20000),
    {ok,_} = ct_telnet:cmd(rs232,"rm /var/lib/rpm/__db.001"),
    ct_telnet:send(rs232,"rpm -ivh --nodeps /root/make-3.82-r2.armv7a_vfp_neon.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["Preparing..."],[{timeout,90000},no_prompt_check]),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    ct_telnet:send(rs232,"rpm -ivh --nodeps /root/ltp-git-r8.armv7a_vfp_neon.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["Preparing..."],[{timeout,90000},no_prompt_check]),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    ct_telnet:send(rs232,"rpm -ivh --nodeps /root/ltp-testsuite-git-r8.armv7a_vfp_neon.rpm"),
    {ok,_} = ct_telnet:expect(rs232,["Preparing..."],[{timeout,90000},no_prompt_check]),
    {ok,_} = ct_telnet:expect(rs232,["@"],[{timeout,90000}]),
    {ok,_} = ct_telnet:cmd(rs232,"echo 'TEST END 123456' > endcommand.txt"),
    ct_telnet:send(rs232,"/opt/ltp/wrLinux_ltp/wr-runltp | tee ltp-tee.log ; cat endcommand.txt"),
    dus_ltp_loop(),
    ct_telnet:send(rs232,"ls"),
    timer:sleep(5000),
    MakePath = os:getenv("PWD"),
    rct_scp:from_target(scpn1, "/opt/ltp/wrLinux_ltp/results/LTP.log", MakePath, 20000).

