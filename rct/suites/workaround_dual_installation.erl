%% Author: eraahhl
%% Created: Dec 22, 2015
%% @copyright Ericsson AB 2015
%% Description: WA script needed after dual installation an A10 nodes.

-module(workaround_dual_installation).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]). % edoc is not generated when -compile

-export([init_per_suite/1,end_per_suite/1,init_per_testcase/2,end_per_testcase/2]).

-define(KNOWN_ERRORS,sysloglist).

-define(DEFAULT_TIMEOUT, 60000).


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_power, rct_consserv, rct_rs232
%% @end
%%--------------------------------------------------------------------
suite() ->
IGNORELIST=string:tokens(ct:get_config({restart_SUITE,?KNOWN_ERRORS}),","),
    [{timetrap, {hours, 1}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_power,pow},
		 {rct_scp,scpn1},
		 {rct_consserv,cs},
		 {rct_rs232,[node1,node2]},
                 {rct_logging, {[syslog],[{syslog,{["ERROR", "error", "Error","HWLOG"],IGNORELIST}}], [get_all] }},
                 {rct_cli, {cli,[manual_connect]}},
		 {rct_core, []},
		 {cth_conn_log,[]}
		]}].

%  ct_hooks, see...
%  /vobs/rcs/test/RCT_CRX901275/test/lib/labb/esrc/...
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    compile:file("/vobs/rcs/test/RCT_CRX901275/test/suites/SWM/post_ug_lib.erl"),
    Config.

%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------

init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------

end_per_testcase(_TestCase, Config) ->
    ok.

%%--------------------------------------------------------------------

groups() ->
    [
    {dual,[],[workaround]}
    ].


%%--------------------------------------------------------------------

do_workaround() -> 
	ct:pal("Workaround needed and started..."),
	ct:pal("Reboot -f Regular node and wait 200 seconds"),
	ct_telnet:send(node2, "reboot -f"),
	timer:sleep(200000),
	ct:pal("Regular Node: Restart nltn_server:run_iell()..."),
	ct_telnet:send(node2, "\n"),
    	ct_telnet:send(node2, "to_erl"),
    	ct_telnet:send(node2, "."),
    	ct_telnet:send(node2, "nltn_server:run_iell()."),
	ct:pal("Master Node: Reboot -f... and wait 500 seconds"),
	ct_telnet:send(node1, "reboot -f"),
	timer:sleep(500000).


%%--------------------------------------------------------------------

check_prompt(Prompt) -> 
	ct_telnet:send(node2,"\n"),
    	case ct_telnet:expect(node2, Prompt,[{timeout,5000},no_prompt_check]) of
        	{ok,Message} -> 
		ct:pal("System loaded and configured - PASSED"),
			ok;
        	{_,_} 	->  
			do_workaround(), 
			failed
    	end.


%%--------------------------------------------------------------------

workaround(_) ->
 	check_prompt("login*"),
 	ok = check_prompt("login*"),
	ct_telnet:cmd(node1,"ls -al", ?DEFAULT_TIMEOUT),
	ct_telnet:cmd(node1,"cd /home/sirpa", ?DEFAULT_TIMEOUT),
	ct_telnet:cmd(node1,"chmod 777 *", ?DEFAULT_TIMEOUT),
	ct_telnet:cmd(node1,"ls -al", ?DEFAULT_TIMEOUT).
       	


