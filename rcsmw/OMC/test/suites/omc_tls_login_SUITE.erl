%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_tls_login_SUITE.erl %
%%% @author emarnek
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R8A/R12A/1
%%% 
%%% @doc == Test Suite for testing that TLS login works
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end


-module(omc_tls_login_SUITE).
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date        Name        What
%%% -----      ----------  --------    -----------------------
%%% Rx         2015-02-27  etxlg       Checkin for weekend
%%% R3A/2      2015-03-02  etxlg       More TCs
%%% R3A/3      2015-03-03  etxlg       Different use of priv_dir
%%% R3A/4      2015-03-03  etxlg       Ensure netconf session is always closed
%%% R3A/5      2015-03-03  etxlg       Timing issues/workarounds
%%% R3A/6      2015-03-04  etxlg       Workaround for CERT enable CA issue
%%% R3A/7      2015-03-04  etxlg       ensure exit before closing socket
%%% R3A/8      2015-03-04  etxlg       Get rid of error printouts
%%% R3A/9      2015-03-04  etxlg       ?config -> ct:get_config(...)
%%% R3A/10     2015-03-05  etxlg       one more ?config -> ct:get_config(...)
%%% R3A/11     2015-03-18  etxlg       groups()
%%% R3A/12     2015-06-12  etxjotj     Removed extra doc tag that crashed edoc
%%% R3A/13     2015-07-10  etxjovp     Add group definitions used by CS CI
%%% R3A/15     2015-07-15  etxjovp     modify group definitions used by CS CI
%%% R3A/16     2015-07-16  etxjovp     modify group definitions used by CS CI
%%% R4A/1      2015-09-15  etxlg       New model, netconf and cli splitted
%%% R6A/2      2016-08-30  ehsake      New test stress_rcscoli_cli, refactoring
%%% R12A/1     2017-10-25  emarnek     Remove export_all
%%% ----------------------------------------------------------
%%% 

-export([suite/0,
     init_per_suite/1,
     end_per_suite/1,
     init_per_testcase/2,
     end_per_testcase/2,
     all/0,
     groups/0
    ]).

%testcases for development and debug
-export([only_configure/1, interact/1, print_config/1, clean_tls/1]).

%actual testcases
-export([connect_cli/1, connect_netconf/1, connect_rcscoli/1]).
-export([connect_cli_wrong_cert/1, disable_tls_misconnect/1]).
-export([stress_rcscoli_cli/1]).
-export([connect_max_cli_sessions/1]).

%for test of test
-export([activate_ca/0, create_tc/0, check_nc/0, is_tls_login_active/0]).
-export([activate_tls_login/0]).

-export([connect_cli_ignore_errors/2, connect_rcscoli_ignore_errors/2, set_user_label/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_rpc, rpc1},
		 {rct_netconf,{nc1, expert_auth}},
                 {cth_conn_log,[]},
                 {rct_logging, {all,
			[{erlang,{["ERROR REPORT","CRASH REPORT"],
				  ["SSL: certify: ssl_handshake.erl:"]}}]}}
    		]
     }].


%% @hidden
init_per_suite(In_config) ->
    crypto:start(),
    ssh:start(),
    ssl:start(),

    ct:pal("Preparing ct-loopdata (Config)"),
    Config = prepare_config(In_config), %add; things not present in SIM

    ct:pal("Checking/updating cert files in my_priv_dir: ~p",
	   [?config(my_priv_dir, Config)]),
    prepare_files(Config), %copy needed cert-mtrl data_dir -> my_priv_dir
    Config.


%% @hidden
end_per_suite(_Config) ->
	%duh
    ok.

%% @hidden
init_per_testcase(print_config, Config) ->
    Config;
init_per_testcase(clean_tls, Config) ->
    Config;
init_per_testcase(_Tc, Config) ->
    %matching all testcases except those listed above
    %Config is already prepared in init_per_suite/1
    os:getenv("SIM_OR_TARGET") =:= "sim" andalso configure_ldap(Config),
    ct:pal("Checking/Configuring the site for TLS login"),
    configure_for_tls(Config),
    ct:pal("TLS login configured - ready to run testcase"),
    Config.

end_per_testcase(disable_tls_misconnect, Config) ->
    %if this case crashed midpoint we re-enable TLS here
    enable_disable_tls(?config(me_id, Config), enable),
    ok;
end_per_testcase(_Tc, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [connect_cli, connect_netconf, connect_rcscoli,
     connect_cli_wrong_cert, disable_tls_misconnect,
     stress_rcscoli_cli,connect_max_cli_sessions].

groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []},  
     {sbc__upgrade__all__1__group, [], []},  
     {sdc__cover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},  
     {sdc__qual__all__1__group, [], [{group, all_working_1}]},
     {all_working, [], [{group, all_working_1}]},  
     {all_working_1, [sequence], [connect_cli,
				connect_netconf,
				connect_rcscoli,
				disable_tls_misconnect,
				connect_max_cli_sessions]}].
%%--------------------------------------------------------------------
%% @doc
%% Using netconf to just add all config (CERT, LDAP),
%% the testcase is actually empty, all work done in init_per_suite(..).
%% This is not actually a testcase.
%%
%% @spec only_configure(_Config) -> void()
%% @end
%%--------------------------------------------------------------------
only_configure(_Config) ->
    %nothing here, all is done in init_per_suite/1
    ct:pal("Node configured for TLS-login").

%for test
%%--------------------------------------------------------------------
%% @doc
%% Used for testing, just start CT and drop the user in the shell by
%% means of the ct:break(...) function.
%% All config will be added as that is done in init_per_suite(..).
%% This is not actually a testcase.
%% @spec interact(Config) -> void()
%% @end
%%--------------------------------------------------------------------
interact(Config) ->
    ct:pal("Config: ~p", [Config]),
    ct:break("Interactive, resume by ct:continue()."),
    ct:pal("Test case 'interact' complete", []).

print_config(Config) ->
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    ct:pal("Running towards simulated DUT");
	_ ->
	    ct:pal("Running towards real DUT")
    end,
    ct:pal("Config: ~p", [Config]).

%%--------------------------------------------------------------------
%% @doc
%% Test that it is possible to connect to CLI using TLS
%%
%% @spec connect_cli(Config) -> ok
%% @end
%%--------------------------------------------------------------------
connect_cli(Config) ->
    connect_cli_tmo(Config,1000).

connect_cli_ignore_errors(Config,Timeout) ->
   
   try connect_cli_tmo(Config,Timeout) of
    ok -> 
        ok
    catch
    _:_ ->
        ok
    end.
        

connect_tls(Config,Type,Timeout) ->
    
    Host = ?config(dut_ip, Config),
    Port = ?config(Type, Config),
    Certfile = ?config(expert_cert_file, Config),
    Keyfile = ?config(expert_key_file, Config),
    Opts = [{certfile, Certfile}, {keyfile, Keyfile},
        {reuse_sessions, false}, %needed as we have this in server (BUG!)
        {verify, verify_none},
        {mode, binary}],
    flush_ssl(),
    ssl:connect(Host, Port, Opts, Timeout).

close_tls(Sock) ->
    ok = wait_for_ssl_exit(Sock),
    ok = ssl:close(Sock),
    flush_ssl(),
    ok.
   

connect_cli_tmo(Config,Timeout) ->
    ct:pal("Running testcase connect_cli(~p)", [Timeout]),
    {ok, Sock} = connect_tls(Config,cli_tls,Timeout),
    
    Motd_part = <<"case of doubt shall seek advice from his/her manager.">>,
    ok = look_for_ssl(Sock, Motd_part),
    flush_ssl(),
    ok = ssl:send(Sock, <<"show\n">>),
    ok = look_for_ssl(Sock, <<"ManagedElement=">>),
    ok = ssl:send(Sock, <<"exit\n">>),
    close_tls(Sock).

%%--------------------------------------------------------------------
%% @doc
%% Test that it is possible to connect to netconf using TLS
%%
%% @spec connect_netconf(Config) -> ok
%% @end
%%--------------------------------------------------------------------
connect_netconf(Config) ->
    ct:pal("Running testcase connect_netconf()", []),
    {ok, Sock} = connect_tls(Config,netconf_tls,30000),
    send_netconf_hello(Sock),
    {ok, Session_id} = receive_and_check_hello(Sock, <<>>),
    ct:pal("Received netconf hello message with session-id: ~p", [Session_id]),
    send_netconf_close(Sock, Session_id),
    close_tls(Sock).


%%--------------------------------------------------------------------
%% @doc
%% Test that it is possible to connect to RCS-COLI using TLS
%%
%% @spec connect_rcscoli(Config) -> ok
%% @end
%%--------------------------------------------------------------------
connect_rcscoli(Config) ->
    connect_rcscoli_tmo(Config,1000).

connect_rcscoli_ignore_errors(Config,Timeout) ->
   
   try connect_rcscoli_tmo(Config,Timeout) of
    ok -> 
        ok
    catch
    _:_ ->
        ok
    end.

connect_rcscoli_tmo(Config,Timeout) ->
    ct:pal("Running testcase connect_rcscoli()", []),
    {ok, Sock} = connect_tls(Config,coli_tls,Timeout),
    Motd_part = <<"case of doubt shall seek advice from his/her manager.">>,
    ok = look_for_ssl(Sock, Motd_part),
    flush_ssl(),
    ok = ssl:send(Sock, <<"/misc/info\n">>),
    ok = look_for_ssl(Sock, <<"User: expert">>),
    ok = ssl:send(Sock, <<"exit\n">>),
    close_tls(Sock).


%%--------------------------------------------------------------------
%% @doc
%% Test that it is NOT possible to connect to cli using TLS with a cert
%% from a different CA
%%
%% @spec connect_cli_wrong_cert(Config) -> ok
%% @end
%%--------------------------------------------------------------------
connect_cli_wrong_cert(Config) ->
    ct:pal("Running testcase connect_cli() with wrong CERT", []),
    Host = ?config(dut_ip, Config),
    Port = ?config(cli_tls, Config),
    Certfile = ?config(alt_expert_cert_file, Config),
    Keyfile = ?config(alt_expert_key_file, Config),
    Opts = [{certfile, Certfile}, {keyfile, Keyfile},
	    {reuse_sessions, false}, %needed as we have this in server (BUG!)
	    {verify, verify_none},
	    {mode, binary}],
    flush_ssl(),
    case ssl:connect(Host, Port, Opts, 1000) of
	{error, {tls_alert, "unknown ca"}} ->
	    ct:pal("Unable to connect: \"unknown ca\" - GOOD");
	{error, Other} ->
	    ct:pal("Unable to connect: ~p - unexpected but OK", [Other])
    end,
    flush_ssl(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Test that it is NOT possible to connect using TLS when TLS is
%% disabled in sysM.
%%
%% @spec disable_tls_misconnect(Config) -> void()
%% @end
%%--------------------------------------------------------------------
disable_tls_misconnect(Config) ->
    ct:pal("Running testcase disable_tls_misconnect()", []),
    enable_disable_tls(?config(me_id, Config), disable),
    try connect_cli(Config) of
	ok -> 
	    ct:fail("Connect worked but service (cli) should be disabled")
    catch
	_:_ ->
	    ok
    end,
    try connect_netconf(Config) of
	ok -> 
	    ct:fail("Connect worked but service (netconf) should be disabled")
    catch
	_:_ ->
	    ok
    end,
    try connect_rcscoli(Config) of
	ok -> 
	    ct:fail("Connect worked but service (rcs-coli) should be disabled")
    catch
	_:_ ->
	    ok
    end,
    enable_disable_tls(?config(me_id, Config), enable),
    ct:pal("Wait 5 seconds to give the TLS service time to start"),
    timer:sleep(5000),
    ok = connect_cli(Config),
    ok = connect_netconf(Config),
    ok = connect_rcscoli(Config),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Test that it is possible to connect no more (exactly) than 20 sessions
%% using TLS.
%%
%% @spec connect_max_cli_sessions(Config) -> ok
%% @end
%%--------------------------------------------------------------------
connect_max_cli_sessions(Config) ->
    ct:pal("Running testcase connect_max_cli_sessions() -> 20 sessions", []),
    Motd_part = <<"case of doubt shall seek advice from his/her manager.">>,
    flush_ssl(),
    %it may take 5 seconds for each connection when the bucket is empty
    % 5 * 20 = 100 seconds
    Connected_socks =
	[begin
	    {ok, Sock} = connect_tls(Config,cli_tls,1000*200), %200sec
	    ok = look_for_ssl(Sock, Motd_part),
	    ct:pal("Connected session number ~p out of total 20", [N]),
	    flush_ssl(),
	    Sock
	 end || N<- lists:seq(1, 20)],
    ct:pal("All 20 sessions connected - closing"),
    [begin
	ssl:send(Sock, <<"exit\n">>),
	ok = wait_for_ssl_exit(Sock),
        catch ssl:close(Sock)
     end || Sock <- Connected_socks],
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Test that spamming CLI and COLI port simultaneously does not cause
%% worker list to be corrupt (HV18193)
%%
%% @spec stress_rcscoli_cli(Config) -> ok
%% @end
%%--------------------------------------------------------------------

stress_rcscoli_cli(Config) ->
    
    N = 15,
    Timeout = 1000* 200, %200 sec
    
    ct:pal("Spawn cli connections"),
    [spawn(?MODULE,connect_cli_ignore_errors,[Config,Timeout]) || _ <- lists:seq(1,N)],
    
    ct:pal("Spawn coli connections"),
    [spawn(?MODULE,connect_rcscoli_ignore_errors,[Config,Timeout]) || _ <- lists:seq(1,N)],
    
    ct:pal("Wait for connections to terminate"),
    timer:sleep(60000),
    ct:pal("Verify that we can connect again"),
    connect_cli_tmo(Config,12000),%% need to increase timeout due to limit rate.
    
    ok.
    

%%--------------------------------------------------------------------
%% @doc
%% Remove the TLS config (not really a test), useful when developing this suite
%%
%% @spec clean_tls(Config) -> ok
%% @end
%%--------------------------------------------------------------------
%HERE
clean_tls(Config) ->
    Me_id = ?config(me_id, Config),
    Nc_name = ?config(nodeCredentialId, Config),
    Tc_name = ?config(tc_name, Config),
    Subject = ?config(user_ca_subject, Config),
    Edit =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{sysMId,[],["1"]},
             {'NetconfTls', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{netconfTlsId,[],["1"]},
               {administrativeState, [], ["LOCKED"]},
               {nodeCredential, [{'xc:operation', "delete"}], []},
               {trustCategory, [{'xc:operation', "delete"}], []}]}]}]}]},
    ct:pal("Unconfigure TLSnetconf: (SysM)"),
    case netconf(edit_config, [nc1, running, Edit]) of
        ok -> ok;
        Error0 ->
            ct:pal("Result: ~p", [Error0])
    end,
    Edit1 =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{sysMId,[],["1"]},
             {'CliTls', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{cliTlsId,[],["1"]},
               {administrativeState, [], ["LOCKED"]},
               {nodeCredential, [{'xc:operation', "delete"}], []},
               {trustCategory, [{'xc:operation', "delete"}], []}]}]}]}]},
    ct:pal("Unconfigure TLScli: (SysM)"),
    case netconf(edit_config, [nc1, running, Edit1]) of
        ok -> ok;
        Error1 ->
            ct:pal("Result: ~p", [Error1])
    end,
    Edit2 =
	{'ManagedElement',
            [{xmlns,"urn:com:ericsson:ecim:ComTop"},
             {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
            [{managedElementId,[],[Me_id]},
             {'SystemFunctions',
              [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                  [{certMId,[],["1"]},
                   {'TrustCategory', [{'xc:operation', "delete"}],
                    [{trustCategoryId, [], [Tc_name]}]}]}]}]}]},

    ct:pal("Delete TrustCategory: ~p",[Tc_name]),
    case netconf(edit_config, [nc1, running, Edit2]) of
        ok -> ok;
        Error2 ->
            ct:pal("Result: ~p", [Error2])
    end,
    Edit3 =
	{'ManagedElement',
            [{xmlns,"urn:com:ericsson:ecim:ComTop"},
             {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
            [{managedElementId,[],[Me_id]},
             {'SystemFunctions',
              [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                  [{certMId,[],["1"]},
                   {'NodeCredential', [{'xc:operation', "delete"}],
                    [{nodeCredentialId, [], [Nc_name]}]}]}]}]}]},

    ct:pal("Delete NodeCredential: ~p",[Nc_name]),
    case netconf(edit_config, [nc1, running, Edit3]) of
        ok -> ok;
        Error3 ->
            ct:pal("Result: ~p", [Error3])
    end,
    %there may be seveal CAs - just remove the one we find

    case find_ca(Me_id, Subject) of
	{Ca_id, _, _} ->
	    Mo_ref = 
		"ManagedElement="++Me_id++",SystemFunctions=1,SecM=1,CertM=1,"
		"TrustedCertificate="++Ca_id,
	    Action =
	    {'ManagedElement',
	     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	     [{managedElementId,[],[Me_id]},
	      {'SystemFunctions',
	       [{systemFunctionsId,[],["1"]},
	        {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
		 [{secMId,[],["1"]},
		  {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
		   [{certMId,[],["1"]},
		    {removeTrustedCert, [],
		     [{trustedCert, [Mo_ref]}]
		}]}]}]}]},
	    ct:pal("Action delete TrustedCerificate: ~p",[Mo_ref]),
	    case  netconf(action, [nc1, Action]) of
		{ok, _} -> ok;
		Error -> ct:pal("Result: ~p", [Error])
	    end;
	_ ->
	    ok
    end.


enable_disable_tls(Me_id, Dis_or_en) ->
    [enable_disable_tls(Service, Me_id, Dis_or_en) ||
	Service <- [netconf, cli]].

enable_disable_tls(Service, Me_id, Dis_or_en) ->
    Service_atom = service_atom(Service),
    Service_id = service_id(Service),
    Edit =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{sysMId,[],["1"]},
             {Service_atom, [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{Service_id,[],["1"]},
               {administrativeState, [],
		case Dis_or_en of
		    enable -> 
			ct:pal("sysM: ~p: Enable TLS login",[Service]),
 			["UNLOCKED"];
		    disable ->
			ct:pal("sysM: ~p: Disable TLS login",[Service]),
 			["LOCKED"]
		end}]}]}]}]},
    case netconf(edit_config, [nc1, running, Edit]) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

send_netconf_hello(Sock) ->
    Hello =
	<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	  "<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
	  "<capabilities>"
	  "<capability>urn:ietf:params:netconf:base:1.0</capability>"
	  "</capabilities>"
	  "hello> ]]>]]>">>,
    ssl:send(Sock, Hello).

send_netconf_close(Sock, Session_id) ->
    Close =
        <<"<rpc message-id=\"",
           (integer_to_binary(Session_id))/binary,
           "\" xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
          "<close-session></close-session> </rpc>]]>]]>">>,
    ssl:send(Sock, Close).

receive_and_check_hello(Sock, Acc)->
    receive
        {ssl, Sock, Bin} ->
            New_acc = <<Acc/binary, Bin/binary>>,
            case check_hello(New_acc) of
                {ok, _} = Good ->
                    Good;
                nok ->
                    receive_and_check_hello(Sock, New_acc)
            end;
        Other ->
            ct:pal("receive_and_check_hello(...): got other: ~p~n", [Other]),
            nok
after
    10000 ->
        ct:pal("receive_and_check_hello/2, timeout: ~p~n", [Acc]),
        nok
end.

check_hello(Hello) ->
    case binary:split(Hello, <<"]]>]]>">>) of
        [H,_] ->
            xml_check_hello(xmerl_scan:string(binary_to_list(H)));
        _ ->
            nok
    end.

xml_check_hello({#xmlElement{name = hello} = E, _}) ->
    xml_check_hello(E#xmlElement.content);
xml_check_hello([#xmlElement{name = 'session-id', content = [C]} | _]) ->
    {ok, list_to_integer(C#xmlText.value)};
xml_check_hello([_ | T]) ->
    xml_check_hello(T);
xml_check_hello(_) ->
    nok.

wait_for_ssl_exit(Sock) ->
    receive
        {ssl_closed, Sock} ->
            ct:pal("SSL connection closed", []),
            ok;
        _ ->
            wait_for_ssl_exit(Sock)
    after
        10000 ->
        ct:pal("wait_for_ssl_exit/1, timeout waiting for ssl to close", []),
        nok
    end.

look_for_ssl(Sock, Find) ->
    look_for_ssl(Sock, Find, <<>>).
look_for_ssl(Sock, Find, Acc)->
    receive
        {ssl, Sock, Bin} ->
            New_acc = <<Acc/binary, Bin/binary>>,
            case binary:match(New_acc, Find) of
                nomatch ->
                    look_for_ssl(Sock, Find, New_acc);
                _ ->
                    ct:pal("Matched: ~p", [Find]),
                    ok
            end;
        Other ->
            ct:pal("look_for_ssl(...): got other: ~p~n", [Other]),
            look_for_ssl(Sock, Find, Acc)
after
    10000 ->
        ct:pal("look_for_ssl/3, timeout: ~p~n", [Acc]),
        nok
end.

flush_ssl() ->
    receive
        {ssl, _, _}->
            flush_ssl()
    after
        0 -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Add needed parameters to Config -proplist,
%% the majority of environment hardcoding is here.
%% @spec prepare_config(Config) -> New_config
%% @end
%%--------------------------------------------------------------------
prepare_config(Config) ->
    Target_dep =
	case os:getenv("SIM_OR_TARGET") of
	    "sim" ->
		Ports =
		    lists:map(
			fun(Type) ->
			    {Type, rct_rpc:call(rpc1, sysEnv, get_port_conf,
						[Type], 10000)}
			end, [cli_tls, coli_tls, netconf_tls]),
		[{dut_ip, "localhost"} | Ports];
	    _ ->
		% default portnumbers on target (in SYS:make_release.escript)
		Dut = ?config(1, ct:get_config(test_nodes)),
		Dut_props =  ct:get_config(Dut),
		Dut_ip = ?config(ssh, ?config(ssh_lmt_ipv4, Dut_props)),
		[{dut_ip, Dut_ip}, {cli_tls, 9830}, {coli_tls, 9831},
		 {netconf_tls, 6513}]
	end,
    My_priv_dir =
	filename:join(
	    lists:droplast(
		filename:split(
		    ?config(priv_dir, Config)))),
    Nc_file_name = "tls_nodecert_pkcs12",
    User_ca_file_name = "user-ca.crt",
    Me_id = "1", 

    [{me_id, Me_id},
     {my_priv_dir, My_priv_dir},
     {expert_cert_file,
      filename:join([?config(data_dir, Config), "user_expert.crt"])},
     {expert_key_file,
      filename:join([?config(data_dir, Config), "user_expert.key"])},
     {alt_expert_cert_file,
      filename:join([?config(data_dir, Config), "alt_user_expert.crt"])},
     {alt_expert_key_file,
      filename:join([?config(data_dir, Config), "alt_user_expert.key"])},
     {nodeCredentialId, "TLS_login"},
     {nc_subject_name, "esekilvxen519.rnd.ki.sw.ericsson.se"},
     {nc_key_info, "RSA_2048"},
     {nc_finger_p,
      "17:8b:19:ef:57:e1:12:62:67:33:f5:bd:bd:8c:28:8e:bd:4b:c2:ce"},
     {nc_cred_pwd, "idiocy"},
     {nc_file, Nc_file_name},
     {tc_name, "TLS_login_tc"},
     {user_ca_file , User_ca_file_name},
     {user_ca_subject,
	"C=se,ST=Stockholm,L=Kista,O=RBS-CS,OU=etxlg's User CA,CN=User CA"},
     {user_ca_finger_p,
      "DB:59:94:FB:BE:E6:1B:83:D4:77:88:BF:F8:27:9B:B9:BC:A0:5D:23"},
     {cert_to_sftp_files,[Nc_file_name, User_ca_file_name]},
     {sftp_server, ct:get_config(sftp_server)},
     {ldap_server, ?config(host, ct:get_config(ldap_server))},
     {ldap_base_dn, "ou=people,dc=mordor,dc=invalid"},
     {ldap_bind_dn, "cn=king,dc=mordor,dc=invalid"},
     {ldap_bind_pw, "1:7TcvZCTcqkKUI6RNL3IKSlMB/kas"} |
	Target_dep] ++ Config.

set_user_label(Label) ->
% make a harmless change to confirm we have access
    {'ManagedElement',          
     [{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],

     [{managedElementId,[],["1"]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM',
         [],
         [{secMId,[],["1"]},
          {'UserManagement',[],
           [{userManagementId,[],["1"]},
            {'LdapAuthenticationMethod',
             [],
             [{ldapAuthenticationMethodId,[],["1"]},
              {'Ldap',[],
               [{ldapId,[],["1"]},
                {userLabel, [], [Label]}
            ]}]}]}]}]}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
configure_ldap(Config) ->
    %needed for SIM where there is normally no LDAP conf done
    Me_id = ?config(me_id, Config),
    Ldap_server = ?config(ldap_server, Config),
    Base_dn = ?config(ldap_base_dn, Config),
    Bind_dn = ?config(ldap_bind_dn, Config),
    Bind_pw = ?config(ldap_bind_pw, Config),
    configure_ldap(Me_id, Ldap_server, Base_dn, Bind_dn, Bind_pw).

configure_ldap(Me_id, Ldap_server, Base_dn, Bind_dn, Bind_pw) ->
    Edit =
 {'ManagedElement',
     [{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
     [{managedElementId,[],[Me_id]},
      {'SystemFunctions',
       [{systemFunctionsId,[],["1"]},
        {'SecM', [], [{secMId,[],["1"]},
          {'UserManagement',[],
           [{userManagementId,[],["1"]},
                    {targetType, [{'xc:operation', "delete"}],[]},
            {'LdapAuthenticationMethod',
             [],
             [{ldapAuthenticationMethodId,[],["1"]},
              {administrativeState,[],["UNLOCKED"]},
              {'Ldap',[],
               [{ldapId,[],["1"]},
                {ldapIpAddress,[],[Ldap_server]},
                {fallbackLdapIpAddress,[{'xc:operation', "delete"}],[]},
                {nodeCredential,[{'xc:operation', "delete"}],[]},
                {serverPort,[{'xc:operation', "delete"}],[]},
                {trustCategory,[{'xc:operation', "delete"}],[]},
                {tlsMode,[],["STARTTLS"]},
                {useTls,[],["false"]},
                {useReferrals,[],["false"]},
                {bindDn,[],[Bind_dn]},
                {bindPassword,[{struct,"EcimPassword"}],
                 [{password,[],[Bind_pw]}]},
                {baseDn, [], [Base_dn]},
                {profileFilter,[],["POSIX_GROUPS"]},
                {userLabel, [], [
                    "Default (created by " ++ atom_to_list(?MODULE) ++  ")"]},
                {'EricssonFilter',[],
                 [{ericssonFilterId,[],["1"]},
                          {roleAliasesBaseDn,
                           [{'xc:operation', "delete"}],[]},
                  {targetBasedAccessControl,[],["LOCKED"]}]},
                {'Filter',[],
                 [{filterId,[],["1"]},
                          {filter,[{'xc:operation', "delete"}],[]},
                          {type,[{'xc:operation', "delete"}],[]},
                  {userLabel,[{'xc:operation', "delete"}],[]}]}
               ]}]}]}]}]}]},
    ct:pal("Configure default LDAP setting: ~p",[Ldap_server]),
    case netconf(edit_config, [nc1, running, Edit]) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

configure_for_tls(Config) ->
    Me_id = ?config(me_id, Config),
    Nc_id = ?config(nodeCredentialId, Config),
    Subject = ?config(user_ca_subject, Config),
    Tc_name = ?config(tc_name, Config),
    try find_ca(Me_id, Subject) of
	{Maybe_ca_id, "ENABLED", "VALID"}  when length(Maybe_ca_id) >= 1 ->
	    ct:pal("CA already present and ENABLED"),
	    ok;
	{Maybe_ca_id, _, _}  when length(Maybe_ca_id) >= 1 ->
	% a CA that works is installed
	    ct:pal("CA already present - activating"),
	    activate_ca(Me_id, Maybe_ca_id, Subject),
	    ct:pal("Waiting 5 sec to enable activation to go through"),
	    ok;
	What -> 
	    ct:pal("No CA found: ~p", [What]),
	    install_ca(Config, Subject)
    catch
	A:B ->
	    ct:pal("No CA found: ~p", [{A,B, erlang:get_stacktrace()}]),
	    install_ca(Config, Subject)
    end,
    {Ca_id, _, _} = find_ca(Me_id, Subject),
    case check_nc(Me_id, Nc_id) of
	ok -> 
	    ct:pal("NC already present"),
	    ok;
	nok ->
	    install_nc(Config)
    end,
    %just create/recreate the TCat, no matter if already there (quick)
    ok = create_tc(Me_id, Tc_name, Ca_id),
    %now we have everything needed in CERT (and LDAP is also done if SIM)
    case is_tls_login_active(Me_id, Nc_id, Tc_name) of
	ok ->
	    ct:pal("TLS already configured in SysM"),
	    ok;
	nok ->
	    activate_tls_login(Me_id, Nc_id, Tc_name),
	    ct:pal("Sleep 5 seconds after activation to give the server "
		   "time to start"),
	    timer:sleep(5000)
    end.

%for test of test
find_ca() ->
    Subject =
        "C=se,ST=Stockholm,L=Kista,O=RBS-CS,OU=etxlg's User CA,CN=User CA",
    find_ca("1", Subject).

%return a tuple ex:
%{"1", "ENABLED", "VALID"} | {"1", "DISABLED", "NOT_VALID_YET"}
find_ca(Me_id, Wanted_subject) ->
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','SystemFunctions','SecM','CertM',
             'TrustedCertificate'],
    All_trusted = struct(Conf, Names),
    Id_sub_valid_and_state =
        [begin
            [Id] = struct(T, [trustedCertificateId]),
            [Valid] = struct(T, [managedState]),
            [State] = struct(T, [certificateState]),
            [Cert_cont] = struct(T, [certificateContent]),
            [Subject] = struct(Cert_cont, [subject]),
            {Id, Subject, Valid, State}
        end || T <- All_trusted],
    %if there are several just chose the first
    Found_ids =
	[begin 
	    ct:pal(
		"Matching CA, Id: ~p, managedState: ~p, certificateState: ~p",
		[Id, Valid, State]),
	    {Id, Valid, State}
	end  || {[Id], Subject, [Valid], [State]} <- Id_sub_valid_and_state,
                    Subject =:= [Wanted_subject]],
    ct:pal("Matching CA ids: ~p", [Found_ids]),
    case Found_ids of %jenkins console parser doesn't like crashes/errors
	[] -> {nok, "no matching CA installed"};
	_ ->  hd(Found_ids)
    end.


install_ca(Config, Subject) ->
    Me_id = ?config(me_id, Config),
    Subject = ?config(user_ca_subject, Config),
    Dir = ?config(my_priv_dir, Config),
    Sftp_data = ?config(sftp_server, Config),
    Shost = ?config(host, Sftp_data),
    Uname = ?config(username, Sftp_data),
    Spwd = ?config(password, Sftp_data),
    File =  filename:join([Dir, ?config(user_ca_file, Config)]),
    Uri = "sftp://" ++ Uname ++ "@" ++ Shost ++ File,
    Finger_p = ?config(user_ca_finger_p, Config),

    Action =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]},
                {installTrustedCertFromUri, [],
                 [{uri, [Uri]},
                  {uriPassword, [Spwd]},
                  {fingerprint, [Finger_p]}]}]}]}]}]},
    ct:pal("Install CA for TLS-login",[]),
    case  netconf(action, [nc1, Action]) of
        {ok, _} ->
	    Ca_id = wait_for_ca(Me_id, Subject, 10),
	    ct:pal("CA was found: ~p, - waiting 10 seconds to see if CERT "
		   "allows activation (remove this when not needed).", [Ca_id]),
	    timer:sleep(10 * 1000),
	    activate_ca(Me_id, Ca_id, Subject);
        Error ->
            ct:fail(Error)
    end.

%for test of test
activate_ca() ->
    {Ca_id, _, _}  = find_ca(),
    Subject =
	"C=se,ST=Stockholm,L=Kista,O=RBS-CS,OU=etxlg's User CA,CN=User CA",
    activate_ca("1", Ca_id, Subject).

activate_ca("1", Ca_id, Subject) ->
    activate_ca("1", Ca_id, Subject, 1).

activate_ca(_, _, _, 11) ->
    ct:fail("Unable to activate the CA");

activate_ca(Me_id, Ca_id, Subject, N) ->
    ct:pal("Activating CA with ID: ~p, try: ~p of 10", [Ca_id, N]),
    Edit_enable = get_activate_ca_edit(Me_id, Ca_id, "ENABLED"),
    Edit_disable = get_activate_ca_edit(Me_id, Ca_id, "DISABLED"),
    ok = netconf(edit_config, [nc1, running, Edit_enable]),
    case find_ca(Me_id, Subject) of
	{_Id, "ENABLED", "VALID"} ->
	    ok;
	{_Id, Manstate, Certstate} ->
	    ct:pal("CERT workaround, managedState: ~p, certificateState: ~p~n"
		   "DISABLING CA", [Manstate, Certstate]),
	    ok = netconf(edit_config, [nc1, running, Edit_disable]),
	    ct:pal("CERT workaround, ENABLING again and waiting 1 second"),
	    ok = netconf(edit_config, [nc1, running, Edit_enable]),
	    timer:sleep(1000),
	    activate_ca(Me_id, Ca_id, Subject, N + 1)
    end.

get_activate_ca_edit(Me_id, Ca_id, Enable_disable) ->
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]},
                {'TrustedCertificate',
                 [{trustedCertificateId,[],[Ca_id]},
                  {managedState,[],[Enable_disable]}]}]}]}]}]}.


wait_for_ca(_, Subject, 0) ->
    ct:fail("Timeout waiting for CA creation: ~p", [Subject]);
wait_for_ca(Me_id, Subject, N) ->
    try find_ca(Me_id, Subject) of
	{Ca_id, _, _}  when length(Ca_id) >= 1 ->
	    Ca_id;
	_ -> 
	    timer:sleep(1000),
	    wait_for_ca(Me_id, Subject, N - 1)
    catch
	_:_ ->
	    timer:sleep(1000),
	    wait_for_ca(Me_id, Subject, N - 1)
    end.

install_nc(Config) ->
    Nc_name = ?config(nodeCredentialId, Config),
    Subject_name = ?config(nc_subject_name, Config),
    Key_info = ?config(nc_key_info, Config),
    Finger_p = ?config(nc_finger_p, Config),
    Cred_pwd = ?config(nc_cred_pwd, Config),
    Me_id = ?config(me_id, Config),
    Dir = ?config(my_priv_dir, Config),
    Sftp_data = ?config(sftp_server, Config),
    Shost = ?config(host, Sftp_data),
    Uname = ?config(username, Sftp_data),
    Spwd = ?config(password, Sftp_data),
    File =  filename:join([Dir, ?config(nc_file, Config)]),
    Uri = "sftp://" ++ Uname ++ "@" ++ Shost ++ File,

    ok = prepare_nc(Me_id, Subject_name, Key_info, Nc_name),
    ok = install_nc(Me_id, Uri, Spwd, Cred_pwd, Finger_p, Nc_name),
    wait_for_nc(Me_id, Nc_name, 10).

wait_for_nc(_, Nc_name, 0) -> 
    ct:fail("Timeout waiting for NC creation: ~p", [Nc_name]);
wait_for_nc(Me_id, Nc_name, N) -> 
    case check_nc(Me_id, Nc_name) of
	ok -> ok;
	nok ->
	    ct:pal("NC not yet imported, wait another second: ~p", [N]),
	    timer:sleep(1000),
	    wait_for_nc(Me_id, Nc_name, N - 1)
    end.

%test of test
check_nc() ->
    check_nc("1", "TLS_login").

check_nc(Me_id, Nc) ->
    Get =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{secMId,[],["1"]},
             {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{certMId,[],["1"]},
               {'NodeCredential',
                [{nodeCredentialId, [Nc]}]}]}]}]}]},
    Names = ['ManagedElement','SystemFunctions','SecM','CertM',
                'NodeCredential', 'certificateState'],
    try
        begin
            {ok, Conf} = netconf(get, [nc1, Get]),
            struct(Conf, Names)
        end of
        [["VALID"]] ->
            ok;
        Bad ->
            ct:pal("check_nc, bad -> nok: ~p", [Bad]),
            nok
    catch
        A:B ->
            ct:pal("check_nc, catch  -> nok: ~s", [make_safe({A, B})]),
            nok
    end.


install_nc(Me_id, Uri, Pwd, Cred_pwd, Finger_print, Nc_name) ->
    ct:pal("Uri: ~p~n", [Uri]),
    Install = {'ManagedElement',
              [{xmlns,"urn:com:ericsson:ecim:ComTop"},
               {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
              [{managedElementId,[],[Me_id]},
               {'SystemFunctions',
                [{systemFunctionsId,[],["1"]},
                 {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                  [{secMId,[],["1"]},
                   {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                    [{certMId,[],["1"]},
                     {'NodeCredential',
                      [{nodeCredentialId, [Nc_name]},
                       {installCredentialFromUri,
                        [{uri, [Uri]},
                         {uriPassword, [Pwd]},
                         {credentialPassword, [Cred_pwd]},
                         {fingerprint, [Finger_print]}]}]}]}]}]}]},

    ct:pal("Action install TLS NC",[]),
    case  netconf(action, [nc1, Install]) of
        {ok, _} ->
            ok;
        Error ->
            ct:fail(Error)
    end.

prepare_nc(Me_id, Subject_name, Key_info, Nc_name) ->
    Edit = {'ManagedElement',
            [{xmlns,"urn:com:ericsson:ecim:ComTop"},
             {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
            [{managedElementId,[],[Me_id]},
             {'SystemFunctions',
              [{systemFunctionsId,[],["1"]},
               {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
                [{secMId,[],["1"]},
                 {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
                  [{certMId,[],["1"]},
                   {'NodeCredential',
                    [{nodeCredentialId, [Nc_name]},
                     {userLabel, ["Created by " ++ atom_to_list(?MODULE)]},
                     {subjectName, [Subject_name]},
                     {keyInfo, [Key_info]}]}]}]}]}]},

    ct:pal("Create TLS Node Credential",[]),
    case netconf(edit_config, [nc1, running, Edit]) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.


%for test of test
create_tc() ->
    {Ca_id, _, _} = find_ca(),
    create_tc("1", "TLS_login_tc", Ca_id).

create_tc(Me_id, Tc_name, Ca_id) ->
    Mo_ref = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
             "TrustedCertificate=" ++ Ca_id,
    Edit =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SecM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
             [{secMId,[],["1"]},
              {'CertM', [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
               [{certMId,[],["1"]},
                   {'TrustCategory', [],
                    [{trustCategoryId, [], [Tc_name]},
                     {userLabel, ["Created by " ++ atom_to_list(?MODULE)]},
                     {trustedCertificates, [Mo_ref]}]}]}]}]}]},

    ct:pal("Creating TC, Id: ~p~nContaining: ~p", [Tc_name, Mo_ref]),
    ok = netconf(edit_config, [nc1, running, Edit]).

%for test of test
is_tls_login_active() ->
    Nc_name = "TLS_login",
    Tc_name = "TLS_login_tc",
    is_tls_login_active("1", Nc_name, Tc_name).

is_tls_login_active(Me_id, Nc_name, Tc_name) ->
    case {is_tls_login_active(netconf, Me_id, Nc_name, Tc_name), 
	  is_tls_login_active(cli, Me_id, Nc_name, Tc_name)} of
	{ok, ok}  ->
	    ok;
	{nok, nok}  ->
	    nok;
	{_, _}  ->
	    ct:pal("WARNING cli/netconf partly enabled/disabled", []),
	    nok
    end.

is_tls_login_active(Service, Me_id, Nc_name, Tc_name) ->
    Service_atom = service_atom(Service),
    Service_id = service_id(Service),
    Nc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "NodeCredential=" ++ Nc_name,
    Tc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "TrustCategory=" ++ Tc_name,
    Get =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{sysMId,[],["1"]},
             {Service_atom, [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{Service_id,[],["1"]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),

    Tls_base = ['ManagedElement','SystemFunctions','SysM', Service_atom],

    case {struct(Conf, Tls_base ++ ['nodeCredential']),
          struct(Conf, Tls_base ++ ['trustCategory']),
          struct(Conf, Tls_base ++ ['administrativeState'])} of
        {[[Nc_dn]], [[Tc_dn]], [["UNLOCKED"]]} -> ok;
        _ -> nok
    end.


%for test of test
activate_tls_login() ->
    Nc_name = "TLS_login",
    Tc_name = "TLS_login_tc",
    Nc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "NodeCredential=" ++ Nc_name,
    Tc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "TrustCategory=" ++ Tc_name,
    activate_tls_login("1", Nc_dn, Tc_dn).

activate_tls_login(Me_id, Nc_name, Tc_name) ->
    [activate_tls_login(Service, Me_id, Nc_name, Tc_name) ||
	Service <- [netconf, cli]].

activate_tls_login(Service, Me_id, Nc_name, Tc_name) ->
    Service_atom = service_atom(Service),
    Service_id = service_id(Service),
    Nc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "NodeCredential=" ++ Nc_name,
    Tc_dn = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,"
              "TrustCategory=" ++ Tc_name,
    Edit =
        {'ManagedElement',
        [{xmlns,"urn:com:ericsson:ecim:ComTop"},
         {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
        [{managedElementId,[],[Me_id]},
         {'SystemFunctions',
          [{systemFunctionsId,[],["1"]},
           {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
            [{sysMId,[],["1"]},
             {Service_atom, [{xmlns,"urn:com:ericsson:ecim:RcsCertM"}],
              [{Service_id,[],["1"]},
               {administrativeState, [], ["UNLOCKED"]},
               {nodeCredential, [], [Nc_dn]},
               {trustCategory, [], [Tc_dn]}]}]}]}]},
    ct:pal("Activate TLS: ~p: login",[Service]),
    case netconf(edit_config, [nc1, running, Edit]) of
        ok -> ok;
        Error1 ->
            ct:fail(Error1)
    end.

service_atom(netconf) -> 'NetconfTls';
service_atom(cli) -> 'CliTls'.

service_id(netconf) -> 'netconfTlsId';
service_id(cli) -> 'cliTlsId'.

prepare_files(Config) ->
    % copy the files that the node may need to fetch using sftp
    % tls_nodecert_pkcs12: used as nodecredential in the TLS server
    % user-ca.crt: CA, (trusted-certificate) used to validate the login-cert
    Files = ?config(cert_to_sftp_files, Config),
    Priv_dir = ?config(my_priv_dir, Config),
    Data_dir = ?config(data_dir, Config),
    ct:pal("prepare_files: ~p, ~p, ~p", [Files, Priv_dir, Data_dir]),
    [begin
        Fpriv = filename:join([Priv_dir, F]),
        Fdata = filename:join([Data_dir, F]),
        case {file:read_file(Fpriv), file:read_file(Fdata)} of
            {{ok, Same}, {ok, Same}} -> ok;
            {_, {ok, Bin_file}} ->
                ok = file:write_file(Fpriv, Bin_file)
        end
     end || F <- Files].


netconf(F, A) ->
    %%{ok, _} = ct_netconfc:open(nc1, [{user, "expert"}, {password, "expert"}]),
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    case Res of
        {error, _} ->
	    catch ct_netconfc:close_session(nc1),
            Res;
        _ ->
            ok = ct_netconfc:close_session(nc1)
    end,
    Res.

%descend into the structure following list(Names), when only one Name
%remains collect all matching and return them
struct([{Name, _, Next} | T], [Name]) ->
    [Next | struct(T, [Name])];
struct([{_, _, _} | T], [Name]) ->
    struct(T, [Name]);
struct([], _) -> [];
struct([{Name, _, Next} | _], [Name | Names]) ->
    struct(Next, Names);
struct([{_, _, _} | T], Names) ->
    struct(T, Names).

make_safe({A, B}) ->
    "{" ++ m_safe(A) ++ "," ++ m_safe(B) ++ "}".

m_safe(C) ->
    re:replace(
	io_lib:format("~p", [C]),
	"error", "boogaloo",
	[global, {return, list}]).
