%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_sec_event_SUITE.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/1
%%%
%%% @doc == Test Suite for testing the transfer ESI from SUT using netconf.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end


-module(log_sec_event_SUITE).
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
%%% R2A/1      2013-09-23  etxasta     Created
%%% R3A/13     2015-02-05  erarafo     Edoc complaints fixed
%%% R3A/14     2015-02-28  etxkols     Preparation for 2 labs
%%% R3A/15     2015-02-28  etxkols     Preparation for 2 labs
%%% R3A/17     2015-03-24  etxpejn     Changed CXC no for RTSEL
%%% R3A/18     2015-05-19  etxkols     sftpserver and syslogserver are different in new lab
%%% R3A/19     2015-05-27  etxpejn     Added tc sec_event2 due to HT78791
%%% R3A/20     2015-06-08  etxpejn     Extended test in sec_event2 due to HT81770 
%%% R3A/4      2015-07-10  etxjovp     Add group definitions used by CS CI
%%% R4A/1      2016-02-12  etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R6A/1      2016-05-24  etxpejn     Added call to log_license_support
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 sec_event/1,
	 sec_event2/1,
         sec_event_tls/1,
	 groups/0]).

%% Used for UDP
%% -define(SYSLOG_HOST1, "10.68.200.11").
 -define(SYSLOG_PORT1, 22).
%% -define(USER1,        "mauve").
%% -define(PWD1,         "dilbert").
%% Used for TLS
%% -define(SYSLOG_HOST2, "10.68.200.9").
 -define(SYSLOG_PORT2, 22).
%% -define(USER2,        "sniffer").
%% -define(PWD2,         "rcsrcs12").

-define(CMD1,         "cat /var/log/syslog | grep ").
-define(CMD2,         "cat /var/log/auth.log | grep ").
-define(LMA_DIR, "/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/LMA/LMA_CNX9013077/test/suites/").
-define(SFTP_DIR, "/proj/rcs-tmp/stps/").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc},
		 {rct_netconf,{nc1, html}},
                 {cth_conn_log,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
]}].


%% @hidden
init_per_suite(Config) ->
    crypto:start(),
    ssh:start(),
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.
%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []} 
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [sec_event,
     sec_event2].

%%--------------------------------------------------------------------
%% @doc
%% Using netconf to configure a syslog log from SUT to a external syslog server.<br/>
%% This TC test that a security event can be sent to a syslog server using netconf<br/>
%% configuration. The syslog event will be fetched on the external syslog server to<br/>
%% verify that it has arrived correctly.<br/>
%% TC will check progressreport using netconf.
%%
%% @spec sec_event(Config) -> ok
%% @end
%%--------------------------------------------------------------------
sec_event(_Config) ->
    [{host, SysLogUdpHost},{username, _Username},{password, _Password}] = 
	ct:get_config(syslog_udp_server),

    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),

    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    
    %% configure security event streaming using netconf
    configure_logPushTransfer(SysLogUdpHost),
    ct:pal("---> Security event configured~n",[]),
    timer:sleep(1000),

    %% Trig a security event
    ct:pal("---> Trig a security event~n",[]),
    {A1, B1, C1} = os:timestamp(),
    Tag = "TAG" ++ integer_to_list(A1) ++ integer_to_list(B1) ++ integer_to_list(C1),
    ok = rct_rpc:call(rpc, logI, write_log,
		      ["SecurityLog", "-", "log_sec_event_SUITE", 16, info, {A1, B1, C1}, Tag], 
		      10000),

    OpState = check_logPushTransfer_operation_state(),
    
    case OpState of
	["DISABLED"] ->
	    ct:pal("---> OperationalState Disabled, security event should NOT have reached the "
		   "configured syslog server~n",[]),
	    {error, _Reason} = check_syslog_file(Tag),

	    %% Change the OPstate to enabled by installing the LKF and activate RTSEL
	    log_license_support:install_lkf(SFTP_URL, Password),
	    ct:pal("---> LKF is installed"),
	    change_featureState_for_rtsel("ACTIVATED"),
	    timer:sleep(1000),

	    %% Trig a new security event
	    ct:pal("---> Trig a security event~n",[]),
	    {A2, B2, C2} = os:timestamp(),
	    Tag2 = "TAG" ++ integer_to_list(A2) ++ integer_to_list(B2) ++ integer_to_list(C2),
	    ok = rct_rpc:call(rpc, logI, write_log,
			      ["SecurityLog", "-", "log_sec_event_SUITE", 16, info,
			       {A2, B2, C2}, Tag2], 10000),

	    %% Make sure that the operationalstate has change to enabled
	    OpState2 = check_logPushTransfer_operation_state(),

	    case OpState2 of
		["ENABLED"] ->
		    ct:pal("---> OperationalState Enabled, security event should have reached the "
			   "configured syslog server~n",[]),
		    %% Check result of the event
		    ct:pal("---> Check if security event reached the configured syslog server~n",
			   []),
		    ok = check_syslog_file(Tag2),
		    ct:pal("---> Test passed!!!", []);
		["DISABLED"] ->
		    ct:pal("---> OperationalState disabled, since the RTSEL license doesn't seem "
			   "available, this tc will fail!"),
		    %% Check result of the event
		    ct:pal("---> Check if security event reached the configured syslog server~n",
			   []),
		    ok = check_syslog_file(Tag2)
	    end;
	["ENABLED"] ->
	    ct:pal("---> OperationalState Enabled, security event should have reached the "
		   "configured syslog server~n",[]),
	    ok = check_syslog_file(Tag),
	    ct:pal("---> Test passed!!!", [])
    end.


sec_event2(_Config) ->
    %% Added due to HT78791 & HT81770
    [{host, SysLogUdpHost},{username, _Username},{password, _Password}] = 
	ct:get_config(syslog_udp_server),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),

    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    
    %% ct:pal("---> Deactivate RTSEL license~n",[]),
    %% change_featureState_for_rtsel("DEACTIVATED"),
  
    configure_logPushTransfer(SysLogUdpHost, "SecurityLog"),
    configure_logPushTransfer(SysLogUdpHost, "AuditTrailLog"),

    log_license_support:install_lkf(SFTP_URL, Password),
    ct:pal("---> LKF is installed"),
    change_featureState_for_rtsel("ACTIVATED"),
    timer:sleep(1000),

    %% Make sure that the operationalstate has change to enabled
    OpState = check_logPushTransfer_operation_state("SecurityLog"),
    OpState2 = check_logPushTransfer_operation_state("AuditTrailLog"),
    case {OpState,  OpState2} of
	{["ENABLED"], ["ENABLED"]} ->
	    ct:pal("---> OperationalState Enabled for both SecurityLog & AuditTrailLog~n",[]),

	    %% HT81770
	    ct:pal("---> Delete logPushTransfer for SecurityLog~n",[]),
	    ok = delete_logPushTransfer("SecurityLog"),
	    ct:pal("---> Create logPushTransfer for SecurityLog again ~n",[]),
	    configure_logPushTransfer(SysLogUdpHost, "SecurityLog"),
	    ["ENABLED"] = check_logPushTransfer_operation_state("SecurityLog"),
	    
	    ct:pal("---> Delete logPushTransfer for both SecurityLog & AuditTrailLog~n",[]),

	    ok = delete_logPushTransfer("SecurityLog"),
	    ok = delete_logPushTransfer("AuditTrailLog"),
	    timer:sleep(1000),

	    ct:pal("---> Create logPushTransfer again ~n",[]),
	    configure_logPushTransfer(SysLogUdpHost, "SecurityLog"),
	    configure_logPushTransfer(SysLogUdpHost, "AuditTrailLog"),
	    timer:sleep(1000),
	    
	    ct:pal("---> Check OperationalState for both SecurityLog & AuditTrailLog~n",[]),
	    OpState3 = check_logPushTransfer_operation_state("SecurityLog"),
	    OpState4 = check_logPushTransfer_operation_state("AuditTrailLog"),
	    case {OpState3,  OpState4} of
		{["ENABLED"], ["ENABLED"]} ->
		    ct:pal("---> OperationalState Enabled for both SecurityLog & "
			   "AuditTrailLog~n",[]),
		    ct:pal("---> Test passed!!!", []);
		{Op3, Op4} ->
		    ct:pal("---> OperationalState NOT Enabled : ~p ~p~n", [Op3, Op4]),
		    ct:fail("The OperationalState has not changed!")
	    end;
	R ->
	    ct:pal("---> OperationalState NOT Enabled R: ~p~n", [R]),
	    ct:fail("The OperationalState has not changed!")
    end.


%%--------------------------------------------------------------------
%% @doc
%% Using netconf to configure a syslog log from SUT to a external syslog server<br/>
%% using TLS.<br/>
%% This TC test that a security event can be sent to a syslog server using netconf<br/>
%% configuration. The syslog event will be fetched on the external syslog server to<br/>
%% verify that it has arrived correctly.<br/>
%% TC will check progressreport using netconf.
%%
%% Observe that this test case require that the syslogs test case in cert has been<br/>
%% executed in advance. To install needed certificates.
%%
%% @spec sec_event_tls(Config) -> ok
%% @end
%%--------------------------------------------------------------------
sec_event_tls(_Config) ->
    %% configure security event streaming using netconf
    [{host, SysLogTlsHost},{username, Username},{password, Password}] = ct:get_config(syslog_tls_server),
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    
    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    
    NC = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=certm_SUITE_pkcs12_offline_http",
    TCAT = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=certm_SUITE",
    A = {'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',[],
	   [{systemFunctionsId,[],["1"]},
	    {'LogM',[{xmlns,"urn:com:ericsson:ecim:LogM"}],
	     [{logMId,[],["1"]},
                 {nodeCredential, [], [NC]},
                 {trustCategory, [], [TCAT]},
	      {'Log',[],
	       [{logId,[],["SecurityLog"]},
		{'LogPushTransfer',[],
		 [{logPushTransferId,[],["2"]},
		  {uri,[],["syslogs://"++SysLogTlsHost++":10514"]},
		  %% {uri,[],["syslogs://10.68.200.9:10514"]},
		  {transferType,[],["STREAM"]}]}]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    ok = ct_netconfc:edit_config(nc1, running, A),
    ct:pal("---> Security event configured~n",[]),
    ct_netconfc:close_session(nc1),
    timer:sleep(1000),

    %% Trig a security event
    ct:pal("---> Trig a security event~n",[]),
    {A1, B1, C1} = os:timestamp(),
    Tag = "TAG" ++ integer_to_list(A1) ++ integer_to_list(B1) ++ integer_to_list(C1),
    ok = rct_rpc:call(rpc, logI, write_log,
        ["SecurityLog", "-", "log_sec_event_SUITE", 4, info, {A1, B1, C1}, Tag], 10000),

    LogPushTransfer = {'ManagedElement',
		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId,[],["1"]},
			{'SystemFunctions',
			 [{systemFunctionsId,[],["1"]},
			  {'LogM',
			   [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			   [{logMId,[],["1"]},
			    {'Log',
			     [{logId,[],["SecurityLog"]},
			      {'LogPushTransfer',
			       [{logPushTransferId,[],["2"]},
				{operationalState,[],[]}
			       ]}]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, R} = ct_netconfc:get(nc1, LogPushTransfer),
    ct_netconfc:close_session(nc1),
    {ok,{operationalState,_,OpState}} = extract_element(operationalState, R),

    case OpState of
	["DISABLED"] ->
	    ct:pal("---> OperationalState Disabled, security event should NOT have reached the "
		   "configured syslog server~n",[]),
	    {error, _Reason} = check_syslog_file(Tag, Username, Password , SysLogTlsHost, 
						 ?SYSLOG_PORT2, ?CMD2),


	    %% Change the OPstate to enabled by installing the LKF and activate RTSEL
	    log_license_support:install_lkf(SFTP_URL, Password),
	    change_featureState_for_rtsel("ACTIVATED"),
	    timer:sleep(1000),

	    %% Trig a new security event
	    ct:pal("---> Trig a security event~n",[]),
	    {A2, B2, C2} = os:timestamp(),
	    Tag2 = "TAG" ++ integer_to_list(A2) ++ integer_to_list(B2) ++ integer_to_list(C2),
	    ok = rct_rpc:call(rpc, logI, write_log,
			      ["SecurityLog", "-", "log_sec_event_SUITE", 16, info,
			       {A2, B2, C2}, Tag2], 10000),

	    %% Make sure that the operationalstate has change to enabled
	    LogPushTransfer2 = {'ManagedElement',
				[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				[{managedElementId,[],["1"]},
				 {'SystemFunctions',
				 [{systemFunctionsId,[],["1"]},
				  {'LogM',
				   [{xmlns,"urn:com:ericsson:ecim:LogM"}],
				   [{logMId,[],["1"]},
				    {'Log',
				     [{logId,[],["SecurityLog"]},
				      {'LogPushTransfer',
				       [{logPushTransferId,[],["2"]},
					{operationalState,[],[]}
				       ]}]}]}]}]},
	    {ok,_} = ct_netconfc:open(nc1,[]),
	    {ok, R2} = ct_netconfc:get(nc1, LogPushTransfer2),
	    ct_netconfc:close_session(nc1),
	    {ok,{operationalState,_,OpState2}} = extract_element(operationalState, R2),
	    case OpState2 of
		["ENABLED"] ->
		    ct:pal("---> OperationalState Enabled, security event should have reached the "
			   "configured syslog server~n",[]),
		    %% Check result of the event
		    ct:pal("---> Check if security event reached the configured syslog server~n",
			   []),
		    ok = check_syslog_file(Tag2, Username, Password, SysLogTlsHost,?SYSLOG_PORT2 , ?CMD2),
%		    ok = check_syslog_file(Tag2, ?USER2, ?PWD2, ?SYSLOG_HOST2, ?SYSLOG_PORT2, ?CMD2),
		    ct:pal("---> Test passed!!!", []);
		["DISABLED"] ->
		    ct:pal("---> OperationalState disabled, since the RTSEL license doesn't seem "
			   "available, this tc will fail!"),
		    %% Check result of the event
		    ct:pal("---> Check if security event reached the configured syslog server~n",
			   []),
		    ok = check_syslog_file(Tag2, Username, Password, SysLogTlsHost, ?SYSLOG_PORT2, ?CMD2)
%		    ok = check_syslog_file(Tag2, ?USER2, ?PWD2, ?SYSLOG_HOST2, ?SYSLOG_PORT2, ?CMD2)
            end;
        ["ENABLED"] ->
	    ct:pal("---> OperationalState Enabled, security event should have reached the "
                "configured syslog server~n",[]),
            ok = check_syslog_file(Tag, Username, Password, SysLogTlsHost, ?SYSLOG_PORT2, ?CMD2),
%            ok = check_syslog_file(Tag, ?USER2, ?PWD2, ?SYSLOG_HOST2, ?SYSLOG_PORT2, ?CMD2),
            ct:pal("---> Test passed!!!", [])
    end,
    ct:pal("---> Test case complete~n",[]).





%%%--------------------------------------------------------------------
%%% Description: Fetch the syslog log file from the external syslog
%%%              server via ssh.
%%%--------------------------------------------------------------------
check_syslog_file(Tag) ->
    [{host, SysLogUdpHost},{username, Username},{password, Password}] = ct:get_config(syslog_udp_server),
    check_syslog_file(Tag, Username, Password, SysLogUdpHost, ?SYSLOG_PORT1, ?CMD1).
%    check_syslog_file(Tag, ?USER1, ?PWD1, ?SYSLOG_HOST1, ?SYSLOG_PORT1, ?CMD1).

check_syslog_file(Tag, User, Pwd, Host, Port, Cmd) ->
    case ssh:connect(Host,
                     Port,
                     [{user, User},
                      {password, Pwd},
                      {silently_accept_hosts, true},
                      {user_interaction,false},
                      {quiet_mode, true},
                      {connect_timeout, 30000}],
                     30000) of
      {ok, ConnectionRef} ->
        ct:pal("---> ssh:connect {ok, ~p}", [ConnectionRef]),
        case ssh_connection:session_channel(ConnectionRef, infinity) of
          {ok, ChannelId} ->
             ct:pal("---> ssh_connection:session_channel {ok, ~p}", [ChannelId]),
             case ssh_connection:exec(ConnectionRef, ChannelId, Cmd ++ Tag, infinity) of
               success ->
                 ct:pal("---> ssh_connection:exec success", []),
                 case  do_recv_response(ConnectionRef, ChannelId, []) of
                   {ok, []} ->
                     ssh:close(ConnectionRef),
                     io:format("---> error, event not found, Tag: ~p", [Tag]),
                     {error, event_not_found};
                   {ok, Data} ->
                     ssh:close(ConnectionRef),
                     io:format("---> ok, Tag: ~p, Data: ~p", [Tag, Data]),
                     ok;
                   {timeout, Data} ->
                     ssh:close(ConnectionRef),
                     io:format("---> timeout, Tag: ~p, Data: ~p", [Tag, Data]), %% FIXME Match the Tag in Data
                     {error, timeout}
                 end;
               {error, Reason} ->
                 ct:pal("---> ssh_connection:exec {error, ~p}", [Reason]),
                 {error, Reason}
             end;
           {error, Reason} ->
             ct:pal("---> ssh_connection:session_channel {error, ~p}", [Reason]),
             {error, Reason}
        end;
      {error, Reason} ->
        ct:pal("---> ssh:connect {error, ~p}", [Reason]),
        {error, Reason}
    end.

do_recv_response(SSH, Chn, Data) ->
    receive
	{ssh_cm, SSH, {closed, Chn}} ->
	    ssh_connection:close(SSH, Chn),
	    ct:pal("Closed:~n~p ~p", [SSH, Chn]),
	    {ok, Data};
	{ssh_cm, SSH, {data, Chn,_, NewData}} ->
	    ssh_connection:adjust_window(SSH, Chn, size(NewData)),
	    ct:pal("Received data:~n~p", [binary_to_list(NewData)]),
            RetData = Data ++ binary_to_list(NewData),
            do_recv_response(SSH, Chn, RetData);
	{ssh_cm, SSH, {eof,Chn}} ->
            ct:pal("Received EOF:~n~p ~p", [SSH,Chn]),
	    {ok,Data};
	{ssh_cm, SSH, {exit_status,Chn,Status}} ->
            ct:pal("Received exit_status:~n~p ~p~n~p", [SSH,Chn,Status]),
	    do_recv_response(SSH, Chn, Data);
	Other ->
	    ct:pal("Unexpected msg:~n~p ~p~n~p", [SSH,Chn,Other]),
	    do_recv_response(SSH, Chn, Data)

    after 30000 ->
            {timeout, Data}
    end.







%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

configure_logPushTransfer(SysLogUdpHost) ->
    configure_logPushTransfer(SysLogUdpHost, "SecurityLog").

configure_logPushTransfer(SysLogUdpHost, Log) ->
    %% configure security event streaming using netconf
    A = {'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',[],
	   [{systemFunctionsId,[],["1"]},
	    {'LogM',[{xmlns,"urn:com:ericsson:ecim:LogM"}],
	     [{logMId,[],["1"]},
	      {'Log',[],
	       [{logId,[],[Log]},
		{'LogPushTransfer',[],
		 [{logPushTransferId,[],["1"]},
		  {uri,[],["syslog://" ++ SysLogUdpHost]},
		  {transferType,[],["STREAM"]}]}]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    ok = ct_netconfc:edit_config(nc1, running, A),
    ct_netconfc:close_session(nc1),
    ok.

delete_logPushTransfer(Log) ->
    A = {'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',[],
	   [{systemFunctionsId,[],["1"]},
	    {'LogM',[{xmlns,"urn:com:ericsson:ecim:LogM"}],
	     [{logMId,[],["1"]},
	      {'Log',[],
	       [{logId,[],[Log]},
		{'LogPushTransfer',[{'xmlns:nc',
				     "urn:ietf:params:xml:ns:netconf:"
				     "base:1.0"},
				    {'nc:operation',"delete"}],
		 [{logPushTransferId,[],["1"]}
		 ]}]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    ok = ct_netconfc:edit_config(nc1, running, A),
    ct_netconfc:close_session(nc1),
    ok.

change_featureState_for_rtsel(State) ->
    %% Activate/Deactivate RTSEL
    D = {'ManagedElement',
	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	 [{managedElementId,[],["1"]},
	  {'SystemFunctions',[],
	   [{systemFunctionsId,[],["1"]},
	    {'Lm',
	     [{xmlns,"urn:com:ericsson:ecim:LM"}],
	     [{lmId,[],["1"]},
	      {'FeatureState', [],
	       [{featureStateId,[],["CXC4040010"]},
		{featureState,[],[State]}]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    ok = ct_netconfc:edit_config(nc1, running, D),
    ct_netconfc:close_session(nc1).

check_logPushTransfer_operation_state() ->
    check_logPushTransfer_operation_state("SecurityLog").

check_logPushTransfer_operation_state(Log) ->
    LogPushTransfer = {'ManagedElement',
		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId,[],["1"]},
			{'SystemFunctions',
			 [{systemFunctionsId,[],["1"]},
			  {'LogM',
			   [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			   [{logMId,[],["1"]},
			    {'Log',
			     [{logId,[],[Log]},
			      {'LogPushTransfer',
			       [{logPushTransferId,[],["1"]},
				{operationalState,[],[]}
			       ]}]}]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    {ok, R} = ct_netconfc:get(nc1, LogPushTransfer),
    ct_netconfc:close_session(nc1),
    {ok,{operationalState,_,OpState}} = extract_element(operationalState, R),
    OpState.
