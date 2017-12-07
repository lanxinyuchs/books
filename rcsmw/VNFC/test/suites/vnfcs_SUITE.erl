%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile: vnfcs_SUITE.erl %
%%% @author etxaldu
%%% @copyright Ericsson AB 2016
%%% @version /main/R7A/1
%%%
%%% @doc == Test Suite for testing application logs.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(vnfcs_SUITE).

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% Rev      Date       Name    What
%%% -------- ---------- ------- ------------------------------
%%% R7A/1    2016-10-31 etxaldu Created
%%% git      2017-03-28 etxasta Redirected to real https server
%%% git      2017-04-05 etxarnu kill_erl updated
%%% git      2017-09-21 uabesvi new heart beat interface
%%% git      2017-10-19 uabesvi fixed httpc request
%%% ----------------------------------------------------------
%%%

-include_lib("common_test/include/ct.hrl").
-include("../../inc/vnfcI.hrl").
-include("../../esrc/vnfcs.hrl").

-export([
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         all/0,
         groups/0
]).

-export([
         vnfcs_hb/1,
         vnfcs_hb_change/1,
         vnfcs_hb_err_no_port/1,
         vnfcs_hb_err_port/1,
         vnfcs_hb_err_str/1,
         vnfcs_hb_err_url/1,
         vnfcs_hb_err_vid/1,
         vnfcs_hb_err_no_vid/1,
         vnfcs_app_heal_vnf/1,
         vnfcs_app_heal_failed_vnf/1,
         vnfcs_restart_vnf/1,
         vnfcs_man_heal_vnf/1,
         vnfcs_send_heal_err_vnf/1
        ]).

-export([init_vnfmserver/0]).

-define(DEFAULT_VNFPORT, "4443").
-define(VNFID1, "92ba5b8e-1111-11e6-ad11-fa163e13b2f0").
-define(VNFID2, "92ba5b8e-2222-11e6-ad11-fa163e13b2f0").

-define(HB_RI_USR,  "{\"restart_type\":\""++?RESTART_USR_STR++"\"}").
-define(HB_RI_APP,  "{\"restart_type\":\""++?RESTART_APP_STR++"\"}").
-define(HB_RI_OSMW, "{\"restart_type\":\""++?RESTART_OSMW_STR++"\"}").

-define(DUMMY_PORT, 9090).

-define(SPACE, 32).
-define(DOUBLE_QUOTE, 34).

%% --------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%% --------------------------------------------------------------------
suite() ->
    [{ct_hooks, [
                 {rct_rpc, rpc1},
                 {rct_netconf, nc1},
                 {rct_coli, {coli, [manual_connect]}}
                ]}].

%% @hidden
init_per_suite(Config) ->
    ssl:start(),
    application:start(inets),
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_group(_GroupName, Config) ->
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(TC, Config) when TC == vnfcs_hb;
                                   TC == vnfcs_hb_change ->
    case rct_rpc:call(rpc1, sysEnv, vrcs, [], 120000, noprint) of
        true ->
            rct_rpc:call(rpc1, mnesia, dirty_delete,
                         [vnfcs, vnfm_data], 120000, noprint),
            Config;
        _ ->
            {skip, vrcs_specific_testcase}
    end;
init_per_testcase(_TC, Config) ->
    case rct_rpc:call(rpc1, sysEnv, vrcs, [], 120000, noprint) of
        true ->
            Config;
        _ ->
            {skip, vrcs_specific_testcase}
    end.

%% @hidden
end_per_testcase(TC, _Config) when TC == vnfcs_app_heal_vnf,
                                   TC == vnfcs_app_heal_failed_vnf,
                                   TC == vnfcs_restart_vnf,
                                   TC == vnfcs_man_heal_vnf ->
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    [{default__group, [], all()},
     {ci_tests, [], ci_tests()}].

all() ->
    ci_tests() ++
    [ %% These tests can't be run by ci due to blocked ports
      vnfcs_app_heal_vnf,
      vnfcs_app_heal_failed_vnf,
      vnfcs_man_heal_vnf,
      vnfcs_send_heal_err_vnf
    ].

ci_tests() ->
    [
     vnfcs_hb,
     vnfcs_hb_change,
     vnfcs_hb_err_no_port,
     vnfcs_hb_err_port,
     vnfcs_hb_err_str,
     vnfcs_hb_err_url,
     vnfcs_hb_err_vid,
     vnfcs_hb_err_no_vid,
     vnfcs_restart_vnf
    ].


%% ===========================================================================
%% TEST CASES
%% ===========================================================================
%% --------------------------------------------------------------------
%% @doc
%% - Send successful heartbeat
%% @end
%% --------------------------------------------------------------------
vnfcs_hb(_Config) ->
    ct:pal("TestCase: vnfcs_hb"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    Resp2 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    ct:pal("TestCase: RESP ~p" , [Resp2]),
    {ok, Body} = validate_response(Resp2, 200),
    validate_resp_body(Body, [{"restart", undefined}]),
    check_node_data(VIp, VPort, ?VNFID1),
    stop_web_server(LSock),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Send heartbeat and change Server data
%% @end
%% --------------------------------------------------------------------
vnfcs_hb_change(_Config) ->
    ct:pal("TestCase: vnfcs_hb_change"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    %% Need to add a check towards the node to read the vnfms table
    %% Call the get IP/PORT/VNFID function on the node
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    stop_web_server(LSock),
    reboot_vnf(),
    {VIp2, VPort2, LSock2} = start_web_server(),
    Resp2 = send_hb(Ip,Port,VIp2,VPort2,?VNFID2),
    %% Need to add a check towards the node to read the vnfms table
    %% Call the get IP/PORT/VNFID function on the node and conmpare
    validate_response(Resp2, 200),
    check_node_data(VIp2, VPort2, ?VNFID2),
    stop_web_server(LSock2),
    reboot_vnf(),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Send heartbeat with missing Port data
%% @end
%% --------------------------------------------------------------------
vnfcs_hb_err_no_port(_Config) ->
    ct:pal("TestCase: vnfcs_hb_err_no_port"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp = send_hb_err_no_port(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp, 200),
    ok = validate_error_code(Resp, "\"status\":400"),
    stop_web_server(LSock),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Send heartbeat with faulty Port data (string)
%% @end
%% --------------------------------------------------------------------
vnfcs_hb_err_port(_Config) ->
    ct:pal("TestCase: vnfcs_hb_err_port"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp = send_hb_err_port(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp, 200),
    ok = validate_error_code(Resp, "\"status\":400"),
    stop_web_server(LSock),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Send heartbeat with faulty unterminated string
%% @end
%% --------------------------------------------------------------------
vnfcs_hb_err_str(_Config) ->
    ct:pal("TestCase: vnfcs_hb_err_str"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp = send_hb_err_str(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp, 200),
    ok = validate_error_code(Resp, "\"status\":400"),
    stop_web_server(LSock),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Send heartbeat with invalid url
%% @end
%% --------------------------------------------------------------------
vnfcs_hb_err_url(_Config) ->
    ct:pal("TestCase: vnfcs_hb_err_url"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp = send_hb_err_url(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp, 404),
    stop_web_server(LSock),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Send heartbeat with wrong vid
%% @end
%% --------------------------------------------------------------------
vnfcs_hb_err_vid(_Config) ->
    ct:pal("TestCase: vnfcs_hb_err_vid"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    %% Need to add a check towards the node to read the vnfms table
    %% Call the get IP/PORT/VNFID function on the node
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    Resp2 = send_hb_err_vid(Ip,Port,VIp,VPort,?VNFID2),
    validate_response(Resp2, 200),
    ok = validate_error_code(Resp2, "\"status\":400"),
    stop_web_server(LSock),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Send heartbeat with no "VNF_ID" on the node
%% @end
%% --------------------------------------------------------------------
vnfcs_hb_err_no_vid(_Config) ->
    ct:pal("TestCase: vnfcs_hb_err_no_vid"),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    %% Need to add a check towards the node to read the vnfms table
    %% Call the get IP/PORT/VNFID function on the node
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    reboot_vnf(),
    Resp2 = send_hb_err_vid(Ip,Port,VIp,VPort,?VNFID2),
    validate_response(Resp2, 200),
    ok = validate_error_code(Resp2, "\"status\":400"),
    stop_web_server(LSock),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Test a successful heal operation from the VNF
%%   . Start 'VNFM' webserver
%%   . Send heartbeat
%%   . Kill an APP to trigger a heal request
%%   . Receive heal request (Reply 200 OK)
%%   . Restart vnf
%%   . Send heartbeat,
%%   . Receive heartbeat reply with restart_ind with ?RESTART_APP
%%   . Close 'VNFM' webserver
%% @end
%% --------------------------------------------------------------------
vnfcs_app_heal_vnf(_Config) ->
    ct:pal("TestCase: vnfcs_vnf_heal"),
    reboot_vnf(),
    reset_restart_list(),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    kill_app("test_app", 3),
    ct:pal("Waiting for the Heal Request"),
    HealRes = wait_for_heal(),
    case HealRes of
        {ok, ?VNFID1} ->
            ok;
        {ok, _} ->
            ct:fail({error, unexpected_vnfid});
        Error ->
            ct:fail(Error)
    end,
    reboot_vnf(),
    Resp2 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    {ok, Body} = validate_response(Resp2, 200),
    validate_resp_body(Body, ?RESTART_APP),
    check_node_data(VIp, VPort, ?VNFID1),
    stop_web_server(LSock),
    reset_restart_list(),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Test heal operation fails, stop heartbeats from the VNF
%%   . Start 'VNFM' webserver
%%   . Send heartbeat
%%   . Kill an APP to trigger a heal request
%%   . VNF fails to send heal request
%%   . Receive heal request (No Reply)
%%   . VNF disables heartbeat replies
%%   . Send heartbeat (no reply)
%%   . Receive 503 reply
%%   . Restart VNF
%%   . Send heartbeat
%%   . Receive heartbeat reply 200 OK, with restart_ind with ?RESTART_APP
%%   . Close 'VNFM' webserver
%% @end
%% --------------------------------------------------------------------
vnfcs_app_heal_failed_vnf(_Config) ->
    ct:pal("TestCase: vnfcs_app_heal_failed_vnf"),
    reboot_vnf(),
    reset_restart_list(),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    kill_app("test_app", 3),
    stop_web_server(LSock),
    %% Check HB stopped in the vnfcs process
    ct:pal("Check heartbeat stopped in vnfcs process."),
    wait_for_hb_stopped(),
    Resp2 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp2, 503),
    reboot_vnf(),
    {VIp2, VPort2, LSock2} = start_web_server(),
    Resp3 = send_hb(Ip,Port,VIp2,VPort2,?VNFID1),
    {ok, Body} = validate_response(Resp3, 200),
    validate_resp_body(Body, ?RESTART_APP),
    check_node_data(VIp2, VPort2, ?VNFID1),
    stop_web_server(LSock2),
    reset_restart_list(),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Kill the VNF and test next HB reply includes a ?RESTART_OSMW
%%   . Start 'VNFM' webserver
%%   . Send heartbeat
%%   . Restart vnf
%%   . Send heartbeat,
%%   . Receive heartbeat reply with restart_ind with ?RESTART_OSMW
%%   . Close 'VNFM' webserver
%% @end
%% --------------------------------------------------------------------
vnfcs_restart_vnf(_Config) ->
    ct:pal("TestCase: vnfcs_restart_vnf"),
    reboot_vnf(),
    reset_restart_list(),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    kill_vnf(),
    Resp2 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    {ok, Body} = validate_response(Resp2, 200),
    validate_resp_body(Body, [{"restart_type", "RECOVERY"}]),
    check_node_data(VIp, VPort, ?VNFID1),
    stop_web_server(LSock),
    reset_restart_list(),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Manually restart the VNF and test next HB reply includes a ?RESTART_USR
%%   . Start 'VNFM' webserver
%%   . Send heartbeat
%%   . Manually restart the VNF (coli) to trigger a heal request
%%   . Receive heal request (Reply 200 OK)
%%   . Restart vnf
%%   . Send heartbeat,
%%   . Receive heartbeat reply with restart_ind with ?RESTART_USR
%%   . Close 'VNFM' webserver
%% @end
%% --------------------------------------------------------------------
vnfcs_man_heal_vnf(_Config) ->
    ct:pal("TestCase: vnfcs_man_heal_vnf"),
    reboot_vnf(),
    reset_restart_list(),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    manual_restart_vnf(),
    ct:pal("Waiting for the Heal Request"),
    HealRes = wait_for_heal(),
    case HealRes of
        {ok, ?VNFID1} ->
            ok;
        {ok, _} ->
            ct:fail({error, unexpected_vnfid});
        Error ->
            ct:fail(Error)
    end,
    reboot_vnf(),
    Resp2 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    {ok, Body} = validate_response(Resp2, 200),
    validate_resp_body(Body, ?RESTART_USR),
    check_node_data(VIp, VPort, ?VNFID1),
    stop_web_server(LSock),
    reset_restart_list(),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% - Manually restart the VNF and test next HB reply includes a ?RESTART_USR
%%   . Start 'VNFM' webserver
%%   . Send heartbeat
%%   . Call the send_heal function to send a heal to the webserver
%%   . Receive heal request (Reply 200 OK)
%%   . Call the send_heal function to try and send a 2nd heal
%%   . heal not sent and function gets {error, Reason}
%%   . Restart VNF
%%   . Send heartbeat,
%%   . Close 'VNFM' webserver
%% @end
%% --------------------------------------------------------------------
vnfcs_send_heal_err_vnf(_Config) ->
    ct:pal("TestCase: vnfcs_send_heal_err_vnf"),
    reboot_vnf(),
    reset_restart_list(),
    Site = get_config({test_nodes, 1}),
    Ip = get_config({Site, erl_dist_ip}),
    Port = ?DEFAULT_VNFPORT,
    {VIp, VPort, LSock} = start_web_server(),
    Resp1 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    validate_response(Resp1, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    manual_restart_vnf(),
    ct:pal("Waiting for the Heal Request"),
    HealRes = wait_for_heal(),
    case HealRes of
        {ok, ?VNFID1} ->
            ok;
        {ok, _} ->
            ct:fail({error, unexpected_vnfid});
        Error ->
            ct:fail(Error)
    end,

    case send_heal(test_send_heal_vnf) of
        {error, already_started} ->
            ct:pal("Additional heal request successully blocked"
                   " due to an ongoing heal request"),
            ok;
        AnythingElse ->
            ct:fail({unexpected, AnythingElse})
    end,
    %% Heal considered ongoing until the reboot
    reboot_vnf(),
    Resp2 = send_hb(Ip,Port,VIp,VPort,?VNFID1),
    {ok, _} = validate_response(Resp2, 200),
    check_node_data(VIp, VPort, ?VNFID1),
    stop_web_server(LSock),
    reset_restart_list(),
    ok.


%% ===========================================================================
%% VNFM SERVER PROCESS
%% ===========================================================================
%% --------------------------------------------------------------------
%% @doc
%% Start the vnfmServer web server process.<br/>
%% <br/>
%% @spec start_web_server() -> {Ip, Port, LSock} | {error, Reason}
%%  Ip, Port : string()
%%  LSock :gen_tcp listerner socket : term()
%% @end
%% --------------------------------------------------------------------
start_web_server() ->
    %% Web server to catch everything
    Pid = spawn_link(?MODULE, init_vnfmserver, []),
    register(vnfmServer, Pid),
    get_vnfmserver_data().


%% --------------------------------------------------------------------
%% @doc
%% Initialize the vnfmServer web server process.<br/>
%% <br/>
%% @spec init_vnfmserver(Socket)
%% @end
%% --------------------------------------------------------------------
init_vnfmserver() ->
    ct:pal("Start vnfmServer: ~p", [self()]),
    {ok, LSock} = gen_tcp:listen(0, [{active, false}]),
    Ip = local_ip_v4(),
    {ok, Port} = inet:port(LSock),
    put(vnfm_data, {Ip,integer_to_list(Port),LSock}),
    vnfmserver_loop(LSock).


%% --------------------------------------------------------------------
%% @doc
%% The main loop for the vnfmServer process.<br/>
%% <br/>
%% @spec vnfmserver_loop(Socket)
%% @end
%% --------------------------------------------------------------------
vnfmserver_loop(LSock) ->
    receive
        {Caller, get_data} ->
            bang(Caller, {get_data_res, get(vnfm_data)}),
            vnfmserver_loop(LSock);
        {Caller, wait_for_heal, Timeout} ->
            Res = vnfmserver_receiver(LSock, Timeout),
            bang(Caller, {wait_for_heal_res, Res}),
            vnfmserver_loop(LSock)  ;
        {Caller, stop} ->
            bang(Caller, {stop_res, ok}),
            ok
    end.


%% --------------------------------------------------------------------
%% @doc
%% Spawn a gen_tcp process to receive a request to the vnfmServer process
%% Wait for a specified time to receive a request from the temporary
%% handler.<br/>
%% <br/>
%% @spec vnfmserver_receiver(Socket, Timeout)
%% @end
%% --------------------------------------------------------------------
vnfmserver_receiver(LSock, Timeout) ->
    case gen_tcp:accept(LSock, Timeout) of
        {ok, Sock} ->
            Handler = spawn_link(fun() -> http_request(Sock) end),
            gen_tcp:controlling_process(Sock, Handler),
            receive
                {http_request_res, Result} ->
                    Result
            after 20000 ->
                    ct:fail("heal_timeout", [])
            end;
        Err ->
            Err
    end.


%% --------------------------------------------------------------------
%% @doc
%% Temporary gen_tcp controlling process used to receive requests to the
%% vnfmServer<br/>
%% <br/>
%% @spec http_request(Conn)
%% @end
%% --------------------------------------------------------------------
http_request(Conn) ->
    Res = rcv_data(gen_tcp:recv(Conn, 0)),
    bang(vnfmServer, {http_request_res, Res}),
    case Res of
        {ok, _} ->
            ct:pal("Send response to VNF: 200 OK"),
            gen_tcp:send(Conn, build_response("")),
            gen_tcp:close(Conn);
        Error ->
            gen_tcp:close(Conn),
            ct:fail("~p", [Error])
    end.


%% --------------------------------------------------------------------
%% @doc
%% Data recieved <br/>
%% <br/>
%% @spec rcv_data(Url) -> {ok, VnfId} | {error, Reason}
%% @end
%% --------------------------------------------------------------------
rcv_data({ok, Data}) ->
    case erlang:decode_packet(http, list_to_binary(Data),[]) of
        {ok, {http_request,'POST',
              {abs_path,URL},_},_} ->
            extract_vnfid(URL);
        _ ->
            {error, unexpected_request}
    end;
rcv_data(_) ->
    {error, unexpected_data}.


%% ===========================================================================
%% VNFM SERVER CALLING FUNCTIONS
%% These functions bang the vnfmServer and wait for a response.
%% ===========================================================================
%% --------------------------------------------------------------------
%% @doc
%% Message the vnfmServer that the calling process is waiting for a
%% notification of the next heal request from the VNF<br/>
%% <br/>
%% @spec wait_for_heal() -> {ok, VnfId} | {error, Reason}
%% @end
%% --------------------------------------------------------------------
wait_for_heal() ->
    bang(vnfmServer, {self(), wait_for_heal, 60000}),
    receive
        {wait_for_heal_res, Res} ->
            Res
    after 60000 ->
            {error, heal_not_received}
    end.


%% --------------------------------------------------------------------
%% @doc
%% Stop the vnfmServer web server process.<br/>
%% <br/>
%% @spec stop_web_server(LSock)
%%  LSock :gen_tcp listerner socket : term()
%% @end
%% --------------------------------------------------------------------
stop_web_server(LSock) ->
    bang(vnfmServer, {self(), stop}),
    receive
        {stop_res, ok} ->
            ok
    after 5000 ->
            timeout
    end,
    gen_tcp:close(LSock).


%% --------------------------------------------------------------------
%% @doc
%% Get the vnfm data from the vnfmServer process.<br/>
%% <br/>
%% @spec get_vnfmserver_data() -> {Ip, Port, LSock} | {error, Reason}
%%  Ip, Port : string()
%%  LSock :gen_tcp listerner socket : term()
%% @end
%% --------------------------------------------------------------------
get_vnfmserver_data() ->
    case whereis(vnfmServer) of
        undefined ->
            {error, no_proc};
        _Pid ->
            bang(vnfmServer, {self(), get_data}),
            receive
                {get_data_res, Res} ->
                    Res
            after 10000 ->
                    {error, timeout}
            end
    end.


%% ===========================================================================
%% HELPER FUNCTIONS
%% ===========================================================================
%% --------------------------------------------------------------------
%% @doc
%% Send a heal request to the VNFM <br/>
%% <br/>
%% @spec send_heal(Caller) -> ok | {error, Reason}
%% @end
%% --------------------------------------------------------------------
send_heal(Caller) ->
    rct_rpc:call(rpc1, vnfcI, send_heal, [Caller], 120000, noprint).


%% --------------------------------------------------------------------
%% @doc
%% Send a valid heart beat <br/>
%% <br/>
%% @spec send_hb(Ip, Port, VIp, VPort, VId)
%% @end
%% --------------------------------------------------------------------
send_hb(Ip, Port, VIp, VPort, VId) ->
    put_vnfid(VId),
    Url = "https://"++Ip++":"++Port++
        "/erisup/vnfcs/heartbeat",
    GoodReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnfm_http_port\":"++VPort++","
        "\"vnf_instance_id\":\""++VId++"\"}",
    ct:pal("Send Heartbeat Request~n"
           "  Url:  ~p~n"
           "  Body: ~p", [Url, GoodReq]),
    send_hb(Url, GoodReq).



%% --------------------------------------------------------------------
%% @doc
%% Send a valid heart beat but VId doesn't match the node VId <br/>
%% <br/>
%% @spec send_hb_err_vid(Ip, Port, VIp, VPort, VId)
%% @end
%% --------------------------------------------------------------------
send_hb_err_vid(Ip, Port, VIp, VPort, VId) ->
    Url = "https://"++Ip++":"++Port++
        "/erisup/vnfcs/heartbeat",
    GoodReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnfm_http_port\":"++VPort++","
        "\"vnf_instance_id\":\""++VId++"\"}",
    ct:pal("Send Heartbeat Request~n"
           "  Url:  ~p~n"
           "  Body: ~p", [Url, GoodReq]),
    send_hb(Url, GoodReq).


%% --------------------------------------------------------------------
%% @doc
%% Send a valid heart beat, then a faulty heart beat no port specifiedbr/>
%% <br/>
%% @spec send_hb_err_no_port(Ip, Port, VIp, VPort, VId)
%% @end
%% --------------------------------------------------------------------
send_hb_err_no_port(Ip, Port, VIp, VPort, VId) ->
    put_vnfid(VId),
    Url = "https://"++Ip++":"++Port++
        "/erisup/vnfcs/heartbeat",
    GoodReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnfm_http_port\":"++VPort++","
        "\"vnf_instance_id\":\""++VId++"\"}",
    Resp1 = send_hb(Url, GoodReq),
    validate_response(Resp1, 200),
    NoPortReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnf_instance_id\":\""++VId++"\"}",
    ct:pal("Send Heartbeat Request (Missing Port)~n"
           "  Url:  ~p~n"
           "  Body: ~p", [Url, NoPortReq]),
    send_hb(Url, NoPortReq).


%% --------------------------------------------------------------------
%% @doc
%% Send a valid heart beat, then a faulty heart beat with the port as a string
%% instead of an integer<br/>
%% <br/>
%% @spec send_hb_err_port(Ip, Port, VIp, VPort, VId)
%% @end
%% --------------------------------------------------------------------
send_hb_err_port(Ip, Port, VIp, VPort, VId) ->
    put_vnfid(VId),
    Url = "https://"++Ip++":"++Port++
        "/erisup/vnfcs/heartbeat",
    GoodReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnfm_http_port\":"++VPort++","
        "\"vnf_instance_id\":\""++VId++"\"}",
    Resp1 = send_hb(Url, GoodReq),
    validate_response(Resp1, 200),
    StrPortReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnfm_http_port\":\""++VPort++"\","
        "\"vnf_instance_id\":\""++VId++"\"}",
    ct:pal("Send Heartbeat Request (Port as String)~n"
           "  Url:  ~p~n"
           "  Body: ~p",
           [Url, StrPortReq]),
    send_hb(Url, StrPortReq).


%% --------------------------------------------------------------------
%% @doc
%% Send a valid heart beat, then a heart beat with an string error in the URL
%% <br/>
%% @spec send_hb_err_str(Ip,Port,VIp,VPort,VId)
%% @end
%% --------------------------------------------------------------------
send_hb_err_str(Ip,Port,VIp,VPort,VId) ->
    put_vnfid(VId),
    Url = "https://"++Ip++":"++Port++
        "/erisup/vnfcs/heartbeat",
    GoodReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnfm_http_port\":"++VPort++","
        "\"vnf_instance_id\":\""++VId++"\"}",
    Resp1 = send_hb(Url, GoodReq),
    validate_response(Resp1, 200),
    BadStrReq = "{\"vnfm_http_ip\":\""++VIp++","
        "\"vnfm_http_port\":"++VPort++","
        "\"vnf_instance_id\":\""++VId++"\"}",
    ct:pal("Send Heartbeat Request (Unterminated String)~n"
           "  Url:  ~p~n"
           "  Body: ~p",
           [Url, BadStrReq]),
    send_hb(Url, BadStrReq).


%% --------------------------------------------------------------------
%% @doc
%% Send a valid heart beat, then a heart beat with an invalid URL <br/>
%% <br/>
%% @spec send_hb_err_url(Ip, Port, VIp, VPort, VId)
%% @end
%% --------------------------------------------------------------------
send_hb_err_url(Ip, Port, VIp, VPort, VId) ->
    put_vnfid(VId),
    Url = "https://"++Ip++":"++Port++
        "/erisup/vnfcs/heartbeat",
    GoodReq = "{\"vnfm_http_ip\":\""++VIp++"\","
        "\"vnfm_http_port\":"++VPort++","
        "\"vnf_instance_id\":\""++VId++"\"}",
    Resp1 = send_hb(Url, GoodReq),
    validate_response(Resp1, 200),
    BadUrl = "https://"++Ip++":"++Port++
        "/erisup/vnfcs/junk",
    ct:pal("Send Heartbeat Request (Invalid Url)~n"
           "  Url:  ~p~n"
           "  Body: ~p",
           [BadUrl, GoodReq]),
    send_hb(BadUrl, GoodReq).


%% --------------------------------------------------------------------
%% @doc
%% Send a heart beat request to the VNF<br/>
%% <br/>
%% @spec send_hb(Url, Request)
%% @end
%% --------------------------------------------------------------------
send_hb(Url, Request) ->
    httpc:request(post, 
		  {Url,[],"application/json", Request},
		  [{timeout,15000}, 
		   {ssl,[{server_name_indication,"test_vc"}]}],
		  []).


%% --------------------------------------------------------------------
%% @doc
%% Validate the heart beat response and return code was as expected<br/>
%% <br/>
%% @spec validate_response(Response, ReturnCode) -> {ok, Body} | ct:fail
%% @end
%% --------------------------------------------------------------------
validate_response({ok, {{_, ReturnCode, _}, _, Body}}, ReturnCode) ->
    ct:pal("Received expected Response Code - ~p", [ReturnCode]),
    {ok, Body};
validate_response({ok, {ReturnCode, Body}}, ReturnCode) ->
    ct:pal("Received expected Response Code - ~p", [ReturnCode]),
    {ok, Body};
validate_response({ok, {{_, ReturnCode, _}, _, _}}, ReturnCode2) ->
    ct:pal("Unexpected Response Code~n"
           "  Received: ~p~n"
           "  Expected: ~p", [ReturnCode, ReturnCode2]),
    ct:fail("~p",[{unexp_resp_code,{ReturnCode, ReturnCode2}}]);
validate_response({ok, {ReturnCode, _}}, ReturnCode2) ->
    ct:pal("Unexpected Response Code~n"
           "  Received: ~p~n"
           "  Expected: ~p", [ReturnCode, ReturnCode2]),
    ct:fail("~p",[{unexp_resp_code,{ReturnCode, ReturnCode2}}]);
validate_response(Error, _) ->
    ct:fail("~p", [{unexpected_response, Error}]).


%% --------------------------------------------------------------------
%% @doc
%% Validate the restart indication in the heart beat response body was as
%% expected<br/>
%% <br/>
%% @spec validate_resp_body(JsonBody, Type) -> ok | ct:fail
%% @end
%% --------------------------------------------------------------------
validate_resp_body(Body, Expected) ->
   ct:pal("validate resp body - ~p  ~p ", [Body, Expected]),
   ct:pal("decode body - ~p  ", [decode_body(Body)]),
   check_restart(proplists:get_value("restart", decode_body(Body)),
                 proplists:get_value("restart", Expected)).

decode_body(Body) ->
    BL = string:strip(Body, left, ${), 
    BR = string:strip(BL, right, $}),
    BT = string:tokens(BR, ","),
    ct:pal("decode body - BT  ~p  ", [BT]),
    [{db_strip(K), db_strip(V)} || [K, V] <- [string:tokens(S, ":") || S <- BT]].

db_strip(Str) ->   
    Space  = string:strip(Str,   both, ?SPACE),
    _Fnutt = string:strip(Space, both, ?DOUBLE_QUOTE).


%% --------------------------------------------------------------------
%% @doc
%% Validate the error code
%% @spec validate_error_code(JsonBody, ErrorCode) -> ok | ct:fail
%% @end
%% --------------------------------------------------------------------
validate_error_code({ok, {_, _, Resp}}, ErrorCode) ->
   case string:str(Resp, ErrorCode) of
      0 -> ct:fail({error, {error_code_not_found, ErrorCode}});
      _ -> ok
   end.


%% --------------------------------------------------------------------
%% @doc
%% A Crude check on the restart indicator. Maybe redo when the 3pp Jsone
%% Json decode is in the system<br/>
%% <br/>
%% @spec check4restart(Json, Type) -> ok | {error, Reason}
%% @end
%% --------------------------------------------------------------------
check_restart(Restart, Restart) -> ok;
check_restart(Unexpected, Type) -> 
   ct:fail("~p", [{unexpected_restartInd, Unexpected, Type}]).


%% --------------------------------------------------------------------
%% @doc
%% Read a specific parameter from the test config<br/>
%% <br/>
%% @spec get_config(Par) -> Value term()
%% @end
%% --------------------------------------------------------------------
get_config(Par) ->
    case ct:get_config(Par) of
        undefined ->
            ct:log(lightred,
                   "~p ~p Could not read config parameter ~p for "
                   "pes_pei_proxy, Reason: undefined",
                   [?MODULE, get_config, Par]),
            throw({?MODULE, {{fail, {undefined, Par}}}});
        Val ->
            Val
    end.


%% --------------------------------------------------------------------
%% @doc
%% Read the local IP V4 address<br/>
%% <br/>
%% @spec local_ip_v4() -> LocalV4IpAddress
%%  LocalV4IpAddress : string() "Ip1.Ip2.Ip3,Ip4"
%% @end
%% --------------------------------------------------------------------
local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    inet_parse:ntoa(hd([Addr || {_, Opts} <- Addrs,
                                {addr, Addr={IP1,_,_,_}} <- Opts,
                                size(Addr) == 4, IP1 =/= 127])).


%% --------------------------------------------------------------------
%% @doc
%% A wrapper function to timer:sleep(Msec)<br/>
%% <br/>
%% @spec sleep(Msec)
%% @end
%% --------------------------------------------------------------------
sleep(Msec) ->
    timer:sleep(Msec).


%% --------------------------------------------------------------------
%% @doc
%% Check the Ip, Port, VnfId sent to the node was stored correctly<br/>
%% <br/>
%% @spec check_node_data(Ip, StrPort, Id) -> ok | ct:fail
%% @end
%% --------------------------------------------------------------------
check_node_data(Ip, StrPort, Id) ->
    Port = list_to_integer(StrPort),
    case rct_rpc:call(rpc1, mnesia, dirty_read,
                      [vnfcs, vnfm_data], 120000, noprint) of
        [#vnfcs{value = VnfmData}] ->
            case {VnfmData#vnfm_data.vnfm_http_ip,
                  VnfmData#vnfm_data.vnfm_http_port,
                  VnfmData#vnfm_data.vnf_instance_id} of
                {Ip, Port, Id} ->
                    ok;
                _ ->
                    ct:fail({error, vnfcs_node_data_error})
            end;
        _ ->
            ct:fail({error, vnfcs_node_data_missing})
    end.





%% --------------------------------------------------------------------
%% @doc
%% Send a message to the destination process<br/>
%% <br/>
%% @spec bang(Destination, Message)
%% @end
%% --------------------------------------------------------------------
bang(Pid, Message) ->
    Pid ! Message.


%% --------------------------------------------------------------------
%% @doc
%% Extract the VnfId from the received URL<br/>
%% <br/>
%% @spec extract_vnfid(Url) -> {ok, VnfId} | {error, Reason}
%% @end
%% --------------------------------------------------------------------
extract_vnfid(Url)->
    case filename:basename(Url) of
        "healVnf" ->
            VnfId = filename:basename(filename:dirname(Url)),
            ct:pal("healVnf received:~n  Url - ~s", [Url]),
            {ok, VnfId};
        _ ->
            ct:pal("Unexpected URL received:~n  Url - ~s", [Url]),
            {error, unexpected_url}
    end.


%% --------------------------------------------------------------------
%% @doc
%% Build a valid 200 OK response message<br/>
%% <br/>
%% @spec build_response(String) -> BinaryString
%% @end
%% --------------------------------------------------------------------
build_response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).


%% --------------------------------------------------------------------
%% @doc
%% Restart the VNF node by calling init reboot. <br/>
%% <br/>
%% @spec reboot_vnf()
%% @end
%% --------------------------------------------------------------------
reboot_vnf() ->
    ct:pal("Reboot the VNF..."),
    rct_rpc:call(rpc1, swmI, force_auto_backup, [], 120000, noprint),
    rct_rpc:call(rpc1, init, reboot, [], 120000, noprint),
    sleep(5000),
    ct:pal("Waiting for the VNF to start..."),
    wait_for_appl_started("test_app"),
    wait_for_netconf_started().


%% --------------------------------------------------------------------
%% @doc
%% Brutally kill the VNF node by killing the Erlang beam process. <br/>
%% <br/>
%% @spec kill_vnf()
%% @end
%% --------------------------------------------------------------------
kill_vnf() ->
    kill_erl(),
    wait_for_netconf_started().


%% --------------------------------------------------------------------
%% @doc
%% Reset the APPM restart list. <br/>
%% <br/>
%% @spec reset_restart_list() -> ok
%% @end
%% --------------------------------------------------------------------
reset_restart_list() ->
    ct:pal("Reset the Restart List in VNF"),
    rct_rpc:call(rpc1, appmServer, reset_restart_list, [], 120000, noprint),
    ok.


%% --------------------------------------------------------------------
%% @doc
%% Brutally kill the run_erl process. <br/>
%% Wait 15 sec after kill. <br/>
%% @spec kill_erl()
%% @end
%% --------------------------------------------------------------------
kill_erl() ->
    ct:pal("Killing the erlang node to simulate MW crash"),
    rct_rpc:call(rpc1, swmI, force_auto_backup, [], 120000, noprint),
    rct_rpc:call(rpc1, os, cmd, ["pkill -9 -f beam.smp"],
                 120000, noprint),
    sleep(15000).


%% --------------------------------------------------------------------
%% @doc
%% Kill an application X number of times. <br/>
%% Wait for the App to be restarted between kills. <br/>
%% @spec kill_app(App, X) -> ok
%% @end
%% --------------------------------------------------------------------
kill_app(App, X) ->
    ct:pal("Killing the ~s to trigger a Heal request",[App]),
    Pid = wait_for_appl_started(App),
    kill_app(App, [], Pid, X).
kill_app(_, _, _, 0) ->
    ok;
kill_app(App, Pid, Pid, Cnt) ->
    Pid2 = wait_for_appl_started(App),
    kill_app(App, Pid, Pid2, Cnt);
kill_app(App, _, Pid, Cnt) ->
    rct_rpc:call(rpc1, os, cmd, ["pkill "++App], 120000, noprint),
    kill_app(App, Pid, Pid, Cnt-1).


%% --------------------------------------------------------------------
%% @doc
%% Check for Netconf to be started. <br/>
%% Wait for ct_netconfc:get_config... returns  ok. <br/>
%% @spec wait_for_netconf_started() -> ok
%% @end
%% --------------------------------------------------------------------
wait_for_netconf_started() ->
    wait_for_netconf_started(60000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct_netconfc:close_session(nc1),
    ct:fail("VNF not started, timeout after restart.");

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
        {ok,_} ->
            {ok,_} = ct_netconfc:get_config(nc1,
                                            running,
                                            {'ManagedElement',
                                             [{xmlns,
                                               "urn:com:ericsson:ecim:ComTop"}],
                                             [{managedElementId,[],["1"]}]}),
            ok = ct_netconfc:close_session(nc1),
            ct:pal("VNF started"),
            ok;
        _  ->
            sleep(500),
            wait_for_netconf_started(Timeout - 500)
    end.



%% --------------------------------------------------------------------
%% @doc
%% Check for a specific Application to be started. <br/>
%% Wait for App is started. <br/>
%% @spec wait_for_appl_started(App) -> pid()
%% @end
%% --------------------------------------------------------------------
wait_for_appl_started(App) ->
    wait_for_appl_started(App, 60000).

wait_for_appl_started(App, Timeout) when Timeout < 500 ->
    ct:fail("~s not started.",[App]);

wait_for_appl_started(App, Timeout) ->
    case rct_rpc:call(rpc1, appmServer, get_apps, [], 500, noprint) of
        {badrpc, _} ->
            sleep(2000),
            wait_for_appl_started(App, Timeout - 2000);
        [] ->
            sleep(500),
            wait_for_appl_started(App, Timeout - 500);
        AppProplist ->
            case lists:keysearch(App, 1, AppProplist) of
                {value, {App, OsPid}} ->
                    OsPid;
                _ ->
                    sleep(500),
                    wait_for_appl_started(App, Timeout - 500)
            end
    end.



%% --------------------------------------------------------------------
%% @doc
%% Check that the heart beat replies have been disabled. <br/>
%% @spec wait_for_hb_stopped() -> ok
%% @end
%% --------------------------------------------------------------------
wait_for_hb_stopped() ->
    wait_for_hb_stopped(60000).

wait_for_hb_stopped(Timeout) when Timeout < 500 ->
    ct:fail("Heartbeat not stopped in vnfcs process.");

wait_for_hb_stopped(Timeout) ->
    case rct_rpc:call(rpc1, vnfcs, hb_state, [], 12000, noprint) of
        {ok, {disabled, _}, _} ->
            ok;
        {badrpc, _} ->
            sleep(2000),
            wait_for_hb_stopped(Timeout - 2000);
        _ ->
            sleep(500),
            wait_for_hb_stopped(Timeout - 500)
    end.


%% --------------------------------------------------------------------
%% @doc
%% Send Manual cold restart via coli. <br/>
%% @spec manual_restart_vnf()
%% @end
%% --------------------------------------------------------------------
manual_restart_vnf()->
    ok = rct_coli:connect(coli),
    ct:pal("Trigger cold restart via coli /board/restart -c"),
    {ok,_} = rct_coli:send(coli,"/board/restart -c"),
    rct_coli:disconnect(coli).


%% --------------------------------------------------------------------
%% @doc
%% Set the "VNF_ID" env on the node. <br/>
%% @spec put_vnfid(VnfId)
%% @end
%% --------------------------------------------------------------------
put_vnfid(VnfId) ->
    rct_rpc:call(rpc1, os, putenv, ["VNF_ID", VnfId], 120000, noprint),
    ok.

