%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sys_coli_SUITE.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R9A/R10A/R11A/1
%%% 
%%% @doc 
%%% @end


-module(sys_coli_SUITE).
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
%%% R2A/1      2014-05-19  etxpejn     Created
%%% R2A/3      2014-05-21  etxpejn     Temp removed check in coli_who
%%% R2A/4      2014-06-02  erarafo     Removed zero padding for 'day'
%%% R2A/5      2014-06-04  etxberb     changed /sys/date to /os/date and
%%%                                    /sys/uptime to /os/uptime.
%%% R2A/7      2014-09-24  erarafo     Using UTC when checking /os/date
%%% R3A/1      2015-03-05  etxpejn     Moved COLI commands from sys to sysm
%%% R3A/2      2015-07-10  etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-10-12  etxjotj     OTP18 adaption. Erlang:now removed
%%% R9A/1      2017-02-07  estjako     Added ftpes_group with coli export 
%%%                                    dump tests
%%% R9A/2      2017-02-17  estjako     Small change in TC US12.8-1a
%%% R9A/3      2017-03-13  estjako     Changed ct_pal to ct:log where needed
%%% R9A/4      2017-04-03  estjako     ftpes_group added in all()
%%% R9A/5      2017-04-04  estjako     Removed ftpes_group from all()- fails on 5G environment
%%% R11A/1     2017-10-18  uabesvi     restart ift in coli_free_up_disc_space to 
%%%                                    recreate ift_app.log
%%% ----------------------------------------------------------
%%% 

-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
     init_per_group/2,
     end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 coli_allocated_disc_space/1,
	 coli_cpuinfo/1,
	 coli_date/1,
	 coli_df/1,
	 coli_free_up_disc_space/1,
	 coli_meminfo/1,
         coli_uptime/1,
	 coli_who/1,
	 groups/0]).

-export([coli_export_dump_ftpes/1,
         coli_export_dump_ftpes_wrong_uri/1,
         coli_export_dump_ftpes_wrong_uri_host/1,
         coli_export_dump_ftpes_wrong_password/1]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc},
		  {rct_netconf,nc1},
          {rct_ssh,{ssh,[manual_connect]}},
                  {cth_conn_log,[]},
		 {rct_rs232, {console,[{connect_retries, 3}]}},                 
		 {rct_coli, {coli, [manual_connect]}},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
                                               ["appmServer:Program ID"]}}]}},
     ftpes_test_lib:ftpes_hook()
]}].


%% @hidden
init_per_suite(Config) ->
    %% ok = rct_coli:connect(coli),
    %% sftp by default
    [{ftp_protocol, sftp}|Config].

%% @hidden
end_per_suite(_Config) ->
    %% ok = rct_coli:disconnect(coli),
    ok.

init_per_group(ftpes_group, Config) ->
    rct_ftpes_client:open(),
    rct_ftpes_client:cd("syscoli"),
    NewConfig = lists:keyreplace(ftp_protocol, 1, Config, {ftp_protocol, ftpes}),
    FtpProtocol = ?config(ftp_protocol, NewConfig),
    
    %% enable ftp over tls on the node
    NewConfig2 = ftpes_test_lib:initialize_nc_tc(NewConfig),
    ftpes_test_lib:enable_ftpes_tls(NewConfig2),
    ftpes_test_lib:start_server(rpc),
    
    [{host, Host},{username, Username},{password, Password}] = 
        ftpes_test_lib:get_ftp_config(FtpProtocol),
    SysColiPath=ftpes_test_lib:get_ftp_file_path(FtpProtocol, NewConfig2),
    Url = atom_to_list(FtpProtocol) ++ "://"++Username++"@"++Host ++ SysColiPath,
	ct:pal("Reset restart list"),
    ok = rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    ct:pal("Executing ~w over ~p~n",[coli_export_dump_ftpes, FtpProtocol]),
   
    ct:pal("Make crash for test"),
    Cmd = "pkill -6 ift_app",
    ftpes_test_lib:exec_command(ssh, Cmd),
    
    timer:sleep(10000),
    
    LastDir = get_last_dumpdir(),
    [{url, Url}, {password, Password}, {sys_coli_path, SysColiPath}, {host, Host},
     {last_dumpdir, LastDir}| NewConfig2];

init_per_group(_Other, Config) ->
    Config.

end_per_group(ftpes_group, Config) ->
    Path = ?config(sys_coli_path, Config),
    LastDir = ?config(last_dumpdir, Config),
    DumpPath = "/rcs/dumps/pmd/",
    Cmd = " rm -rf " ++ DumpPath ++ LastDir,
    ftpes_test_lib:exec_command(ssh, Cmd),
    ok = rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    {ok, RawDirData} = rct_ftpes_client:list_dir(Path),
    DirData = [filename:basename(string:strip(File)) || File <- string:tokens(RawDirData, "\r\n")],
    ct:log("DirData: ~p~n", [DirData]),
    lists:foreach(fun(Dump) -> rct_ftpes_client:delete_file(filename:join(Path, Dump)) end, DirData),
    ftpes_test_lib:disable_ftpes_tls(),
    ftpes_test_lib:clean_nc_tc(Config),
    rct_ftpes_client:close(),
    Config;

end_per_group(_Other, Config) ->
    Config.

   
   

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @hidden
groups() ->
    AllGroup = all(),
    FtpesGroup = ftpes_group(),
    [
     {default__group, [], AllGroup},
     {ftpes_group, [], FtpesGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []},  
     {sbc__upgrade__all__1__group, [], []},
     {sdc__cover__sim__1__group, [], []},  
     {sdc__nocover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},  
     {sdc__qual__all__1__group, [], []}  
   ].
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------

all() ->
    [coli_allocated_disc_space,
     coli_cpuinfo,
     coli_date,
     coli_df,
     coli_free_up_disc_space,
     coli_meminfo,
     coli_uptime,
     coli_who].

ftpes_group() ->
    [coli_export_dump_ftpes,
     coli_export_dump_ftpes_wrong_uri,
     coli_export_dump_ftpes_wrong_uri_host,
     coli_export_dump_ftpes_wrong_password
     ].


%%--------------------------------------------------------------------
%% @doc Check that the COLI command allocated_disc_space returns an integer followed by %
%% 
%%
%% @spec coli_allocated_disc_space(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_allocated_disc_space(_Config) ->
    ok = rct_coli:connect(coli),
    {ok, Answer} = rct_coli:send(coli,"/sysm/discspace -p"),
    A = string:tokens(lists:flatten(Answer), "\r\n \t"),
    IntString = lists:subtract(lists:nth(1, A),"%"),
    true = is_integer(list_to_integer(IntString)),
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc Check that the COLI command cpuinfo returns a printout that contains processor
%% 
%%
%% @spec coli_cpuinfo(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_cpuinfo(_Config) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/os/cpuinfo"),
    %% ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n \t"), "Processor"), 
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n \t"), "processor"), 
    %% ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n \t"), "BogoMIPS"), 
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc Check that the COLI command date returns a date that corresponds to the ct date.
%% 
%%
%% @spec coli_date(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_date(_Config) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/os/date"),
    {Year, Day} = fn_date_utc(),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), Year), 
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), Day), 
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc Check that the COLI command df returns a printout that contains Use%
%% 
%%
%% @spec coli_df(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_df(_Config) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/os/df"),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "Use%"), 
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc Check that the COLI command free_up_disc_space returns a printout that contains Removing files
%% 
%%
%% @spec coli_free_up_disc_space(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_free_up_disc_space(_Config) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/sysm/discspace -c"),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "Removing"), 
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "files"), 
    ok = rct_coli:disconnect(coli),
    ok = rct_rpc:call(rpc, appmServer, start_lm, ["ift_app"], 10000),
    ok.

%%--------------------------------------------------------------------
%% @doc Check that the COLI command meminfo returns a printout that contains MemTotal: and MemFree:
%% 
%%
%% @spec coli_meminfo(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_meminfo(_Config) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/os/meminfo"),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "MemTotal:"), 
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "MemFree:"), 
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc Check that the COLI command uptime returns a printout that contains up
%% 
%%
%% @spec coli_uptime(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_uptime(_Config) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/os/uptime"),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "up"),
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc Check that the COLI command who returns the correct user 
%% 
%%
%% @spec coli_who(Config) -> ok
%% @end
%%--------------------------------------------------------------------
coli_who(_Config) ->
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/os/who"),
    Me = find_user(os:getenv("SIM_OR_TARGET")), 
    %% Temporary removed check
    %% ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), Me),
    check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), Me),
    ok = rct_coli:disconnect(coli).

% TC US12.8-1
coli_export_dump_ftpes(Config)  ->
    Url = ?config(url, Config),
    Pass = ?config(password, Config),
    LastDir = ?config(last_dumpdir, Config),
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/diagm/exportdump export "++ Url ++ " " ++ Pass ++ " " ++ LastDir),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "Uploading"), 

    ok = rct_coli:disconnect(coli).

% TC US12.8-1a
coli_export_dump_ftpes_wrong_uri(Config)  ->
    FtpProtocol = ?config(ftp_protocol, Config),
    Host = ?config(host, Config),
    LastDir = ?config(last_dumpdir, Config),
    Url = atom_to_list(FtpProtocol) ++ "://"++"labuser"++"@"++Host ++ "/WrongFolder/",
    Pass = ?config(password, Config),

    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/diagm/exportdump export "++ Url ++ " " ++ Pass ++ " " ++ LastDir),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "efnamena"), 
  
    ok = rct_coli:disconnect(coli).

% TC US12.8-1a
coli_export_dump_ftpes_wrong_uri_host(Config)  ->
    FtpProtocol = ?config(ftp_protocol, Config),
    Path = ?config(sys_coli_path, Config),
    NodeHost = ftpes_test_lib:get_node_ip(rpc, ipv4),
    Host = inet:ntoa(NodeHost),
    Url = atom_to_list(FtpProtocol) ++ "://"++"labuser"++"@"++ Host ++ Path,
    Pass = ?config(password, Config),
    LastDir = ?config(last_dumpdir, Config),
   
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/diagm/exportdump export "++ Url ++ " " ++ Pass ++ " " ++ LastDir),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "ehost"), 
  
    ok = rct_coli:disconnect(coli).

% TC US12.8-1b
coli_export_dump_ftpes_wrong_password(Config)  ->
    FtpProtocol = ?config(ftp_protocol, Config),
    Host = ?config(host, Config),
    Path = ?config(sys_coli_path, Config),
    LastDir = ?config(last_dumpdir, Config),
    Url = atom_to_list(FtpProtocol) ++ "://"++"labuser"++"@"++Host ++ Path ,
   
    ok = rct_coli:connect(coli),
    {ok,Answer} = rct_coli:send(coli,"/diagm/exportdump export "++ Url ++ " " ++ "wrongpass" ++ " " ++ LastDir),
    ok = check_coli_answer(string:tokens(lists:flatten(Answer), "\r\n "), "euser"), 
  
    ok = rct_coli:disconnect(coli).

%% Help functions

check_coli_answer(Candidates, WantedAnswer) ->
    ct:log("wanted answer: ~p, candidates: ~p", [WantedAnswer, Candidates]),
    case lists:member(WantedAnswer, Candidates) of
	true ->
	    ok;
	false ->
	    ct:log("Did not find wanted answer: ~p", [WantedAnswer]),
	    nok
    end.


find_user("target") ->
    ok = rct_rs232:login(console),
    {ok,[_, User, _]} = ct_telnet:cmd(console,"whoami"),
    User;
find_user("sim") ->
    [User] = string:tokens(os:cmd("whoami"),"\r\n "),
    User.


fn_date_utc() ->
    {{Y,_M,D}, _} = calendar:now_to_universal_time(os:timestamp()),
    {integer_to_list(Y), integer_to_list(D)}.


%% padzero(N) ->
%%     if N<10 -> [$0, N+$0];
%%        true -> integer_to_list(N)
%%     end.

% ftpes helper functions

 get_last_dumpdir() ->
    Res = ftpes_test_lib:exec_command(ssh, "ls -At  /rcs/dumps/pmd/"),
    ct:log("Res ~p", [Res]),
    Tokens = string:tokens(Res, "\n"),
    ct:log("Tokens ~p", [Tokens]),
    LastDir = lists:nth(1, Tokens),
    LastDir.
