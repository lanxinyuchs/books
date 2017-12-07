%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_logging.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/1

%%% @doc ==Common test hook for collecting logs==
%%% This module contains a common test hook for collecting data 
%%% written in logfiles from target or simulated nodes during a 
%%% testcase.
%%%
%%% If the SUT cannot be reached at the beginning of the testcase, 
%%% the whole logs will be fetched.
%%%
%%% The collected logfiles can be searched for certain strings.
%%%
%%% Hook formats:
%%% ```{rct_logging, [{N, Name, Logs, Filter}]}
%%%    {rct_logging, [{N, Name, Logs, Filter, Opts}]}'''
%%%
%%% There are short formats when running towards single node:
%%% ```{rct_logging, {Logs, Filter}}       expands to {rct_logging, [{1, log1, Logs, Filter, []}]}
%%%    {rct_logging, {Logs, Filter, Opts}} expands to {rct_logging, [{1, log1, Logs, Filter, Opts}]}'''
%%% 
%%% There are short formats when running towards clustered nodes:
%%% ```{rct_logging, [{Logs, Filter},{Logs,Filter}]}       expands to {rct_logging, [{1, log1, Logs, Filter, []}]}
%%%    {rct_logging, [{Logs, Filter, Opts},{Logs, Filter, Opts}]} expands to {rct_logging, [{1, log1, Logs, Filter, Opts},{2, log2, Logs, Filter, Opts}]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file when running on target.
%%%                                              Not used in simulated environment.
%%%    Name     = atom()                         Used as identifier
%%%    Logs     = all | upgrade | license |
%%%               oi_testapp | [Log]             all = all Log
%%%                                              upgrade = all Log + swmInternal
%%%                                              license = all Log + LicensingLog
%%%                                              oi_testapp =  Log + erlang logs for oi_testapp
%%%    Log      = erlang | alert | system | com | com_alarm | com_stdout | com_trace | kern | syslog | swmInternal | licensingLog
%%%    Filters  = [{Log, [FailStr], [SubtrStr]}] If number of occurences of FailStr - SubtrStr > 0, the testcase fails
%%%    FailStr  = string()                       NOTE: regexp characters MUST be \\
%%%    SubtrStr = string()                       NOTE: regexp characters MUST be \\
%%%    Opts     = get_all |                      Will fetch the whole logs. Useful when system is installed
%%%               {board_type, DU} |             future use. It will map Logs = all to the logs
%%%                                              for the board_type. Defaults to du.
%%%               sim_no_u_flag |                trick to collect logfiles by building a fixed path.
%%%                                              The need for this is when rcssim crashes in Jenking during startup.
%%%                                              Will only work for NON clustered nodes and rcssim started WITHOUT -u flag.
%%%               {connect_timeout, integer()} | (seconds) used for TLM where ssh connect is slow, default 5 seconds
%%%               {retries, integer()} |         default 5 , number of retries that will be made when ssh connect fails, default 5
%%%               {retry_delay, integer()}       (seconds) default 1 second, approximate time between ssh connect retries, 
%%%               {no_logs,[TC]                  Adaption for node CI, no logs fetching/checking will be done for TCs 
%%%    TC         atom()                         Testcases in suite.
%%%    DU = du'''
%%% For target environment it is required that config variables below is specified in stp.cfg file
%%% ```{ssh_lmt_ipv4, [{ssh, string()}, {port, integer()}, {user, string()}, {password, string()}]}'''
%%% Examples:<br/>
%%% Hook below will fetch all logs and search for "ERROR REPORT" or "CRASH REPORT" in erlang log. If found, the testcase will fail.
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}]}].'''
%%%
%%% Hook below will fetch all content of erlang and com logs (suitable when installing system). In erlang log, the number of occurences of "ERROR REPORT" or "CRASH REPORT" will be subtracted with the number of occurences of "linx kernel module not installed". If result is > 0 the testcase will fail.
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_logging, {[erlang,com], [{erlang,{["ERROR REPORT","CRASH REPORT"],["linx kernel module not installed"]}}], [get_all]}}]}].'''
%%%
%%% Hook below will fetch logs from two nodes. Note that [ and ] in SubtrStr are \\.
%%%```suite() -> 
%%%       [{ct_hooks, [{rct_logging, [{1, log1,[erlang, com], [{erlang,{["ERROR REPORT","CRASH REPORT"],["\\[{pid,<0.658.0>}\\]"]}}]},
%%%    				      {2, log2,[erlang, alarm, messages], [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}]}]}].'''
%%% @end


-module(rct_logging).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/1').
-date('2017-10-06').
-author('etxkols').
-include_lib("common_test/include/ct.hrl").
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-06-02 etxkols     Created
%%% R1A/2      2012-06-11 etxkols     Updated documentation
%%% R1A/3      2012-06-11 etxkols     added get_all/1
%%% R1A/4      2012-06-12 etxkols     added get_log when old data
%%%                                   could not be collected
%%% R1A/5      2012-06-13 etxkols     Changed printouts in table
%%% R1A/6      2012-08-07 etxkols     Changed lists to binary so save memory
%%% R1A/7      2012-08-14 etxkols     Removed io:formats
%%% R1A/8      2012-09-11 etxkols     Undocumented return value ssh_connection:exec/4
%%% R1A/9      2012-10-15 etxkols     Did not take case of when an exec times out
%%% R2A/1      2012-10-26 etxkols     Updated ssh:connect options
%%% R2A/2      2012-10-30 etxkols     More robust message handling, data in ets instead
%%%                                   of Config (problems at timetrap_timeout).
%%% R2A/3      2012-11-29 etxkols     Bug in lists:keysearch
%%% R2A/4      2012-11-29 etxkols     Edoc fix
%%% R2A/5      2012-12-06 etxkols     com_start.log does not exist in blackdelivery
%%% R2A/6      2013-01-08 etxkols     ssh exec and close could pick up others messages
%%% R2A/7      2013-01-23 etxkols     Undocumented return value from ssh_connection:exec/4
%%% R2A/10     2013-02-04 etxkols     Undocumented return value {closed,9} from ssh_connection:exec/4
%%% R2A/12     2013-02-15 etxkols     Quick fix for moved com logs
%%% R2A/13     2013-02-15 etxkols     More clever way of implementation of all/2
%%% R2A/14     2013-02-18 etxkols     Replaced search for com logs
%%% R2A/15     2013-04-10 etxkols     Added support to reuse Aliases
%%% R2A/16     2013-04-29 etxkols     Fix in get_all/1 for elamarc when (not) using external ct_hooks
%%% R2A/17     2013-06-02 etxjovp     added start_rcs.log in MiddlewareFiles
%%% R2A/18     2013-08-06 etxkols     Fixed problem with "ls -lt" for wrapping files
%%% R2A/19     2013-08-30 etxkols     start_rcs.log moved to /rcs/erlang/
%%% R2A/20     2013-08-30 etxkols     temporary remove check for start_rcs.log
%%% R2A/21     2013-09-04 etxkols     Added logfiles com.log, start_rcs.log & watchdogd.log
%%% R2A/22     2013-09-27 etxkols     Removed watchdogd.log until it exists on TCU
%%% R2A/23     2013-10-10 etxkols     Added retries on ssh
%%% R2A/24     2013-10-18 etxkols     Added 2 sec delay in post_end_per_testcase 
%%% R2A/25     2013-10-22 etxkols     Fault in ct:log calls
%%% R2A/26     2013-12-04 etxkols     New function dont_get_logs
%%% R2A/27     2013-12-06 etxkols     Added license option
%%% R2A/27     2013-12-11 etxkols     Clean away ct_attributes in post_end_per_suite
%%% R2A/30     2014-01-22 etxkols     Added oi_testapp option
%%% R2A/31     2014-03-03 etxkols     Added sim_no_u_flag option
%%% R2A/32     2014-03-26 etxkols     Faulty init/2 return value
%%% R2A/33     2014-04-23 etxkols     Fixes for TLM
%%% R2A/34     2014-05-12 etxkols     Longer polling times to sute node CI
%%% R2A/35     2014-05-16 etxkols     Added no_logs option
%%% R2A/36     2014-07-09 uabesvi     removed AlarmLog
%%% R2A/38     2014-12-16 etxkols     Added licensingLog to all
%%% R2A/39     2015-02-12 etxkols     Replaced post_end_per_suite/4 with terminate/1
%%% R4A/1      2015-05-20 etxpejn     Removed SystemLog
%%% R4A/2      2015-05-26 etxkols     Fixed clustering
%%% R4A/3      2015-05-27 etxkols     Fixed regular MP paths 
%%% R4A/4      2015-06-03 etxkols     temporary fix for check_after_install_SUITE which is still in R3
%%% R4A/5      2015-09-10 etxkols     Cluster fixes
%%% R4A/6      2015-09-16 etxkols     First regular change mp id from 2 to 3
%%% R4A/7      2015-12-02 etxkols     Added rabbe_logs
%%% R4A/8      2016-03-29 etxkols     Cloud
%%% R5A/1      2016-04-19 etxkols     Problem for RCSEE that os:cmd/1 returns [] + html tags changed in CT
%%% R5A/2      2016-08-30 etxkols     Added change_logging_parameters_in_this_tc/1 for RCSEE
%%% R7A/1      2016-11-11 etxkols     New prodno for DUMMY CXP
%%% R8A/2      2016-11-21 eolaand     Check for new /vnf dir in cloud env
%%% R9A/2      2017-01-31 etxkols     Refactoring for cloud.
%%% R9A/3      2017-02-01 etxkols     Removed comments
%%% R9A/4      2017-02-01 etxkols     Removed comments
%%% R9A/5      2017-02-02 etxkols     Collecting VNFM erlang logs
%%% R10A/1     2017-05-09 etxkols     Type = sd (service discovery) added
%%% R10A/2     2017-05-23 etxkols     SVNFM erlang logs moved from /var/log/frontend to /rcs
%%% R11A/1     2017-10-06 etxkols     Support rcs-sim in cloud env (GIT preparation)
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([get_all/1,
	 dont_get_logs/1,
	 change_logging_parameters_in_this_tc/1,
	 init/2,
	 pre_init_per_suite/3,
	 pre_init_per_testcase/3,
	 post_end_per_testcase/4,
	 terminate/1,
	 update_config/2,
	 ssh_connect/6,
	 ets_proc/1]).

-export([check_timestamp_and_format/1]).

%$RCT_TOP/test/bin/rct_run.sh -stp mystp -shell -pa /home/etxkols/RCS/logging
%rct_logging:pre_init_per_suite(apa,asdf,[{1, log1, username, [erlang, alarm, messages], [{erlang,["ERROR REPORT","CRASH REPORT"],[]}], [{board_type, du}]}]).
-define(WRITE_FILE_SLEEP, 2000). % Erlang seems to buffer output to erlang.log.x
-define(TIMEOUT_FETCH_FILE,  10000).
-define(SSH_CONNECT_TIMEOUT, 5).
-define(SSH_CONNECT_RETRIES_NO, 30).
-define(SSH_CONNECT_RETRY_DELAY,1).

-define(SIM_PATH, "rcs").
-define(RCS_SIM_PATH, "/rcs").
-define(TARGET_PATH, "/rcs").
-define(CLOUD_PATH, "/vnf").
-define(VNFM_PATH, "/rcs").
-define(SD_PATH, "/rcs").

-define(AllMiddlewareFiles,   [{erlang,      {"erlang/erlang.log",                                   num, [sim,du,vnf,rcs_sim,vnfm,sd]}},
			       {alert,       {"log/NotificationLog/NotificationLog",                 num, [sim,du,vnf,rcs_sim]}},
			       {com,         {"comte/com.log",                                       num, [sim,du,vnf,rcs_sim]}},
			       {com_alarm,   {"comte/com_alarm.log",                                 num, [sim,du,vnf,rcs_sim]}},
			       {com_start,   {"comte/com_start.log",                                 log, [sim,du,vnf,rcs_sim]}},
			       {start_rcs,   {"erlang/start_rcs.log",                                log, [sim,du,vnf,rcs_sim]}},
			       {com_stdout,  {"comte/com_stdout.log",                                log, [sim,du,vnf,rcs_sim]}},
			       {com_trace,   {"comte/com_trace.log",                                 num, [sim,du,vnf,rcs_sim]}}]).

-define(ExtraMiddlewareFiles, [{swmInternal, {"log/SwmInternal/SwmInternal",                         num, [sim,du,vnf,rcs_sim]}},
			       {licensingLog,{"log/LicensingLog/LicensingLog",                       num, [sim,du,vnf,rcs_sim]}},
			       {oi_testapp,  {"../tmp/oi_testapp/erlang.log",                        num, [sim,du,vnf,rcs_sim]}},
			       {rabbe_logs,  {"applicationlogs/*DUMMY*_CXP[0-9]*_[0-9]/ift_app.log", log, [sim,du,vnf,rcs_sim]}}]).

-define(AllLinuxFiles,        [{syslog,      {"/var/log/syslog",                                     log, [du,vnf,rcs_sim,vnfm,sd]}}]).

-record(?MODULE, {ct_logname,          % "mytest_log1_alarm.log"
		  name,                % log1
		  log,                 % alarm
		  fail_filter,         % ["ERROR REPORT","CRASH REPORT"]
		  success_filter,      % ["appm"]
		  node_logtype,        % num | log
		  node_logfile,        % "/rcs/log/AlarmLog/AlarmLog" | "/local/scratch/etxkols/RCS_ROOT/rcs/log/AlarmLog/AlarmLog" | sim_node_is_down
		  node_logdata = []}). % {ok, [{"/rcs/log/AlarmLog/AlarmLog.1",0,0}]} | {error, Reason}

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #--------------------------------------------------------- 

%%% @doc Complete logs for Name will be fetched at the end of testcase (same as Opts = get_all in hook, but is ordered within testcase).
get_all(log) -> %%% temporary fix for check_after_install_SUITE which is still in R3
    get_all(log1);
get_all(Name) -> 
    case ets:info(?MODULE) of % In case previous ct_hook has failed, Config is not reliable
	undefined -> % Fix for ELAMARC when using external ct_hooks
	    ok;
	_ ->
	    [ets:insert(rct_logging,{{Name2,F},option_get_all})||{{Name2,F},_}<-ets:tab2list(?MODULE), Name2 =:= Name],
	    ok
    end.

%%% @doc Logs for Name will NOT be fetched at the end of testcase (ordered within testcase).
dont_get_logs(Name) -> 
    case ets:info(?MODULE) of % In case previous ct_hook has failed, Config is not reliable
	undefined -> % Fix for ELAMARC when using external ct_hooks
	    ok;
	_ ->
	    ets:insert(rct_logging,{Name,dont_get_logs}),
	    ok
    end.

%%% @doc Changes log search parameters for a testcase.
%%% 
%%% ```LogParameters = [{N, Name, Logs, Filter, Opts}] |     Used for more complex changes and for clustered nodes, see description of paramaters above
%%%                    {filter, Filter}                      Short version when running towards one board, see description of Filter above
%%% 
%%%    Examples:
%%%    rct_logging:change_logging_parameters_in_this_tc({filter,[{erlang,{["ERROR REPORT","CRASH REPORT"],["blaha"]}}]}).
%%%    rct_logging:change_logging_parameters_in_this_tc([{1,log1,all,[{erlang,{["ERROR REPORT","CRASH REPORT"],["blaha"]}}],[]}]).'''
change_logging_parameters_in_this_tc(LogParameters) ->
    case ets:info(?MODULE) of % In case previous ct_hook has failed, Config is not reliable
	undefined -> % Fix for ELAMARC when using external ct_hooks
	    ok;
	_ ->
	    ets:insert(rct_logging,{logging_parameters_in_this_tc,LogParameters}),
	    ok
    end.

%%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%%% @doc Verifies that wanted logs are valid and checks that config variables exist for target.
pre_init_per_suite(_Suite,Config = {fail,_},States) ->
    {Config,States};
pre_init_per_suite(_Suite,Config = {skip,_},States) ->
    {Config,States};
pre_init_per_suite(_Suite,Config,States) when is_tuple(States) ->
    pre_init_per_suite(_Suite,Config,[States]);
pre_init_per_suite(_Suite,Config,States) ->
    case do_pre_init_per_suite(States,1,[],[]) of
	{ok,States2} ->
	    AliasToHooks = [{Name, {N, ?MODULE}}||{N,Name,_,_,_}<-States2],		
	    NewConfig = case proplists:get_value(alias_to_hooks,Config) of
			    undefined ->
				Config ++ [{alias_to_hooks,AliasToHooks}];
			    OldAliasToHooks ->
				lists:keyreplace(alias_to_hooks,1,Config,{alias_to_hooks,OldAliasToHooks ++ AliasToHooks})
			end,
	    {NewConfig, States2};
	Fail ->
	    {Fail, States}
    end.

fail_generic(FUNCTION_NAME, LINE, {Format, Vars}) ->
    Return = lists:flatten(io_lib:format(Format,Vars)),
    ct:pal(lightred,"~p:~p line ~p ~s",[?MODULE, FUNCTION_NAME, LINE, Return]),
    {fail, Return}.

%%% Verify that wanted logs are valid and check that config variables exist for target.
do_pre_init_per_suite([],_,States2,R) ->
    ok = rct_multi_node_cfg:require(?MODULE,R),
    {ok,States2};
do_pre_init_per_suite([{Logs, Filter}|T], Num, States2, R) ->
    do_pre_init_per_suite([{Num,list_to_atom("log"++integer_to_list(Num)), Logs, Filter, []}|T], Num, States2, R); 
do_pre_init_per_suite([{Logs, Filter, Opts}|T], Num, States2, R) ->
    do_pre_init_per_suite([{Num,list_to_atom("log"++integer_to_list(Num)), Logs, Filter, Opts}|T], Num, States2, R); 
do_pre_init_per_suite([State = {N, Name, Logs, _Filter, _Opts}|T], Num, States2, R) ->
    SimOrTarg = os:getenv("SIM_OR_TARGET"),
    case SimOrTarg of
	TargetOrCloud when TargetOrCloud == "target";
			   TargetOrCloud == "cloudish" ->
	    Board = atom_to_list(ct:get_config({test_nodes,N})),
	    BoardType = rct_multi_node_cfg:get_config(N,board_type),
	    case validate(N,BoardType,Logs) of
	    	{ok, ValidLogs} ->
	    	    case rct_multi_node_cfg:get_config(N,ssh_lmt_ipv4) of
	    		undefined ->
			    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[ssh_lmt_ipv4,Name,N]});
	    		[{ssh,IP},{port,Port},{user,User},{password,Pwd}] ->
			    NodeType = case rct_node_state:get_type(N) of
					   undefined -> "";
					   Type -> atom_to_list(Type)
				       end,
	    		    ColumnInfo = Board ++ "(" ++ IP ++ ") " ++ NodeType,
	    		    do_pre_init_per_suite(T, Num + 1, States2 ++ [State], R ++ [{Name,Board,IP,Port,User,Pwd,ValidLogs,ColumnInfo}]);
	    		_LoginData ->
			    fail_generic(?FUNCTION_NAME,?LINE,{"Could not find config parameter(s) ~p for ~p in node ~p",[{ssh_lmt_ipv4,[ssh,port,user,password]},Name,N]})
	    	    end;
	    	{error, Reason} ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Not supported logfiles ~p for ~p in node ~p",[Reason,Name,N]})
	    end;
	"sim" ->
	    {ok, HostName} = inet:gethostname(),          
	    Username = os:getenv("USER"),
	    ErlNode = list_to_atom(Username ++ "@" ++ HostName),
	    case validate(N,"sim",Logs) of
		{ok, ValidLogs} ->
		    case rpc:call(ErlNode, file, get_cwd, []) of
			{ok,CWD} ->
			    SIM_PATH = filename:dirname(filename:dirname(CWD)),
			    ValidLogs2 = [{Log,filename:join(SIM_PATH,Path),Type}||{Log,Path,Type}<-ValidLogs],
			    ColumnInfo = atom_to_list(ErlNode),
			    do_pre_init_per_suite(T, Num + 1, States2 ++ [State], R ++ [{ValidLogs2,ColumnInfo}]);
			Other ->
			    fail_generic(?FUNCTION_NAME,?LINE,{"Could not get cwd for ~p, Reason: ~p in node ~p",[ErlNode,Other,N]})
		    end;
		{error, Reason} ->
		    fail_generic(?FUNCTION_NAME,?LINE,{"Not supported logfiles ~p for ~p in node ~p",[Reason,Name,N]})
	    end
    end.

%%% Verify that wanted logs are valid
validate(N,BoardType,Logs) ->
    NodeType = 
	case rct_node_state:get_type(N) of
	    undefined -> % rct_node_state has NOT been used to declare the different node types in multinode suite.
		case BoardType of
		    "rcf" -> 
			case re:run(atom_to_list(ct:get_config({test_nodes,N})),"^sim") of
			    {match,_} -> rcs_sim;
			    nomatch   -> vnf
			end;
		    "sim" -> sim;
		    _     -> du
		end;
	    Type ->
		Type
	end,
    validate(NodeType, Logs).
		      				     
%%% rct_logging:validate(vnf,all).
%%% {ok,[{erlang,"/rcs/erlang/erlang.log",num},
%%%   {alert,"/vnf/log/NotificationLog/NotificationLog",num},
%%%   {com,"/vnf/comte/com.log",num},
%%%   {com_alarm,"/vnf/comte/com_alarm.log",num},
%%%   {com_start,"/vnf/comte/com_start.log",log},
%%%   {start_rcs,"/rcs/erlang/start_rcs.log",log},
%%%   {com_stdout,"/vnf/comte/com_stdout.log",log},
%%%   {com_trace,"/vnf/comte/com_trace.log",num},
%%%   {syslog,"/var/log/syslog",log}]}
validate(NodeType, Logs) ->
    LogList = expand(NodeType, Logs),
    AllLogs = ?AllMiddlewareFiles++?ExtraMiddlewareFiles++?AllLinuxFiles,
    AllowedLogs = [Log||{Log,{_,_,NodeList}}<-AllLogs, lists:member(NodeType,NodeList)],
    case [X||X<-LogList, lists:member(X,AllowedLogs)] of
	LogList ->
	    LogData = [{Log,Path,Type}||{Log,{Path,Type,_}}<-AllLogs, lists:member(Log,LogList)],
	    LogPaths = make_paths(NodeType,LogData,[]),
	    {ok, LogPaths};
	Other ->
	    {error, LogList -- Other}
    end.	    

%%% rct_logging:make_paths(vnf,[{erlang,"erlang/erlang.log",num},
%%% 			    {alert,"log/NotificationLog/NotificationLog",num},
%%% 			    {com,"comte/com.log",num},
%%% 			    {com_alarm,"comte/com_alarm.log",num},
%%% 			    {com_start,"comte/com_start.log",log},
%%% 			    {start_rcs,"erlang/start_rcs.log",log},
%%% 			    {com_stdout,"comte/com_stdout.log",log},
%%% 			    {com_trace,"comte/com_trace.log",num},
%%% 			    {syslog,"/var/log/syslog",log}],[]).
%%% [{erlang,"/rcs/erlang/erlang.log",num},
%%%  {alert,"/vnf/log/NotificationLog/NotificationLog",num},
%%%  {com,"/vnf/comte/com.log",num},
%%%  {com_alarm,"/vnf/comte/com_alarm.log",num},
%%%  {com_start,"/vnf/comte/com_start.log",log},
%%%  {start_rcs,"/rcs/erlang/start_rcs.log",log},
%%%  {com_stdout,"/vnf/comte/com_stdout.log",log},
%%%  {com_trace,"/vnf/comte/com_trace.log",num},
%%%  {syslog,"/var/log/syslog",log}].

make_paths(_NodeType,[],R) ->
    R;
make_paths(NodeType,[{Log,Path,Type}|T],R) ->
    case {NodeType,Path} of
	{vnf, "log/" ++ _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?CLOUD_PATH,   Path),Type}]);
	{vnf, "comte/" ++ _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?CLOUD_PATH,   Path),Type}]);
	{vnf, _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?TARGET_PATH,  Path),Type}]);
	{vnfm, _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?VNFM_PATH,    Path),Type}]);
	{sd, _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?SD_PATH,      Path),Type}]);
	{sim, _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?SIM_PATH,     Path),Type}]);
	{rcs_sim, _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?RCS_SIM_PATH, Path),Type}]);
	{du, _} ->
	    make_paths(NodeType,T,R ++ [{Log,filename:join(?TARGET_PATH,  Path),Type}])
    end.

expand(sim,all)            -> [K||{K,_}<-?AllMiddlewareFiles];
expand(sim,upgrade)        -> [K||{K,_}<-?AllMiddlewareFiles] ++ [swmInternal];
expand(sim,rabbe_logs)     -> [K||{K,_}<-?AllMiddlewareFiles] ++ [rabbe_logs];
expand(sim,license)        -> [K||{K,_}<-?AllMiddlewareFiles] ++ [licensingLog];
expand(sim,oi_testapp)     -> [K||{K,_}<-?AllMiddlewareFiles] ++ [oi_testapp];
expand(rcs_sim,all)        -> [K||{K,_}<-?AllMiddlewareFiles];
expand(rcs_sim,upgrade)    -> [K||{K,_}<-?AllMiddlewareFiles] ++ [swmInternal];
expand(rcs_sim,rabbe_logs) -> [K||{K,_}<-?AllMiddlewareFiles] ++ [rabbe_logs];
expand(rcs_sim,license)    -> [K||{K,_}<-?AllMiddlewareFiles] ++ [licensingLog];
expand(rcs_sim,oi_testapp) -> [K||{K,_}<-?AllMiddlewareFiles] ++ [oi_testapp];
expand(du,all)             -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles];
expand(du,upgrade)         -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [swmInternal];
expand(du,rabbe_logs)      -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [rabbe_logs];
expand(du,license)         -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [licensingLog];
expand(du,oi_testapp)      -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [oi_testapp];
expand(vnf,all)            -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles];
expand(vnf,upgrade)        -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [swmInternal];
expand(vnf,rabbe_logs)     -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [rabbe_logs];
expand(vnf,license)        -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [licensingLog];
expand(vnf,oi_testapp)     -> [K||{K,_}<-?AllMiddlewareFiles++?AllLinuxFiles] ++ [oi_testapp];
expand(vnfm,all)           -> [erlang, syslog];
expand(vnfm,upgrade)       -> [erlang, syslog] ++ [swmInternal];
expand(vnfm,rabbe_logs)    -> [erlang, syslog] ++ [rabbe_logs];
expand(vnfm,license)       -> [erlang, syslog] ++ [licensingLog];
expand(vnfm,oi_testapp)    -> [erlang, syslog] ++ [oi_testapp];
expand(sd,all)             -> [erlang, syslog];
expand(sd,upgrade)         -> [erlang, syslog] ++ [swmInternal];
expand(sd,rabbe_logs)      -> [erlang, syslog] ++ [rabbe_logs];
expand(sd,license)         -> [erlang, syslog] ++ [licensingLog];
expand(sd,oi_testapp)      -> [erlang, syslog] ++ [oi_testapp];
expand(_,Y) when is_atom(Y) -> [Y];
expand(_,Y)  -> Y.

%%% @doc Store logfile data in etstable, i.e to be used at post_end_per_testcase when logs are fetched.
pre_init_per_testcase(_TC,Config = {fail,_},States) -> {Config,States};
pre_init_per_testcase(_TC,Config = {skip,_},States) -> {Config,States};
pre_init_per_testcase(TC,Config,States) ->
    case lists:keysearch(priv_dir, 1, Config) of
	false ->
	    {{fail, no_priv_dir}, States};
	{value,{priv_dir, PrivDir}} ->
	    case whereis(?MODULE) of
		Pid when is_pid(Pid) -> 
		    ?MODULE ! {self(),stop},
		    receive
			stopped -> ok
		    end;
		_ -> 
		    ok
	    end,
	    register(?MODULE, spawn(?MODULE, ets_proc, [self()])),
	    receive
		ok -> 
		    do_pre_init_per_testcase(TC,Config,States),
		    ets:insert(?MODULE,{priv_dir,PrivDir}),
		    {Config, States}
	    after 1000 ->
		    {{fail, logging_ets_table}, States}
	    end
    end.

%%% Store data in ets table ?MODULE, i.e. logfile names in common test and logfile names and number of lines on node.
do_pre_init_per_testcase(_TC,_Config,[]) ->
    ok;
do_pre_init_per_testcase(TC,Config,[{N, Name, _Logs, Filter, Opts}|T]) ->
    NoGetLogs = 
	case rct_node_state:get_state(N) of
	    down ->
		ct:log("Node ~p in node ~p is marked as down, will not attempt to start collecting logfiles",[Name, N]),
		{_,_,_,_,_,LogData} = build_data(TC, N, Name, Filter, Opts),
		[ets:insert(?MODULE,{{D#?MODULE.name,D#?MODULE.ct_logname},
				     {error, target_node_is_down}})||D<-LogData],
		true;
	    _ -> % This is for RCSEE
		case proplists:get_value(no_logs,Opts) of
		    undefined ->
			false;
		    TCList ->
			ct:log("Searching and collection of logfiles switched off for user: ~p in node ~p for testcases ~p",[Name,N,TCList]),
			lists:member(TC,TCList) 
		end
	end,
    case NoGetLogs of
	true ->
	    do_pre_init_per_testcase(TC,Config,T);
	false ->
	    ConnectTimeout = proplists:get_value(connect_timeout,Opts,?SSH_CONNECT_TIMEOUT),
	    Retries = proplists:get_value(retries,Opts,?SSH_CONNECT_RETRIES_NO),
	    RetryDelay = proplists:get_value(retry_delay,Opts,?SSH_CONNECT_RETRY_DELAY),
	    {IP, Port, User, Passwd, _ColumnInfo, Data} = build_data(TC, N, Name, Filter, Opts),
	    Data3 = case lists:member(get_all,Opts) of
			true ->
			    [D#?MODULE{node_logdata=option_get_all}||D<-Data];
			false ->
			    case ssh_connect(IP, Port, User, Passwd, ConnectTimeout, Retries, RetryDelay) of
				{ok, Pid} ->
				    Data2 = get_line_counts(Pid, yellow, Data, []),
				    ssh_close(Pid),
				    Data2;
				{error, _Reason} -> 
				    [D#?MODULE{node_logdata={error, target_node_is_down}}||D<-Data]
			    end
		    end,
	    [ets:insert(?MODULE,{{D#?MODULE.name,D#?MODULE.ct_logname},D#?MODULE.node_logdata})||D<-Data3],
	    do_pre_init_per_testcase(TC,Config,T)
    end.
    

%%% @doc Fetches the logfiles and searches for strings, also destroys etstable created in pre_init_per_testcase.
post_end_per_testcase(TC,Config,Return,States) ->
    States2 = case ets:lookup(rct_logging,logging_parameters_in_this_tc) of
		  [] -> 
		      States;
		  [{logging_parameters_in_this_tc,{filter,Filter}}] -> % Filter ex [{erlang,{["ERROR REPORT","CRASH REPORT"],["blaha"]}}]
		      [{N, Name, Logs, Filter, Opts}||{N, Name, Logs, _, Opts}<-States];
		  [{logging_parameters_in_this_tc,LogParameters}] -> % LogParameters [{1,log1,all,[{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}],[]}]
		      ct:pal("Changing rct_logging parameters for this test case to ~p",[LogParameters]),
		      LogParameters
	      end,
    timer:sleep(?WRITE_FILE_SLEEP),
    case ets:info(?MODULE) of % In case previous ct_hook has failed, Config is not reliable
	undefined ->
	    {{fail, no_priv_dir}, States2};
	_ ->
	    case ets:lookup(?MODULE, priv_dir) of % In case previous ct_hook has failed, Config is not reliable
		[] ->
		    {{fail, no_priv_dir}, States2};
		[{priv_dir, TCLogPath}] ->
		    R = do_post_end_per_testcase(TC, TCLogPath,Config,States2,[]),
		    R2 = print_logs(R),
		    case whereis(?MODULE) of
			Pid when is_pid(Pid) -> 
			    ?MODULE ! {self(),stop},
			    receive
				stopped -> ok
			    end;
			_ -> 
			    ok
		    end,
		    case R2 of
			{ok,_} -> {Return, States}; % LogParameters ONLY used in one testcase
			_ -> {{fail, logging_fault}, States2}
		    end
	    end
    end.
		  
do_post_end_per_testcase(_TC, _TCLogPath,_Config,[],R) ->
    R;
do_post_end_per_testcase(TC, TCLogPath,Config,[{N, Name, _Logs, Filter, Opts}|T],R) ->   
    NoGetLogs = 
	case rct_node_state:get_state(N) of
	    down ->
		ct:log("Node ~p is marked as down, will not attempt to collect logfiles",[Name]),
		true;
	    _ -> % This is for RCSEE
		case proplists:get_value(no_logs,Opts) of
		    undefined ->
			false;
		    TCList ->
			ct:log("Searching and collection of logfiles switched off for user: ~p for testcases ~p",[Name,TCList]),
			lists:member(TC,TCList) 
		end
	    end,
    case NoGetLogs of
	true ->
	    do_post_end_per_testcase(TC, TCLogPath,Config,T,R);
	false ->
	    ConnectTimeout = proplists:get_value(connect_timeout,Opts,?SSH_CONNECT_TIMEOUT),
	    Retries = proplists:get_value(retries,Opts,?SSH_CONNECT_RETRIES_NO),
	    RetryDelay = proplists:get_value(retry_delay,Opts,?SSH_CONNECT_RETRY_DELAY),
	    Result = case ets:lookup(?MODULE,Name) of
			 [{Name,dont_get_logs}] ->
			     [];
			 [] ->		     
			     {IP, Port, User, Passwd, ColumnInfo, Data} = build_data(TC, N, Name, Filter, Opts),
			     Data4 = case ssh_connect(IP, Port, User, Passwd, ConnectTimeout, Retries, RetryDelay) of
					 {ok, Pid} ->
					     Data2 = get_line_counts(Pid, lightred, Data, []),
					     Data3 = get_logs(Pid, TCLogPath, Data2, []),			     
					     ssh_close(Pid),
					     Data3;
					 {error, Reason} -> 
					     ct:log(lightred,"~p: ~p ~p Could not connect to ~p ~p ~p ~p, Reason: ~p",[Name, ?MODULE, find_log_path, IP, Port, User, Passwd, {error, Reason}]),
					     [D#?MODULE{node_logdata={error, target_node_is_down}}||D<-Data]
				     end,
			     [{ColumnInfo,[{D#?MODULE.log, 
					    D#?MODULE.ct_logname, 
					    D#?MODULE.node_logdata, 
					    D#?MODULE.fail_filter,  
					    D#?MODULE.success_filter}||D<-Data4]}]
		     end,
	    do_post_end_per_testcase(TC, TCLogPath,Config,T,R++Result)
    end.

%% @hidden
%% Clean away items in ct_attributes
terminate(CthState) ->
    do_terminate(CthState),
    rct_multi_node_cfg:remove_config(?MODULE),
    ok.

do_terminate([]) ->
    ok;
do_terminate({_Logs, _Filter}) ->
    do_terminate([log]);
do_terminate({_Logs, _Filter, _Opts}) ->
    do_terminate([log]);
do_terminate([{_N, Name, _Sname, _Logs, _Filter}|T]) ->
    do_terminate([Name|T]);
do_terminate([{_N, Name, _Sname, _Logs, _Filter, _Opts}|T]) ->
    do_terminate([Name|T]);
do_terminate([Name|T]) ->
    rct_multi_node_cfg:remove_config(make_name_module(Name)),
    do_terminate(T).

%%% rct_logging:build_data(test1,1,log1,[{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}],[])
%%% {"10.67.239.232",22,
%%%  "root","root",
%%%  "du1(rcf_etxkols)",
%%%  [{rct_logging,"test1_log1_erlang.log",log1,erlang,["ERROR REPORT","CRASH REPORT"],[],num,"/rcs/erlang/erlang.log",[]},
%%%   {rct_logging,"test1_log1_alert.log",log1,alert,[],[],num,"/vnf/log/NotificationLog/NotificationLog",[]},
%%%   {rct_logging,"test1_log1_com.log",log1,com,[],[],num,"/vnf/comte/com.log",[]},
%%%   {rct_logging,"test1_log1_com_alarm.log",log1,com_alarm,[],[],num,"/vnf/comte/com_alarm.log",[]},
%%%   {rct_logging,"test1_log1_com_start.log",log1,com_start,[],[],log,"/vnf/comte/com_start.log",[]},
%%%   {rct_logging,"test1_log1_start_rcs.log",log1,start_rcs,[],[],log,"/rcs/erlang/start_rcs.log",[]},
%%%   {rct_logging,"test1_log1_com_stdout.log",log1,com_stdout,[],[],log,"/vnf/comte/com_stdout.log",[]},
%%%   {rct_logging,"test1_log1_com_trace.log",log1,com_trace,[],[],num,"/vnf/comte/com_trace.log",[]},
%%%   {rct_logging,"test1_log1_licensingLog.log",log1,licensingLog,[],[],num,"/vnf/log/LicensingLog/LicensingLog",[]},
%%%   {rct_logging,"test1_log1_syslog.log",log1,syslog,[],[],log,"/var/log/syslog",[]}]}
build_data(TC, N, Name, Filter, Opts) ->
    BoardType   = proplists:get_value(board_type,Opts,du),
    TestNodeType = os:getenv("SIM_OR_TARGET"),
    {_Name,_Board,IP,Port,User,Pwd,ValidLogs, ColumnInfo} = 
	case TestNodeType of
	    "sim" ->
		{SimValidLogs,SimColumnInfo} = lists:nth(N,ct:get_config(?MODULE)),
		{sim,sim,sim,sim,sim,sim,SimValidLogs,SimColumnInfo};
	    "cloudish" ->
		lists:nth(N,ct:get_config(?MODULE));
	    "target" ->
		lists:nth(N,ct:get_config(?MODULE))
	end,
    %% VNF = is_vnf_node(TestNodeType, {IP,Port,User,Pwd}, Opts),
    Data = build_records(ValidLogs, TC, Name, BoardType, Filter, []),
    {IP, Port, User, Pwd, ColumnInfo, Data}.

%%% rct_logging:build_records([erlang,alert,com,com_alarm,com_start,start_rcs,com_stdout,com_trace,licensingLog,syslog],test1,log1,true,du,"/rcs",[{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}],[])
%%% [{rct_logging,"test1_log1_erlang.log",log1,erlang,["ERROR REPORT","CRASH REPORT"],[],num,"/rcs/erlang/erlang.log",[]},
%%%  {rct_logging,"test1_log1_alert.log",log1,alert,[],[],num,"/vnf/log/NotificationLog/NotificationLog",[]},
%%%  {rct_logging,"test1_log1_com.log",log1,com,[],[],num,"/vnf/comte/com.log",[]},
%%%  {rct_logging,"test1_log1_com_alarm.log",log1,com_alarm,[],[],num,"/vnf/comte/com_alarm.log",[]},
%%%  {rct_logging,"test1_log1_com_start.log",log1,com_start,[],[],log,"/vnf/comte/com_start.log",[]},
%%%  {rct_logging,"test1_log1_start_rcs.log",log1,start_rcs,[],[],log,"/rcs/erlang/start_rcs.log",[]},
%%%  {rct_logging,"test1_log1_com_stdout.log",log1,com_stdout,[],[],log,"/vnf/comte/com_stdout.log",[]},
%%%  {rct_logging,"test1_log1_com_trace.log",log1,com_trace,[],[],num,"/vnf/comte/com_trace.log",[]},
%%%  {rct_logging,"test1_log1_licensingLog.log",log1,licensingLog,[],[],num,"/vnf/log/LicensingLog/LicensingLog",[]},
%%%  {rct_logging,"test1_log1_syslog.log",log1,syslog,[],[],log,"/var/log/syslog",[]}]
build_records([], _, _, _, _, R) ->
    lists:reverse(R);
build_records([{Log,NodeLogFile,Type}|T], TC, Name, BoardType, Filter, R) ->
    {Fail, Succ} = proplists:get_value(Log, Filter, {[],[]}),
    Data = #?MODULE{ct_logname     = atom_to_list(TC) ++ "_" ++ atom_to_list(Name) ++ "_" ++ atom_to_list(Log) ++ ".log",
		    name           = Name,
		    log            = Log,
		    fail_filter    = Fail,
		    success_filter = Succ,
		    node_logtype   = Type,
		    node_logfile   = NodeLogFile},
    build_records(T, TC, Name, BoardType, Filter, [Data|R]).


%%% Updates record with line counts
get_line_counts(_, _, [],R) ->
    R;
get_line_counts(Pid, Color, [Data|T], R) ->
    Count = get_line_count(Pid, Color, Data),
    get_line_counts(Pid, Color, T, R++[Data#?MODULE{node_logdata=Count}]).

%%% Gets logfiles and number of lines in them
get_line_count(sim, _, #?MODULE{node_logfile=LogPath}) when LogPath == sim_node_is_down ->
    {error, sim_node_is_down};
get_line_count(Pid, Color, #?MODULE{name=Name, node_logfile=LogPath, node_logtype=Type} = Rec) ->
    Extension = case Type of 
		    num -> ".*";
		    log -> ""
		end,
    LogName2 = filename:basename(LogPath),
    case exec(Pid, "ls -lt "++LogPath++Extension++" | grep -v idx | grep -v siz", 5000) of
	{ok, R} ->
	    ParsedLogs = [string:tokens(X," ")||X<-string:tokens(binary_to_list(R),"\r\n")],
	    LSData = [{LogName,Month,Day,Time,list_to_integer(Size)}||[_,_,_,_,Size,Month,Day,Time,LogName]<-ParsedLogs, string:str(LogName,LogName2)/=0],
%	    io:format("~p",[LSData]),
	    case check_timestamp_and_format(LSData) of
		[{LogName,Size}|Tail] ->
		    case exec(Pid, "wc -l "++LogName, 5000) of
			{ok, R2} ->
			    case [list_to_integer(Line)||[Line,_]<-[string:tokens(X," ")||X<-string:tokens(binary_to_list(R2),"\r\n")]] of
				[] ->
				    ct:log(Color,"~p: ~p ~p ~s line count NOT found",[Name, ?MODULE, get_line_count, LogName]),
				    {error, {LogName,R2}};			    
				    
				[Line] ->
				    {ok,[{LogName,Size,Line}]++Tail}
			    end;
			{error, Reason} ->
			    ct:log(Color,"~p: ~p ~p wc -l ~s NOK, Reason: ~p",[Name, ?MODULE, get_line_count, LogName, {error, Reason}]),
			    {error, Reason}
		    end;
		_ ->
		    ct:log(Color,"~p: ~p ~p ~s NOT found",[Name, ?MODULE, get_line_count, LogName2]),
		    {error, {LogPath,R}}
	    end;
	{error, Reason} ->
	    case LogPath of
		"/rcs/log/" ++ RestLogPath ->
		    get_line_count(Pid, Color, Rec#?MODULE{node_logfile = "/vnf//log/" ++ RestLogPath});
		_ ->
		    ct:log(Color,"~p: ~p ~p ls -lt ~s ~s NOK, Reason: ~p",[Name, ?MODULE, get_line_count, LogPath, Extension, {error, Reason}]),
		    {error, Reason}
	    end
    end.

%% This is a bloody hell. If files are wrapping quickly (i.e. same timestamp), 
%% "ls -lt" may print an older file before the newest.
%% This function checks the two latest files (counting on that succeding files 
%% have different timestamps). If they have the same timestamp, the file with
%% the smallest size will be considered to be the latest one.
check_timestamp_and_format([]) -> % No file
    [];
check_timestamp_and_format([{LogName,_,_,_,Size}]) -> % Only one file
    [{LogName,Size}];
check_timestamp_and_format([{LogName,Month,Day,Time,Size},{LogName2,Month,Day,Time,Size2}|Tail]) ->
%    ct:pal("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),
    Tail3 = [{LogName3,Size3}||{LogName3,_,_,_,Size3}<-Tail],
    case Size > Size2 of
	true ->
	    [{LogName2,Size2},{LogName,Size}|Tail3];
	false ->
	    [{LogName,Size},{LogName2,Size2}|Tail3]
    end;
check_timestamp_and_format(LogData) ->
    [{LogName,Size}||{LogName,_,_,_,Size}<-LogData].

	    
%%% @hidden
ets_proc(From) -> 
    ets:new(?MODULE,[ordered_set,public,named_table]),
    From ! ok,
    receive
        {Pid,stop} ->
            ets:delete(?MODULE),
	    Pid ! stopped
    end.

get_logs(_Pid, _TCLogPath, [], R) ->
    R;
get_logs(Pid, TCLogPath, [#?MODULE{name=Name, ct_logname=LogName, node_logtype=Type, node_logdata=LogDataN} = DataN|T], R) ->
    [{{Name,LogName},LogDataO}] = ets:lookup(?MODULE,{Name,LogName}),
    FileName = filename:join(TCLogPath,LogName),
%    io:format("FileName ~p Name ~p LogDataN ~p LogDataO ~p",[FileName,Name,LogDataN,LogDataO]),
    Result = case {LogDataO, LogDataN} of
		{_, {error, Reason}} ->                      % Example: Target node is down or sim node never up, can not collect data
		    {error, Reason};
		{{ok,LogData}, {ok,LogData}} ->              % Old and new logs are same, I.e. nothing written to log
		    {ok, FileName};
		{_,_} ->
		     case file:open(FileName, [write]) of
			 {ok, FileId} ->
%			     io:format("rct_logging:get_log(~p,~p,~p,~p,~p)",[Pid, FileId, Type, LogDataO, LogDataN]),
			     Result2 = case get_log(Pid, FileId, Type, LogDataO, LogDataN) of
					  ok ->
					      {ok, FileName};
					  {error, Reason} ->
					      {error, Reason}
				      end,
			     file:close(FileId),
			     Result2;
			 {error,Reason} ->
			     ct:log(lightred,"~p: ~p ~p Can not open log file ~s, Reason: ~p",[Name, ?MODULE, get_logs, FileName, {error, Reason}]),
			     {error, Reason}
		     end
	     end,
    get_logs(Pid, TCLogPath, T, R ++ [DataN#?MODULE{node_logdata=Result}]).
    
get_log(Pid,FileId,_Type,Reason,{ok,[{Name,Size,Line}|GZ]}) when Reason == {error, target_node_is_down};
								 Reason == {error, sim_node_is_down};
								 Reason == option_get_all -> % No old data exist, get everything
    get_all_files(Pid,FileId,{Name,Size,Line},lists:reverse(GZ));
get_log(Pid,FileId,_Type,{error,_},{ok,[{Name,Size,Line}|GZ]}) -> % No old data exist (could not be collected), get everything
    get_all_files(Pid,FileId,{Name,Size,Line},lists:reverse(GZ));
get_log(Pid,FileId,_Type,{ok,[{Name,_SizeO,LineO}|GZ]},{ok,[{Name,_SizeN,LineN}|GZ]}) -> % All log changes in one file
    case exec(Pid, "cat "++Name++" | sed 1,"++integer_to_list(LineO)++ "d | sed "++ integer_to_list(LineN-LineO+1)++"q", ?TIMEOUT_FETCH_FILE) of
	{ok, R} -> %io:format(FileId, "~s", [R]),
	  	   file:write(FileId, R),
		   ok;
	{error, Reason} -> {error, Reason}
    end;
get_log(Pid,FileId,num,{ok,[{NameO,_SizeO,LineO}|_]},{ok,[{NameN,SizeN,LineN}|N]}) -> % logs have wrapped.
    case match_name(NameO,N,[]) of
	no_match ->
	    get_all_files(Pid,FileId,{NameN,SizeN,LineN},lists:reverse(N));
	FileList ->
	    get_some_files(Pid,FileId,{NameN,SizeN,LineN},{LineO,lists:reverse(FileList)})
    end.
    
get_some_files(_Pid,_FileId,{_Name,_SizeN,0},[]) -> % sed 0q not allowed
    ok;
get_some_files(Pid,FileId,{Name,_SizeN,LineN},[]) -> % get newest file
    case exec(Pid,"cat "++Name++" | sed "++integer_to_list(LineN)++ "q", ?TIMEOUT_FETCH_FILE) of
	{ok, R} -> %io:format(FileId, "~s", [R]),
	  	   file:write(FileId, R),
		   ok;
	{error, Reason} -> {error, Reason}
    end;
get_some_files(Pid,FileId,{Name,SizeN,LineN},{0,[{File,_}|T]}) -> % bug in sed 1,0d
    case exec(Pid, "cat "++File, ?TIMEOUT_FETCH_FILE) of
	{ok, R} -> %io:format(FileId, "~s", [R]),
	  	   file:write(FileId, R),
		   get_some_files(Pid,FileId,{Name,SizeN,LineN},T);
	{error, Reason} -> {error, Reason}
    end;
get_some_files(Pid,FileId,{Name,SizeN,LineN},{LineO,[{File,_}|T]}) -> % get oldest file
    case exec(Pid, "cat "++File++" | sed 1,"++integer_to_list(LineO)++ "d", ?TIMEOUT_FETCH_FILE) of
	{ok, R} -> %io:format(FileId, "~s", [R]),
	  	   file:write(FileId, R),
		   get_some_files(Pid,FileId,{Name,SizeN,LineN},T);
	{error, Reason} -> {error, Reason}
    end;
get_some_files(Pid,FileId,{Name,SizeN,LineN},[{File,_}|T]) -> % get whole middle file
    case exec(Pid, "cat "++File, ?TIMEOUT_FETCH_FILE) of
	{ok, R} -> %io:format(FileId, "~s", [R]),
	  	   file:write(FileId, R),
		   get_some_files(Pid,FileId,{Name,SizeN,LineN},T);
	{error, Reason} -> {error, Reason}
    end.
    
get_all_files(_Pid,_FileId,{_Name,_SizeN,0},[]) -> % sed 0q not allowed
    ok;
get_all_files(Pid,FileId,{Name,_SizeN,LineN},[]) ->
    case exec(Pid, "cat "++Name++" | sed "++integer_to_list(LineN)++ "q", ?TIMEOUT_FETCH_FILE) of
	{ok, R} -> %io:format(FileId, "~s", [R]), 
	  	   file:write(FileId, R),
		   ok;
	{error, Reason} -> {error, Reason}
    end;
get_all_files(Pid,FileId,{Name,SizeN,LineN},[{File,_}|T]) ->
    case exec(Pid, "cat "++File, ?TIMEOUT_FETCH_FILE) of
	{ok, R} -> %io:format(FileId, "~s", [R]),
	           file:write(FileId, R),
		   get_all_files(Pid,FileId,{Name,SizeN,LineN},T);
	{error, Reason} -> {error, Reason}
    end.
    
match_name(_Name,[],_R) ->
    no_match;
match_name(Name,[{Name,Size}|_],R) ->
    R++[{Name,Size}];
match_name(Name,[{Name2,Size}|T],R) ->
    match_name(Name,T,R++[{Name2,Size}]).
	    
%call rct_logging:print_logs([{log1,[{erlang,"mytest_log1_erlang.log","/proj/rcs-tmp/stps/mystp/logs/ct_run.mystp@esekilxxen465.2012-04-05_10.21.24/suites.examples.logging_SUITE.mytest.logs/run.2012-04-05_10.21.27/log_private/mytest_log1_erlang.log",["ERROR REPORT","CRASH REPORT"],[]},
%				    {alarm,"mytest_log1_alarm.log","/proj/rcs-tmp/stps/mystp/logs/ct_run.mystp@esekilxxen465.2012-04-05_10.21.24/suites.examples.logging_SUITE.mytest.logs/run.2012-04-05_10.21.27/log_private/mytest_log1_alarm.log",[],[]}]}],[])
%[{ok,[{ok,"/proj/rcs-tmp/stps/mystp/logs/ct_run.mystp@esekilxxen465.2012-04-05_10.21.24/suites.examples.logging_SUITE.mytest.logs/run.2012-04-05_10.21.27/log_private/mytest_log1_alarm.log"},
%      {ok,"/proj/rcs-tmp/stps/mystp/logs/ct_run.mystp@esekilxxen465.2012-04-05_10.21.24/suites.examples.logging_SUITE.mytest.logs/run.2012-04-05_10.21.27/log_private/mytest_log1_erlang.log"}]}]
print_logs([]) ->
    {ok,[]};
print_logs(Blades) ->
    %% io:format("<table border=\"1\">~n"
    %% 	      "<tr><td>Type</td>~n", []),
    format_html("<table border=\"1\">~n"
		"<tr><td>Type</td>~n"),
    %% [io:format("<td>~s</td>~n",[Blade])||{Blade,_Logs}<-Blades],
    %% io:format("</tr>~n"),
    [format_html("<td>~s</td>~n",[Blade])||{Blade,_Logs}<-Blades],
    format_html("</tr>~n"),
    LogTypes=find_logtypes(Blades),
    R2=print_log_types(LogTypes,Blades,[]),
    %% io:format("</table>~n"),
    format_html("</table>~n"),
    look_for_error(R2).

print_log_types([],_,R) ->
    R;
print_log_types([LogType|T],Blades,R) ->
    %% io:format("<tr><td>~p</td>",[LogType]),
    format_html("<tr><td>~p</td>",[LogType]),
    R2=print_log_type(LogType,Blades,[]),
    %% io:format("</tr>~n"),
    format_html("</tr>~n"),
    print_log_types(T,Blades,R++R2).

print_log_type(_,[],R) ->
    R;
print_log_type(Type,[{_Blade,Logs}|T],R) ->
    case lists:keysearch(Type,1,Logs) of
	{value,{Type,_Name,{error, Reason},_Bad,_Good}} ->
	    %% io:format("<td><font color=red>~p</font></td>", [{error, Reason}]),
	    format_html("<td><font color=red>~p</font></td>", [{error, Reason}]),
	    print_log_type(Type,T,R++[{error,Reason}]);
	{value,{Type,Name,{ok,Path},Bad,Good}} ->
	    R2= case file:read_file_info(Path) of
		    {error,enoent} ->
			%% io:format("<td>~s</td>", [Name]),
			format_html("<td>~s</td>", [Name]),
			{ok,Path};
		    {ok,_} ->
			case egrep(Bad,Path,2) of
			    {ok, NBad} ->
				case egrep(Good,Path,2) of
				    {ok, NGood} ->
					case NBad-NGood of
					    N when N =:= 0, NBad =:= 0 ->
						%% io:format("<td><a href=\"~s\">~s</a></td>", [Path, Name]),
						format_html("<td><a href=\"~s\">~s</a></td>", [Path, Name]),
						{ok,Path};
					    N when N =:= 0, NBad > 0 ->
						%% io:format("<td><a href=\"~s\">~s</a><font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, NBad]),
						format_html("<td><a href=\"~s\">~s</a><font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, NBad]),
						{ok,Path};
					    N when N > 0, NGood =:= 0  ->
						%% io:format("<td><a href=\"~s\">~s</a><font color=red>(~p ERRORS)</font></td>", [Path, Name, N]),
						format_html("<td><a href=\"~s\">~s</a><font color=red>(~p ERRORS)</font></td>", [Path, Name, N]),
						{error,Path};
					    N when N > 0, NGood > 0  ->
						%% io:format("<td><a href=\"~s\">~s</a><font color=red>(~p ERRORS)</font> <font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, N, NGood]),
						format_html("<td><a href=\"~s\">~s</a><font color=red>(~p ERRORS)</font> <font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, N, NGood]),
						{error,Path};
					    N when N < 0, NBad =:= 0 ->
						%% io:format("<td><a href=\"~s\">~s</a><font color=red>(-~p ERRORS)</font></td>", [Path, Name, N]),
						format_html("<td><a href=\"~s\">~s</a><font color=red>(-~p ERRORS)</font></td>", [Path, Name, N]),
						{error,Path};
					    N when N < 0, NBad > 0 ->
						%% io:format("<td><a href=\"~s\">~s</a><font color=red>(-~p ERRORS)</font> <font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, N, NBad]),
						format_html("<td><a href=\"~s\">~s</a><font color=red>(-~p ERRORS)</font> <font color=green>(~p filtered ERRORS)</font></td>", [Path, Name, N, NBad]),
						{error,Path}
					end;
				    {error, Reply} ->
					%% io:format("<td><a href=\"~s\">~s</a><font color=red>(regexp ERROR ~p)</font></td>", [Path, Name, Reply]),
					format_html("<td><a href=\"~s\">~s</a><font color=red>(regexp ERROR ~p)</font></td>", [Path, Name, Reply]),
					{error,Path}
				end;
			    {error, Reply} ->
				%% io:format("<td><a href=\"~s\">~s</a><font color=red>(regexp ERROR ~p)</font></td>", [Path, Name, Reply]),
				format_html("<td><a href=\"~s\">~s</a><font color=red>(regexp ERROR ~p)</font></td>", [Path, Name, Reply]),
				{error,Path}
			end
		end,
	    print_log_type(Type,T,R++[R2]);
	_ ->
	    %% io:format("<td></td>"),
	    format_html("<td></td>"),
	    print_log_type(Type,T,R)
    end.   
   
find_logtypes(Blades) ->
    remove_duplicates([Type||{Type,_,_,_,_}<-lists:flatten([L||{_,L}<-Blades])],[]).

remove_duplicates([],R) ->
    lists:sort(R);
remove_duplicates([H|T],R) ->
    case lists:member(H,T) of
        true  -> remove_duplicates(T,R);
        false -> remove_duplicates(T,R++[H])
    end.

look_for_error(L) ->
    case lists:keysearch(error,1,L) of 
        {value,_} -> {error,L}; 
        false     -> {ok,L}
    end.

format_html(String) ->
    ct:log(default, 1, String, [], [no_css]).

format_html(String,Args) ->
    ct:log(default, 1, String, Args, [no_css]).

%logging:egrep(["ERROR REPORT","CRASH REPORT"],"/home/test34/ct/log/rst_scx_0_0_erlang.log").
egrep([],_,_) ->
    {ok, 0};
egrep(L,File,N) ->
    [T|H] = lists:reverse(L),
    Cmd=lists:flatten("egrep '(" ++ [X++"|"||X<-lists:reverse(H)]++T++")' "++File++" | wc -l"),
%"egrep '(ERROR REPORT|CRASH REPORT)' erlang.log.1 | wc -l"
    Reply = os:cmd(Cmd)--"\n",
    case re:run(Reply,"^[0-9]+$",[]) of
	nomatch -> 
	    case {Reply, N} of
		{Reply,0} ->
		    ct:log(yellow,"~p:egrep/3 os:cmd(~p) returned ~p", [?MODULE, Cmd, Reply]),
		    {error, Reply};
		_ ->
		    ct:log(yellow,"~p:egrep/3 os:cmd(~p) returned ~p, retrying ~p times", [?MODULE, Cmd, Reply, N]),
		    timer:sleep(1000),
		    egrep(L,File,N-1)
	    end;
	{match,_} -> {ok, list_to_integer(Reply)}
    end.

exec(sim, Cmd, _TMO) ->
    {ok, list_to_binary(os:cmd(Cmd))};
exec(Pid, Cmd, TMO) ->
    ssh_exec(Pid, Cmd, TMO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SSH PART SSH PART SSH PART SSH PART SSH PART SSH PART %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%rct_logging:ssh("137.58.180.130",22,"root","root","uname -a", 10000).
%io:format("~p",[[rct_logging:ssh("137.58.180.130",22,"root","root","uname -a", 10000)||_<-lists:seq(1,500)]]).
%%% ssh_connect(IP, Port, User, Pwd) -> {ok, Pid} | {error, 
ssh_connect(sim, sim, sim, sim, _,_, _) ->
    {ok, sim};
ssh_connect(IP, Port, User, Pwd, ConnectTimeout, NoRetries, RetryDelay) ->
    crypto:start(),
    ssh:start(),
    Pid = spawn(?MODULE, ssh_connect, [self(), {IP, Port, User, Pwd}, connect, ConnectTimeout, NoRetries, RetryDelay]),
    receive {Pid, Result} -> Result end.

ssh_exec(Pid, Cmd, TMO) ->
    Pid ! {exec, self(), Cmd, TMO},
    receive {Pid, Result} -> Result end.
    
ssh_close(sim) ->
    ok;
ssh_close(Pid) ->
    Pid ! {close, self()},
    receive {Pid, Result} -> Result end.

%%% @hidden
ssh_connect(From, LoginData, connect, ConnectTimeout, Retries, RetryDelay) ->
    case do_ssh_connect(LoginData, "", ConnectTimeout, Retries, RetryDelay) of
	{ok, SSH} ->
	    From ! {self(), {ok, self()}},
	    ssh_connected_loop(From, LoginData, SSH, ConnectTimeout, Retries, RetryDelay);
	Error ->
	    From !  {self(), Error}
    end.

% {ok, SSH} | {error, Reason}
do_ssh_connect(_LoginData, {error, Reason}, _, 0, _) ->
    {error, Reason};
do_ssh_connect(LoginData={IP, Port, User, Pwd}, _Result, ConnectTimeout, Retries, RetryDelay) ->
    case ssh:connect(IP,Port,[{user,User},
			      {password,Pwd},
			      {silently_accept_hosts, true}, 
			      {user_interaction,false},
                              {connect_timeout,ConnectTimeout*1000}],ConnectTimeout*1000) of
	{ok, SSH} ->
	    {ok, SSH};
	{error, Reason} ->
	    ct:log(yellow,"~p: ssh connect ~p failed, Retrying ~p time(s) with ~p seconds delay, Reason: ~p",[?MODULE, IP, Retries, RetryDelay, {error, Reason}]),
	    timer:sleep(RetryDelay * 1000),
	    do_ssh_connect(LoginData, {error, Reason},ConnectTimeout, Retries-1, RetryDelay)
    end.

ssh_connected_loop(From, LoginData, SSH, ConnectTimeout, Retries, RetryDelay) ->
    receive
	{close, From} ->
	    ssh:close(SSH),
	    From ! {self(), ok};
	{exec, From, Cmd, TMO} ->
	    {SSH2, Reply} = ssh_channel(SSH,LoginData, Cmd,TMO, "", ConnectTimeout, Retries, RetryDelay),
	    From ! {self(), Reply},
	    ssh_connected_loop(From, LoginData, SSH2, ConnectTimeout, Retries, RetryDelay);
	Other ->
	    ct:log(yellow,"!!!!!!!!!!!!!!!!!! ~p: ~p",[?MODULE, Other]),
	    ssh_connected_loop(From, LoginData, SSH, ConnectTimeout, Retries, RetryDelay)
end.

%{SSH,{ok,Reply}} | {SSH,{error, Reason}}
ssh_channel(SSH,_LoginData,_Cmd,_TMO,{error, Reason},_,0, _) ->
    {SSH,{error, Reason}};
ssh_channel(SSH,LoginData,Cmd, TMO,_Status,ConnectTimeout,Retries, RetryDelay) ->
    Result = case SSH of
		 disconnected ->
		     do_ssh_connect(LoginData, "",ConnectTimeout, Retries, RetryDelay);
		 SSH ->
		     {ok,SSH}
	     end,
    case Result of
	{ok, SSH2} ->
	    case do_ssh_channel(SSH2,Cmd,TMO) of
		{ok,Reply} ->
		    {SSH2,{ok,Reply}};
		{error, Reason} ->            
		    ct:log(yellow,"~p: ssh channel failed, Retrying ~p time(s), Reason: ~p",[?MODULE, Retries, {error, Reason}]),
		    timer:sleep(1000),
		    ssh_channel(disconnected, LoginData, Cmd,TMO,{error, Reason},ConnectTimeout,Retries-1, RetryDelay)
	    end;
	{error, Reason} ->
	    {SSH,{error, Reason}}
    end.

do_ssh_channel(SSH,Cmd,TMO) ->
    debug("CMD~n~p ~p", [SSH,Cmd]),    
    case ssh_connection:session_channel(SSH, 5000) of
	{ok, Chn} ->
	    Exec = case ssh_connection:exec(SSH, Chn, Cmd, 2000) of
		       success ->
			   do_recv_response(SSH, Chn, [], TMO);
		       failure ->
			   {error, {ssh_connection, exec, failure}};
                       % Undocumented return values
		       {error,Reason} ->
			   ct:log(yellow,"~p: !!!!! Undocumented return value from ssh_connection:exec/4 ~p",[?MODULE, {error,Reason}]),
			   {error, {ssh_connection, exec, Reason}};
		       {closed,Ch} ->
			   ct:log(yellow,"~p: !!!!! Undocumented return value from ssh_connection:exec/4 ~p",[?MODULE, {closed,Ch}]),
			   {error, {ssh_connection, exec,{closed,Ch}}};
		       Other ->
			   ct:log(yellow,"~p: !!!!! Undocumented return value from ssh_connection:exec/4 ~p",[?MODULE, Other]),
			   {error, {ssh_connection, exec,Other}}
		   end,
	    ssh_connection:close(SSH, Chn),
	    Exec;
	{error, Reason} ->
	    {error, {ssh_connection, session_channel, Reason}};
        % OTP ssh bug??????????
	{closed,Ch} ->
	    ct:log(yellow,"############# ~p: CHANNEL_CLOSED ~p",[?MODULE, Ch]),
	    {error,{ssh_connection, session_channel, {closed,Ch}}}
    end.
   
do_recv_response(SSH, Chn, Data, Timeout) ->
%    io:format("process_info do_recv_response ~p",[erlang:process_info(self())]),
    receive	
	{ssh_cm, SSH, {closed,Chn}} ->
	    debug("CLSD~n~p ~p", [SSH,Chn]),
	    {ok,list_to_binary(lists:reverse(Data))};

	{ssh_cm, SSH, {data,Chn,_,NewData}} ->
	    ssh_connection:adjust_window(SSH, Chn, size(NewData)),
	    debug("RECVD~n~p ~p ~p", [SSH, Chn, binary_to_list(NewData)]),
	    do_recv_response(SSH, Chn, [NewData|Data], Timeout);

	{ssh_cm, SSH, {eof,Chn}} ->
	    debug("RECVD EOF~n~p ~p", [SSH,Chn]),
	    do_recv_response(SSH, Chn, Data, Timeout);	    

	{ssh_cm, SSH, {exit_signal,Chn,Signal,Err,_Lang}} ->
	    debug("RECVD exit_signal~n~p ~p ~p ~p", [SSH,Chn,Signal,Err]),
	    do_recv_response(SSH, Chn, Data, Timeout);

	{ssh_cm, SSH, {exit_status,Chn,Status}} ->
	    debug("RECVD exit_status~n~p ~p ~p", [SSH,Chn,Status]),
	    do_recv_response(SSH, Chn, Data, Timeout);

	Other ->
	    ct:log(yellow,"########### ~p: ~p", [?MODULE, Other]),
	    debug("UNEXPECTED MESSAGE~n~p ~p ~p", [SSH,Chn,Other]),
	    do_recv_response(SSH, Chn, Data, Timeout)

    after Timeout ->
	    debug("SSHTIMEOUT~n~p ~p ~p", [SSH,Chn,Data]),
%	    {timeout,list_to_binary(lists:reverse(Data))}
	    {error, {timeout,list_to_binary(lists:reverse(Data))}}
    end.

debug(F,A) ->
%    io:format("~p" ++ F ++ "~n",[time()] ++ A).
    {F,A}.

make_name_module(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(?MODULE)).

%% @spec update_config(Name,ConfigLine) -> ok | no_configuration_found | not_affected_by_update
%% @doc callback function for updating configuration data.
%% ```Name = atom()                          Alias for cli or coli towards node.
%%    ConfigLine = tuple                     {ssh_lmt_ipv4, [{ssh, SSH}, {port, Port}, {user, User}, {password, Password}]} '''
update_config(Name, {ssh_lmt_ipv4, [{ssh, SSH}, {port, Port}, {user, User}, {password, Pwd}]}) ->
    case ct:get_config(?MODULE) of
	undefined -> 
	    no_configuration_found;
	Config ->
	    [{Name,Board,_,_,_,_,Logs,_}] = [X||X<-Config,element(1,X) =:= Name],
	    NewConfig=lists:keyreplace(Name,1,Config,{Name,Board,SSH,Port,User,Pwd,Logs,Board ++ "(" ++ SSH ++ ")"}),
	    rct_multi_node_cfg:remove_config(?MODULE),
	    ok = rct_multi_node_cfg:require(?MODULE,NewConfig)
    end;
update_config(_Name,_Config) ->
    not_affected_by_update.

