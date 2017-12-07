%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_app_SUITE.erl %
%%% @author eolaand
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R6A/R8A/3
%%% 
%%% @doc == Test Suite for testing application logs.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end


-module(log_app_SUITE).

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/1      2013-10-07 uabesvi     Created
%%% R3A/2      2015-02-28 etxkols     Preparation for 2 labs
%%% R3A/3      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-10-12 etxjotj     OTP18 Replaced erlang:now
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
	 groups/0]).

-export([fake_log_exists/1]).
-export([open_error/1]).
-export([write_to_fake_log/1]).
-export([check_esi/1]).
-export([change_severity/1]).
-export([check_esi_filter/1]).

-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_log.hrl").

%% -define(SFTP_HOST, "10.68.200.11").
%% -define(USER, "mauve").
%% -define(PSWD, "dilbert").
%% -define(SFTP_URL, "sftp://"++?USER++"@"++?SFTP_HOST).


-define(INV1, 123).
-define(INV2, 456).
-define(INV3, 789).
-define(INV4, 100).

-define(TESTNODE, testnode).
-define(APP_LOGS_DIR, "saf_log").
-define(SA_TIME_UNKNOWN, 16#8000000000000000).
-define(VERSION, #safe_version{release_code  = $A,
			       major_version = 2,
			       minor_version = 1}).

-define(VNF_LOG_BASE_DIRS, ["vnf", "rcs"]).
-define(LOG_BASE_DIRS, ["rcs"]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks,
      [{rct_safe_log_rpc, [{safe_debug_level, 2}]},
       {rct_htmllink,[]},
       {rct_rpc, rpc_1},
       {rct_netconf,{nc1, html}},
       {cth_conn_log,[]},
       {rct_logging, {oi_testapp, 
		      [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
       %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
      ]}].


%% @hidden
init_per_suite(Config) ->
    crypto:start(),
    ssh:start(),
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    
    %% reset severity filters
    ok = set_severity_filter("FakeLog", []),
 
    [{meId, MeId}|Config].
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    %% sys:trace(rct_safe_log_rpc,true),
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
    [
     fake_log_exists,
     write_to_fake_log,
     open_error,
     check_esi,
     change_severity,
     check_esi_filter
    ].













%%--------------------------------------------------------------------
%% @doc
%% check that the FakeLog exists in the MO tree
%% @end
%%--------------------------------------------------------------------
fake_log_exists(_) ->
    {ok, _Filters} = get_severity_filter("FakeLog").

%%--------------------------------------------------------------------
%% @doc
%% check that the FakeLog exists in the MO tree
%% @end
%%--------------------------------------------------------------------
change_severity(_) ->
    ok = set_severity_filter("FakeLog", ["ERROR"]),
    ok = set_severity_filter("FakeLog", []).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
open_error(_) ->
    {ok, H1, _} = initialize(get_cb_fun(), ?VERSION),

    Name   = "FakeLog",
    Path   = "wrong",
    Action = ?SAFE_LOG_FILE_FULL_ACTION_ROTATE,
    Rot    = 3,
    Fmt    = "@Cr @CY-@Cm-@CdT@Ch:@Cn:@Cs @Sv @Sl @Cb",
    CA = #safe_log_file_create_attributes_2{log_file_name        = Name,
					    log_file_path_name   = Path,
					    max_log_file_size    = 500,
					    max_log_record_size  = 100,
					    ha_property          = false,
					    log_file_full_action = Action,
					    max_files_rotated    = Rot,
					    log_file_fmt         = Fmt},

    {error, ?SAFE_AIS_ERR_INVALID_PARAM} = 
	stream_open_2(H1, [{ca, CA}, {open_flag, 0}]),
    {error, ?SAFE_AIS_ERR_EXIST} = 
	stream_open_2(H1, [{ca, CA}, {open_flag, 1}]),
    ok = finalize(H1).


%%--------------------------------------------------------------------
%% @doc
%% write to fake log
%% @end
%%--------------------------------------------------------------------
write_to_fake_log(_) ->
    {ok, H1, _} = initialize(get_cb_fun(), ?VERSION),

    {ok, SH1} = stream_open_2(H1),

    ok = write_log_async(SH1, "I sit by myself and", error, ?INV1),
    ok = expected_cb({write_log, {?INV1, ok}, undefined}),
    ct:pal("#### CB ~p~n", [ok]),

    ok = write_log_async(SH1, "write myself a letter", error, ?INV2),
    ok = expected_cb({write_log, {?INV2, ok}, undefined}),
    ct:pal("#### CB ~p~n", [ok]),

    ok = stream_close(SH1),
    ok = finalize(H1).



%%--------------------------------------------------------------------
%% @doc
%% write to fake log
%% @end
%%--------------------------------------------------------------------
check_esi(Config) ->
    StreamName = "FakeLog",
    
    {ok, H1, _} = initialize(get_cb_fun(), ?VERSION),
    {ok, SH1} = stream_open_2(H1),

    Msgs = ["If the river was whiskey and I was a duck",
	    "I'd dive to the bottom and I'd never come up",
	    "Oh, tell me how long have I got to wait?",
	    "Oh, can I get you now, must I hesitate?"
	   ],

    SF = rct_rpc:call(rpc_1, ets, tab2list, [log], 10000),
    ct:log("##### severity filter LOG ~p~n", [SF]),
    RT = rct_rpc:call(rpc_1, ets, tab2list, [imm_rt_cache], 10000),
    ct:log("##### severity filter RT  ~p~n", [RT]),
    ok = set_severity_filter("FakeLog", []),
    ok = write(Msgs, ?INV1, error, SH1),

    ok   = stream_close(SH1),
    ok   = finalize(H1),

    timer:sleep(10000),
    ok = check_esi(Config, StreamName, Msgs).



%%--------------------------------------------------------------------
%% @doc
%% write to fake log
%% @end
%%--------------------------------------------------------------------
check_esi_filter(Config) ->
    StreamName = "FakeLog",
    
    {ok, H1, _} = initialize(get_cb_fun(), ?VERSION),
    {ok, SH1} = stream_open_2(H1),

    Msgs1 = ["Woke up this morning",
	     "The vision in my head",
	     "Looks so good",
	     "I flipped right back to bed"
	    ],
    
    Msgs2 = ["Stream run to the ocean",
	     "Ocean run down to the sea",
	     "If the river was whiskey",
	     "You'd have no trouble drowning me"
	   ],

    Msgs3 = ["Haven't felt this good",
	     "Since I don't know when",
	     "Whatever bit ends up dead",
	     "Wanna do it all again"
	   ],
    {A,B,C} = os:timestamp(),
    Time = [integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C)],

    ok = set_severity_filter("FakeLog", ["INFO"]),
    %% wait to be sure that the filter setting is done
    timer:sleep(10000),
    ok = write(Time,  ?INV1, error, SH1),
    ok = write(Msgs1, ?INV2, error, SH1),
    ok = write(Msgs2, ?INV3, info,  SH1),
    ok = write(Msgs3, ?INV4, error, SH1),

    ok   = stream_close(SH1),
    ok   = finalize(H1),

    timer:sleep(10000),
    ok         = check_esi(Config, StreamName, Time ++ Msgs1 ++ Msgs3),
    {error, _} = check_esi(Config, StreamName, Msgs2).




write([], _Invoke, _, _) ->
    ok;
write([Msg | T], Invoke, Level, SH) ->
    ok = write_log_async(SH, Msg, Level, Invoke),
%%     ok = expected_cb({write_log, {Invoke, ok()}, undefined}),
    timer:sleep(400), %% needed to get different names for the log files
    write(T, Invoke + 1, Level, SH).
   


%%========================================================================
%% check_esi(TestCaseConfig, StreamName, Msgs) -> ok | {error, Reason}
%% 
%% Use esi to check that the logs contain the expected entries.
%%========================================================================
check_esi(Config, StreamName, Msgs) ->
    LogDir  = ?APP_LOGS_DIR,
    PrivDir = ?config(priv_dir, Config),
    {EsiLogPath, EsiLogName} = export_esi(PrivDir),
    timer:sleep(3000),
    os:cmd("cd " ++ EsiLogPath ++ " ; tar zxvf " ++ EsiLogName),
    ct:pal("check_esi  Path ~p  Name ~p~n  ", [EsiLogPath, EsiLogName]),
    TmpLogFileDirs = [filename:join([EsiLogPath, Dir, LogDir, StreamName]) || 
			 Dir <- log_base_dirs()],
    LogFileDirs = [LogFileDir || LogFileDir <- TmpLogFileDirs, 
				 filelib:is_dir(LogFileDir)],
    check_fetched_esi(LogFileDirs, StreamName, Msgs, EsiLogPath, LogDir).


check_fetched_esi([FileDir | FileDirs], StreamName, Msgs, EsiLogPath, LogDir) ->
    %% FileDir  = filename:join([EsiLogPath, Dir, LogDir, StreamName]),
    FilesStr = os:cmd("cd " ++ FileDir ++ " ; ls"),
    ct:pal("check_esi  FileStr ~p~n  ", [FilesStr]),
    Files    = string:tokens(FilesStr, "\n"),
    ct:pal("check_esi  Files ~p~n  ", [Files]),
    LogFiles = [LF || LF <- Files, lists:suffix(".log", LF)],
    FilesRes = ce_files([LF || LF <- LogFiles, lists:prefix(StreamName, LF)],
			FileDir),
    case ce_msgs(FilesRes, Msgs, FileDir) of
	ok ->
	    ok;
	_Error when FileDirs =/= [] ->
	    %% ct:log("Failed to fetch logs from ~s~n~p~n", [FileDir, _Error]),
	    check_fetched_esi(FileDirs, StreamName, Msgs, EsiLogPath, LogDir);
	Error ->
	    ct:log("Failed to fetch logs from ~s~n~p~n", [FileDir, Error]),
	    Error
    end.


log_base_dirs() ->
    case os:getenv("SIM_OR_TARGET") of
    	"cloudish" ->
    	    ?VNF_LOG_BASE_DIRS;
    	_Other ->
    	    ?LOG_BASE_DIRS
    end.

%%------------------------------------------------------
%% Check if any files found
%%------------------------------------------------------
ce_files([], _) ->
    {error, no_files_found};
ce_files([File | FT], FileDir) ->
    {ok, file:read_file(filename:join([FileDir, File])), FT}.


%%------------------------------------------------------
%% Check that the messages are written to the files
%%------------------------------------------------------
ce_msgs({ok, {ok, Bin}, Files}, Msgs, FileDir) ->
    cem_rc(cem(string:tokens(binary_to_list(Bin), "\n"), Msgs), Files, FileDir);
ce_msgs({ok, Error, _}, _Msgs, _FileDir) ->
    {error, {file_read, Error}};
ce_msgs({error, _} = Error, _, _) ->
    Error;
ce_msgs({Error, _, _}, _, _) ->
    Error.

%% check if we have to continue to read the file
cem_rc(ok, _, _) ->
    ok;
cem_rc({cont, MsgsT}, [File | FT], FileDir) ->
    ce_msgs({ok, file:read_file(filename:join([FileDir, File])), FT},
	    MsgsT,
	    FileDir);
cem_rc({cont, _Error}, [], _) ->
    {error, not_found};
cem_rc(Error, _, _) ->
    Error.



%% Find all messages in this file
cem(_, []) ->
    ok;
cem([], Msgs) ->
    {cont, Msgs};
cem([L | LT], [M | MT] = Msgs) ->
    case string:str(L, M) of
	0 -> cem(LT, Msgs);
	_ -> cem(LT, MT)
    end.



%%--------------------------------------------------------------------
%% @doc
%% Using netconf to trig a export ESI log from SUT to a sftp server<br/>
%% This TC export ESI log from SUT to a sftp server using netconf.<br/>
%% The log file will be stored .../log_private/ , that will be created when TC starts.<br/>
%% TC will check progressreport using netconf and also check that correct file is exported correct.
%%
%% @spec export_esi(Config) -> ok
%% @end
%%--------------------------------------------------------------------
export_esi(EsiLogPath)->
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    SftpURL = "sftp://"++Username++"@"++SftpHost,
    os:cmd("chmod 777 " ++ EsiLogPath), % else permission.
    Action = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	      [{managedElementId,[],["1"]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'LogM',
		  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		  [{logMId,[],["1"]},
		   {'exportEsi',[],
		    [{uri, [], [SftpURL++EsiLogPath]},
		     {password, [], [Password]}]}
		    %% [{uri, [], [?SFTP_URL++EsiLogPath]},
		    %%  {password, [], [?PSWD]}]}
		  ]}]}]},
    ProgressFilter = {'ManagedElement',          
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'LogM',
			  [{xmlns,"urn:com:ericsson:ecim:LogM"}],
			  [{logMId,[],["1"]},
			   {progressReport,[],[]}
			  ]}]}]},
    {ok,_} = ct_netconfc:open(nc1,[]),
    %% trig the action export_esi using netconf.
    case ct_netconfc:action(nc1, Action) of
	{ok, A} ->
	    Return = extract_element(returnValue, A),
	    ct:log("export_esi ~p~n",[Return]);
	Error ->
	    ct:pal("~p~n",[Error]),
	    ct:fail("action error")
    end,
    ct_netconfc:close_session(nc1),
    timer:sleep(500),
    te(wait_for_progress(progressReport, ProgressFilter), EsiLogPath).

te({["SUCCESS"], [EsiLogName]}, EsiLogPath) ->
    ct:pal("export_esi: SUCCESS~n",[]),
    check_esi_log_exported(EsiLogName, EsiLogPath);
te(Result, _) ->
    ct:pal("export_esi: ~p~n",[Result]),
    ct:fail(Result).




%% @hidden
%% Will check that esi log is tranfered to expected path and also the size of the file.
%% If the size is to small, then maybe it does not consist on anything!
check_esi_log_exported(EsiLogName, EsiLogPath) ->
    [{host, SftpHost},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    ct:pal("#### EsiLogPath check_esi_log_exported ~p~n", [EsiLogPath]),
    {ok,ChPid,_ChRef} = 
	%% ssh_sftp:start_channel(?SFTP_HOST, 
	%% 		       [{user, ?USER},
	%% 			{password, ?PSWD},
	%% 			{silently_accept_hosts, true},
	%% 			{timeout, 10000}]),
	ssh_sftp:start_channel(SftpHost, 
			       [{user, Username},
				{password, Password},
				{silently_accept_hosts, true},
				{timeout, 10000}]),

    {ok, DirData} = ssh_sftp:list_dir(ChPid, EsiLogPath, 2000),
    % DirData = lists of strings from the directory.
    Pred= fun(Str) ->
		  if Str == EsiLogName ->
			  true;
		     true ->
			  false
		  end
	  end,

    case lists:any(Pred,DirData) of
	true -> 
	    ct:pal("### Esi log file: ~p, exist in \n path : ~p \n",
		   [EsiLogName, EsiLogPath]);
	false -> 
	    ct:fail(" Could not find the ESI log file, on sftp server.")
    end,
	     
    {ok, FileInfo} = 
	ssh_sftp:read_file_info(ChPid, EsiLogPath++EsiLogName, 2000),
    %ct:pal("### Recieved FileInfo: ~p", [FileInfo]),

    Size = lists:nth(2, tuple_to_list(FileInfo)),
    %{file_info, Size, _, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,

    if Size > 10000 ->
	    true;
       true  ->
	    ct:pal("### Size of the esi tar file is: ~p. ~n "
		   "It is smaller than expected. ~n "
		   "Unpack the file and ckeck that it look OK. \n",[Size]),
	    ct:fail("Size of the esi log file is to small! check if it "
		    "looks ok after unpack!.")
    end,  

    ssh_sftp:stop_channel(ChPid),
 
    {EsiLogPath, EsiLogName}.

%%%--------------------------------------------------------------------
%%% Description: Loop until the progress information says FINISHED
%%%              Requires a name and a netconf filter extracting the
%%%              progress report attribute
%%%--------------------------------------------------------------------

wait_for_progress(Attribute, ProgressFilter) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    {ok, A} = ct_netconfc:get(nc1, ProgressFilter),
    ct_netconfc:close_session(nc1),
    {ok, Report} = extract_element(Attribute, A),
    {ok, State} = extract_element(state, [Report]),
    timer:sleep(1000),
    case State of
	{state, _, ["FINISHED"]} ->
	    ct:log("~p~n",[Report]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} = 
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, ["CANCELLED"]} ->
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, {resultInfo, _, ResultInfo}} = 
		extract_element(resultInfo, [Report]),
	    {Result, ResultInfo};
	{state, _, [Current]} ->
	    ct:log("State: ~s~n",[Current]),
	    wait_for_progress(Attribute, ProgressFilter)
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





%%========================================================================
%% get_severity_filter(LogName)
%% 
%% 
%%========================================================================
get_severity_filter(LogName) ->
    SevFilter = {'ManagedElement',          
		 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		 [{managedElementId,[],["1"]},
		  {'SystemFunctions',
		   [{systemFunctionsId,[],["1"]},
		    {'LogM',
		     [{xmlns,"urn:com:ericsson:ecim:LogM"}],
		     [{logMId,[],["1"]},
		      {'Log',
			[{logId,[],[LogName]},
			 {severityFilter,[],[]}
			]}]}
		   ]}]},
    
    {ok, _}   = ct_netconfc:open(nc1, []),
    {ok, Res} = ct_netconfc:get(nc1, SevFilter),

    [{'ManagedElement',
      [_],
      [{managedElementId,[],["1"]},
       {'SystemFunctions',[],
	[{systemFunctionsId,[],["1"]},
	 {'LogM',
	  [_],
	  [{logMId,[],["1"]},
	   {'Log',[], CurrentFilters}]}]}]}] = Res,
    
    Filters = [F || {severityFilter,[],[F]} <- CurrentFilters],

    ct_netconfc:close_session(nc1),

    {ok, Filters}.



%%========================================================================
%% set_severity_filter(LogName)
%% 
%% 
%%========================================================================
set_severity_filter(LogName, NewFilter) ->
    Filter = [{severityFilter, [], [F]} || F <- NewFilter],
    Cmd = {'ManagedElement',          
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'LogM',
	       [{xmlns,"urn:com:ericsson:ecim:LogM"}],
	       [{logMId,[],["1"]},
		{'Log', [{logId,[],[LogName]} | Filter]
		}]}
	     ]}]},
    
    {ok, _}   = ct_netconfc:open(nc1, []),
    ok = ct_netconfc:edit_config(nc1, running, Cmd),
    ct_netconfc:close_session(nc1),
    timer:sleep(5000), %% have to wait to let the filters be activated in saf
    ok.





initialize(CB, Vsn) ->
    rct_safe_log_rpc:initialize(CB, Vsn).
    

stream_open_2(Handle) ->
    StreamName = "safLgStr=FakeLog,safApp=safLogService",    
    rct_safe_log_rpc:stream_open_2(Handle, StreamName, 0, 5000, undefined).

stream_open_2(Handle, [{ca, CA}, {open_flag, OF}]) ->
    StreamName = "safLgStr=FakeLog,safApp=safLogService",    
    rct_safe_log_rpc:stream_open_2(Handle, StreamName, OF, 5000, CA).


write_log_async(SH, Data, Level, Inv) ->
	    GL = {safe_log_generic_log_header, undefined, "userName", l(Level)},
	    LR = {safe_log_record, 
		  ?SA_TIME_UNKNOWN, 
		  2, 
		  GL,
		  list_to_binary(Data)},
	    rct_safe_log_rpc:write_log_async(SH, Inv, 1, LR).


l(emergency) -> 0;
l(alert)     -> 1;
l(citical)   -> 2;
l(error)     -> 3;
l(warning)   -> 4;
l(notice)    -> 5;
l(info)      -> 6.

stream_close(SH) ->
    rct_safe_log_rpc:stream_close(SH).

finalize(LH) ->
    rct_safe_log_rpc:finalize(LH).

%%========================================================================
%% get_cb_fun() -> function()
%% 
%% Get a function used as callback fun in initialize/2
%%========================================================================
get_cb_fun() ->
    Self = self(),
    fun(Data) ->
	    Self ! {log_cb, Data}, 
	    ok
    end.


expected_cb({Msg, timeout, Pid}) ->
    expected_cb({Msg, {0, timeout}, Pid});
expected_cb({Msg, {Inv, Res}, _Pid} = Exp) ->
    receive 
	{log_cb, 
	 {safe_log_write_log_callback, Inv, Res}} when Msg == write_log ->
	    ok;
	_Other ->
	    ct:pal("Got unexpected msg: ~p", [_Other]),
	    expected_cb(Exp)
    after 3000 ->
	    case Res of
		timeout ->
		    ok;
		_ ->
		    ct:fail("### CB cc Timeout ~p~n", [Exp])
	    end
    end.

