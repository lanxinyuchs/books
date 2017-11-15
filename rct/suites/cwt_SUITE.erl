%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:cwt_SUITE.erl %
%%% @author :ERAMKKA
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/10, checkout by etxkols in etxkols_rcs
%%% @doc
%%% Runs execution environment tests.
%%%
%%% Execution environment test run my EE Jenkins and MW Jenking.
%%% @end

-module(cwt_SUITE).
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
%%
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-compile([export_all]). % edoc is not generated when -compile
-export([all/0,check_testagt/1,fetch_file/4,check_cwt_result/2]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_power, rct_consserv, rct_rs232
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {hours, 1}},
     {ct_hooks, [{rct_htmllink,[]},
		 {rct_power,pow},
		 {rct_scp,scpn1},
		 {rct_consserv,cs},
		 {rct_rs232,rs232},
                 {rct_logging, {[erlang,syslog], 
						[{syslog,{["ERROR", "error", "Error"],
						  ["init: open\\(/dev/console\\): Input/output error",
						  "User reported error",
						   "processing log message:",
						   "failed to stop snmp daemon",
						   "SBB Error in function sdo_do\\_get",
						   "SBB ISR 4 \\- ERROR_ROOTKEY",
						   "Error\\[TED]:get_session_details",
						   "ncp: adk_netd_error:: ",
						   "PCIE0: Error interrupt 0x20000",
						   "ERRORS 0",
						   "0 errors",
						   "Setup complete \\(0 errors\\)"
						   ]}}],	 
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
    ok.

all() ->
[{group, tcu03}, {group, dus52}].

groups() ->
    [
    {tcu03,[],[run_cwt]},
    {dus52,[],[run_cwt]}
    ].

%%====================================================================
%% @doc
%% Check cwt log if it contains the expected test result or not
%% @end
%%====================================================================
check_testagt([H|T])->
ct_telnet:send(rs232,"cat /rcs/ee-testmgr/cwt_log"),
 case ct_telnet:expect(rs232,[{messgdone,"HTMD_MSG_DONE received \\[agtId=1 testId="++H++" result=0\\]"}],[{timeout,50000},no_prompt_check]) of
         {ok,_}->
                 ct:pal("Test with id ~p passed",[H]);
         {error,timeout} ->
                 ct:pal("Test with id ~p failed",[H])
  end,
  check_testagt(T);


 check_testagt([]) ->

  true.

%%--------------------------------------------------------------------
%%Check hardware log difference
%%
%%--------------------------------------------------------------------
isHwLogLines(HwLogLinesBefore,HwLogLinesAfter)->

HwLogLinesBeforeInt=list_to_integer(HwLogLinesBefore),
HwLogLinesAfterInt=list_to_integer(HwLogLinesAfter),
  if 
       (HwLogLinesAfterInt == HwLogLinesBeforeInt + 2) ->
          true;
       true->
          ct_telnet:cmd(rs232, "hwlog"),
	  false
end.
%%--------------------------------------------------------------------
%%Check cwt result message
%%
%%--------------------------------------------------------------------

check_cwt_result(Config,HwLogLinesBefore) ->
{ok,[_,HwLogLinesAfter,_]}=ct_telnet:cmd(rs232, "hwlog|wc -l"),
ct:pal("total number of hwlog entries after cwt is ~p",[HwLogLinesAfter]),
case ct_telnet:cmd(rs232, "cat /sys/rbs-fn/rbs-sys/board_type") of
        {ok,[_Cmd,"dus32" ,_Prompt]} -> 
     	              BoardTypeTc="31",                    
check_testagt(["589926","65598","65626","196639","196640","196618","196629","196630","196631","196632","262174","262184","262204","262209","327715","327720","458773","458774","458775","458776","458777","458778","458779","458780","720916","720926","720936","720946","720951","720956","720966"]);
         {ok,[_Cmd,"dus52" ,_Prompt]} -> 
     	              BoardTypeTc="31",                     
check_testagt(["589926","65598","65626","196639","196640","196618","196629","196630","196631","196632","262174","262184","262204","262209","327715","327720","458773","458774","458775","458776","458777","458778","458779","458780","720916","720926","720936","720946","720951","720956","720966"]);
       {ok,[_Cmd, "tcu03" ,_Prompt]} ->	                     
check_testagt(["589925","65598","65626","196639","196640","196618","196629","196630","196631","196632","262174","262184","262204","262209","327715","327720","458773","458774","458775","458776","458777","655380","655400"]),
      BoardTypeTc="23"
 end,
	 ct_telnet:send(rs232,"cat /rcs/ee-testmgr/cwt_log|grep -c 'result=0\\]'"),
             case ct_telnet:expect(rs232,BoardTypeTc,[{timeout,50000},no_prompt_check]) of
                 {ok,_}->
				ct:pal("check if HTMI TEST NO FAULT is seen or not"),
				ct_telnet:send(rs232,"cat /rcs/ee-testmgr/cwt_log"),
		 		case ct_telnet:expect(rs232,"HTMI_TEST_NO_FAULT",[{timeout,50000},no_prompt_check]) of
                                        {ok,_}->
					        ct:pal(" HTMI TEST NO FAULT is seen"),
						case isHwLogLines(HwLogLinesBefore,HwLogLinesAfter) of
						      true-> ct:pal("No hwlog entry found as expected");
                                                      false -> ct:fail("unexpected hwlog is found")
						end;
                                        {error,timeout} ->
                                                ct:fail("HTMI TEST NO FAULT is not seen")
		 		end;
		{error,timeout} ->
                            ct:pal("check if HTMI TEST COMPLETE_ENTITY_FAILURE is seen or not"),
                            ct_telnet:send(rs232,"cat /rcs/ee-testmgr/cwt_log"),
                            case ct_telnet:expect(rs232,"HTMI_TEST_COMPLETE_ENTITY_FAILURE",[{timeout,50000},no_prompt_check]) of
                                {ok,_}->
				     fetch_file("Downloading /var/log/syslog from board", "/var/log/syslog", "syslog", Config),
				     fetch_file("Downloading cwt log from node","/rcs/ee-testmgr/cwt_log","cwt_log",Config),
				     case  isHwLogLines(HwLogLinesBefore,HwLogLinesAfter) of       
                                                   true -> ct:fail("No hwlog entry found unexpected behaviour");
                                                   false -> ct:pal("expected hwlog entry is found")
				      end,
				      ct:fail("HTMI TEST COMPLETE ENTITY FAILURE is seen");
                                {error,timeout} ->
                                     ct:fail("HTMI TEST COMPLETE ENTITY FAILURE is not seen")
		 	    end
	    end,
ok.

%% ===========================================================================
%% @doc
%%		Downloads file from board to host<br>
%%		In the priv_dir under the common_test's logs directory
%% @end
%% =========================================================================== 
fetch_file(Heading, Path, Name, Config) ->
	PrivDir = ?config(priv_dir, Config),
	Type = filename:extension(Path),
	PathInFS = PrivDir ++ Name,
	ct:log("PathInFS ~p", [PathInFS]),
	Result = rct_scp:from_target(scpn1, Path, PathInFS, 20000),
	%ct:pal(?DEBUG, "Copied file from target  : ~p", [Result]),
	case Result of
		{ok, _Log} ->
			ct:log("~p",[Heading]),
			ct:log("<a href=~p type=~p>~s</a>\n",[PathInFS,Type,Name]),
			ok;
		_->ct:fail("Failed to copy file from board ~p", [Path])
	end.
%% ===========================================================================
%% @doc
%%		Returns nth member of List <br>
%%		And converts it to integer <br/>
%% @end
%% ===========================================================================
get_int(N, List) ->
	Head = lists:nth(N, List),
	{H, _Rest} = string:to_integer( Head ),
	H.
%% ===========================================================================
%% @doc
%%		Extracts Date and time from List <br>
%%		Start is the first index of member in the list <br/>
%%		Returns datetime <br/>
%% @end
%% ===========================================================================
get_date(List, Start) ->
	R1 = Start + 1, %month
	R2 = Start + 2, %day
	R3 = Start + 3,	%hour
	R4 = Start + 4, %min
	R5 = Start + 5, %sec
	Date = {get_int(Start, List), get_int(R1, List), get_int(R2, List)} ,
	Time = {get_int(R3, List), get_int(R4, List), get_int(R5, List)} ,
	%ct:pal(?DEBUG, "date time ~p~p", [Date,Time]),
	{Date, Time}.

%% ===========================================================================
%% @doc
%%		Returns  time in list format <br/>
%%		@spec get_current_time() -> {{Date}{Time}}
%% @end
%% ===========================================================================
get_current_time(DateStr) ->
	get_date_time(DateStr, 2).

%% ===========================================================================
%% @doc
%%		Parses date string and converts to datetime<br/>
%%		@spec get_date_time(DateStr, Start) -> {{Date}{Time}}
%% @end
%% ===========================================================================
get_date_time(DateStr, Start) ->
	%%2013-12-19-14-02-12
	Regex = "(\\d+)-(\\d+)-(\\d+)-(\\d+)-(\\d+)-(\\d+).*",
	get_date_time_for_regex(DateStr, Start, Regex).

%% ===========================================================================
%% @doc
%%		Returns { {Date}, {Time}} for matching Regex <br> 
%% @end
%% ===========================================================================
get_date_time_for_regex(DateStr, Start, Regex) ->
	case re:run(DateStr, Regex, [{capture, all, list}]) of
		{match, List} ->
			ct:pal("Parsed date ~p~p", [DateStr, List]),
			get_date(List, Start);	
		nomatch ->
			ct:fail("Failed to parse date for ~p", [DateStr])
	end.
%%====================================================================
%% @doc
%% Checks for time stamp
%% @end
%%====================================================================
check_timestamp(StartTime) ->
 {ok,[_,TimeStampDate,_]} = ct_telnet:cmd(rs232,"cat /rcs/ee-testmgr/cwt_log |grep ts= |awk \'{print substr($1\"-\"$2, 4,length($1$2) - 1)}\'",60000),
 StartDateTime=get_current_time(re:replace(TimeStampDate, "\\:", "\\-", [{return, list}, global]) ),
 EndDateTime=get_current_time(StartTime),
 {Days, {HH, MM, _SS}} =  calendar:time_difference(EndDateTime,StartDateTime),
 if
     (Days == 0) and (HH == 0) and (MM <11)->
                    ct:pal("the time stamp value is correct and the time taken for cwt  is within the limit: ~p minutes",[MM]),
		    ok;
      true->
              ct:pal("the timestamp value is incorrect and the time taken for cwt is out of the range: Days:~p,Hours:~p,Minutes:~p",[Days,HH,MM]),
	      ct:fail("time stamp verification failed")
end.
%%--------------------------------------------------------------------
%% @doc
%% Runs the test manager and test agents with the passed configuration
%% @end
%%--------------------------------------------------------------------
run_cwt(Config)->
	{ok,_}=ct_telnet:cmd(rs232,"rm -rf /rcs/ee-testmgr/cwt_log"),
	{ok,[_,Hwlines,_]}=ct_telnet:cmd(rs232, "hwlog|wc -l"),
	ct:pal("total number of hwlog entries before cwt is ~p",[Hwlines]),
	ct_telnet:cmd(rs232, "cp /dev/null /var/log/syslog"),
	{ok,[_,StartDate,_]}=ct_telnet:cmd(rs232,"date +%Y-%m-%d-%H-%M-%S",60000),
	ct:pal("node date ~p",[StartDate]),
	ok=ct_telnet:send(rs232, "pgh_restartbrd 1"),
	timer:sleep(720000),     %% Sleep for 12 minutes to allow all the tests to execute
        ok = rct_rs232:login(rs232),
        ok = ct_telnet:send(rs232, ""),
       {ok, _} = ct_telnet:expect(rs232, "root@.*:~# $", [{timeout,5000},no_prompt_check]),       
       fetch_file("Downloading cwt log from node","/rcs/ee-testmgr/cwt_log","cwt_log",Config),
       check_cwt_result(Config,Hwlines),
       check_timestamp(StartDate).
