%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %File: librcsee.erl %
%%% @author eramkka
%%% @copyright Ericsson AB 2015-2016

-module(librcsee).
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
%%% %CopyrightEnd%
-export([poll_connection_coli/4,execute_cmd/1,parse_llog/2,check_restartOrwatchdog/0,board_type/0]).
%%--------------------------------------------------------------------
%% @doc
%% To poll the coli connection.
%% @end
%%--------------------------------------------------------------------
poll_connection_coli(HookName, _Timer, _Result, 0)->
    ct:fail("Tried too many times to connect to rct_~p", [HookName]) ;
poll_connection_coli(HookName, Timer, Result, NumTry)->
    timer:sleep(Timer),
    case HookName of
	coli -> case rct_coli:connect(coli) of
                   ok -> ct:log("Coli connection is ok"),
                         rct_coli:disconnect(HookName),
			 ok;
                   Result -> ct:log("OK: ~p",[Result]),
			      rct_coli:disconnect(HookName),
			      ok;
		    _-> ok = rct_coli:disconnect(HookName),
			poll_connection_coli(HookName, Timer, Result, NumTry - 1)
		end;
	_-> ct:fail("HookName ~p not suported",[HookName])
    end.
%% ===========================================================================
%% @doc
%%		Sends command to the board using telnet<br>
%% @end
%% ===========================================================================
execute_cmd(Cmd) ->
	ok = rct_rs232:login(rs232),
	case ct_telnet:cmd(rs232, Cmd, 60000) of
		{ok,Data} ->
			Data;
		{error, Reason}->
			ct:log(lightred,"Error ~p", [Reason]),
			nok
	end.
%% ===========================================================================
%% @doc
%%		Checks if llog entry contains information for  restarts <br/>
%%
%% @end
%% ===========================================================================
parse_llog(RestartType,Extrainfo) ->
	Llog = execute_cmd("llog -l"),
	F = fun(X) ->
				X ++ "\n"
		end,
	New_llog = lists:flatmap(F, Llog),
	ct:pal("New llog ~p",[New_llog]),
	%% No:      1
	%% Reason:  Ordered Restart
	%% Time:    2014-06-17 09:38:46
	%% Program: -
	%% Pid:     -
	%% Rank:    Cold With Test
	%% Signal:  -
	%% PMD:     -
	%% Extra:   -
	Regex = ".*No:\\s+1\\n"++
	 	"Reason:\\s+Ordered restart\\s+\\n" ++
	        "Time:\\s+.*\\n" ++
		"Program:\\s+\\-.*\\n"++
		"Pid:\\s+\\-.*\\n"
		"Rank:\\s+"++RestartType++".*\\n"++
		"Signal:\\s+\\-.*\\n"++
		"PMD:\\s+\\-.*\\n" ++
		"Extra:\\s+"++Extrainfo++"\\nroot.*",
	case re:run(New_llog, Regex, [{capture, all, list}, global]) of
		{match, List} ->
		        io:format("~p",List),
			ct:pal("correct llog entry found in llog");
		nomatch -> ct:fail("llog entry is not correctly printed in llog")
	end,
ok.
%% ===========================================================================
%% @doc
%%              Checks if llog entry contains unexpected restart or  watchdog for  restarts <br/>
%%
%% @end
%% ========================================================================
check_restartOrwatchdog()->
ct_telnet:send(rs232, "llog"),
case ct_telnet:expect(rs232,[{restartUnexp,"Unexpected Restart"},{watchdog,".*watchdog.*"}],[{timeout,2000},no_prompt_check]) of
      {ok, {restartUnexp,_}} ->
                             ct:fail("Unexpected restart found");
      {ok, {watchdog,_}} ->
                             ct:fail("watchdog found");
      {_,_} ->
                ct:pal("No extra restart or watchdog is found")
end,
ok.
%%%------------------------------------------------------------------------
%%%returns the board type
%%%------------------------------------------------------------------------
board_type() ->
 case ct_telnet:cmd(rs232, "cat /sys/rbs-fn/rbs-sys/board_type") of
        {ok,[_Cmd,"dus32" ,_Prompt]} ->
                      BoardType="dus32";
         {ok,[_Cmd,"dus52" ,_Prompt]} ->
                      BoardType="dus52";
        {ok,[_Cmd, "tcu03" ,_Prompt]} ->
                      BoardType="tcu03";
        {ok,[_Cmd, "tcu04" ,_Prompt]} ->
                      BoardType="tcu04"
 end,
BoardType.

