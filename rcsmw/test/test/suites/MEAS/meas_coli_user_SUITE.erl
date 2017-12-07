%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_coli_user_SUITE.erl %
%%% @author eransbn
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/1
%%% 
%%% @doc == Measure times when one nc user add, get, delete several mo instances.==
%%% This Test Suite can be used on target and simulated enviroment.<br/>
%%%
%%% 
%%% @end

-module(meas_coli_user_SUITE).
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2016-03-8  eransbn     Created
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 several_coli_sessions/1
	]).
-define(ColiTimeOut,15*60*1000). %%coli connection time out 15 min
-define(NrOfColiConfUsers, 21).
-define(Coli_SessionNameList, [list_to_atom("coli_"++integer_to_list(N)) || N <-  lists:seq(1,?NrOfColiConfUsers)]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% runs the rct_netconf hook for each user, to be able to open a netconf session for each user.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    ColiHooks= [{rct_coli, {list_to_atom("coli_"++integer_to_list(N)), 
			    [manual_connect, {connect_timeout, 1400}]}}
		|| N <- lists:seq(1,?NrOfColiConfUsers)],
    [{timetrap, {hours, 72}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {cth_conn_log, []},
		 {rct_power,node},
		 {rct_consserv,cs1},
		 {rct_rs232,console},
		 {rct_core,[]},
		 {rct_logging, {all, [{erlang,{[],[]}}]}} |
		 ColiHooks
		]}].


init_per_suite(Config) ->
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
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    Config.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases defined in list.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [several_coli_sessions].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].


%%--------------------------------------------------------------------
%% @doc
%% Set up max coli connections (today 20) and let them time out after 15 min. <br/>
%% - Set up max coli connections.<br/>
%% - Try to set up one more  coli connections and it should be refused.<br/>
%% - After 15 min all the coli connections should be released.<br/>
%% - Set up one coli connection<br/>
%% @spec several_coli_sessions(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
several_coli_sessions(_Config) ->
    SessionNameListLastElem = lists:last(?Coli_SessionNameList),
    ct:log("SessionNameListLastElem ~p",[SessionNameListLastElem]),
    SessionNameListExeptLastElem = lists:droplast(?Coli_SessionNameList),
    ct:log("SessionNameListExeptLastElem ~p",[SessionNameListExeptLastElem]),

    %%Set up max allowed coli connection
    ct:log("Set up max allowed coli connection (~p)",[string:len(SessionNameListExeptLastElem)]),
    lists:foreach(fun(Name) -> coli_open_session(Name) end, SessionNameListExeptLastElem),

    ct:log("Set up one more to se that it is refused"),
    coli_open_session_fail(SessionNameListLastElem, {error,"Connection closed"}),	   


    ct:log("timer sleep ~p minutes",[?ColiTimeOut/(3*60*1000)] ),

    timer:sleep(?ColiTimeOut div 3),
    ct:log("Set up one more to se that it is refused"),
    coli_open_session_fail(SessionNameListLastElem, {error,"Connection closed"}),

    ct:pal("timer sleep ~p minutes",[?ColiTimeOut/(3*60*1000)] ),
    timer:sleep(?ColiTimeOut div 3),
    coli_open_session_fail(SessionNameListLastElem, {error,"Connection closed"}),

    ct:log("timer sleep ~p minutes",[?ColiTimeOut/(3*60*1000)] ),
    timer:sleep(?ColiTimeOut div 3),
    coli_open_session(SessionNameListLastElem),
    rct_coli:disconnect(SessionNameListLastElem),

    ok.




%%--------------------------------------------------------------------
%% @doc 
%% Open coli sessions. <br/>
%% @end
%%--------------------------------------------------------------------
coli_open_session(Coli_Session) ->
    ct:log("### Open Name: ~p", [Coli_Session]),
    case rct_coli:connect(Coli_Session) of
	ok ->ok;
	Reply -> ct:fail(Reply)
    end.
%%--------------------------------------------------------------------
%% @doc 
%% Open coli sessions failed. <br/>
%% @end
%%--------------------------------------------------------------------
coli_open_session_fail(Coli_Session, Reason) ->
    ct:log("### Open Name: ~p", [Coli_Session]),
    case rct_coli:connect(Coli_Session) of
	Reason -> ok;
	Reply -> ct:fail(Reply)
    end.

