%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_scp.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R4A/4
%%% @doc
%%%
%%% == Usage ==
%%%
%%% This module contains a common test hook copying files to and from target. 
%%%
%%% Hook formats:
%%% ```{rct_scp, [{N, Name}]}'''
%%%
%%% There is a short format when running towards one card:
%%% ```{rct_scp, Name} expands to {rct_scp, [{1, Name}]}'''
%%% 
%%% There is a short format for testing towards clustered node:
%%% ```{rct_scp, [Name1,Name2]} expands to {rct_scp, [{1, Name1},{2, Name2}]}'''
%%% 
%%% Argument description:
%%% ```N        = integer()                      Used to match card in stp.cfg file.
%%%    Name     = atom()                         Used as identifier'''
%%% 
%%% Following config variables are required to be specified in stp.cfg file
%%% ```{ssh_lmt_ipv4, [{ssh, string()}, {port, integer()}, {user, string()}, {password, string()}]}'''
%%% Examples:<br/>
%%% Hooks below will prepare copying of files to and from target node.
%%% 
%%% Example single node:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_scp, node}]}].'''
%%% Example clustered nodes:
%%% ```suite() -> 
%%%        [{ct_hooks, [{rct_scp, [node1, node2]}]}].'''
%%% Testcase example:
%%% ```testcase(_) ->
%%%        rct_scp:to_target(node1, "/home/etxkols/RCS/bin/rcs_scp_tmp.exp", "/rcs", 5),
%%%        rct_scp:from_target(node1, "/rcs/erlang/erlang.log.1", "/tmp", 5).'''
%%% @end
-module(rct_scp).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/4').
-date('2016-08-19').
-author('etxkols').
-include_lib("common_test/include/ct.hrl").
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-01-21 etxkols     Created
%%% R2A/2      2013-04-17 etxkols     Wrong reply in doc
%%% R2A/3      2013-05-13 etxkols     Change re:run to string:str
%%% R2A/4      2014-03-26 etxkols     Faulty init/2 return value
%%% R2A/4      2014-05-03 etxkols     Changed ct:pal to ct:log
%%% R4A/1      2015-06-03 etxkols     Cluster fixes
%%% R4A/2      2016-03-09 etxkols     5G
%%% R4A/3      2016-06-21 etxkols     GIT
%%% R4A/4      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/2,
	 pre_init_per_suite/3,
	 terminate/1,
	 to_target/4,
	 to_target/5,
	 from_target/4,
	 from_target/5]).

-define(RCS_SCP_TMP, {"RCT_TOP","test/bin/rcs_scp_tmp.exp "}).

%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
%%===========================================================================
%% @spec pre_init_per_suite(Suite, Config, CthStates) -> 
%%    {Config, CthStates} | {{fail,Reason}, CthStates}
%%
%% @doc Verifies CT config parameter ssh_lmt_ipv4. Makes a CT target_name() (Alias)
%% containing ssh_lmt_ipv4 data<br/>
%%===========================================================================
pre_init_per_suite(_Suite, {SkipOrFail, _Reason} = Ret, CthState) 
  when SkipOrFail =:= skip; SkipOrFail =:= fail ->
    {Ret, CthState};
pre_init_per_suite(Suite,Config,Name) when is_atom(Name) ->
    pre_init_per_suite(Suite,Config,[Name]);
pre_init_per_suite(_Suite,Config,States) ->
    SuiteType = case length(States) > 1 of
    		    true  -> clustered;
    		    false -> single
    		end,
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    {Config, States};
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    States2 = do_pre_init_per_suite(States,[],SuiteType,1),
	    {Config,States2}
    end.

%%===========================================================
%% Verifies CT config parameter ssh_lmt_ipv4. Makes a CT target_name() 
%% (Alias) containing ssh_lmt_ipv4 data and paths to corefile locations
%%===========================================================
do_pre_init_per_suite([],R,_SuiteType,_Num) ->
    R;
do_pre_init_per_suite([Name|T],R,SuiteType,Num) when is_atom(Name) ->
    do_pre_init_per_suite([{Num,Name}|T],R,SuiteType,Num);
do_pre_init_per_suite([{N,Name}|T],R,SuiteType,Num) ->
    BoardNum = rct_cluster_lib:get_target_du(N,SuiteType), 
    Board = ct:get_config({test_nodes,BoardNum}),
    case ct:get_config({Board,ssh_lmt_ipv4}) of
	SSH = [{ssh,_},
	       {port,_},
	       {user,_},
	       {password,_}] ->
	    rct_multi_node_cfg:require(Name,SSH),
	    do_pre_init_per_suite(T,R ++ [Name],SuiteType,Num + 1 );
	Other ->
	    ct:log(lightred,"~p: ~p ~p Config parameters not matching {ssh_lmt_ipv4,[ssh,port,user,password]} for ssh to SUT, Reason: ~p",[Name, ?MODULE, pre_init_per_suite, {error, Other}]),
            {{fail,Other }, [Name]}
    end.

%%% @hidden
terminate(Names) ->
   case os:getenv("SIM_OR_TARGET") of
        "sim" ->
	    ok;
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    do_terminate(Names)
    end.

do_terminate([]) ->
    ok;
do_terminate([Name|T])->
    rct_multi_node_cfg:remove_config(Name),
    do_terminate(T).

to_target(Name, From, To, Timeout) ->
    to_target(Name, From, To, Timeout, print).
%%===========================================================================
%% @spec to_target(Name, From, To, Timeout, Print) -> 
%%   {ok, Reply} | {error, Reason}
%% Name = atom()
%% From = string()
%% To = string()
%% Timeout = integer()
%% Reply = string()
%% Reason = term()
%% Print = print | noprint
%%
%% @doc Copy file to target
%% If Print = noprint, no printouts are made to CT htmllog except for failure.
%% <br/>
%% If Print = print, progress printouts are made to CT htmllog. 
%% This equals to_target/4.<br/>
%%===========================================================================
to_target(Name, From, To, Timeout, Print) ->
    copy(to_target, Name, From, To, Timeout, Print).
    
from_target(Name, From, To, Timeout) ->
    from_target(Name, From, To, Timeout, print).
%%===========================================================================
%% @spec from_target(Name, From, To, Timeout, Print) -> 
%%   {ok, Reply} | {error, Reason}
%% Name = atom()
%% From = string()
%% To = string()
%% Timeout = integer()
%% Reply = string()
%% Reason = term()
%% Print = print | noprint
%%
%% @doc Copy file from target
%% If Print = noprint, no printouts are made to CT htmllog except for failure.
%% <br/>
%% If Print = print, progress printouts are made to CT htmllog. 
%% This equals from_target/4.<br/>
%%===========================================================================
from_target(Name, From, To, Timeout, Print) ->
    copy(from_target, Name, From, To, Timeout, Print).
   
copy(Type, Name, From, To, Timeout, Print) ->
    case os:getenv("SIM_OR_TARGET") of
        "sim" ->
            ct:log(lightred,"~p: Cannot SCP node, Reason NOT supported in SIMULATED environment",[Name]);
        TargetOrCloud when TargetOrCloud == "target";
                           TargetOrCloud == "cloudish" ->
	    IP   = ct:get_config({Name, ssh}),
	    User = ct:get_config({Name, user}),
	    Pwd  = ct:get_config({Name, password}),
	    {RCT_TOP, SCP_PATH} = ?RCS_SCP_TMP,
	    case os:getenv(RCT_TOP) of
		false ->
		    {error, "Could not resolve environment variable $RCT_TOP"};
		Base ->
		    SCP_TMP = filename:join(Base,SCP_PATH),
		    ct:pal("SCP_TMP ~p~n",[SCP_TMP]),
		    Cmd = case Type of
			      from_target ->
				  lists:concat([SCP_TMP, User, "@", IP,":",From," ",To," ",Pwd," ",integer_to_list(Timeout)]);
			      to_target ->
				  lists:concat([SCP_TMP, From," ", User, "@", IP,":",To," ",Pwd," ",integer_to_list(Timeout)])
			  end,
		    Reply = os:cmd(Cmd),
		    FileName = filename:basename(From),
		    case string:str(Reply, FileName) of
			0 ->
			    ct:log(lightred, "~p: ~s failed,~nReason: ~s",[Name, Cmd, Reply]),
			    {error, Reply};
			_ ->  % Successfull copy always prints filename in Reply
			    case re:run(Reply, "No such file or directory") of
				nomatch ->		
				    case Print of
					print -> ct:log("~p: ~s~n~s",[Name, Cmd, Reply]);
					_ -> ok
				    end,
				    {ok, Reply};
				{match,_} ->
				    ct:log(lightred, "~p: ~s failed,~nReason: ~s",[Name, Cmd, Reply]),
				    {error, Reply}
			    end
		    end
	    end
    end.
 
