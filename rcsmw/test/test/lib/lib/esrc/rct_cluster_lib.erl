%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_cluster_lib.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/4
%%% @doc  ==Support functions for running single node testsuites on clustered nodes==
%%% @end
-module(rct_cluster_lib).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/4').
-date('2015-10-22').
-author('etxkols').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R3A/1      2015-03-09 etxkols     Created
%%% R4A/1      2015-05-28 etxkols     added get_sim_erl_node/3,get_target_du/3
%%% R4A/2      2015-09-17 etxkols     First regular is 3
%%% R4A/4      2015-10-13 etxkols     Added get_target_du_mpid/2 and get_target_du_mpid/3
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([get_sim_erl_node/2,
	 get_sim_erl_node/3,
	 get_target_du/2,
	 get_target_du/3,
	 get_target_du_mpid/2,
	 get_target_du_mpid/3,
	 get_ct_arg/1]).

%%% @doc 
%%% Returns simulated erlang node name.<br/>
%%% ```N = Indicates which node number in a cluster the erlang nodename will be calculated for
%%%    SuiteType = clustered | single'''
%%%
%%% This function returns a erlang node name, based on:<br/>
%%% - node number in a cluster<br/>
%%% - if testsuite is clustered or single<br/>
%%% - if -mp parameter [du1,du2,...] is given as option for rct_run.sh (for running single node testsuites on clustered nodes)<br/>
%%% - if -rcssim_sname parameter is given as option for rct_run.sh (mostly for Jenkins to keep track of nodes)<br/>
%%% @end
-spec get_sim_erl_node(integer(), atom()) -> atom().
get_sim_erl_node(N,SuiteType) ->
    MP = get_ct_arg(mp),
    get_sim_erl_node(N,SuiteType,MP).

%%% @doc 
%%% Returns simulated erlang node name.<br/>
%%% For time beeing there is a need to override -mp parameter in rct_run.sh for hooks working towards core MP (du1 in 16A), ex cli, netconf, coli, httpserver.
%%% @end
-spec get_sim_erl_node(integer(), atom(), string()) -> atom().
get_sim_erl_node(N,SuiteType,MP) ->
    DU = "du" ++ integer_to_list(map_N_to_mpid(N)),
    Username = os:getenv("USER"),
    {ok, HostName} = inet:gethostname(),          
    Rcssim_sname = get_ct_arg(rcssim_sname),
    ErlNode = case {SuiteType,Rcssim_sname,MP} of
		  {single,undefined,undefined} ->                   
		      Username++"@"++HostName;                 % etxkols@esekilxxen3016 One rcssim per user/host
		  {single,Rcssim_sname,undefined} ->                
		      Rcssim_sname++"@"++HostName;             % sim006@esekilxxen3016 Several rcssim per user/host (Jenkins)
		  {single,undefined,MP} ->                    
		      MP++"_"++Username++"@"++HostName;        % du1_etxkols@esekilxxen3016                   
		  {single,Rcssim_sname,MP} ->               
		      MP++"_"++Rcssim_sname++"@"++HostName;    % du1_sim006@esekilxxen3016                
		  {clustered,undefined,_} ->           
		      DU++"_"++Username++"@"++HostName;        % du1_etxkols@esekilxxen3016, du2_etxkols@esekilxxen3016,...
		  {clustered,Rcssim_sname,_} ->                 		      
		      DU++"_"++Rcssim_sname++"@"++HostName     % du1_sim006@esekilxxen3016,  du2_sim006@esekilxxen3016,..
	      end,
    list_to_atom(ErlNode).

%%% @doc 
%%% Returns target node number.<br/>
%%% ```N = Indicates which node number in a cluster the erlang nodename will be calculated for
%%%    SuiteType = clustered | single'''
%%%
%%% This function returns a target node number, based on:<br/>
%%% - node number in a cluster<br/>
%%% - if testsuite is clustered or single<br/>
%%% - if -mp parameter is given as option for rct_run.sh (for running single node testsuites on clustered nodes)<br/>
%%% REMOVE this function and use get_target_du_mpid/2 instead.
%%% @end
-spec get_target_du(integer(), atom()) -> integer().
get_target_du(N,SuiteType) ->
    MP = get_ct_arg(mp),
    get_target_du(N,SuiteType, MP).

%%% @doc 
%%% Returns target node number.<br/>
%%% For time beeing there is a need to override -mp parameter in rct_run.sh for hooks working towards core MP (du1 in 16A), ex cli, netconf, coli, httpserver.
%%% REMOVE this function and use get_target_du_mpid/3 instead.
%%% @end
-spec get_target_du(integer(), atom(), string()) -> integer().
get_target_du(N,SuiteType, MP) ->
    case {SuiteType,MP} of
        {single,undefined} -> N;
        {single,MP} -> list_to_integer(MP -- "du");
        {clustered,_} -> N
    end.
    
%%% @doc 
%%% Returns target node number.<br/>
%%% ```N = Indicates which node number in a cluster the erlang nodename will be calculated for
%%%    SuiteType = clustered | single'''
%%%
%%% This function returns a target node number(number in .cfg file) and MPID, based on:<br/>
%%% - node number in a cluster<br/>
%%% - if testsuite is clustered or single<br/>
%%% - if -mp parameter is given as option for rct_run.sh (for running single node testsuites on clustered nodes)<br/>
%%% @end
-spec get_target_du_mpid(integer(), atom()) -> integer().
get_target_du_mpid(N,SuiteType) ->
    MP = get_ct_arg(mp),
    get_target_du_mpid(N,SuiteType,MP).

%%% @doc 
%%% Returns target node number.<br/>
%%% For time beeing there is a need to override -mp parameter in rct_run.sh for hooks working towards core MP (du1 in 16A), ex cli, netconf, coli, httpserver.
%%% @end
-spec get_target_du_mpid(integer(), atom(), string()) -> integer().
get_target_du_mpid(N,SuiteType,MP) ->
    case {SuiteType,MP} of
	{single,undefined} -> {N,map_N_to_mpid(N)};
	{single,MP} -> MPID = list_to_integer(MP -- "du"),
		       {map_mpid_to_N(MPID),MPID};
	{clustered,_} -> {N,map_N_to_mpid(N)}
    end.
    
%%% @doc 
%%% Read Common Test argument.<br/>
%%% ```Arg = Argument'''
%%%
%%% This function returns common test argument if it exist
%%% @end
-spec get_ct_arg(atom()) -> string() | undefined.
get_ct_arg(Arg) ->
    case init:get_argument(Arg) of
	{ok,[[Reply]]} -> Reply;
	error -> undefined
    end.

map_N_to_mpid(N) ->
%    ct:pal("~p",[ct:get_config(cluster_mp_ids)]),
    case ct:get_config(cluster_mp_ids) of
	undefined ->
	    N;
	L when is_list(L) ->
	    lists:nth(N,L)
    end.
	    
map_mpid_to_N(MPID) ->
    case ct:get_config(cluster_mp_ids) of
	undefined ->
	    MPID;
	L when is_list(L) ->
	    map_mpid_to_N(MPID,L,1)
    end.

map_mpid_to_N(MPID,[],_) ->
    MPID;
map_mpid_to_N(MPID,[MPID|_],N) ->
    N; 
map_mpid_to_N(MPID,[_|T],N) ->
    map_mpid_to_N(MPID,T,N+1).
