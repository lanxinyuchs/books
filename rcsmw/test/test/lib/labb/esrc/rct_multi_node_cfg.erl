%%%-------------------------------------------------------------------
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2015
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2012 by Ola Andersson <eolaand@sekix404.rnd.ki.sw.ericsson.se>
%%%-------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2011-12-01 eolaand     Created
%%% R1A/2      2012-06-08 etxkols     Added require/2
%%% R1A/3      2012-06-15 etxkols     Added extra checks to avoid
%%%                                   double config (i.e. ct specs)
%%% R1A/4      2012-02-03 etxkols     Added remove_config/1,
%%%                                   add_config/2, append_config/2
%%% R1A/5      2012-07-05 etxkols     Updated for new ct_netconfc
%%% R1A/6      2012-09-03 etxkols     Modified to pass edoc
%%% R2A/1      2013-05-22 etxkols     Added get_config/2
%%% R4A/1      2016-06-01 etxkols     Added remove_config/2
%%% ----------------------------------------------------------
-module(rct_multi_node_cfg).
 
%% API
-export([require/2, require/3, require/4, get_config/2,  remove_config/1, remove_config/2, add_config/2, append_config/2, check_parameter/1, read_config/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Adds new configuration and makes alias for it
%%% rct_multi_node_cfg:require(kalle,[{a,1},{b,2},{c,3}]). ->
%%% ok | {error, Reason}
require(Name, Vals) ->
    case ct:get_config(Name) of
	undefined ->
	    CfgString = lists:flatten(io_lib:format("[{~w,~w}].",[Name, Vals])),
	    case ct:add_config(?MODULE, CfgString) of
		ok ->
		    ct:require(Name, {Name, [X||{X,_}<-Vals]});
		{error, Reason} ->
		    {error, Reason}
	    end;
	Vals ->
	    ct:require(Name, {Name, [X||{X,_}<-Vals]});
	Other ->
	    {error, {already_in_use, Name, Other}}
    end.

require(Name, NodeIndex, {RequiredSubKey, Required}) ->
    require(Name, NodeIndex, {RequiredSubKey, Required}, test_nodes).

require(Name, NodeIndex, {RequiredSubKey, Required}, NodeKey) ->
    case ct:get_config({NodeKey, NodeIndex}) of
	undefined ->
	    {error, {get_config,NodeKey,NodeIndex}};
	Node ->
	    case ct:get_config({Node, RequiredSubKey}) of
		undefined ->
		    {error, {get_config,Node,RequiredSubKey}};
		Val ->
		    case ct:get_config(Name) of
			undefined ->
			    CfgString = lists:flatten(io_lib:format("[{~w,~w}].",[Name, Val])),
			    case ct:add_config(?MODULE, CfgString) of
				ok ->
				    ct:require(Name, {Name, Required});
				{error, Reason} ->
				    {error, Reason}
			    end;
			Val ->
			    ct:require(Name, {Name, Required});
			Other ->
			    {error, {already_in_use, Name, Other}}
		    end
	    end
    end.
	
%%% Runs ct:get_config one level down
%%% rct_multi_node_cfg:get_config(1,power). -> undefined | Value
%%% rct_multi_node_cfg:get_config(1,{power,power_cons_ip}). -> undefined | Value
get_config(NodeIndex, SubKey) when is_atom(SubKey) ->
    get_config(NodeIndex, {SubKey});
get_config(NodeIndex, SubKey) ->
    case ct:get_config({test_nodes, NodeIndex}) of
	undefined ->
	    undefined;
	Node ->
	    ct:get_config(list_to_tuple([Node|tuple_to_list(SubKey)]))
    end.

%%% These 4 functions are mostly intended for new cth_conn_log to update
%%% config variable ct_conn_log
remove_config(Name) ->
    case ct:get_config(Name) of
	undefined ->
	    [];
	Vals ->
	    CfgString = lists:flatten(io_lib:format("[{~w,~w}].",[Name, Vals])),
	    ct:remove_config(?MODULE,CfgString),
	    Vals
    end.

remove_config(Name, Key) ->
    case ct:get_config(Name) of
	undefined ->
	    [];
	Vals ->
	    OldCfgString = lists:flatten(io_lib:format("[{~w,~w}].",[Name, Vals])),
	    ct:remove_config(?MODULE,OldCfgString),
	    NewVals = lists:keydelete(Key,1,Vals),
	    NewCfgString = lists:flatten(io_lib:format("[{~w,~w}].",[Name, NewVals])),
	    ct:add_config(?MODULE, NewCfgString),	    
	    NewVals
    end.

add_config(Name, Vals) ->		
    case ct:get_config(Name) of
	undefined ->
	    CfgString = lists:flatten(io_lib:format("[{~w,~w}].",[Name, Vals])),
	    ct:add_config(?MODULE, CfgString);
	Other ->
	    {error, {already_in_use, Name, Other}}
    end.
    
append_config(Name, Vals) ->
    OldVals = remove_config(Name),
    add_config(Name, Vals ++ OldVals).

check_parameter(Cfg) ->
    {ok, {config, Cfg}}.

read_config(Cfg) ->
    {ok, Tokens, _} = erl_scan:string(Cfg),
    {ok, Term} = erl_parse:parse_term(Tokens),
    {ok, Term}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
