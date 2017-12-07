%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_lib.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R5A/R6A/R7A/3
%%%
%%% @doc == support lib. ==
%%% <br/>
%%%
%%%
%%% @end

-module(meas_lib).
-include_lib("common_test/include/ct.hrl").


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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R4A/2      2015-04-28 etxivri     Created
%%% R4A/3      2015-05-04 etxivri     Bugfix
%%% R5A/1      2015-10-07 etxivri     Update to use info from jenkins config.
%%% R5A/2      2015-10-07 etxivri     Add get_board_type
%%% R6A/1      2016-05-23 etxivri     Add check_if_cxs_is_bpu
%%% R7A/1      2016-08-30 etxivri     Update to handle when test is not 
%%%                                   runed by jenkins.
%%% R7A/2      2016-09-05 etxivri     Bugfix
%%% R7A/3      2016-10-25 etxivri     Add check_product_on_board.
%%% ----------------------------------------------------------


-export([get_build_label/0,
	 get_cs_cxp/1,
	 get_branch/0,
	 get_branch/1,
	 get_board_type/0,
	 check_if_cxs_is_bpu/0,
	 check_product_on_board/0
	]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_build_label() ->
    BuildLabel = case ct:get_config({jenkins_config, cxs}) of
		 undefined ->
		     "undef";
		 Value ->
		     Value
	     end,
    ct:log("BuildLabel: ~p", [BuildLabel]),
    BuildLabel.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_cs_cxp(Cli_Sess) ->
    BoardType = 
	proplists:get_value(board_type,
			    ct:get_config(
			      ct:get_config({test_nodes,1}))),
    CS_CXP = case BoardType of
		 BoardType when BoardType == "dus5201";
				BoardType == "dus3201" ->
		     "CXP9031275";
		 BoardType when BoardType == "tcu03";
				BoardType == "tcu0401" ->
		     "CXP9031274";

		 %% "dus5201" ->
		     %%    "CXP9031275";
		 %% "tcu03" -> 
		 %%     "CXP9031274";
		 %% "tcu04" -> 
		 %%     "CXP9031274";
		 _NoCheck ->
		     "dum"
	     end,
			 
    ct:pal("### Get SW version for CS CXP : ~p",[CS_CXP]),
    rct_cli:connect(Cli_Sess),
    case  rct_cli:send(Cli_Sess,"show ManagedElement=1,SystemFunctions=1,SwInventory=1") of
	{ok , RecievedData} ->
	    ct:log("RecievedData: ~s", [RecievedData]),
	    Var = string:tokens(RecievedData, "=\r\n /-"),
	    %% ct:log("SwInventory: ~p", [Var]),
	    %% test_server:break("break"),
	    case lists:dropwhile(fun(X) ->
					 X =/=  CS_CXP
				 end, Var) of
		[] ->
		    ct:pal("No cs cxp was found!", []),
		    %% Ind = "dummy",
		    %% RBS_CS_Rev = "dummy",
		    CsCxp = "dummy";
		RBS_CS_SwItem ->
		    ct:log("# RBS_CS_SwItem: ~p", [RBS_CS_SwItem]),
		    [CS_CXP, Ind, RBS_CS_Rev | _ ] = RBS_CS_SwItem,
		    CsCxp = CS_CXP++"/"++Ind++"-"++RBS_CS_Rev
	    end;
	_Other ->
	    ct:pal("Unexpected data: ~p", [_Other]),
	    %% Ind = "dummy",
	    %% RBS_CS_Rev = "dummy",
	    CsCxp = "dummy"
    end,
    rct_cli:disconnect(Cli_Sess),

    %% CsCxp = CS_CXP++"/"++Ind++"-"++RBS_CS_Rev,
    ct:pal("## CS CXP: ~p", [CsCxp]),
    CsCxp.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
get_branch() ->
    Branch = case ct:get_config({jenkins_config, branch}) of
		  undefined ->
		      "undef";
		  Value ->
		      Value
	      end,
    ct:log("Branch: ~p", [Branch]),
    Branch.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
get_branch(CXP_REV) ->
    ct:log("CXP_REV: ~p",[CXP_REV]),
    case CXP_REV of
	"dummy" ->
	    Branch = "no_branch";
	_Other ->
	    CXP_REV_List = string:tokens(CXP_REV,"-"),
	    [Rev| _] = lists:reverse(CXP_REV_List),
	    ct:log("Rev: ~p",[Rev]),
   	    {Branch, _} = lists:split(2, Rev)
    end,
    ct:log("Branch: ~p",[Branch]),
    Branch.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_board_type() ->
    BoardType = proplists:get_value(board_type,
				    ct:get_config(
				      ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    BoardType.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_if_cxs_is_bpu() ->
    CXS = ct:get_config({jenkins_config, cxp}),
    ct:log("CXS: ~p", [CXS]),
    case CXS of
	undefined ->
	    Board_Type = "undefined";
	_Other ->
	    Board_Type = case re:run(CXS, "CXS101665_3") of
			     {match, _} ->
				 "bpu";
			     _Other2->
				 "no_bpu"
			 end
    end,
    Board_Type.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_product_on_board() ->
    CXS = ct:get_config({jenkins_config, cxp}),
    ct:log("CXS: ~p", [CXS]),
    case CXS of
	undefined ->
	    CxsProduct = "undefined";
	_Else ->
	    [CxsProduct | _T] = string:tokens(CXS,"-"),
	    ct:log("CxsProduct: ~p", [CxsProduct]),
	    ct:log("Tail: ~p", [_T])
    end,

    ProductType = case CxsProduct of
		      "CXS101665_3" ->
			  "bpu"; %% CC
		      "CXS101665_5" ->
			  "git_brcs";
		      "CXS101698_6" ->
			  "git_msrcs";
		      _Other2->
			  "no_match"
		  end,
        ct:log("ProductType: ~p", [ProductType]),
    ProductType.
