%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	decupling_lib.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R7A/R8A/1
%%%
%%% @doc == support lib when testing HW SW decupling. ==
%%% <br/>
%%%
%%%
%%% @end

-module(decupling_lib).
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
%%% R5A/2      2016-03-04 etxivri     Created
%%% R5A/3      2016-03-16 etxivri     updated
%%% R5A/5      2016-03-22 etxivri     Update check_selected_vs_installed_cxps
%%%                                   Due to swInventory will consist of all
%%%                                   CXPs fron contentinfo.
%%% R5A/6      2016-03-22 etxivri     add get_name_id_version
%%% R7A/1      2016-09-30 etxivri     Update for dus52 to handle unbundle MW cxp
%%% R8A/1      2017-01-31 etxivri     Update for dus3
%%% ----------------------------------------------------------


-export([
	 check_xml_not_exist/1,
	 cleanup_tmp_dir/3,
	 scp_xml/5,
	 run_mfa_to_get_cxps/2,
	 fix_string/2,
	 match_cxplist/2,
	 match_and_remove_cxps/2,
	 %% set_hw_item/4,   %% Obsolete until sensitive install is impl.
	 %% delete_hwitem/2, %% Obsolete until sensitive install is impl.
	 check_expected_hw_id/3,

	 %% for upgrade and install
	 check_selected_vs_installed_cxps/2,
	 get_rcs_cxp/2,
	 get_name_id_version/1,
	 get_hw/1,
	 %% get_all_hw/1, %% Obsolete until sensitive install is impl.
	 get_selected_products/2,
	 get_swinventory/1,
	 get_sel_cxps/1,
	 get_installed_cxps/1,
	 get_cxps_from_swinventory/1,
	 %% match_and_remove_cxps/4,
	 match_and_remove_cxps/3,

	 %% get_correct_rcs_cxp/1, %% Shall not be used.
	 get_switem_consistsof/2,
	 
	 break_auboot/3,
	 start_node_from_uboot/1,
	 change_hw_id/3,
	 get_board_type/0
	]).


%%--------------------------------------------------------------------
get_board_type() ->
    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    ct:log("# Hwa: ~p", [Hwa]),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("### BoardType: ~p ###", [BoardType]),
    BoardType.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% set_hw_item
%% %% @end
%% %%--------------------------------------------------------------------
%% set_hw_item(RPC, HwItem, ProductNumber, ProductRevision) ->
%%     Props = #{productNumber=>ProductNumber,productRevision=>ProductRevision},
%%     ok = rct_rpc:call(RPC, hwInventory, create_hwItem, [HwItem, Props], 
%% 		     10000, print),
%%     ok.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% delete_hwitem
%% %% @end
%% %%--------------------------------------------------------------------
%% delete_hwitem(RPC, HwItem) ->
%%     ok = rct_rpc:call(RPC, hwInventory, delete_hwItem, [HwItem], 
%% 		      10000, print),
%%     ok.

%%--------------------------------------------------------------------
%% @doc
%% check_expected_hw_id, Note! all_hw must in this case return one product.
%% @end
%%--------------------------------------------------------------------
%% check_expected_hw_id(RPC, ProductNumber, ProductRevision) ->
%%     HwId = rct_rpc:call(RPC, swmBoardList, boardType, [], 10000, print),
%%     ct:pal("# HwId: ~p", [HwId]),
%%     {ProductNumber, ProductRevision} = HwId.

check_expected_hw_id(RPC, ProductNumber, ProductRevision) ->
    check_expected_hw_id(RPC, ProductNumber, ProductRevision, 180000).

check_expected_hw_id(_RPC, _ProductNumber, _ProductRevision, Timeout) when Timeout < 5 ->
    ct:fail("Tc fail due to expected HW not rcvd within max timeout!");
check_expected_hw_id(RPC, ProductNumber, ProductRevision, Timeout) ->
    case rct_rpc:call(RPC, swmBoardList, boardType, [], 10000, print) of
	{badrpc,nodedown} ->
	    timer:sleep(10000),
	    check_expected_hw_id(RPC, ProductNumber, ProductRevision, Timeout-10000);
	HwId ->
	    ct:pal("# HwId: ~p", [HwId]),
	    {ProductNumber, ProductRevision} = HwId
    end.
   



%%--------------------------------------------------------------------
%% @doc
%% check_xml_not_exist
%% @end
%%--------------------------------------------------------------------
check_xml_not_exist(LS) ->
    case re:run(LS, "-up.xml") of
	{match, _} ->
	    ct:fail("TC fail due to Unexpected up.xml "
		    "exist before test start.");
	nomatch ->
	    case re:run(LS, "-hal.xml") of
		{match, _} ->
		    ct:fail("TC fail due to Unexpected hal.xml "
			    "exist before test start.");
		nomatch ->
		    ok
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% check_xml_not_exist
%% @end
%%--------------------------------------------------------------------
cleanup_tmp_dir(Config, RPC, Console) ->
    TmpDir = proplists:get_value(tmp_dir, Config),
    Cmd = "rm -rf "++TmpDir++"*.xml",
    case proplists:get_value(sim_or_target, Config) of
    	"sim" -> 
    	    rct_rpc:call(RPC, os, cmd, [Cmd], 10000, noprint);
    	_ ->
    	    %%%% rpc call remove will result in permission denied.
    	    rct_rs232:login(Console),
    	    ok = ct_telnet:send(Console, Cmd)
    end, 
    timer:sleep(5000),
    Ls = rct_rpc:call(RPC, os, cmd, ["ls "++TmpDir], 10000, 
    		      noprint),
    LS = fix_string(Ls, "\n\r\"> "),
    ct:log("# LS after remove : ~p", [LS]),
    check_xml_not_exist(LS).

%%--------------------------------------------------------------------
%% @doc
%% scp_xml
%% @end
%%--------------------------------------------------------------------
scp_xml(Config, FROM_XML_PATH, XML_FILE, RPC, SCP) ->
    TO_PATH = proplists:get_value(tmp_dir, Config),
    ct:log("# From : ~p ~n# File: ~p ~n# To: ~p", [FROM_XML_PATH,
						   XML_FILE,
						   TO_PATH]),

    case proplists:get_value(sim_or_target, Config) of
	"sim" ->
	    rct_rpc:call(RPC, os, cmd, ["pwd"], 10000, noprint),
	    CMD_A = "cp "++ FROM_XML_PATH ++ XML_FILE ++ " " ++ TO_PATH,
	    A = rct_rpc:call(RPC, os, cmd, [CMD_A], 10000, noprint),
	    ct:log("# rpc : ~p", [A]);
	_Other ->
	    A = rct_scp:to_target(SCP, FROM_XML_PATH++XML_FILE, TO_PATH, 
				  10000, noprint),
	    ct:log("# scp : ~p", [A])
    end,
    timer:sleep(1000),    
    Ls = rct_rpc:call(RPC, os, cmd, ["ls "++TO_PATH], 10000, 
    		      noprint),

    LS = fix_string(Ls, "\n\r\"> "),
    ct:pal("# LS after scp : ~p", [LS]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% run_mfa_to_get_cxps
%% @end
%%--------------------------------------------------------------------
run_mfa_to_get_cxps(Config, HWid) ->
    ct:pal("# search for HW: ~p", [HWid]),
    PathToXML = proplists:get_value(tmp_dir, Config),
    MFA = {swmBoardList, 
    	   products, 
    	   [HWid, 
    	    {all, PathToXML}]},
    
    CxpList = rct_rcsTestServer:run(MFA, Config),
    ct:log("# CxpList: ~p", [CxpList]),
    CxpList.

%%--------------------------------------------------------------------
%% @doc
%% fix_string
%% @end
%%--------------------------------------------------------------------
fix_string(Str, Separator) ->
    string:tokens(Str, Separator).


%%--------------------------------------------------------------------
%% @doc
%% match_cxplist
%% @end
%%--------------------------------------------------------------------
match_cxplist(ExpCxpList, {ok, RcvdCxpList}) ->
    ct:log("### ExpCxpList : ~p", [ExpCxpList]),
    ct:log("### RcvdCxpList : ~p", [RcvdCxpList]),
    ok = match_and_remove_cxps(ExpCxpList, RcvdCxpList).

%%--------------------------------------------------------------------
%% @doc
%% match_and_remove_cxps
%% @end
%%--------------------------------------------------------------------
match_and_remove_cxps([], []) ->
    ok;
match_and_remove_cxps([Cxp | CxpRest], RcvdCxpList) ->
    ct:log("Cxp : ~p", [Cxp]),
    true = lists:member(Cxp, RcvdCxpList),
    NewRcvdCxpList = lists:delete(Cxp, RcvdCxpList),
    ct:log("NewRcvdCxpList : ~p", [NewRcvdCxpList]),
    match_and_remove_cxps(CxpRest, NewRcvdCxpList).



%%%%%%%%%% For upgrade and install %%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%%
%% @spec check_selected_vs_installed_cxps(Rpc, NC_Session) -> ok
%% @end
%%--------------------------------------------------------------------
check_selected_vs_installed_cxps(Rpc, NC_Session) ->
    %% HwIdList = get_all_hw(Rpc),
    HwId = get_hw(Rpc),
    ct:pal("# HwId : ~p", [HwId]),

    Sel_Products = get_selected_products(Rpc, HwId),
    ct:pal("# Selected Cxps for all HW : ~p", [Sel_Products]),
    SelCxps = get_sel_cxps(Sel_Products),
    ct:log("## get_sel_cxps : ~p", [SelCxps]),
    BoardType = get_board_type(),
    RcsEECxp = case BoardType of
		   "dus5201" ->
		       get_rcs_cxp(Rpc, "RCSEE");
		   BoardType when BoardType == "dus5301";
				  BoardType == "dus3301" ->
		       ct:log("No need due to EE is not boundle")
    end,
    %% RcsMWCxp = get_rcs_cxp(Rpc, "RCSMW"),
    %% RcsCxps = [RcsEECxp] ++ [RcsMWCxp],
    %% ct:log("RcsCxps from /home/sirpa/software/cxp90312* and cxs101549*:~n ~p", [RcsCxps]),
    %% All_Sel_Cxps = SelCxps++RcsCxps,
    All_Sel_Cxps = case BoardType of
		       "dus5201" ->
			   SelCxps ++ [RcsEECxp];
		       _Otheeer ->
			   SelCxps
		   end,
    ct:log("## AllSelCxps: ~p", [All_Sel_Cxps]),
    AllSelCxps = list_filter(All_Sel_Cxps, "RCS-"),
    ct:pal("## Relevant Selected Cxps : ~p", [AllSelCxps]),

    Installed_CXPs = get_installed_cxps(Rpc),
    InstalledCXPs = list_filter(Installed_CXPs, ".xml"),
    ct:pal("## Relevant Installed_CXPs: ~p", [InstalledCXPs]),

    match_and_remove_cxps(AllSelCxps, 
    			  InstalledCXPs, 
    			  NC_Session),

    %% SwInventory = get_installed_cxps(NC_Session),
    %% Installed_CXPs = get_cxps_from_swinventory(SwInventory),

    %% match_and_remove_cxps(AllSelCxps, 
    %% 			  Installed_CXPs, 
    %% 			  SelCxps, 
    %% 			  NC_Session),
    ok.

list_filter(List, Str) ->
    FilteredList = lists:filter(fun(X) ->
				case re:run(X, Str) of
				    {match, _} ->
					false;
				    _Other ->
					ct:log("XX: ~p", [X]),
					true
				end
			end, List),
    ct:log("## FilteredList: ~p", [FilteredList]),
    FilteredList.
    %% NewList = lists:map(fun(X) ->
    %% 				case re:run(X, Str) of
    %% 				    {match, _} ->
    %% 					ct:pal("X: ~p", [X]),
    %% 					"";
    %% 				    _Other ->
    %% 					ct:pal("XX: ~p", [X]),
    %% 					X
    %% 				end
    %% 			end, List),
    %% ct:pal("## NewList: ~p", [NewList]),
    %% NewList.

%% check_selected_vs_installed_cxps(Rpc, NC_Session) ->
%%     %% HwIdList = get_all_hw(Rpc),
%%     HwId = get_hw(Rpc),
%%     ct:pal("# HwId : ~p", [HwId]),

%%     Sel_Products = get_selected_products(Rpc, HwId),
%%     ct:pal("# Selected Cxps for all HW : ~p", [Sel_Products]),
%%     SelCxps = get_sel_cxps(Sel_Products),

%%     RcsEECxp = get_rcs_cxp(Rpc, "RCSEE"),
%%     RcsMWCxp = get_rcs_cxp(Rpc, "RCSMW"),
%%     RcsCxps = RcsEECxp ++ RcsMWCxp,
%%     ct:log("RcsCxps from /home/sirpa/software/cxp90312*:~n ~p", [RcsCxps]),
%%     AllSelCxps = SelCxps++RcsCxps,
%%     ct:pal("## AllSelCxps: ~p", [AllSelCxps]),

%%     SwInventory = get_installed_cxps(NC_Session),
%%     Installed_CXPs = get_cxps_from_swinventory(SwInventory),

%%     match_and_remove_cxps(AllSelCxps, 
%% 			  Installed_CXPs, 
%% 			  SelCxps, 
%% 			  NC_Session),
%%     ok.



%%%--------------------------------------------------------------------
%%% get_rcs_cxp
%%%--------------------------------------------------------------------
get_rcs_cxp(Rpc, RCSCXP) ->
    SwPath = "/home/sirpa/software/",
    Grep = case RCSCXP of
	       "RCSEE" ->
		   RcsCxp = "cxp90312*",
		   A = rct_rpc:call(Rpc, os, cmd, 
				    ["grep "++RCSCXP++" "++SwPath++RcsCxp], 
				    10000, print),
		   A;
	       "RCSMW" ->
		   RcsCxp = "cxs101549*.xml",
		   B = rct_rpc:call(Rpc, os, cmd, 
				    ["grep -m 1 "++RCSCXP++" "++SwPath++RcsCxp], 
				    10000, print),
		   B
	   end,
    ct:log("## Grep for : ~p", [RCSCXP]),
    
    %% B = string:tokens(A, "\" </>\n_"),
    C = string:tokens(Grep, "\" </>\n"),
    ct:log("GrepList: ~p", [C]),
    get_name_id_version(C).



%%%--------------------------------------------------------------------
%%% get_name_id_version
%%%--------------------------------------------------------------------
get_name_id_version(List) ->
    L = lists:dropwhile(fun(X) ->
    				X =/= "name="
    			end, List),
    ct:pal("L: ~p", [L]),
    ["name=", Name | _] = L,

    M = lists:dropwhile(fun(Y) ->
    				Y =/= "id="
    			end, List),
    ct:pal("M: ~p", [M]),
    ["id=", Id | _] = M,

    N = lists:dropwhile(fun(Z) ->
    				Z =/= "version="
    			end, List),
    ct:pal("N: ~p", [N]),
    ["version=", Version | _] = N,

    CXP = Name++"_"++Id++"_"++Version,
    ct:pal("# ~p", [CXP]),
    CXP.

%%--------------------------------------------------------------------
%% @doc
%% get_hw
%% @end
%%--------------------------------------------------------------------
get_hw(Rpc) ->
    HwId = rct_rpc:call(Rpc, swmBoardList, boardType, [],
			      10000, print),
    ct:pal("# HwId : ~p",[HwId]),
    HwId.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% get_all_hw
%% %% @end
%% %%--------------------------------------------------------------------
%% get_all_hw(Hook) ->
%%     case aic_httpc:check_if_vc_board() of
%% 	"yes" -> 
%% 	    %% ok = rct_cli:connect(cli),
%% 	    %% {ok, A} = rct_cli:send(cli, 
%% 	    %% 		     "show ManagedElement=1,SystemFunctions=1,HwInventory=1,HwItems", 
%% 	    %% 		     print),
%% 	    %% ok = rct_cli:disconnect(cli),
%% 	    %% ct:log("# A : ~p",[A]),
%% 	    ct:pal("# To be done!", []),
%% 	    AllHwList= dummy;
%% 	_Other ->
%% 	    AllHwList = rct_rpc:call(Hook, swmBoardList, all_hw, [],
%% 			      10000, print),
%% 	    ct:pal("# AllHwList : ~p",[AllHwList]),
%% 	    AllHwList
%%     end,
%%     AllHwList.


%%--------------------------------------------------------------------
%% @doc
%% get_selected_cxps
%% @end
%%--------------------------------------------------------------------
get_selected_products(Hook, HwIdTuple) ->
    {ok, CXPs} = rct_rpc:call(Hook, swmBoardList, products, [HwIdTuple],
			      10000, print),
    ct:pal("# Selected CXPs for HwID : ~p, ~n~p",[HwIdTuple,  CXPs]),
    CXPs.


%%--------------------------------------------------------------------
%% @doc
%% get_swinventory
%% @end
%%--------------------------------------------------------------------
get_swinventory(CliHook) ->
    ok = rct_cli:connect(CliHook),
    {ok, A} = rct_cli:send(CliHook, 
		     "show ManagedElement=1,SystemFunctions=1,SwInventory=1", 
		     print),
    ok = rct_cli:disconnect(CliHook),
    ct:log("# A : ~p",[A]),
    
    SwInvList = fix_string(A, "\n\r\"> "),
    ct:log("# SwInv : ~p",[SwInvList]),
    
    B = lists:map(fun(Element) ->
			  case re:run(Element, "SwItem=") of
			      {match, _} ->
				  Element;
			      nomatch ->
				  ""
			  end
		  end, SwInvList),
    ct:log("# B : ~p",[B]),
    
    C = lists:flatten(B),
    ct:log("# C : ~p",[C]),

    SwItemCxpList = fix_string(C, "SwItem="),
    ct:pal("# SwItemCxpList : ~p", [SwItemCxpList]),
    SwItemCxpList.


%%%--------------------------------------------------------------------
%%% get_sel_cxps
%%%--------------------------------------------------------------------
%% [{{"DUMMY-ARM","CXP9021691_3","R5B01"},{global,"DUMMY-ARM_CXP9021691_3.cxp"}}
%% get_sel_cxps(Sel_Products) ->
%%     SelCxps = lists:map(fun({{_Name, CXP, REV},{_Xml, _CXP_NAME}}) ->
%% 				Cxp = re:replace(CXP,"_","/",[{return,list}]),
%% 				CxpAndRev = Cxp++"-"++REV,
%% 				ct:pal("# Fixed CxpAndRev : ~p", [CxpAndRev]),
%% 				CxpAndRev
%% 			end, Sel_Products),
%%     ct:pal("# Selected CXPs frpm algorithm : ~p", [SelCxps]),
%%     SelCxps.

get_sel_cxps(Sel_Products) ->
    SelCxps = lists:map(fun({{Name, CXP, REV},{_Xml, _CXP_NAME}}) ->
				NameCxpRev = Name++"_"++CXP++"_"++REV,
				ct:pal("# NameCxpRev : ~p", [NameCxpRev]),
				NameCxpRev
			end, Sel_Products),
    ct:pal("# Selected CXPs from algorithm : ~p", [SelCxps]),
    SelCxps.

%%%--------------------------------------------------------------------
%%% get_installed_cxps
%%%--------------------------------------------------------------------
get_installed_cxps(Rpc) ->
    LS = rct_rpc:call(Rpc, os, cmd, ["ls /home/sirpa/software/"],10000),
    %% ct:pal("LS : ~p", [LS]),
    InstCxps = string:tokens(LS, "\n"),
    ct:log("ls /home/sirpa/software/ : ~p", [InstCxps]),
    InstCxps.

%% get_installed_cxps(NcSess) ->
%%     MeId = "1",

%%     Get_SwInventory = {'ManagedElement',
%%     		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%%     		       [{managedElementId,[],[MeId]},
%%     			{'SystemFunctions',
%%     			 [{systemFunctionsId,[],["1"]},
%%     			  {'SwInventory',[],
%%     			   [{swInventoryId,[],["1"]}
%%     			   ]}]}]},
%%     {ok, SwInventory} = netconf(NcSess, get_config, 
%% 				[running, Get_SwInventory]),
%%     ct:pal("# SwInventory : ~p", [SwInventory ]),

%%     [{'ManagedElement',
%%      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%%      [{managedElementId,[],["1"]},
%%       {'SystemFunctions',[],
%%           [{systemFunctionsId,[],["1"]},
%%            {'SwInventory',
%%                [{xmlns,"urn:com:ericsson:ecim:RcsSwIM"}],
%% 	    SW_Inventory}]}]}] = SwInventory,
%%     ct:pal("# Inventory : ~p", [SW_Inventory ]),

%%     SW_Inventory .


%%%--------------------------------------------------------------------
%%% get_cxps_from_swinventory
%%%--------------------------------------------------------------------
get_cxps_from_swinventory(SwInventory) ->
    Sw_Versions =
	[X || {'SwItem',[],[{swItemId,[],[X]}]} <- SwInventory],
    ct:pal("# Installed CXPs : ~p", [Sw_Versions]),
    Sw_Versions.



%%%--------------------------------------------------------------------
%%% match_and_remove_cxps
%%%--------------------------------------------------------------------
match_and_remove_cxps([], [], _NC_Session) ->
    ok;

%% %% If there exist cxp after expected is removed.
%% %% Search for MW and EE CXP:er in LeftOver, Note differs between TCU and DUS
%% match_and_remove_cxps([], LeftOver, SelCxps, NC_Session) ->
%%     ct:pal("# LeftOver :  ~p", [LeftOver]),
%%     RCS_CXP = get_correct_rcs_cxp(SelCxps),
%%     %% RcsEECxp = get_rcs_cxp("RCSEE"),
%%     %% RcsMWCxp = get_rcs_cxp("RCSMW"),
%%     %% RcsCxps = RcsEECxp ++ RcsMWCxp,
%%     %% ct:pal("RcsCxps from /home/sirpa/software/cxp90312*:~n ~p", [RcsCxps]),
%%     RcsCxps = get_switem_consistsof(NC_Session, RCS_CXP),
%%     match_and_remove_cxps(RcsCxps, LeftOver, SelCxps, NC_Session);

match_and_remove_cxps([Cxp | CxpRest], RcvdCxpList, NC_Session) ->
    ct:log("Cxp : ~p", [Cxp]),
    true = lists:member(Cxp, RcvdCxpList),
    NewRcvdCxpList = lists:delete(Cxp, RcvdCxpList),
    ct:log("NewRcvdCxpList : ~p", [NewRcvdCxpList]),
    match_and_remove_cxps(CxpRest, NewRcvdCxpList, NC_Session).


%% %%%--------------------------------------------------------------------
%% %%% get_correct_rcs_cxp
%% %%%--------------------------------------------------------------------
%% get_correct_rcs_cxp(SelCxps) ->

%%     BoardType =
%%     	proplists:get_value(board_type,
%%     			    ct:get_config(
%%     			      ct:get_config({test_nodes,1}))),
%%     ct:pal("BoardType : ~p", [BoardType]),
%%     RCS_CXP = case BoardType of
%%     		  BoardType when BoardType == "tcu03";
%%     				 BoardType == "tcu0401" ->
%%     		      "CXP9031274/4";
%%     		  _Dus ->
%%     		      "CXP9031275/3"% dus5201, dus3201?
%%     	      end,

%%     ct:pal("Search for RCS CXP: ~p", [RCS_CXP]),
%%     ct:log("SelCxps: ~p", [SelCxps]),

%%     List = lists:dropwhile(fun(X) ->
%% 				   case re:run(X, RCS_CXP) of
%% 				       {match, _} ->
%% 				   	   false;
%% 				       nomatch ->
%% 				   	   true
%% 				   end
%% 			   end, SelCxps),
%%     ct:log("BuIdList: ~p", [List]),
    
%%     [RcsCxp | _] = List,
%%     ct:pal("RCS CXP: ~p", [RcsCxp]),
%%     RcsCxp.

%%%--------------------------------------------------------------------
%%% get_switem_consistsof
%%%--------------------------------------------------------------------
get_switem_consistsof(NcSess, RCS_CXP) ->
    MeId = "1",

    Get_SwInventory = {'ManagedElement',
    		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    		       [{managedElementId,[],[MeId]},
    			{'SystemFunctions',
    			 [{systemFunctionsId,[],["1"]},
    			  {'SwInventory',[],
    			   [{swInventoryId,[],["1"]},
			    {'SwItem', [],
			     [{swItemId,[],[RCS_CXP]},
			     {'consistsOf', [], []} ]}
			 ]}]}]},

    {ok, SwInventory} = netconf(NcSess, get, 
				[ Get_SwInventory]),
    ct:log("# SwInventory SwItem : ~p", [SwInventory ]),

    [{'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',[],
          [{systemFunctionsId,[],["1"]},
           {'SwInventory',
               [{xmlns,"urn:com:ericsson:ecim:RcsSwIM"}],
	    [{swInventoryId,[],["1"]},
	     {'SwItem',[],
	      SwItemList}
    	    ]}]}]}] = SwInventory,

    ct:log("# SwItem : ~p", [SwItemList]),

    RcsCxpList = [A || {consistsOf, _ , _} = A <- SwItemList],
    ct:log("# RcsCxpList, ee and mw : ~p", [RcsCxpList]),

    RcsCxps = lists:map(fun({X, _Y, [Z]} ) ->
    				 case X of
    				     consistsOf ->
    					 CXP =lists:last(string:tokens(Z,"=")),
    					 CXP;
    				     _Other ->
    					 ""
    				 end
    			 end, RcsCxpList),
    ct:pal("# EE and MW cxps: ~p", [RcsCxps]),
    RcsCxps.


%%--------------------------------------------------------------------
%% @doc
%% break_auboot
%% @end
%%--------------------------------------------------------------------
break_auboot(Console, Power, BOARDTYPE) ->
    %% rct_rs232:login(Console),
    break_uboot_arm(Console, Power, BOARDTYPE, 3, 3),
    timer:sleep(2000),
    ct_telnet:send(Console,"printenv "),
    ct_telnet:expect(Console, "Environment").

%%--------------------------------------------------------------------
%% @doc
%% start_node_from_uboot(Console)
%% @end
%%--------------------------------------------------------------------
start_node_from_uboot(Console) ->
    ct:pal("Start node from uboot"),
    ct_telnet:send(Console,"rcs"), %% start up the node
    {ok, _} = ct_telnet:expect(Console, "du1 login", 
			       [{timeout,180000},no_prompt_check]),

    timer:sleep(10000).

%%--------------------------------------------------------------------
%% @doc
%% change_hw_id(Console, ProductName, ProductNumber, ProductRevision)
%% @end
%%--------------------------------------------------------------------
change_hw_id(Console, ProductNumber, ProductRevision) ->
    ct:pal("## Set ProductNumber and ProductRevision in AUBOOT."),
    ct:log("## ProductNumber: ~p", [ProductNumber]),
    ct:log("## ProductRevision: ~p", [ProductRevision]),

    %% ct_telnet:send(Console,"setenv productname "++ ProductName),
    ct_telnet:send(Console,"setenv productnumber "++ProductNumber),
    ct_telnet:send(Console,"setenv productrevision "++ProductRevision ),
    timer:sleep(2000),
    ct_telnet:send(Console,"saveenv"),
    ct_telnet:expect(Console, "Valid environment:"),

    timer:sleep(5000),
    ct:pal("## Printenv after changed uboot varaibles."),
    ct_telnet:send(Console,"printenv "),
    ct_telnet:expect(Console, "Environment"),
    timer:sleep(5000).



%%%%% Internal
%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------
netconf(Session, F, A) ->
    {ok, _} = ct_netconfc:open(Session, []),
    Res = apply(ct_netconfc, F, [Session | A]),			   
    %% ok = ct_netconfc:close_session(Session),
    ct_netconfc:close_session(Session),
    Res.


%% %%%--------------------------------------------------------------------
%% %%% break_uboot_arm
%% %%%--------------------------------------------------------------------
%% break_uboot_arm(_Console,_Power,_,0,_) ->
%%     ct:log(lightred, "Could not break board in uboot"),
%%     ct:fail("Could not break uboot");
%% break_uboot_arm(Console,Power, BOARDTYPE,N,M) ->
%%     case N < M of
%% 	true ->
%% 	    ct:log(yellow,"Could not break board in uboot, retry ~p more times",[N]);
%% 	_ ->
%% 	    ok
%%     end,
%%     ct_telnet:get_data(Console), %flush console not to hit old printouts
%%     case rct_power:cycle(Power) of
%% 	ok ->
%% 	    Uboot = case BOARDTYPE of
%% 			"tcu03" ->
%% 			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/03.*\\)",
%% 							    "Ericsson Version: 4/CXC1736593/03.*\\)",
%% 							    "Ericsson Version: 7/CXC1736593/03.*\\)"], [{timeout,30000}]) of
%% 				{ok, Match} ->
%% 				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/03_(.*) \\(",[{capture,[1],list}]) of
%% 					{match,[Rev]} ->
%% 					    ct:pal("Uboot: ~s",[Rev]),
%% 					    other
%% 				    end;
%% 				_ ->
%% 				    error
%% 			    end;
%% 			"tcu0401" ->
%% 			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/04.*\\)",
%% 							    "Ericsson Version: 4/CXC1736593/04.*\\)",
%% 							    "Ericsson Version: 7/CXC1736593/04.*\\)"], [{timeout,30000}]) of
%% 				{ok, Match} ->
%% 				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/04_(.*) \\(",[{capture,[1],list}]) of
%% 					{match,[Rev]} ->
%% 					    ct:pal("Uboot: ~s",[Rev]),
%% 					    other
%% 				    end;
%% 				_ ->
%% 				    error
%% 			    end;
%% 			"dus5201" ->
%% 			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/52.*\\)",
%% 							    "Ericsson Version: 4/CXC1736593/52.*\\)",
%% 							    "Ericsson Version: 7/CXC1736593/52.*\\)"],[{timeout,30000}]) of
%% 				{ok, Match} ->
%% 				    ct:pal("Match: ~p", [Match]),
%% 				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/52_(.*) \\(",[{capture,[1],list}]) of
%% 					{match,[Rev]} ->
%% %% 					    ct:pal("Uboot: ~s",[Rev]),
%% 					    other
%% 				    end;
%% 				_ ->
%% 				    error
%% 			    end;
%% 			"dus3201" ->
%% 			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/32.*\\)", 
%% 							    "Ericsson Version: 4/CXC1736593/32.*\\)",
%% 							    "Ericsson Version: 7/CXC1736593/32.*\\)"],[{timeout,30000}]) of
%% 				{ok, Match} ->
%% 				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/32_(.*) \\(",[{capture,[1],list}]) of
%% 					{match,[Rev]} ->
%% 					    ct:pal("Uboot: ~s",[Rev]),
%% 					    other
%% 				    end;
%% 				_ ->
%% 				    error
%% 			    end
%% 		    end,
%% 	    case Uboot of
%% 		error ->
%% 		    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M);
%% 		Uboot ->
%% 		    case ct_telnet:expect(Console, "Hit any key to stop autoboot:", [{timeout,30000},no_prompt_check]) of
%% 			{ok, _} ->
%% 						%		    timer:sleep(1000),
%% 			    ct_telnet:send(Console, "\n"),
%% 			    case ct_telnet:expect(Console, "=> $", [{timeout,10000},no_prompt_check]) of
%% 				{ok, _} ->
%% 				    Uboot;
%% 				_ ->
%% 				    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
%% 			    end;
%% 			_ ->
%% 			    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
%% 		    end
%% 	    end;
%% 	_ ->
%% 	    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
%%     end.


%%%--------------------------------------------------------------------
%%% break_uboot_arm
%%%--------------------------------------------------------------------
break_uboot_arm(_Console,_Power,_,0,_) ->
    ct:log(lightred, "Could not break board in uboot"),
    ct:fail("Could not break uboot");
break_uboot_arm(Console,Power, BOARDTYPE,N,M) ->
    case N < M of
	true ->
	    ct:log(yellow,"Could not break board in uboot, retry ~p more times",[N]);
	_ ->
	    ok
    end,
    ct_telnet:get_data(Console), %flush console not to hit old printouts
    case rct_power:cycle(Power) of
	ok ->
	    Uboot = case BOARDTYPE of
			BOARDTYPE when BOARDTYPE == "tcu03";
				       BOARDTYPE == "tcu0401";
				       BOARDTYPE == "dus5201";
				       BOARDTYPE == "dus5301";
				       BOARDTYPE == "dus3201" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: "++"[0-9]/CXC1736593/.*\\("], [{timeout,30000}]) of
				{ok, Match} ->
				    %% ct:pal("Match: ~p", [Match]),
				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/[0-9][0-9]_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end
		    end,
	    case Uboot of
		error ->
		    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M);
		Uboot ->
		    case ct_telnet:expect(Console, "Hit any key to stop autoboot:", [{timeout,30000},no_prompt_check]) of
			{ok, _} ->
						%		    timer:sleep(1000),
			    ct_telnet:send(Console, "\n"),
			    case ct_telnet:expect(Console, "=> $", [{timeout,10000},no_prompt_check]) of
				{ok, _} ->
				    Uboot;
				_ ->
				    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
			    end;
			_ ->
			    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
		    end
	    end;
	_ ->
	    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
    end.
