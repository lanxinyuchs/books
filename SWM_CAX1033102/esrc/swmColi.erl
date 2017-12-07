%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmColi.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/6
%%%
%%% @doc ==SWM COLI commands==

-module(swmColi).
-vsn('/main/R2A/R3A/R4A/R5A/6').
-date('2016-03-14').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
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
%%% R2A/1      2014-10-15 etxjotj     Created
%%% R3A/2      2015-06-05 etxjotj     Restore cancel
%%% R4A/1      2015-08-31 etxjotj     Use sysInit printouts
%%% R4A/2      2014-11-26 etxjotj     Ignore UP limit
%%% R5A/1      2016-01-14 etxjotj     Reset db monitor quarantine
%%% R5A/4      2016-01-25 etxpejn     Corr call to swmDbMonitor:quarantine_reset
%%% R5A/5      2016-03-02 etxjotj     Perform backup housekeeping
%%% R5A/6      2016-03-14 etxarnu     Added show_hal_sw/1
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% The real deal
-export([disable_max_up_check/1, enable_max_up_check/1]).
-export([perform_backup_housekeeping/1]).
-export([show_hal_sw/1]).

%%% Only on unsecure boards
-export([set_housekeeping_delay/1, reset_housekeeping_delay/1,
	 set_rollback_timer/1, reset_rollback_timer/1,
	 set_restore_timer/1, reset_restore_timer/1]).

-export([reset_quarantine/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% # 2.3   IMPORT OF DEFINITIONS
-include_lib("xmerl/include/xmerl.hrl").

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

disable_max_up_check([]) ->
    swmLib:set_ram_variable(disable_max_up_check, true);
disable_max_up_check(_) ->
    io:format("Usage: disable-max-up-check~n").
    
enable_max_up_check([]) ->
    swmLib:erase_ram_variable(disable_max_up_check);
enable_max_up_check(_) ->
    io:format("Usage: enable-max-up-check~n").


perform_backup_housekeeping([]) ->
    %% Do a direct case here to circumvent waiting period
    gen_server:cast(swmBackup, internal_housekeeping).

%%% ----------------------------------------------------------
%%% @doc Reset quarantine
%%% @end
%%% ----------------------------------------------------------

reset_quarantine([]) ->
    swmDbMonitor:quarantine_reset(),
    ok;
reset_quarantine(_) ->
    io:format("Usage: reset-quarantine~n").

    

%%% ----------------------------------------------------------
%%% @doc Set housekeeping delay for testing
%%% @end
%%% ----------------------------------------------------------

set_housekeeping_delay([WaitTimeStr]) ->
    try list_to_integer(WaitTimeStr) of
	WaitTime when WaitTime >= 0 ->
	    swmLib:set_variable(swm_test_housekeeping_delay, WaitTime),
	    io:format("Internal housekeeping delay is now ~w ms~n",[WaitTime]);
	_ ->
	    io:format("~s is not a positive integer~n", [WaitTimeStr])
    catch error:badarg ->
	    io:format("~s is not a positive integer~n", [WaitTimeStr])
    end;
set_housekeeping_delay(_) ->
    io:format("Usage: set-housekeeping-delay WaitTime~n").


%%% ----------------------------------------------------------
%%% @doc Reset housekeeping delay
%%% @end
%%% ----------------------------------------------------------

reset_housekeeping_delay([]) ->
    swmLib:erase_variable(swm_test_housekeeping_delay),
    io:format("Internal housekeeping delay is now reset~n");
reset_housekeeping_delay(_) ->
    io:format("Usage: reset-houskeeping-delay~n").

%%% ----------------------------------------------------------
%%% @doc Set housekeeping delay for testing
%%% @end
%%% ----------------------------------------------------------

set_rollback_timer([WaitTimeStr]) ->
    try list_to_integer(WaitTimeStr) of
	WaitTime when WaitTime >= 0 ->
	    swmLib:set_variable(swm_test_rollback_timer, WaitTime),
	    io:format("Internal housekeeping delay is now ~w ms~n",[WaitTime]);
	_ ->
	    io:format("~s is not a positive integer~n", [WaitTimeStr])
    catch error:badarg ->
	    io:format("~s is not a positive integer~n", [WaitTimeStr])
    end;
set_rollback_timer(_) ->
    io:format("Usage: set-rollback-timer WaitTime~n").


%%% ----------------------------------------------------------
%%% @doc Reset housekeeping delay
%%% @end
%%% ----------------------------------------------------------

reset_rollback_timer([]) ->
    swmLib:erase_variable(swm_test_rollback_timer),
    io:format("Data conversion rollback timer is now reset~n");
reset_rollback_timer(_) ->
    io:format("Usage: reset-rollback-timer~n").

%%% ----------------------------------------------------------
%%% @doc Set restore cancel delay
%%% @end
%%% ----------------------------------------------------------

set_restore_timer([WaitTimeStr]) ->
    try list_to_integer(WaitTimeStr) of
	WaitTime when WaitTime >= 0 ->
	    swmLib:set_variable(swm_test_restore_timer, WaitTime),
	    io:format("Cancel restore timer is now ~w ms~n",[WaitTime]);
	_ ->
	    io:format("~s is not a positive integer~n", [WaitTimeStr])
    catch error:badarg ->
	    io:format("~s is not a positive integer~n", [WaitTimeStr])
    end;
set_restore_timer(_) ->
    io:format("Usage: set-restore-timer WaitTime~n").


%%% ----------------------------------------------------------
%%% @doc Reset housekeeping delay
%%% @end
%%% ----------------------------------------------------------

reset_restore_timer([]) ->
    swmLib:erase_variable(swm_test_restore_timer),
    io:format("Cancel restore timer is now reset~n");
reset_restore_timer(_) ->
    io:format("Usage: reset-restore-timer~n").


%%% ----------------------------------------------------------
%%% @doc Show HAL UP contents
%%% @end
%%% ----------------------------------------------------------
show_hal_sw([]) ->
    MD=swmLib:get_current_up_metadata(),
    case proplists:lookup(hwSwCompatibilityIndex,MD) of
	none ->
	    io:format("UP has no hwSwCompatibilityIndex ~n",[]);
	{hwSwCompatibilityIndex,undefined} ->
	    io:format("UP has no hwSwCompatibilityIndex ~n",[]);
	{hwSwCompatibilityIndex,HwIdx} ->
	    show_hal_sw(["idx",HwIdx])
    end;

show_hal_sw(["idx",HwIdx]) ->
    HalDir = swmLib:software_hal_dir(),
    WC=filename:join([HalDir,"*/*hal.xml"]),
    XmlFiles=filelib:wildcard(WC),
    [show_up(File,HwIdx) || File <- XmlFiles ],
    ok;
    
show_hal_sw(["all"]) ->
    HalDir = swmLib:software_hal_dir(),
    WC=filename:join([HalDir,"*/*hal.xml"]),
    XmlFiles=filelib:wildcard(WC),
    [show_up(File) || File <- XmlFiles ],
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

show_up(File) ->
    case xmerl_scan:file(File) of
	{error,Error} ->
	    io:format("Error (~p) in HAL UP file ~p~n",[Error,File]);
	{#xmlElement{content=Content},[]} ->
	    R = parse_elements( Content, []),
	    print_hal_info(R)
    end.

show_up(File,HwIdx) ->
    case xmerl_scan:file(File) of
	{error,Error} ->
	    io:format("Error (~p) in HAL UP file ~p~n",[Error,File]);
	{#xmlElement{content=Content},[]} ->
	    R = parse_elements( Content, []),
	    print_hal_info(R,HwIdx)
    end.

parse_elements([],Acc) ->
    lists:reverse(Acc);
parse_elements([#xmlElement{name = Name,
			    attributes = Attributes,
			    content = Content} | T] , Acc) ->
    parse_elements(T,[ {Name,
			parse_attributes(Attributes,[]) ,
			parse_elements(Content,[])
		       } | Acc]);
parse_elements([_H|T],Acc) ->
    parse_elements(T,Acc).


%%% ----------------------------------------------------------
parse_attributes([],Acc) ->
    lists:reverse(Acc);
parse_attributes([#xmlAttribute{ name = Name, value = Value}|T],Acc) ->
    parse_attributes(T,[{Name,Value}|Acc]).

print_hal_info(R,HwIdx) ->
    {hwSwCompatibilityIndex,H,[]}=proplists:lookup(hwSwCompatibilityIndex,R),
    {index,Idx}=proplists:lookup(index,H),
    if
	HwIdx == Idx ->
	    print_hal_info(R);  
	true ->
	    ok
    end.
print_hal_info(R) ->
    print_product(R),
    print_hwSwCompatibilityIndex(R),
    io:format("ContentInfo~n",[]),
    io:format("===========~n",[]),
    print_contentinfo(R),
    io:format("~nBoardlists~n",[]),
    io:format("==========~n",[]),
    print_boardLists(R).

print_product(R) ->
    {product,P,[]}=proplists:lookup(product,R),
    {name,Name}=proplists:lookup(name,P),
    {id,Id}=proplists:lookup(id,P),
    {version,Version}=proplists:lookup(version,P),
    io:format("~n=========================================================~n",[]),
    io:format("HAL UP -  Name: ~s   Id: ~s   Version:  ~s~n",[Name,Id,Version]),
    io:format("=========================================================~n",[]).
    

print_hwSwCompatibilityIndex(R)->
    {hwSwCompatibilityIndex,H,[]}=proplists:lookup(hwSwCompatibilityIndex,R),
    {index,Idx}=proplists:lookup(index,H),
    io:format("HwSwCompatibilityIndex:  ~s~n~n",[Idx]).

    
print_contentinfo(R)->
    {contentinfo,[],C}=proplists:lookup(contentinfo,R),
    io:format("~-12s ~-15s ~-10s ~-100s~n",
	      ["Name","Id","Version","Filename"]),    
    print_ci_products(C).

print_ci_products([])->ok;
print_ci_products([{product,P,[]}|T])->
    {name,Name}=proplists:lookup(name,P),
    {id,Id}=proplists:lookup(id,P),
    {version,Version}=proplists:lookup(version,P),
    {filename,Filename}=proplists:lookup(filename,P),
    io:format("~-12s ~-15s ~-10s ~-100s~n",
	      [Name,Id,Version,Filename]),    
    print_ci_products(T).
    


print_boardLists(R)->
    {boardLists,[],BL}=proplists:lookup(boardLists,R),
    print_boardList(BL).

    
print_boardList([])-> ok;
print_boardList([{boardList,HWC,BLD}|T]) ->
    {hwcategory,HwCategory}=lists:keyfind(hwcategory,1,HWC),
    {hwmodel,HwModel}=lists:keyfind(hwmodel,1,HWC),
    io:format("------------------------------------------------~n",[]),
    io:format("HwCategory: ~s   HwModel: ~s ~n",[HwCategory,HwModel]),
    io:format("~n~-12s ~-15s~n", ["ProdNo","Revision"]),    
    print_board(BLD),
    io:format("~n~-12s ~-15s ~-10s ~-100s~n",
	      ["Name","Id","Version","Filename"]),    
    print_cxp(BLD),
    print_boardList(T).
    
print_board([])-> ok; 
print_board([{boardType,Pr,[]}|T]) ->
    {productNumber,PrNo}=lists:keyfind(productNumber,1,Pr),
    {revision,Rev}=lists:keyfind(revision,1,Pr),
    io:format("~-12s ~-15s~n", [PrNo,Rev]),    
    print_board(T);
print_board([_|T]) ->
    print_board(T).

    
    
print_cxp([])-> ok; 
print_cxp([{product,P,[]}|T]) ->
    {name,Name}=proplists:lookup(name,P),
    {id,Id}=proplists:lookup(id,P),
    {version,Version}=proplists:lookup(version,P),
    {filename,Filename}=proplists:lookup(filename,P),
    io:format("~-12s ~-15s ~-10s ~-100s~n",
	      [Name,Id,Version,Filename]),    
    print_cxp(T);
print_cxp([_|T]) ->
    print_cxp(T).



%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
