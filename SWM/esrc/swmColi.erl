%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmColi.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R12A/1
%%%
%%% @doc ==SWM COLI commands==

-module(swmColi).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R12A/1').
-date('2017-10-23').
-author('etxpejn').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R5A/7      2016-04-14 etxberb     Changed tag name hwSwCompatibilityIndex to
%%%                                   hwSwCompatibility.
%%% ----------------------------------------------------------
%%% R6A/1      2016-04-21 etxberb     Bug fix in show_hal_sw.
%%% R6A/2      2016-09-21 etxjotj     Confirm restore
%%% ----------------------------------------------------------
%%% R7A/1      2016-09-29 etxjotj     Disable fallback timer reset
%%% ----------------------------------------------------------
%%% R8A/1      2016-10-26 etxpejn     Added signing_cert_update but unused
%%% R8A/2      2016-11-03 erarafo     Fixed rollback timer help text
%%% R8A/3      2017-01-15 etxpejn     Added WP5618 Signed sw Certificate Revocation & Anti-rollback 
%%% R8A/4      2017-01-15 etxpejn     Changed rcs_mode to rcs_mode_2
%%% ----------------------------------------------------------
%%% R9A/1      2017-01-27 etxjotj     Enable HSI
%%% ----------------------------------------------------------
%%% R10A/1     2017-06-10 etxjotj     HV92792 Enable soaking
%%% R10A/2     2017-06-12 etxjotj     Fixed soaking printouts
%%% R10A/3     2017-06-12 etxjotj     Fixed soaking printouts again
%%% R10A/4     2017-06-27 etxjotj     Activation complete msg resend
%%% ----------------------------------------------------------
%%% R12A/1     2017-10-23 etxpejn     Added enable_anti_rollback
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
-export([signing_cert_update/1]).
-export([soaking_time/1, force_soaking_expiry/1]).
-export([resend_activation_complete/1]).

%%% Only on unsecure boards
-export([set_housekeeping_delay/1, reset_housekeeping_delay/1,
	 set_rollback_timer/1, reset_rollback_timer/1,
	 set_restore_timer/1, reset_restore_timer/1]).
-export([reset_quarantine/1]).
-export([confirm_restore/1]).
-export([disable_fallback_timer_reset/1]).
-export([enable_hsi/1]).
-export([enable_anti_rollback/1]).

-define(HEAD, string:copies("=", 80)).
-define(HEAD_TOP, io:format("~n" ++ ?HEAD ++ "~n")).
-define(HEAD_BOT, io:format(?HEAD ++ "~n")).
-define(SUBHEAD, string:copies("-", 70)).
-define(SUBHEAD_TOP, io:format(?SUBHEAD ++ "~n")).
-define(TAB_2, "~-12s ~s~n").
-define(TAB_4, "~-12s ~-15s ~-10s ~s~n").

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
%%% @doc Disable fallback timer reset
%%% @end
%%% ----------------------------------------------------------

disable_fallback_timer_reset([]) ->
    swmLib:set_variable(disable_fallback_timer_reset, true),
    io:format("Fallback timer reset disabled!~n");
disable_fallback_timer_reset(_) ->
    io:format("Usage: disable-fallback-timer-reset~n").

%%% ----------------------------------------------------------
%%% @doc Set housekeeping delay for testing
%%% @end
%%% ----------------------------------------------------------

set_rollback_timer([WaitTimeStr]) ->
    try list_to_integer(WaitTimeStr) of
	WaitTime when WaitTime >= 0 ->
	    swmLib:set_variable(swm_test_rollback_timer, WaitTime),
	    io:format("Data conversion rollback timer is now ~w ms~n",[WaitTime]);
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
    case proplists:lookup(hwSwCompatibilityIndex, MD) of
	none ->
	    ?HEAD_TOP,
	    io:format("UP has no hwSwCompatibilityIndex~n"),
	    ?HEAD_BOT;
	{hwSwCompatibilityIndex, undefined} ->
	    ?HEAD_TOP,
	    io:format("UP has no hwSwCompatibilityIndex~n"),
	    ?HEAD_BOT;
	{hwSwCompatibilityIndex, HwIdx} ->
	    show_hal_sw(["idx",HwIdx])
    end;

show_hal_sw(["idx"]) ->
    show_hal_sw([]);
show_hal_sw(["idx" | HwIdxs]) ->
    HalDir = swmLib:software_hal_dir(),
    WC=filename:join([HalDir,"*/*hal.xml"]),
    case filelib:wildcard(WC) of
	[] ->
	    ?HEAD_TOP,
	    io:format("No HAL SW found~n"),
	    ?HEAD_BOT;
	XmlFiles ->
	    [[show_up(File, HwIdx) || File <- XmlFiles] || HwIdx <- HwIdxs]
    end,
    ok;
    
show_hal_sw(["all"]) ->
    HalDir = swmLib:software_hal_dir(),
    WC=filename:join([HalDir,"*/*hal.xml"]),
    case filelib:wildcard(WC) of
	[] ->
	    ?HEAD_TOP,
	    io:format("No HAL SW found~n"),
	    ?HEAD_BOT;
	XmlFiles ->
	    [show_up(File) || File <- XmlFiles]
    end,
    ok;
show_hal_sw(_) ->
    io:format("argument error~n"),
    ok.


confirm_restore([]) ->
    State = case swmLib:get_variable(confirm_restore) of
		true -> enabled;
		_ -> disabled
	    end,
    io:format("Confirm restore state is: ~w~n",[State]),
    ok;
confirm_restore(["disable"]) ->
    swmLib:set_variable(confirm_restore, false);
confirm_restore(["enable"]) ->
    swmLib:set_variable(confirm_restore, true);
confirm_restore(["enable-hard"]) ->
    io:format("This option is not supported yet",[]),
    ok.
		       

%%% ----------------------------------------------------------
%%% @doc Signing certificate status
%%% @end
%%% ----------------------------------------------------------
signing_cert_update([]) ->
    signing_cert_update(["status"]);
signing_cert_update(["status"]) ->
    case swmServer:get_sign_cert_timer() of
     	undefined ->
     	    io:format("No new software signing certificates are included in the current"
     		      " UP i.e. no countdown is ongoing~n~n"),
    	    io:format("~s~n", [swmOs:get_crl_ids(sysEnv:rcs_mode_2())]);
     	TimeLeft ->
	    io:format("New software signing certificates are included in the current UP "
		      "and will be auto confirmed at: ~p~n~n", [TimeLeft]),
    	    io:format("~s~n", [swmOs:get_crl_ids(sysEnv:rcs_mode_2())])
    end,
    ok;
signing_cert_update(["confirm"]) ->
    io:format("If a countdown is started it will be stopped and the new software "
     	      "signing certificate is confirmed and stored.~n"),
    swmServer:confirm_sign_cert(),
    ok;
signing_cert_update(["timeoutconfig", Timeout]) ->
    case swmServer:change_sign_cert_timer(list_to_integer(Timeout)) of
     	ok ->
    	    io:format("The timeout has been change to ~p days~n", 
		      [list_to_integer(Timeout)]);
     	nok ->
	    io:format("Not possible to change the timeout since a countdown is ongoing~n");
	nok_outofrange ->
    	    io:format("Valid range for timeout is 0-14 days~n")
    end,
    ok;
signing_cert_update(["timeoutreset"]) ->
    case swmServer:change_sign_cert_timer(default) of
     	ok ->
    	    io:format("The timeout has been change to default, 1 day~n", []);
	nok ->
	    io:format("Not possible to reset the timeout since a countdown is ongoing~n")
    end,
    ok;
signing_cert_update(E) ->
    io:format("argument error E: ~p~n", [E]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Enable HSI
%%% @end
%%% ----------------------------------------------------------

enable_hsi(_) ->
    swmLib:set_variable(hsi_state, on).

%%% ----------------------------------------------------------
%%% @doc Enable Anti rollback
%%% @end
%%% ----------------------------------------------------------

enable_anti_rollback(_) ->
    swmLib:set_variable(anti_rollback, on).


%%% ----------------------------------------------------------
%%% @doc Set soaking time
%%% @end
%%% ----------------------------------------------------------
soaking_time(["disable"]) ->
    swmLib:erase_variable(soaking_period);
soaking_time(["infinity"]) ->
    swmLib:set_variable(soaking_period, infinity);
soaking_time([]) ->
    case swmLib:get_variable(soaking_period) of
	undefined -> io:format("Fast restore mode time period is not set~n");
	infinity -> io:format("Fast restore mode time period is open ended~n");
	86400 ->
	    io:format("Fast restore mode period is 1 day~n");
	Number when Number rem 86400 == 0 ->
	    io:format("Fast restore mode time period is ~w days~n",
		      [Number div 86400]);
	Number ->
	    io:format("Fast restore mode time period is ~w seconds~n",[Number])
    end;
soaking_time(["s"++Number]) ->
    %% loophole for faster testing
    try list_to_integer(Number) of
	Int -> swmLib:set_variable(soaking_period, Int)
    catch error:badarg ->
	    io:format("Not in the correct format \"sNNNNN\"")
    end;
soaking_time([Number]) ->
    try list_to_integer(Number) of
	Int ->
	    swmLib:set_variable(soaking_period, Int*86400)
    catch error:badarg ->
	    io:format("Please enter a non zero positive integer or a valid input")
    end;    
soaking_time(_) ->
    io:format("Usage: period [Time] where Time = disable | infinity | NumberOfDays~n",[]).


%%% ----------------------------------------------------------
%%% @doc Forces soaking expirer
%%% @end
%%% ----------------------------------------------------------

force_soaking_expiry([]) ->
    swmServer:stop_soaking_timer(),
    swmServer:audit_software_directly();
force_soaking_expiry(_) ->
    io:format("Usage: expire~n").


%%% ----------------------------------------------------------
%%% @doc Resend activation complete
%%% @end
%%% ----------------------------------------------------------

resend_activation_complete([]) ->
    case swmLib:get_variable(resend_activation_complete) of
	disabled ->
	    io:format("Resending of activation complete messages is DISABLED~n",
		      []);
	_ ->
	    io:format("Resending of activation complete messages is ENABLED~n",
		      [])
    end;
resend_activation_complete(["once"]) ->
    case swmServer:resend_activation_complete() of
	{ok, UpKeys} ->
	    [io:format("Message sent for UpgradePackage ~p ~n",
		       [element(4,UpKey)])||UpKey<-UpKeys],
	    ok;
	{error, Msg} ->
	    io:format("ERROR: ~s~n",[Msg])
    end;
resend_activation_complete(["disable"]) ->
    swmLib:set_variable(resend_activation_complete, disabled);
resend_activation_complete(["enable"]) ->
    swmLib:erase_variable(resend_activation_complete);
resend_activation_complete(_) ->
    io:format("Usage: resend-activation-complete [Arg]~n",[]).


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
	    R = parse_elements(Content, []),
	    print_hal_info(R)
    end.

show_up(File,HwIdx) ->
    case xmerl_scan:file(File) of
	{error,Error} ->
	    io:format("Error (~p) in HAL UP file ~p~n",[Error,File]);
	{#xmlElement{content=Content},[]} ->
	    R = parse_elements(Content, []),
	    print_hal_info(R, HwIdx)
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
    {hwSwCompatibility,H,[]}=proplists:lookup(hwSwCompatibility,R),
    {index,Idx}=proplists:lookup(index,H),
    if
	HwIdx == Idx ->
	    print_hal_info(R);  
	true ->
	    ?HEAD_TOP,
	    io:format("No HAL SW found for hwSwCompatibilityIndex ~s~n",
		      [HwIdx]),
	    ?HEAD_BOT,
	    ok
    end.
print_hal_info(R) ->
    print_product(R),
    print_hwSwCompatibilityIndex(R),
    io:format("ContentInfo~n"),
    io:format("===========~n"),
    print_contentinfo(R),
    io:format("~nBoardlists~n"),
    io:format("==========~n"),
    print_boardLists(R).

print_product(R) ->
    {product,P,[]}=proplists:lookup(product,R),
    {name,Name}=proplists:lookup(name,P),
    {id,Id}=proplists:lookup(id,P),
    {version,Version}=proplists:lookup(version,P),
    ?HEAD_TOP,
    io:format("HAL UP -  Name: ~s   Id: ~s   Version:  ~s~n",[Name,Id,Version]),
    ?HEAD_BOT.
    

print_hwSwCompatibilityIndex(R)->
    {hwSwCompatibility,H,[]}=proplists:lookup(hwSwCompatibility,R),
    {index,Idx}=proplists:lookup(index,H),
    io:format("HwSwCompatibility -  Index: ~s~n~n", [Idx]).

    
print_contentinfo(R)->
    {contentinfo,[],C}=proplists:lookup(contentinfo,R),
    io:format(?TAB_4, ["Name","Id","Version","Filename"]),    
    print_ci_products(C).

print_ci_products([])->ok;
print_ci_products([{product,P,[]}|T])->
    {name,Name}=proplists:lookup(name,P),
    {id,Id}=proplists:lookup(id,P),
    {version,Version}=proplists:lookup(version,P),
    {filename,Filename}=proplists:lookup(filename,P),
    io:format(?TAB_4, [Name,Id,Version,Filename]),    
    print_ci_products(T).
    


print_boardLists(R)->
    {boardLists,[],BL}=proplists:lookup(boardLists,R),
    print_boardList(BL).

    
print_boardList([])-> ok;
print_boardList([{boardList,HWC,BLD}|T]) ->
    {hwcategory,HwCategory}=lists:keyfind(hwcategory,1,HWC),
    {hwmodel,HwModel}=lists:keyfind(hwmodel,1,HWC),
    ?SUBHEAD_TOP,
    io:format("BoardList -  HwCategory: ~s   HwModel: ~s ~n",
	      [HwCategory, HwModel]),
    io:format("~n" ++ ?TAB_2, ["ProdNo","Revision"]),    
    print_board(BLD),
    io:format("~n" ++ ?TAB_4, ["Name","Id","Version","Filename"]),    
    print_cxp(BLD),
    io:format("~n"),
    print_boardList(T).
    
print_board([])-> ok; 
print_board([{boardType,Pr,[]}|T]) ->
    {productNumber,PrNo}=lists:keyfind(productNumber,1,Pr),
    {revision,Rev}=lists:keyfind(revision,1,Pr),
    io:format(?TAB_2, [PrNo,Rev]),    
    print_board(T);
print_board([_|T]) ->
    print_board(T).

    
    
print_cxp([])-> ok; 
print_cxp([{product,P,[]}|T]) ->
    {name,Name}=proplists:lookup(name,P),
    {id,Id}=proplists:lookup(id,P),
    {version,Version}=proplists:lookup(version,P),
    {filename,Filename}=proplists:lookup(filename,P),
    io:format(?TAB_4, [Name,Id,Version,Filename]),    
    print_cxp(T);
print_cxp([_|T]) ->
    print_cxp(T).



%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
