%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	check_HW_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/1
-module(check_HW_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/1').
-date('2015-05-25').
-author('etxkols').
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
%%% R2A/1      2014-06-04 etxkols     Created
%%% R2A/2      2015-05-25 etxkols     Fix compiler warning
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 check_HW/1,
	 check_HW/2]).
%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_logging, rct_core
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_consserv,cs1},
                 {rct_rs232,console},                 
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		 {rct_core,[]}]}].

%% @hidden
init_per_suite(Config) -> 
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [check_HW].

%%--------------------------------------------------------------------
%% @doc
%% Checks Cobra/Taipan and Trinity states.
%% @end
%%--------------------------------------------------------------------
check_HW(_) ->
    ok = rct_rs232:login(console),
    BOARDTYPE = ct:get_config({ct:get_config({test_nodes,1}),board_type}),
    check_HW(console,BOARDTYPE).

%% @spec check_HW(Console, BOARDTYPE) -> ok | term()
%% Console = atom()
%% BOARDTYPE = string()
%% @doc Exported function to be used from other testsuites that checks Cobra/Taipan and Trinity states.
%%
%% This function is intended to be called from other testsuites
%% and if checks fails, ct:fail/1 is called.
%%
%% Example: check_HW_SUITE:check_HW(console,"dus5201").
%% @end
check_HW(Console, BOARDTYPE) when BOARDTYPE == "tcu03";
				  BOARDTYPE == "dus5201";
				  BOARDTYPE == "dus3201" ->						
    Is_Restructured = is_restructured(Console),
    su(Console,Is_Restructured),
    Result1 = check_FPGA(Console),
    case BOARDTYPE of
	BOARDTYPE when BOARDTYPE == "dus5201";
		       BOARDTYPE == "dus3201" ->
	    {ProductName,ProductRevision} = get_board_info(Console),
	    Result2 = check_npumac_status(Console,ProductName,ProductRevision),
	    Result3 = check_bbmif(Console, ProductRevision);
	_ ->
	    Result2 = ok,
	    Result3 = ok
    end,
    exit_su(Console,Is_Restructured),
    case lists:keysearch(error,1,[Result1,Result2,Result3]) of
	{value,{error,Reason}} ->
	    ct:fail(Reason);
	false ->
	    ok
    end;
check_HW(_Console, _BOARDTYPE) ->
    ok.
    	    
check_FPGA(Console) ->
    {ok,IcmStatus} = ct_telnet:cmd(Console,"testbox icm_status --verbose 2"),
    FPGAState = "FPGA state: CONFIGURED",
    FPGAFW = "FPGA FW: CORRECT",
    case [lists:member(FPGAState,IcmStatus),
	  lists:member(FPGAFW,IcmStatus)] of
	[true,true] ->
	    ct:log("Cobra/Taipan state ok~n~s~n~s",[FPGAState,FPGAFW]),
	    ok;
	_ ->
	    ct:log(lightred, "COBRA/TAIPAN STATE NOT OK, expected:~n~s~n~s",[FPGAState,FPGAFW]),
	    {error,"faulty COBRA/TAIPAN state"}
    end.

check_npumac_status(Console,ProductName,ProductRevision) ->
    BBM0="bbm0: rx:1 tx:1 link:1 an:0 speed:0x40 port:XGMAC96",
    BBM2="bbm2: rx:1 tx:1 link:1 an:0 speed:0x40 port:XGMAC112",
    BBM4="bbm4: rx:1 tx:1 link:1 an:0 speed:0x40 port:XGMAC64",
    BBM6="bbm6: rx:1 tx:1 link:1 an:0 speed:0x40 port:XGMAC80",
    case ProductName of
	"DUS5201" ->
	    case string:str(ProductRevision,"P1") of % Only boards > P1 have trinity
		0 ->
		    {ok,NpumacStatus} = ct_telnet:cmd(Console,"testbox npumac_status --verbose 2"),
		    case [lists:member(BBM4,NpumacStatus),
			  lists:member(BBM2,NpumacStatus),
			  lists:member(BBM4,NpumacStatus),
			  lists:member(BBM6,NpumacStatus)] of
			[true,true,true,true] ->
			    ct:log("DUS5201 NPU links ok~n~s~n~s~n~s~n~s",[BBM0,BBM2,BBM4,BBM6]),
			    ok;
			_ ->
			    ct:log(lightred,"DUS5201 NPU links NOT OK, expected:~n~s~n~s~n~s~n~s",[BBM0,BBM2,BBM4,BBM6]),
			    {error,"Faulty Taipan links State"}
		    end;
		_  ->
		    ok
	    end;
	"DUS3201" ->
	    {ok,NpumacStatus} = ct_telnet:cmd(Console,"testbox npumac_status"),
	    case [lists:member(BBM4,NpumacStatus),
		  lists:member(BBM6,NpumacStatus)] of
		[true,true] ->
		    ct:log("DUS5201 NPU links ok~n~s~n~s",[BBM4,BBM6]),
		    ok;
		_ ->
		    ct:log(lightred,"DUS5201 NPU links NOT OK, expected:~n~s~n~s",[BBM4,BBM6]),
		    {error,"Faulty Taipan links State"}
	    end;
	_ ->
	    ct:log(lightred,"Unknown ProductName ~s",[ProductName]),
	    {error,"Unknown ProductName"}
    end.
    
check_bbmif(Console, ProductRevision) ->
    case string:str(ProductRevision,"P1") of % Only boards > P1 have trinity
	0 ->
	    {ok,BBMIF} = ct_telnet:cmd(Console,"testbox bbmif --verbose 2"),
	    BBMIFState = "bbmif :: TESTBOX_PASS",
	    case lists:member(BBMIFState,BBMIF) of
		true ->
		    ct:log("bbmif state ok~n~s",[BBMIFState]),
		    ok;
		_ ->
		    ct:log(lightred, "BBMIF STATE NOT OK, expected:~n~s",[BBMIFState]),
		    {error,"Faulty Taipan State"}
	    end;
	_  ->
	    ok
    end.   


get_board_info(Console) ->
    {ok,BoardInfo} = ct_telnet:cmd(Console,"testbox boardinfo --verbose 2"),
    [ProductName] = [T||[$P,$r,$o,$d,$u,$c,$t,$ ,$n,$a,$m,$e,$:|T]<-BoardInfo],
    [ProductRevisionString] = [X||X<-BoardInfo, string:str(X,"Product revision:") == 1],
    {match,[ProductRevision]} = re:run(ProductRevisionString,"^Product revision:.*([P,R][0-9]+[A-Z]+)",[{capture,[1],list}]),
    ct:log("ProductRevision ~s",[ProductRevision]),
    {re:replace(ProductName," ","",[global,{return, list}]), ProductRevision}.

%%%%% Can be removed when only restructured ARM boards %%%%%
is_restructured(Console) ->
    {ok,Data} = ct_telnet:cmd(Console,"ls -d /home/testbox"),
    case [X||X<-Data,string:str(X, "No such file or directory") =/= 0] of
	[] -> true;
	_ -> false
    end.

su(Console,true) ->
    {ok,_} = ct_telnet:cmd(Console,"su - testbox"),
    timer:sleep(1000); % seems to be needed
su(_Console,false) ->
    ok.
     
exit_su(Console,true) ->
    {ok,_} = ct_telnet:cmd(Console,"exit");
exit_su(_Console,false) ->
    ok.
%%%%% Can be removed when only restructured ARM boards %%%%%
