%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_check_HW.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R7A/1
-module(rct_check_HW).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R7A/1').
-date('2016-11-01').
-author('etxivri').
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
%%% R2A/2      2014-06-04 etxkols     Added login
%%% R2A/3      2014-07-03 etxkols     Missed --verbose 2 for dus32
%%% R2A/4      2014-07-04 etxkols     Temp remove of trinity checks for dus3201
%%% R2A/5      2014-07-07 etxkols     Reinserted trinity checks for dus3201
%%% R2A/6      2014-08-07 etxkols     Change in testbox reply
%%% R3B/1      2015-02-20 etxkols     increased testbox timer
%%% R7A/1      2016-11-01 etxivri     Add get profuct type. 
%%%                                   To check what is installed on board.
%%%                                   cc, git, bpu, msrcs.
%%% ----------------------------------------------------------
-export([check_Taipan_Cobra_Trinity/2,
	 check_product_on_board/0,
	 check_product_on_board/1
	]).

%% @spec check_Taipan_Cobra_Trinity(Console, BOARDTYPE) -> ok | {error, Reason}
%% Console = atom()
%% BOARDTYPE = string()
%% Reason = string()
%% @doc Function to be used from other testsuites that checks Cobra/Taipan and Trinity states.
%%
%% Function requires that calling testsuite have defined the hook rct_rs232 and that Console is the alias defined for the hook.
%%
%% Example: check_HW_SUITE:check_HW(console,"dus5201").
%% @end
check_Taipan_Cobra_Trinity(Console, BOARDTYPE) when BOARDTYPE == "tcu03";
				  BOARDTYPE == "dus5201";
				  BOARDTYPE == "dus3201" ->
    ok = rct_rs232:login(Console),	
    Is_Restructured = is_restructured(Console),
    su(Console,Is_Restructured),
    Result1 = check_FPGA(Console),
    case BOARDTYPE of
	BOARDTYPE when BOARDTYPE == "dus5201";
		       BOARDTYPE == "dus3201" ->
	%% BOARDTYPE when BOARDTYPE == "dus5201" ->
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
	    {error,Reason};
	false ->
	    ok
    end;
check_Taipan_Cobra_Trinity(_Console, _BOARDTYPE) ->
    ok.
    	    
check_FPGA(Console) ->
    {ok,IcmStatus} = ct_telnet:cmd(Console,"testbox icm_status --verbose 2"),
    FPGAState = "FPGA state .*: CONFIGURED",
    FPGAFW = "FPGA FW: CORRECT",
    case [scan(FPGAState,IcmStatus),
	  lists:member(FPGAFW,IcmStatus)] of
	[true,true] ->
	    ct:log("Cobra/Taipan state ok~n~s~n~s",[FPGAState,FPGAFW]),
	    ok;
	_ ->
	    ct:log(lightred, "COBRA/TAIPAN STATE NOT OK, expected:~n~s~n~s",[FPGAState,FPGAFW]),
	    {error,"faulty COBRA/TAIPAN state"}
    end.

scan(_, []) ->
    false;
scan(Match, [H|T]) ->
    case re:run(H,Match) of
	{match,_} ->
	    true;
	_ ->
	    scan(Match, T)
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
	    {ok,NpumacStatus} = ct_telnet:cmd(Console,"testbox npumac_status --verbose 2"),
	    case [lists:member(BBM4,NpumacStatus),
		  lists:member(BBM6,NpumacStatus)] of
		[true,true] ->
		    ct:log("DUS3201 NPU links ok~n~s~n~s",[BBM4,BBM6]),
		    ok;
		_ ->
		    ct:log(lightred,"DUS3201 NPU links NOT OK, expected:~n~s~n~s",[BBM4,BBM6]),
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
    timer:sleep(10000); % seems to be needed
su(_Console,false) ->
    ok.
     
exit_su(Console,true) ->
    {ok,_} = ct_telnet:cmd(Console,"exit");
exit_su(_Console,false) ->
    ok.
%%%%% Can be removed when only restructured ARM boards %%%%%



%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_product_on_board() ->
    CXS = ct:get_config({jenkins_config, cxp}),
    ct:log("CXS: ~p", [CXS]),
    check_product_on_board(CXS).

check_product_on_board(CXS) ->
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
