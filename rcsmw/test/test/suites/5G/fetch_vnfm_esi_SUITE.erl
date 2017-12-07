%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	fetch_vnfm_esi_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R12A/4
%%%
%%% @doc == Fetch ESI from VNFM ==
%%% <br/><br/>
%%% @end
%%%

-module(fetch_vnfm_esi_SUITE).
-vsn('/main/R9A/R10A/R12A/4').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R9A/1      2017-03-01 etxivri     Created
%%% R9A/5      2017-04-03 etxivri     Add rct_htmllink hook
%%% R10A/1     2017-05-04 etxivri     Add check in erlang log for error.  
%%% R10A/2     2017-05-04 etxivri     Changed som pal to log.
%%% R10A/3     2017-05-04 etxivri     Disable error check, not needed now.
%%% R12A/1     2017-11-15 etxivri     -Set vnfm to down in hook due to soon rpc will not work 
%%%                                    on vnfm and the logging will fail.
%%% R12A/2     2017-11-20 etxivri     Update to not use rpc towards vnfm
%%% ----------------------------------------------------------


-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
	 break/1,

	 %% Fetch esi from vnfm using rest api. Note after SP419 is ready logM Mo shall be used
	 fetch_vnfm_esi_rest_api/1
	 ]).

suite() ->  
    [{ct_hooks,[{rct_htmllink,[]},
		{rct_node_state, [{1,vnfm,down}]},
		{rct_logging, [{1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []}]},
		%% {rct_rpc,[rpc1]},
		{rct_vnfm,[vnfm1]}
		]}].

init_per_suite(Config) -> 
    Config.
end_per_suite(_Config) -> 
    ok.
init_per_testcase(_TestCase, Config) -> 
    Config.
end_per_testcase(_TestCase, _Config) -> 
    ok.

all() -> [].



break(_Config) ->
    test_server:break("AA").


-define(ERROR_STRING, "\"ERROR REPORT|CRASH REPORT\"").


%%%--------------------------------------------------------------------
%%% @doc
%%%  upgrade_vnf <br/>
%%% @end
%%%--------------------------------------------------------------------
fetch_vnfm_esi_rest_api(Config) ->
    ct:pal("Create a esi name using the os cmd date.", []),
    {{Y,Mo,D},{H,Mi,S}}=calendar:local_time(),
    VnfmTimeStamp = lists:flatten(io_lib:format("date_~4.4.0w-~2.2.0w-~2.2.0w_time_~2.2.0w-~2.2.0w-~2.2.0w",[Y,Mo,D,H,Mi,S])),

    ct:log("VnfmTimeStamp: ~n~p", [VnfmTimeStamp]),
    VnfmEsiName = VnfmTimeStamp++"_vnfm_esi.tgz",
    ct:log("VnfmEsiName: ~n~p", [VnfmEsiName]),

    ct:log("# Get esi log path.", []),
    EsiLogPath=proplists:get_value(priv_dir,Config),
    ct:log("EsiLogPath: ~p", [EsiLogPath]),

    ct:pal("# Get vnfm esi.", []),
    Res = rct_vnfm:fetch_vnfm_esi_rest_api(vnfm1, VnfmEsiName, EsiLogPath),
    ct:log("Res : ~n~p", [Res]),
    
    ok = wait_for_esi_exist(VnfmEsiName, EsiLogPath, 120000),
    ct:pal("# Unpack esi.", []),

    UnpackEsiDir = unpack_esi(VnfmEsiName, EsiLogPath),
    ct:log("# UnpackEsiDir. ~p", [UnpackEsiDir]),

    ct:log("check for unexpected stuff in VNMF esi."),
    %% dumps and pmd dir does not exist in ESI, yet!
    %% ok = check_esi_for_pmd(UnpackEsiDir, ["rcs/dumps/appdump/","rcs/dumps/pmd/"]),
    read_esi_log_file(UnpackEsiDir, "/rcs/erlang*/*", ?ERROR_STRING),
    
    ok.


%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%
wait_for_esi_exist(_VnfmEsiName, _EsiLogPath, Timeout) when Timeout < 0 -> 
    ct:log("### Did not foun esi within expected timeout", []),
    nok;
wait_for_esi_exist(VnfmEsiName, EsiLogPath, Timeout) ->
    Ls = os:cmd("ls "++EsiLogPath),
    ct:log("### ls ~p : ~n~p", [EsiLogPath, Ls]),
    %% Check esi exist in logdir
    case re:run(Ls, VnfmEsiName) of
	{match, _} ->
	    ct:pal("esi : ~p fetched ok. ~nExist under : ~p", [VnfmEsiName, EsiLogPath]),
	    ok;
	nomatch ->
	    ct:pal("Esi not found! Sleep and try check again!"),
	    timer:sleep(10000),
	    wait_for_esi_exist(VnfmEsiName, EsiLogPath, Timeout-10000)
    end.

unpack_esi(VnfmEsiName, EsiLogPath) ->
    %% Construct a dir name where to unpack the esi
    UnpackDir = lists:nth(1, string:tokens(VnfmEsiName, ".")), 
    UnpackEsiDir = filename:join(EsiLogPath, UnpackDir),
    ct:log("UnpackEsiDir : ~p", [UnpackEsiDir]),
    os:cmd("mkdir " ++ UnpackEsiDir),
    os:cmd("tar -xf " ++ EsiLogPath ++ VnfmEsiName ++" --gunzip --directory=" ++ UnpackEsiDir),
    format_html("<a href=\"~s\">~s</a>", [UnpackEsiDir, "Esi unpacked dir"]),
    format_html("<a href=\"~s\">~s</a>", [filename:join(EsiLogPath,VnfmEsiName), VnfmEsiName]),
    UnpackEsiDir.


format_html(String,Args) ->
     ct:log(default, 1, String, Args, [no_css]).


%% check_esi_for_pmd(_EsiLogPath, []) ->
%%     ok;
%% check_esi_for_pmd(EsiLogPath, [DumpDir|Rest]) ->
%%     case os:cmd("ls " ++ filename:join(EsiLogPath, DumpDir)) of
%% 	[] -> check_esi_for_pmd(EsiLogPath, Rest);
%% 	Dump -> case string:str(Dump,"cannot access") > 0 of
%% 		    true -> ct:pal("WARNING:~p no such file or directory",[DumpDir]),
%% 			    check_esi_for_pmd(EsiLogPath, Rest);
%% 		    false ->  ct:fail("Dumps found in dir ~p: ~p",[DumpDir, Dump])
%% 		end
%%     end.

read_esi_log_file(EsiLogPath, LogFileDir, ErrorString) ->
    UnpackedErlDir = EsiLogPath++LogFileDir,
    ct:log("UnpackedErlDir:~p ",[UnpackedErlDir]),
    ct:log(" Search for ErrorString:~p ",[ErrorString]),
    %% case os:cmd("egrep " ++  ErrorString  ++ " " ++  filename:join(EsiLogPath, LogFileDir))  of
    case os:cmd("egrep " ++  ErrorString  ++ " " ++ UnpackedErlDir)   of
	[]->
	    ok;
	Error -> ct:fail("Error in erlang log: ~n~p",[Error])
    end.
