%%% #0.    BASIC INFORMATION
%% coding: latin-1
%%% ----------------------------------------------------------
%%% %CCaseFile:	meas_mem_size_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/R5A/1
%%% 
%%% @doc == Measure backup and RAM memory usage after create imm objects.==
%%% This Test Suite can be used on target enviroment.
%%% Used sw version and measured data is written to a file.
%%% Path: /proj/rcs/measurements/
%%%
%%% <br/><br/>
%%% 
%%% @end

-module(meas_mem_size_SUITE).
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
%%% R3A/2      2015-04-22 etxivri     Created
%%% R4A/1      2015-04-27 etxivri     Update log dir
%%% R4A/2      2015-07-03 etxmlar     Changed rct_netconf hook format 
%%% R5A/1      2015-10-07 etxivri     update for R5
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 all/0,
	 meas_mem_1/1,
	 meas_mem_10/1,
	 meas_mem_100/1
	]).

-define(CLI_Sess, cli1). %% CLI hook name
-define(NC_Sess, nc1). %% CLI hook name

-define(LOG_DIR, "/proj/rcs/measurements/ca_char/mem_size/").
%% -define(LOG_DIR, "/home/etxivri/tmp/").


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {hours, 2}},
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 %% {cth_conn_log, []},
		 %% {rct_power,node},
		 %% {rct_consserv,cs1},
                 %% {rct_rs232,console},
		 {rct_cli, {cli1, [manual_connect]}},
		 {rct_core,[]},
		 %% {rct_netconf, check_session},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],[]}}]}},
		 {rct_netconf, nc1}
     		]}].

%% @hidden
init_per_suite(Config) ->
    WordSize = rct_rpc:call(rpc, erlang, system_info, [wordsize],
			    5000, noprint),
    ct:pal("WordSize: ~p", [WordSize]),
    [{wordsize, WordSize}|Config].
%% @hidden
end_per_suite(_Config) ->
    rct_rpc:call(rpc, os, cmd, ["rm /tmp/aaa.bup"], 5000, noprint),
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.


%% @hidden
end_per_testcase(_TestCase, Config) ->
    ct:pal("End per TC.", []),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up.", [Reason])
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [	
	meas_mem_1,
	meas_mem_10,
	meas_mem_100
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @spec meas_mem_1(Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_mem_1(Config) ->
    Nr = 1,
    FileName = "used_mem_for_"++integer_to_list(Nr)++"_mo_and_attr",
    do_meas_mem_operation(Nr, FileName, Config).

meas_mem_10(Config) ->
    Nr = 10,
    FileName = "used_mem_for_"++integer_to_list(Nr)++"_mo_and_attr",
    do_meas_mem_operation(Nr, FileName, Config).

meas_mem_100(Config) ->
    Nr = 100,
    FileName ="used_mem_for_"++integer_to_list(Nr)++"_mo_and_attr",
    do_meas_mem_operation(Nr, FileName, Config).

%% ===========================================================================
%% @doc
%% This will do netconf operations and measure used memory size. <br/>
%% @spec do_meas_mem_operation(Nr, FileName, Config) -> ok
%% @end
%% ===========================================================================
do_meas_mem_operation(Nr, FileName, Config) ->

    ct:pal("Used mem size before create MOs."),
    {Start_Bup, Start_Ram} = get_used_mem(Config),

    create_mo_and_attr(Nr),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
    timer:sleep(10000),
    check_mo_and_attr_created(Nr),

    ct:pal("Used mem size after create MOs."),
    {End_Bup, End_Ram} = get_used_mem(Config),

    %% test_server:break("A"),
    
    ct:pal("Start_Bup: ~p bytes,  Start_Ram: ~p bytes ~n", 
    	   [Start_Bup, Start_Ram]),
    
    ct:pal("End_Bup: ~p bytes,  End_Ram: ~p bytes ~n", 
    	   [End_Bup, End_Ram]),
    
    DiffBup = End_Bup - Start_Bup,
    DiffRam = End_Ram - Start_Ram,

    ct:pal("DiffBup: ~p, DiffRam : ~p ~n", 
    	   [DiffBup, DiffRam]),

    delete_mo(Nr),
    [] = rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint),
    timer:sleep(10000),
    check_mo_deleted(Nr),


    %%%%
    %% Write size to file
    %%%%
    CXS_label = get_sw_version(),
    [{_, NodeName}] = ct:get_config(test_nodes),
    %% File_Name = rct_tlib:
    %% 	get_filename(FileName),
    BoardType = meas_lib:get_board_type(),
    Branch = meas_lib:get_branch(),
    File_Name = Branch++"_"++BoardType++FileName,
    rct_tlib:writeDataToFile(?LOG_DIR, 
    			     File_Name, 
    			     "~p;~w;~p;~p;~p;~p;~n", 
    			     [httpd_util:rfc1123_date(),
    			      CXS_label,
    			      Nr,
    			      DiffBup,
    			      DiffRam,
    			      NodeName
    			     ]),

    ct:pal("LogDir: ~p ",[?LOG_DIR++File_Name]),

    ok.


%% ===========================================================================
%% @doc
%% Create MO instance. <br/>
%% - Open sessions <br/>
%% - Create MO and Attr. <br/>
%% - Close sessions.
%% @spec  create_mo_and_attr(Nr) -> ok
%% @end
%% ===========================================================================
create_mo_and_attr(Nr) ->
    ct:pal("Create nr of MO with attribute. Nr : ~p", [Nr]),
    NrList = lists:seq(1, Nr),

    {ok,_} = ct_netconfc:open(?NC_Sess, 
			  [{timeout, 600000}]),
    
    lists:foreach(fun(X)->
			  InstName = 
			      atom_to_list(?NC_Sess)++
			      "_"++integer_to_list(X),
			  AttrName = integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_add_mo_instance_and_attr(?NC_Sess, 
							  InstName, 
							  AttrName)
		  end, NrList),

    ok = ct_netconfc:close_session(?NC_Sess, 600000),
    ct:pal("Create Done.", []),

    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).


%% ===========================================================================
%% @doc
%% Delete MO instance. <br/>
%% - Open sessions <br/>
%% - Delete MO. <br/>
%% - Close sessions.
%% @spec delete_mo(Nr) -> ok
%% @end
%% ===========================================================================
delete_mo(Nr) ->
    ct:pal("Delete nr of MO. Nr : ~p", [Nr]),
    NrList = lists:seq(1, Nr),
    
    {ok,_} = ct_netconfc:open(?NC_Sess, 
			  [{timeout, 600000}]),

    lists:foreach(fun(X)->
			  InstName = 
			      atom_to_list(?NC_Sess)++
			      "_"++integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_delete_mo_instance(?NC_Sess, 
						    InstName)
		  end, NrList),

    ok = ct_netconfc:close_session(?NC_Sess, 600000),
    ct:pal("Delete Done.", []),

    rct_rpc:call(rpc, os, cmd, ["sync"], 10000, noprint).

%% ===========================================================================
%% @doc
%% Check MO and attributes created. <br/>
%% @spec check_mo_and_attr_created(Nr) -> ok
%% @end
%% ===========================================================================
check_mo_and_attr_created(Nr)->
    ct:pal("Check MO and attributes created. Nr : ~p", [Nr]),
    NrList = lists:seq(1, Nr),

    {ok,_} = ct_netconfc:open(?NC_Sess, 
			  [{timeout, 600000}]),
    
    lists:foreach(fun(X)->
			  InstName = 
			      atom_to_list(?NC_Sess)++
			      "_"++integer_to_list(X),
			  AttrName = integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_get_attribute_check(?NC_Sess, 
						     InstName,
						     AttrName)
		  end, NrList),

    ok = ct_netconfc:close_session(?NC_Sess, 600000),
    ct:pal("Check create Done.", []), 

    ok.

%% ===========================================================================
%% @doc
%% Check MO deleted. <br/>
%% @spec  check_mo_deleted(Nr) -> ok
%% @end
%% ===========================================================================
check_mo_deleted(Nr)->
    ct:pal("Check MO and deleted. Nr : ~p", [Nr]),
    NrList = lists:seq(1, Nr),

    {ok,_} = ct_netconfc:open(?NC_Sess, 
			  [{timeout, 600000}]),
    
    lists:foreach(fun(X)->
			  InstName = 
			      atom_to_list(?NC_Sess)++
			      "_"++integer_to_list(X),
			  ok = rct_nc_testclass1_lib:
			      nc_check_mo_instance_deleted(?NC_Sess, 
							   InstName)
		  end, NrList),

    ok = ct_netconfc:close_session(?NC_Sess, 600000),
    ct:pal("Check delete Done.", []), 

    ok.

%%--------------------------------------------------------------------
%% @doc 
%% get_used_mem. <br/>
%% @end
%%--------------------------------------------------------------------
get_used_mem(Config) ->
    
    {ok, _, _} = rct_rpc:call(rpc, mnesia, activate_checkpoint, 
		     [[{name, aaa}, {min, [imm_objects, imm_reverse_refs]}]], 
		     5000, noprint),
    ok = rct_rpc:call(rpc, mnesia, backup_checkpoint, 
		     [aaa, "/tmp/aaa.bup"], 
		     5000, noprint),

    timer:sleep(2000),
    
    BUP_Ls = rct_rpc:call(rpc, os, cmd, ["ls -l /tmp/aaa.bup"], 5000, noprint),
    BUP_LsList = string:tokens(BUP_Ls, " "),
    BUP_Size = lists:nth(5, BUP_LsList),
    BackupSize = list_to_integer(BUP_Size),
    ct:pal("BackupSize: ~p bytes", [BackupSize]),

    ok = rct_rpc:call(rpc, mnesia, deactivate_checkpoint, [aaa], 5000, noprint),

    %%%%
    %% RamSize , Detta ger RAM storlek i ORD.
    %%%%
    RamORD = rct_rpc:call(rpc, mnesia, table_info, [imm_objects, memory], 
			   5000, noprint),
    ct:log("RamSize in ORD: ~p", [RamORD]),

    WordSize = proplists:get_value(wordsize, Config),
    RamSize = RamORD * WordSize,
    ct:log("RamSize : ~p in bytes", [RamSize]),

    {BackupSize, RamSize}.
    


%% %% Used by cleanup if tc fail.
%% %% ===========================================================================
%% %% @doc
%% %% Delete MO no check. Used in cleanup if TC fail<br/>
%% %% - Delete MO. <br/>
%% %% @spec delete_mo_no_check(SessionName, NrList) -> ok
%% %% @end
%% %% ===========================================================================
%% delete_mo_no_check(SessionName, NC_SessionNameList)->
%%     lists:foreach(fun(Name)->
%% 			  ct_netconfc:open(SessionName,[{timeout, 60000}]),
%% 			  TestClassName = atom_to_list(Name),
%% 			  ok = rct_nc_testclass1_lib:
%% 			      nc_delete_mo_instance_no_check(SessionName, 
%% 							     TestClassName),
%% 			  ct_netconfc:close_session(SessionName, 60000)

%% 		  end, NC_SessionNameList).


%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    CXS_label = rct_tlib:get_sw_version(?CLI_Sess),
    CXS_label.
