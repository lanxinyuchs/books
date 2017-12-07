%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	prepareUP_eeAndFpgaFromHAL_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/2
%%%
%%% @doc == Prepare for HAL SBC Test Cases.==
%%%
%%%
%%% @end

-module(prepareUP_eeAndFpgaFromHAL_SUITE).
-author('erarube').
-vsn('/main/R8A/2').
-date('2017-11-21').

%%% 
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
%%% R8A/1      2017-11-21 erarube     Created
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
	 all/0,
	 prep_eeAndFpga_to_be_used/1
	]).


-define(APP4, "restart").
-define(NC, nc1).
%% -define(Protocol, "http").
%% -define(Port, "8080").
-define(Protocol, "https").
%% -define(Port, "443"). %% Not needed
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
	[{timetrap,{minutes,60}}].

%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_Config) ->
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
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	_  ->
	    nok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     prep_eeAndFpga_to_be_used
    ].

prep_eeAndFpga_to_be_used(Config) ->
    [{_, Node_Name}] = ct:get_config(test_nodes),
    NodeName =  atom_to_list(Node_Name),
    ct:pal("# NodeName: ~p", [NodeName]),


    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("# BoardType: ~p", [BoardType]),


    TftpDir = ct:get_config({Hwa, tftpboot}),
    ct:pal("TftpDir:~n~p~n",[TftpDir]),

    TmpDir = ?config(priv_dir, Config),
    %% TmpDir = "/proj/rcs-tmp/etxivri/hal/hal_ee/tmp_dir",
    ct:pal("Tmp_Dir:~n~p~n",[TmpDir]),

    %% {OrgProdNr, OrgProdRev} = decupling_lib:get_hw(rpc),
    %% ct:pal("### OrgProdNr: ~p", [OrgProdNr]),
    %% ct:pal("### OrgProdRev: ~p", [OrgProdRev]),

    %% Get dummy UP name
    Up_Name_Path = os:cmd("ls "++TftpDir++"/RCS-*_CXS*"),
    ct:pal("Up_Name Path: ~p",[Up_Name_Path]),
    UpName = lists:last(string:tokens(Up_Name_Path, "/\n") ),
    ct:pal("UpName: ~p",[UpName]),

    %% cp org dummy build to a backup
    os:cmd("cd "++TftpDir++"; cp "++UpName++" "++"backup_orginal_dummy_up.zip"),
    ok = check_for_expected_str("ls "++TftpDir, "backup_orginal_dummy_up.zip"),
    timer:sleep(5000),

    %% mv org dummy build tmp_dir
    os:cmd("mv "++TftpDir++"/"++UpName++" "++TmpDir),
    ok = check_for_expected_str("ls "++TmpDir, UpName),
    timer:sleep(5000),
    nok = check_for_expected_str("ls "++TftpDir, UpName),
    
    %% Check if right board for test.
    %% May be board revision shall be checked on the future
    case BoardType of
		"dus5301" -> %dus53
		    ct:pal("# BoardType: ~p ok for this kind of test.", [BoardType]),
		    EE_name = "RCSEE-DUS3",
		    FPGA_name ="TIGER",
		    EE_FPGA_dir = "/proj/rcs/hal_eeAndFpga/bb662_3/17Q1/R1A_R1B_R2A_R2B_Rxy/",
		    EeAndFpgaXmlDataFromHAL_FilePath = EE_FPGA_dir++"ee_fpga.xml",
		    ok = replaceEeAndFpgaWith_HAL_CXPs(TmpDir, TftpDir, UpName,  EE_name, FPGA_name, EE_FPGA_dir, EeAndFpgaXmlDataFromHAL_FilePath);
		"dus3301" -> %dus33
		    ct:pal("# BoardType: ~p ok for this kind of test.", [BoardType]),
		    EE_name = "RCSEE-DUS3",
		    FPGA_name ="TIGER",
		    EE_FPGA_dir = "/proj/rcs/hal_eeAndFpga/bb662_3/17Q1/R1A_R1B_R2A_R2B_Rxy/",
		    EeAndFpgaXmlDataFromHAL_FilePath = EE_FPGA_dir++"ee_fpga.xml",
		    ok = replaceEeAndFpgaWith_HAL_CXPs(TmpDir, TftpDir, UpName,  EE_name, FPGA_name, EE_FPGA_dir, EeAndFpgaXmlDataFromHAL_FilePath);
		"dus3201tk" -> %dus32 tk (dus32 from R9 and up)
		    ct:pal("# BoardType: ~p ok for this kind of test.", [BoardType]),
		    EE_name = "RCSEE-DUS3",
		    FPGA_name ="COBRA-TK",
		    EE_FPGA_dir = "/proj/rcs/hal_eeAndFpga/BB521_T/17Q1/R1A_Rxy/",
		    EeAndFpgaXmlDataFromHAL_FilePath = EE_FPGA_dir++"ee_fpga.xml",
		    ok = replaceEeAndFpgaWith_HAL_CXPs(TmpDir, TftpDir, UpName,  EE_name, FPGA_name, EE_FPGA_dir, EeAndFpgaXmlDataFromHAL_FilePath);
		"dus5201tk" -> %dus52 tk (dus52 from R9 and up)
		    ct:pal("# BoardType: ~p ok for this kind of test.", [BoardType]),
		    EE_name = "RCSEE-DUS3",
		    FPGA_name ="COBRA-TK",
		    EE_FPGA_dir = "/proj/rcs/hal_eeAndFpga/BB521_T/17Q1/R1A_Rxy/",
		    EeAndFpgaXmlDataFromHAL_FilePath = EE_FPGA_dir++"ee_fpga.xml",
		    ok = replaceEeAndFpgaWith_HAL_CXPs(TmpDir, TftpDir, UpName,  EE_name, FPGA_name, EE_FPGA_dir, EeAndFpgaXmlDataFromHAL_FilePath);
		_ ->
		    %% remove dummy build from TmpDir, to save disk space.
		    os:cmd("rm "++TmpDir++"/"++UpName),
		    ct:fail("# TC will fail due to unknown BoardType ~p for this kind of test.", [BoardType])
	    end,
    ok.



replaceEeAndFpgaWith_HAL_CXPs(TmpDir, TftpDir, UpName,  EE_name, FPGA_name, EE_FPGA_dir, EeAndFpgaXmlDataFromHAL_FilePath) ->
    %% Unzip up in tmp_dir
    os:cmd("cd "++TmpDir++"; "++"unzip "++UpName),
    ok = check_for_expected_str("ls "++TmpDir, EE_name),
    ok = check_for_expected_str("ls "++TmpDir, FPGA_name),

    %% Remove zip,ee cxp, tiger cxp
    os:cmd("cd "++TmpDir++"; "++"rm "++UpName),
    os:cmd("cd "++TmpDir++"; "++"rm "++EE_name++"*"),
    nok = check_for_expected_str("ls "++TmpDir, EE_name),
    os:cmd("cd "++TmpDir++"; "++"rm *"++FPGA_name++"*"),
    nok = check_for_expected_str("ls "++TmpDir, FPGA_name),

    %% cp ee to TmpDir
    os:cmd("cp "++EE_FPGA_dir++EE_name++"*.cxp "++TmpDir),
    ok = check_for_expected_str("ls "++TmpDir, EE_name),
    %% cp tiger to TmpDir
    os:cmd("cp "++EE_FPGA_dir++FPGA_name++"*.cxp "++TmpDir),
    ok = check_for_expected_str("ls "++TmpDir, FPGA_name),

    %% Get Up-xml name
    UP_xml_name_path = os:cmd("ls "++TmpDir++"/*-up.xml"),
    UpXmlName = lists:last(string:tokens(UP_xml_name_path, "/\n") ),
    ct:pal("UpXmlName: ~p",[UpXmlName]),

    %% Remove EE from up xml
    os:cmd("cd "++TmpDir++"; sed -i /"++EE_name++"/d "++UpXmlName),
    ok = check_for_expected_str("grep "++EE_name++" "++TmpDir++"/"++UpXmlName, ""),
    %% Remove FPGA from up xml
    os:cmd("cd "++TmpDir++"; sed -i /"++FPGA_name++"/d "++UpXmlName),
    ok = check_for_expected_str("grep "++FPGA_name++" "++TmpDir++"/"++UpXmlName, ""),

    %% Add hal EE and FPGA to up xml, after line with RCSMW
    {ok, NewEExmlBin} = file:read_file(EeAndFpgaXmlDataFromHAL_FilePath),
    XML = binary:bin_to_list(NewEExmlBin),
    ct:pal("# XML: ~p ", [XML]),
    SedCmd = "sed -i '/RCSMW-ARM/a "++XML++"' ",
    os:cmd("cd "++TmpDir++"; "++SedCmd++" "++UpXmlName),
    ok = check_for_expected_str("grep "++EE_name++" "++TmpDir++"/"++UpXmlName, EE_name),
    ok = check_for_expected_str("grep "++FPGA_name++" "++TmpDir++"/"++UpXmlName, FPGA_name),

    %% zip a new up with hal EE and FPGA
    os:cmd("cd "++TmpDir++"; zip " ++UpName++ " * "),
    timer:sleep(10000),
    ok = check_for_expected_str("ls "++TmpDir, UpName),

    %% mv new up build to tftp dir
    os:cmd("mv "++TmpDir++"/" ++UpName++ " "++TftpDir),
    timer:sleep(5000),
    ok = check_for_expected_str("ls "++TftpDir, UpName),
    
    % remove cxp files from TmpDir, to save disk space.
    os:cmd("rm "++TmpDir++"/*cxp"),
    timer:sleep(5000),
    % Keep xml file on temporal directory, in case of debug
    %os:cmd("rm "++TmpDir++"/*xml"),
    %timer:sleep(5000),
    
    ok.


check_for_expected_str(OsCmd, ExpectedStr) ->
    Answ = os:cmd(OsCmd),
    ct:pal("# Answ: ~p", [Answ]),
    case re:run(Answ, ExpectedStr) of
	{match, _} -> ok;
	nomatch -> nok
    end.
