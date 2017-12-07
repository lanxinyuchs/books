%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_cli_testclass1_lib.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R8A/1
%%%
%%% @doc == support lib when using cli on testClass1 in TestRoot model. ==
%%% <br/>
%%%
%%%
%%% @end

-module(rct_cli_testclass1_lib).
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
%%% R2A/1      2013-10-01 etxivri     Created
%%% R2A/3      2014-07-10 etxivri     Update to make a check more robust.
%%% R2A/4      2014-08-22 etxivri     Make check_mo_inst_deleted more robust.
%%% R8A/1      2016-12-20 etxpeno     Don't set attribute struct1
%%%                                   (no support in SAFE for "Struct as attribute")
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).

-export([
	 add_mo_inst/2,
	 add_mo_inst/3,
	 add_mo_inst_match/2,
	 add_mo_inst_match/3,
	 add_several_mo_inst/2,
	 add_several_mo_inst/3,

	 add_mo_inst_and_attr/3,
	 add_mo_inst_and_attr/4,
	 add_mo_inst_and_attr_match/3,
	 add_mo_inst_and_attr_match/4,
	 add_several_mo_inst_and_attr/3,
	 add_several_mo_inst_and_attr/4,
	 add_several_mo_inst_and_attr_list/2,
	 add_several_mo_inst_and_attr_list/3,
	 add_several_mo_inst_and_attr_list_no_commit/3,

	 delete_mo_inst/2,
	 delete_mo_inst/3,
	 delete_mo_inst_match/2,
	 delete_mo_inst_match/3,
	 delete_several_mo_inst/2,
	 delete_several_mo_inst/3,
	 delete_several_mo_inst_no_commit/3,

	 check_mo_inst_created/2,
	 check_mo_inst_created/3,
	 check_several_mo_inst_created/2,
	 check_several_mo_inst_created/3,

	 check_mo_inst_deleted/2,
	 check_mo_inst_deleted/3,
	 check_several_mo_inst_deleted/2,
	 check_several_mo_inst_deleted/3,

	 get_no_of_created_mo_inst/2,
	 check_exp_nr_of_mo_inst_exist/2

	]).


-define(CONFIGURE, "configure").
-define(COMMIT, "commit").
-define(TOP, "top").

-define(TESTROOT, "ManagedElement=1,TestRoot=1").
-define(CREATE_TESTCLASS1_MO, "ManagedElement=1,TestRoot=1,TestClass1=").
-define(DEL_TESTCLASS1_MO, "no TestClass1=").
-define(SHOW_TESTCLASS1_MO_INST, "show ManagedElement=1,TestRoot=1,TestClass1=").

%% ===========================================================================
%% @doc
%% Create a mo instance of testClass1. <br/>
%% Cli_session = atom() <br/>
%% InstName = string() <br/>
%% @spec add_mo_inst(Cli_session, InstName) -> ok
%% @end
%% ===========================================================================
add_mo_inst(Cli_session, InstName) ->
    add_mo_inst(Cli_session, InstName, print).
%% ===========================================================================
%% @doc
%% Create a mo instance of testClass1. <br/>
%% Cli_session = atom() <br/>
%% InstName = string() <br/>
%% PrintOpt = print | noprint , default print <br/>
%% @spec add_mo_inst(Cli_session, InstName, PrintOpt) -> ok
%% @end
%% ===========================================================================
add_mo_inst(Cli_session, InstName, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    rct_cli:send(Cli_session, ?CREATE_TESTCLASS1_MO++InstName, PrintOpt),
    rct_cli:send(Cli_session, ?COMMIT, PrintOpt),
    rct_cli:send(Cli_session, ?TOP, PrintOpt),
    ok.


add_mo_inst_match(Cli_session, InstName) ->
    add_mo_inst_match(Cli_session, InstName, print).
add_mo_inst_match(Cli_session, InstName, PrintOpt) ->
    rct_cli:send(Cli_session, "configure", "\\(config\\)>", PrintOpt),
    rct_cli:send(Cli_session,
		 "ManagedElement=1,TestRoot=1,TestClass1="++InstName,
		 "\\(config-TestClass1="++InstName++"\\)>",
		 PrintOpt),
    rct_cli:send(Cli_session, "commit", "\\(TestClass1="++InstName++"\\)>", PrintOpt),
    rct_cli:send(Cli_session, "top", ">", PrintOpt),
    ok.


add_several_mo_inst(Cli_session, InstNameList) ->
    add_several_mo_inst(Cli_session, InstNameList, print).
add_several_mo_inst(Cli_session, InstNameList, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    lists:foreach(fun(InstName) ->
			  rct_cli:send(Cli_session,
				       ?CREATE_TESTCLASS1_MO++InstName,
				       PrintOpt)
		  end,
		  InstNameList),
    rct_cli:send(Cli_session, ?COMMIT, PrintOpt),
    rct_cli:send(Cli_session, ?TOP, PrintOpt),
    ok.

%% ===========================================================================
%% @doc
%% Create a mo instance and a attribute of testClass1. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% AttrName = list of integer() example. "12345" <br/>
%% @spec add_mo_inst_and_attr(Cli_session, InstName, AttrName) -> ok
%% @end
%% ===========================================================================
add_mo_inst_and_attr(Cli_session, InstName, AttrName) ->
    add_mo_inst_and_attr(Cli_session, InstName, AttrName, print).
%% ===========================================================================
%% @doc
%% Create a mo instance and a attribute of testClass1. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% AttrName = list of integer() example. "12345" <br/>
%% PrintOpt = print | noprint , default print <br/>
%% @spec add_mo_inst_and_attr(Cli_session, InstName, AttrName, PrintOpt) -> ok
%% @end
%% ===========================================================================
add_mo_inst_and_attr(Cli_session, InstName, AttrName, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    %% Add MO inst.
    rct_cli:send(Cli_session, ?CREATE_TESTCLASS1_MO++InstName, PrintOpt),
    %% Add attribute.
    rct_cli:send(Cli_session, "int32="++AttrName, PrintOpt),
    rct_cli:send(Cli_session, ?COMMIT, PrintOpt),
    rct_cli:send(Cli_session, ?TOP, PrintOpt),
    ok.


add_mo_inst_and_attr_match(Cli_session, InstName, AttrName) ->
    add_mo_inst_and_attr_match(Cli_session, InstName, AttrName, print).
add_mo_inst_and_attr_match(Cli_session, InstName, AttrName, PrintOpt) ->
    rct_cli:send(Cli_session, "configure", "\\(config\\)>", PrintOpt),
    rct_cli:send(Cli_session,
		 "ManagedElement=1,TestRoot=1,TestClass1="++InstName,
		 "\\(config-TestClass1="++InstName++"\\)>",
		 PrintOpt),
    %% Add attribute.
    rct_cli:send(Cli_session, "int32="++AttrName, "\\(config-TestClass1="++InstName++"\\)>",PrintOpt),
    rct_cli:send(Cli_session, "commit", "\\(TestClass1="++InstName++"\\)>", PrintOpt),
    rct_cli:send(Cli_session, "top", ">", PrintOpt),
    ok.


add_several_mo_inst_and_attr(Cli_session, InstNameList, AttrName) ->
    add_several_mo_inst_and_attr(Cli_session, InstNameList, AttrName, print).
add_several_mo_inst_and_attr(Cli_session, InstNameList, AttrName, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    lists:foreach(fun(InstName) ->
			  rct_cli:send(Cli_session,
				       ?CREATE_TESTCLASS1_MO++InstName,
				       PrintOpt),
			  %% Add attribute.
			  rct_cli:send(Cli_session, "int32="++AttrName, PrintOpt),
			  rct_cli:send(Cli_session, ?TOP, PrintOpt)
		  end,
		  InstNameList),
    rct_cli:send(Cli_session, ?COMMIT, PrintOpt),
    ok.

%% ===========================================================================
%% @doc
%% Create a mo instance and a attribute of testClass1. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% AttrName = list of integer() example. "12345" <br/>
%% InstAttrNameList = list of touples with InstName and AttrName. ex [{InstName,AttrName]
%% @spec add_several_mo_inst_and_attr_list(Cli_session, InstAttrNameList) -> ok
%% @end
%% ===========================================================================
add_several_mo_inst_and_attr_list(Cli_session, InstAttrNamesList) ->
    add_several_mo_inst_and_attr_list(Cli_session, InstAttrNamesList, print).
%% ===========================================================================
%% @doc
%% Create a mo instance and a attribute of testClass1. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% AttrName = list of integer() example. "12345" <br/>
%% InstAttrNameList = list of touples with InstName and AttrName. ex [{InstName,AttrName]
%% PrintOpt = print | noprint , default print <br/>
%% @spec add_several_mo_inst_and_attr_list(Cli_session, InstAttrNameList, PrintOpt) -> ok
%% @end
%% ===========================================================================
add_several_mo_inst_and_attr_list(Cli_session, InstAttrNameList, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    lists:foreach(fun({InstName, AttrName}) ->
			  rct_cli:send(Cli_session,
				       ?CREATE_TESTCLASS1_MO++InstName,
				       PrintOpt),
			  %% Add attribute.
			  rct_cli:send(Cli_session, "int32="++AttrName, PrintOpt),
			  rct_cli:send(Cli_session, ?TOP, PrintOpt)
		  end,
		  InstAttrNameList),
    rct_cli:send(Cli_session, ?COMMIT, PrintOpt),
    ok.


add_several_mo_inst_and_attr_list_no_commit(Cli_session, InstAttrNameList, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    lists:foreach(fun({InstName, AttrName}) ->
			  rct_cli:send(Cli_session,
				       ?CREATE_TESTCLASS1_MO++InstName,
				       PrintOpt),
			  %% Add attribute.
			  rct_cli:send(Cli_session, "int32="++AttrName, PrintOpt),
			  rct_cli:send(Cli_session, ?TOP, PrintOpt)
		  end,
		  InstAttrNameList),
    ok.

%% ===========================================================================
%% @doc
%% Delete a mo instance of testClass1. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% @spec delete_mo_inst(Cli_session, InstName) -> ok
%% @end
%% ===========================================================================
delete_mo_inst(Cli_session, InstName) ->
    delete_mo_inst(Cli_session, InstName, print).
%% ===========================================================================
%% @doc
%% Delete a mo instance of testClass1. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% PrintOpt = print | noprint , default print <br/>
%% @spec delete_mo_inst(Cli_session, InstName, PrintOpt) -> ok
%% @end
%% ===========================================================================
delete_mo_inst(Cli_session, InstName, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    rct_cli:send(Cli_session, ?TESTROOT, PrintOpt),
    rct_cli:send(Cli_session, ?DEL_TESTCLASS1_MO++InstName, PrintOpt),
    rct_cli:send(Cli_session, ?COMMIT, PrintOpt),
    rct_cli:send(Cli_session, ?TOP, PrintOpt),
    ok.


delete_mo_inst_match(Cli_session, InstName) ->
    delete_mo_inst_match(Cli_session, InstName, print).
delete_mo_inst_match(Cli_session, InstName, PrintOpt) ->
    rct_cli:send(Cli_session, "configure", "\\(config\\)>", PrintOpt),
    rct_cli:send(Cli_session, "ManagedElement=1,TestRoot=1", "\\(config-TestRoot=1\\)>", PrintOpt),
    rct_cli:send(Cli_session, "no TestClass1="++InstName, "\\(config-TestRoot=1\\)>", PrintOpt),
    rct_cli:send(Cli_session, "commit", "\\(TestRoot=1\\)>", PrintOpt),
    rct_cli:send(Cli_session, "top", ">", PrintOpt),
    ok.


delete_several_mo_inst(Cli_session, InstNameList) ->
    delete_several_mo_inst(Cli_session, InstNameList, print).
delete_several_mo_inst(Cli_session, InstNameList, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    lists:foreach(fun(InstName) ->
			  rct_cli:send(Cli_session,
				       ?TESTROOT,
				       PrintOpt),
			  rct_cli:send(Cli_session, ?DEL_TESTCLASS1_MO++InstName, PrintOpt),
			  rct_cli:send(Cli_session, ?TOP, PrintOpt)
		  end,
		  InstNameList),
    rct_cli:send(Cli_session, ?COMMIT, PrintOpt),
    ok.

delete_several_mo_inst_no_commit(Cli_session, InstNameList, PrintOpt) ->
    rct_cli:send(Cli_session, ?CONFIGURE, PrintOpt),
    lists:foreach(fun(InstName) ->
			  rct_cli:send(Cli_session,
				       ?TESTROOT,
				       PrintOpt),
			  rct_cli:send(Cli_session, ?DEL_TESTCLASS1_MO++InstName, PrintOpt),
			  rct_cli:send(Cli_session, ?TOP, PrintOpt)
		  end,
		  InstNameList),
    ok.


%% ===========================================================================
%% @doc
%% Check MO instance created. <br/>
%% - read MO one by one. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% @spec check_mo_inst_created(Cli_Session, InstName) -> ok
%% @end
%% ===========================================================================
check_mo_inst_created(Cli_Session, InstName) ->
    check_mo_inst_created(Cli_Session, InstName, print).
%% ===========================================================================
%% @doc
%% Check MO instance created. <br/>
%% - read MO one by one. <br/>
%% Cli_Session = atom() <br/>
%% InstName = string() <br/>
%% PrintOpt = print | noprint , default print <br/>
%% @spec check_mo_inst_created(Cli_Session, InstName, PrintOpt) -> ok
%% @end
%% ===========================================================================
check_mo_inst_created(Cli_Session, InstName, PrintOpt) ->
    {ok , Recieved_Data} = rct_cli:send(Cli_Session,
				       ?SHOW_TESTCLASS1_MO_INST++InstName,
				       PrintOpt),
    RecievedData = lists:append(string:tokens(Recieved_Data, "\r\n")),
    case re:run(RecievedData, "TestClass1="++InstName++"ERROR: Specific element not found") of
	{match, _} ->
	    ct:pal("InstName does not exist: ~p.",[InstName]),
	    ct:fail("Expected InstName does not exist");
	nomatch ->
	    ok
    end,
    ok.


check_several_mo_inst_created(Cli_Session, InstNameList) ->
    check_several_mo_inst_created(Cli_Session, InstNameList, print).
check_several_mo_inst_created(Cli_Session, InstNameList, PrintOpt) ->
    lists:foreach(fun(InstName)->
			  {ok , Recieved_Data} = rct_cli:send(Cli_Session,
							     ?SHOW_TESTCLASS1_MO_INST++InstName,
							     PrintOpt),
			  RecievedData = lists:append(string:tokens(Recieved_Data, "\r\n")),
			  case re:run(RecievedData, "TestClass1="++
					  InstName++
					  "ERROR: Specific element not found") of
			      {match, _} ->
				  ct:pal("InstName does not exist: ~p.",[InstName]),
				  ct:fail("Expected InstName does not exist");
			      nomatch ->
				  ok
			  end
		  end, InstNameList),
    ok.

%% ===========================================================================
%% @doc
%% Check MO instance deleted. <br/>
%% - read MO one by one. <br/>
%% Cli_SessionName = atom() <br/>
%% InstName = string() <br/>
%% @spec check_mo_inst_deleted(Cli_Session, InstName) -> ok
%% @end
%% ===========================================================================
check_mo_inst_deleted(Cli_SessionName, InstName) ->
    check_mo_inst_deleted(Cli_SessionName, InstName, print).
%% ===========================================================================
%% @doc
%% Check MO instance deleted. <br/>
%% - read MO one by one. <br/>
%% Cli_SessionName = atom() <br/>
%% InstName = string() <br/>
%% PrintOpt = print | noprint , default print <br/>
%% @spec check_mo_inst_deleted(Cli_Session, InstName, PrintOpt) -> ok
%% @end
%% ===========================================================================
check_mo_inst_deleted(Cli_SessionName, InstName, PrintOpt) ->
    check_mo_inst_deleted(Cli_SessionName, InstName, PrintOpt, 30000).
check_mo_inst_deleted(_Cli_SessionName, InstName, _PrintOpt, Timeout)
  when Timeout < 0 ->
    ct:pal("InstName does still exist after delete!: ~p.",[InstName]),
    ct:fail("UnExpected InstName does still exist after delete");
check_mo_inst_deleted(Cli_SessionName, InstName, PrintOpt, Timeout) ->
    %% do show first to ensure no crap is rcvd before check.
    rct_cli:send(Cli_SessionName, "show", PrintOpt),
    {ok ,RecievedData} = rct_cli:send(Cli_SessionName,
				      ?SHOW_TESTCLASS1_MO_INST++InstName,
				      PrintOpt),
    case re:run(RecievedData, "ERROR: Specific element not found") of
	{match, _} ->
	    ok;
	nomatch ->
	    ct:log("Unexpected data rcvd, try again!: ~p, ~p.",[InstName,
								RecievedData]),
	    timer:sleep(2000),
	    check_mo_inst_deleted(Cli_SessionName,
				  InstName,
				  PrintOpt,
				  Timeout-2000)
    end,
    ok.

%% check_mo_inst_deleted(Cli_SessionName, InstName, PrintOpt) ->
%%     %% do show first to ensure no crap is rcvd before check.
%%     rct_cli:send(Cli_SessionName, "show", PrintOpt),
%%     {ok ,RecievedData} = rct_cli:send(Cli_SessionName,
%% 				      ?SHOW_TESTCLASS1_MO_INST++InstName,
%% 				      PrintOpt),
%%     case re:run(RecievedData, "ERROR: Specific element not found") of
%% 	{match, _} ->
%% 	    ok;
%% 	nomatch ->
%% 	    ct:pal("InstName does still exist after delete!: ~p.",[InstName]),
%% 	    ct:fail("UnExpected InstName does still exist after delete")
%%     end,
%%     ok.


check_several_mo_inst_deleted(Cli_SessionName, InstNameList) ->
    check_several_mo_inst_deleted(Cli_SessionName, InstNameList, print).
check_several_mo_inst_deleted(Cli_SessionName, InstNameList, PrintOpt) ->
    lists:foreach(fun(InstName)->
			  {ok ,RecievedData} = rct_cli:send(Cli_SessionName,
							    ?SHOW_TESTCLASS1_MO_INST++InstName,
							    PrintOpt),
			  case re:run(RecievedData, "ERROR: Specific element not found") of
			      {match, _} ->
				  ok;
			      nomatch ->
				  ct:pal("InstName does still exist after delete!: ~p.",[InstName]),
				  ct:fail("InstName does still exist after delete")
			  end
		  end, InstNameList),
    ok.


%% ===========================================================================
%% @doc
%% @spec get_no_of_created_mo_inst(Cli_Session, PrintOpt) -> ok
%% @end
%% ===========================================================================
get_no_of_created_mo_inst(Cli_Session, PrintOpt)->
    {ok , Recieved_Data} = rct_cli:send(Cli_Session,
					"show ManagedElement=1,TestRoot=1",
					PrintOpt),
    %% Make a list of Recieved_Dataand remowe some characters.
    RecievedData=string:tokens(Recieved_Data, "\r\n> "),
    %% ct:pal("### RecievedData: ~p", [RecievedData]),
    %% Delete known strings that is not a created MO inst from list.
    A = lists:delete("show" , RecievedData),
    B = lists:delete("ManagedElement=1,TestRoot=1", A),
    C = lists:delete("TestRoot=1", B),
    D = lists:delete("TestClass1=1", C),
    E = lists:delete("TestClass2=1", D),
    NrOfMoInst = length(E),
    %% ct:pal("Inst: ~p, NrOfInst: ~p", [E, length(E)]),
    NrOfMoInst.

%% ===========================================================================
%% @doc
%% @spec check_exp_nr_of_mo_inst_exist(Cli_Session, PrintOpt) -> ok
%% @end
%% ===========================================================================
check_exp_nr_of_mo_inst_exist(Recieved_Data, NrOfExpMo)->
    case Recieved_Data of
	{Data, _MatchStr} ->
	    RecievedData=string:tokens(Data, "\r\n> ");
	    _List ->
	    RecievedData=string:tokens(Recieved_Data, "\r\n> ")
    end,
    %% ct:pal("### RecievedData: ~p", [RecievedData]),
    %% Delete known strings that is not a created MO inst from list.
    A = lists:delete("show" , RecievedData),
    B = lists:delete("ManagedElement=1,TestRoot=1", A),
    C = lists:delete("TestRoot=1", B),
    D = lists:delete("TestClass1=1", C),
    E = lists:delete("TestClass2=1", D),
    NrOfCreatedMO = length(E),
    %% ct:pal("Inst: ~p, NrOfInst: ~p", [E, length(E)]),
    case NrOfCreatedMO of
	NrOfExpMo ->
	    ct:pal("NrOfCreatedMO: ~p match NrOfExpMo: ~p.\n",[NrOfCreatedMO,
	    						   NrOfExpMo]),
	    ok;
	_Other ->
	    ct:pal("Inst: ~p, NrOfInst: ~p", [E, length(E)]),
	    ct:fail("NrOfCreatedMO does not match NrOfExpMo, after create")
    end,
    ok.
