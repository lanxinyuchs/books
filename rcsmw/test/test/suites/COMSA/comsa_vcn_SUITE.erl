%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsa_vcn_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/3
%%% 
%%% @doc == Test Suite for value change notification==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(comsa_vcn_SUITE).
-vsn('/main/R3A/3').

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
%%% R3A/1      2014-11-04 etxjotj     Created
%%% R3A/2      2015-05-29 etxkols     Changed rct_netconf hook format 
%%% R3A/3      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------
%%% 

%compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).

-export([check_vcn_subscriptions/1]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,nc1},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       []}}]}}
		]}].

%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_) ->
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:print("Now running ~w~n",[TestCase]),
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.



%%--------------------------------------------------------------------
%% @doc Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [check_vcn_subscriptions].

%%--------------------------------------------------------------------
%% @doc Checks that vcn subscriptions have been registered for all RCS models
%% @end
%%--------------------------------------------------------------------

check_vcn_subscriptions(_) ->
    ComsaEvent = call(ets, tab2list, [comsaEvent]),
%    ct:pal("ComsaEvent: ~n~p~n",[lists:keysort(3, ComsaEvent)]),

    %% Get model files
    Info = call(comte, info, []),

    {value, {environment, Env}} = lists:keysearch(environment, 1, Info),
    {value, {start_com_prog, StartComFile}} =
    	lists:keysearch(start_com_prog, 1, Env),
    {value, {com_top, ComTop}} =
    	lists:keysearch(com_top, 1, Env),

    ModelFilePath = filename:join(filename:dirname(StartComFile),
    				  "model_file_list.cfg"),
    put(fail, []),
    case call(file, read_file, [ModelFilePath]) of
	{ok, Bin}  ->
	    Files = string:tokens(binary_to_list(Bin), "\r\n"),
	    IncDir = filename:join([ComTop, "opt", "com", "etc", "model"]),
	    check_model(ComsaEvent, IncDir, Files);
	{error, Reason} ->
	    ct:pal("Error reading ~p~n",[ModelFilePath]),
	    ct:fail({error, Reason})
    end,
    case get(fail) of
	[] -> 
	    ok;
	undefined -> % should never happen but hey
	    ok;
	List when is_list(List), length(List) > 0 ->
	    ct:fail({missing_registrations, lists:sort(List)})
    end,
    ok.

check_model(ComsaEvent, _,  []) ->
    case ComsaEvent of
	[] -> 
	    ok;
	_ ->
	    ct:pal("Classes registered without recognized model~n~p~n", 
		   [ComsaEvent]),
	    ct:fail({unknown_registered_classes, ComsaEvent})
    end;
check_model(ComsaEvent, IncDir, [File|Files])->
    case re:run(File, "software.*RCS.*_CXP") of
	nomatch ->
	    check_model(ComsaEvent, IncDir, Files);

	{match, _} ->
	    do_check_model(ComsaEvent, IncDir, [File|Files])
    end.

do_check_model(ComsaEvent, IncDir, [File|Files]) ->
%    ct:pal("Parsing ~p~n",[File]),
    Options = [{validation, dtd},
	       {fetch_path, [IncDir]}],
    case call(xmerl_scan, file, [File, Options]) of
	{ModelsE, []} ->
	    MimE = find_element(mim, ModelsE),
	    MimName = find_attribute(name, MimE),
	    Fun = fun(Class, List) ->
			  case lists:keyfind({MimName, Class}, 3, List) of
			      false ->
				  case is_imm(Class) of
				      true ->
					  ok;
				      false ->
					  M = re:run(File, "[A-Z]+[0-9]*_CXC"),
					  {match, [{Start, Length}]} = M,
					  Cxc = string:substr(File, Start+1,
							      Length-4),
					  Failed = get(fail),
					  put(fail,[{Cxc,MimName,Class}|Failed])
				  end,
				  List;
			      _ ->
				  lists:keydelete({MimName, Class}, 3, List)
			  end
		  end,
	    Classes = call(comsaEcimModelAdaptor, get_model_classes, [MimName]),
	    NewComsaEvent = lists:foldr(Fun, ComsaEvent, Classes),
	    check_model(NewComsaEvent, IncDir, Files);
	Error ->
	    ct:pal("Error parsing ~p~n",[File]),
	    ct:fail(Error)
    end.

is_imm(Class) ->
    case [X||X<-call(ets, tab2list, [comsaCallbacks]), 
	     binary_to_list(hd(element(2,X)))==Class] of
	[{comsaCallbacks, _, Cb}] when is_tuple(Cb), element(1,Cb)==gmfComteI ->
	    true;
	_ ->
	    false
    end.
    


call(M,F,A) ->
    rct_rpc:call(rpc_1, M, F, A, 10000).
%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
	lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    {value, Attribute} =
	lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList),
    Attribute#xmlAttribute.value.
