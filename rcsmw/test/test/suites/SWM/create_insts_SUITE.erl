%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	create_insts_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2016
%%% @version /main/R6A/5
%%%
%%% @doc == Creation and deletion of MO instances ==
%%% For capacity and performance tests.
%%% <br/><br/>
%%% @end

-module(create_insts_SUITE).
-vsn('/main/R6A/5').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R6A/1      2016-06-10 erarafo     First version
%%% R6A/2      2016-06-14 erarafo     Cleaned up
%%% R6A/3      2016-06-15 erarafo     Refactored
%%% R6A/4      2016-06-15 erarafo     Quadratic behaviour fixed
%%% R6A/5      2016-06-16 erarafo     Variant testcases
%%% ----------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include("imm.hrl").

-define(IMM, 22).

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).

-export([generate_config_12000_5/1,
	 generate_config_30_2/1]).


%%--------------------------------------------------------------------
%% @doc Runs ct_hooks.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,
      [{rct_htmllink,[]},
       {rct_logging,
	{upgrade,
	 [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
       {cth_conn_log,[]},
       {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]}
      ]
     }
    ].


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
end_per_testcase(create_config, _Config) ->
    rct_proxy:stop_proxy(node1, child1),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in this suite.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [generate_config_12000_5].

-type ecimDn() :: string().

-define(MAX_LAYERS, 5).





generate_config_12000_5(_Config) ->
    MoInstances = 12000,    % n. of instances in deepest layer
    CcbSize = 97,           % CCB size to be used
    Layers = 5,             % n. of layers, 1..5
    Divisor = 4,            % see above
    generateConfig(MoInstances, CcbSize, Layers, Divisor).


generate_config_30_2(_Config) ->
    MoInstances = 30,       % n. of instances in deepest layer
    CcbSize = 5,            % CCB size to be used
    Layers = 2,             % n. of layers, 1..5
    Divisor = 4,            % see above
    generateConfig(MoInstances, CcbSize, Layers, Divisor).


%%%--------------------------------------------------------------------
%%% @doc Generate a configuration of non-trivial size. This TC can be
%%% used for capacity and performance tests (run the TC before doing
%%% an upgrade e g).
%%%
%%% The maximum value for Layers is 5 (the BUG model can be extended
%%% if more layers are needed).
%%%
%%% A fraction of the created instances will have a struct value
%%% assigned. The ratio of such instances is 1/Divisor (set a very
%%% high value for Divisor to disable structs assignment entirely).
%%% @end
%%%--------------------------------------------------------------------
-spec generateConfig(integer(), integer(), integer(), integer()) ->
	  ok.

generateConfig(MoInstances, CcbSize, Layers, Divisor) ->
    Nativity = nativity(MoInstances, Layers),
    One = hiddenOne(),
    if
	One*Layers > ?MAX_LAYERS ->
	    ct:fail("max number of layers exceeded", []);
	true ->
	    ok
    end,
    ct:pal(
      "generating ~w instance names, layers: ~w, nativity: ~w",
      [MoInstances, Layers, Nativity]),
    {EcimDns, _} =
	tc(fun generateDns/5,
	   [MoInstances, "LadyBug=1", "Cricket", Layers, Nativity],
	   "prepared instance names"),
    {{EcimDnsPlus, InstInfo}, _} = 
	tc(fun assignStructValues/2,
	   [EcimDns, Divisor],
	   "prepared struct values"),
    {ok, client_started} = rct_proxy:start_proxy(node1, child1, 22),
    TestContext = #testContext{node=node1, child=child1},
    {_, Millis} =
	tc(fun createInstances/4,
	   [TestContext, EcimDnsPlus, CcbSize, InstInfo],
	   "created IMM instances"),
    Ninst = ordsets:size(EcimDnsPlus),
    ct:pal("created ~w IMM instances, rate: ~w instances/s", [Ninst, (1000*Ninst) div Millis]).
    

%%%--------------------------------------------------------------------
%%% @doc Timed execution, similar to timer:tc/2.
%%% @end
%%%--------------------------------------------------------------------
tc(Fun, Args, What) ->
    Begin = erlang:monotonic_time(milli_seconds),
    Result = apply(Fun, Args),
    End = erlang:monotonic_time(milli_seconds),
    Millis = End - Begin,
    ct:pal("~s, milliseconds: ~w", [What, Millis]),
    {Result, Millis}.


%%%--------------------------------------------------------------------
%%% @doc Returns a "nativity" value as small as possible
%%% but large enough to generate an MO tree with M or more
%%% nodes at the leaf layer. The parameter L is the number
%%% of layers below the root node. "Nativity" is the number
%%% of children per node. For example, nativity(11, 2) -> 4.
%%% @end
%%%--------------------------------------------------------------------
-spec nativity(integer(), integer()) -> integer().

nativity(M, L) ->
    nativity(M, L, 1).


nativity(M, L, B) ->
    N = nofNodes(L, B),
    if
	N < M ->
	    nativity(M, L, B+1);
	true ->
	    B
    end.

%%%--------------------------------------------------------------------
%%% @doc Returns the number of nodes in the leaf layer
%%% of a tree with nativity B and number of layers L
%%% (the root node itself is not counted as a layer).
%%% @end
%%%--------------------------------------------------------------------

nofNodes(L, B) ->
    power(B, L).


power(_, 0) ->
    1;

power(B, L) ->
    B*power(B, L-1).


%%%--------------------------------------------------------------------
%%% @doc Generates ECIM Dns for an MO tree.
%%% @end
%%%--------------------------------------------------------------------
-spec generateDns(integer(), string(), string(), integer(), integer()) ->
	  ordsets:ordset(string()).

generateDns(NofDns, DnPrefix, ClassPrefix, Layers, Nativity) ->
    generateDns(NofDns-1, ordsets:new(), Layers, Nativity, ClassPrefix, DnPrefix).


generateDns(-1, Result, _, _, _ClassPrefix, _DnPrefix) ->
    Result;
generateDns(U, Set, L, B, ClassPrefix, DnPrefix) ->
    Dn = generateDn(U, L, B, ClassPrefix),
    NewSet = insertSupported(Dn, Set, DnPrefix),
    generateDns(U-1, NewSet, L, B, ClassPrefix, DnPrefix).


-spec generateDn(integer(), integer(), integer(), string()) -> string().
	
generateDn(_, 0, _, ClassPrefix) ->
    ClassPrefix++"0=1";

generateDn(U, L, B, ClassPrefix) ->
    string:join(
      [generateDn(U div B, L - 1, B, ClassPrefix),
       ClassPrefix++integer_to_list(L)++"="++integer_to_list((U rem B)+1)], ",").


-spec insertSupported(ecimDn(), ordsets:ordset(ecimDn()), string()) ->
	  ordsets:ordset(ecimDn()).
	  
insertSupported(Dn, Set, DnPrefix) ->
    case string:tokens(Dn, ",") of
	[_] ->
	    ordsets:add_element(DnPrefix++","++Dn, Set);
	Multi ->
	    NewSet =
		insertSupported(
		  string:join(
		    lists:reverse(
		      tl(
			lists:reverse(Multi))), ","),
		  Set,
		  DnPrefix),
	    ordsets:add_element(DnPrefix++","++Dn, NewSet)
    end.


%%%--------------------------------------------------------------------
%%% @doc Scans the given EcimDns and assign struct value to a fraction
%%% of them.
%%% @end
%%%--------------------------------------------------------------------
-spec assignStructValues(ordsets:ordset(ecimDn()), integer()) ->
	  {ordsets:ordset(ecimDn()), dict:dict()}.

assignStructValues(EcimDns, Divisor) ->
    {_, EcimDnsPlusSet, InstInfo} =
	ordsets:fold(
	  fun(Dn, {Count, D, A}) ->
		  if
		      Count rem Divisor > 0 ->
			  {Count+1, 
			   sets:add_element(Dn, D), 
			   dict:store(Dn, {object, []}, A)};
		      true ->
			  % assign the struct attribute
			  InstInfo1 = dict:store(Dn,
						 {object, makeStructMoRef(Dn)},
						 A),
			  % create the struct instance
			  StructInstEcimDn = string:join([Dn, "Legs=x"], ","),
			  InstInfo2 = 
			      dict:store(StructInstEcimDn,
					 {struct, makeStructMemberValues()},
					 InstInfo1),
			  NewDns =
			      sets:add_element(Dn,
					sets:add_element(StructInstEcimDn, D)),
			  {Count+1, NewDns, InstInfo2}
		  end
	  end,
	  {1, sets:new(), dict:new()},
	  EcimDns),
    EcimDnsPlus = ordsets:from_list(lists:sort(sets:to_list(EcimDnsPlusSet))),
    {EcimDnsPlus, InstInfo}.


makeStructMoRef(Dn) ->
    {_A, B, C, D} = getImmNameInfo(Dn),
    StructMoRef = string:join(["id=legs_1", B++"="++C, D], ","),
    [#attrValues{name="legs", type=name, values=[StructMoRef]}].
    
makeStructMemberValues() ->
    [#attrValues{name="leftFrontLeg", type=int32, values=[111]},
     #attrValues{name="rightFrontLeg", type=int32, values=[222]}].


%%%--------------------------------------------------------------------
%%% @doc Create instances according to the given list of Ecim Dns,
%%% using CCBs of the given size.
%%% @end
%%%--------------------------------------------------------------------
-spec createInstances(#testContext{}, [string()], integer(), dict:dict()) ->
	  ok.

createInstances(_TestContext, [], _CcbSize, _InstInfo) ->
    ok;

createInstances(TestContext, EcimDns, CcbSize, InstInfo) ->
    {CcbDns, More} = splitNfirst(CcbSize, EcimDns),
    ExternalParents = externalParents(ordsets:from_list(CcbDns)),
    
    % get IMM DNs of external parents, order is not significant
    EpsImm = 
	ordsets:fold(
	  fun(EcimDn, Acc) ->
		  {_A, B, C, D} = getImmNameInfo(EcimDn),
		  ImmDn =
		      case D of
			  "" ->
			      string:join([B, C], "=");
			  _ ->
			      string:join([string:join([B, C], "="), D], ",")
		      end,
		  [ImmDn|Acc]
	  end,
	  [],
	  ExternalParents),
    InstDescrs = 
	lists:map(
	  fun(EcimDn) ->
		  case dict:fetch(EcimDn, InstInfo) of
		      {object, Attrs} ->
			  {A, B, C, D} = getImmNameInfo(EcimDn),
			  #instDescr{className=A, 
				     parentName=D, 
				     attrValues=[#attrValues{name=B, 
							     type=string, 
							     values=[C]}|Attrs]};
		      {struct, MemValues} ->
			  {A, _B, _C, D} = getImmNameInfo(EcimDn),
			  #instDescr{className=A, 
				     parentName=D, 
				     attrValues=[#attrValues{name="id", 
							     type=string, 
							     values=["legs_1"]}
						|MemValues]}
		  end
	  end,
	  CcbDns),
    imm:createConfigInsts(TestContext, EpsImm, InstDescrs),
    createInstances(TestContext, More, CcbSize, InstInfo).
    

%%%--------------------------------------------------------------------
%%% @doc Returns the external parents of the given DNs. It is trusted
%%% that each DN has a non-null parent (in other words each DN contains
%%% at least one comma).
%%%--------------------------------------------------------------------
-spec externalParents(ordsets:ordset(string())) -> ordsets:ordset(string()).

externalParents(EcimDns) ->
    Parents =
	ordsets:fold(
	  fun(Dn, Acc) ->
		  [_|RdnsRevTail] = lists:reverse(string:tokens(Dn, ",")),
		  ordsets:add_element(string:join(lists:reverse(RdnsRevTail), ","), Acc)
	  end,
	  ordsets:new(),
	  EcimDns),
    ordsets:subtract(Parents, EcimDns).


%%%--------------------------------------------------------------------
%%% @doc Split the N first elements off the given list. Return
%%% two lists that equals the given list when appended. If the given
%%% list has less than N elements then those elements are returned.
%%% @end
%%%--------------------------------------------------------------------
-spec splitNfirst(non_neg_integer(), list()) -> {list(), list()}.

splitNfirst(0, L) ->
    {[], L};

splitNfirst(_, []) ->
    {[], []};

splitNfirst(N, [A|T]) ->
    {X, Y} = splitNfirst(N-1, T),
    {[A|X], Y}.


%%%--------------------------------------------------------------------
%%% @doc Returns a quad of ECIM RDN name, IMM RDN name, RDN value and
%%% IMM parent name.
%%% @end
%%%--------------------------------------------------------------------
-spec getImmNameInfo(string()) -> {string(), string(), string(), string()}.

getImmNameInfo(EcimDn) ->
    [EcimRdn|Parent] = lists:reverse(string:tokens(EcimDn, ",")),
    [EcimRdnName, RdnValue] = string:tokens(EcimRdn, "="),
    {EcimRdnName,
     getImmRdnName(EcimRdnName),
     RdnValue,
     string:join(
       lists:map(
	 fun(P) ->
		 [X, Y] = string:tokens(P, "="),
		 getImmRdnName(X)++"="++Y
	 end,
	 Parent)
       ,
       ",")}.


-spec getImmRdnName(string()) -> string().

getImmRdnName([LeadingChar|Rest]) ->
    NewLeadingChar = string:to_lower(LeadingChar),
    lists:flatten([NewLeadingChar, Rest, "Id"]).


%%%--------------------------------------------------------------------
%%% @doc Returns 1 in a way that the dialyzer does not understand.
%%% @end
%%%--------------------------------------------------------------------
-spec hiddenOne() -> 0..1.

hiddenOne() ->
    case os:type() of
	{_, eniac} ->
	    0;
	_ ->
	    1
    end.
