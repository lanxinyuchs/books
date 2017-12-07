%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_verify_mo_instances_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2015
%%% @version /main/R2A/R3A/2
%%%
%%% @doc == This suite is invoked by bin/swm_icti_extensive_test.sh.
%%% The purpose is to verify correct MO instances after an upgrade.
%%% The suite contains hard-coded knowledge of the MO classes used
%%% in the upgrade test.
%%% @end

-module(swm_verify_mo_instances_SUITE).
-vsn('/main/R2A/R3A/2').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R2A/1      2013-08-06 erarafo     First version
%%% R2A/2      2013-08-29 erarafo     Added support for the DEMO MOM
%%% R2A/3      2013-10-02 erarafo     Adjusted, drops managedElementType
%%% R2A/4      2013-10-22 erarafo     Eliminated knowledge of unrelated MOs
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-29 etxkols     Changed rct_netconf hook format 
%%% ----------------------------------------------------------


-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).

-export([verify_mo_instances/1]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_upgrade,ug1},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
		 {rct_netconf,nc1}]}].


%% @hidden
init_per_suite(Config) ->

    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,

    Get = {'ManagedElement',
	   [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId, [], [MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'SwM',
	       [{xmlns,"urn:com:ericsson:ecim:SwM"}],
	       [{swMId,[],["1"]}]}]}]},

    {ok, R} = netconf(get, [nc1, Get]),
    ct:pal("Upgrade packages ~p", [R]),
    [{host, SftpHost},{username, Username},{password, Password}] = 
	ct:get_config(sftp_server),
    [{meId, MeId},
     {sftp_host, SftpHost},
     {sftp_user, Username},
     {sftp_pass, Password}| Config].

%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    [verify_mo_instances].



%%%--------------------------------------------------------------------
%%% @doc Verify that MO instances after upgrade are the expected ones.
%%% @end
%%%--------------------------------------------------------------------

verify_mo_instances(Config) ->

    MeId = proplists:get_value(meId, Config),

    Get_config = {'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],[MeId]}]
		 },

    {ok, [{'ManagedElement', _, MeContent} | _]} =
	netconf(get_config, [nc1, running, Get_config]),
    
    checkUnexpected(MeContent, ['RootS0']),

    ActualPostUpgradeInstances =
	sortSimpleXmlContent(
	  keepTriplets(
	    MeContent,
	    getTags(referencePostUpgradeInsts()))),

    ct:pal("actual post-upgrade MO instances:~n~n~p", [ActualPostUpgradeInstances]),

    case referencePostUpgradeInsts() of
	ActualPostUpgradeInstances ->
	    ct:pal("actual result equals expected result", []),
	    ok;
	_ ->
	    ct:pal("expected post-upgrade MO instances:~n~n~p", [referencePostUpgradeInsts()]),
	    ct:fail("mismatch between actual and expected MO instances after upgrade", [])
    end.


%% @doc Reference post-upgrade MO instances. This simple-XML structure
%% must be updated if the DX ECIM Toolchain models are updated, or if
%% the "upgrade engine" behaviour is changed. Lexicographic ordering
%% of classnames and attribute names must be observed.

referencePostUpgradeInsts() ->
    [{'Root',
      [{xmlns,"urn:com:ericsson:ecim:DEMO"}],
      [{'Leaf',[],[{area,[],["1738"]},{leafId,[],["1"]},{shape,[],["oval"]}]},
       {rootId,[],["1"]}]},
     {'RootS1V0',
      [{xmlns,"urn:com:ericsson:ecim:S1V0"}],
      [{'AlphaS1V0',[],[{alphaA,[],["17"]},{alphaS1V0Id,[],["1"]}]},
       {rootS1V0Id,[],["1"]}]},
     {'RootS1V1',
      [{xmlns,"urn:com:ericsson:ecim:S1V1"}],
      [{'GammaS1V1',[],
	[{'DeltaS1V1',[],[{deltaA,[],["17"]},{deltaS1V1Id,[],["A"]}]},
	 {gammaS1V1Id,[],["U"]}]},
       {rootS1V1Id,[],["1"]}]},
     {'RootS1V2',
      [{xmlns,"urn:com:ericsson:ecim:S1V2"}],
      [{'AlphaS1V2',[],
	[{alphaS,[],["bird"]},
	 {alphaS1V2Id,[],["1"]},
	 {alphaT,[],["blackbird"]}]},
       {rootS1V2Id,[],["1"]}]},
     {'RootS1V3',
      [{xmlns,"urn:com:ericsson:ecim:S1V3"}],
      [{'AlphaS1V3',[],[{alphaA,[],["17"]},{alphaS1V3Id,[],["1"]}]},
       {rootS1V3Id,[],["1"]}]},
     {'RootS2',
      [{xmlns,"urn:com:ericsson:ecim:S2"}],
      [{'AlphaS2',[],
	[{'BetaS2',[],
	  [{betaM,[],["krypton"]},
	   {betaM,[],["xenon"]},
	   {betaS2Id,[],["1"]}]},
	 {alphaA,[],["18"]},
	 {alphaS2Id,[],["2"]}]},
       {'AlphaS2',[],[{alphaA,[],["17"]},{alphaS2Id,[],["1"]}]},
       {rootS2Id,[],["1"]}]}
    ].



%% @doc Fails the testcase if any of the given triplets match
%% against the given list of atoms.

-spec checkUnexpected([tuple()], [atom()]) -> any().

checkUnexpected(Content, Tags) ->
    [
     case lists:member(Tag, Tags) of
	 true ->
	     ct:fail("unexpected appearance of MO subtree: ~p", [C]);
	 _ ->
	     ok
     end
     || {Tag, _, _}=C <- Content].


%% @doc Keep triplets from the given list if the 1st element
%% occurs in the given lists of tags.

-spec keepTriplets([tuple()], [atom()]) -> [tuple()].

keepTriplets(Content, Tags) ->
    lists:append(
      [case lists:member(Tag, Tags) of
	   true -> 
	       [C]; 
	   _ -> 
	       [] 
       end
       || {Tag, _, _}=C <- Content]).


%% @doc Returns the tags that occur in the given content.

-spec getTags([tuple()]) -> [atom()].

getTags(Content) ->
    [Tag || {Tag, _, _} <- Content].


%% @doc Sort a (restricted) simple-XML term recursively.
%% It is trusted that every simple_xml() term is a 3-tuple
%% (hopefully ct_netconfc works that way). It is also
%% assumed that an element contains text OR subelements
%% but not both.

sortSimpleXml(T) when is_tuple(T), tuple_size(T) =/= 3 ->
    ct:fail("cannot handle simple_xml: ~p");

sortSimpleXml({Tag, Attrs, Content}) ->
    {Tag, lists:sort(Attrs), sortSimpleXmlContent(Content)}.


sortSimpleXmlContent(Content) ->
    case isTextOnly(Content) of
	true ->
	    Content;
	_ ->
	    lists:sort([sortSimpleXml(C) || C <- Content])
    end.


isTextOnly([]) ->
    false;

isTextOnly([X]) when is_list(X) ->
    true;

isTextOnly(Content) ->
    case lists:member(false, [is_tuple(C) || C <- Content]) of
	true ->
	    ct:fail("cannot handle content: ~p", [Content]);
	_ ->
	    false
    end.


netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    ok = ct_netconfc:close_session(nc1),
    Res.
