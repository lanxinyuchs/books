%%% %CCaseFile:	comsa_avc_SUITE.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2016
%%% @version /main/R6A/2

%%% @doc ==Tests of AVC for runtime IMM entities==
%%%
%%% This suite uses the test_avc module in IFT.
%%%
%%% TODO: Revise testcases to use the more flexible IFT 'imm' module
%%% instead. Eventually the test_avc module can be eliminated.
%%%
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
%%% -----      -------    --------    ------------------------
%%% R6A/1      2016-06-03 erarafo     First version, copied from com_avc_imm_SUITE
%%% R6A/2      2016-06-20 erarafo     test_hu67516/1 works for old and new struct

-module(comsa_avc_SUITE).
-id('Updated by CCase').
-vsn('/main/R6A/2').
-date('2016-06-20').
-author('erarafo').

-export([test_create_delete_radon/1,
	 test_create_delete_francium/1,
	 test_update_rt_attr/1,
	 test_update_rt_attr_enum/1,
	 test_update_rt_attr_enum_out_of_range/1,
	 test_update_rt_attr_boolean_false/1,
	 test_update_rt_attr_boolean_true/1,
	 test_update_rt_attr_boolean_out_of_range/1,
	 test_add_change_remove_attr/1,
	 test_multi_values/1,

	 tmv1/1, tmv2/1, tmv3/1, tmv4/1, tmv5/1,

	 test_hu67516/1,
	 test_classE/1,
	 test_update_cfg_attr/1,
	 test_struct_version/1,
	 
	 test_oi_session/1
	]).

-export([
	 suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0
	]).

-include_lib("common_test/include/ct.hrl").
-include("comsa_avc.hrl").

-include("oi.hrl").
-include("imm.hrl").

-define(DU, du1).
-define(NODE, node1).
-define(CHILD_AVC, child_avc).
-define(CHILD_IMM, child_imm).
-define(CHILD_SELF, child_self).
-define(NC_USER, nc1).


%% Must match definition in ift_app: master.h
-define(AVC, 12).
-define(IMM, 22).
-define(SELF, 16).

%% Must match definition in ift_app: test_self.c
-define(GET_STRUCT_VERSION, 21).


%%% ----------------------------------------------------------
%%% COMMON TEST CALLBACK FUNCTIONS
%%% For descriptions, see the Common Test manual.
%%% ----------------------------------------------------------

-type config() :: [{atom(), term()}].


%% @hidden
-spec suite() -> config().

suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks,
      [{rct_htmllink,[]},
       {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"], []}}]}},
       {rct_proxy,[{1, ?NODE, ssh_lmt_ipv4, ?DU, username}]},
       {rct_netconf, ?NC_USER},
       {cth_conn_log, []},
       {rct_core,[]},
       {rct_cli, cli}
      ]}
    ].


%% @hidden
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    % for use by many testcases
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD_AVC, ?AVC),
    Config.


%% @hidden
-spec end_per_suite(config()) -> any().

end_per_suite(_Config) ->
    {ok, client_stopped} = rct_proxy:stop_proxy(?NODE, ?CHILD_AVC),
    ok.


%% @hidden
-spec init_per_group(atom(), config()) -> config().

init_per_group(_GroupName, Config) ->
    Config.


%% @hidden
-spec end_per_group(atom(), config()) -> any().

end_per_group(_GroupName, _Config) ->
    ok.


%% @hidden
-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
    if
	TestCase =:= test_oi_session ->
	    [{testContextImm, #testContext{node=?NODE, child=?CHILD_IMM}}|Config];
	TestCase =:= test_hu67516 ->
	    [{testContextImm, #testContext{node=?NODE, child=?CHILD_IMM}}|Config];
	TestCase =:= test_update_cfg_attr ->
	    [{testContextImm, #testContext{node=?NODE, child=?CHILD_IMM}}|Config];
	true ->
	    Config
    end.


%% @hidden
-spec end_per_testcase(atom(), config()) -> any().

end_per_testcase(TestCase, Config) ->
    if
	TestCase =:= test_oi_session ->
	    #testContext{node=Node, child=Child} = ?config(testContextImm, Config),
	    {ok, client_stopped} = rct_proxy:stop_proxy(Node, Child);
	TestCase =:= test_hu67516 ->
	    #testContext{node=Node, child=Child}=TC = ?config(testContextImm, Config),
	    imm:deleteConfigInst(TC, "testClassFId=881,TESTMOMtestRootId=1"),
	    {ok, client_stopped} = rct_proxy:stop_proxy(Node, Child);
	TestCase =:= test_update_cfg_attr ->
	    #testContext{node=Node, child=Child} = ?config(testContextImm, Config),
	    {ok, client_stopped} = rct_proxy:stop_proxy(Node, Child);
	true ->
	    ok
    end.


%% @hidden
-spec groups() -> [{atom(), list(), list()}].

groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], []},
     {sbc__upgrade__all__1__group, [], []},
     {sdc__cover__sim__1__group, [], []},
     {sdc__def__all__1__group, [], []},
     {sdc__qual__all__1__group, [], []},
     %% This suite can be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].


%% @hidden
-spec all() -> list() | {skip, term()}.

all() ->
    [
     test_create_delete_radon,
     test_create_delete_francium,
     test_update_rt_attr,
     test_add_change_remove_attr,
     test_update_rt_attr_enum,
     test_update_rt_attr_enum_out_of_range,
     test_update_rt_attr_boolean_false,
     test_update_rt_attr_boolean_true,
     test_update_rt_attr_boolean_out_of_range,
     %% ,test_multi_values
     test_hu67516,
     test_classE
    ].


%%% ----------------------------------------------------------
%%% TEST CASES
%%% ----------------------------------------------------------

%%% @doc Test creation and deletion of a runtime class instance
%%% in the NOBLE MOM.
%%% @end

test_create_delete_radon(_Config) ->

    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),

    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-radon"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    MOM = "NOBLE",
    ParentDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    Class = MOM++"Radon",
    RdnName = "radonId",
    RdnValue = "1",
    %InstanceMimDn = "1,Radon,1,Argon,1,Neon,1,Helium,1,ManagedElement",
    AttrName = "massNumber",
    AttrValue = "19",
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_CREATE_INSTANCE_SINGLE_ATTR,
				{Handle, ParentDn, Class, RdnName, RdnValue,
				 "uint32", AttrName, AttrValue}),

    pause(3000, "pause so that the instance can be seen in CLI"),

    Dn = RdnName++"="++RdnValue++","++ParentDn,
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DELETE_INSTANCE,
				{Handle, Dn}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    Pause = 2000,
    ct:pal("pause for notifications to arrive: ~w ms", [Pause]),
    timer:sleep(Pause),

    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications: ~p", [Notifications]),

    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    case Notifications of
	[] ->
	    ct:fail("expected TWO notifications, got zero",
		    []);
	[_] ->
	    ct:fail("expected TWO notifications, got just one",
		    []);
	[_, _, _|_] ->
	    ct:fail("expected TWO notifications, got more than that: ~w",
		    [length(Notifications)]);
	[_, _] ->
	    case Notifications of
		[{notification,
		  [{xmlns,
		    "urn:ietf:params:xml:ns:netconf:notification:1.0"}],
		  [{eventTime,[],[_TimeStamp1]},
		   {events,
		    [{xmlns,
		      "urn:ericsson:com:netconf:notification:1.0"},
		     {dnPrefix,[]}],
		    [{objectCreated,
		      [{dn,
			"ManagedElement=1,Helium=1,Neon=1,Argon=1,Radon=1"}],
		      [{attr,[{name,"massNumber"}],[{v,[],["19"]}]},
		       {attr,[{name,"persistentNickname"}],[]},
		       {attr,[{name,"volatileBird"}],[]},
		       {attr,[{name,"volatileBat"}],[]}]}]}]},
		 {notification,
		  [{xmlns,
		    "urn:ietf:params:xml:ns:netconf:notification:1.0"}],
		  [{eventTime,[],[_TimeStamp2]},
		   {events,
		    [{xmlns,
		      "urn:ericsson:com:netconf:notification:1.0"},
		     {dnPrefix,[]}],
		    [{objectDeleted,
		      [{dn,
			"ManagedElement=1,Helium=1,Neon=1,Argon=1,Radon=1"}],
		      []}]}]}]->
		    ok;
		_Other->
		    ct:fail("unexpected TWO notifications : ~w",
			       [Notifications])
	    end
    end.


%%  Fix this when SAF and NTF finally works
%%     case Notifications of
%% 	[{notification,
%%                 [{xmlns, XmlNs1}],
%%                 [{eventTime,[],[_Timestamp1]},
%%                  {events,
%%                      [{xmlns, XmlNs2},
%%                       {dnPrefix,[]}],
%%                      [{objectCreated,
%%                           [{dn, InstanceMimDn}],
%%                           [{attr,[{name,AttrName}],[{v,[],[AttrValue]}]}]}]}]},
%%             {notification,
%%                 [{xmlns, XmlNs1}],
%%                 [{eventTime,[],[_Timestamp2]},
%%                  {events,
%%                      [{xmlns, XmlNs2},
%%                       {dnPrefix,[]}],
%%                      [{objectDeleted,
%%                           [{dn, InstanceMimDn}],
%%                           []}]}]}] ->
%% 	    ok;
%% 	_ ->
%% 	    ct:fail("unexpected notifications: ~p", [Notifications])
%%
%%     ,
%%
%%     [{notification,
%%                            [{xmlns,
%%                              "urn:ietf:params:xml:ns:netconf:notification:1.0"}],
%%                            [{eventTime,[],["2014-04-11T15:11:48Z"]},
%%                             {events,
%%                              [{xmlns,
%%                                "urn:ericsson:com:netconf:notification:1.0"},
%%                               {dnPrefix,[]}],
%%                              [{objectCreated,
%%                                [{dn,
%%                                  "1,Radon,1,Argon,1,Neon,1,Helium,1,ManagedElement"}],
%%                                [{attr,
%%                                  [{name,"SaImmAttrImplemeterName"}],
%%                                  [{v,[],["implementer-radon"]}]},
%%                                 {attr,
%%                                  [{name,"massNumber"}],
%%                                  [{v,[],["19"]}]}]}]}]},
%%                           {notification,
%%                            [{xmlns,
%%                              "urn:ietf:params:xml:ns:netconf:notification:1.0"}],
%%                            [{eventTime,[],["2014-04-11T15:11:52Z"]},
%%                             {events,
%%                              [{xmlns,
%%                                "urn:ericsson:com:netconf:notification:1.0"},
%%                               {dnPrefix,[]}],
%%                              [{objectDeleted,
%%                                [{dn,
%%                                  "1,Radon,1,Argon,1,Neon,1,Helium,1,ManagedElement"}],
%%                                []}]}]}]
%%
%%
%%     end.


%%% @doc Test creation and deletion of a runtime class instance
%%% in the ALKALI MOM. The root class of ALKALI has a contribution
%%% (interMim) relation to a non-root class of NOBLE. The DNs used
%%% extend all the way up to the NOBLE root class.
%%% @end

test_create_delete_francium(_Config) ->

    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),

    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-francium"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    TOPMOM = "NOBLE",
    SUBMOM = "ALKALI",
    ParentDn = SUBMOM++"lithiumId=1,neonId=1,"++TOPMOM++"heliumId=1",
    Class = SUBMOM++"Francium",
    RdnName = "franciumId",
    RdnValue = "1",
    AttrName = "halfLife",
    AttrValue = "27300",

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_CREATE_INSTANCE_SINGLE_ATTR,
				{Handle, ParentDn, Class, RdnName, RdnValue,
				 "int32", AttrName, AttrValue}),

    pause(3000, "pause so that the instance can be seen in CLI"),

    Dn = RdnName++"="++RdnValue++","++ParentDn,
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DELETE_INSTANCE, {Handle, Dn}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    pause(2000, "allow notifications to go through"),

    R = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected: ~p", [R]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    case R of
	[{notification,
	  [{xmlns, XmlNs1}],
	  [{eventTime, [], [_]},
	   {events,
	    [{xmlns, XmlNs2},
	     {dnPrefix, []}],
	    [{objectCreated,
	      [{dn, "ManagedElement=1,Helium=1,Neon=1,Lithium=1,Francium=1"}],
	      [{attr, [{name, AttrName}],[{v,[],[AttrValue]}]}]}]}]},
	 {notification,
	  [{xmlns, XmlNs1}],
	  [{eventTime, [], [_]},
	   {events,
	    [{xmlns, XmlNs2},
	     {dnPrefix, []}],
	    [{objectDeleted,
	      [{dn, "ManagedElement=1,Helium=1,Neon=1,Lithium=1,Francium=1"}],
	      []}]}]}] ->
	    ok;
	Other ->
	    ct:fail("unexpected notifications: ~p", [Other])
    end.


%%% @doc Test update of a runtime attribute in a config class.
%%% @end

test_update_rt_attr(_Config) ->

    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),

    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-krypton"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    ImmDn = "kryptonId=1,argonId=1,neonId=1,NOBLEheliumId=1",
    MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1,Krypton=1",
    AttrName = "atomicNumber",
    AttrValue = 36,
    AttrValueS = integer_to_list(AttrValue),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_SET_32,
				{Handle, ImmDn, AttrName, 1, AttrValue}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    pause(2000, "allow notifications to get out"),

    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),
    case Notifications of
	 [{notification,
                [{xmlns, _XmlNs1}],
                [{eventTime, [], [_Timestamp]},
                 {events,
                     [{xmlns, _XmlNs2},
                      {dnPrefix, []}],
                     [{'AVC',
                          [{dn, MimDn}],
                          [{attr,
                               [{name, AttrName}],
                               [{v,[],[AttrValueS]}]}]}]}]}] ->
	     ok;
	_ ->
	     ct:fail("unexpected notification: ~p", [Notifications])
    end.


%%% ----------------------------------------------------------
%%% @doc Demonstrate the HT52686 issue. The notification that
%%% goes out from COM should be the ENUM string, not the numeric
%%% value.
%%% @end
%%% ----------------------------------------------------------

test_update_rt_attr_enum(_Config) ->

    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),

    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "rank",
    AttrValue = 1,
    AttrValueE = rankEnumValue(AttrValue),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_SET_32,
				{Handle, ImmDn, AttrName, 0, AttrValue}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    pause(2000, "allow notifications to get out"),

    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    Expected =
    [{notification,
	  [{xmlns, "any.."}],
	  [{eventTime, [], ["any....."]},
	   {events,
	    [{xmlns, "any..."},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],[AttrValueE]}]}]}]}]}],

    case Notifications of
	[{notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_Timestamp]},
	   {events,
	    [{xmlns, _XmlNs2},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],[AttrValueE]}]}]}]}]}] ->
	    ok;
	_ ->
	    ct:fail("wrong notification;~n"
		    "expected: ~p~n"
		    "actual:~p",
		    [Expected, Notifications])
    end.

rankEnumValue(0) -> "KNIGHT";
rankEnumValue(1) -> "BARON";
rankEnumValue(2) -> "DUJE".       %% ought to be DUKE; adapted to a defect in FAKE :(


%%% ----------------------------------------------------------
%%% @doc If NTF sends a notification with an out-of-range
%%% value then COI will produce a "no value" result which is
%%% then used for the notification. There is no warning.
%%% @end
%%% ----------------------------------------------------------

test_update_rt_attr_enum_out_of_range(_Config) ->
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "rank",
    AttrValue = 99,   % totally out of range

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_SET_32,
				{Handle, ImmDn, AttrName, 0, AttrValue}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    pause(2000, "allow notifications to get out"),

    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    Expected =
    [{notification,
	  [{xmlns, "any.."}],
	  [{eventTime, [], ["any....."]},
	   {events,
	    [{xmlns, "any..."},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],[]}]}]}]}]}],

    case Notifications of
	[{notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_Timestamp]},
	   {events,
	    [{xmlns, _XmlNs2},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],[]}]}]}]}]}] ->
	    ok;
	_ ->
	    ct:fail("wrong notification;~n"
		    "expected: ~p~n"
		    "actual:~p",
		    [Expected, Notifications])
    end.





%%% ----------------------------------------------------------
%%% @doc Set a runtime boolean attribute to false.
%%% @end
%%% ----------------------------------------------------------

test_update_rt_attr_boolean_false(_Config) ->

    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),

    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "isInert",
    Unsigned = 0,
    AttrValue = 0, % assuming this represents 'false'

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_SET_32,
				{Handle, ImmDn, AttrName, Unsigned, AttrValue}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    pause(2000, "allow notifications to get out"),

    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    Expected =
    [{notification,
	  [{xmlns, "any.."}],
	  [{eventTime, [], ["any....."]},
	   {events,
	    [{xmlns, "any..."},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],["false"]}]}]}]}]}],

    case Notifications of
	[{notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_Timestamp]},
	   {events,
	    [{xmlns, _XmlNs2},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],["false"]}]}]}]}]}] ->
	    ok;
	_ ->
	    ct:fail("wrong notification;~n"
		    "expected: ~p~n"
		    "actual:~p",
		    [Expected, Notifications])
    end.


%%% ----------------------------------------------------------
%%% @doc Set a runtime boolean attribute to true.
%%% @end
%%% ----------------------------------------------------------

test_update_rt_attr_boolean_true(_Config) ->
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "isInert",
    Unsigned = 0,
    AttrValue = 1, % true

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_SET_32,
				{Handle, ImmDn, AttrName, Unsigned, AttrValue}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    pause(2000, "allow notifications to get out"),

    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    Expected =
    [{notification,
	  [{xmlns, "any.."}],
	  [{eventTime, [], ["any....."]},
	   {events,
	    [{xmlns, "any..."},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],["true"]}]}]}]}]}],

    case Notifications of
	[{notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_Timestamp]},
	   {events,
	    [{xmlns, _XmlNs2},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],["true"]}]}]}]}]}] ->
	    ok;
	_ ->
	    ct:fail("wrong notification;~n"
		    "expected: ~p~n"
		    "actual:~p",
		    [Expected, Notifications])
    end.


%%% ----------------------------------------------------------
%%% @doc Set a runtime boolean attribute to an out-of-range
%%% value. IMM will treat this value as any INT32 value and
%%% NTF will perform a callback with this value. COI will
%%% map the value to 'true' with no warning.
%%% @end
%%% ----------------------------------------------------------

test_update_rt_attr_boolean_out_of_range(_Config) ->
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),
    ct:pal("OI session opened, handle: ~s", [Handle]),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "isInert",
    Unsigned = 0,
    AttrValue = 11, % invalid value; valid values are 0 and 1

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_SET_32,
				{Handle, ImmDn, AttrName, Unsigned, AttrValue}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),
    ct:pal("OI session closed, handle: ~s", [Handle]),

    pause(2000, "allow notifications to get out"),

    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    Expected =
    [{notification,
	  [{xmlns, "any.."}],
	  [{eventTime, [], ["any....."]},
	   {events,
	    [{xmlns, "any..."},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],["true"]}]}]}]}]}],

    case Notifications of
	[{notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_Timestamp]},
	   {events,
	    [{xmlns, _XmlNs2},
	     {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,
		[{name, AttrName}],
		[{v,[],["true"]}]}]}]}]}] ->
	    ok;
	_ ->
	    ct:fail("wrong notification;~n"
		    "expected: ~p~n"
		    "actual:~p",
		    [Expected, Notifications])
    end.










%%% @doc Add an attribute value, update it, and remove it. All
%%% operations rely on the SAF IMM OI "object update" function,
%%% using the "replace" mode of operation. The attribute is
%%% singled-valued; IMM allows it to have zero or one values.

test_add_change_remove_attr(_Config) ->
    Delay = 1000,
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "a",
    AttrValue = "strawberry",
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_ADD_STRING,
				{Handle, ImmDn, AttrName, AttrValue}),

    pause(Delay, "change can be viewed in CLI"),

    AttrValueOther = "blackberry",
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_UPDATE_STRING,
				{Handle, ImmDn, AttrName, AttrValueOther}),

    pause(Delay, "change can be viewed in CLI"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_REMOVE_STRING,
				{Handle, ImmDn, AttrName}),



    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),


    pause(2000, "allow notifications to get out"),
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),


    case Notifications of
	[{notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_TS1]},
	   {events,
	    [{xmlns, _XmlNs2}, {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr, [{name, AttrName}], [{v, [], [AttrValue]}]}]}]}]},
	 {notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_TS2]},
	   {events,
	    [{xmlns, _XmlNs2}, {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr,[{name, AttrName}], [{v, [], [AttrValueOther]}]}]}]}]},
	 {notification,
	  [{xmlns, _XmlNs1}],
	  [{eventTime, [], [_TS3]},
	   {events,
	    [{xmlns, _XmlNs2}, {dnPrefix, []}],
	    [{'AVC',
	      [{dn, MimDn}],
	      [{attr, [{name, AttrName}], []}]}]}]}]
	  ->
	    ok;
	_ ->
	    ct:fail("unexpected notification: ~p", [Notifications])
    end,
    ok.


%%% @doc Exercise operations on a multi-value attribute which is
%%% RUNTIME, CACHED, NOTIFY. Unclear if this works as of 2014-03-26.
%%% See also the tmv1 thru tmv5 tests for running stepwise.

test_multi_values(_Config) ->
    Delay = 15000,
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    %MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "c",

    AttrValues1 = ["strawberry", "raspberry"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_ADD_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues1}),
    pause(Delay, "expect: strawberry, raspberry"),

    AttrValues2 = ["plum", "pear", "apple"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_ADD_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues2}),
    pause(Delay, "expect: strawberry, raspberry, plum, pear, apple"),

    AttrValues3 = ["apple", "strawberry"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DELETE_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues3}),
    pause(Delay, "expect: raspberry, plum, pear"),

    AttrValues4 = ["potato", "carrot"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_REPLACE_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues4}),
    pause(Delay, "expect: potato, carrot"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_CLEAR_STRINGS,
				{Handle, ImmDn, AttrName}),
    pause(Delay, "expect: EMPTY"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),

    pause(2000, "allow notifications to get out"),
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    ct:print("notifications: ~p", [Notifications]),
    ok.


%%% @doc Add strings.
%%% When precondition is zero values present: Only the first value
%%% is used.

tmv1(_Config) ->
    Delay = 100,
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    %MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "c",

    AttrValues1 = ["strawberry", "raspberry"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_ADD_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues1}),
    pause(Delay, "expect: strawberry, raspberry"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DISP, {Handle}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),

    pause(2000, "allow notifications to get out"),
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),
    ct:print("notifications: ~p", [Notifications]),
    ok.


%%% @doc If this test is run after tmv1 then some internally corrupt state
%%% occurs? Displaying the attribute in COM CLI no longer works, and
%%% accessing it via OM fails with code: SA_AIS_ERR_UNAVAILABLE

tmv2(_Config) ->
    Delay = 100,
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    %MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "c",

    AttrValues2 = ["plum", "pear", "apple"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_ADD_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues2}),
    pause(Delay, "expect: strawberry, raspberry, plum, pear, apple"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DISP, {Handle}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),


    pause(2000, "allow notifications to get out"),
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),
    ct:print("notifications: ~p", [Notifications]),
    ok.


%%% @doc Test: Remove two values. Preconditions can be: values
%%% exist, values do not exist.
%%% If run when values do not exist an error occurs. The code is
%%% SA_AIS_ERR_INVALID_PARAM, which is reasonable but not explicitly
%%% listed in the SAF documentation.
%%% @end

tmv3(_Config) ->
    Delay = 1000,
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    %MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "c",

    AttrValues3 = ["apple", "strawberry"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DELETE_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues3}),
    pause(Delay, "expect: raspberry, plum, pear"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DISP, {Handle}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),

    pause(2000, "allow notifications to get out"),
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),
    ct:print("notifications: ~p", [Notifications]),
    ok.

%%% @doc Test: Replacing existing attribute values with 2 strings.
%%% Status 2014-03-26: Apparently only one value is used. This one
%%% value (potato) can be seen in CLI and in the OM interface.
%%% @end

tmv4(_Config) ->
    Delay = 100,
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    %MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "c",

    AttrValues4 = ["potato", "carrot"],
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_REPLACE_STRINGS,
				{Handle, ImmDn, AttrName, AttrValues4}),
    pause(Delay, "expect: potato, carrot"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DISP, {Handle}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),

    pause(2000, "allow notifications to get out"),
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),
    ct:print("notifications: ~p", [Notifications]),
    ok.

%%% @doc Test: Replacing existing attribute values with 0 strings.
%%% Status 2014-03-26: Works properly.
%%% @end

tmv5(_Config) ->
    Delay = 100,
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    {ok, Handle} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_INIT),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_SET,
				{Handle, "implementer-argon"}),

    ImmDn = "argonId=1,neonId=1,NOBLEheliumId=1",
    %MimDn = "ManagedElement=1,Helium=1,Neon=1,Argon=1",
    AttrName = "c",

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_CLEAR_STRINGS,
				{Handle, ImmDn, AttrName}),
    pause(Delay, "expect: EMPTY"),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_DISP, {Handle}),

    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_IMPL_CLEAR, {Handle}),
    {ok} = rct_proxy:send_proxy(?NODE, ?CHILD_AVC, ?AVC_FIN, {Handle}),

    pause(2000, "allow notifications to get out"),
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications:~n~p", [Notifications]),
    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),
    ct:print("notifications: ~p", [Notifications]),
    ok.


%%% ----------------------------------------------------------
%%% @doc This TC was used to verify the provisional solution
%%% for AVCs when updating a seq-of-struct attribute.
%%%
%%% The actions of the TC depend on whether the old or new
%%% struct implementation is in effect.
%%%
%%% NOTE: end_per_testcase/1 has hardcoded knowledge of the
%%% name of this TC.
%%% @end
%%% ----------------------------------------------------------
test_hu67516(Config) ->
    case structVersion() of
	object ->
	    test_hu67516_object(Config);
	attribute ->
	    test_hu67516_attribute(Config)
    end.


test_hu67516_attribute(Config) ->
    ct:pal("struct implemented as: attribute", []),
    TC = ?config(testContextImm, Config),
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD_IMM, ?IMM),
    
    % create a config instance with a runtime seq-of-struct attribute
    imm:createConfigInst(TC,
			 "TESTMOMtestRootId=1", 
			 "TESTMOMTestClassF", 
			 "testClassFId", 
			 "881"),
    ct:pal("config instance created", []),
    % some delay so that the "create" notification is not captured
    timer:sleep(2000),
    
    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),
    
    % get an OI handle
    {ok, {H, OiVersion}} = imm:oiInitialize(TC),
    ct:pal("OI handle: ~p, version: ~p", [H, OiVersion]),
    
    ok = imm:oiImplementerSet(TC, H, "lars_magnus"),
    ct:pal("OI implementer set", []),
    
    % assign a sequence of struct values
    Object = "testClassFId=881,TESTMOMtestRootId=1",
    
    Struct11=#struct{name="TESTMOMStructR",
		    members=[#memberValues{name="intA", type=int32, values=[102]},
			     #memberValues{name="intB", type=int32, values=[102]}]},
    
    Struct12=#struct{name="TESTMOMStructR",
		    members=[#memberValues{name="intA", type=int32, values=[103]},
			     #memberValues{name="intB", type=int32, values=[103]}]},
    
    AttrValues1 = #attrValues{name="structSeqA", type=struct, values=[Struct11, Struct12]},
    AttrMod1 = #attrMod{modType=?SA_IMM_ATTR_VALUES_REPLACE, modAttr=AttrValues1},
    ok = imm:oiRtObjectUpdate(TC, H, Object, [AttrMod1]),
    ct:pal("seq-of-struct attribute assigned", []),
    
    % ct:break("can the seq-of-struct attribute be inspected?"),
    
    % assign a different sequence of struct values
    Struct21=#struct{name="TESTMOMStructR",
		    members=[#memberValues{name="intA", type=int32, values=[902]},
			     #memberValues{name="intB", type=int32, values=[902]}]},
    
    Struct22=#struct{name="TESTMOMStructR",
		    members=[#memberValues{name="intA", type=int32, values=[903]},
			     #memberValues{name="intB", type=int32, values=[903]}]},
    
    AttrValues2 = #attrValues{name="structSeqA", type=struct, values=[Struct21, Struct22]},
    AttrMod2 = #attrMod{modType=?SA_IMM_ATTR_VALUES_REPLACE, modAttr=AttrValues2},
    ok = imm:oiRtObjectUpdate(TC, H, Object, [AttrMod2]),
    ct:pal("seq-of-struct attribute updated", []),
    
    % clear the seq-of-struct attribute
    ok = imm:oiRtObjectUpdate(TC,
			      H,
			      "testClassFId=881,TESTMOMtestRootId=1",
			      [#attrMod{modType= ?SA_IMM_ATTR_VALUES_REPLACE,
					modAttr=#attrValues{name="structSeqA",
							    type=struct,
							    values=[]}}]),
    ct:pal("struct attribute cleared", []),
    % timer:sleep(5000),

    
    %-spec oiImplementerClear(testContext(), oiHandle()) -> ok | {error, any()}.

    ok = imm:oiImplementerClear(TC, H),
    ct:pal("implementer cleared", []),
    
    % finalize the handle
    ok = imm:oiFinalize(TC, H),
    ct:pal("OI handle finalized, handle: ~w", [H]),
    
    
    % pause to let all AVCs out
    timer:sleep(2000), 
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications: ~p", [Notifications]),

    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),
    
        case Notifications of
    	[] ->
    	    ct:fail("expected 3 notifications, got zero", []);
    	[_] ->
    	    ct:fail("expected 3 notifications, got just one", []);
    	[_, _] ->
    	    ct:fail("expected 3 notifications, got just two", []);
    	[_, _, _] ->
	    verifyNotificationsHu67516(Notifications);
	Other ->
    	    ct:fail("expected 3 notifications, got more: ~w", [length(Other)])
    end.


test_hu67516_object(Config) ->
    TC = ?config(testContextImm, Config),
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD_IMM, ?IMM),

    imm:createConfigInst(TC,
      "TESTMOMtestRootId=1", "TESTMOMTestClassF", "testClassFId", "881"),
    % some delay so that the "create" notification is not captured
    timer:sleep(2000),

    {ok, NcSubscr} = comsa_avc_netconf_subscriber:start(?NC_USER),

    {ok, {H, OiVersion}} = imm:oiInitialize(TC),
    ct:pal("OI handle: ~p, version: ~p", [H, OiVersion]),
    ok = imm:oiImplementerSet(TC, H, "lars_magnus"),

    Indexes = [1, 2],
    ct:pal("======== create structs using 101", []),
    [ok, ok] = createStructsHu67516(TC, H, 101, Indexes),
    % timer:sleep(5000),

    ct:pal("======== update attribute", []),
    ok = replAttrHu67516(TC, H, Indexes),
    % some delay needed to ensure that any correlation
    % entries time out
    timer:sleep(3000),

    ct:pal("======== new structs using 901", []),
    [ok, ok] = deleteStructsHu67516(TC, H, Indexes),
    [ok, ok] = createStructsHu67516(TC, H, 901, Indexes),
    % some delay needed to ensure that the delete-create
    % correlation in comsaEvent has time to act
    timer:sleep(3000),

    ct:pal("======== update attribute again", []),
    ok = replAttrHu67516(TC, H, Indexes),

    ct:pal("======== del structs", []),
    [ok, ok] = deleteStructsHu67516(TC, H, Indexes),

    ct:pal("======== clear attr", []),
    ok = clearAttrHu67516(TC, H),

    ct:pal("======== finalize OI session", []),
    ok = imm:oiFinalize(TC, H),

    % pause to let all AVCs out
    timer:sleep(2000), 
    Notifications = comsa_avc_netconf_subscriber:getNotifications(NcSubscr),
    ct:pal("collected notifications: ~p", [Notifications]),

    ok = comsa_avc_netconf_subscriber:stop(NcSubscr),

    case Notifications of
    	[] ->
    	    ct:fail("expected 3 notifications, got zero", []);
    	[_] ->
    	    ct:fail("expected 3 notifications, got just one", []);
    	[_, _] ->
    	    ct:fail("expected 3 notifications, got just two", []);
    	[_, _, _] ->
	    verifyNotificationsHu67516(Notifications);
	Other ->
    	    ct:fail("expected 3 notifications, got more: ~w", [length(Other)])
    end.


verifyNotificationsHu67516(Actual) ->
    Expected = 
	[{notification,
	  [{xmlns,
	    "urn:ietf:params:xml:ns:netconf:notification:1.0"}],
	  [{eventTime,[],['*']}, % timestamp
	   {events,
	    [{xmlns,
	      "urn:ericsson:com:netconf:notification:1.0"},
	     {dnPrefix,[]}],
	    [{'AVC',
	      [{dn,
		"ManagedElement=1,TestRoot=1,TestClassF=881"}],
	      [{attr,
		[{name,"structSeqA"}],
		[{v,[],
		  [{elem,[{name,"intA"}],[{v,[],["102"]}]},
		   {elem,[{name,"intB"}],[{v,[],["102"]}]}]},
		 {v,[],
		  [{elem,[{name,"intA"}],[{v,[],["103"]}]},
		   {elem,
		    [{name,"intB"}],
		    [{v,[],["103"]}]}]}]}]}]}]},
	 {notification,
	  [{xmlns,
	    "urn:ietf:params:xml:ns:netconf:notification:1.0"}],
	  [{eventTime,[],['*']},  % timestamp
	   {events,
	    [{xmlns,
	      "urn:ericsson:com:netconf:notification:1.0"},
	     {dnPrefix,[]}],
	    [{'AVC',
	      [{dn,
		"ManagedElement=1,TestRoot=1,TestClassF=881"}],
	      [{attr,
		[{name,"structSeqA"}],
		[{v,[],
		  [{elem,[{name,"intA"}],[{v,[],["902"]}]},
		   {elem,[{name,"intB"}],[{v,[],["902"]}]}]},
		 {v,[],
		  [{elem,[{name,"intA"}],[{v,[],["903"]}]},
		   {elem,
		    [{name,"intB"}],
		    [{v,[],["903"]}]}]}]}]}]}]},
	 {notification,
	  [{xmlns,
	    "urn:ietf:params:xml:ns:netconf:notification:1.0"}],
	  [{eventTime,[],['*']},  % timestamp
	   {events,
	    [{xmlns,
	      "urn:ericsson:com:netconf:notification:1.0"},
	     {dnPrefix,[]}],
	    [{'AVC',
	      [{dn,
		"ManagedElement=1,TestRoot=1,TestClassF=881"}],
	      [{attr,[{name,"structSeqA"}],[]}]}]}]}],
    
    lists:map(
      fun({E, A}) ->
	      case match(E, A) of
		  true ->
		      ok;
		  false ->
		    ct:fail("bad match: expected: ~p~n             actual: ~p", [E, A])
	      end
      end,
      lists:zip(Expected, Actual)).


match([], []) ->
    true;

match([], _Other) ->
    false;

match([HeadE|TailE], [HeadA|TailA]) ->
    match(HeadE, HeadA) andalso match(TailE, TailA);

match({v, [], ListE}, {v, [], ListA}) when is_list(ListE) andalso is_list(ListA) ->
    match(lists:sort(ListE), lists:sort(ListA));

match(TupleE, TupleA) when is_tuple(TupleE) andalso 
			       is_tuple(TupleA) andalso 
			       tuple_size(TupleE) =:= tuple_size(TupleA) ->
    matchTuple(TupleE, TupleA, tuple_size(TupleE));

match(TupleE, TupleA) when is_tuple(TupleE) andalso is_tuple(TupleA) ->
    false;

match('*', _) ->
    true;

match(X, X) ->
    true;

match(_, _) ->
    false.


matchTuple(_, _, 0) ->
    true;

matchTuple(E, A, K) ->
    match(element(K, E), element(K, A)) andalso matchTuple(E, A, K-1).
    


test_classE(_Config)->
     rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
     rct_cli:send(cli, "configure"),
     rct_cli:send(cli, "TestClassE=2"),
     rct_cli:send(cli, "intA=11"),
     rct_cli:send(cli, "intB=22"),
     rct_cli:send(cli, "intC=55"),
     rct_cli:send(cli, "strE=\"strE\""),
     rct_cli:send(cli, "commit"),
     %% Remove strE
     rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
     rct_cli:send(cli, "configure"),
     rct_cli:send(cli, "TestClassE=2"),
     rct_cli:send(cli, "no strE"),
     rct_cli:send(cli, "commit"),

     %% Add structF
     rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
     rct_cli:send(cli, "configure"),
     rct_cli:send(cli, "TestClassE=2"),
     rct_cli:send(cli, "structF"),
     rct_cli:send(cli, "intA=11"),
     rct_cli:send(cli, "intSeqB=[12,23,34,34]"),
     rct_cli:send(cli, "commit"),

     %% Remove structF
     rct_cli:send(cli, "ManagedElement=1,TestRoot=1"),
     rct_cli:send(cli, "configure"),
     rct_cli:send(cli, "TestClassE=2"),
     rct_cli:send(cli, "no structF"),
     rct_cli:send(cli, "commit"),
    ok.


test_update_cfg_attr(Config) ->
    % start a client
    TC = ?config(testContextImm, Config),
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD_IMM, ?IMM),
    
    {ok, {ImmHandle, _ImmVersion}} = imm:omInitialize(TC),
    
    {ok, AccHandle} = imm:omAccessorInitialize(TC, ImmHandle),
    
    {ok, [#attrValues{values=[InitialValue]}]} = 
	imm:omAccessorGet(TC, AccHandle, "testClass1Id=1,TESTMOMtestRootId=1", ["int32"]),
    ct:pal("initial value: ~w", [InitialValue]),
    
    ok = imm:omAccessorFinalize(TC, AccHandle),
    
    {ok, AoHandle} = imm:omAoInitialize(TC, ImmHandle, "pettson", true),
    
    ok = imm:omAoSet(TC, AoHandle, ["testClass1Id=1,TESTMOMtestRootId=1"], ?SA_IMM_ONE),
    
    {ok, CcbHandle} = imm:omCcbInitialize(TC, AoHandle, ?SA_IMM_CCB_NO_FLAG),
    
    AttrMods =
	[#attrMod{modType=?SA_IMM_ATTR_VALUES_REPLACE, 
		  modAttr=#attrValues{name="int32", type=int32, values=[4711]}}],
    ok = imm:omCcbObjModify(TC, CcbHandle, "testClass1Id=1,TESTMOMtestRootId=1", AttrMods),
    
    ok = imm:omCcbApply(TC, CcbHandle),
    
    ok = imm:omFinalize(TC, ImmHandle),
    

    % restore the initial value
    {ok, {ImmHandle2, _ImmVersion2}} = imm:omInitialize(TC),
    
    {ok, AoHandle2} = imm:omAoInitialize(TC, ImmHandle2, "findus", true),
    
    ok = imm:omAoSet(TC, AoHandle2, ["testClass1Id=1,TESTMOMtestRootId=1"], ?SA_IMM_ONE),
    
    {ok, CcbHandle2} = imm:omCcbInitialize(TC, AoHandle2, ?SA_IMM_CCB_NO_FLAG),
    
    AttrMods2 =
	[#attrMod{modType=?SA_IMM_ATTR_VALUES_REPLACE, 
		  modAttr=#attrValues{name="int32", type=int32, values=[InitialValue]}}],
    ok = imm:omCcbObjModify(TC, CcbHandle2, "testClass1Id=1,TESTMOMtestRootId=1", AttrMods2),
    
    ok = imm:omCcbApply(TC, CcbHandle2),
    
    ok = imm:omFinalize(TC, ImmHandle2).


%%% ----------------------------------------------------------
%%% @doc Sanity check: Test that an OI session can be created
%%% and finalized immediately.
%%% @end
%%% ----------------------------------------------------------
test_oi_session(Config) ->
    TC = ?config(testContextImm, Config),
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD_IMM, ?IMM),

    {ok, {H, OiVersion}} = imm:oiInitialize(TC),
    ct:pal("OI handle: ~p, is_integer: ~p, OI version: ~p", [H, is_integer(H), OiVersion]),
    
    ok = imm:oiFinalize(TC, H),
    ct:pal("handle finalized: ~p", [H]).


%%% ----------------------------------------------------------
%%% @doc This TC reports the struct implementation to the
%%% console.
%%% @end
%%% ----------------------------------------------------------
test_struct_version(_Config) ->
    ct:pal("struct version is: ~w", [structVersion()]),
    ok.


pause(Millis, Message) ->
    ct:print("pause ~w ms: ~s", [Millis, Message]),
    timer:sleep(Millis).


createStructsHu67516(TC, H, ValueBase, Indexes) ->
    lists:map(
      fun(Index) ->
	      RdnValue = "structSeqA_"++integer_to_list(Index),
	      imm:oiObjCreate(TC, H,
		     "TESTMOMStructR",
		     "testClassFId=881,TESTMOMtestRootId=1",
		     [#attrValues{name="id", type=string, values=[RdnValue]},
		      #attrValues{name="intA", type=int32, values=[ValueBase + Index]},
		      #attrValues{name="intB", type=int32, values=[ValueBase + Index]}
		     ])
      end,
      Indexes).

replAttrHu67516(TC, H, Indexes) ->
    Dn = "testClassFId=881,TESTMOMtestRootId=1",
    AttrName = "structSeqA",
    Values =
	["id="++AttrName++"_"++integer_to_list(Index)++","++Dn
	 || Index <- Indexes],
    imm:oiRtObjectUpdate(
      TC, H, Dn,
      [#attrMod{modType= ?SA_IMM_ATTR_VALUES_REPLACE,
		modAttr=#attrValues{name=AttrName,
				    type=name,
				    values=Values}}]).

clearAttrHu67516(TC, H) ->
    imm:oiRtObjectUpdate(
      TC, H, "testClassFId=881,TESTMOMtestRootId=1",
      [#attrMod{modType= ?SA_IMM_ATTR_VALUES_REPLACE,
		modAttr=#attrValues{name="structSeqA",
				    type=name,
				    values=[]}}]).


deleteStructsHu67516(TC, H, Indexes) ->
    Dn = "testClassFId=881,TESTMOMtestRootId=1",
    AttrName = "structSeqA",
    [imm:oiObjDelete(TC, H, "id="++AttrName++"_"++integer_to_list(Index)++","++Dn)
    ||Index <- Indexes].



-spec structVersion() -> object|attribute|undefined.

structVersion() ->
    {ok, client_started} = rct_proxy:start_proxy(?NODE, ?CHILD_SELF, ?SELF),
    {ok, Result} = rct_proxy:send_proxy(?NODE, ?CHILD_SELF, ?GET_STRUCT_VERSION, {}),
    {ok, client_stopped} = rct_proxy:stop_proxy(?NODE, ?CHILD_SELF),
    Result.
