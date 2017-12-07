%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxlg
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/3
%%%
%%% @doc ==New radio model==
%%% Basic implementation of the New radio model based on the S&T prototype
%%% @end

-module(nr).
-vsn('/main/R9A/3').
-date('2017-04-06').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      -------    --------    ------------------------
%%% xxxx	20170314  etxlg		V1->V2 + misc.
%%% R9A/2	20170323  etxlg		bookkeeping delete
%%% R9A/1	20170405  etxlg		Copied into FAKE
%%% R9A/2	20170405  etxlg		deleted io:format
%%% R9A/3	20170406  etxlg		Fixed typo in childspec
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% This is the callback interface towards COM

-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 action/4,
	 createMo/5]).

%% This is part of RCS startup procedure
-export([children/0, activate/0]).

-export([instPhSeqBeg_begin/0, instPhParallel_init/1,
	 instPhParallel_init_data/0, instPhSeqBeg_post_init/0,
	 instPhParallel_post_init/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).
-include("NrFunction.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% @doc Returns true if the specified instance exists.
%%% @end
%%% ----------------------------------------------------------

-spec existsMo([binary()], integer()) -> boolean().

existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%%% ----------------------------------------------------------
%%% @doc Returns the number of MO instances of given class directly below the specified parent.
%%% ----------------------------------------------------------

-spec countMoChildren([binary()], binary(), integer()) -> non_neg_integer().

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).


%%% ----------------------------------------------------------
%%% @doc Gets MO attribute values. 
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% ----------------------------------------------------------

getMoAttributes(AttrNames, DnRev, TxHandle) ->
    [getMoAttribute([AttrName|DnRev], TxHandle)||AttrName<-AttrNames].


getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).




%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).


setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).


setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


%%% ----------------------------------------------------------
%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.
%%% ----------------------------------------------------------


createMo([ClassName | ParentDnRev],
	 _KeyAttrName,
	 KeyValue,
	 InitAttrs,
	 _TransId) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).


deleteMo(DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev,Table).


prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

action(_Name, _DnRev, _Params, _TransId) ->
    ok.



instPhSeqBeg_begin() ->
    ok.

instPhParallel_init(DbNodes) ->
%v1->v2
%    Tables = [{bpu, ?bpu_types},
%	      {mme, ?mme_types},
%	      {nrFunction, ?nrFunction_types},
%	      {sa, ?sa_types},
%	      {sau, ?sau_types},
%	      {slice, ?slice_types},
%	      {tp, ?tp_types}],
    Tables = [{nrFunction, ?nrFunction_types},
	      {mme, ?mme_types},
	      {domain, ?domain_types},
	      {vue, ?vue_types},
	      {vsa, ?vsa_types},
	      {vpp, ?vpp_types},
	      {sa, ?sa_types},
	      {bpu, ?bpu_types},
	      {tp, ?tp_types},
	      {radioConfig, ?radioConfig_types},
	      {drbPdcp, ?drbPdcp_types},
	      {srbPdcp, ?srbPdcp_types},
	      {drbLogicalChannel, ?drbLogicalChannel_types},
	      {srbLogicalChannel, ?srbLogicalChannel_types},
	      {mac, ?mac_types},
	      {phys, ?phys_types}],

    [create_table(TableDef, DbNodes)||TableDef<-Tables],
    ok.

create_table({Name, Types}, DbNodes) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]).

add_clh_option(_) ->
    [].

instPhParallel_init_data() ->
    mnesia:dirty_write(#nrFunction{nrFunctionId = {"1","1"}, release="1.0",
				   userLabel = ""}).

instPhSeqBeg_post_init() ->
    %% Register this module as callback for the NrFunction branch
    %%comsaI:register_callback(["ManagedFunction","NrFunction"], ?MODULE),
    comsaI:register_callback(["ManagedElement","NrFunction"], ?MODULE),
    %% Register the MO classes for NETCONF events
%v1->v2
%    comsaI:register_subscriptions(
%      "Nr", [{"Bpu", bpu},
%	     {"Mme", mme},
%	     {"NrFunction", nrFunction},
%	     {"Sa", sa},
%	     {"Sau", sau},
%	     {"Slice", slice},
%	     {"Tp", tp}]),
    comsaI:register_subscriptions(
	"NrFunction", [{"NrFunction", nrFunction},
		       {"Mme", mme},
		       {"Domain", domain},
		       {"Vue", vue},
		       {"Vsa", vsa},
		       {"Vpp", vpp},
		       {"Sa", sa},
		       {"Bpu", bpu},
		       {"Tp", tp},
		       {"RadioConfig", radioConfig},
		       {"DrbPdcp", drbPdcp},
		       {"SrbPdcp", srbPdcp},
		       {"DrbLogicalChannel", drbLogicalChannel},
		       {"SrbLogicalChannel", srbLogicalChannel},
		       {"Mac", mac},
		       {"Phys", phys}]),
    ok.

instPhParallel_post_init() ->
    ok.

children() ->
    Children = 
	[{nr, {nr, start, []}, permanent, 1000, worker, [nr]},
	 {nr_emp, {nr_emp, start, []}, permanent, 1000, worker, [nr_emp]}],
    case clhI:mp_role() of
	core ->
	    {ok, Children};
	_ ->
	    {ok, []}
    end.    

activate() ->
    ok.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

init(_) ->
%v1->v2
%[mnesia:subscribe({table, Table, detailed})||
%	Table <- [bpu, mme, nrFunction, sa, sau, slice, tp]],
    {ok, #{}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

%v1->v2
handle_info(_, State) ->
    {noreply, State}.
%handle_info({mnesia_table_event, _Event}, State) ->
    %%% Find out what has changed and make a REST call 
    %%% this is all done in the nr_emp- process/module
    %{noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.


%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%v1->v2
%table("Bpu") -> bpu;
%table("Mme") -> mme;
%table("NrFunction") -> nrFunction;
%table("Sa") -> sa;
%table("Sau") -> sau;
%table("Slice") -> slice;
%table("Tp") -> tp.

table("NrFunction") -> nrFunction;
table("Mme") -> mme;
table("Domain") -> domain;
table("Vue") -> vue;
table("Vsa") -> vsa;
table("Vpp") -> vpp;
table("Sa") -> sa;
table("Bpu") -> bpu;
table("Tp") -> tp;
table("RadioConfig") -> radioConfig;
table("DrbPdcp") -> drbPdcp;
table("SrbPdcp") -> srbPdcp;
table("DrbLogicalChannel") -> drbLogicalChannel;
table("SrbLogicalChannel") -> srbLogicalChannel;
table("Mac") -> mac;
table("Phys") -> phys.


%v1->v2
%types(bpu) -> ?bpu_types;    
%types(mme) -> ?mme_types;
%types(nrFunction) -> ?nrFunction_types;
%types(sa) -> ?sa_types;
%types(sau) -> ?sau_types;
%types(slice) -> ?slice_types;
%types(tp) -> ?tp_types.

types(nrFunction) -> ?nrFunction_types;
types(mme) -> ?mme_types;
types(domain) -> ?domain_types;
types(vue) -> ?vue_types;
types(vsa) -> ?vsa_types;
types(vpp) -> ?vpp_types;
types(sa) -> ?sa_types;
types(bpu) -> ?bpu_types;
types(tp) -> ?tp_types;
types(radioConfig) -> ?radioConfig_types;
types(drbPdcp) -> ?drbPdcp_types;
types(srbPdcp) -> ?srbPdcp_types;
types(drbLogicalChannel) -> ?drbLogicalChannel_types;
types(srbLogicalChannel) -> ?srbLogicalChannel_types;
types(mac) -> ?mac_types;
types(phys) -> ?phys_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
