%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaImm.erl %
%%% @author eolaand
%%% @version /main/R11A/2
%%%
%%% @doc == COMSA IMM OI ==
%%% This module is the Object Implementer for NodeSupport  
%%% for certain managed elements
%%%
-module(comsaImm).
-vsn('/main/R11A/2').
-author(eolaand).
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
%%% R11A/1     20171018   eolaand     Create module
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%----------------------------------------------------------------------
%% API functions
%%----------------------------------------------------------------------
-export([get_node_support/0,
	 set_node_support_release/2]).

%%----------------------------------------------------------------------
%% Upgrade
%%----------------------------------------------------------------------
-export([wait_for_upgrade_transform/0]).

%%----------------------------------------------------------------------
%% Life-cycle
%%----------------------------------------------------------------------
-export([
         initialize_ns_oi/0,
         finalize_ns_oi/1
        ]).

%%----------------------------------------------------------------------
%% OI callback functions
%%----------------------------------------------------------------------
-export([
	 ccb_object_create_2/5,
	 ccb_object_delete/3,
	 ccb_object_modify_2/4,
	 ccb_completed/2,
	 ccb_apply/2,
	 ccb_abort/2,
	 rt_attr_update/3
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("safs_imm_ct_p.hrl").
-include("gmf.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SAF_VSN, #safsVersion{releaseCode  = $A,
			      majorVersion = 2,
			      minorVersion = 11}).

-define(COMSA_OI_NAME, <<"ComsaOI">>).
-define(NODE_SUPPORT, <<"NodeSupport">>).
-define(NODE_SUPPORT_DN, <<"nodeSupportId=1">>).
-define(ERR_EXIST, sa_ais_err_exist).
-define(REL_ATTR_VALS(Rel), 
	#safsImmAttrValues_2{attrName = release,
			     attrValueType = sa_imm_attr_sastringt,
			     attrValuesNumber = 1,
			     attrValues = 
				 [#safsImmAttrValue{sastring = Rel}]}).

%% {ldap,
%%      [<<"nodeSupportId=1">>],
%%      config,
%%      [{supportFunction,[1]},{release,[<<"R11V21">>]}],
%%      'NodeSupport',undefined,undefined,[],
%%      [{'RcsImmAttrEcimDn',sa_imm_attr_sastringt,
%%           [<<"ManagedElement=1,NodeSupport=1">>]},
%%       {'RcsImmAttrObjId',sa_imm_attr_sauint32t,"$"}]},

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Called by comsaServer to get and set NodeSupport
%%% @end
%%% ----------------------------------------------------------
get_node_support() ->
    H = initialize_om_ah(),
    Res = get_object(H, ?NODE_SUPPORT_DN),
    finalize_om_ah(H),
    Res.


set_node_support_release(OiH, Release) ->
    AttrMods = build_attr_mods([?REL_ATTR_VALS(to_binary(Release))]),
    safs_imm_oi:rt_object_update_2(OiH, ?NODE_SUPPORT_DN, AttrMods).

%%% ----------------------------------------------------------
%%% @doc Called by comsaServer during upgrade 
%%% @end
%%% ----------------------------------------------------------
wait_for_upgrade_transform() ->
    gmfImmUgIcti:waitForDataConversion(?REQUEST_ID_COMSA_1).

%%% ----------------------------------------------------------
%%% @doc Called by comsaServer to initialize OI
%%% @end
%%% ----------------------------------------------------------
initialize_ns_oi() ->
    {ok, OiHandle, _Vsn} = safs_imm_oi:initialize(?MODULE, ?SAF_VSN),
    ok = safs_imm_oi:implementer_set(OiHandle, ?COMSA_OI_NAME),
    case safs_imm_oi:class_implementer_set(OiHandle, ?NODE_SUPPORT) of
	Res when Res =:= ok; Res =:= {error, ?ERR_EXIST} ->
	    ok;
	Error ->
	    safs_imm_oi:finalize(OiHandle),
	    throw({"Failed to set class implementer", Error})
    end,
    OiHandle.


finalize_ns_oi(undefined) ->
    ok;

finalize_ns_oi(OiH) ->
    safs_imm_oi:implementer_clear(OiH),
    safs_imm_oi:finalize(OiH).

%%====================================================================
%% Callback functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: ccb_object_create_2/5
%% Description:
%%--------------------------------------------------------------------
ccb_object_create_2(_OiHandle, _CcbId, _ClassName, _ParentName, _Attr) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_object_delete/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_delete(_OiHandle, _CcbId, _ObjectName) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_object_modify_2/4
%% Description:
%%--------------------------------------------------------------------
ccb_object_modify_2(_OiHandle, _CcbId, _DN, _AttrMods) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_completed/2
%% Description:
%%--------------------------------------------------------------------
ccb_completed(_OiHandle, _CcbId) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_apply/2
%% Description:
%%--------------------------------------------------------------------
ccb_apply(_OiHandle, _CcbId) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_abort/2
%% Description:
%%--------------------------------------------------------------------
ccb_abort(_OiHandle, _CcbId) ->
    ok.

%%--------------------------------------------------------------------
%% Function: rt_attr_update/3
%% Description:
%%--------------------------------------------------------------------
rt_attr_update(_OiHandle, _ObjectName, _AttributeNames) ->
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
get_object({_OH, AH}, ObjDN) ->
    get_object(AH, ObjDN);

get_object(AH, ObjDN) ->
    safs_imm_om:accessor_get_2(AH, ObjDN, []).


initialize_om_ah() ->
    OH = initialize_om(),
    AH = initialize_ah(OH),
    {OH, AH}.


finalize_om_ah({OmHandle, AH}) ->
    finalize_ah(AH),
    finalize_om(OmHandle).


initialize_om() ->
    {ok, OmHandle, _} = safs_imm_om:initialize(undefined, ?SAF_VSN),
    OmHandle.


finalize_om(OmHandle) ->
    ok = safs_imm_om:finalize(OmHandle).


initialize_ah(OH) ->
    {ok, AH} = safs_imm_om:accessor_initialize(OH),
    AH.


finalize_ah({_OmHandle, AH}) ->
    ok = finalize_ah(AH);

finalize_ah(AH) ->
    ok = safs_imm_om:accessor_finalize(AH).


build_attr_mods(AttrVals) ->
    [#safsImmAttrModification_2{modType = sa_imm_attr_values_replace,
				modAttr = AttrVal} || AttrVal <- AttrVals].


to_binary(L) when is_list(L) ->
    iolist_to_binary([L]);

to_binary(B) when is_binary(B) ->
    B.


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
