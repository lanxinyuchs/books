%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmOmI.erl %
%%% Author:
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfImmOmI).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R10A/R12A/1').
-author('etxpeno').
-shaid('61e59cc718cbab48c0d829fe6d01fdbd5d1cbdbb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-06-14 uabesvi     Created
%%% R2A/15     2013-12-06 etxberb     Added prepare_imm/2.
%%% R3A/1      2014-10-09 etxberb     Changed integer_to_list to
%%%                                   sysUtil:term_to_string.
%%% R3A/2      2015-01-28 etxpeno     Changed ccbflags in ccb_initialize
%%% R3A/3      2015-02-16 etxpeno     * Change ?CCBFLAGS
%%%                                   * Improve get_error_strings_imm/2
%%% R3A/4      2015-02-19 etxpeno     * Change ?CCBFLAGS again
%%% R3A/5      2015-03-13 etxpeno     * Change ?CCBFLAGS to ccb_registered_oi
%%% R4A/1      2015-05-12 erarafo     AVCs now handled by SAF IMM and NTF
%%% R5A/2      2016-04-04 uabesvi     HU68290
%%% R6A/1      2016-04-27 etxpeno     add catch in finish_imm/2
%%% R6A/2      2016-06-17 etxpeno     struct as attribute
%%% R7A/1      2016-11-01 etxpeno     add search_class_initialize_s2/5
%%% R8A/1      2016-12-22 etxpeno     use gmfSALib:imm_to_mim/1 instead of
%%%                                   gmfTrService:imm_to_mim/1
%%% R10A/1     2017-04-21 etxpeno     Improve admin owner handling
%%% R12A/1     2017-12-04 etxpeno     Add retries in admin_owner_set when locked
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%% IMM Global Registration
-export([activate/0]).

%% IMM Global callbacks
-export([init_imm/1,
	 commit_imm/2,
	 get_imm_funs/2,
	 completed_imm/2,
	 prepare_imm/2,
	 apply_imm/2,
	 abort_imm/2,
	 finish_imm/2,
	 error_strings_imm/2, % Obsolete
	 get_error_strings_imm/2,
	 set_error_string_imm/2]).

-export([initialize/0,
	 finalize/1,
	 admin_owner_initialize/3,
	 admin_owner_finalize/1,
	 admin_owner_set/3,
	 admin_owner_release/3,
	 ccb_initialize/2,
	 ccb_object_create_2/4,
	 ccb_object_create_s2/5,
	 ccb_object_delete/2,
	 ccb_object_modify_2/3,
	 ccb_object_read/3,
	 ccb_object_modify_extra_attrs_s2/3,
	 ccb_apply/1,
	 ccb_finalize/1,
	 accessor_initialize/3,
	 accessor_get_2/3,
	 accessor_finalize/1
	]).


-export([search_initialize_2/2,
	 search_initialize_2/3,
	 search_initialize_2/4,
	 search_class_initialize_s2/5,
	 search_next_2/1,
	 search_next_n_s2/2,
	 search_finalize/1]).

-export([admin_operation_invoke_o2/6]).
-export([admin_operation_invoke_async_2/6]).
-export([admin_operation_invoke_callback_o2/4]).


-export([coc_2/4]).
-export([coc_s2/5]).
-export([cod/2]).
-export([com_2/3]).
-export([ca/1]).
-export([cf/1]).
-export([aoi/3]).
-export([aof/1]).
-export([aos/3]).
-export([aor/3]).
-export([comea_s2/3]).
-export([cses/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("gmf.hrl").
-include("om.hrl").


-define(GMFLOG, gmfLog:imm).

-define(CCBFLAGS, [ccb_registered_oi]).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%% INTERNAL CCB MODE HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%

get_ccb_mode(Cxp, Rev) ->
    Key = string:join(["ccbmode", Cxp, Rev], "_"),
    gmfDb:lookup_value(gmfData, Key, bundle).

get_ccb_handle(_Cxp, _Rev, _Tid, single, OH) ->
    ccb_initialize(OH, get_ccbflags());
get_ccb_handle(Cxp, Rev, Tid, bundle, OH) ->
    CcbKey = string:join(["ccb", Cxp, Rev, sysUtil:term_to_string(Tid)], "_"),
    case gmfDb:lookup_value(gmfData, CcbKey, undefined) of
	undefined ->
	    ci(CcbKey, ccb_initialize(OH, get_ccbflags()));
	CcbHandle ->
	    {ok, CcbHandle}
    end.

finalize(single, CcbHandle) -> ccb_finalize(CcbHandle);
finalize(_,      _)         -> ignore.


ccb_apply(Res, _, bundle)           -> Res;
ccb_apply({error, _} = Error, _, _) -> imm_error(Error);
ccb_apply(_, CcbHandle, _)          -> ccb_apply(CcbHandle).

ci(CcbKey, {ok, CcbHandle} = Res) ->
    ets:insert(gmfData, {CcbKey, CcbHandle}),
    Res;
ci(_, Res) ->
    imm_error(Res).

%%%%%%%%%%%%%%%%%%%%%% INTERNAL CCB MODE HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Probably we can build funs for all these and have one wrapper function
%% Duplicate code: Let's fix it in the next step
coc_2({tid, TransId}, Class, Parent, Values) ->
    CcbId = get_key("gmf", "ccb", "id",   TransId),
    CcbHandle = gmfDb:lookup_value(gmfData, CcbId, undefined),
    ccb_object_create_2(CcbHandle, Class, Parent, Values);

coc_2({{Cxp, Rev},Tid} = Key, Class, Parent, Values) ->
    OH        = get_aoh(Key),
    CcbMode   = get_ccb_mode(Cxp, Rev),
    CcbHandle = get_ccb_handle(Cxp, Rev, Tid, CcbMode, OH),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev},
	     [{class, Class},
	      {parent, Parent},
	      {values, Values},
	      {tid, Tid}]}),
    coc_2(CcbHandle, Class, Parent, Values, CcbMode).

coc_2({error, _} = Error, _, _, _, _) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [Error]}),
    imm_error(Error);
coc_2({ok, CcbHandle}, Class, Parent, Values, CcbMode) ->
    Res = ccb_object_create_2(CcbHandle, Class, Parent, Values),
    Res2 = ccb_apply(Res, CcbHandle, CcbMode),
    finalize(CcbMode, CcbHandle),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [{ccbhandle, CcbHandle},
	      {res, Res},
	      {class, Class},
	      {parent, Parent},
	      {values, Values}]}),
    Res2.

coc_s2({tid, TransId}, Class, Parent, Values, ExtraValues) ->
    CcbId = get_key("gmf", "ccb", "id",   TransId),
    CcbHandle = gmfDb:lookup_value(gmfData, CcbId, undefined),
    ccb_object_create_s2(CcbHandle, Class, Parent, Values, ExtraValues).

com_2({tid, TransId}, ObjectName, Values) ->
    CcbId     = get_key("gmf", "ccb", "id",   TransId),
    CcbHandle = gmfDb:lookup_value(gmfData, CcbId, undefined),
    ccb_object_modify_2(CcbHandle, ObjectName, Values);

com_2({{Cxp, Rev},Tid} = Key, ObjectName, Values) ->
    OH        = get_aoh(Key),
    CcbMode   = get_ccb_mode(Cxp, Rev),
    CcbHandle = get_ccb_handle(Cxp, Rev, Tid, CcbMode, OH),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev},
	     [{objectname, ObjectName},
	      {values, Values},
	      {tid, Tid}]}),
    com_2(CcbHandle, ObjectName, Values, CcbMode).

com_2({error, _} = Error, _, _, _) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [Error]}),
    imm_error(Error);
com_2({ok, CcbHandle}, ObjectName, Values, CcbMode) ->
    Res = ccb_object_modify_2(CcbHandle, ObjectName, Values),
    Res2 = ccb_apply(Res, CcbHandle, CcbMode),
    finalize(CcbMode, CcbHandle),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [{ccbhandle, CcbHandle},
	      {res, Res},
	      {objectname, ObjectName},
	      {values, Values}]}),
    Res2.

cod({tid, TransId}, ObjectName) ->
    CcbId     = get_key("gmf", "ccb", "id",   TransId),
    CcbHandle = gmfDb:lookup_value(gmfData, CcbId, undefined),
    ccb_object_delete(CcbHandle, ObjectName);

cod({{Cxp, Rev},Tid} = Key, ObjectName) ->
    OH        = get_aoh(Key),
    CcbMode   = get_ccb_mode(Cxp, Rev),
    CcbHandle = get_ccb_handle(Cxp, Rev, Tid, CcbMode, OH),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev},
	     [{objectname, ObjectName},
	      {tid, Tid}]}),
    cod(CcbHandle, ObjectName, CcbMode).

cod({error, _} = Error, _, _) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [Error]}),
    imm_error(Error);
cod({ok, CcbHandle}, ObjectName, CcbMode) ->
    Res = ccb_object_delete(CcbHandle, ObjectName),
    Res2 = ccb_apply(Res, CcbHandle, CcbMode),
    finalize(CcbMode, CcbHandle),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [{ccbhandle, CcbHandle},
	      {res, Res},
	      {objectname, ObjectName}]}),
    Res2.

ccb_object_read({tid, TransId}, ObjectName0, AttrNamesFromCom) ->
    CcbId     = get_key("gmf", "ccb", "id",   TransId),
    CcbHandle = gmfDb:lookup_value(gmfData, CcbId, undefined),
    ccb_object_read(CcbHandle, ObjectName0, AttrNamesFromCom);
ccb_object_read(CcbHandle, ObjectName0, AttrNamesFromCom) ->
    ObjectName = bin(ObjectName0),
    AttrNames = [?L2A(A) || A <- AttrNamesFromCom], %% Translate between IMM/COMTE
    Fun = fun() ->
		  safs_imm_om:ccb_object_read(CcbHandle, ObjectName, AttrNames)
	  end,
    case run(Fun) of
	{ok, Values} = Ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {objectname, ObjectName},
		      {attrnames, AttrNames},
		      {values, Values}]}),
	    Ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {objectname, ObjectName},
		      {attrnames, AttrNames},
		      Err]}),
	    imm_error(Err)
    end.

ccb_object_modify_extra_attrs_s2(CcbId, ObjectName, AttrModsFromCom) ->
    %% Translate between IMM/COMTE
    AttrMods = [#safsImmAttrModification_2{modType = sa_imm_attr_values_replace,
					   modAttr = AttrVal}
		|| AttrVal <- com2imm(AttrModsFromCom)],
    Fun = fun() ->
		  safs_imm_om:ccb_object_modify_extra_attrs_s2(CcbId,
							       bin(ObjectName),
							       AttrMods)
	  end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbId},
		      {objectname, ObjectName},
		      {attrmods, AttrMods}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbId},
		      {objectname, ObjectName},
		      {attrmods, AttrMods},
		      Err]}),
	    imm_error(Err)
    end.

comea_s2({tid, TransId}, ObjectName, Values) ->
    CcbId     = get_key("gmf", "ccb", "id", TransId),
    CcbHandle = gmfDb:lookup_value(gmfData, CcbId, undefined),
    ccb_object_modify_extra_attrs_s2(CcbHandle, ObjectName, Values).

ca({{Cxp, Rev}, Tid} = Key) ->
    OH        = get_aoh(Key),
    CcbHandle = get_ccb_handle(Cxp, Rev, Tid, bundle, OH),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev},
	     [{tid, Tid}]}),
    ca(CcbHandle);
ca({error, _} = Error) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [Error]}),
    imm_error(Error);
ca({ok, CcbHandle}) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [{ccbhandle, CcbHandle}]}),
    ccb_apply(CcbHandle).


cf({{Cxp, Rev}, Tid} = Key) ->
    OH        = get_aoh(Key),
    CcbHandle = get_ccb_handle(Cxp, Rev, Tid, bundle, OH),
    %% Need to build a better interface for this.
    catch ets:delete(gmfData, get_key("ccb", Cxp, Rev, Tid)),
    catch ets:delete(gmfData, get_key("accessor", Cxp, Rev, Tid)),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev},
	     [{tid, Tid},
	      {ccbhandle, CcbHandle}]}),
    cf(CcbHandle);
cf({error, _} = Error) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [Error]}),
    imm_error(Error);
cf({ok, CcbHandle}) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [{ccbhandle, CcbHandle}]}),
    ccb_finalize(CcbHandle).

cses({tid, TransId}, Reason) ->
    GmfCcbId = get_key("gmf", "ccb", "id", TransId),
    CcbHandle = gmfDb:lookup_value(gmfData, GmfCcbId, undefined),
    case safs_imm_om:get_ccbid_from_handle(CcbHandle) of
	{ok, CcbId} ->
	    safs_imm_om:ccb_set_error_string(CcbId, Reason);
	_ ->
	    ok
    end.

%%===========================================================================
%%   ImmInterface functions
%%
%%===========================================================================


%%---------------------------------------------------------------------------
%% START - 4.3 Library Life Cycle
%% --------------------------------------------------------------------------
initialize() ->
    %%  callbacks :: 'undefined' | #safe_imm_callbacks{},
    %%  version :: 'undefined' | #safe_version{}}).

    %%    Callbacks = #safe_imm_callbacks{
    %%      sa_imm_om_admin_operation_invoke_callback = false
    %%      }, %% sa_imm_om_admin_operation_invoke_callback is boolean
    %%    Version = #safe_version,{
    %%                 release_code,
    %%                 major_version,
    %%                 minor_version}
    Callbacks = ?MODULE,
    %%Version   = undefined,
    Version   = #safsVersion{releaseCode  = $A,
			     majorVersion = 2,
			     minorVersion = 11},

    Fun = fun() -> safs_imm_om:initialize(Callbacks, Version) end,
    case run(Fun) of
	{ok, ImmHandle, _}  ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle}]}),
	    {ok, ImmHandle};
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [Err]}),
	    imm_error(Err)
    end.

finalize(ImmHandle) ->
    Fun = fun() -> safs_imm_om:finalize(ImmHandle) end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      Err]}),
	    imm_error(Err)
    end.
%%---------------------------------------------------------------------------
%% END - 4.3 Library Life Cycle
%% --------------------------------------------------------------------------

%%---------------------------------------------------------------------------
%% START - 4.5 Object Search
%% --------------------------------------------------------------------------
search_initialize_2(ImmHandle, RootObjDN) ->
    search_initialize_2(ImmHandle, RootObjDN, 16#0100).
search_initialize_2(ImmHandle, RootObjDN0, Option) ->
    search_initialize_2(ImmHandle, RootObjDN0, sa_imm_subtree, Option).
search_initialize_2(ImmHandle, RootObjDN0, Scope, Option) ->
    RootObjDN = bin(RootObjDN0),
    case safs_imm_om:search_initialize_2(ImmHandle,
					 RootObjDN,
					 Scope,
					 Option,
					 undefined,
					 []) of
	{ok, SearchHandle} = Handle ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {root, RootObjDN},
		      {option, Option},
		      {searchhandle, SearchHandle}]}),
	    Handle;
	Error ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {root, RootObjDN},
		      {option, Option},
		      Error]}),
	    imm_error(Error)
    end.

search_class_initialize_s2(ImmHandle, RootObjDN0, Scope, Option, ClassNames) ->
    RootObjDN = bin(RootObjDN0),
    case safs_imm_om:search_class_initialize_s2(ImmHandle,
						RootObjDN,
						Scope,
						Option,
						ClassNames,
						[]) of
	{ok, SearchHandle} = Handle ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {root, RootObjDN},
		      {scope, Scope},
		      {option, Option},
		      {classnames, ClassNames},
		      {searchhandle, SearchHandle}]}),
	    Handle;
	Error ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {root, RootObjDN},
		      {scope, Scope},
		      {option, Option},
		      {classnames, ClassNames},
		      Error]}),
	    imm_error(Error)
    end.

bin(Bin) when is_list(Bin) ->
    list_to_binary(Bin);
bin(Bin) ->
    Bin.


search_finalize(Handle) ->
    Res = safs_imm_om:search_finalize(Handle),
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     [{searchhandle, Handle},
	      {res, Res}]}),
    Res.

search_next_2(Handle) ->
    case safs_imm_om:search_next_2(Handle) of
	{ok, Obj, AVs} ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{searchhandle, Handle},
		      {obj, Obj},
		      {avs, AVs}]}),
	    {ok, binary_to_list(Obj), AVs};
	Error ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{searchhandle, Handle},
		      Error]}),
	    imm_error(Error)
    end.

search_next_n_s2(Handle, N) ->
    case safs_imm_om:search_next_n_s2(Handle, N) of
	{ok, Objs} ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{searchhandle, Handle},
		      {n, n},
		      {objects, Objs}]}),
	    {ok, Objs};
	Error ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{searchhandle, Handle},
		      Error]}),
	    imm_error(Error)
    end.

%% snobj(AVs) ->
%%     [{N, snobj_r(Vs)} || {N, Vs} <- AVs].
%% snobj_r(Vs) ->
%%     [sno_r(V) || V <- Vs].
%% sno_r(V) when is_binary(V) ->
%%     binary_to_list(V);
%% sno_r(V) ->
%%     V.

%%---------------------------------------------------------------------------
%% END - 4.5 Object Search
%% --------------------------------------------------------------------------

%%---------------------------------------------------------------------------
%% START - 4.7 Object Administration Ownership
%% --------------------------------------------------------------------------
aoi(ImmH, {{Cxp, Rev}, Tid}, ReleaseOwnerShip) ->
    AoiKey = get_key("aoh", Cxp, Rev, Tid),
    case gmfDb:lookup_value(gmfData, AoiKey, undefined) of
	undefined ->
	    aoi(AoiKey, admin_owner_initialize(ImmH, AoiKey, ReleaseOwnerShip));
	AoHandle ->
	    {ok, AoHandle}
    end.
aoi(AoiKey, {ok, AoHandle} = Res) ->
    ets:insert(gmfData, {AoiKey, AoHandle}),
    Res;
aoi(_, Res) ->
    imm_error(Res).

aof({{Cxp, Rev}, Tid} = Key) ->
    Aoh = get_aoh(Key),
    catch ets:delete(gmfData, get_key("aoh", Cxp, Rev, Tid)),
    aof(Aoh);
aof(undefined) ->
    ok;
aof(Aoh) ->
    admin_owner_finalize(Aoh).

aos(Key, ObjectNames, Scope) ->
    Aoh = get_aoh(Key),
    aos_int(Aoh, ObjectNames, Scope).
aos_int(undefined, _, _) ->
    imm_error({error, sa_ais_bad_handle});
aos_int(Aoh, ObjectNames, Scope) ->
    admin_owner_set(Aoh, ObjectNames, Scope).

aor(Key, ObjectNames, Scope) ->
    Aoh = get_aoh(Key),
    aor_int(Aoh, ObjectNames, Scope).
aor_int(undefined, _, _) ->
    imm_error({error, sa_ais_bad_handle});
aor_int(Aoh, ObjectNames, Scope) ->
    admin_owner_release(Aoh, ObjectNames, Scope).

get_aoh({tid, TransId}) ->
    AoName    = get_key("gmf", "ao",  "name", TransId),
    gmfDb:lookup_value(gmfData, AoName, undefined);
get_aoh({{Cxp, Rev}, Tid}) ->
    AoiKey = get_key("aoh", Cxp, Rev, Tid),
    gmfDb:lookup_value(gmfData, AoiKey, undefined).

admin_owner_initialize(ImmHandle, AdminOwnerName, ReleaseOwnership) ->
    %% #safs_imm_om_admin_owner_initialize{
    %%   handle :: non_neg_integer(),
    %%   admin_owner_name :: binary(),
    %%   release_ownership_on_finalize :: boolean()}).
    Fun = fun() -> safs_imm_om:admin_owner_initialize(ImmHandle,
						      AdminOwnerName,
						      ReleaseOwnership)
	  end,
    case run(Fun) of
	{ok, OwnerHandle} ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {releaseownership, ReleaseOwnership},
		      {ownerhandle, OwnerHandle}]}),
	    {ok, OwnerHandle};
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {releaseownership, ReleaseOwnership},
		      Err]}),
	    imm_error(Err)
    end.

admin_owner_finalize(OwnerHandle) ->
    %% #safs_imm_om_admin_owner_finalize{
    %%    owner_handle :: non_neg_integer()}
    Fun = fun() -> safs_imm_om:admin_owner_finalize(OwnerHandle) end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle},
		      Err]}),
	    imm_error(Err)
    end.


admin_owner_set(OwnerHandle, ObjectNames, Scope) ->
    %% What objects/level should be owned
    %% #safs_imm_om_admin_owner_set{
    %%   owner_handle :: non_neg_integer(),
    %%   object_names = [] :: [binary()],
    %%   scope :: safe_imm_scope()}).
    %% safe_imm_scope() :: sa_imm_one | sa_imm_sublevel | sa_imm_subtree
    Fun = fun() ->
		  safs_imm_om:admin_owner_set(OwnerHandle,
					      lists:map(fun to_binary/1, ObjectNames),
					      gmfSALib:convert_ao_scope(Scope))
	  end,
    RetryList = [sa_ais_err_try_again, sa_ais_err_exist],
    case run(Fun, RetryList) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle},
		      {scope, Scope},
		      {objectnames, ObjectNames}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle},
		      {scope, Scope},
		      {objectnames, ObjectNames},
		      Err]}),
	    imm_error(Err)
    end.

admin_owner_release(OwnerHandle, ObjectNames, Scope) ->
    %% What objects/level should be owned
    %% #safs_imm_om_admin_owner_release{
    %%   owner_handle :: non_neg_integer(),
    %%   object_names = [] :: [binary()],
    %%   scope :: safe_imm_scope()}).
    %% safe_imm_scope() :: sa_imm_one | sa_imm_sublevel | sa_imm_subtree
    Fun = fun() ->
		  safs_imm_om:admin_owner_release(OwnerHandle,
						  lists:map(fun to_binary/1, ObjectNames),
						  gmfSALib:convert_ao_scope(Scope))
	  end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle},
		      {scope, Scope},
		      {objectnames, ObjectNames}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle},
		      {scope, Scope},
		      {objectnames, ObjectNames},
		      Err]}),
	    imm_error(Err)
    end.

%%---------------------------------------------------------------------------
%% END - 4.7 Object Administration Ownership
%% --------------------------------------------------------------------------


%%---------------------------------------------------------------------------
%% START - 4.8 Configuration Changes
%% --------------------------------------------------------------------------
ccb_initialize(OwnerHandle, CcbFlags) ->
    %%  #safs_imm_om_ccb_initialize{
    %%    owner_handle :: non_neg_integer(),
    %%    ccb_flags :: non_neg_integer()}.
    %% From spec, CCB Flags from spec
    %% #define SA_IMM_CCB_ALLOW_ABSENT_VALIDATORS 0x00000001
    %% #define SA_IMM_CCB_ALLOW_ABSENT_APPLIERS 0x00000002
    %% typedef SaUint64T SaImmCcbFlagsT_3;

    %% SA_IMM_CCB_ALLOW_ABSENT_VALIDATORS-if this flag is specified, the CCB can
    %% hold changes for objects for which CCB validators have been specified
    %% but are currently not registered. If this flag is not set, all specified
    %% validators must be registered for all objects that are
    %% changed in the CCB.
    %% SA_IMM_CCB_ALLOW_ABSENT_APPLIERS-if this flag is specified, the CCB can
    %% hold changes for objects for which CCB appliers have been specified
    %% but are currently not registered. If this flag is not set, all specified
    %% appliers must be registered for all objects that are changed in the CCB.

    Fun = fun() -> safs_imm_om:ccb_initialize(OwnerHandle, CcbFlags) end,
    case run(Fun) of
	{ok, CcbH} = Ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle},
		      {ccbflags, CcbFlags},
		      {ccbhandle, CcbH}]}),
	    Ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ownerhandle, OwnerHandle},
		      {ccbflags, CcbFlags},
		      Err]}),
	    imm_error(Err)
    end.

ccb_object_create_2(CcbHandle, ClassName, ParentName, AttrValuesFromCom) ->
    %% #safs_imm_om_ccb_object_create_2{
    %%   ccb_id :: non_neg_integer(),
    %%   class_name :: binary(),
    %%   parent_name :: 'undefined' | binary(),
    %%   attr_values = [] :: [#safe_imm_attr_values_2{}]}).

    %% #safe_imm_attr_values_2{
    %%   attr_name :: binary(),
    %%   attr_value_type :: safe_imm_value_type(),
    %%   attr_values_number :: non_neg_integer(),
    %%   attr_values = [] :: [#safe_imm_attr_value{}]}).

    %% #safe_imm_attr_value{
    %%   saint32 :: 'undefined' | integer(),
    %%   sauint32 :: 'undefined' | non_neg_integer(),
    %%   saint64 :: 'undefined' | integer(),
    %%   sauint64 :: 'undefined' | integer(),
    %%   satime :: 'undefined' | non_neg_integer(),
    %%   saname :: 'undefined' | binary(),
    %%   safloat :: 'undefined' | float(),
    %%   sadouble :: 'undefined' | float(),
    %%   sastring :: 'undefined' | binary(),
    %%   saany :: 'undefined' | binary()}).

    AttrValues = com2imm(AttrValuesFromCom), %% Translate between IMM/COMTE
    Fun = fun() ->
		  safs_imm_om:ccb_object_create_2(CcbHandle,
						  bin(ClassName),
						  bin(ParentName),
						  AttrValues)
	  end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {class, ClassName},
		      {parent, ParentName},
		      {attrvals, AttrValues}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {class, ClassName},
		      {parent, ParentName},
		      Err]}),
	    imm_error(Err)
    end.

ccb_object_create_s2(CcbHandle, ClassName, ParentName, AttrValuesFromCom,
		     ExtraAttrValuesFromCom) ->

    %% Translate between IMM/COMTE
    AttrValues = com2imm(AttrValuesFromCom),
    ExtraAttrValues = com2imm(ExtraAttrValuesFromCom),
    Fun = fun() ->
		  safs_imm_om:ccb_object_create_s2(CcbHandle,
						   bin(ClassName),
						   bin(ParentName),
						   AttrValues,
						   ExtraAttrValues)
	  end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {class, ClassName},
		      {parent, ParentName},
		      {attrvals, AttrValues},
		      {extraattrvals, ExtraAttrValues}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {class, ClassName},
		      {parent, ParentName},
		      Err]}),
	    imm_error(Err)
    end.

ccb_object_delete(CcbHandle, ObjectName) ->
    %% #safs_imm_om_ccb_object_delete{
    %%    ccb_id :: non_neg_integer(),
    %%    object_name :: binary()}
    Fun = fun() -> safs_imm_om:ccb_object_delete(CcbHandle,
						 bin(ObjectName))
	  end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {objectname, ObjectName}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      {objectname, ObjectName},
		      Err]}),
	    imm_error(Err)
    end.

ccb_object_modify_2(CcbId, ObjectName, AttrModsFromCom) ->
    %% #safs_imm_om_ccb_object_modify_2{
    %%    ccb_id :: non_neg_integer(),
    %%    object_name :: binary(),
    %%    attr_mods = [] :: [#safe_imm_attr_modification_2{}]}).
    %%
    %% #safe_imm_attr_modification_2{
    %%    mod_type :: safe_imm_attr_modification_type(),
    %%    mod_attr :: #safe_imm_attr_values_2{}}).
    %%
    %% safe_imm_attr_modification_type() :: sa_imm_attr_values_add | sa_imm_attr_values_delete | sa_imm_attr_values_replace
    %% #safe_imm_attr_values_2{
    %%   attr_name :: binary(),
    %%   attr_value_type :: safe_imm_value_type(),
    %%   attr_values_number :: non_neg_integer(),
    %%   attr_values = [] :: [#safe_imm_attr_value{}]}).

    %% #safe_imm_attr_value{
    %%   saint32 :: 'undefined' | integer(),
    %%   sauint32 :: 'undefined' | non_neg_integer(),
    %%   saint64 :: 'undefined' | integer(),
    %%   sauint64 :: 'undefined' | integer(),
    %%   satime :: 'undefined' | non_neg_integer(),
    %%   saname :: 'undefined' | binary(),
    %%   safloat :: 'undefined' | float(),
    %%   sadouble :: 'undefined' | float(),
    %%   sastring :: 'undefined' | binary(),
    %%   saany :: 'undefined' | binary()}).

    %% HOW TO DECIDE modification type
    %% Hardcode it to add for the timebeing
    AttrMods = [#safsImmAttrModification_2{modType = sa_imm_attr_values_replace,
					   modAttr = AttrVal}
		|| AttrVal <- com2imm(AttrModsFromCom)], %% Translate between IMM/COMTE
    Fun = fun() ->
		  safs_imm_om:ccb_object_modify_2(CcbId,
						  bin(ObjectName),
						  AttrMods)
	  end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbId},
		      {objectname, ObjectName},
		      {attrmods, AttrMods}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbId},
		      {objectname, ObjectName},
		      {attrmods, AttrMods},
		      Err]}),
	    imm_error(Err)
    end.

ccb_apply(CcbHandle) ->
    %% #safs_imm_om_ccb_apply{
    %%    ccb_id :: non_neg_integer()}
    Fun = fun() -> safs_imm_om:ccb_apply(CcbHandle) end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      Err]}),
	    imm_error(Err)
    end.

ccb_finalize(CcbHandle) ->
    %% #safs_imm_om_ccb_finalize{
    %%    ccb_id :: non_neg_integer()}
    Fun = fun() -> safs_imm_om:ccb_finalize(CcbHandle) end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{ccbhandle, CcbHandle},
		      Err]}),
	    imm_error(Err)
    end.

%%---------------------------------------------------------------------------
%% END - 4.8 Configuration Changes
%% --------------------------------------------------------------------------

%%---------------------------------------------------------------------------
%% START - 4.8 Object Access
%% --------------------------------------------------------------------------
accessor_initialize(ImmHandle, {Cxp, Rev}, Tid) ->
    AccessorKey = get_key("accessor", Cxp, Rev, Tid),
    case gmfDb:lookup_value(gmfData, AccessorKey, undefined) of
	undefined      ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {cxprev, {Cxp, Rev}},
		      {tid, Tid}]}),
	    ai(ImmHandle);
	AccessorHandle ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {cxprev, {Cxp, Rev}},
		      {tid, Tid},
		      {accessorhandle, AccessorHandle}]}),
	    {ok, AccessorHandle}
    end.


ai(ImmHandle) ->
    %% #safs_imm_om_accessor_initialize{
    %%    handle :: non_neg_integer()}

    %% Integer typedef SaUint64T SaImmAccessorHandleT
    Fun = fun() -> safs_imm_om:accessor_initialize(ImmHandle) end,
    case run(Fun) of
	{ok, Handle} = Ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      {accessorhandle, Handle}]}),
	    Ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{immhandle, ImmHandle},
		      Err]}),
	    imm_error(Err)
    end.


accessor_get_2({tid, TransId}, ObjectName0, AttrNamesFromCom) ->
    AccName = get_key("gmf", "acc", "name", TransId),
    AccHandle = gmfDb:lookup_value(gmfData, AccName, undefined),
    accessor_get_2(AccHandle, ObjectName0, AttrNamesFromCom);

accessor_get_2(AccessorHandle, ObjectName0, AttrNamesFromCom) ->
    %% #safs_imm_om_accessor_get_2{
    %%    handle :: non_neg_integer(),
    %%    object_name :: binary(),
    %%    attribute_names = [] :: [binary()]})
    ObjectName = bin(ObjectName0),
    AttrNames = [?L2A(A) || A <- AttrNamesFromCom], %% Translate between IMM/COMTE

    Fun = fun() -> safs_imm_om:accessor_get_2(AccessorHandle, ObjectName,
					      AttrNames)
	  end,
    case run(Fun) of
	{ok, Values} = Ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{accessorhandle, AccessorHandle},
		      {objectname, ObjectName},
		      {attrnames, AttrNames},
		      {values, Values}]}),
	    Ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{accessorhandle, AccessorHandle},
		      {objectname, ObjectName},
		      {attrnames, AttrNames},
		      Err]}),
	    imm_error(Err)
    end.


accessor_finalize(AccessorHandle) ->
    %% #safs_imm_om_accessor_finalize,{
    %%    handle :: non_neg_integer()}

    Fun = fun() -> safs_imm_om:accessor_finalize(AccessorHandle) end,
    case run(Fun) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{accessorhandle, AccessorHandle}]}),
	    ok;
	{error, _} = Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{accessorhandle, AccessorHandle},
		      Err]}),
	    imm_error(Err)
    end.




%%% Adm operations <=> COM actions
admin_operation_invoke_o2(Key,
			  ObjectName,
			  ContinuationId,
			  OperationId,
			  ComParams,
			  Timeout) ->
    Aoh = get_aoh(Key),
    ImmParams = com2immadm(ComParams),
    case safs_imm_om:admin_operation_invoke_o2(Aoh,
					       bin(ObjectName),
					       ContinuationId,
					       OperationId,
					       ImmParams,
					       Timeout) of
	{ok, sa_ais_ok, Values} ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{key, Key},
		      {objectname, ObjectName},
		      {continuationid, ContinuationId},
		      {operationid, OperationId},
		      {comparams, ComParams},
		      {timeout, Timeout},
		      {values, Values}]}),
	    {ok, Values};
	Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     [{key, Key},
		      {objectname, ObjectName},
		      {continuationid, ContinuationId},
		      {operationid, OperationId},
		      {comparams, ComParams},
		      {timeout, Timeout},
		      Err]}),
	    imm_error({error, element(2, Err)})
    end.



admin_operation_invoke_async_2(Key,
			       InvId,
			       ObjectName,
			       ContinuationId,
			       OperationId,
			       ComParams) ->
    Aoh = get_aoh(Key),
    ImmParams = com2immadm(ComParams),
    case safs_imm_om:admin_operation_invoke_async_2(Aoh,
						    InvId,
						    bin(ObjectName),
						    ContinuationId,
						    OperationId,
						    ImmParams) of
	ok ->
	    ok;
	Res ->
	    imm_error({error, element(2, Res)})
    end.


admin_operation_invoke_callback_o2(InvId, OperRetVal, Err, Params) ->
    %% Don't lookup that adminOpPid table here
    gmfComteLib:action_result(InvId, aoic(OperRetVal), aoic(Err), Params).

aoic(ok)                -> ok;
aoic(sa_ais_ok)         -> ok;
aoic(V) when is_atom(V) -> imm_error({error, V});
aoic(V)                 -> imm_error({error, element(2, V)}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
com2imm(AttrValuesFromCom) ->
    [com2imm(Attr, {Type, Values}) ||
	{Attr, {Type, Values}} <- AttrValuesFromCom].

com2imm(Attr, {Type, Values}) ->
    SafeType = gmfSALib:get_safe_type(Type),
    AttrVals = [gmfSALib:get_imm_attr_value(SafeType, V) || V <- Values],
    #safsImmAttrValues_2{attrName         = bin(Attr),
			 attrValueType    = SafeType,
			 attrValuesNumber = length(Values),
			 attrValues       = AttrVals}.


com2immadm(ParamValuesFromCom) ->
    [com2immadm(Param, {Type, Value}) ||
	{Param, {Type, Value}} <- ParamValuesFromCom].
com2immadm(Param, {Type, Value}) ->
    SafeType = gmfSALib:get_safe_type(Type),
    ParamVal = gmfSALib:get_imm_attr_value(SafeType, Value),
    #safsImmAdminOperationParams_2{paramName   = bin(Param),
				   paramType   = SafeType,
				   paramBuffer = ParamVal}.


get_key(Tag, Cxp, Rev, Tid) ->
    string:join([Tag, Cxp, Rev, sysUtil:term_to_string(Tid)], "_").


to_binary(X) when is_list(X)   -> list_to_binary(X);
to_binary(X) when is_binary(X) -> X.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

activate() ->
    catch comsaI:register_imm_global_callback(?MODULE).

init_imm(TransId) ->
    AoName    = get_key("gmf", "ao",  "name", TransId),
    CcbId     = get_key("gmf", "ccb", "id",   TransId),
    AccName   = get_key("gmf", "acc", "name", TransId),
    case gmfDb:lookup_value(gmfData, "gmf_imm_handle", undefined) of
	undefined ->
	    [];
	ImmHandle ->
	    {ok, AoHandle}  = admin_owner_initialize(ImmHandle, AoName, true),
	    {ok, CcbHandle} = ccb_initialize(AoHandle, get_ccbflags()),
	    {ok, AccHandle} = ai(ImmHandle),
	    ets:insert(gmfData, [{AoName, AoHandle},
				 {CcbId, CcbHandle},
				 {AccName, AccHandle}]),
	    [{immhandle, ImmHandle},
	     {aohandle,  AoHandle},
	     {ccbhandle, CcbHandle},
	     {acchandle, AccHandle},
	     {names, [AoName, CcbId, AccName]}]
    end.

commit_imm(_TransId, []) ->
    ok;
commit_imm(_TransId, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    %%ccb_apply(CcbHandle).
    Res = send_completed(CcbHandle),
    send_apply(Res, CcbHandle).

send_completed(CcbHandle) ->
    safs_imm_om:send_completed(CcbHandle).

send_apply({ok, Funs}, CcbHandle) ->
    [Fun() || Fun <- Funs],
    safs_imm_om:send_apply(CcbHandle);
send_apply(Error, _) ->
    imm_error(Error).

get_imm_funs(_TransId, []) ->
    {ok, []};
get_imm_funs(_TransId, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    safs_imm_om:get_fun_list(CcbHandle).

completed_imm(_TransId, []) ->
    {ok, []};
completed_imm(_TransId, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    safs_imm_om:send_completed(CcbHandle).

prepare_imm(_Fun, []) ->
    ok;
prepare_imm(Fun, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    safs_imm_om:ccb_insert_fun(CcbHandle, Fun).

apply_imm(_TransId, []) ->
    ok;
apply_imm(_TransId, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    %% erarafo 2015-05-06
    %% COM no longer handles AVC notifications itself, so
    %% let SAF IMM and NTF do it instead.
    %% safs_imm_special_applier:filter_ccb(CcbHandle),
    safs_imm_om:send_apply(CcbHandle).

abort_imm(_TransId, []) ->
    ok;
abort_imm(_TransId, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    safs_imm_om:send_abort(CcbHandle).

finish_imm(_TransId, []) ->
    ok;
finish_imm(_TransId, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    ccb_finalize(CcbHandle),
    AoHandle = proplists:get_value(aohandle, ImmData),
    admin_owner_finalize(AoHandle),
    AccHandle = proplists:get_value(acchandle, ImmData),
    accessor_finalize(AccHandle),
    [catch ets:delete(gmfData, Key) ||
	Key <- proplists:get_value(names, ImmData)],
    ok.

%% This function should be removed as soon as comsaTransactionServer doesn't
%% use it anymore
error_strings_imm(TransId, ImmData) ->
    get_error_strings_imm(TransId, ImmData).

get_error_strings_imm(_TransId, []) ->
    [];
get_error_strings_imm(_TransId, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    case safs_imm_om:ccb_get_error_strings(CcbHandle) of
    	{ok, ErrorStrings} ->
    	    ErrorStrings;
    	{error, _} ->
    	    []
    end.

set_error_string_imm(_Reason, []) ->
    ok;
set_error_string_imm(Reason, ImmData) ->
    CcbHandle = proplists:get_value(ccbhandle, ImmData),
    case safs_imm_om:get_ccbid_from_handle(CcbHandle) of
	{ok, CcbId} ->
	    set_error_string(CcbId, Reason);
	_ ->
	    ok
    end.

set_error_string(CcbId,
		 {invalid_no_dangling, not_config_obj, {DN, AttrName, Value}}) ->
    MimDN = translate_dn(DN),
    MimValue = translate_dn(Value),
    String = io_lib:format("Attribute '~p' in object: '~s' "
			   "is a reference to non configuration object: '~s'",
			   [AttrName, MimDN, MimValue]),
    FlattenedString = lists:flatten(String),
    safs_imm_om:ccb_set_error_string(CcbId, FlattenedString);
set_error_string(CcbId,
		 {invalid_no_dangling, obj_not_exist, {DN, AttrName, Value}}) ->
    MimDN = translate_dn(DN),
    MimValue = translate_dn(Value),
    String = io_lib:format("Attribute '~p' in object: '~s' "
			   "is a reference to non existent object: '~s'",
			   [AttrName, MimDN, MimValue]),
    FlattenedString = lists:flatten(String),
    safs_imm_om:ccb_set_error_string(CcbId, FlattenedString);
set_error_string(CcbId,
		 {invalid_no_dangling, incoming_reference, {FromDN, ToDN}}) ->
    MimFromDN = translate_dn(FromDN),
    MimToDN = translate_dn(ToDN),
    String = io_lib:format("Object: '~s' cannot be removed due to a reference "
			   "from object: '~s'", [MimToDN, MimFromDN]),
    FlattenedString = lists:flatten(String),
    safs_imm_om:ccb_set_error_string(CcbId, FlattenedString);
set_error_string(CcbId, {object_changed, What, DN}) ->
    WhatString = translate_object_changed(What),

    MimDN = translate_dn(DN),
    String = io_lib:format("Object: '~s' has changed. ~s", [MimDN, WhatString]),

    safs_imm_om:ccb_set_error_string(CcbId, lists:flatten(String));
set_error_string(CcbId, {class_changed, What, ClassName}) ->
    WhatString = translate_class_changed(What),
    String = io_lib:format("~s : ~p", [WhatString, ClassName]),

    safs_imm_om:ccb_set_error_string(CcbId, lists:flatten(String));
set_error_string(_CcbId, _Reason) ->
    ok.

translate_dn(undefined) ->
    "unknown";
translate_dn(DN) ->
    case gmfSALib:imm_to_mim(DN) of
	{ok, MimDN} ->
	    MimDN;
	Error ->
	    sysInitI:error_msg("~p:translate_dn(~p)~n"
			       "Error:~p~n",[?MODULE, DN, Error]),
	    "unknown"
    end.

translate_object_changed(oi) -> "Object Implementer Changed";
translate_object_changed(ao) -> "Administrative Owner Changed";
translate_object_changed(oi_and_ao) ->
    "Both Object Implementer and Administrative Owner Changed";
translate_object_changed(attrs) -> "Some Attribute changed";
translate_object_changed(obj_add_or_rm) ->
    "One or more object have been added or deleted in the subtree".

translate_class_changed(oi)         -> "Class Implementer Changed";
translate_class_changed(definition) -> "Class Definition Changed".

imm_error({error, Err}) when is_atom(Err)->
    {error, list_to_binary(atom_to_list(Err))};
imm_error(Err) ->
    Err.

run(Fun) ->
    RetryList = [sa_ais_err_try_again],
    run(Fun, RetryList).

run(Fun, RetryList) ->
    %% Start with 100 milliseconds
    %% Double the sleep each iteration
    %% The 7 iterations give 12.7 seconds
    run(Fun, 7, 100, RetryList).

run(Fun, Count, Timeout, RetryList) ->
    case Fun() of
	{error, Reason} when Count > 0 ->
	    check_reason(Reason, Fun, Count, Timeout, RetryList);
	Result ->
	    Result
    end.

check_reason(Reason, Fun, Count, Timeout, RetryList) ->
    case lists:member(Reason, RetryList) of
	true ->
	    %% Sleep and retry again
	    timer:sleep(Timeout),
	    run(Fun, Count-1, 2*Timeout, RetryList);
	false ->
	    %% Give up
	    {error, Reason}
    end.

get_ccbflags() ->
    case application:get_env(gmf, ccbflags) of
	{ok, CcbFlags} ->
	    get_ccbflags(CcbFlags);
	undefined ->
	    ?CCBFLAGS
    end.

get_ccbflags(CcbFlags) ->
    case validate_ccbflags(CcbFlags) of
	true -> CcbFlags;
	false -> ?CCBFLAGS
    end.

validate_ccbflags([])                    -> true;
validate_ccbflags([ccb_registered_oi|T]) -> validate_ccbflags(T);
validate_ccbflags([ccb_allow_null_oi|T]) -> validate_ccbflags(T);
validate_ccbflags(_)                     -> false.
