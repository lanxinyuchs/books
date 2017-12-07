%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfDataInit.erl %
%%% @author etxlg
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1

%%% @doc ==Data initialization for GMF==
%%% This module contains the necessary initialization for the GMF application
%%% @end

-module(gmfDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1').
-author('etxlg').
-date('2017-10-26').

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
%%% R4A/1   2015-06-09 etxpeno  support for test mom
%%% R4A/4   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/6   2015-08-24 eolaand  Register for OamAccessPoint even if no Transport.
%%% R4A/8   2015-09-03 etxberb  Moved 'comsaI:register_mims_and_cli_exts' to
%%%                             post_init and added call to
%%%                             coiMim:post_init_from_gmf.
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2015-11-17 etxpeno  only start servers on the active core
%% R5A/3   2016-01-07 etxberb  Changed installation phases in all blocks.
%% R5A/4   2016-01-20 etxpeno  refactoring after changes in R5A/3
%% R5A/6   2016-02-15 etxpejn  Changed module call to lmaI
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-06-17 etxpeno  Struct as attribute
%% R6A/2   2016-09-15 etxpeno  Improvement of MIB sync
%% ----    ---------- -------  ------------------------------------------------
%% R7A/1   2016-10-07 etxpeno  Improvement of MIB sync
%% ----    ---------- -------  ------------------------------------------------
%% R8A/1   2016-12-13 etxpeno  Remove support for 'struct as object'
%% R8A/2   2017-01-03 etxpeno  use gmfSALib:imm_to_mim/1
%% ----    ---------- -------  ------------------------------------------------
%% R9A/2   2017-02-13 ecaiyan  delete_struct_objs, return ok even when object does not exists
%% R9A/3   2017-02-15 etxpeno  Comments and minor improvements
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-08-15 etxpeno  improve handling of parent-child relations
%% ----------------------------------------------------------
%% R12A/1  2017-10-26 etxlg    wildcard handling filename_join/2
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 init_tables/1,
	 instPhParallel_init_board/0,
	 instPhSeqBeg_post_init/0]).
-export([children/0,
	 activate/0,
	 prep_stop/0,
	 register_mims/2,
	 restart_strategy/0]).

-export([prep_warm/0, warm/0, warm_done/0]).

-export([update_imm_objs/0, register_com_data/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("gmfMeta.hrl").
-include("gmf.hrl").
-include("om.hrl").


%% The following macros are used for OaM over TN
-define(DN_OAP, [<<"ManagedElement">>,
		 <<"SystemFunctions">>,
		 <<"SysM">>,
		 <<"OamAccessPoint">>]).

-define(FILE_OAP, "RcsOamAccessPoint.xml").

-define(MonoTime, erlang:monotonic_time()).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%% ----------------------------------------------------------
%% @spec instPhParallel_init(Nodes)
%%
%%    -> ok
%% where
%%  Nodes = list()
%% @doc
%%  Create mnesia tables used by GMF.
%%  @end
%% ----------------------------------------------------------
instPhParallel_init(DbNodes) ->
    %% Temporary solution
    Path = filename:join([code:lib_dir(safs), "imm_init.xml"]),

    application:set_env(safs, imm_xml, Path),
    application:start(safs),
    application:unset_env(safs, imm_xml),

    init_tables(DbNodes).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%% ----------------------------------------------------------
%% @spec instPhParallel_init_data()
%%     -> ok
%%
%% @doc
%%  Registers gmfAppData in SWM as the receiver of all appdata files
%%  stamped with tag "gmf".
%%  @end
%% ----------------------------------------------------------

instPhParallel_init_data()->
    [swmI:register_appdata_receiver(Tag, gmfAppData)
     || Tag <- [?GMFAPP_MIM, ?GMFAPP_IMM, ?GMFAPP_CLI_EXTENSION]],

    IsUpgradeOngoing = swmI:is_upgrade_ongoing(),

    F = get_gmfVars_fun(IsUpgradeOngoing),

    {atomic, ok} = mnesia:transaction(F),
    ok = gmfEsi:init_data().

get_gmfVars_fun(false) ->
    fun() ->
	    ok = mnesia:write(#gmfVars{key   = admin_invoc_id,
				       value = 0})
    end;
get_gmfVars_fun(true) ->
    fun() ->
	    List = swmI:all_objects(gmfVars),
	    %% Only keep the object with key 'admin_invoc_id'
	    lists:foreach(
	      fun(#gmfVars{key = admin_invoc_id} = Obj) ->
		      ok = mnesia:write(Obj);
		 (_) ->
		      ok
	      end, List),
	    ok
    end.

%% ----------------------------------------------------------
%% @spec init_tables(Nodes)
%%     -> ok
%%  where
%%     Nodes = lists()
%% @doc
%% init_tables
%% @end
%% ----------------------------------------------------------
-define(GMF_MNESIA_TABLE(_Table, _DbNodes),
	{atomic, ok} =
	    clhI:mnesia_create_table(_Table,
				     [{type, set},
				      {disc_copies, DbNodes},
				      {attributes, record_info(fields, _Table)} |
				      add_clh_option(_Table)])).

init_tables(DbNodes)->
    %% IMM related tables
    ?GMF_MNESIA_TABLE(gmfImmClass,       DbNodes),
    ?GMF_MNESIA_TABLE(gmfImmObject,      DbNodes),
    ?GMF_MNESIA_TABLE(gmfImmObjectFile,  DbNodes),

    %% MIM related tables
    init_gmfMimClass(DbNodes),
    ?GMF_MNESIA_TABLE(gmfMimStruct,      DbNodes),
    ?GMF_MNESIA_TABLE(gmfMimDerivedType, DbNodes),
    ?GMF_MNESIA_TABLE(gmfMimChildren,    DbNodes),
    init_MimRelation(DbNodes),

    %% Mapping related tables
    init_gmfDnMap(DbNodes),

    %% Meta data related tables
    init_gmfMetaBiDir(DbNodes),

    %% CXP/Rev related tables
    ?GMF_MNESIA_TABLE(gmfCxpRev,         DbNodes),

    %% GMF variables
    ?GMF_MNESIA_TABLE(gmfVars,           DbNodes),

    %% Test/possible future updates for ERL i/f handling for mim files.
    ?GMF_MNESIA_TABLE(gmfMims,           DbNodes),

    ok = gmfTrService:init_tables(DbNodes),

    ok.

init_gmfMimClass(DbNodes) ->
    AttrList = record_info(fields, gmfMimClass),
    IdxList = [#gmfMimClass.imm_rdn],
    Table = gmfMimClass,
    {atomic, ok} =
	clhI:mnesia_create_table(Table, [{disc_copies, DbNodes},
					 {attributes, AttrList},
					 {index, IdxList} |
					 add_clh_option(Table)]).

init_gmfMetaBiDir(DbNodes) ->
    AttrList = record_info(fields, gmfMetaBiDir),
    IdxList = [#gmfMetaBiDir.imm_class],
    Table = gmfMetaBiDir,
    {atomic, ok} =
	clhI:mnesia_create_table(Table, [{disc_copies, DbNodes},
					 {attributes, AttrList},
					 {index, IdxList} |
					 add_clh_option(Table)]).

init_gmfDnMap(DbNodes) ->
    AttrList = record_info(fields, gmfDnMap),
    IdxList = [#gmfDnMap.imm_names],
    Table = gmfDnMap,
    {atomic, ok} =
	clhI:mnesia_create_table(Table, [{disc_copies, DbNodes},
					 {attributes, AttrList},
					 {index, IdxList} |
					 add_clh_option(Table)]).

init_MimRelation(DbNodes) ->
    AttrList = record_info(fields, gmfMimRelation),
    Table = gmfMimRelation,
    {atomic, ok} =
	clhI:mnesia_create_table(Table, [{disc_copies, DbNodes},
					 {attributes, AttrList},
					 {type, bag} |
					 add_clh_option(Table)]).


%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_board() ->
    ok = gmfEsi:init_board(clhI:core_state()).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhSeqBeg_post_init() ->
    T0 = ?MonoTime,
    ok = update_imm_objs(),
    T1 = ?MonoTime,
    UseNbi = not lmaI:load_test_mom(),
    T2 = ?MonoTime,
    ok = register_com_data(UseNbi),
    T3 = ?MonoTime,
    ok = appmI:register_warm_cb(?MODULE),
    T4 = ?MonoTime,
    sysInitI:info_report([{?MODULE, ?FUNCTION_NAME},
			  {'update_imm_objs', sysUtil:time_to_string(T1 - T0)},
			  {'load_test_mom', sysUtil:time_to_string(T2 - T1)},
			  {'register_com_data', sysUtil:time_to_string(T3 - T2)},
			  {'register_warm_cb', sysUtil:time_to_string(T4 - T3)},
			  "-------------- Summary --------------",
			  {'TOTAL', sysUtil:time_to_string(T4 - T0)}]),
    ok.

%% ----------------------------------------------------------
%% @spec children()
%%
%%     -> {ok, ChildSpec}
%%
%% where
%%    ChildSpec = list()
%%
%% @doc
%%  Define GMF child processes for gmfServer. See the OTP documentation
%%  of the 'supervisor' module for a description of the child_spec()
%%  6-tuples that make up the ChildSpec list.
%%
%%  The gmfImmUgMaster process is declared as transient, allowing it
%%  to shut down immediately if the context is not an upgrade To-version
%%  start.
%%  @end
%% ----------------------------------------------------------

children() ->
    CoreState = clhI:core_state(),
    children(CoreState).

children(active) ->
    Spec = [{gmfServer, {gmfServer, start_link, []},
	     permanent, 10000, worker, [gmfServer]},
	    {gmfImmOiI, {gmfImmOiI, start_link, []},
	     permanent, 10000, worker, [gmfImmOiI]},
	    {gmfTrService, {gmfTrService, start, []},
	     permanent, 10000, worker, [gmfTrService]},
	    {gmfImmUgMaster, {gmfImmUgMaster, start, []},
	     transient, 10000, worker, [gmfImmUgMaster]}],

    {ok, Spec};
children(_) ->
    {ok, []}.

%% ----------------------------------------------------------
%% @spec activate()
%%     -> ok
%%
%% @doc
%% Activate start phase.
%% Initialize om
%% Register mos
%% Register mims
%%  @end
%% ----------------------------------------------------------

activate() ->
    activate(clhI:core_state()).

activate(active) ->
    catch application:start(safs),
    sysInitI:info_msg("~p: ~p~n", [?MODULE, ?FUNCTION_NAME]),

    {ok, ImmHandle} = gmfImmOmI:initialize(),
    ets:insert(gmfData, {"gmf_imm_handle", ImmHandle}),

    register_comsa_callbacks(ImmHandle),

    gmfImmOmI:activate(),
    gmfImmOiI:activate(),
    gmfServer:activate(),
    gmfTrService:activate(),
    ok;
activate(_CoreState) ->
    catch application:start(safs),
    sysInitI:info_msg("~p: ~p~n", [?MODULE, ?FUNCTION_NAME]).

prep_stop() ->
    prep_stop(clhI:core_state()).

prep_stop(active) ->
    gmfServer:stop_com();
prep_stop(_CoreState) ->
    ok.

register_mims(Key, [_|_] = MimList) ->
    %% {RelPath, FileName}
    F = fun() ->
		GmfMims = mnesia:wread({gmfMims, Key}),
		NewGmfMims = rmims(GmfMims, Key, MimList),
		mnesia:write(NewGmfMims)
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

rmims([], Key, MimList) ->
    #gmfMims{key      = Key,
	     mim_list = MimList};
rmims([#gmfMims{mim_list = ML} = GmfMims], _, MimList) ->
    GmfMims#gmfMims{mim_list = lists:append(ML, MimList)}.

restart_strategy() ->
    {ok, {one_for_one, 0, 1}}.

register_com_data(UseNbi) ->
    T0 = ?MonoTime,
    {atomic, {MimFiles, _RootMos, SharedLibs}} =
	mnesia:transaction(fun() -> get_mim_data(UseNbi) end),
    T1 = ?MonoTime,
    {atomic, OtherMimFiles} = mnesia:transaction(fun get_other_mims/0),
    T2 = ?MonoTime,
    NewMimFiles = remove_oap_mim_files(lists:reverse(MimFiles, OtherMimFiles)),
    T3 = ?MonoTime,
    comsaI:register_mims_and_cli_exts(NewMimFiles, SharedLibs),
    T4 = ?MonoTime,
    sysInitI:info_report([{?MODULE, ?FUNCTION_NAME},
			  {'get_mim_data', sysUtil:time_to_string(T1 - T0)},
			  {'get_other_mims', sysUtil:time_to_string(T2 - T1)},
			  {'remove_oap_mim_files',
			   sysUtil:time_to_string(T3 -T2)},
			  {'register_mims_and_cli_exts',
			   sysUtil:time_to_string(T4 - T3)},
			  "-------------- Summary --------------",
			  {'TOTAL', sysUtil:time_to_string(T4 - T0)}]),
    ok.

get_other_mims() ->
    mnesia:foldr(fun gom/2, [], gmfMims).

gom(#gmfMims{mim_list = MimInfo}, Acc) ->
    lists:append(MimInfo, Acc).

%% @doc Picks up MIM related info from the gmfCxpRev table.
%% The result is a 3-tuple of lists that are
%% accumulated over all CXPs. The list elements
%% are:
%%
%%   {CxpPath, [AbsPath, ...]}  tuple of CxpPath and list of final NBI MIM abspaths
%%   {{CxpId, CxpRev}, LDN}  tuple
%%   SharedLib               final CLI Extension shared lib file
%%
%% The UseNbi argument controls which type of MIM that COM should load.
%% @end

-spec get_mim_data(boolean()) -> {list(), list(), list()}.

get_mim_data(UseNbi) ->
    CurrentArch = sysEnv:architecture(),
    F = fun(#gmfCxpRev{key      = Key,
		       ldn      = RootMos,
		       cxp_path = CxpPath,
		       mim_info = MimInfos,
		       cli_info = SharedLibs},
	    {Acc1, Acc2, Acc3}) ->
		{[{CxpPath, mi(CxpPath, MimInfos, UseNbi)}|Acc1],
		 [{Key, RootMos}|Acc2],
		 lists:append(resolveSharedLibs(CxpPath, SharedLibs,
						CurrentArch), Acc3)
		}
	end,

    mnesia:foldr(F, {[], [], []}, gmfCxpRev).

mi(CxpPath, MimInfo, UseNbi) ->
    lists:map(
      fun([File, NbiFile, Rel|_]) ->
	      RealFile = choose(UseNbi, NbiFile, File),
	      AbsPath = filename_join(CxpPath, Rel, RealFile),
	      swmI:find_file(AbsPath)
      end, MimInfo).

resolveSharedLibs(CxpPath, SharedLibs, CurrentArch) ->
    [begin
	 AbsPath = filename_join(CxpPath, RelPath),
	 swmI:find_file(AbsPath)
     end || #cliSharedLib{type=Type, relpath=RelPath} <- SharedLibs,
	    Type =:= CurrentArch].

register_comsa_callbacks(ImmHandle) ->
    [reg_imm_comte(RootMos, Key, ImmHandle) ||
	{Key, RootMos} <- get_root_mo_data()],
    ok.

get_root_mo_data() ->
    F = fun() -> mnesia:foldr(fun grmd/2, [], gmfCxpRev) end,
    {atomic, RootMoData} = mnesia:transaction(F),
    RootMoData .

grmd(#gmfCxpRev{key = Key, ldn = RootMos}, Acc) ->
    [{Key, RootMos}|Acc].

reg_imm_comte(RootMos, Key, ImmHandle) ->
    [begin
	 CB = {gmfComteI, RootMo, Key, ImmHandle},
	 ok = comsaI:register_callback(RootMo, CB)
     end || RootMo <- RootMos].


filename_join(CxpPath, RelPath) ->
%    lists:foldl(fun fj/2, CxpPath, string:tokens(RelPath, "/")).
    WildPath = filename:join([CxpPath, RelPath]),
    case filelib:wildcard(WildPath) of
	[AbsDir] -> AbsDir;
	[]	 -> erlang:error({non_existent_path, WildPath});
	Any	 -> erlang:error({ambiguous_path, {WildPath, Any}})
    end.

filename_join(CxpPath, RelPath, FileName) ->
    AbsPath = filename_join(CxpPath, RelPath),
    filename:join([AbsPath, FileName]).

%fj(WildDir, Acc) ->
%    PartialPath = filename:join([Acc, WildDir]),
%    Res = filelib:wildcard(PartialPath),
%    fj1(Res, PartialPath).
%
%fj1([],            PartialPath) ->
%    erlang:error({non_existent_path, PartialPath});
%fj1([AbsDir],     _PartialPath) ->
%    AbsDir;
%fj1(MultiplePaths, PartialPath) ->
%    erlang:error({ambiguous_path, {PartialPath, MultiplePaths}}).

%% check if transport fragment is loaded
%% is_transport_loaded() ->
%%     true == ets:foldl(gmfMimClass, fun itl/2).

%% itl(_, true) ->
%%     true;
%% itl(#gmfMimClass{key = {Mom, _}}, _) ->
%%     lists:prefix("Rtn", Mom).


%% remove OamAccessPoint if Transport does not exist
%% check_tn_root_mos(true, RootMos) ->
%%     RootMos;
%% check_tn_root_mos(false, RootMos) ->
%%     lists:reverse(lists:foldl(fun ctn_rm/2, [], RootMos)).

%% ctn_rm({Rev, L}, Acc) ->
%%     [{Rev, lists:delete(?DN_OAP, L)} | Acc].


%% remove OamAccessPoint file
remove_oap_mim_files(MimFiles) ->
    lists:reverse(lists:foldl(fun ctn_mf/2, [], MimFiles)).

ctn_mf({Cxp, Files}, Acc) ->
    case [File || File <- Files, string:str(File, ?FILE_OAP) == 0] of
	[]  -> Acc;
	Res -> [{Cxp, Res} | Acc]
    end;
ctn_mf(File, Acc) when is_list(File) ->
    case string:str(File, ?FILE_OAP) of
	0 -> [File | Acc];
	_ -> Acc
    end.

update_imm_objs() ->
    %% This function updates all IMM object instances
    %% * Add extra IMM attribute RcsImmAttrObjId
    %% * Add extra IMM attribute RcsImmAttrEcimDn
    %% * Convert all structs from "Struct As Object" representation to
    %%   "Struct As Attribute" representation

    {ok, OmHandle} = gmfImmOmI:initialize(),

    %% Fetch all IMM objects
    ImmObjs = get_imm_objs(OmHandle),

    %% Update all IMM objects
    NoUpdatedImmObjs = update_imm_objs(OmHandle, ImmObjs),

    ok = gmfImmOmI:finalize(OmHandle),

    sysInitI:info_report([{?MODULE, ?FUNCTION_NAME},
			  {no_of_updated_imm_objects, NoUpdatedImmObjs}]),
    ok.

get_imm_objs(OmHandle) ->
    {ok, SearchHandle} = gmfImmOmI:search_initialize_2(OmHandle, undefined),
    ImmObjs = get_imm_objs(SearchHandle, []),
    ok = gmfImmOmI:search_finalize(SearchHandle),

    ImmObjs.

get_imm_objs(SearchHandle, Acc) ->
    Result = gmfImmOmI:search_next_n_s2(SearchHandle, 1000),
    gio(Result, SearchHandle, Acc).

gio({ok, L}, SearchHandle, Acc) -> get_imm_objs(SearchHandle, L ++ Acc);
gio({error, <<"sa_ais_err_not_exist">>}, _, Acc) -> Acc.

update_imm_objs(OmHandle, ImmObjs) ->
    {ok, AOHandle} = gmfImmOmI:admin_owner_initialize(OmHandle, <<"GMF">>, true),
    {ok, CcbHandle} = gmfImmOmI:ccb_initialize(AOHandle, []),

    %% Map the data in the IMM objects to an internal format
    UpdatedImmObjs = get_updated_imm_objs(ImmObjs),

    %% Update the data in the IMM objects
    update_imm_objects(AOHandle, CcbHandle, ImmObjs, UpdatedImmObjs),

    ok = gmfImmOmI:ccb_apply(CcbHandle),

    ok = gmfImmOmI:ccb_finalize(CcbHandle),
    ok = gmfImmOmI:admin_owner_finalize(AOHandle),

    length(UpdatedImmObjs).

get_updated_imm_objs(L) ->
    GuioFun =
	fun({ImmDn, V}) ->
		M = #{immDn => ImmDn},
		M1 = get_extra_attributes(M, ImmDn),
		{_, _, [ImmClass]} =
		    lists:keyfind(<<"SaImmAttrClassName">>, 1, V),
		get_struct_info(M1, ImmDn, ImmClass)
	end,

    F = fun() -> lists:map(GuioFun, L) end,
    {atomic, R} = mnesia:transaction(F),
    R.

get_extra_attributes(M, ImmDn) ->
    Result = gmfSALib:imm_to_mim(ImmDn),
    gea(M, Result).

gea(M, {ok, EcimDn}) ->
    ObjId = gmfTrService:get_object_id(EcimDn),
    M#{ecimDn => EcimDn, objId => ObjId};
gea(M, {error, _}) ->
    M.

get_struct_info(M, ImmDn, ImmClass) ->
    ClassL = binary_to_list(ImmClass),
    Result = mnesia:read(gmfImmClass, ClassL),
    gsi(M, ImmDn, Result).

gsi(M, _ImmDn,
    [#gmfImmClass{gmfMimClass = MimClass}]) when MimClass /= undefined->
    Result = mnesia:read({gmfMimClass, MimClass}),
    gsi1(M, Result);
gsi(M, _ImmDn, _) ->
    M.

gsi1(M, []) -> M;
gsi1(M, [Mim]) ->
    F = fun({Attr, Props}) ->
		BasicType = gmfSALib:get_com_basic_type(Props),
		gsi2(BasicType, Attr)
	end,
    Attrs = Mim#gmfMimClass.attributes,
    StructAttrs = lists:filtermap(F, Attrs),
    gsi3(M, StructAttrs).

gsi2({{structRef, StructName, StructMomName}, _}, StructAttr) ->
    Key = {StructMomName, StructName},
    [Obj] = mnesia:read(gmfMimStruct, Key),

    Attributes = Obj#gmfMimStruct.attributes,
    ImmNs = Obj#gmfMimStruct.imm_ns,

    %% Concatenate strings ImmNs and StructName to a binary
    ImmStructName = iolist_to_binary([ImmNs, StructName]),

    StructMembers = [list_to_binary(Attr) || {Attr, _} <- Attributes],

    {true, {list_to_binary(StructAttr),
	    ImmStructName,
	    StructMembers}};
gsi2(_, _) ->
    false.

gsi3(M, []) -> M;
gsi3(M, StructData) -> M#{structData => StructData}.

update_imm_objects(AOHandle, CcbHandle, ImmObjs, UpdatedImmObjs) ->
    F = fun(#{immDn := ImmDn,
	      ecimDn := EcimDn,
	      objId := ObjId,
	      structData := StructData}) ->
		%% update extra attributes and struct attributes
		ok = gmfImmOmI:admin_owner_set(AOHandle, [ImmDn], one),
		update_extra_attrs(CcbHandle, ImmDn, EcimDn, ObjId),
		update_struct_attrs(AOHandle, CcbHandle, ImmDn, ImmObjs,
				    StructData);
	   (#{immDn := ImmDn,
	      ecimDn := EcimDn,
	      objId := ObjId}) ->
		%% update extra attributes
		ok = gmfImmOmI:admin_owner_set(AOHandle, [ImmDn], one),
		update_extra_attrs(CcbHandle, ImmDn, EcimDn, ObjId);
	   (#{immDn := ImmDn,
	      structData := StructData}) ->
		%% update struct attributes
		ok = gmfImmOmI:admin_owner_set(AOHandle, [ImmDn], one),
		update_struct_attrs(AOHandle, CcbHandle, ImmDn, ImmObjs,
				    StructData);
	   (#{}) ->
		%% no update of IMM object
		ok
	end,

    lists:foreach(F, UpdatedImmObjs).

update_extra_attrs(CcbHandle, ImmDn, EcimDn, ObjId) ->
    ExtraAttrMods = [{<<"RcsImmAttrObjId">>,  {uint32, [ObjId]}},
		     {<<"RcsImmAttrEcimDn">>, {string, [EcimDn]}}],
    ok = gmfImmOmI:ccb_object_modify_extra_attrs_s2(CcbHandle,
						    ImmDn,
						    ExtraAttrMods).

update_struct_attrs(AOHandle, CcbHandle, ImmDn, ImmObjs, StructData) ->
    Values = usa(ImmDn, ImmObjs),
    StructValues =
	lists:flatmap(
	  fun({AttrName, StructName, StructMembers}) ->
		  {_, _, ImmDnList} = lists:keyfind(AttrName, 1, Values),
		  AttrList =
		      [usa1(Dn, ImmObjs, StructMembers) || Dn <- ImmDnList],
		  [delete_struct_objs(AOHandle, CcbHandle, Dn) ||
		      Dn <- ImmDnList],

		  usa2(AttrName, StructName, AttrList)
	  end, StructData),

    ok = safs_imm_om:ccb_object_modify_2(CcbHandle, ImmDn, StructValues).

usa(ImmDn, ImmObjs) ->
    {_, Values} = lists:keyfind(ImmDn, 1, ImmObjs),
    Values.

usa1(ImmDn, ImmObjs, StructMembers) ->
    AllValues = usa(ImmDn, ImmObjs),
    lists:map(
      fun(StructMemberName) ->
	      {StructMemberName, AttrValueType, Values} =
		  lists:keyfind(StructMemberName, 1, AllValues),
	      AttrValues =
		  [gmfSALib:get_imm_attr_value(AttrValueType, V) || V <- Values],
	      AttrValuesNumber = length(AttrValues),
	      #safsImmAttrValues_2{attrName         = StructMemberName,
				   attrValueType    = AttrValueType,
				   attrValuesNumber = AttrValuesNumber,
				   attrValues       = AttrValues}
      end, StructMembers).

usa2(_AttrName, _StructName, []) ->
    [];
usa2(AttrName, StructName, AttrList) ->
    AttrValueType = sa_imm_attr_csstructt,
    AttrValues =
	[begin
	     S = #safsImmCsStruct{structName    = StructName,
				  structMembers = A},
	     #safsImmAttrValue{csstruct = S}
	 end || A <- AttrList],
    AttrValuesNumber = length(AttrValues),

    ModType = sa_imm_attr_values_replace,
    ModAttr = #safsImmAttrValues_2{attrName         = AttrName,
				   attrValueType    = AttrValueType,
				   attrValuesNumber = AttrValuesNumber,
				   attrValues       = AttrValues},

    [#safsImmAttrModification_2{modType = ModType,
				modAttr = ModAttr}].

delete_struct_objs(AOHandle, CcbHandle, ImmDn) ->
    ok = safs_imm_om:admin_owner_set(AOHandle, [ImmDn], sa_imm_subtree),
    gmfImmOmI:ccb_object_delete(CcbHandle, ImmDn),
    ok.

prep_warm() -> safs:rcs_prep_warm().

warm() -> safs:rcs_warm().

warm_done() -> ok.

choose(true,  T, _) -> T;
choose(false, _, F) -> F.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
