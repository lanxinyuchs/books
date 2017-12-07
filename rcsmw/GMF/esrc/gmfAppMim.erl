%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfAppMim.erl %
%%% Author:
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfAppMim).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-10-26').
-author('etxlg').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%% Rev     Date       Name     What
%% ----    ---------- -------  ------------------------------------------------
%% R4A/1   2015-06-09 etxpeno  support for test mom
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2016-01-13 etxberb  Parallelized appdata routines and removed some
%%                             mnesia transactions.
%% R5A/3   2016-01-20 etxpeno  support for ifType = erl
%% R5A/4   2016-03-08 etxpeno  no mixing between ets:lookup and mnesia:write
%% ----    ---------- -------  ------------------------------------------------
%% R6A/2   2016-09-15 etxpeno  Improvement of MIB sync
%% ----    ---------- -------  ------------------------------------------------
%% R7A/1   2016-10-07 etxpeno  Improvement of MIB sync
%% R7A/2   2016-10-13 etxpeno  Correction of populate_dn_map
%% ----    ---------- -------  ------------------------------------------------
%% R8A/1   2016-11-14 etxberb  Added is_registerOnNode/1.
%% R8A/2   2016-11-15 etxpeno  code review
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-03-06 etxberb  Added call to swmI:is_node_type_valid
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-08-15 etxpeno  improve handling of parent-child relations
%% R11A/2  2017-10-16 etxpeno  OTP20 improvements
%% ----------------------------------------------------------
%% R12A/1  2017-10-26 etxlg    wildcard handling for mim-reg
%% R12A/2  2017-10-26 etxlg    fixed above
%%% ----------------------------------------------------------
%%%
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([appdata/3]).
-export([ad_complete/0,
	 populate_parent/1,
	 update_root_mos/1,
	 update_mim_map/1,
	 update_mim_class/1,
	 update_meta_bidir/1,
	 populate_dn_map/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("gmf.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(MonoTime, erlang:monotonic_time()).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	sysInitI:error_report(?RepInfo(__ReportInfo))).
-define(LOG_INFO(__ReportInfo),
	sysInitI:info_report(?RepInfo(__ReportInfo))).
-define(LOG_WARN(__ReportInfo),
	sysInitI:warning_report(?RepInfo(__ReportInfo))).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION_NAME} | __RepInfo]).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% @spec appdata(CxpProdId, CxpVersion, AppDataE)
%%
%%       -> ok
%%
%% where
%%
%%   CxpProdId  = string()
%%   CxpVersion = string()
%%   AppDataE   = #xmlElement{}
%%
%% @doc
%%   Use AppDataE to store mim information in #gmfMimClass{}
%%   and #gmfCxpRev{} tables.
%% @end
%%===========================================================================
appdata(CxpProdId, CxpRev, AppDataE) ->
    T0 = ?MonoTime,
    create_mim_map(),
    T1 = ?MonoTime,
    {ok, CxpPath} = swmI:get_cxp_path(CxpProdId, CxpRev),

    %% Cannot be more than one MimInfosE
    MimInfosE = find_element(mimInfos, AppDataE),

    %% Collect all <mimInfo> elements in <mimInfos>
    MimInfos  = [MimInfoE ||
		    MimInfoE <- MimInfosE#xmlElement.content,
		    MimInfoE#xmlElement.name == mimInfo],

    T2 = ?MonoTime,
    FileList = get_mim_filelist(MimInfos),
    T3 = ?MonoTime,

    TableList = get_tablelist(CxpProdId, CxpRev, CxpPath, FileList),
    T4 = ?MonoTime,

    {atomic, ok} =
	mnesia:transaction(
	  fun() ->
		  ok = update_tables(TableList),
		  ok = update_gmfCxpRev(CxpProdId, CxpRev, CxpPath, FileList)
	  end),
    T5 = ?MonoTime,
    ?LOG_INFO([{'create_mim_map', sysUtil:time_to_string(T1 - T0)},
	       {'get_cxp_path', sysUtil:time_to_string(T2 - T1)},
	       {'get_mim_filelist', sysUtil:time_to_string(T3 - T2)},
	       {'get_tablelist', sysUtil:time_to_string(T4 -T3)},
	       {'update_tables', sysUtil:time_to_string(T5 -T4)},
	       "-------------- Summary --------------",
	       {'TOTAL', sysUtil:time_to_string(T5 - T0)}]),
    ok.

ad_complete() ->
    T0 = ?MonoTime,
    ok = populate_parent(),
    T1 = ?MonoTime,
    update_root_mos(),
    T2 = ?MonoTime,
    ok = update_mim_map(),
    T3 = ?MonoTime,
    update_mim_class(),
    T4 = ?MonoTime,
    ok = update_meta_bidir(),
    T5 = ?MonoTime,
    ok = delete_mim_map(),
    T6 = ?MonoTime,
    ok = populate_dn_map(),
    T7 = ?MonoTime,
    ok = populate_mim_children(),
    T8 = ?MonoTime,
    ?LOG_INFO([{'populate_parent', sysUtil:time_to_string(T1 - T0)},
	       {'update_root_mos', sysUtil:time_to_string(T2 - T1)},
	       {'update_mim_map', sysUtil:time_to_string(T3 - T2)},
	       {'update_mim_class', sysUtil:time_to_string(T4 - T3)},
	       {'update_meta_bidir', sysUtil:time_to_string(T5 - T4)},
	       {'delete_mim_map', sysUtil:time_to_string(T6 - T5)},
	       {'populate_dn_map', sysUtil:time_to_string(T7 - T6)},
	       {'populate_mim_children', sysUtil:time_to_string(T8 - T7)},
	       "-------------- Summary --------------",
	       {'TOTAL', sysUtil:time_to_string(T8 - T0)}]),
    ok.


%%% ----------------------------------------------------------
%%% @doc Returns a list of file info items. Each item is
%%% a list of 4 strings: the file basename,
%%% the basename of the file used for NBI,
%%% the relative path (which may have wildcards)
%%% and an interface type specification.
%%% @end
%%% ----------------------------------------------------------
-spec get_mim_filelist([#xmlElement{}]) -> [list()].

get_mim_filelist([MimInfoE | MimInfos]) ->
    case is_registerOnNode(MimInfoE) of
	true ->
	    File = find_attribute(file, MimInfoE),
	    Path = find_attribute(path, MimInfoE),
	    IfType = find_attribute(ifType, MimInfoE),
	    NbiFile = get_nbi_file(MimInfoE, File),
	    [[File, NbiFile, Path, IfType] | get_mim_filelist(MimInfos)];
	false ->
	    ?LOG_INFO([{"MimInfo not registered",
			"Node Type not specified in the registerOnNodes list."},
		       {file, (catch find_attribute(file, MimInfoE))},
		       {path, (catch find_attribute(path, MimInfoE))},
		       {ifType, (catch find_attribute(ifType, MimInfoE))},
		       {node_type, swmI:node_type()},
		       {registerOnNodes, find_registerOnNodes(MimInfoE)}]),
	    get_mim_filelist(MimInfos);
	error ->
	    get_mim_filelist(MimInfos)
    end;
get_mim_filelist([]) -> [].

get_nbi_file(MimInfoE, File) ->
    try find_attribute(nbiFile, MimInfoE) of
	V -> V
    catch error:{badmatch, false} ->
	    File
    end.

get_tablelist(CxpProdId, CxpRev, CxpPath, [FileInfo|FileList]) ->
    case parse(CxpProdId, CxpRev, CxpPath, FileInfo) of
	undefined ->
	    get_tablelist(CxpProdId, CxpRev, CxpPath, FileList);
	{FD, Tid} ->
	    warn_illegal_namespace(Tid, {CxpProdId, CxpRev, FileInfo}),
	    [{FD, Tid}|get_tablelist(CxpProdId, CxpRev, CxpPath, FileList)]
    end;
get_tablelist(_, _, _, []) -> [].

warn_illegal_namespace(Tid, {CxpProdId, CxpRev, FileInfo}) ->
    case is_legal_namespace(Tid) of
	false ->
	    Path = filename:join([sysEnv:rcs_root(),"dumps","ECIM_ERROR.txt"]),
	    filelib:ensure_dir(Path),
	    case file:open(Path, [append]) of
		{ok, Fd} ->
		    io:format(Fd, "Illegal file ~p in ~p ~p~n",
			      [FileInfo, CxpProdId, CxpRev]),
		    file:close(Fd);
		{error, Reason} ->
		    sysInitI:error_msg("~w: file:open(~p,[append]) = ~p",
				       [?MODULE, Path, {error, Reason}])
	    end,

	    sysInitI:error_msg("Illegal file ~p in ~p ~p ~n",
			       [FileInfo, CxpProdId, CxpRev]);
	true ->
	    ok
    end.

is_legal_namespace(Tid) ->
    try
	[case proplists:get_value(namespace, Attributes) of
	     "urn:com:ericsson:ecim:ECIM"++_ ->
		 throw(false);
	     "urn:com:ericsson:ecim:"++_->
		 true;
	     _  ->
		 throw(false)
	 end||
	    [{_, Attributes}] <-gmfMimXml:get_all_mom_info(Tid)] of
	_ ->
	    true
    catch throw:_ ->
	    false
    end.

%%% ----------------------------------------------------------
find_registerOnNodes(MimInfoE) ->
    RegisterOnNodesE = find_optional_element(registerOnNodes, MimInfoE),
    NodeElements = find_elements(node, RegisterOnNodesE),
    [find_attribute(type, NodeE) || NodeE <- NodeElements].

%%% ----------------------------------------------------------
is_registerOnNode(MimInfoE) ->
    RegisterOnNodes = find_registerOnNodes(MimInfoE),
    NodeType = swmI:node_type(),
    try
	ok = validate_registerOnNodes(RegisterOnNodes),
	lists:member(NodeType, RegisterOnNodes)
    catch
	throw : registerOnAllNodes ->
	    true;
	throw : {warnings, Warnings} ->
	    Stack = erlang:get_stacktrace(),
	    File = (catch find_attribute(file, MimInfoE)),
	    Path = (catch find_attribute(path, MimInfoE)),
	    IfType = (catch find_attribute(ifType, MimInfoE)),
	    ?LOG_WARN([{file, File},
		       {path, Path},
		       {ifType, IfType},
		       "--- Anomalies in xml file ---"
		       | Warnings] ++
			  ["--- Stacktrace ---" | Stack]),
	    lists:member(NodeType, RegisterOnNodes);
	ErrClass : ErrReason ->
	    Stack = erlang:get_stacktrace(),
	    File = (catch find_attribute(file, MimInfoE)),
	    Path = (catch find_attribute(path, MimInfoE)),
	    IfType = (catch find_attribute(ifType, MimInfoE)),
	    ?LOG_ERR([{"Error in xml file", "Not able to register mimInfo"},
		      {file, File},
		      {path, Path},
		      {ifType, IfType},
		      {node_type, NodeType},
		      {ErrClass, ErrReason},
		      "--- Stacktrace ---" | Stack]),
	    error
    end.

%%% ----------------------------------------------------------
validate_registerOnNodes([]) ->   % Default, legacy, = "ALL"
    throw(registerOnAllNodes);
validate_registerOnNodes(["ALL"]) ->
    throw(registerOnAllNodes);
validate_registerOnNodes(NodeTypes) ->
    case lists:member("ALL", NodeTypes) of
	false ->
	    validate_registerOnNodes_list(NodeTypes);
	true ->
	    erlang:error({illegal_combination, registerOnNodes, NodeTypes})
    end.

validate_registerOnNodes_list([NodeType | Tail]) ->
    case swmI:is_node_type_valid(NodeType) of
	true ->
	    validate_registerOnNodes_list(Tail);
	deprecated ->
	    add_proc_dict(registerOnNodes,
			  [{"Deprecated Node Type", NodeType}]),
	    validate_registerOnNodes_list(Tail);
	false ->
	    erlang:error({illegal_node_type, registerOnNodes, NodeType})
    end;
validate_registerOnNodes_list([]) ->
    case erase(registerOnNodes) of
	undefined ->
	    ok;
	Values ->
	    throw({warnings, Values})
    end.

%%% ----------------------------------------------------------
add_proc_dict(Tag, Values) ->
    case get(Tag) of
	undefined ->
	    put(Tag, Values);
	OldValues ->
	    put(Tag, OldValues ++ Values)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

parse(CxpProdId, CxpRev, CxpPath,
      [FileName, _NbiFileName, RelPath, "imm"|_] = FD) ->
    AbsPath = filename_join(CxpPath, RelPath, FileName),
    Path = swmI:find_file(AbsPath),
    {ok, Tid} = gmfMimXml:parse_mim(CxpProdId, CxpRev, Path),
    {FD, Tid};
parse(_CxpProdId, _CxpRev, _CxpPath, _FD) ->
    undefined.

update_tables(TableFiles) ->
    %% 1. Fetch all parent-child relationships and write them to gmfMimRelation
    %%    for later usage.
    %% 2. Get all moms
    %% 3. Get domainExtension at mom level
    %% 4. Get all classes for each mom
    %% 5. Write to gmfMimClass
    [ut(TableFile) || TableFile <- TableFiles],
    ok.
ut({[_FileName, _RelPath, _IfTable|_], Tid}) ->
    ut_rel(Tid),
    Moms = gmfMimXml:get_all_mom_info(Tid),
    [ut_de(Mom, Tid) || Mom <- Moms],
    ok.
ut_rel(Tid) ->
    AllRels = gmfMimXml:get_all_relations(Tid),
    lists:foreach(
      fun({{C1,C2}, Parent}) ->
	      Rec = #gmfMimRelation{child = {C2,C1}, parent = Parent},
	      ok = mnesia:write(Rec)
      end, AllRels),
    ok.
ut_de({{_Mom, _CxpProdId, _CxpRev} = MPR, Attrs}, Tid) ->
    %% This can happen in case of Library Moms
    case gmfMimXml:get_domain_extension(Tid, MPR) of
	[] ->
	    DEAttrs = [];
	[{_, DEAttrs}]  ->
	    %%= gmfMimXml:get_domain_extension(Tid, MPR),
	    DEAttrs
    end,
    ImmNsAttr = proplists:get_value(immNamespace, DEAttrs),
    MomName   = proplists:get_value(name, Attrs), %% == Mom

    case proplists:get_value(ecimMomName, DEAttrs, MomName) of
	MomName ->
	    ok;
	EcimMomName ->
	    case ets:insert_new(gmfMimMap, {EcimMomName, MomName}) of
		true -> ok;
		false ->
		    case ets:lookup(gmfMimMap, EcimMomName) of
			[{EcimMomName, MomName}] ->
			    ok;
			[{EcimMomName, OldMomName}] ->
			    sysInitI:error_msg("~p~n"
					       "Error in mapping~n"
					       "EcimMomName: ~p~n"
					       "OldMomName: ~p~n"
					       "MomName: ~p~n",
					       [?MODULE, EcimMomName,
						OldMomName, MomName]),
			    exit("Error in mapping")
		    end
	    end
    end,
    ImmNameSpace = choose(ImmNsAttr == "MOM_NAME", MomName, ""),
    %% These could be long lists we will see what happens
    Classes = gmfMimXml:get_all_classes(Tid, MPR),
    Structs = gmfMimXml:get_all_structs(Tid, MPR),
    DTypes  = gmfMimXml:get_all_derivedtypes(Tid, MPR),
    BiDirs  = gmfMimXml:get_all_bidirectionalassociations(Tid, MPR),
    lists:foreach(fun(Class) ->
			  ut_class(Class, MPR, ImmNameSpace)
		  end, Classes),
    lists:foreach(fun(Struct) ->
			  ut_struct(Struct, MPR, ImmNameSpace)
		  end, Structs),
    lists:foreach(fun(DType) -> ut_dtype(DType, MPR) end, DTypes),
    lists:foreach(fun(BiDir) -> ut_bidir(BiDir, MPR) end, BiDirs).

ut_class({ClassName, Attr}, {Mom, CxpProdId, CxpRev}, ImmNameSpace) ->
    Key = {Mom, ClassName},
    Root = proplists:is_defined(root, Attr),
    Attributes = [{N, A1} || {attribute, N, A1} <- Attr],
    Actions = [{N, A1} || {action, N, A1} <- Attr],
    CxpInfo = {CxpProdId, CxpRev},
    ok = mnesia:write(#gmfMimClass{key        = Key,
				   root       = Root,
				   attributes = Attributes,
				   actions    = Actions,
				   imm_ns     = ImmNameSpace,
				   cxp_info   = CxpInfo}).
ut_struct({StructName, Attr}, {Mom, _CxpProdId, _CxpRev}, ImmNameSpace) ->
    Key = {Mom, StructName},
    Attributes = [{N, A1} || {structMember, N, A1} <- Attr],
    ok = mnesia:write(#gmfMimStruct{key        = Key,
				    attributes = Attributes,
				    imm_ns     = ImmNameSpace}).

ut_dtype({DTypeName, Type}, {Mom, _CxpProdId, _CxpRev}) ->
    Key = {Mom, DTypeName},
    ok = mnesia:write(#gmfMimDerivedType{key      = Key,
					 basetype = Type}).

ut_bidir({{ReservingAssocEndName, _Class1, _Mim1, Props1},
	  {ReservedAssocEndName, Class2, _Mim2, Props2}},
	 {Mom, _CxpProdId, _CxpRev}) ->
    Key = {Mom, Class2},
    OldBidirList =
	case mnesia:wread({gmfMetaBiDir, Key}) of
	    [] ->
		[];
	    [#gmfMetaBiDir{bidir_list = M}] ->
		M
	end,

    ReservingAssocEnd = [{name, ReservingAssocEndName}|Props1],
    ReservedAssocEnd = [{name, ReservedAssocEndName}|Props2],
    BiDir = [{reservingAssocEnd, ReservingAssocEnd},
	     {reservedAssocEnd, ReservedAssocEnd}],
    NewBidirList = [BiDir|OldBidirList],
    ok = mnesia:write(#gmfMetaBiDir{mim_class  = Key,
				    bidir_list = NewBidirList}).

update_gmfCxpRev(CxpProdId, CxpRev, CxpPath, FileList) ->
    Key = {CxpProdId, CxpRev},
    GmfCxpRev = mnesia:wread({gmfCxpRev, Key}),
    NewGmfCxpRev = ucr(GmfCxpRev, Key, CxpPath, FileList),
    mnesia:write(NewGmfCxpRev).

ucr([], Key, CxpPath, FileList) ->
    #gmfCxpRev{key      = Key,
	       mim_info = FileList,
	       cxp_path = CxpPath};
ucr([GmfCxpRev], Key, _CxpPath, FileList) ->
    MimInfo = GmfCxpRev#gmfCxpRev.mim_info,
    NewMimInfo = get_mimInfo(Key, MimInfo, FileList),
    GmfCxpRev#gmfCxpRev{mim_info = NewMimInfo}.

get_mimInfo({CxpProdId, CxpRev}, MimInfo, FileList) ->
    try [case lists:member(X, MimInfo) of
	     true -> throw({error, X});
	     false -> ok
	 end||X<-FileList] of
	_ ->
	    lists:append(MimInfo, FileList)
    catch throw:{error, [File, Path, _]} ->
	    sysInitI:warning_msg(
	      "~w: ~s/~s in ~s (~s) is already registered~n",
	      [?MODULE, Path,File,CxpProdId,CxpRev]),
	    MimInfo
    end.

populate_parent() ->
    GmfMimClasses =
	[{gmfMimClass, Key} || Key <- mnesia:dirty_all_keys(gmfMimClass)],
    {ok, ObjResults} =
	sysUtil:parallel_call([{?MODULE, populate_parent, [Obj]}
			       || Obj <- GmfMimClasses]),
    [case ObjResult of
	 ok ->
	     ok
     end
     || ObjResult <- ObjResults],
    ok.

populate_parent({Tbl, Key}) ->
    case mnesia:dirty_read(Tbl, Key) of
	[#gmfMimClass{parent = []} = Rec] ->
	    Rels = mnesia:dirty_read(gmfMimRelation, Key),
	    Parent = lists:map(
		       fun(#gmfMimRelation{parent = P}) -> P end, Rels),
	    NewRec = Rec#gmfMimClass{parent = Parent},
	    ok = mnesia:dirty_write(NewRec);
	[#gmfMimClass{}] ->
	    ok
    end.

update_root_mos() ->
    GmfCxpRevs = mnesia:dirty_all_keys(gmfCxpRev),
    {ok, GmfCxpRevResults} =
	sysUtil:parallel_call([{?MODULE, update_root_mos, [GmfCxpRev]}
			       || GmfCxpRev <- GmfCxpRevs]),
    GmfCxpRevInfoList =
	[case GmfCxpRevResult of
	     {ok, GmfCxpRevInfo} ->
		 GmfCxpRevInfo
	 end
	 || GmfCxpRevResult <- GmfCxpRevResults],
    ?LOG_INFO(["------- CXP / Rev -------:" |
	       GmfCxpRevInfoList]),
    ok.

update_root_mos(Key) ->
    T0 = ?MonoTime,
    [#gmfCxpRev{key = CxpInfo,
		ldn = Ldn} = GmfCxpRev] = mnesia:dirty_read(gmfCxpRev, Key),

    Pattern = #gmfMimClass{cxp_info = CxpInfo,
			   root     = true,
			   _        = '_'},
    RootRecs = mnesia:dirty_match_object(Pattern),
    %% This gets complicated because a class can have multiple parents.
    %% This is allowed in MOM/ECIM design rules
    RootMos = lists:flatmap(fun(#gmfMimClass{key    = {_, Class},
					     parent = Parents}) ->
				    updrm(Parents, [list_to_binary(Class)])
			    end, RootRecs),

    mnesia:dirty_write(GmfCxpRev#gmfCxpRev{ldn = lists:append(Ldn, RootMos)}),
    {ok, {Key, sysUtil:time_to_string(?MonoTime - T0)}}.


updrm(Parents, MoAcc) ->
    List = lists:flatmap(fun(Parent) -> updrm_acc(Parent, MoAcc) end, Parents),
    lists:map(fun updrm/1, List).

%% ManagedElement is always there
updrm([<<"ManagedElement">>|_] = RootMo) ->
    RootMo.
%% This is a special. OamAccessPoint is an IMM class hanging under SysM.
%% It is used for OaM over TN and has a reference to Transport AddressIPv4
%% to find the IP address used for OaM.
updrm_acc({"SysM", _},  [<<"OamAccessPoint">>]) ->
    [
     [<<"ManagedElement">>,
      <<"SystemFunctions">>,
      <<"SysM">>,
      <<"OamAccessPoint">>]
    ];
updrm_acc({"ManagedElement", _},  RootMo) ->
    [
     [<<"ManagedElement">>|RootMo]
    ];
updrm_acc({Class, _}, RootMo) when Class == "Equipment";
				   Class == "SystemFunctions" ->
    [
     [<<"ManagedElement">>, list_to_binary(Class)|RootMo]
    ];
updrm_acc({Class, MomName}, RootMo) ->
    Key = {MomName, Class},
    case mnesia:dirty_read(gmfMimClass, Key) of
	[#gmfMimClass{parent = Parents}] ->
	    updrm(Parents, [list_to_binary(Class)|RootMo]);
	Data ->
	    sysInitI:error_msg("~p:updrm_acc()~n"
			       "Error with parents in MOM ~p~n"
			       "Class ~p~n"
			       "RootMo ~p~n"
			       "Data ~p~n",
			       [?MODULE, MomName, Class, RootMo, Data]),
	    exit("Error in MOM " ++ MomName)
    end.

update_mim_class() ->
    GmfMimClasses =
	[{gmfMimClass, Key} || Key <- mnesia:dirty_all_keys(gmfMimClass)],
    {ok, ObjResults} =
	sysUtil:parallel_call([{?MODULE, update_mim_class, [Obj]}
			       || Obj <- GmfMimClasses]),
    [case ObjResult of
	 ok ->
	     ok
     end
     || ObjResult <- ObjResults],
    ok.

update_mim_class({Tbl, Key}) ->
    case mnesia:dirty_read(Tbl, Key) of
	[#gmfMimClass{root = true}] ->
	    ok;
	[MimClass] ->
	    %% This is a non-root mimclass
	    %% Check if all parents is in the same CXP as the MIM
	    CheckParents = check_parents(MimClass),
	    umc(CheckParents, MimClass)
    end,
    ok.

check_parents(#gmfMimClass{cxp_info = CxpInfo,
			   parent   = Parents}) ->
    lists:all(fun({_C, M}) ->
		      CxpInfo == get_cxp_info(M)
	      end,
	      Parents).

umc(true, _) ->
    %% All parents is in the same CXP as the class
    %% Do nothing
    ok;
umc(false, #gmfMimClass{key = {Mom, ClassName} = Key,
			parent = Parents} = MimClass) ->
    %% There is at least one parent that is not in the same CXP as the class

    Dict =
	lists:foldl(
	  fun({C, M}, Acc) ->
		  case orddict:is_key(M, Acc) of
		      true ->
			  orddict:append(M, {C, M}, Acc);
		      false ->
			  orddict:store(M, [{C, M}], Acc)
		  end
	  end, orddict:store(Mom, [], orddict:new()), Parents),

    lists:foreach(
      fun({M, []}) when M == Mom ->
	      %% The original mnesia instance has no parents left
	      %% Delete that instance
	      sysInitI:info_msg("~p:umc()~n"
				"deleting key ~p from gmfMimClass~n",
				[?MODULE, Key]),
	      mnesia:dirty_delete({gmfMimClass, Key}),
	      ok;
	 ({M, NewParents}) ->
	      %% Create a key value. It could be the same as the original one.
	      NewCxpInfo = get_cxp_info(M),
	      NewKey = {M, ClassName},

	      case MimClass#gmfMimClass{key      = NewKey,
    					parent   = NewParents,
					cxp_info = NewCxpInfo} of
		  MimClass ->
		      %% No changes. Do Nothing
		      ok;
		  NewMimClass ->
		      %% This is either an update of the fields parent or cxp in
		      %% original instance or a new instance.
		      sysInitI:info_msg("~p:umc()~n"
					"writing key: ~p~n"
					"        cxp_info: ~p~n"
					"        parent: ~p to gmfMimClass~n",
					[?MODULE, NewKey, NewCxpInfo,
					 NewParents]),
		      mnesia:dirty_write(NewMimClass)
	      end
      end, orddict:to_list(Dict)),
    ok.

get_cxp_info(Mom) ->
    case mnesia:dirty_match_object(#gmfMimClass{key = {Mom, '_'},
						_   = '_'}) of
	[#gmfMimClass{cxp_info = CxpInfo}|_]  ->
	    CxpInfo;
	_ ->
	    erlang:error({mom_not_found, Mom}, [Mom])
    end.

%% Move this to gmfLib the new module
filename_join(CxpPath, RelPath, FileName) ->
%     AbsPath = lists:foldl(fun fj/2, CxpPath, string:tokens(RelPath, "/")),
%    filename:join([AbsPath, FileName]).
     WildPathName = filename:join([CxpPath, RelPath, FileName]),
     [AbsPathName] = filelib:wildcard(WildPathName),
     AbsPathName.

% fj(WildDir, Acc) ->
%     [AbsDir|_] = filelib:wildcard(filename:join([Acc, WildDir])),
%     AbsDir.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

choose(true,  T, _) -> T;
choose(false, _, F) -> F.

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
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

%%% ----------------------------------------------------------
find_optional_element(ElementName, Element)
  when is_record(Element, xmlElement) ->
    ContentList = Element#xmlElement.content,
    case lists:keyfind(ElementName, #xmlElement.name, ContentList) of
	false ->
	    false;
	Value  ->
	    Value
    end.

%%% ----------------------------------------------------------
find_elements(_, false) ->
    [];
find_elements(ElementName, Element) when is_record(Element, xmlElement) ->
    find_elements(ElementName, Element#xmlElement.content);
find_elements(ElementName, ContentList) ->
    case sysUtil:keyfind_all(ElementName, #xmlElement.name, ContentList) of
	false ->
	    [];
	Elems ->
	    Elems
    end.

%%% ----------------------------------------------------------
create_mim_map() ->
    try ets:new(gmfMimMap, [public, set, named_table]) of
	gmfMimMap ->
	    case sysUtil:parallel_call_parent() of
		Pid when is_pid(Pid) ->
		    ets:setopts(gmfMimMap, {heir, Pid, ?MODULE});
		_ ->
		    ok
	    end;
	_ ->
	    ok
    catch
	error : badarg ->
	    ok
    end.

delete_mim_map() ->
    receive
	{'ETS-TRANSFER', gmfMimMap, FromPid, HeirData} ->
	    ?LOG_INFO([{'ETS-TRANSFER', gmfMimMap},
		       {fromPid, FromPid},
		       {heirData, HeirData}]),
	    delete_mim_map()
    after
	0 ->
	    ets:delete(gmfMimMap),
	    ok
    end.

update_mim_map() ->
    GmfMimClasses =
	[{gmfMimClass, Key} || Key <- mnesia:dirty_all_keys(gmfMimClass)],
    GmfMimStructs =
	[{gmfMimStruct, Key} || Key <- mnesia:dirty_all_keys(gmfMimStruct)],
    {ok, ObjResults} =
	sysUtil:parallel_call([{?MODULE, update_mim_map, [Obj]}
			       || Obj <- GmfMimClasses ++ GmfMimStructs]),
    [case ObjResult of
	 ok ->
	     ok
     end
     || ObjResult <- ObjResults],
    ok.

update_mim_map({Tbl, Key}) ->
    case mnesia:dirty_read(Tbl, Key) of
	[#gmfMimClass{attributes = Attributes} = GMC] ->
	    NewAttributes = get_mapped_attributes(Attributes),
	    case GMC#gmfMimClass{attributes = NewAttributes} of
		GMC ->
		    ok;
		NewGmc ->
		    mnesia:dirty_write(NewGmc)
	    end;
	[#gmfMimStruct{attributes = Attributes} = GMS] ->
	    NewAttributes = get_mapped_attributes(Attributes),
	    case GMS#gmfMimStruct{attributes = NewAttributes} of
		GMS ->
		    ok;
		NewGms ->
		    mnesia:dirty_write(NewGms)
	    end
    end,
    ok.

get_mapped_attributes(Attributes) ->
    lists:map(
      fun({N, A}) ->
	      A1 = [get_mapped_attribute(Attribute) || Attribute <- A],
	      {N, A1}
      end, Attributes).

get_mapped_attribute({dataType, DataType}) ->
    {dataType, get_mapped_dataType(DataType)};
get_mapped_attribute(Attribute) ->
    Attribute.

get_mapped_dataType({sequence, DataType}) ->
    {sequence, get_mapped_dataType(DataType)};
get_mapped_dataType({moRef, ClassName, MimName}) ->
    {moRef, ClassName, get_mapped_mimName(MimName)};
get_mapped_dataType({structRef, StructName, MimName}) ->
    {structRef, StructName, get_mapped_mimName(MimName)};
get_mapped_dataType({derivedDataTypeRef, DType, MimName}) ->
    {derivedDataTypeRef, DType, get_mapped_mimName(MimName)};
get_mapped_dataType(DataType) ->
    DataType.

get_mapped_mimName(MimName) ->
    case ets:lookup(gmfMimMap, MimName) of
	[] -> MimName;
	[{MimName, MappedMimName}] ->
	    MappedMimName
    end.

update_meta_bidir() ->
    GmfMetaBiDirs =
	[{gmfMetaBiDir, Key} || Key <- mnesia:dirty_all_keys(gmfMetaBiDir)],
    {ok, ObjResults} =
	sysUtil:parallel_call([{?MODULE, update_meta_bidir, [Obj]}
			       || Obj <- GmfMetaBiDirs]),
    [case ObjResult of
	 ok ->
	     ok
     end
     || ObjResult <- ObjResults],
    ok.

update_meta_bidir({Tbl, Key}) ->
    [#gmfMetaBiDir{bidir_list = OldBidirList} = MBD] =
	mnesia:dirty_read(Tbl, Key),
    NewBidirList =
	lists:map(
	  fun(BiDir) ->
		  [{reservingAssocEnd, include_scope(reservingAssocEnd, BiDir)},
		   {reservedAssocEnd, include_scope(reservedAssocEnd, BiDir)}]
	  end, OldBidirList),
    NewMBD = MBD#gmfMetaBiDir{bidir_list = NewBidirList},
    ok = mnesia:dirty_write(NewMBD).

include_scope(AssocEndType, BiDir) ->
    AssocEnd = proplists:get_value(AssocEndType, BiDir),
    {ClassName, MimName} = proplists:get_value(hasClass, AssocEnd),
    MappedMimName = get_mapped_mimName(MimName),
    Key = {MappedMimName, ClassName},
    IsScoped =
	case mnesia:dirty_read(gmfMimClass, Key) of
	    [] ->
		%% Must likely this bidir points to a non-GMF model
		false;
	    [#gmfMimClass{imm_ns = ImmNs}] ->
		ImmNs /= ""
	end,
    [{isScoped, IsScoped}|AssocEnd].

populate_dn_map() ->
    Keys = mnesia:dirty_all_keys(gmfMimClass),
    sysUtil:parallel_call([{?MODULE, populate_dn_map, [Key]} || Key <- Keys]),
    ok.

populate_dn_map(MimClass) ->
    DnMap = #gmfDnMap{mim_class = MimClass},
    {MimName, ClassName} = MimClass,
    List = populate_dn_map(DnMap, [{ClassName, MimName}]),
    [mnesia:dirty_write(Rec) || Rec <- List].

populate_dn_map(#gmfDnMap{mim_class = {"ECIM_Top", "ManagedElement"}} = Rec,
		_) ->
    [Rec#gmfDnMap{mim_names = [<<"ManagedElement">>]}];
populate_dn_map(#gmfDnMap{mim_class = {"ComTop", "ManagedElement"}} = Rec,
		_) ->
    [Rec#gmfDnMap{mim_names = [<<"ManagedElement">>]}];
populate_dn_map(#gmfDnMap{mim_class = MimClass} = Rec,
		Keys) ->
    {_, ClassName} = MimClass,
    DnMapData = get_dnMap_data(MimClass, Keys),
    Parents = maps:get(parents, DnMapData),
    ImmRdn = maps:get(imm_rdn, DnMapData),

    F = fun({ParentClassName, ParentMimName} = Key) ->
		ParentMinClass = {ParentMimName, ParentClassName},
		ParentDnMap = #gmfDnMap{mim_class = ParentMinClass},

		NewKeys = [Key|Keys],
		DnMaps = populate_dn_map(ParentDnMap, NewKeys),
		lists:map(
		  fun(#gmfDnMap{mim_names = MimNames,
				imm_names = ImmNames}) ->
			  NewMimNames = [list_to_binary(ClassName)|MimNames],
			  NewImmNames = [list_to_binary(ImmRdn)|ImmNames],
			  Rec#gmfDnMap{mim_names = NewMimNames,
				       imm_names = NewImmNames}
		  end, DnMaps)
	end,

    lists:flatmap(F, Parents).

%% Some hardcoded parents
get_dnMap_data({"RcsOamAccessPoint", "OamAccessPoint"}, _Keys) ->
    #{parents => [{"SysM", "RcsSysM"}],
      imm_rdn => "oamAccessPointId"};
get_dnMap_data({"RcsSysM","SysM"}, _Keys) ->
    #{parents => [{"SystemFunctions", "ECIM_Top"}],
      imm_rdn => "sysMId"};
get_dnMap_data({"ECIM_Top","SystemFunctions"}, _Keys) ->
    #{parents => [{"ManagedElement", "ECIM_Top"}],
      imm_rdn => "systemFunctionsId"};
get_dnMap_data(Key, Keys) ->
    Pattern = #gmfMimClass{key     = Key,
			   parent  = '$1',
			   imm_rdn = '$2',
			   _       = '_'},
    [[Parents, ImmRdn]] = ets:match(gmfMimClass, Pattern),

    %% Remove all the parents that have been used in an earlier phase to
    %% avoid loops
    FilteredParents =
	lists:filter(fun(P) -> not lists:member(P, Keys) end, Parents),

    #{parents => FilteredParents,
      imm_rdn => ImmRdn}.

populate_mim_children() ->
    Keys = mnesia:dirty_all_keys(gmfMimClass),
    [populate_mim_children(MimClass) || MimClass <- Keys],
    ok.

populate_mim_children(MimClass) ->
    Pattern = #gmfMimClass{key     = MimClass,
			   parent  = '$1',
			   _       = '_'},
    [[Parents]] = ets:match(gmfMimClass, Pattern),

    lists:foreach(
      fun({ParentClassName, ParentMimName}) ->
	      ParentMimClass = {ParentMimName, ParentClassName},
	      Res = mnesia:dirty_read(gmfMimChildren, ParentMimClass),
	      Rec = get_mim_children(ParentMimClass, MimClass, Res),
	      ok = mnesia:dirty_write(Rec)
      end, Parents).

get_mim_children(ParentMimClass, MimClass, []) ->
    #gmfMimChildren{key      = ParentMimClass,
		    children = [MimClass]};
get_mim_children(_ParentMimClass, MimClass, [Rec]) ->
    OldChildren = Rec#gmfMimChildren.children,
    NewChildren = [MimClass|OldChildren],
    Rec#gmfMimChildren{children = NewChildren}.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
