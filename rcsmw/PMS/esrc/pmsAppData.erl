%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsAppData.erl %
%%% 	
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pmsAppData).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/1').
-date('2016-06-13').
-author('eolaand').
-shaid('e05b26c634a4814bf5a953024a2cd07636dc5157').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R1A/1      2012-07-03 uabesvi     added basic functionality
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([appdata/3]).
-export([appdata_complete/0]).

-export([get_counter_names/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("pms.hrl").
-include("RcsPm.hrl").
-include_lib("xmerl/include/xmerl.hrl").

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
%%   Use AppDataE to store mim information in #gmfMoClass{}
%%   and #gmfCxpRev{} tables.
%% @end
%%===========================================================================
appdata(CxpProdId, 
	CxpVersion, 
	#xmlElement{attributes = Attrs,
		    content    = Content} = AppDataE) ->

    %% count a hash for the appdata, used as version for the PM Groups
    Bin      = term_to_binary(AppDataE),
    BinHash  = crypto:hash(sha, Bin),
    ListHash = binary_to_list(base64:encode(BinHash)),

    {ok, Target} = get_target(Attrs),

%%=======================================================================
%%     Do not add ?LOG_RAM entries into this module
%%     It will crash in some escript that is run offline.
%%     ?LOG_RAM(?SEV_5, {"appdata ~p ~p~n"
%% 		  "  Target = ~p~n", [CxpProdId, CxpVersion, Target]}),
%%=======================================================================

    case Target of
	"pms" ->
	    pms_groups(Content, ListHash, CxpProdId, CxpVersion);
	"pmsCounter" ->
	    pms_groups(Content, ListHash, CxpProdId, CxpVersion);
	"pmsJob" ->
	    pms_jobs(Content);
	"pmsGroupAlias" ->
	    pms_aliases(Content)
    end.


pms_groups(Content, ListHash, CxpProdId, CxpVersion) ->
    PmGrpCont = get_grp(Content, [pmGroup], []),
    PmGrp     = get_elements(PmGrpCont, record_info(fields, pmGroup), []),
    MT        = get_elements(PmGrpCont, [measurementType], []),
    save_grp(lists:zip(PmGrp, MT), ListHash),
    save_apps_info(CxpProdId, CxpVersion, PmGrp),
    ok.


pms_jobs(Content) ->
    PmJobCont = get_grp(Content, [pmJob], []),
    PmJob     = get_elements(PmJobCont, record_info(fields, pmJob), []),
    MR        = get_elements(PmJobCont, [measurementReader], []),
    save_job(lists:zip(PmJob, MR)),
    ok.


pms_aliases(Content) ->
    {ok, MapId} = get_map_id(Content),
    Aliases     = get_grp(Content, [pmGroupAlias], []),

    save_alias(Aliases, MapId, []),
    ok.



%%===========================================================================
%% @spec appdata_complete() 
%%
%%       -> ok
%%
%% where
%%
%% @doc
%%   appdata files for applications have been reveived. 
%% @end
%%===========================================================================
appdata_complete() ->
    get_counter_names(),
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


get_elements([], _, Acc) ->
    Acc;
get_elements([{_, E} | T], Elements, Acc) ->
    Res = ge(E, Elements, []),
    get_elements(T, Elements, [Res | Acc]).


ge([], _, Acc) ->
    Acc;
ge([#xmlElement{name       = Name,
		attributes = [],
	        content    = Content} | T],
   Elements,
   Acc) when Name == moClass ->
    [MC] = get_elements([{undefined, Content}],
			record_info(fields, 'ManagedObjectClass'),
			[]),
    AccN = pmsLib:choose(lists:member(Name, Elements), [{Name, MC} | Acc], Acc),
    ge(T, Elements, AccN);
ge([#xmlElement{name       = Name,
		attributes = Attr} | T],
   Elements,
   Acc) when Name == moClass ->
    MOC  = get_attrs(Attr, []),
    AccN = pmsLib:choose(lists:member(Name, Elements), [{Name, MOC} | Acc], Acc),
    ge(T, Elements, AccN);
ge([#xmlElement{name    = Name,
		content = Content} | T],
   Elements,
   Acc) when Name == measurementType ->
    [MT] = get_elements([{undefined, Content}],
			record_info(fields, measurementType),
			[]),
    AccN = pmsLib:choose(lists:member(Name, Elements), [{Name, MT} | Acc], Acc),
    ge(T, Elements, AccN);
ge([#xmlElement{name    = Name,
		content = Content} | T],
   Elements,
   Acc) when Name == measurementReader;
	     Name == measurementReaderNameValue;
	     Name == measurementSpecification ->
    [Rec] = get_elements([{undefined, Content}],
			ri(Name),
			[]),
    AccN = pmsLib:choose(lists:member(Name, Elements), [{Name, Rec} | Acc], Acc),
    ge(T, Elements, AccN);
ge([#xmlElement{name    = Name,
		content = [#xmlText{} | _] = Content} | T],
   Elements,
   Acc) ->
    Vals = lists:append([V || #xmlText{value = V} <- Content]),
    Val  = string:strip(Vals, both, 34),
    AccN = pmsLib:choose(lists:member(Name, Elements), [{Name, Val} | Acc], Acc),
    ge(T, Elements, AccN);
ge([_|T], Elements, Acc) ->
    ge(T, Elements, Acc).


ri(measurementReader) ->
    record_info(fields, measurementReader);
ri(measurementReaderNameValue) ->
    record_info(fields, 'MeasurementReaderNameValue');
ri(measurementSpecification) ->
    record_info(fields, 'MeasurementSpecification').

%%===========================================================================
%%  get_attrs(XmlElements, Find, Acc)
%%    
%%       -> Acc
%%
%% where
%%
%%   XmlElements = [#xmlElement{}]
%%   Find        = [atom()]
%%   Acc         = [{key, value}]
%%   
%% Description: 
%%    First dig down to the #xmlElement with name as defined by Find. 
%%    Return #xmlElement.attributes for the last record.
%%    
%%===========================================================================
get_attrs([], Acc) ->
    Acc;
get_attrs([#xmlAttribute{name  = Name,
			 value = Value} | T], 
	  Acc) ->
    get_attrs(T, [{Name, Value} | Acc]).


%%===========================================================================
%%  get_grp(XmlElements, Find, Acc)
%%    
%%       -> Acc
%%
%% where
%%
%%   XmlElements = [#xmlElement{}]
%%   Find        = [atom()]
%%   Acc         = [{key, value}]
%%   
%% Description: 
%%    First dig down to the #xmlElement with name as defined by Find. 
%%    Return #xmlElement.attributes for the last record.
%%    
%%===========================================================================
get_grp([], _, Acc) ->
    Acc;
get_grp([#xmlElement{name       = Find,
		     attributes = Attrs,
		     content    = Cont} | T], 
	[Find],
	Acc) ->
    get_grp(T, [Find], [{Attrs, Cont} | Acc]);
get_grp([#xmlElement{name    = Find,
		     content = Content} | _], 
	[Find | FT],
	Acc) ->
    get_grp(Content, FT, Acc);
get_grp([_|T], Find, Acc) ->
    get_grp(T, Find, Acc).


%%===========================================================================
%% save_grp([{Attr, MoClass, [MeasType]}], GroupVsn)
%%    
%%       -> Acc
%%
%% where
%%
%%   Attr     = [{Key, Value}]
%%   MoClass  = [{Key, Value}]
%%   MeasType = [{Key, Value}]
%%   
%% Description: 
%%    
%%    
%%    
%%===========================================================================
save_grp([], _) ->
    ok;
save_grp([{PmGroup, MeasType} | T], GrpVsn) ->
    sg_group(PmGroup, GrpVsn),
    sg_meas_type(MeasType, gv(pmGroupId, PmGroup)),
    save_grp(T, GrpVsn).


sg_group(PmGroup, GrpVsn) ->
    PmGrpId   = gv(pmGroupId, PmGroup),
    PmGrpKeys = record_info(fields, pmGroup),
    PmGrpAttr = [gv(K, PmGroup) || K <- PmGrpKeys],
    PmGrp     = list_to_tuple([pmGroup | PmGrpAttr]),

    MoClassKeys = record_info(fields, 'ManagedObjectClass'),
    MoClass     = gv(moClass, PmGroup),
    MoCAttr     = [gv(K, MoClass) || K <- MoClassKeys],
    MoC         = list_to_tuple(['ManagedObjectClass' | MoCAttr]),
    Descr       = gv(description,  PmGroup, ""),

    PmGrpIdNew  = {"1", "1", "1", PmGrpId},
    PmGrpTrans  = transform(PmGrp#pmGroup{pmGroupId      = PmGrpIdNew,
					  pmGroupVersion = GrpVsn,
					  moClass        = MoC,
					  description    = Descr}, 
			    ?pmGroup_types),

    is_ok(pmsDb:pm_group_set(PmGrpTrans), {pmGroup, PmGrpIdNew}).


sg_meas_type([], _) ->
    ok;
sg_meas_type([{_, MeasType} | T], PmGroupId) ->
    MTId      = gv(measurementTypeId, MeasType),
    MeasTKeys = record_info(fields, measurementType),
    MeasTAttr = [gv(K, MeasType) || K <- MeasTKeys],
    MT        = list_to_tuple([measurementType | MeasTAttr]),
    IsCompr   = gv(isCompressed, MeasType, "false"),
    Descr     = gv(description,  MeasType, ""),
    MTIdNew   = {"1", "1", "1", PmGroupId, MTId},
    MTNew     = MT#measurementType{measurementTypeId = MTIdNew,
				   isCompressed      = IsCompr,
				   description       = Descr},
    MTTrans   = transform(MTNew, ?measurementType_types),
    
    is_ok(pmsDb:measurement_type_set(MTTrans), {measurementType, MTIdNew}),
    sg_meas_type(T, PmGroupId).



transform(Rec, TypesList) ->
    [Name | Attrs] = tuple_to_list(Rec),
    Types = [Type || {_, Type} <- TypesList],
    list_to_tuple([Name] ++ trans(Types, Attrs, [])).


trans([], [], Acc) ->
    r(Acc);
trans([_ | TT], [undefined | T], Acc) ->
    trans(TT, T, [undefined | Acc]);
trans([{struct, 'MeasurementSpecification'} | TT], [H | T], Acc) ->
    trans(TT, T, [trans_ms(H, #'MeasurementSpecification'{}) | Acc]);
trans([{struct, _} | TT], [H | T], Acc) ->
    trans(TT, T, [H | Acc]);
trans([{sequence, Seq} | TT], [H | T], Acc) ->
    S = trans_seq(Seq, H),
    trans(TT, T, [S | Acc]);
trans([Fnc | TT], [H | T], Acc) ->
    trans(TT, T, ['RcsPm':Fnc(H) | Acc]).

trans_seq({struct, 'MeasurementReaderNameValue'}, H) ->
    [ts_mrnv(MRNV, #'MeasurementReaderNameValue'{}) || MRNV <- H];
trans_seq('RcsPm.MoFilter', H) ->
    [H].


ts_mrnv([], Rec) -> 
    Rec;
ts_mrnv([{currentValue, V} | T], Rec) ->
    ts_mrnv(T, Rec #'MeasurementReaderNameValue'{currentValue = V});
ts_mrnv([{lastUpdated, V} | T], Rec) ->
    ts_mrnv(T, Rec #'MeasurementReaderNameValue'{lastUpdated = V});
ts_mrnv([{moClassInstance, V} | T], Rec) ->
    ts_mrnv(T, Rec #'MeasurementReaderNameValue'{moClassInstance = V});
ts_mrnv([{suspectFlag, V} | T], Rec) ->
    ts_mrnv(T, Rec #'MeasurementReaderNameValue'{suspectFlag = V}).




trans_ms([], Rec) ->
    Rec;
trans_ms([{groupRef, V} | T], Rec) ->
    BV = list_to_binary(V),
    trans_ms(T, Rec#'MeasurementSpecification'{groupRef = BV});
trans_ms([{measurementTypeRef, V} | T], Rec) ->
    BV = list_to_binary(V),
    trans_ms(T, Rec#'MeasurementSpecification'{measurementTypeRef = BV}).


%%===========================================================================
%% save_job([{PmJob, MeasReader}])
%%    
%%       -> Acc
%%
%% where
%%
%%   
%% Description: 
%%    
%%    
%%    
%%===========================================================================
save_job([]) ->
    ok;
save_job([{PmJob, MeasReader} | T]) ->
    sj_job(PmJob),
    sj_meas_reader(MeasReader, gv(pmJobId, PmJob)),
    save_job(T).


sj_job(PmJob) ->
    PmJobId   = gv(pmJobId, PmJob),
    PmJobKeys = record_info(fields, pmJob),
    PmJobAttr = [gv(K, PmJob) || K <- PmJobKeys],
    PmJobT    = list_to_tuple([pmJob | PmJobAttr]),

    PmJobIdNew  = {"1", "1", "1", PmJobId},
    PmJobTrans  = #pmJob{requestedJobState = RS} = 
	transform(PmJobT#pmJob{pmJobId = PmJobIdNew}, ?pmJob_types),
    ReqState    = pmsLib:choose(RS == undefined, ?JobState_ACTIVE, RS),

    PmJobTrans2 = PmJobTrans#pmJob{currentJobState   = ?JobState_STOPPED,
				   requestedJobState = ReqState,
				   jobControl        = ?JobControl_STARTSTOP},

    is_ok(pmsDb:pm_job_set(PmJobTrans2), {pmJob, PmJobIdNew}).


sj_meas_reader([], _) ->
    ok;
sj_meas_reader([{_, MeasReader} | T], PmJobId) ->
    MRId      = gv(measurementReaderId, MeasReader),
    MeasRKeys = record_info(fields, measurementReader),
    MeasRAttr = [gv(K, MeasReader) || K <- MeasRKeys],
    MR        = list_to_tuple([measurementReader | MeasRAttr]),
    %% special sequence handling for measurementReaderNameValue
    MRNV      = gav(measurementReaderNameValue, MeasReader),

    MRIdNew   = {"1", "1", "1", PmJobId, MRId},
    sj_mr_mo_instances(MR, MRIdNew),
    MRNew     = MR#measurementReader{measurementReaderId        = MRIdNew,
				     measurementReaderNameValue = MRNV,
				     moInstances                = undefined},
    MRTrans   = transform(MRNew, ?measurementReader_types),
 
    is_ok(pmsDb:measurement_reader_set(MRTrans), {measurementReader, MRIdNew}),
    sj_meas_reader(T, PmJobId).



sj_mr_mo_instances(#measurementReader{moInstances = undefined}, _) ->
    ok;
sj_mr_mo_instances(#measurementReader{moInstances = MoI}, Mid) ->
    ?LOG_RAM(?SEV_WARNING, 
	     {"moInstances are not supported. Discarding data.~n"
	      "measurementReaderId = ~p~n"
	      "moInstances         = ~p~n", 
	      [Mid, MoI]}),
    sysInitI:warning_msg("moInstances are not supported. Discarding data.~n"
			 "measurementReaderId = ~p~n"
			 "moInstances         = ~p~n", 
			 [Mid, MoI]),
    ok.


%%===========================================================================
%% save_apps_info()
%%    
%%       -> 
%%
%% where
%%
%%   
%% Description: 
%%    
%%    
%%    
%%===========================================================================
save_apps_info(CxpProdId, CxpVersion, PmGrps) ->
    Pids  = [proplists:get_value(pmGroupId, PG) || PG <- PmGrps],
    PgIds = [{"1", "1", "1", Pid} || Pid <- Pids],
    Res = pmsDb:apps_info_set(#pmsAppsInfo{key    = {CxpProdId, CxpVersion},
					   pg_ids = PgIds,
					   state  = ?APP_ACTIVE}),
    is_ok(Res, {pmsAppsInfo, CxpProdId, CxpVersion}).



%%===========================================================================
%% save_alias([{Attr, MoClass, [MeasType]}], Hash)
%%    
%%       -> Acc
%%
%% where
%%
%%   Attr     = [{Key, Value}]
%%   MoClass  = [{Key, Value}]
%%   MeasType = [{Key, Value}]
%%   
%% Description: 
%%    
%%    
%%    
%%===========================================================================
save_alias([], MapId, Acc) ->
    pmsDb:pms_counter_aliases_set(MapId, Acc),
    ok;
save_alias([{_, Elements} | T], MapId, Acc) ->
    %%pmsDebug:tpl(r, pesAppData, get_grp),
    {ok, Aliases} = get_grp_alias(Elements, undefined, undefined, []),

    save_alias(T, MapId, sa(Aliases, Acc)).
    
sa({GrpId, GrpAlias, TypeList}, Acc) ->
    Types = [{{GrpAlias, TypeAlias}, {to_binary(GrpId), to_binary(TypeId)}} || 
		{TypeId, TypeAlias} <- TypeList],
    lists:append([[{GrpAlias, to_binary(GrpId)}], Types, Acc]). 


%%===========================================================================
%%  get_map_id(XmlElements)
%%    
%%       -> Acc
%%
%% where
%%
%%   XmlElements = [#xmlElement{}]
%%   Find        = [atom()]
%%   Acc         = [{key, value}]
%%   
%% Description: 
%%    First dig down to the #xmlElement with name as defined by Find. 
%%    Return #xmlElement.attributes for the last record.
%%    
%%===========================================================================
get_map_id([]) ->
    {error, no_map_id_defind};
get_map_id([#xmlElement{name    = counterMapId,
			content = [#xmlText{value = MapId}]} | _]) ->
    {ok, MapId};
get_map_id([_|T]) ->
    get_map_id(T).


%%===========================================================================
%%  get_grp_alias(XmlElements)
%%    
%%       -> Acc
%%
%% where
%%
%%   XmlElements = [#xmlElement{}]
%%   Find        = [atom()]
%%   Acc         = [{key, value}]
%%   
%% Description: 
%%    First dig down to the #xmlElement with name as defined by Find. 
%%    Return #xmlElement.attributes for the last record.
%%    
%%===========================================================================
get_grp_alias([], undefined, _GrpAlias, Types) ->
    {error, {no_group_id_defined, no_type_id_defined, Types}};
get_grp_alias([], GrpId, undefined, Types) ->
    {error, {GrpId, no_type_id_defined, Types}};
get_grp_alias([], GrpId, GrpAlias, Types) ->
    {ok, {GrpId, GrpAlias, Types}};
get_grp_alias([#xmlElement{name    = pmGroupId,
			   content = [#xmlText{value = GrpId}]} | T],
	      _GrpId, 
	      GrpAlias, 
	      Types) ->
    get_grp_alias(T, GrpId, GrpAlias, Types);
get_grp_alias([#xmlElement{name    = groupAlias,
			   content = [#xmlText{value = GrpAlias}]} | T],
	      GrpId, 
	      _GrpAlias, 
	      Types) ->
    get_grp_alias(T, GrpId, list_to_integer(GrpAlias), Types);
get_grp_alias([#xmlElement{name    = measurementTypeAlias,
			   content = Content} | T],
	      GrpId,
	      GrpAlias,
	      Types) ->
    get_grp_alias(T, 
		  GrpId, 
		  GrpAlias, 
		  get_type_alias(Content, undefined, Types));
get_grp_alias([_|T], GrpId, GrpAlias, Types) ->
    get_grp_alias(T, GrpId, GrpAlias, Types).


%%===========================================================================
%%  get_type_alias(XmlElements)
%%    
%%       -> Acc
%%
%% where
%%
%%   XmlElements = [#xmlElement{}]
%%   Find        = [atom()]
%%   Acc         = [{key, value}]
%%   
%% Description: 
%%    First dig down to the #xmlElement with name as defined by Find. 
%%    Return #xmlElement.attributes for the last record.
%%    
%%===========================================================================
get_type_alias([], _, Acc) ->
    Acc;
get_type_alias([#xmlElement{name    = measurementTypeId,
			    content = [#xmlText{value = TypeId}]} | T],
	       undefined,
	       Acc) ->
    get_type_alias(T, TypeId, Acc);
get_type_alias([#xmlElement{name    = typeAlias,
			    content = [#xmlText{value = TypeAlias}]} | T],
	       TypeId,
	       Acc) ->
    get_type_alias(T, undefined, [{TypeId, list_to_integer(TypeAlias)} | Acc]);
get_type_alias([_|T], Res, Acc) ->
    get_type_alias(T, Res, Acc).


%%===========================================================================
%%  get_target(XmlElements)
%%    
%%       -> Acc
%%
%% where
%%
%%   XmlElements = [#xmlElement{}]
%%   
%% Description: 
%%    First dig down to the 'target' #xmlElement. 
%%    Return value of the target attribute.
%%    
%%===========================================================================
get_target([]) ->
    {error, no_taget};
get_target([#xmlAttribute{value = Target} | _]) ->
    {ok, Target};
get_target([_ | T]) ->
    get_target(T).




%%===========================================================================
%% is_ok(Res, Info)
%%    
%%       -> ok | erlang:error/2
%%
%% where
%%
%%   
%% Description: 
%%    
%%    
%%    
%%===========================================================================
is_ok(Res, Info) ->
    case Res of
	ok    -> ok;
	Error -> erlang:error(Error, [Info])
    end.





get_counter_names() ->
    [] = ets:foldl(fun gcn/2, [], pmGroup).

gcn(#pmGroup{pmGroupId = {_, _, _, GroupName} =  Gid,
	     moClass = #'ManagedObjectClass'{moClassName = MoClassName}},
    Acc) ->
    {ok, MT} = pmsDb:measurement_type_dirty_match(Gid),
    Fun = fun(#measurementType{measurementTypeId = {_, _, _, _, MN}}, N) -> 
		  [MN | N]
	  end, 
    Rec = #pmsScMoClasses{mo_class = MoClassName, 
			  pm_group = GroupName,
			  counters = lists:foldl(Fun, [], MT)},
    case pmsDb:pms_mo_class_set(Rec) of
	ok    -> Acc; 
	Error -> [{Gid, Error} | Acc]
    end.


%%===========================================================================
%% Misc functions
%%===========================================================================
gv(Key, List) ->
    proplists:get_value(Key, List).
gv(Key, List, Def) ->
    proplists:get_value(Key, List, Def).

gav(Key, List) ->
    proplists:get_all_values(Key, List).

%% f(L) -> lists:flatten(L).
r(L) -> lists:reverse(L).


to_binary(Term) ->    
    pmsLib:to_binary(Term).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

