%%% ----------------------------------------------------------
%%% %CCaseFile:	pesAppData.erl %
%%% Author:	etxbjca
%%% Description:
%%%
%%% Parse all PmEvent appdata files and create corresponding
%%% MO instances.
%%% ----------------------------------------------------------
-module(pesAppData).
-vsn('/main/R3A/R4A/R5A/R11A/1').
-date('2017-09-04').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/2      2014-10-27 etxjotj     First real version
%%% R4A/5      2015-09-08 uabesvi     error_logger -> sysInitI
%%% R5A/1      2016-01-26 uabesvi     fixes for predefined jobs
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% MODULE INTERFACE
%%% ----------------------------------------------------------
-export([appdata/3]).
-export([appdata_complete/0]).

%%% ----------------------------------------------------------
%%% OTHER EXPORTED FUNCTIONS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% HEADER FILES AND MACRO DEFINITIONS
%%% ----------------------------------------------------------
%%-include("comte_types.hrl").
-include("RcsPMEventM.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%% ----------------------------------------------------------
%%% TYPES AND RECORDS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% FUNCTIONS
%%% ----------------------------------------------------------
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
%%   Use AppDataE to store mim information in eventGroup, eventType,
%%   eventProducer (incl all capabilities) and eventJob records.
%% @end
%%===========================================================================
appdata(CxpProdId,
	CxpVersion,
	#xmlElement{attributes = Attrs,
		    content    = Content} = AppDataE) ->
    %%io:format("~n~n#####AppDataE  ~p~n", [AppDataE]),

    %% count a hash for the appdata, used as version for the PM Groups
    Bin      = term_to_binary(AppDataE),
    BinHash  = crypto:hash(sha, Bin),
    ListHash = binary_to_list(base64:encode(BinHash)),

    %%?LOG_MISC("appdata ~p ~p~n", [CxpProdId, CxpVersion]),

    {ok, Target} = get_target(Attrs),

    case Target of
	"pmEvent" ->
	    pm_event(Content, ListHash);
	"pmEventFilter" ->
	    pm_filter(Content);
	"pmEventProducer" ->
	    %% mount -t tmpfs -o size=2m,mode=0777 tmpfs /tmp/applicationtmp/nisse
	    %% mount -o remount,size=2m tmpfs /tmp/applicationtmp/nisse
	    %% swmI:remount_tmpfs_size_cmd("10k", "/tmp/applicationtmp/nisse")
%% 	    sysInitI:info_msg("~p: CxpProdId = ~p, CxpVersion = ~p~n"
%% 				  "AppData = ~p", 
%% 				  [?MODULE, CxpProdId, CxpVersion, AppDataE]),
	    pm_producer(Content, {CxpProdId, CxpVersion});
	"pmEventAlias" ->
	    pm_alias(Content);
	"pmEventJob" ->
	    pm_job(Content)
    end,
    ok.


%%===========================================================================
%% @spec appdata_complete() 
%%
%%       -> ok
%%
%% where
%%
%% @doc
%%   all appdata files for applications have been reveived. 
%% @end
%%===========================================================================
appdata_complete() ->
    ok.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%===========================================================================
%%  pm_event(Content)
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
%%    
%%    Find all eventGroup and eventType specifications
%%    and store them in the MO tree.
%%    
%%===========================================================================
pm_event(Content, ListHash) ->
    EventCont = get_grp(Content, [eventGroup], []),
    Attr      = record_info(fields, eventGroup) ++ [eventProducer],
    PmEv      = get_elements(EventCont, Attr, []),
    Type      = get_elements(EventCont, [eventType], []),
    save_grp(lists:zip(PmEv, Type), ListHash),
    ok.


%%===========================================================================
%%  pm_filter(Content)
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
%%    
%%    Find all eventFilter specifications and store them in the MO tree.
%%    
%%===========================================================================
pm_filter(Content) ->
    FilterCont = get_grp(Content, [eventFilter], []),
    Attr       = record_info(fields, eventFilterType) ++ [eventProducer],
    Elem       = get_elements(FilterCont, Attr, []),
    save_filter(Elem),
    ok.


%%===========================================================================
%%  pm_producer(Content, CXP)
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
%%    
%%   Find all eventProducer specifications and its capabilities
%%   store them in the MO tree 
%%    
%%===========================================================================
pm_producer(Content, CXP) ->
    ProdCont  = get_grp(Content, [eventProducer], []),

    Attr      = record_info(fields, eventProducer),
    Producer  = get_elements(ProdCont, Attr, []),
    StreamC   = get_elements(ProdCont, [streamingCapabilities], []),
    FilePullC = get_elements(ProdCont, [filePullCapabilities], []),
    EventC    = get_elements(ProdCont, [eventCapabilities], []),

    save_producer(zip4(Producer, StreamC, FilePullC, EventC, []), CXP),
    ok.


zip4([], [], [], [], Acc) -> 
    lists:reverse(Acc);
zip4([H1 | T1], [H2 | T2], [H3 | T3], [H4 | T4], Acc) -> 
    zip4(T1, T2, T3, T4, [{H1, H2, H3, H4} | Acc]).
    

%%===========================================================================
%%  pm_alias(Content)
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
%%    
%%    Find all alias definitions and store them in
%%    #pesGroupAliases and #pesTypeAliases
%%    
%%===========================================================================
pm_alias(Content) ->
    {ok, MapId} = get_value(Content, eventMapId),

    GrpAliases  = get_grp(Content, [eventGroupAlias], []),
    TypeAliases = get_grp(Content, [eventTypeAlias], []),

    save_grp_alias(GrpAliases,   MapId, []),
    save_type_alias(TypeAliases, MapId, []),
    ok.
    

%%===========================================================================
%%  pm_job(Content)
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
%%    Find all predefined jobs and store them in
%%    #eventJob 
%%    
%%===========================================================================
pm_job(Content) ->

    EventCont = get_grp(Content, [eventJob], []),
    Attr      = record_info(fields, eventJob) ++ [eventProducer],
    Job       = get_elements(EventCont, Attr, []),
    EvFilter  = get_elements(EventCont, [eventFilter], []),
    save_job(lists:zip(Job, EvFilter)),
    ok.
    

%%===========================================================================
%% save_grp([{Attr, MoClass, [MeasType]}], Hash)
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
save_grp([{EvGroup, EvType} | T], Hash) ->
    EvGrpId  = gv(eventGroupId, EvGroup),
    Producer = gv(eventProducer, EvGroup),
    sg_event_group(EvGroup, Producer, EvGrpId, Hash),
    sg_event_type(EvType, Producer, EvGrpId),
    save_grp(T, Hash).


sg_event_group(EvGroup, Producer, EvGrpId, Hash) ->
    EvGrpKeys = record_info(fields, eventGroup),
    EvGrpAttr = [gv(K, EvGroup) || K <- EvGrpKeys],
    EvGrp     = list_to_tuple([eventGroup | EvGrpAttr]),
    Descr     = gv(description, EvGroup, ""),

    MoC = sg_get_mo_class(gv(moClass, EvGroup)),

    EvGrpIdNew  = {"1", "1", "1", Producer, EvGrpId},

    %% sequence: validFilters
    Vf = sg_valid_filters(gv_sequence(validFilters, EvGroup), Producer),

    EvGrpTrans  = transform(EvGrp#eventGroup{eventGroupId      = EvGrpIdNew,
					     eventGroupVersion = Hash,
					     moClass           = MoC,
					     validFilters      = Vf,
					     description       = Descr}, 
			    ?eventGroup_types),

    is_ok(pesDb:event_group_set(EvGrpTrans), {eventGroup, EvGrpIdNew}).


sg_event_type([], _, _) ->
    ok;
sg_event_type([{_, Types} | T], Producer, EvGrpId) ->
    ETId   = gv(eventTypeId, Types),
    ETKeys = record_info(fields, eventType),
    ETAttr = [gv(K, Types) || K <- ETKeys],
    ET     = list_to_tuple([eventType | ETAttr]),

    ETIdNew = {"1", "1", "1", Producer, EvGrpId, ETId},
    ETNew   = ET#eventType{eventTypeId = ETIdNew},
    ETTrans = transform(ETNew, ?eventType_types),
    
    is_ok(pesDb:event_type_set(ETTrans), {eventType, ETIdNew}),
    sg_event_type(T, Producer, EvGrpId).


sg_valid_filters(Filters, Producer) ->
    ["ManagedElement=1,SystemFunctions=1,PmEventM=1,EventProducer=" ++
     Producer ++
     ",EventFilterType=" ++
     FT || FT <- Filters].


sg_get_mo_class(undefined) ->    
    undefined;
sg_get_mo_class(MoClass) ->    
    MoClassKeys = record_info(fields, 'ManagedObjectClass'),
    MoCAttr     = [gv(K, MoClass) || K <- MoClassKeys],
    list_to_tuple(['ManagedObjectClass' | MoCAttr]).
    


%%===========================================================================
%% save_filter([{Attr, MoClass, [MeasType]}], Hash)
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
save_filter([]) ->
    ok;
save_filter([Filter | T]) ->
    EvFilterId   = gv(eventFilterTypeId, Filter),
    Producer     = gv(eventProducer, Filter),
    DefaultVal   = gv(defaultValue, Filter),
    Method       = gv(filterMethod, Filter),
    ValueSpec    = gv(valueSpec, Filter),
    EvFilterKeys = record_info(fields, eventGroup),
    EvFilterAttr = [gv(K, Filter) || K <- EvFilterKeys],
    EvFilter     = list_to_tuple([eventFilterType | EvFilterAttr]),
    Descr        = gv(description, Filter, ""),


    FilterIdNew = {"1", "1", "1", Producer, EvFilterId},
    Trans = transform(EvFilter#eventFilterType{eventFilterTypeId = FilterIdNew,
					       defaultValue      = DefaultVal,
					       filterMethod      = Method,
					       valueSpec         = ValueSpec,
					       description       = Descr}, 
		      ?eventFilterType_types),
    
    is_ok(pesDb:event_filter_set(Trans), {eventFilter, FilterIdNew}),
    save_filter(T).


%%===========================================================================
%% save_producer([{Attr, MoClass, [MeasType]}], AppTmpDir)
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
save_producer([], _) ->
    ok;
save_producer([{Producer, StreamC, FilePullC, EventC} | T], CXP) ->
    ProdId  = gv(eventProducerId, Producer),
    sg_producer(Producer, ProdId, CXP),
    sg_stream_cap(StreamC, ProdId),
    sg_file_pull_cap(FilePullC, ProdId),
    sg_event_cap(EventC, ProdId),
    save_producer(T, CXP).
    


sg_producer(Producer, ProdId, CXP) ->
    EvProdKeys = record_info(fields, eventProducer),
    EvProdAttr = [gv(K, Producer) || K <- EvProdKeys],
    EvProd     = list_to_tuple([eventProducer | EvProdAttr]),

    NewId = {"1", "1", "1", ProdId},
    TmpDirKey = {NewId, cxp_info},
    is_ok(pesDb:pes_env_set(TmpDirKey, CXP), TmpDirKey),
    Trans = transform(EvProd#eventProducer{eventProducerId = NewId}, 
		      ?eventProducer_types),
    
    is_ok(pesDb:event_producer_set(Trans), {eventProducer, NewId}).


sg_stream_cap([], _) ->
    ok;
sg_stream_cap([{_, StreamC} | T], ProdId) ->
    Id     = gv(streamCapabilitiesId, StreamC),
    Fields = record_info(fields, streamingCapabilities),
    Attr   = [gv(F, StreamC) || F <- Fields],
    %% sequence: supportedCompressionTypes
    Sct    = gv_sequence(supportedCompressionTypes, StreamC),

    SC     = list_to_tuple([streamingCapabilities | Attr]),

    IdNew = {"1", "1", "1", ProdId, Id},
    SCNew   = SC#streamingCapabilities{streamCapabilitiesId      = IdNew,
				       supportedCompressionTypes = Sct},
    Trans = transform(SCNew, ?streamingCapabilities_types),
    
    is_ok(pesDb:streaming_cap_set(Trans), {streamingCapabilities, IdNew}),
    sg_stream_cap(T, ProdId).



sg_file_pull_cap([], _) ->
    ok;
sg_file_pull_cap([{_, FilePullC} | T], ProdId) ->
    Id     = gv(filePullCapabilitiesId, FilePullC),
    Fields = record_info(fields, filePullCapabilities),
    Attr   = [gv(F, FilePullC) || F <- Fields],
    FPC    = list_to_tuple([filePullCapabilities | Attr]),
    %% seguence: supportedCompressionTypes
    Sct    = gv_sequence(supportedCompressionTypes, FilePullC),
    %% sequence: supportedReportingPeriods
    Srp    = gv_sequence(supportedReportingPeriods, FilePullC),


    IdNew = {"1", "1", "1", ProdId, Id},
    SCNew = FPC#filePullCapabilities{filePullCapabilitiesId    = IdNew,
				     supportedCompressionTypes = Sct,
				     supportedReportingPeriods = Srp},
    Trans = transform(SCNew, ?filePullCapabilities_types),
    
    is_ok(pesDb:file_pull_cap_set(Trans), {filePullCapabilities, IdNew}),
    sg_file_pull_cap(T, ProdId).



sg_event_cap([], _) ->
    ok;
sg_event_cap([{_, EventC} | T], ProdId) ->
    EventCapId = gv(eventCapabilitiesId, EventC),
    MaxJobs    = gv(maxNoOfJobs, EventC),
    IdNew = {"1", "1", "1", ProdId, EventCapId},
    ECNew   = #eventCapabilities{eventCapabilitiesId = IdNew,
				 maxNoOfJobs         = MaxJobs},
    Trans = transform(ECNew, ?eventCapabilities_types),
    
    is_ok(pesDb:event_cap_set(Trans), {eventCapabilities, IdNew}),
    sg_event_cap(T, ProdId).



%%===========================================================================
%% save_grp_alias([{Attr, MoClass, [MeasType]}], Hash)
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
save_grp_alias([], MapId, Acc) ->
    pesDb:pes_group_aliases_set(MapId, Acc);
save_grp_alias([{_, Content} | T], MapId, Acc) ->
    {ok, Id}    = get_value(Content, eventGroupId),
    {ok, Alias} = get_value(Content, groupAlias),
    save_grp_alias(T, MapId, [{list_to_integer(Alias), Id} | Acc]).
    

%%===========================================================================
%% save_type_alias([{Attr, MoClass, [MeasType]}], Hash)
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
save_type_alias([], MapId, Acc) ->
    pesDb:pes_type_aliases_set(MapId, Acc);
save_type_alias([{_, Content} | T], MapId, Acc) ->
    {ok, Id}    = get_value(Content, eventTypeId),
    {ok, Alias} = get_value(Content, typeAlias),
    save_type_alias(T, MapId, [{Id, list_to_integer(Alias)} | Acc]).
    

%%===========================================================================
%% save_job([{Attr, MoClass, [MeasType]}], Hash)
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
save_job([]) ->
    ok;
save_job([{Job, Filter} | T]) ->
    case gv(streamDestinationIpAddress, Job) of
	undefined ->
	    sj({ok, undefined}, undefined, Job, Filter);
	IpAddr ->
	    sj(inet:parse_strict_address(IpAddr), IpAddr, Job, Filter)
    end,
    save_job(T).


sj({ok, _}, _IpAddr, Job, Filter) ->
    JobId    = gv(eventJobId, Job),
    Producer = gv(eventProducer, Job),
    JobKeys  = record_info(fields, eventJob),
    JobAttr  = [gv(K, Job) || K <- JobKeys],
    JobT     = list_to_tuple([eventJob | JobAttr]),
    Descr    = gv(description, Job, ""),

    %% sequences
    EvFilter = gv_sequence(eventFilter, Filter),
    GrpRef   = [l2b(Grp)  || Grp  <- gv_sequence(eventGroupRef, Job)],
    TypeRef  = [l2b(Type) || Type <- gv_sequence(eventTypeRef, Job)],

    JobIdNew = {"1", "1", "1", Producer, JobId},
    
    JobTrans = transform(JobT#eventJob{eventJobId    = JobIdNew,
				       eventFilter   = EvFilter,
				       eventGroupRef = GrpRef,
				       eventTypeRef  = TypeRef,
				       description   = Descr}, 
			 ?eventJob_types),
    JobRec = JobTrans#eventJob{currentJobState   = ?SessionState_STOPPED,
			       requestedJobState = ?SessionState_STOPPED,
			       jobControl        = ?JobControl_FULL},
    sj_mandatory(JobRec);
sj(_, IpAddr, _, _) ->
    sysInitI:error_report([{module, ?MODULE},
			   {streamDestinationIpAddress, IpAddr},
			   {error, illegal_format}]).



sj_mandatory(#eventJob{eventJobId = {_, _, _, _, undefined}}) ->
    sj_error(eventJobId, undefined);
sj_mandatory(#eventJob{eventJobId = {_, _, _, undefined, JobId}}) ->
    sj_error(eventProducer, JobId);
sj_mandatory(#eventJob{eventJobId        = {_, _, _, _, JobId},
		       fileOutputEnabled = undefined}) ->
    sj_error(fileOutputEnabled, JobId);
sj_mandatory(#eventJob{eventJobId          = {_, _, _, _, JobId},
		       streamOutputEnabled = undefined}) ->
    sj_error(streamOutputEnabled, JobId);
sj_mandatory(#eventJob{eventJobId = Jid} = JobRec) ->
    Old = pesDb:pes_env_get(appdata_created),
    pesDb:pes_env_set(appdata_created, [Jid | Old]),
    ok = pesDb:event_job_set(JobRec).


sj_error(Attribute, JobId) ->
    sysInitI:warning_msg("~p not defined for ~p ~n"
			 "EventJob was not created.~n", 
			 [Attribute, JobId]).

   
%%===========================================================================
%%    
%%    
%%===========================================================================
transform(Rec, TypesList) ->
    [Name | Attrs] = tuple_to_list(Rec),
    Types = [Type || {_, Type} <- TypesList],
    list_to_tuple([Name] ++ trans(Types, Attrs, [])).


trans([], [], Acc) ->
    r(Acc);
trans([_ | TT], [undefined | T], Acc) ->
    trans(TT, T, [undefined | Acc]);
trans([{struct, _} | TT], [H | T], Acc) ->
    trans(TT, T, [H | Acc]);
trans([{sequence, Seq} | TT], [H | T], Acc) ->
    S = trans_seq(Seq, H),
    trans(TT, T, [S | Acc]);
trans([Fnc | TT], [H | T], Acc) ->
    trans(TT, T, ['RcsPMEventM':Fnc(H) | Acc]).




trans_seq({struct, 'EventFilter'}, Filters) ->
    trans_ef(Filters, []);
trans_seq('RcsPMEventM.TimePeriod' = Fnc, Vals) ->
    ['RcsPMEventM':Fnc(V) || V <- Vals];
trans_seq('RcsPMEventM.CompressionTypes' = Fnc, Vals) ->
    ['RcsPMEventM':Fnc(V) || V <- Vals];
trans_seq(moRef, Vals) ->
    Vals.

trans_ef([], Acc) ->
    lists:reverse(Acc);
trans_ef([Filter | T], Acc) ->
    Keys = record_info(fields, 'EventFilter'),
    Attr = [gv(K, Filter) || K <- Keys],
    Rec  =list_to_tuple(['EventFilter' | Attr]),
    trans_ef(T, [Rec | Acc]).
    


%%===========================================================================
%%  get_elements(XmlElements, Find, Acc)
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
get_elements([], _, Acc) ->
    Acc;
get_elements([{_, E} | T], Elements, Acc) ->
    Res = ge(E, Elements, []),
    get_elements(T, Elements, [Res | Acc]).


%%==============================================
%% finito
%%==============================================
ge([], _, Acc) ->
    Acc;
%%==============================================
%% moClass
%%==============================================
ge([#xmlElement{name       = Name,
		attributes = [],
	        content    = Content} | T],
   Elements,
   Acc) when Name == moClass ->
    [MC] = get_elements([{undefined, Content}],
			record_info(fields, 'ManagedObjectClass'),
			[]),
    AccN = choose(lists:member(Name, Elements), [{Name, MC} | Acc], Acc),
    ge(T, Elements, AccN);
ge([#xmlElement{name       = Name,
		attributes = Attr} | T],
   Elements,
   Acc) when Name == moClass ->
    MOC  = get_attrs(Attr, []),
    AccN = choose(lists:member(Name, Elements), [{Name, MOC} | Acc], Acc),
    ge(T, Elements, AccN);
%%==============================================
%% eventType
%% streamingCapabilities
%% filePullCapabilities
%%==============================================
ge([#xmlElement{name    = Name,
		content = Content} | T],
   Elements,
   Acc) when Name == eventType orelse
	     Name == eventCapabilities orelse
	     Name == streamingCapabilities orelse
	     Name == filePullCapabilities ->
    [MT] = get_elements([{undefined, Content}],
			get_rec_info(Name),
			[]),
    AccN = choose(lists:member(Name, Elements), [{Name, MT} | Acc], Acc),
    ge(T, Elements, AccN);
%%==============================================
%% eventFilter
%%==============================================
ge([#xmlElement{name    = Name,
		content = Content} | T],
   Elements,
   Acc) when Name == eventFilter ->
    [EF] = get_elements([{undefined, Content}],
			record_info(fields, 'EventFilter'),
			[]),
    AccN = choose(lists:member(Name, Elements), [{Name, EF} | Acc], Acc),
    ge(T, Elements, AccN);
%%==============================================
%% validFilter
%%==============================================
ge([#xmlElement{name    = Name, 
		content = Content} | T],
   Elements,
   Acc) when Name == validFilter ->
    [MT] = get_elements([{undefined, Content}],
			[mimName],
			[]),
    AccN = choose(lists:member(Name, Elements), [{Name, MT} | Acc], Acc),
    ge(T, Elements, AccN);
%%==============================================
%% get text value
%%==============================================
ge([#xmlElement{name    = Name,
		content = [#xmlText{value = Value}]} | T],
   Elements,
   Acc) ->
    Val  = string:strip(Value, both, 34),
    AccN = choose(lists:member(Name, Elements), [{Name, Val} | Acc], Acc),
    ge(T, Elements, AccN);
%%==============================================
%% not an xmlElement
%%==============================================
ge([_|T], Elements, Acc) ->
    ge(T, Elements, Acc).


get_rec_info(eventType) ->
    record_info(fields, eventType);    
get_rec_info(eventCapabilities) ->
    record_info(fields, eventCapabilities);    
get_rec_info(streamingCapabilities) ->
    record_info(fields, streamingCapabilities);    
get_rec_info(filePullCapabilities) ->
    record_info(fields, filePullCapabilities). 




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
%%  get_value(XmlElements)
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
get_value([], Tag) ->
    {error, {not_found, Tag}};
get_value([#xmlElement{name    = Tag,
			content = [#xmlText{value = Value}]} | _],
	   Tag) ->
    {ok, Value};
get_value([_|T], Tag) ->
    get_value(T, Tag).

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
%% get_grp_alias([], Name, []) ->
%%     {error, {no_alias_defined, name}};
%% get_grp_alias([], _, Acc) ->
%%     {ok, Acc};
%% get_grp_alias([#xmlElement{name    = eventGroupId,
%% 			   content = [#xmlText{value = GrpId}]} | T],
%% 	      _GrpId, 
%% 	      GrpAlias, 
%% 	      Types) ->
%%     get_grp_alias(T, GrpId, GrpAlias, Types);
%% get_grp_alias([#xmlElement{name    = groupAlias,
%% 			   content = [#xmlText{value = GrpAlias}]} | T],
%% 	      GrpId, 
%% 	      _GrpAlias, 
%% 	      Types) ->
%%     get_grp_alias(T, GrpId, list_to_integer(GrpAlias), Types);
%% get_grp_alias([_|T], GrpId, GrpAlias, Types) ->
%%     get_grp_alias(T, GrpId, GrpAlias, Types).


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
%% get_type_alias([], _, Acc) ->
%%     Acc;
%% get_type_alias([#xmlElement{name    = eventTypeId,
%% 			    content = [#xmlText{value = TypeId}]} | T],
%% 	       undefined,
%% 	       Acc) ->
%%     get_type_alias(T, TypeId, Acc);
%% get_type_alias([#xmlElement{name    = typeAlias,
%% 			    content = [#xmlText{value = TypeAlias}]} | T],
%% 	       TypeId,
%% 	       Acc) ->
%%     get_type_alias(T, undefined, [{TypeId, list_to_integer(TypeAlias)} | Acc]);
%% get_type_alias([_|T], Res, Acc) ->
%%     get_type_alias(T, Res, Acc).










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




is_ok(Res, Info) ->
    case Res of
	ok    -> ok;
	Error -> erlang:error(Error, [Info])
    end.


gv(Key, List) ->
    proplists:get_value(Key, List).
gv(Key, List, Def) ->
    proplists:get_value(Key, List, Def).

gv_sequence(Key, List) ->
    proplists:get_all_values(Key, List).


%% f(L) -> lists:flatten(L).
r(L) -> lists:reverse(L).

l2b(L) -> list_to_binary(L).

choose(true,  T, _) -> T;
choose(false, _, F) -> F.
    




