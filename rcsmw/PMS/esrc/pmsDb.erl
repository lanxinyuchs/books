%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsDb.erl %
%%% @private
%%% Author:	 eolaand
%%% Description: Common PMS DB API.
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pmsDb).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/2').
-date('2017-03-21').
-author('eolaand').
-shaid('a2bfbe43458cccf1a6b269e42a473d97228a796a').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% Rev     Date       Name     What
%%% ----    ---------- -------  ------------------------------------------------
%%% R2A/1   2013-01-31 eolaand  First version
%%% R4A/2   2015-07-08 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R5A/5   2015-12-22 uabesvi  PmSupport
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API
-export([rop_file_store/2, 
	 rop_file_store/3, 
	 rop_file_store_zipped/2, 
	 rop_file_store_zipped/3, 
	 %% rop_unfetched_to_disc/1,
	 %% rop_unfetched_to_disc_after/2,
	 rop_files_list/0, 
	 rop_files_list/1, 
	 %% rop_files_to_disc/0, 
	 rop_data_get_all/0, 
	 rop_data_get/1, 
	 rop_file_info/1, 
	 rop_atime_set/1,
	 rop_file_get_oldest/0,
	 rop_file_get_oldest/1,
	 rop_file_delete/1,
	 rop_file_delete_all/0,
	 rop_file_delete_older/1,
	 rop_file_delete_oldest/0]).

-export([create_table/2]).

-export([mnesia_subscribe/1]).

-export([measurement_reader_get/1]).
-export([measurement_reader_dirty_get/1]).
-export([measurement_reader_set/1]).
-export([measurement_reader_delete/1]).
-export([measurement_reader_match/1]).
-export([measurement_reader_dirty_match/1]).
-export([measurement_reader_match_pm_group/1]).
-export([measurement_reader_match_meas_type/2]).

-export([measurement_type_get/1]).
-export([measurement_type_get/2]).
-export([measurement_type_dirty_get/1]).
-export([measurement_type_dirty_get/2]).
-export([measurement_types_match/1]).
-export([measurement_type_match/1]).
-export([measurement_type_dirty_match/1]).
-export([measurement_type_set/1]).
-export([measurement_type_flex_dirty_select/2]).

-export([base_flex_name_get/2]).

-export([app_reg_delete/1]).
-export([app_reg_get/1]).
%%-export([app_reg_match/1]).
-export([app_reg_set/1]).

-export([apps_info_set/1]).
%%-export([apps_info_update/2]).

-export([pm_set/1]).
-export([pm_support_set/1]).
-export([pm_support_get/0]).

-export([meas_capabilities_set/1]).
-export([meas_capabilities_get/0,
	 meas_capabilities_get/1,
	 meas_capabilities_get/2,
	 meas_capabilities_update/2,
	 meas_capabilities_update/3]).

-export([pm_job_all_keys/0]).
-export([pm_job_get/1]).
-export([pm_job_dirty_get/1]).
-export([pm_job_set/1]).
-export([pm_job_match/1]).
-export([pm_job_delete_object/1]).

-export([pm_group_get/1]).
-export([pm_group_dirty_get/1]).
-export([pm_group_set/1]).
-export([pm_group_match/1]).
-export([pm_group_find_mo_class/1]).
-export([pm_group_find_obj_ldn/1]).

-export([pms_env_get/1]).
-export([pms_env_set/2]).

-export([pms_mo_class_get/1]).
-export([pms_mo_class_set/1]).

-export([pms_app_mo_ldn_get/1]).
-export([pms_app_mo_ldn_get_all/0]).
-export([pms_app_mo_ldn_set/1]).
-export([pms_app_mo_ldn_delete/1]).

-export([pms_counter_aliases_get/1]).
-export([pms_counter_aliases_set/2]).


-include("RcsPm.hrl").
-include("RmePmSupport.hrl").

-include("pms.hrl").
-include_lib("kernel/include/file.hrl").

%% -define(ROP_DIR, pmsSftpdEnv:rop_dir()).
-define(ROP_DIR, ?DEFAULT_ROP_DIR).
-define(ROP_TAB, pmsROPFile).
-define(ROP_TAB_DISC, pmsROPFileDisc).
-define(ROP_TABS, [?ROP_TAB, ?ROP_TAB_DISC]).

-define(MEAS_DATA, pmsMeasData).
-define(MEAS_CAP, pmMeasurementCapabilities).
-define(MEAS_CAP_KEY, {"1","1","1","1"}).
-define(DEFAULT_MAX_ROP, 400).

-define(MIN_REP_DIFF, 9000000).


%% -record(?ROP_TAB, {name, ctime, atime, data}).

%%%===================================================================
%%% Common functions
%%%===================================================================
create_table(Name, Attributes) ->
    clhI:mnesia_create_table(Name, Attributes ++ add_clh_option(Name)).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].


mnesia_subscribe(Subscription) ->
    mnesia:subscribe(Subscription).

%%%===================================================================
%%% measurementReader API
%%%===================================================================
measurement_reader_get(Key) ->
    Fun = fun() -> mnesia:read(measurementReader, Key) end,
    trans(Fun, measurement_reader_get).
    
measurement_reader_dirty_get(Key) ->
    {ok, mnesia:dirty_read(measurementReader, Key)}.

measurement_reader_set(Rec) when is_record(Rec, measurementReader) ->
    trans(fun() -> mnesia:write(Rec) end, measurement_reader_set).

measurement_reader_match({ME, SF, PM, PmJob}) ->
    MrId = {ME, SF, PM, PmJob, '_'},
    MR   = #measurementReader{measurementReaderId = MrId,
			      _ = '_'},
    Fun = fun() -> mnesia:match_object(measurementReader, MR, read) end,
    trans(Fun, measurement_reader_match).

measurement_reader_dirty_match({ME, SF, PM, PmJob}) ->
    MrId = {ME, SF, PM, PmJob, '_'},
    MR   = #measurementReader{measurementReaderId = MrId,
			      _ = '_'},
    {ok, mnesia:dirty_match_object(measurementReader, MR)}.

measurement_reader_delete(Rec) ->
    Fun = fun() -> mnesia:delete_object(Rec) end,
    trans(Fun, measurement_reader_delete).


measurement_reader_match_pm_group(Grp) ->
    PreFix = "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=",
    GrpRef = list_to_binary(PreFix ++ Grp),
    MS     = #'MeasurementSpecification'{groupRef = GrpRef}, 
    MR     = #measurementReader{measurementSpecification = MS,
				_ = '_'},
    {ok, mnesia:dirty_match_object(measurementReader, MR)}.

measurement_reader_match_meas_type(Grp, MT) ->
    PreFix = "ManagedElement=1,SystemFunctions=1,Pm=1,PmGroup=",
    MtRef  = list_to_binary(PreFix ++ Grp ++ ",MeasurementType=" ++ MT),
    MS     = #'MeasurementSpecification'{measurementTypeRef = MtRef}, 
    MR     = #measurementReader{measurementSpecification = MS,
				_ = '_'},
    {ok, mnesia:dirty_match_object(measurementReader, MR)}.

%%%===================================================================
%%% measurementType API
%%%===================================================================
measurement_type_get(Key) ->
    trans(fun() -> 
    		  mnesia:read(measurementType, Key) 
    	  end, measurement_type_get).


measurement_type_get(Key, Attr) ->
    Attrs = record_info(fields, measurementType),
    Size = record_info(size, measurementType),
    Pos = proplists:get_value(Attr, lists:zip(Attrs, lists:seq(2,Size))),
    Fun = fun() -> 
    		  [Rec] = mnesia:read(measurementType, Key), 
    		  element(Pos, Rec)
    	  end,
    trans(Fun, measurement_type_get).    


measurement_type_dirty_get(Key) ->
    {ok, mnesia:dirty_read(measurementType, Key)}.


measurement_type_dirty_get(Key, Attr) ->
    Attrs = record_info(fields, measurementType),
    Size = record_info(size, measurementType),
    Pos = proplists:get_value(Attr, lists:zip(Attrs, lists:seq(2,Size))),
    [Rec] = mnesia:dirty_read(measurementType, Key),
    {ok, element(Pos, Rec)}.


measurement_types_match(Specs) when is_list(Specs) ->
    Fun = fun() ->
		  [{MS, mnesia:match_object(measurementType, MS, read)} ||
		      MS <- [#measurementType{measurementTypeId = MtId,
					      _ = '_'} || MtId <- Specs]]
	  end,
    trans(Fun, measurement_types_match).
    

measurement_type_match({ME, SF, PM, PmGroup}) ->
    MtId  = {ME, SF, PM, PmGroup, '_'},
    MT    = #measurementType{measurementTypeId = MtId,
				 _ = '_'},
    Fun = fun() -> mnesia:match_object(measurementType, MT, read) end,
    trans(Fun, measurement_type_match).

measurement_type_dirty_match({ME, SF, PM, PmGroup}) ->
    MtId  = {ME, SF, PM, PmGroup, '_'},
    MT    = #measurementType{measurementTypeId = MtId,
				 _ = '_'},
    {ok, mnesia:dirty_match_object(measurementType, MT)}.

measurement_type_set(Rec) when is_record(Rec, measurementType) ->
    trans(fun() -> mnesia:write(Rec) end, measurement_type_set).


measurement_type_flex_dirty_select({ME, SF, PM, PmGroup}, FlexId) ->
    MtId  = {ME, SF, PM, PmGroup, '_'},
    MT    = #measurementType{measurementTypeId = MtId,
			     _ = '_'},
    MS    = [{MT, [], ['$_']}],    
    BaseMTs = mnesia:dirty_select(measurementType, MS),
    F = fun(#measurementType{measurementTypeId = Id} = BMT, {N, _} = Acc) ->
		case re:run(FlexId, element(5, Id)) of
		    {match, [{0, P} | _]} when P > N ->
			{P, [BMT]};
		    _ ->
			Acc
		end
	end,
    {_, Res} = lists:foldl(F, {0, []}, BaseMTs),
    Res;

measurement_type_flex_dirty_select(PmGroup, FlexId) ->
    measurement_type_flex_dirty_select(?PM_GROUP_ID(PmGroup), FlexId).


base_flex_name_get({ME, SF, PM, PmGroup}, FlexId) ->
    MtId  = {ME, SF, PM, PmGroup, '$1'},
    MT    = #measurementType{measurementTypeId = MtId,
			     _ = '_'},
    MS    = [{MT, [], ['$1']}],    
    BaseMTs = mnesia:dirty_select(measurementType, MS),
    F = fun(BaseName, {N, _} = Acc) ->
		case re:run(FlexId, BaseName) of
		    {match, [{0, P} | _]} when P > N ->
			{P, [BaseName]};
		    _ ->
			Acc
		end
	end,
    case lists:foldl(F, {0, []}, BaseMTs) of
	{_N, [Name]} ->
	    {ok, Name};
	_ ->
	    {error, not_found}
    end;

base_flex_name_get(PmGroup, FlexId) ->
    base_flex_name_get(?PM_GROUP_ID(PmGroup), FlexId).


%%%===================================================================
%%% pmsAppRegistry API
%%%===================================================================
app_reg_get(PmGroups) when is_list(PmGroups) ->
    Fun = fun() -> [mnesia:read(pmsAppRegistry, PG) || PG <- PmGroups] end,
    case trans(Fun, app_reg_get) of
	{ok, Res}          -> {ok, lists:append(Res)};
	{error, _} = Error -> Error
    end;
	    
app_reg_get(PmGroup) ->
    Fun = fun() -> mnesia:read(pmsAppRegistry, PmGroup) end,
    trans(Fun, app_reg_get).
	    
%% app_reg_match(Match) when is_tuple(Match) ->
%%     Fun = fun() -> mnesia:match_object(pmsAppRegistry, Match, read) end,
%%     trans(Fun, app_reg_match).
    

app_reg_delete(Match) when is_tuple(Match) ->
    Fun = fun() -> Recs = mnesia:match_object(pmsAppRegistry, Match, read),
		   [mnesia:delete_object(Rec) || Rec <- Recs]
	  end,
    trans(Fun, app_reg_delete).



app_reg_set(Recs) when is_list(Recs) ->
    trans(fun() -> [mnesia:write(Rec) || Rec <- Recs] end, app_reg_set);
app_reg_set(Rec) when is_record(Rec, pmsAppRegistry) ->
    trans(fun() -> mnesia:write(Rec) end, app_reg_set).


%%%===================================================================
%%% pmsAppsInfo API
%%%===================================================================
apps_info_set(Rec) when is_record(Rec, pmsAppsInfo) ->
    trans(fun() -> mnesia:write(Rec) end, apps_info_set).


%%%===================================================================
%%% pm API
%%%===================================================================
pm_set(Rec) when is_record(Rec, pm) ->
    trans(fun() -> mnesia:write(Rec) end, pm_set).

%%%===================================================================
%%% pm support API
%%%===================================================================
pm_support_set(RopFileHandling) ->
    trans(fun() -> mnesia:write(?PM_SUPPORT(RopFileHandling)) end, 
	  pm_support_set).

pm_support_get() ->
    Fun = fun() -> 
		  [#pmSupport{ropFileHandling = RFH}] = 
		      mnesia:read(pmSupport, {"1", "1", "1"}), 
		  RFH
	  end,
    trans(Fun, pm_support_get).    

%%%===================================================================
%%% measurement capabilities API
%%%===================================================================
meas_capabilities_set(Rec) when is_record(Rec, pmMeasurementCapabilities) ->
    trans(fun() -> mnesia:write(Rec) end, meas_capabilities_set).


meas_capabilities_get() ->
    meas_capabilities_get(?MEAS_CAP_KEY).


meas_capabilities_get(Attr) when is_atom(Attr) ->
    meas_capabilities_get(?MEAS_CAP_KEY, Attr);

meas_capabilities_get(Key) when is_tuple(Key) ->
    case mnesia:dirty_read(?MEAS_CAP, Key) of
	[Rec] -> {ok, Rec};
	_     -> undefined
    end.
    
meas_capabilities_get(Key, Attr) ->
    Attrs = record_info(fields, ?MEAS_CAP),
    Size = record_info(size, ?MEAS_CAP),
    Pos = proplists:get_value(Attr, lists:zip(Attrs, lists:seq(2,Size))),

    case mnesia:dirty_read(?MEAS_CAP, Key) of
	[Rec] -> {ok, element(Pos, Rec)};
	_     -> undefined
    end.

meas_capabilities_update(Attr, Val) ->
    meas_capabilities_update(?MEAS_CAP_KEY, Attr, Val).


meas_capabilities_update(Key, Attr, Val) ->
    Attrs = record_info(fields, ?MEAS_CAP),
    Size = record_info(size, ?MEAS_CAP),
    Pos = proplists:get_value(Attr, lists:zip(Attrs, lists:seq(2,Size))),
    Fun = fun() ->
		  [Rec] = mnesia:read(?MEAS_CAP, Key), 
		  ok = mnesia:write(setelement(Pos, Rec, Val))
	  end,
    trans(Fun, meas_capabilities_update).    

%%%===================================================================
%%% pmGroup API
%%%===================================================================
pm_group_get(Key) ->
    Fun = fun() -> mnesia:read(pmGroup, Key) end,
    trans(Fun, pm_group_get).

pm_group_dirty_get(Key) ->
    {ok,  mnesia:dirty_read(pmGroup, Key)}.

pm_group_match({ME, SF, PM}) ->
    PmGrpId = {ME, SF, PM, '_'},
    PmGrp   = #pmGroup{pmGroupId = PmGrpId, _ = '_'},
    Fun = fun() -> mnesia:match_object(pmGroup, PmGrp, read) end,
    trans(Fun, pm_group_match).

pm_group_set(Rec) when is_record(Rec, pmGroup) ->
    trans(fun() -> mnesia:write(Rec) end, pm_group_set).

pm_group_find_mo_class(MoClassName) ->
    PmGrpId = {"1", "1", "1", '_'},
    MoClass = #'ManagedObjectClass'{moClassName = MoClassName, _ = '_'},
    PmGrp   = #pmGroup{pmGroupId = PmGrpId,
		       moClass   = MoClass,
		       _ = '_'},
    Fun = fun() -> mnesia:match_object(pmGroup, PmGrp, read) end,
    trans(Fun, pm_group_find_mo_class).

pm_group_find_obj_ldn(Ldn) ->
    SplitLdn = lists:reverse(string:tokens(Ldn, "=,")),
    [_, MoClassName | _] = SplitLdn,
    BLdn = [list_to_binary(S) || S <- SplitLdn],
    MimName = comsaEcimModelAdaptor:get_class_mim(BLdn),
    PmGrpId = {"1", "1", "1", '_'},
    MoClass = #'ManagedObjectClass'{moClassName = MoClassName, 
				    mimName = MimName, 
				    _ = '_'},
    PmGrp   = #pmGroup{pmGroupId = PmGrpId,
		       moClass   = MoClass,
		       _ = '_'},
    Fun = fun() -> mnesia:match_object(pmGroup, PmGrp, read) end,
    trans(Fun, pm_group_find_obj_ldn).


%%%===================================================================
%%% pmJob API
%%%===================================================================
pm_job_all_keys() ->
    mnesia:dirty_all_keys(pmJob).

pm_job_set(Rec) when is_record(Rec, pmJob) ->
    trans(fun() -> mnesia:write(Rec) end, pm_job_set).

pm_job_get(Key) ->
    Fun = fun() -> mnesia:read(pmJob, Key) end,
    trans(Fun, pm_job_get).

pm_job_dirty_get(Key) ->
    {ok, mnesia:dirty_read(pmJob, Key)}.

pm_job_match({ME, SF, PM}) ->
    PmJobId = {ME, SF, PM, '_'},
    PmJob   = #pmJob{pmJobId = PmJobId, _ = '_'},
    Fun = fun() -> mnesia:match_object(pmJob, PmJob, read) end,
    trans(Fun, pm_job_match).

pm_job_delete_object(Key) ->
    trans(fun() -> pm_jdo(mnesia:read(pmJob, Key), Key) end,
	  pm_job_delete_object).

pm_jdo([], Key) ->
    ?LOG_RAM(?SEV_ERROR, {"Trying to delete an unkown pm job  ~p~n", [Key]});
pm_jdo([Rec], _) ->
    mnesia:delete_object(Rec).
    

%%%===================================================================
%%% pmsEnv API
%%%===================================================================
pms_env_set(Key, Value) ->
    trans(fun() -> mnesia:write(#pmsEnv{key   = Key, 
					value = Value}) 
	  end, 
	  pms_env_setv).

pms_env_get(Key) ->
    case mnesia:dirty_read(pmsEnv, Key) of
	[{_, _, Res}] -> Res;
	_             -> undefined
    end.


%%%===================================================================
%%% pmsScMoClasses API
%%%===================================================================
pms_mo_class_set(Recs) when is_list(Recs) ->
    trans(fun() -> [mnesia:write(Rec) || Rec <- Recs] end, pms_mo_class_set);
pms_mo_class_set(Rec) when is_record(Rec, pmsScMoClasses) ->
    trans(fun() -> mnesia:write(Rec) end, pms_mo_class_set).

pms_mo_class_get(Key) ->
    Fun = fun() -> mnesia:read(pmsScMoClasses, Key) end,
    case trans(Fun, pms_mo_class_get) of
	{ok, Res} -> Res;
	_         -> undefined
    end.

%%%===================================================================
%%% pmsScAppMoLdns  API
%%%===================================================================
pms_app_mo_ldn_set(Recs) when is_list(Recs) ->
    trans(fun() -> [mnesia:write(Rec) || Rec <- Recs] end, pms_top_mo_ldn_set);
pms_app_mo_ldn_set(Rec) when is_record(Rec, pmsScAppMoLdns) ->
    trans(fun() -> mnesia:write(Rec) end, pms_app_mo_ldn_set).

pms_app_mo_ldn_get_all() ->
    mnesia:dirty_all_keys(pmsScAppMoLdns).

pms_app_mo_ldn_get(Key) ->
    Fun = fun() -> mnesia:read(pmsScAppMoLdns, Key) end,
    case trans(Fun, pms_app_mo_ldn_get) of
	{ok, Res} -> Res;
	_         -> undefined
    end.

pms_app_mo_ldn_delete(Match) when is_tuple(Match) ->
    Fun = fun() -> Recs = mnesia:match_object(pmsScAppMoLdns, Match, read),
		   [mnesia:delete_object(Rec) || Rec <- Recs]
	  end,
    trans(Fun, pms_app_mo_ldn_delete).


%%%===================================================================
%%% pmsCounterAliases API
%%%===================================================================
pms_counter_aliases_set(MapId, Values) ->
    trans(fun() -> mnesia:write(#pmsCounterAliases{counterMapId = MapId, 
						   values       = Values}) 
	  end, 
	  pms_counter_alias_set).

pms_counter_aliases_get(MapId) ->
    Fun = fun() -> mnesia:read(pmsCounterAliases, MapId) end,
    case trans(Fun, pms_counter_alias_get) of
	{ok, [{_, _, Res}]} -> Res;
	_                   -> undefined
    end.

%%%===================================================================
%%% ROP API
%%%===================================================================
rop_file_store(Name, Data) ->
    rop_file_store(?ROP_DIR, Name, Data).


rop_file_store(Dir, Name, Data) ->
    ZipFile = Name ++ ".gz",
    ZipData = zlib:gzip(Data),
    rop_file_store_zipped(Dir, ZipFile, ZipData).


rop_file_store_zipped(ZipFile, ZipData) ->
    rop_file_store_zipped(?ROP_DIR, ZipFile, ZipData).


rop_file_store_zipped(Dir, ZipFile, ZipData) ->
    MaxN = get_max_no_of_rop_files(),
    case get_no_of_rop_files(Dir) of
	N when N < MaxN ->
	    rop_file_store_retry(Dir, ZipFile, ZipData);
	_N ->
	    rop_file_delete_oldest(Dir),
	    rop_file_store_retry(Dir, ZipFile, ZipData)
    end.


rop_file_store_retry(Dir, ZipFile, ZipData) ->
    case sysFi:write_file(Dir, ZipFile, ZipData) of
	ok ->
	    ok;	    
	{error, enospc} ->
	    rop_file_delete_oldest(Dir),
	    rop_file_store_retry(Dir, ZipFile, ZipData);
	Error ->
	    ?LOG_RAM(?SEV_ERROR, 
		     {"Failed to write ROP file ~p~n", [Error]}),
	    Error
    end.


rop_files_list() ->
    rop_files_list(?ROP_DIR).


rop_files_list(Dir) ->
    case sysFi:list_dir(Dir) of
	{ok, Files} ->
	    {ok, lists:sort(Files)};
	Error ->
	    Error
    end.


rop_file_info(File) ->
    rop_file_info(?ROP_DIR, File).


rop_file_info(Dir, File) ->
    sysFi:read_file_info(Dir, File, [{time, universal}]).


rop_data_get_all() ->
    rop_data_get_all(?ROP_DIR).


rop_data_get_all(Dir) ->
    case rop_files_list(Dir) of
	{ok, Files} when Files =/= [] ->
	    rop_data_get_all(Dir, Files);
	Error ->
	    Error
    end.   


rop_data_get_all(Dir, Files) ->
    {ok, [rop_data_get_existing(Dir, File) || File <- Files]}.


rop_data_get(File) ->
    rop_data_get(?ROP_DIR, File).


rop_data_get(Dir, File) ->
    sysFi:read_file(Dir, File).


rop_atime_set(File) ->
    rop_atime_set(?ROP_DIR, File).


rop_atime_set(Dir, File) ->
    ATime = now_ut(),
    case rop_file_info(Dir, File) of
	{ok, FileInfo} when FileInfo#file_info.mtime >= ATime ->
	    MTime = time_minus(ATime, 1),
	    NewFileInfo = FileInfo#file_info{atime = ATime, mtime = MTime},
	    sysFi:write_file_info(Dir, File, NewFileInfo, 
				  [{time, universal}]);
	{ok, FileInfo} ->
	    NewATime = now_ut(),
	    sysFi:write_file_info(Dir, File, 
				  FileInfo#file_info{atime = NewATime}, 
				  [{time, universal}]);
	Error ->
	    Error
    end.   

rop_file_get_oldest() ->
    rop_get_oldest(?ROP_DIR).


rop_file_get_oldest(Dir) ->
    rop_get_oldest(Dir).
		 

rop_file_delete_all() ->
    rop_file_delete_all(?ROP_DIR).


rop_file_delete_all(Dir) ->
    case rop_files_list(Dir) of
	{ok, Files} ->
	    rop_file_delete_all(Dir, Files);
	Error ->
	    Error
    end.


rop_file_delete_all(Dir, Files) ->
    lists:foreach(fun(File) ->    
			  rop_file_delete(Dir, File)
		  end, Files).


rop_file_delete(File) ->
    rop_file_delete(?ROP_DIR, File).


rop_file_delete(Dir, File) ->
    sysFi:delete(Dir, File).


rop_file_delete_older(Time) ->
    rop_file_delete_older(?ROP_DIR, Time).


rop_file_delete_older(Dir, Seconds) ->
    sysFi:delete_older(Dir, Seconds).

%% rop_file_delete_older(Dir, Seconds) 
%%   when is_list(Dir), Dir =/= "", is_integer(Seconds) ->
%%     Precipice = time_minus(now_ut(), Seconds),
%%     {ok, Files} = sysFi:list_dir(Dir),
%%     lists:foreach(fun({File, #file_info{mtime = MTime}}) 
%% 			when MTime < Precipice ->
%% 			  sysFi:delete(Dir, File);
%% 		     (_) ->
%% 			  ok
%% 		  end, [rop_info_get_existing(Dir, File) || File <- Files]);

%% rop_file_delete_older(Dir, {seconds, S}) ->
%%     rop_file_delete_older(Dir, S);

%% rop_file_delete_older(Dir, {minutes, M}) ->
%%     rop_file_delete_older(Dir, timer:minutes(M) div 1000);

%% rop_file_delete_older(Dir, {hours, H}) ->
%%     rop_file_delete_older(Dir, timer:hours(H) div 1000).

    
rop_file_delete_oldest() ->
    rop_file_delete_oldest(?ROP_DIR).


rop_file_delete_oldest(Dir) ->
    sysFi:delete_oldest(Dir).

%% rop_file_delete_oldest(Dir) ->
%%     case rop_get_oldest(Dir) of
%% 	[] ->
%% 	    ok;
%% 	File ->
%% 	    ok = sysFi:delete(Dir, File)
%%     end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

now_ut() ->
    calendar:now_to_universal_time(os:timestamp()).
    

time_minus(Time, Seconds) ->
    TSecs = calendar:datetime_to_gregorian_seconds(Time),
    calendar:gregorian_seconds_to_datetime(TSecs - Seconds).


rop_data_get_existing(Dir, File) ->
    case sysFi:read_file(Dir, File) of
	{ok, Data} ->
	    {File, Data};
	_ ->
	    {File, []}
    end.


%% rop_info_get_existing(Dir, File) ->
%%     case rop_file_info(Dir, File) of
%% 	{ok, Info} ->
%% 	    {File, Info};
%% 	_ ->
%% 	    {File, []}
%%     end.


rop_get_oldest(Dir) ->
    case sysFi:list_dir(Dir) of
	{ok, Files} when Files =/= [] ->
	    rop_get_oldest_file(Dir, Files);
	_ ->
	    []
    end.


rop_get_oldest_file(Dir, Files) ->
    F = fun({_File, Rec}, {_OldFile, OldRec} = Acc) 
	      when Rec#file_info.mtime > OldRec#file_info.mtime ->
		Acc;
	   (NewAcc, _) ->
		NewAcc
	end,
    {OldestFile, _} = lists:foldl(F, [], [{File, rop_file_info(Dir, File)} || 
					     File <- Files]),
    OldestFile.
    

get_no_of_rop_files(Dir) ->
    case sysFi:list_dir(Dir) of
	{ok, Files} ->
	    length(Files);
	_Error ->
	    0
    end.


get_max_no_of_rop_files() ->
    case mnesia:dirty_read(?MEAS_CAP, ?MEAS_CAP_KEY) of
	[] ->
	    ?DEFAULT_MAX_ROP;
	[Rec] ->
	    Rec#?MEAS_CAP.maxNoOfPmFiles
    end.


trans(Fun, Fnc) ->
    case mnesia:transaction(Fun) of
	{atomic, ok}  -> 
	    ok;
	{atomic, Res} -> 
	    {ok, Res};
	Error -> 
	    ?LOG_RAM(?SEV_ERROR, 
		     {"Transaction in ~p error: ~p~n", [Fnc, Error]}),
	    {error, Error}
    end.
   
	    

		       
		       
