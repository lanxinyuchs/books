%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesDb.erl %
%%% @private
%%% Author:	 eolaand
%%% Description: Common PMS DB API.
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pesDb).
-vsn('/main/R3A/R4A/R12A/1').
-date('2017-10-24').
-author('eolaand').
-shaid('a2bfbe43458cccf1a6b269e42a473d97228a796a').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2014-11-03 uabesvi     First version
%%% R4A/1      2015-07-07 etxberb     Changed mnesia:create_table to
%%%                                   clhI:mnesia_create_table.
%%% R4A/2      2015-07-15 eolaand     Add fcn file_pull_cap_get
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API

-export([create_table/2]).

-export([mnesia_subscribe/1]).

-export([pm_event_set/1]).

-export([event_type_match/1]).
-export([event_type_set/1]).

-export([event_filter_match/1]).
-export([event_filter_set/1]).

-export([event_producer_set/1]).
-export([event_producer_match/1]).

-export([streaming_cap_set/1]).
-export([file_pull_cap_set/1]).
-export([file_pull_cap_get/1]).
-export([event_cap_set/1]).


%%-export([app_reg_delete/1]).
-export([app_reg_pid_delete/1]).
-export([app_reg_type_delete/1]).
-export([app_reg_get/1]).
%% -export([app_reg_match/1]).
-export([app_reg_set/1]).
-export([app_reg_get_app_jobs/0]).

%% -export([apps_info_set/1]).
%% %%-export([apps_info_update/2]).


%% -export([pm_job_all_keys/0]).
-export([event_job_get/1]).
-export([event_job_set/1]).
-export([event_job_dirty_get/1]).
-export([event_job_match/1]).
%% -export([pm_job_delete_object/1]).

-export([event_group_get/1]).
-export([event_group_set/1]).
-export([event_group_match/1]).


-export([pes_env_get/1]).
-export([pes_env_set/2]).

-export([event_cap_get/1]).

-export([pes_group_aliases_get/1]).
-export([pes_group_aliases_set/2]).

-export([pes_type_aliases_get/1]).
-export([pes_type_aliases_set/2]).



-include("RcsPMEventM.hrl").
-include("pes.hrl").


%%%===================================================================
%%% Common functions
%%%===================================================================
create_table(Name, Attributes) ->
    clhI:mnesia_create_table(Name, Attributes ++ add_clh_option(Name)).


%% Add option clh_changeCopyType_installation for large tables in order for
%% CLH to optimize disc operations. See also edoc for clhI.
add_clh_option(_) ->
    [].


mnesia_subscribe(Subscription) ->
    mnesia:subscribe(Subscription).


%%%===================================================================
%%% pm API
%%%===================================================================
pm_event_set(Rec) when is_record(Rec, pmEventM) ->
    trans(fun() -> mnesia:write(Rec) end, pm_event_set).

%%%===================================================================
%%% eventType API
%%%===================================================================
event_type_match({ME, SF, PM, Prod, EvGrp}) ->
    EvId  = {ME, SF, PM, Prod, EvGrp, '_'},
    ET    = #eventType{eventTypeId = EvId, _ = '_'},
    Fun = fun() -> mnesia:match_object(eventType, ET, read) end,
    trans(Fun, event_type_match).

%% measurement_type_dirty_match({ME, SF, PM, PmGroup}) ->
%%     MtId  = {ME, SF, PM, PmGroup, '_'},
%%     MT    = #measurementType{measurementTypeId = MtId,
%% 				 _ = '_'},
%%     {ok, mnesia:dirty_match_object(measurementType, MT)}.

event_type_set(Rec) when is_record(Rec, eventType) ->
    trans(fun() -> mnesia:write(Rec) end, event_type_set).



%%%===================================================================
%%% eventFilterType API
%%%===================================================================
event_filter_match({ME, SF, PM, Prod}) ->
    EvId  = {ME, SF, PM, Prod, '_'},
    ET    = #eventFilterType{eventFilterTypeId = EvId, _ = '_'},
    Fun = fun() -> mnesia:match_object(eventFilterType, ET, read) end,
    trans(Fun, event_filter_match).

event_filter_set(Rec) when is_record(Rec, eventFilterType) ->
    trans(fun() -> mnesia:write(Rec) end, event_filter_set).


%%%===================================================================
%%% eventProducer API
%%%===================================================================
event_producer_set(Rec) when is_record(Rec, eventProducer) ->
    trans(fun() -> mnesia:write(Rec) end, event_producer_set).

event_producer_match({ME, SF, PM}) ->
    Id = {ME, SF, PM, '_'},
    Prod   = #eventProducer{eventProducerId = Id, _ = '_'},
    Fun = fun() -> mnesia:match_object(eventProducer, Prod, read) end,
    trans(Fun, event_producer_match).

%%%===================================================================
%%% streamingCapabilites API
%%%===================================================================
streaming_cap_set(Rec) when is_record(Rec, streamingCapabilities) ->
    trans(fun() -> mnesia:write(Rec) end, event_streaming_cap_set).

file_pull_cap_set(Rec) when is_record(Rec, filePullCapabilities) ->
    trans(fun() -> mnesia:write(Rec) end, event_file_pull_cap_set).

file_pull_cap_get({ME, SF, PM, Prod}) ->
    Id = {ME, SF, PM, Prod, '_'},
    PC = #filePullCapabilities{filePullCapabilitiesId = Id, _ = '_'},
    Fun = fun() -> mnesia:match_object(filePullCapabilities, PC, read) end,
    trans(Fun, event_file_pull_cap_get).

event_cap_set(Rec) when is_record(Rec, eventCapabilities) ->
    trans(fun() -> mnesia:write(Rec) end, event_cap_set).




%%%===================================================================
%%% pesAppRegistry API
%%%===================================================================
app_reg_get(EvTypes) ->
    Fun = fun() -> [mnesia:read(pesAppRegType, ET) || ET <- EvTypes] end,
    case trans(Fun, app_reg_get) of
	{ok, Res}          -> {ok, lists:append(Res)};
	{error, _} = Error -> Error
    end.
	    
%% app_reg_match(Match) when is_tuple(Match) ->
%%     Fun = fun() -> mnesia:match_object(pesAppRegistry, Match, read) end,
%%     trans(Fun, app_reg_match).
    

app_reg_pid_delete(Match) ->
    Fun = fun() -> Recs = mnesia:match_object(pesAppRegPid, Match, read),
		   [mnesia:delete_object(R) || R <- Recs],
		   ok
	  end,
    trans(Fun, app_reg_pid_delete).

app_reg_type_delete(Match) ->
    Fun = fun() -> Recs = mnesia:match_object(pesAppRegType, Match, read),
		   [mnesia:delete_object(R) || R <- Recs],
		   ok
	  end,
    trans(Fun, app_reg_type_delete).


app_reg_set({TypeRecs, PidRec}) when is_list(TypeRecs) ->
    trans(fun() -> [mnesia:write(TR) || TR <- TypeRecs],
		   mnesia:write(PidRec)
	  end, 
	  app_reg_set);
app_reg_set({TypeRec, PidRec}) when is_record(TypeRec, pesAppRegType) ->
    app_reg_set({[TypeRec], PidRec}).


app_reg_get_app_jobs() ->
    TransFun = 
	fun() ->
		FoldlFun = 
		    fun(#pesAppRegPid{job_pid = JobPid}, Acc) -> [JobPid | Acc]
		    end,
		mnesia:foldl(FoldlFun, [], pesAppRegPid)
	end,
    trans(TransFun, app_reg_get_app_jobs).

%% app_reg_get_app_jobs() ->
%%     TransFun = 
%% 	fun() ->
%% 		FoldlFun = 
%% 		    fun(Rec, Acc) ->
%% 			    JobPid = Rec#pesAppRegistry.job_pid,
%% 			    case lists:member(JobPid, Acc) of
%% 				true ->
%% 				    Acc;
%% 				_False ->
%% 				    [JobPid | Acc]
%% 			    end
%% 		    end,
%% 		mnesia:foldl(FoldlFun, [], pesAppRegistry)
%% 	end,
%%     trans(TransFun, app_reg_get_app_jobs).

%% %%%===================================================================
%% %%% pesAppsInfo API
%% %%%===================================================================
%% apps_info_set(Rec) when is_record(Rec, pesAppsInfo) ->
%%     trans(fun() -> mnesia:write(Rec) end, apps_info_set).



%%%===================================================================
%%% eventGroup API
%%%===================================================================
event_group_get(Key) ->
    Fun = fun() -> mnesia:read(pmGroup, Key) end,
    trans(Fun, event_group_get).

event_group_match({ME, SF, PM, Prod}) ->
    EvGrpId = {ME, SF, PM, Prod, '_'},
    EvGrp   = #eventGroup{eventGroupId = EvGrpId, _ = '_'},
    Fun = fun() -> mnesia:match_object(eventGroup, EvGrp, read) end,
    trans(Fun, event_group_match).

event_group_set(Rec) when is_record(Rec, eventGroup) ->
    trans(fun() -> mnesia:write(Rec) end, event_group_set).


%%%===================================================================
%%% pmJob API
%%%===================================================================
%% pm_job_all_keys() ->
%%     mnesia:dirty_all_keys(pmJob).

event_job_set(Rec) when is_record(Rec, eventJob) ->
    trans(fun() -> mnesia:write(Rec) end, event_job_set).

event_job_get(Key) ->
    Fun = fun() -> mnesia:read(eventJob, Key) end,
    trans(Fun, event_job_get).

event_job_dirty_get(Key) ->
    {ok, mnesia:dirty_read(eventJob, Key)}.

event_job_match({ME, SF, PM, Prod}) ->
    EvJobId = {ME, SF, PM, Prod, '_'},
    EvJob   = #eventJob{eventJobId = EvJobId, _ = '_'},
    Fun = fun() -> mnesia:match_object(eventJob, EvJob, read) end,
    trans(Fun, event_job_match).

%% pm_job_delete_object(Key) ->
%%     trans(fun() -> pm_jdo(mnesia:read(pmJob, Key), Key) end,
%% 	  pm_job_delete_object).

%% pm_jdo([], Key) ->
%%     ?LOG_ERROR("Trying to delete an unkown pm job  ~p~n", [Key]);
%% pm_jdo([Rec], _) ->
%%     mnesia:delete_object(Rec).
    

%%%===================================================================
%%% pesEnv API
%%%===================================================================
pes_env_set(Key, Value) ->
    trans(fun() -> mnesia:write(#pesEnv{key   = Key, 
					value = Value}) 
	  end, 
	  pes_env_setv).

pes_env_get(Key) ->
    Fun = fun() -> mnesia:read(pesEnv, Key) end,
    case trans(Fun, pes_env_get) of
	{ok, [{_, _, Res}]} -> Res;
	_                   -> undefined
    end.


%%%===================================================================
%%% eventCapabilities API
%%%===================================================================
event_cap_get(ProdId) ->
    Key   = {"1", "1", "1", ProdId, '_'},
    EvCap = #eventCapabilities{eventCapabilitiesId = Key, _ = '_'},
    Fun = fun() -> mnesia:match_object(eventCapabilities, EvCap, read) end,
    trans(Fun, event_cap_get).


%%%===================================================================
%%% pesGroupAliases API
%%%===================================================================
pes_group_aliases_set(MapId, Values) ->
    trans(fun() -> mnesia:write(#pesGroupAliases{eventMapId = MapId, 
						 values = Values}) 
	  end, 
	  pes_group_alias_set).

pes_group_aliases_get(MapId) ->
    Fun = fun() -> mnesia:read(pesGroupAliases, MapId) end,
    case trans(Fun, pes_group_alias_get) of
	{ok, [{_, _, Res}]} -> Res;
	_                   -> undefined
    end.


%%%===================================================================
%%% pesTypeAliases API
%%%===================================================================
pes_type_aliases_set(MapId, Values) ->
    trans(fun() -> mnesia:write(#pesTypeAliases{eventMapId = MapId, 
						values = Values}) 
	  end, 
	  pes_type_alias_set).

pes_type_aliases_get(MapId) ->
    Fun = fun() -> mnesia:read(pesTypeAliases, MapId) end,
    case trans(Fun, pes_type_alias_get) of
	{ok, [{_, _, Res}]} -> Res;
	_                   -> undefined
    end.





%%%===================================================================
%%% transaction handling
%%%===================================================================
trans(Fun, _Fnc) ->
    case mnesia:transaction(Fun) of
	{atomic, ok}  -> 
	    ok;
	{atomic, Res} -> 
	    {ok, Res};
	Error -> 
	    %%?LOG_ERROR("Transaction in ~p error: ~p~n", [Fnc, Error]),
	    {error, Error}
    end.
   
	    

		       
		       
