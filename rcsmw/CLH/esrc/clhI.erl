%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	clhI.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/R4A/21

%%% @doc == CLH Interface ==
%%% CLH erlang interface
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(clhI).
-vsn('/main/R2A/R3A/R4A/21').
-date('2015-10-08').
-author('etxpeno').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% ------  ---------- -------  ------------------------------------------------
%% R1A/1   2014-06-13 etxtory  Created
%% R3A/1   2015-02-03 etxpejn  Added create_mp, delete_mp & get_role
%% R3A/2   2015-02-12 etxpejn  Added get_all_mpIds, own_mp_id & role
%% R3A/3   2015-02-19 etxberb  Added erlang_node/0, erlang_node/1,
%%                             erlang_nodes/0, erlang_nodes/1.
%% R3A/4   2015-02-25 etxberb  Added mp_id/x, core_runtime_state/x.
%% R3A/5   2015-02-26 etxberb  Added is_mp/1.
%% R3A/6   2015-02-27 etxberb  Changed core_runtime_state to core_state.
%% R4A/1   2015-04-17 etxberb  Updated according to changed interfaces.
%% R4A/2   2015-04-29 etxberb  Added mp_role/1, & file_XXX_cluster_config/0.
%% R4A/3   2015-05-21 etxberb  Added hunt_path_prefix/1.
%% R4A/4   2015-05-28 etxberb  Added mp_role/0, more options in erlang_nodes/1,
%%                             WARNING REPORT in deprecated functions.
%% R4A/5   2015-05-29 etxberb  Tidying up a bit, one more deprecated WARNING.
%% R4A/6   2015-05-29 etxberb  Added mnesia_create_table/3,
%%                             mnesia_delete_table/1.
%% R4A/7   2015-06-02 etxberb  Added fru_id/0 & fru_id/1.
%% R4A/8   2015-06-05 etxberb  Added make_default_fru_id/1.
%% R4A/9   2015-06-11 etxberb  * Enhanced string option in mp_id/1 for matching
%%                               directly on fruId.
%%                             * Return values changed in core_state/1,
%%                               op_state/1 & mp_id/1 when argument is a list
%%                               of mixed options.
%% R4A/10  2015-06-18 etxberb  Bug fix: erlang_nodes(all) changed to return
%%                             configured nodes instead of visible nodes.
%% R4A/11  2015-06-22 etxberb  Added mnesia_create_table/2.
%% R4A/12  2015-06-26 etxberb  Changed mnesia wrapper functions.
%% R4A/13  2015-07-06 etxberb  Minor improvements and changes for installation
%%                             phases.
%% R4A/14  2015-07-07 etxberb  Added is_installation_ongoing/0.
%% R4A/15  2015-07-08 etxberb  Added Module in mnesia_create_table.
%% R4A/16  2015-08-07 etxberb  Modifications / additions for installation phases
%% R4A/17  2015-09-01 etxpeno  Add ip_address/0
%% R4A/18  2015-09-02 etxpeno  Add ip_address/1
%% R4A/19  2015-09-17 etxberb  Added rpp_call/2, rpp_call/3, rpp_cast/2,
%% R4A/20  2015-09-24 etxpejn  Added short_fru_id
%% R4A/21  2015-10-08 etxpeno  Correction of type mpProperty()
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 CHI (Erlang functions corresponding to C-API functions)
%%% ###---------------------------------------------------------------------###
-export([associate_mp/3,
	 disassociate_mp/1]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.2 CSI (Erlang functions corresponding to C-API functions)
%%% ###---------------------------------------------------------------------###
-export([hunt_path_prefix/1]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.3 Information Queries
%%% ###---------------------------------------------------------------------###
-export([core_state/0,
	 core_state/1,
	 erlang_node/0,
	 erlang_node/1,
	 erlang_nodes/0,
	 erlang_nodes/1,
	 file_content_attr/2,
	 file_content_attr/3,
	 file_content_cluster_config/0,
	 file_content_select/2,
	 file_content_select/3,
	 file_name_cluster_config/0,
	 fru_id/0,
	 fru_id/1,
	 get_node_op_state/0,
	 ip_address/0,
	 ip_address/1,
	 is_installation_ongoing/0,
	 is_mp/1,
	 mp_id/0,
	 mp_id/1,
	 mp_role/0,
	 mp_role/1,
	 op_state/1,
	 own_mp_id/0,
	 short_fru_id/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.4 Wrapper Functions
%%% ###---------------------------------------------------------------------###
-export([mnesia_create_table/2,
	 mnesia_delete_table/1]).

-export([mnesia_resume_disc_items/1]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.5 Cluster Utility Functions
%%% ###---------------------------------------------------------------------###
-export([rpp_call/2,
	 rpp_call/3,
	 rpp_call_validate/2,
	 rpp_cast/2]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.6 Deprecated
%%% ###---------------------------------------------------------------------###
-export([get_all_mpIds/0]).
-export([role/0,
	 role/1]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([fruId_to_binary/1,
	 make_default_fru_id/1,
	 make_erlang_node/1,
	 make_mp_id/1,
	 rpp_call_proxy/6]).

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("clhTables.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(Default_rpp_call_timeout, 10000).

%% General
-define(ELSE, true).

-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###
-type mpId()  :: integer() | undefined.
-type fruId() :: string() | binary() | undefined.
-type erlang_node() :: atom().
-type coreRank()  :: primary | secondary | undefined.
-type coreState() :: active | standby | undefined.
-type mpRole()    :: core | regular | undefined.
-type opState()   :: enabled | disabled | undefined.

-type mpPropValue() :: (coreRank() |
			coreState() |
			mpRole() |
			opState() |
			fruId() |
			erlang_node()).
-type mpPropType() :: mpPropTypeConfig() | mpPropTypeState().
-type mpPropTypeConfig() :: coreRank | mpRole | erlNode | fruId | mpId.
-type mpPropTypeState() :: opState | coreState.
-type mpProperty() :: {mpPropType(), mpPropValue() | list(mpPropValue())}.

-type search_options() :: (all |
			   mpId() |
			   fruId() |
			   erlang_node() |
			   mpPropValue() |
			   mpProperty() |
			   list(mpId() |
				fruId() |
				erlang_node() |
				mpPropValue() |
				mpProperty())).

-type tabDef() :: {Item :: atom(), Value :: term()}.
%% See http://www.erlang.org/doc/man/mnesia.html#create_table-2

-type tabDef_disc() :: ({disc_copies, (all |
				       mpRole() |
				       list(erlang_node()))} |
			{disc_only_copies, (all |
					    mpRole() |
					    list(erlang_node()))}).

-type fileContent()       :: fileContentMpList() | undefined.
-type fileContentMpList() :: list({mpId(), list(fileContentMpProp())}).
-type fileContentMpProp() :: {mpPropTypeConfig(), mpPropValue()}.

-type mfa_tuple() :: {module(),
		      F :: atom(),
		      Args :: list()}.
-type function_args() :: {function(), Args :: list()}.

-type rpp_call_result() :: list({erlang_node(), Result_rpcCall :: term()}).
-type rpp_cast_result() :: list({erlang_node(), Result_rpcCast :: true}).

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.1.1 CHI (Erlang functions corresponding to C-API functions)
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function creates a MP that should be included in a
%%% cluster configuration.
%%% @end
%%% ----------------------------------------------------------
-spec associate_mp(integer(), binary(), atom()) -> ok | nok.
%%% ###=====================================================================###
associate_mp(MpId, FruId, CoreRole) ->
    clh_csi_service:associate_mp(MpId, FruId, CoreRole).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function delets a MP that is included in a
%%% cluster configuration.
%%% @end
%%% ----------------------------------------------------------
-spec disassociate_mp(integer()) -> ok.
%%% ###=====================================================================###
disassociate_mp(MpId) ->
    clh_csi_service:disassociate_mp(MpId).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.2 CSI (Erlang functions corresponding to C-API functions)
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function returns the prefix for a requested MpId.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec hunt_path_prefix(MpId :: integer()) ->
    string().
%%% ###=====================================================================###
hunt_path_prefix(MpId) when is_integer(MpId) ->
    hunt_path_prefix(MpId, own_mp_id()).

%%% ###=====================================================================###
hunt_path_prefix(MpId, MpId) ->
    "";
hunt_path_prefix(MpId, _) ->
    mp_prefix(MpId).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.3 Information Queries
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the own CoreState.
%%% @end
%%% ----------------------------------------------------------
-spec core_state() ->
    active | standby | undefined.
%%% ###=====================================================================###
core_state() ->
    core_state(mp_id()).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the CoreState.
%%%   If the search criteria does not match any MP, the empty list is returned.
%%%   The return value 'undefined' means that an MP matched the search criteria,
%%%   but it has no CoreState - i.e. it is not a core MP.
%%% @end
%%% ----------------------------------------------------------
-spec core_state(Prop :: mpId() | erlang_node() | mpPropValue() |
		 list(mpId() | erlang_node() | mpPropValue())) ->
    coreState() | [] | list(coreState() | []).
%%% ###=====================================================================###
core_state(MpId) when is_integer(MpId) ->
    try mnesia:dirty_read(clhMpState, MpId) of
	[Obj] ->
	    Obj#clhMpState.coreState;
	_ ->
	    []
    catch
	exit : {aborted, {no_exists, _}} ->
	    %% During installation.
	    case file_content_cluster_config() of
		undefined ->
		    %% The core-active MP is doing the installation!
		    active;
		CConfig ->
		    case file_content_attr(CConfig, MpId, mpRole) of
			core ->
			    standby;
			regular ->
			    undefined;
			NotFound ->
			    NotFound
		    end
	    end
    end;
core_state(List) when is_list(List) ->
    try io_lib:format("~s", [List]),
	case is_all_ascii_writeable(List) of
	    true ->
		MpId = mp_id(List),
		core_state(MpId);
	    false ->
		[core_state(Element) || Element <- List]
	end
    catch
	_ : _ ->
	    [core_state(Element) || Element <- List]
    end;
core_state(undefined) ->
    [];
core_state(Prop) ->
    case mp_id(Prop) of
	MpIds when is_list(MpIds) ->
	    [core_state(MpId) || MpId <- MpIds];
	MpId ->
	    core_state(MpId)
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns my own node Id.
%%% @end
%%% ----------------------------------------------------------
-spec erlang_node() ->
    ErlangNode :: erlang_node().
%%% ###=====================================================================###
erlang_node() ->
    erlang:node().

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Translates an MpId to the node Id.
%%% @end
%%% ----------------------------------------------------------
-spec erlang_node(MpId :: mpId()) ->
    ErlangNode :: erlang_node() | undefined | list(erlang_node()).
%%% ###=====================================================================###
erlang_node(MpId) when is_integer(MpId) ->
    case is_mp(MpId) of
	true ->
	    make_erlang_node(MpId);
	false ->
	    undefined
    end;
erlang_node(MpIds) when is_list(MpIds) ->
    [erlang_node(mp_id(MpId)) || MpId <- MpIds];
erlang_node(undefined) ->
    [];
erlang_node(Opt) ->
    erlang_node(mp_id(Opt)).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns a list of erlang node Ids in the cluster.
%%%   erlang_nodes/0 -> All other configured nodes except my own.
%%%   erlang_nodes/1 (Opt = all) -> All configured nodes including my own.
%%%   erlang_nodes/1 (Opt = active / standby) ->
%%%                   All configured nodes matching the specified CoreState.
%%%   erlang_nodes/1 (Opt = enabled / disabled) ->
%%%                   All configured nodes matching the specified OpState.
%%%   erlang_nodes/1 (Opt = core / regular) ->
%%%                   All configured nodes matching the specified MpRole.
%%% @end
%%% ----------------------------------------------------------
-spec erlang_nodes() ->
    ErlangNodes :: list(erlang_node()).
%%% ###=====================================================================###
erlang_nodes() ->
    erlang_nodes(mp_id(all) -- [mp_id()]).

%%% ----------------------------------------------------------
-spec erlang_nodes(Opt :: search_options()) ->
    ErlangNodes :: list(erlang_node()).
%%% ###=====================================================================###
erlang_nodes(Opt) ->
    case mp_id(Opt) of
	MpIds when is_list(MpIds) ->
	    [erlang_node(MpId) || MpId <- MpIds];
	undefined ->
	    [];
	MpId ->
	    [erlang_node(MpId)]
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will extract an attribute value from the content of
%%%   file cluster_config.
%%% @end
%%% ----------------------------------------------------------
-spec file_content_attr(MpId :: mpId() | list(mpId()) | all,
			AttrType :: mpPropTypeConfig()) ->
    AttrValue :: mpPropValue().
%%% ###=====================================================================###
file_content_attr(MpId, AttrType) ->
    file_content_attr(file_content_cluster_config(), MpId, AttrType).

%%% ----------------------------------------------------------
-spec file_content_attr(ClusterConfig :: fileContent(),
			MpId :: mpId() | list(mpId()) | all,
			AttrType :: mpPropTypeConfig()) ->
    AttrValue :: mpPropValue().
%%% ###=====================================================================###
file_content_attr(CConfig, MpId, AttrType) when is_list(CConfig) andalso
						is_integer(MpId) ->
    case lists:keyfind(MpId, 1, CConfig) of
	{MpId, MpConfig} ->
	    case lists:keyfind(AttrType, 1, MpConfig) of
		{AttrType, Value} ->
		    Value;
		false ->
		    catch throw(generate_stacktrace),
		    ErrInfo =
			[{?MODULE, ?FUNCTION},
			 {invalid_attribute_type, AttrType},
			 {stacktrace, erlang:get_stacktrace()}],
		    error_logger:error_report(ErrInfo),
		    []
	    end;
	false ->
	    []
    end;
file_content_attr(CConfig, MpIds, AttrType) when is_list(CConfig) andalso
						 is_list(MpIds) ->
    lists:flatten([file_content_attr(CConfig, MpId, AttrType) ||
		      MpId <- MpIds]);
file_content_attr(CConfig, all, AttrType) when is_list(CConfig) ->
    file_content_attr(CConfig,
		      [MpId || {MpId, _} <- CConfig],
		      AttrType);
file_content_attr(_, _, _) ->
    [].

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the content of file cluster_config.
%%% @end
%%% ----------------------------------------------------------
-spec file_content_cluster_config() ->
    FileContent :: fileContent().
%%% ###=====================================================================###
file_content_cluster_config() ->
    clh_csi_service:file_content_cluster_config().

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Matches the objects in the cluster_config file using a MatchList.
%%% @end
%%% ----------------------------------------------------------
-spec file_content_select(MatchList :: list(),
			  Result :: list()) ->
    SelectedResult :: list().
%%% ###=====================================================================###
file_content_select(MatchList, Result) ->
    file_content_select(file_content_cluster_config(), MatchList, Result).

%%% ----------------------------------------------------------
-spec file_content_select(ClusterConfig :: fileContent(),
			  MatchList :: list(),
			  Result :: list()) ->
    SelectedResult :: list().
%%% ###=====================================================================###
file_content_select(CConfig, MatchList, Result) ->
    lists:flatten([file_content_select_props(MpProps, MatchList, Result) ||
		      {_, MpProps} <- CConfig]).

file_content_select_props(MpProps, MatchList, Result) ->
    try
	[true = lists:member(MatchProp, MpProps) || MatchProp <- MatchList],
	[proplists:get_value(ResultPropKey, MpProps, []) ||
	    ResultPropKey <- Result]
    catch
	error : {badmatch, false} ->
	    []
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the file name of cluster_config.
%%% @end
%%% ----------------------------------------------------------
-spec file_name_cluster_config() ->
    FileName :: string().
%%% ###=====================================================================###
file_name_cluster_config() ->
    clh_csi_service:file_name_cluster_config().

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the FruId of the own MP.
%%% @end
%%% ----------------------------------------------------------
-spec fru_id() ->
    FruId :: fruId().
%%% ###=====================================================================###
fru_id() ->
    fru_id(mp_id()).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the FruId(s) of the specified option(s).
%%% @end
%%% ----------------------------------------------------------
-spec fru_id(Opt :: search_options()) ->
    FruId :: fruId() | list(fruId()).
%%% ###=====================================================================###
fru_id(MpId) when is_integer(MpId) orelse MpId == undefined ->
    MatchHead = #clhMpConf{mpId = MpId,
			   fruId = '$1',
			   _ = '_'},
    Result = ['$1'],
    case ets:select(clhMpConf, [{MatchHead, [], Result}]) of
	[FruId] ->
	    sysUtil:term_to_string(FruId);
	[] ->
	    undefined;
	FruIds when is_list(FruIds) ->
	    [sysUtil:term_to_string(FruId) || FruId <- FruIds]
    end;
fru_id(List) when is_list(List) ->
     [fru_id(mp_id(Element)) || Element <- List];
fru_id(Opt) ->
    fru_id(mp_id(Opt)).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function fetches operational status for the
%%% node from CS point-of-view. Internally used in CS; has
%%% no corresponding function in CSI (external interface).
%%% @end
%%% ----------------------------------------------------------
-spec get_node_op_state() ->
    {ok, NodeOpState::string}.
%%% ###=====================================================================###
get_node_op_state() ->
    clh_csi_service:get_node_op_state().

%%% ###########################################################################
%%% @doc Check if installation is ongoing or not.
%%%
%%% @end
%%% ###=====================================================================###
is_installation_ongoing() ->
    clh_db:is_installation_ongoing().

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Finds out whether an MP is configured or not.
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
is_mp(MpId) ->
    try ets:lookup(clhMpConf, MpId) of
	[_] ->
	    true;
	_ ->
	    false
    catch
	_ : _ ->
	    case clhI:file_content_attr(MpId, mpId) of
		MpId ->
		    true;
		_ ->
		    false
	    end
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the own MpId.
%%% @end
%%% ----------------------------------------------------------
-spec mp_id() -> integer().
%%% ###=====================================================================###
mp_id() ->
    case os:getenv("MPID") of
	false ->
	    1;
	MpId ->
	    list_to_integer(MpId)
    end.

%%% ----------------------------------------------------------
%%% @doc This function will show the MpId or a list of MpIds.
%%%   Different Mp properties are possible to search on:
%%%   'all'                   -> List of all configured MpIds
%%%   OpState |
%%%   {opState, OpState}      -> List of all MpIds that matches the OpState
%%%   CoreState |
%%%   {coreState, CoreState}  -> List of all MpIds that matches the CoreState
%%%   CoreRank |
%%%   {coreRank, CoreRank}    -> List of all MpIds that matches the CoreRank
%%%   MpRole |
%%%   {mpRole, MpRole}        -> List of all MpIds that matches the MpRole
%%%   FruId |
%%%   {fruId, FruId}          -> The MpId of that FruId
%%%   ErlangNodeName          -> The MpId of that ErlangNode
%%%   Mixed list of the above -> Flattened list of the MPs that matched
%%%                              any search criteria in the [mixed] list.
%%%
%%%   * Single MpId is returned when property can only match one Mp.
%%%   * Flattened list of MpIds is returned when property (theoretically) can
%%%     match multiple Mps. Only MpIds (no 'undefined') are included in the list
%%%     without any duplicates.
%%%
%%%   'undefined' means that the requested property does not match an Mp.
%%%
%%%   Examples:
%%%     * To get a list of all core MPs that are either active or standby
%%%       (i.e. alive):
%%%       clhI:mp_id([active, standby])
%%%       (or clhI:mp_id({coreState, [active, standby]}))
%%%       -> [1,2]
%%%     * Alternative; to get a list of the configured core Mps:
%%%       clhI:mp_id(core)
%%%       (or clhI:mp_id({mpRole, core}))
%%%       -> [1,2]
%%%
%%% @end
%%% ----------------------------------------------------------
-spec mp_id(MpProperty :: search_options()) ->
    MpId_or_MpIds :: mpId() | list(mpId() | list(mpId())).
%%% ###=====================================================================###
mp_id(all) ->
    MatchHead = #clhMpState{mpId = '$1',
			    _ = '_'},
    Result = ['$1'],
    TableFun = fun() -> ets:select(clhMpState, [{MatchHead, [], Result}]) end,
    FileFun = fun() -> file_content_attr(all, mpId) end,
    case try_fun([TableFun, FileFun]) of
	[] ->
	    %% During installation phases.
	    mnesia:system_info(running_db_nodes);
	MpIds ->
	    MpIds
    end;
mp_id({opState, OpState}) when is_atom(OpState) ->
    MatchHead = #clhMpState{mpId = '$1',
			    opState = OpState,
			    _ = '_'},
    Result = ['$1'],
    try_fun(fun() -> ets:select(clhMpState, [{MatchHead, [], Result}]) end);
mp_id({opState, List}) when is_list(List) ->
    mp_id_flatten([mp_id({opState, Element}) || Element <- List]);
mp_id(OpState) when OpState == enabled orelse OpState == disabled ->
    mp_id({opState, OpState});
mp_id({coreState, CoreState}) when is_atom(CoreState) ->
    MatchHead = #clhMpState{mpId = '$1',
			    coreState = CoreState,
			    _ = '_'},
    Result = ['$1'],
    try_fun(fun() -> ets:select(clhMpState, [{MatchHead, [], Result}]) end);
mp_id({coreState, List}) when is_list(List) ->
    mp_id_flatten([mp_id({coreState, Element}) || Element <- List]);
mp_id(CoreState) when CoreState == active orelse CoreState == standby ->
    mp_id({coreState, CoreState});
mp_id({coreRank, CoreRank}) when is_atom(CoreRank) ->
    MatchHead = #clhMpConf{mpId = '$1',
			   coreRank = CoreRank,
			   _ = '_'},
    Result = ['$1'],
    try_fun([fun() -> ets:select(clhMpConf, [{MatchHead, [], Result}]) end,
	     fun() -> file_content_select([{coreRank, CoreRank}], [mpId]) end]);
mp_id(CoreRank) when CoreRank == primary orelse CoreRank == secondary ->
    mp_id({coreRank, CoreRank});
mp_id({coreRank, List}) when is_list(List) ->
    mp_id_flatten([mp_id({coreRank, Element}) || Element <- List]);
mp_id({mpRole, MpRole}) when is_atom(MpRole) ->
    MatchHead = #clhMpConf{mpId = '$1',
			   mpRole = MpRole,
			   _ = '_'},
    Result = ['$1'],
    try_fun([fun() -> ets:select(clhMpConf, [{MatchHead, [], Result}]) end,
	     fun() -> file_content_select([{mpRole, MpRole}], [mpId]) end]);
mp_id(MpRole) when MpRole == core orelse MpRole == regular ->
    mp_id({mpRole, MpRole});
mp_id({mpRole, List}) when is_list(List) ->
    mp_id_flatten([mp_id({mpRole, Element}) || Element <- List]);
mp_id({role, MpRole}) ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 {'DEPRECATED atom', role},
				 {called_by, sysUtil:get_previous_module()}]),
    mp_id({mpRole, MpRole});
mp_id({fruId, [E1 | _] = List}) when is_list(E1) orelse is_binary(E1) ->
    [mp_id(Element) || Element <- List];
mp_id({fruId, FruId}) ->
    MatchHead = #clhMpConf{mpId = '$1',
			   fruId = fruId_to_binary(FruId),
			   _ = '_'},
    Result = ['$1'],
    TableFun = fun() -> ets:select(clhMpConf, [{MatchHead, [], Result}]) end,
    FileFun = fun() -> file_content_select([{fruId, FruId}], [mpId]) end,
    case try_fun([TableFun, FileFun]) of
	[MpId] ->
	    MpId;
	[] ->
	    undefined;
	MpIds ->
	    MpIds
    end;
mp_id(List) when is_list(List) ->
    try io_lib:format("~s", [List]),
	case is_all_ascii_writeable(List) of
	    true ->
		mp_id({fruId, List});
	    false ->
		mp_id_flatten([mp_id(Element) || Element <- List])
	end
    catch
	_ : _ ->
	    mp_id_flatten([mp_id(Element) || Element <- List])
    end;
mp_id(Binary) when is_binary(Binary) ->
    mp_id({fruId, Binary});
mp_id(ErlNode) when is_atom(ErlNode) ->
    MatchHead = #clhMpConf{mpId = '$1',
			   erlNode = ErlNode,
			   _ = '_'},
    Result = ['$1'],
    TableFun = fun() -> ets:select(clhMpConf, [{MatchHead, [], Result}]) end,
    FileFun = fun() -> file_content_select([{erlNode, ErlNode}], [mpId]) end,
    case try_fun([TableFun, FileFun]) of
	[MpId] ->
	    MpId;
	_ ->
	    undefined
    end;
mp_id(MpId) when is_integer(MpId) ->
    case ets:lookup(clhMpConf, MpId) of
	[#clhMpConf{mpId = MpId}] ->
	    MpId;
	[] ->
	    undefined
    end.
%%     case make_mp_id(ErlNode) of
%% 	{multi_du_system, MpId} ->
%% 	    case ets:lookup(clhMpConf, MpId) of
%% 		[_] ->
%% 		    MpId;
%% 		_ ->
%% 		    undefined
%% 	    end;
%% 	{single_du_system, MpId} ->
%% 	    MpId;
%% 	_ ->
%% 	    undefined
%%     end.

%%% ###########################################################################
%%% mp_id_flatten
%%%
%%% ###=====================================================================###
mp_id_flatten(List) when is_list(List) ->
    mp_id_flatten(List, []).

%%% ###=====================================================================###
mp_id_flatten([undefined | Tail], Acc) ->
    mp_id_flatten(Tail, Acc);
mp_id_flatten([E | Tail], Acc) when not is_list(E) ->
    case lists:member(E, Acc) of
	false ->
	    mp_id_flatten(Tail, Acc ++ [E]);
	true ->
	    mp_id_flatten(Tail, Acc)
    end;
mp_id_flatten([E | Tail], Acc) when is_list(E) ->
    mp_id_flatten(E ++ Tail, Acc);
mp_id_flatten([], Acc) ->
    Acc.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the MpRole.
%%% @end
%%% ----------------------------------------------------------
-spec mp_role() ->
    MpRole :: mpRole().
%%% ###=====================================================================###
mp_role() ->
    mp_role(mp_id()).

%%% ----------------------------------------------------------
-spec mp_role(integer() |   % MpId
	      atom() |      % ErlNode
	      {coreRank,  CoreRank :: atom()} |
	      {coreState, CoreState :: atom()}) ->
    MpRole :: mpRole().
%%% ###=====================================================================###
mp_role(MpId) when is_integer(MpId) ->
    try mnesia:dirty_read(clhMpConf, MpId) of
	[#clhMpConf{mpRole = MpRole}] ->
	    MpRole;
	_Else ->
	    undefined
    catch
	exit : {aborted, {no_exists, _}} ->
	    %% During installation.
	    %% The core-active MP is doing the installation!
	    core
    end;
mp_role(ErlNode) when is_atom(ErlNode) andalso
		      ErlNode /= undefined ->
    mp_role(mp_id(ErlNode));
mp_role({coreRank, primary}) ->
    core;
mp_role({coreRank, secondary}) ->
    core;
mp_role({coreRank, undefined}) ->
    regular;
mp_role({coreState, active}) ->
    core;
mp_role({coreState, standby}) ->
    core;
mp_role({coreState, undefined}) ->
    regular;
mp_role(_) ->
    undefined.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the OperationalState.
%%%   If the search criteria does not match any MP, the empty list is returned.
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
op_state(MpId) when is_integer(MpId) ->
    try mnesia:dirty_read(clhMpState, MpId) of
	[Obj] ->
	    Obj#clhMpState.opState;
	_ ->
	    []
    catch
	exit : {aborted, {no_exists, _}} ->
	    %% During installation.
	    %% The core-active MP is doing the installation!
	    disabled   % Becomes enabled later during start phases.
    end;
op_state(cluster) ->
    %% TODO: AIC is using 'clhI:get_node_op_state/0' - investigate how this
    %%       really should be implemented. 'get_node_op_state' should be
    %%       replaced by this function clause...
    not_implemented_yet;
op_state(List) when is_list(List) ->
    try io_lib:format("~s", [List]),
	case is_all_ascii_writeable(List) of
	    true ->
		MpId = mp_id(List),
		op_state(MpId);
	    false ->
		[op_state(Element) || Element <- List]
	end
    catch
	_ : _ ->
	    [op_state(Element) || Element <- List]
    end;
op_state(undefined) ->
    [];
op_state(Prop) ->
    case mp_id(Prop) of
	MpIds when is_list(MpIds) ->
	    [op_state(MpId) || MpId <- MpIds];
	MpId ->
	    op_state(MpId)
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the own MpId.
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
own_mp_id() ->
    mp_id().

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the own IP address used for the cluster.
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
-spec ip_address() -> IpAddr :: inet:ip4_address().
ip_address() ->
    ip_address(mp_id()).

-spec ip_address(MpId :: mpId()) -> IpAddr :: inet:ip4_address().
ip_address(MpId) when MpId >= 1,
		      MpId =< 63 ->
    {169, 254, 3, MpId}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This function will show the FruId of the own MP. If the FruId starts
%%%      with "ManagedElement=1,Equipment=1" it will be removed.
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
short_fru_id() ->
    case fru_id() of
	"'$default_fru_id' MpId = 1" ->
	    "";
	FruString ->
	    %% Check if the Fru ID can be stripped from ManagedElement a
	    %% and Equipment MO.
	    [MangmentMo | RestMo] = string:tokens(FruString, ","),
	    case string:str(MangmentMo, "ManagedElement") of
		0 ->
		    FruString;
		_Else ->
		    [EquipmentMo | Rest] = RestMo,
		    case string:str(EquipmentMo, "Equipment") of
			0 ->
			    RestMo;
			_ ->
			    string:join(Rest, ",")
		    end
	    end
    end.

%%% ###---------------------------------------------------------------------###
%%% # 3.1.4 Wrapper Functions
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Wrapper function for 'mnesia:create_table/2'.
%%%   Creates an Mnesia table. Same as 'mnesia:create_table/2', but CLH offers
%%%   a few extra options.
%%%
%%%   See also http://www.erlang.org/doc/man/mnesia.html
%%%   for non CLH specific information.
%%%
%%%   Except for the standard mnesia Table Definitions, these additional options
%%%   are CLH specific:
%%%
%%%   1) User's choice how to distribute copies to core / regular / all MPs.
%%%      This is achieved by specifying the desired distribution instead of
%%%      NodeList. See the type definition for 'tabDef_disc()'.
%%%      ('{CopyTypeItem, NodeList | MpRole | all}')
%%%   2) 'clh_changeCopyType_installation': The 'disc_copies' and
%%%      'disc_only_copies' are set temporarily by CLH to 'ram_copies', and then
%%%      changed to the originally desired CopyType at the end of the
%%%      installation phases.
%%%   3) 'clh_changeCopyType_postpone': Same as 2), but CLH does not change to
%%%      the originally desired CopyType at the end of the installation phases.
%%%      Instead, the user must call function 'mnesia_resume_disc_items/1' to
%%%      complete table settings.
%%%      This option is typically used by background jobs that are expected to
%%%      continue after the installation phases are finished.
%%%
%%%   NOTE: If not using the CLH options for distribution of copies (i.e. using
%%%         the list of nodes received in the post_init function), CLH will not
%%%         update the distribution scheme when nodes are dynamically added to
%%%         the system.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec mnesia_create_table(Name :: atom(),
			  TabDefs :: list(tabDef() |
					  tabDef_disc() |
					  clh_changeCopyType_installation |
					  clh_changeCopyType_postpone)) ->
    Result :: {atomic, ok} | {aborted, Reason :: term()}.
%%% ###=====================================================================###
mnesia_create_table(Name, TabDefs) ->
    mnesia_create_table(is_installation_ongoing(), Name, TabDefs).

mnesia_create_table(true, Name, TabDefs) ->
    clh_db:mnesia_create_table(Name, TabDefs, sysUtil:get_previous_module());
mnesia_create_table(false, Name, TabDefs) ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 {not_recommended,
				  called_outside_installation_phases},
				 {calling_module,sysUtil:get_previous_module()},
				 {table_name, Name},
				 {table_definitions, TabDefs}]),
    mnesia:create_table(Name, TabDefs).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Wrapper function for 'mnesia:delete_table/1'.
%%%   Deletes an Mnesia table that was created with mnesia_create_table/3.
%%% @end
%%% ----------------------------------------------------------
-spec mnesia_delete_table(Name :: atom()) ->
    ok.
%%% ###=====================================================================###
mnesia_delete_table(Name) ->
    clh_db:mnesia_delete_table(Name).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Continuation of the wrapper function 'clhI:mnesia_create_table/2'.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec mnesia_resume_disc_items(Name :: atom()) ->
    ok.
%%% ###=====================================================================###
mnesia_resume_disc_items(Name) ->
    clh_db:mnesia_resume_disc_items(Name).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.5 Cluster Utility Functions
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Remote Parallel Procedure - synchronous CALL.
%%%   Parallel synchronous rpc:call to desired nodes in the cluster.
%%%   Trying to call all nodes in specified search criteria.
%%%   Returns a list with the result of each node call. Only nodes that matched
%%%   the search criteria are included in the result list.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec rpp_call(NodesSpecification :: search_options(),
	       mfa_tuple() | function() | function_args()) ->
    rpp_call_result().
%%% ###=====================================================================###
rpp_call(NodesSpec, Function) ->
    rpp_call(NodesSpec, Function, ?Default_rpp_call_timeout).

%%% ----------------------------------------------------------
-spec rpp_call(NodesSpecification :: search_options(),
	       mfa_tuple() | function() | function_args(),
	       TimeoutMilliseconds :: integer()) ->
    rpp_call_result().
%%% ###=====================================================================###
rpp_call(NodesSpec, Function, Timeout) ->
    {M, F, Args} = mfa(Function),
    ProxyPids =
	[{spawn(?MODULE,
		rpp_call_proxy,
		[M, F, Args, Timeout, Node, self()]),
	  Node} ||
	    Node <- erlang_nodes(NodesSpec)],
    rpp_call_loop(ProxyPids, Timeout + 1000).

%%% ###########################################################################
%%% @doc Validate the result list received from the rpp_call/X function.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec rpp_call_validate(Results :: rpp_call_result(),
			ResultSpec :: list({NodesSpec :: search_options(),
					    ApprovedResults :: list(term())}))->
    ok | {rpp_call_fail, UnwantedResults :: rpp_call_result()}.
%%% ###=====================================================================###
rpp_call_validate(Results, ResultSpec) ->
    Nodes_WantedResults = rpp_call_validate_spec(ResultSpec),
    rpp_call_validate(Nodes_WantedResults, Results, []).

%%% ----------------------------------------------------------
rpp_call_validate([{Node, WantedResults} | Tail], Results, AccUnwanteds) ->
    case lists:keyfind(Node, 1, Results) of
	{Node, Result} ->
	    case lists:member(Result, WantedResults) of
		true ->
		    rpp_call_validate(Tail, Results, AccUnwanteds);
		false ->
		    rpp_call_validate(Tail,
				      Results,
				      [{Node, Result} | AccUnwanteds])
	    end;
	false ->
	    rpp_call_validate(Tail, Results, AccUnwanteds)
    end;
rpp_call_validate([], _, []) ->
    ok;
rpp_call_validate([], _, AccUnwanteds) ->
    {rpp_call_fail, AccUnwanteds}.

%%% ----------------------------------------------------------
rpp_call_validate_spec(ResultSpec) ->
    rpp_call_validate_spec(ResultSpec, []).

rpp_call_validate_spec([{NodesSpec, ApprResults} | Tail], AccResultSpecs) ->
    ResultSpecs = [{Node, ApprResults} || Node <- erlang_nodes(NodesSpec)],
    rpp_call_validate_spec(Tail, AccResultSpecs ++ ResultSpecs);
rpp_call_validate_spec([], AccResultSpecs) ->
    AccResultSpecs.

%%% ###########################################################################
%%% @doc Remote Parallel Procedure - asynchronous CAST.
%%%   Parallel asynchronous rpc:cast to desired nodes in the cluster.
%%%   Trying to cast to all nodes in specified search criteria.
%%%   Returns a list with nodes and the corresponding return value from
%%%   rpc:cast.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec rpp_cast(NodesSpecification :: search_options(),
	       mfa_tuple() | function() | function_args()) ->
    rpp_cast_result().
%%% ###=====================================================================###
rpp_cast(NodesSpec, Function) ->
    {M, F, Args} = mfa(Function),
    [{Node, rpc:cast(Node, M, F, Args)} || Node <- erlang_nodes(NodesSpec)].

%%% ###---------------------------------------------------------------------###
%%% # 3.1.6 Deprecated
%%% ###---------------------------------------------------------------------###
%%% ----------------------------------------------------------
%%% @doc This function will fetch a list with all configured MpIds.
%%% DEPRECATED, use 'clhI:mp_id(all)' instead.
%%% @end
%%% ----------------------------------------------------------
-spec get_all_mpIds() -> MpIdList::list().
get_all_mpIds() ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 'DEPRECATED',
				 {called_by, sysUtil:get_previous_module()}]),
    mnesia:dirty_all_keys(clhMpConf).

%%% ----------------------------------------------------------
%%% @doc This function will show the role.
%%% DEPRECATED, use 'clhI:core_state()' instead.
%%% @end
%%% ----------------------------------------------------------
-spec role() -> active | regular.
%% role() ->
%%     role(mp_id()).
role() ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 'DEPRECATED',
				 {called_by, sysUtil:get_previous_module()}]),
    case mp_id() of
	1 ->
	    active;
	_ ->
	    regular
    end.

%%% ----------------------------------------------------------
%%% @doc This function will show the role for a MpId.
%%% core | regular
%%% DEPRECATED, use mp_role/1 instead.
%%% @end
%%% ----------------------------------------------------------
-spec role(integer()) ->
    Role :: atom().
role(MpId) when is_integer(MpId) ->
    error_logger:warning_report([{?MODULE, ?FUNCTION},
				 'DEPRECATED',
				 {called_by, sysUtil:get_previous_module()}]),
    case mnesia:dirty_read(clhMpConf, MpId) of
	[Obj] ->
	    Obj#clhMpConf.mpRole;
	_Else ->
	    undefined
    end.

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% fruId_to_binary
%%%
%%% ###=====================================================================###
fruId_to_binary(Id) when is_binary(Id) ->
    Id;
fruId_to_binary(Id)  when is_list(Id) ->
    list_to_binary(Id);
fruId_to_binary(Id) ->
    Id.

%%% ###########################################################################
%%% is_all_ascii_writeable
%%%
%%% ###=====================================================================###
is_all_ascii_writeable([E | Tail]) when is_integer(E) andalso E > 16#1F ->
    is_all_ascii_writeable(Tail);
is_all_ascii_writeable([]) ->
    true;
is_all_ascii_writeable(_) ->
    false.

%%% ###########################################################################
%%% make_default_fru_id
%%%
%%% ###=====================================================================###
make_default_fru_id(MpId) ->
    ?FruId_default(MpId).

%%% ###########################################################################
%%% make_erlang_node
%%%
%%% ###=====================================================================###
make_erlang_node(MpId) when is_integer(MpId) ->
    case make_mp_id(node()) of
	{multi_du_system, _} ->
	    Sname = "du" ++ integer_to_list(MpId),
	    list_to_atom(re:replace(atom_to_list(node()),
				    "du[0-9]+",
				    Sname,
				    [{return, list}]));
	{single_du_system, _} ->
	    node();
	_ ->
	    undefined
    end.

%%% ###########################################################################
%%% make_mp_id
%%%
%%% ###=====================================================================###
make_mp_id(ErlNode) when is_atom(ErlNode) ->
    case
	re:run(atom_to_list(ErlNode),
	       "du([0-9]+)",
	       [{capture, [1], list}])
	of
	{match, [Id]} ->
	    {multi_du_system, list_to_integer(Id)};
	_ ->
	    case node() of
		ErlNode ->
		    {single_du_system, 1};
		_ ->
		    undefined
	    end
    end.

%%% ###########################################################################
%%% rpp_call_proxy
%%%
%%% ###=====================================================================###
rpp_call_proxy(M, F, Args, Timeout, Node, LoopPid) ->
    Result = rpc:call(Node, M, F, Args, Timeout),
    LoopPid ! {rpp_call_proxy, Result, self(), Node},
    exit(normal).

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% is_any_process_alive
%%%
%%% ###=====================================================================###
is_any_process_alive([Pid | Tail]) ->
    case erlang:is_process_alive(Pid) of
	true ->
	    true;
	false ->
	    is_any_process_alive(Tail)
    end;
is_any_process_alive([]) ->
    false.

%%% ###########################################################################
%%% mfa
%%%
%%% ###=====================================================================###
mfa({Fun, FunArgs}) when is_function(Fun) ->
    {erlang, apply, [Fun, FunArgs]};
mfa(Fun) when is_function(Fun) ->
    {erlang, apply, [Fun, []]};
mfa(MFA_tuple) ->
    MFA_tuple.

%%% ###########################################################################
%%% mp_prefix
%%%
%%% ###=====================================================================###
mp_prefix(MpId) ->
    "du" ++ integer_to_list(MpId).

%%% ###########################################################################
%%% rpp_call_loop
%%%
%%% ###=====================================================================###
rpp_call_loop([_ | _] = ProxyPids, Timeout) ->
    receive
	{rpp_call_proxy, Result, ProxyPid, Node} ->
	    [{Node, Result} | rpp_call_loop(ProxyPids -- [{ProxyPid, Node}],
					    Timeout)]
    after
	Timeout ->
	    RemainingPids = [Pid || {Pid, _} <- ProxyPids],
	    case is_any_process_alive(RemainingPids) of
		true ->
		    [exit(Pid, kill) || Pid <- RemainingPids],
		    rpp_call_loop(ProxyPids, 0);   % flush msg q
		false ->
		    [{Node, timeout} || {_, Node} <- ProxyPids]
	    end
    end;
rpp_call_loop([], _) ->
    [].

%%% ###########################################################################
%%% try_fun
%%%
%%% ###=====================================================================###
try_fun([Fun | Tail]) ->
    try
	Fun()
    catch
	_ : _ ->
	    try_fun(Tail)
    end;
try_fun(Fun) when is_function(Fun) ->
    try_fun([Fun]);
try_fun([]) ->
    [].

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
