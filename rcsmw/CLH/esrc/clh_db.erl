%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	clh_db.erl %
%%% Author: etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R9A/1

%%% @doc == COI Database functions ==
%%% This module contains wrapper functions for mnesia.
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(clh_db).
-vsn('/main/R2A/R3A/R4A/R9A/1').
-date('2017-03-23').
-author('etxjotj').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile: module.erl %
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% ------  ---------- -------  ------------------------------------------------
%% R2A/1   2012-12-11 etxpeno  Created
%% R4A/1   2015-05-29 etxberb  Added mnesia_create_table/3 &
%%                             mnesia_delete_table/1.
%% R4A/2   2015-06-26 etxberb  Changed mnesia wrapper functions.
%% R4A/3   2015-07-06 etxberb  Added install_begin/0 & install_end/0.
%% R4A/4   2015-07-07 etxberb  Added is_installation_ongoing/0.
%% R4A/5   2015-07-07 etxberb  Minor improvement.
%% R4A/6   2015-07-08 etxberb  Added Module in mnesia_create_table.
%% R4A/7   2015-10-14 etxpeno  Using the time functionality in OTP 18
%% R9A/1   2017-03-23 etxjotj  Removed big printout
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([install_begin/0,
	 init_tables/1,
	 install_end/0]).

-export([is_installation_ongoing/0]).

-export([mnesia_create_table/3,
	 mnesia_delete_table/1]).

-export([mnesia_resume_disc_items/1]).

%%% # Deprecated
-export([add_subscriber/3,
	 remove_subscriber/3,
	 get_subscribers/2]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(clhMnesiaTbl_INFO(Record),
	sysUtil:record_format(record_info(fields, clhMnesiaTbl), Record)).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(clhMnesiaTbl,
	{name,
	 module,
	 copies = [],
	 clh_changeCopyType_installation = false,
	 clh_changeCopyType_postpone = false,
	 copyTypeStatus = normal_operation}).  % clh_override | normal_operation

-record(clhSubscribers,
	{id,
	 spid}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
install_begin() ->
    DbNodes = mnesia:system_info(running_db_nodes),
    Name = clhMnesiaTbl,
    TabDefs_copies = [{disc_copies, DbNodes}],
    {atomic, ok} =
        mnesia:create_table(Name,
			    [{type, set},
			     {attributes, record_info(fields, clhMnesiaTbl)},
			     {ram_copies, DbNodes}]),
    mnesia:dirty_write(#clhMnesiaTbl{name = Name,
				     module = ?MODULE,
				     copies = TabDefs_copies,
				     clh_changeCopyType_installation = true,
				     copyTypeStatus = clh_override}),
    ok.

%%% ###########################################################################
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
init_tables(DbNodes) ->
    ClhSubscribersAttr = record_info(fields, clhSubscribers),
    {atomic, ok} =
        mnesia:create_table(clhSubscribers,
			    [{type, bag},
			     {attributes, ClhSubscribersAttr},
			     {ram_copies, DbNodes}]),
    ok.

%%% ###########################################################################
%%% @doc Installation phase.
%%%
%%% @end
%%% ###=====================================================================###
install_end() ->
    %% T1 = erlang:monotonic_time(),
    Names = mnesia:dirty_all_keys(clhMnesiaTbl),
    MnesiaTbls =
	lists:flatten([mnesia:dirty_read(clhMnesiaTbl, Name) || Name <- Names]),
    %% T2 = erlang:monotonic_time(),
    Result =
	resume_disc_items([Tbl || #clhMnesiaTbl{clh_changeCopyType_postpone =PP,
						copyTypeStatus = CTStatus} = Tbl
				      <- MnesiaTbls,
				  (not PP) andalso (CTStatus == clh_override)]),
    %% T3 =  erlang:monotonic_time(),
    %% NamesWithCctI =
    %% 	[Name || #clhMnesiaTbl{name = Name,
    %% 			       clh_changeCopyType_installation = CctI}
    %% 		     <- MnesiaTbls,
    %% 		CctI],
    %% NamesWithCctPp =
    %% 	[Name || #clhMnesiaTbl{name = Name,
    %% 			       clh_changeCopyType_postpone = CctPp}
    %% 		     <- MnesiaTbls,
    %% 		CctPp],
    %% NamesWithoutCct =
    %% 	[Name || #clhMnesiaTbl{name = Name,
    %% 			       clh_changeCopyType_installation = CctI,
    %% 			       clh_changeCopyType_postpone = CctPp}
    %% 		     <- MnesiaTbls,
    %% 		not CctI andalso not CctPp],
    %% Info =
    %% 	[{?MODULE, ?FUNCTION},
    %% 	 {read_clhMnesiaTbl,
    %% 	  sysUtil:micrSecs_to_string(erlang:convert_time_unit(T2-T1,
    %% 							      native,
    %% 							      micro_seconds))},
    %% 	 {'mnesia:change_table_copy_type',
    %% 	  sysUtil:micrSecs_to_string(erlang:convert_time_unit(T3-T2,
    %% 							      native,
    %% 							      micro_seconds))},
    %% 	 {"---Tables with changeCopyType_installation---",
    %% 	  length(NamesWithCctI)} |
    %% 	 [table_info(Name)|| Name <- NamesWithCctI]] ++
    %% 	[{"---Tables with changeCopyType_postpone---", length(NamesWithCctPp)} |
    %% 	 [table_info(Name) || Name <- NamesWithCctPp]] ++
    %% 	[{"---Tables without changeCopyType---", length(NamesWithoutCct)} |
    %% 	 [table_info(Name) || Name <- NamesWithoutCct]],
    %% error_logger:info_report(Info),
    Result.

%%% ###########################################################################
%%% @doc Check if installation is ongoing or not.
%%%
%%% @end
%%% ###=====================================================================###
is_installation_ongoing() ->
    case mnesia:dirty_read(clhMnesiaTbl, clhMnesiaTbl) of
	[#clhMnesiaTbl{copyTypeStatus = clh_override}] ->
	    true;
	_ ->
	    false
    end.

%%% ###########################################################################
%%% @doc Wrapper function for mnesia:create_table/2'.
%%%
%%% @end
%%% ###=====================================================================###
mnesia_create_table(Name, TabDefs, Module) ->
    {TabDefs_Other, TabDefs_copies, CctI, CctPp} =
	tabDefs_split(tabDefs_add_changeCopyType(false, TabDefs)),
    if
	CctI orelse CctPp ->
	    case tabDefs_disc(TabDefs_copies) of
		[] ->
		    CTStatus = normal_operation,
		    TabDefs_Modified =
			TabDefs_Other ++
			[{Item, clhI:erlang_nodes(Nodes)} ||
			    {Item, Nodes} <- TabDefs_copies];
		_ ->
		    CTStatus = clh_override,
		    TabDefs_Modified =
			TabDefs_Other ++
			[{ram_copies, clhI:erlang_nodes(Nodes)} ||
			    {_, Nodes} <- TabDefs_copies]
	    end;
	?ELSE ->
	    CTStatus = normal_operation,
	    TabDefs_Modified =
		TabDefs_Other ++
		[{Item, clhI:erlang_nodes(Nodes)} ||
		    {Item, Nodes} <- TabDefs_copies]
    end,
    mnesia:dirty_write(#clhMnesiaTbl{name = Name,
				     module = Module,
				     copies = TabDefs_copies,
				     clh_changeCopyType_installation = CctI,
				     clh_changeCopyType_postpone = CctPp,
				     copyTypeStatus = CTStatus}),
    mnesia:create_table(Name, TabDefs_Modified).

%%% ###########################################################################
%%% @doc Wrapper function for mnesia:delete_table/1'.
%%%
%%% @end
%%% ###=====================================================================###
mnesia_delete_table(Name) ->
    mnesia:dirty_delete(clhMnesiaTbl, Name),
    mnesia:delete_table(Name).

%%% ###########################################################################
%%% @doc Continuation of the wrapper function 'clhI:mnesia_create_table/2'.
%%%
%%% @end
%%% ###=====================================================================###
mnesia_resume_disc_items(Name) ->
    T1 = erlang:monotonic_time(),
    Result = resume_disc_items([Obj] = mnesia:dirty_read(clhMnesiaTbl, Name)),
    T2 = erlang:monotonic_time(),
    Options =
	case Obj#clhMnesiaTbl.clh_changeCopyType_installation of
	    true ->
		[changeCopyType_installation];
	    false ->
		[]
	end ++
	case Obj#clhMnesiaTbl.clh_changeCopyType_postpone of
	    true ->
		[changeCopyType_postpone];
	    false ->
		[]
	end,
    Info =
	[{?MODULE, ?FUNCTION},
	 {'mnesia:change_table_copy_type',
	  sysUtil:micrSecs_to_string(erlang:convert_time_unit(T2-T1,
							      native,
							      micro_seconds))},
	 {"---Table with CLH Options---", Options},
	 table_info(Name)],
    error_logger:info_report(Info),
    Result.

%%% ###---------------------------------------------------------------------###
%%% # Deprecated
%%% ###---------------------------------------------------------------------###
add_subscriber(MpId, Spid, Type) ->
    Id = {Type, MpId},
    Rec = #clhSubscribers{id   = Id,
			  spid = Spid},
    ok = mnesia:dirty_write(Rec).

remove_subscriber(MpId, Spid, Type) ->
    Id = {Type, MpId},
    Rec = #clhSubscribers{id   = Id,
			  spid = Spid},
    ok = mnesia:dirty_delete_object(Rec).

get_subscribers(MpId, Type) ->
    Id = {Type, MpId},
    lists:map(fun(#clhSubscribers{spid = Spid}) ->
		      Spid
	      end, mnesia:dirty_read(clhSubscribers, Id)).

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% resume_disc_items
%%%
%%% ###=====================================================================###
resume_disc_items([#clhMnesiaTbl{name = Name, copies = Copies} = Tbl | Tail]) ->
    Res =
	[[mnesia:change_table_copy_type(Name, Node, ToCopyType)
	  || Node <- clhI:erlang_nodes(Nodes)]
	 || {ToCopyType, Nodes} <- tabDefs_disc(Copies)],
    case lists:keyfind(aborted, 1, lists:flatten(Res)) of
	false ->
	    NewTbl =
		Tbl#clhMnesiaTbl{clh_changeCopyType_postpone = false,
				 copyTypeStatus = normal_operation},
	    mnesia:dirty_write(NewTbl);
	_ ->
	    TblInfo =
		try mnesia:table_info(Name, all)
		catch
		    ErrClass : ErrReason ->
			[{ErrClass, ErrReason}]
		end,
	    error_logger:error_report([{?MODULE, ?FUNCTION}, {result, Res}] ++
				      ?clhMnesiaTbl_INFO(Tbl) ++
				      ['---mnesia:table_info---' | TblInfo] ++
				      ['---stacktrace---'] ++
				      erlang:get_stacktrace())
    end,
    resume_disc_items(Tail);
resume_disc_items([]) ->
    ok.

%%% ###########################################################################
%%% tabDefs_add_changeCopyType
%%%
%%% ###=====================================================================###
%% tabDefs_add_changeCopyType(true, TabDefs) ->
%%     case lists:member(clh_changeCopyType_installation, TabDefs) of
%% 	true ->
%% 	    TabDefs;
%% 	false ->
%% 	    [clh_changeCopyType_installation | TabDefs]
%%     end;
tabDefs_add_changeCopyType(false, TabDefs) ->
    TabDefs.

%% tmp(gmfCxpRev) ->
%%     true;
%% tmp(comsaEcimTypes) ->
%%     true;
%% tmp(measurementReader) ->
%%     true;
%% tmp(sysMSchema) ->
%%     true;
%% tmp(pmGroup) ->
%%     true;
%% tmp(measurementType) ->
%%     true;
%% tmp(gmfMimDerivedType) ->
%%     true;
%% tmp(rule) ->
%%     true;
%% tmp(role) ->
%%     true;
%% tmp(gmfMimStruct) ->
%%     true;
%% tmp(gmfImmClass) ->
%%     true;
%% tmp(comsaEcimRelations) ->
%%     true;
%% tmp(comsaEvent) ->
%%     true;
%% tmp(comsaEcimClassTypes) ->
%%     true;
%% tmp(appmLmData) ->
%%     true;
%% tmp(appmBoardData) ->
%%     true;
%% tmp(fmAlarmType) ->
%%     true;
%% tmp(pmsScMoClasses) ->
%%     true;
%% tmp(gmfMimBiDir) ->
%%     true;
%% tmp(_) ->
%%     false.

%%% ###########################################################################
%%% tabDefs_disc
%%%
%%% ###=====================================================================###
tabDefs_disc([{disc_copies, _} = TB | Tail]) ->
    [TB | tabDefs_disc(Tail)];
tabDefs_disc([{disc_only_copies, _} = TB | Tail]) ->
    [TB | tabDefs_disc(Tail)];
tabDefs_disc([_ | Tail]) ->
    tabDefs_disc(Tail);
tabDefs_disc([]) ->
    [].

%%% ###########################################################################
%%% tabDefs_split
%%%
%%% ###=====================================================================###
tabDefs_split(TabDefs) ->
    tabDefs_split(TabDefs, {[], [], false, false}).

tabDefs_split([{Item, _} = TD | Tail], {TD_Other, TD_copies, CctI, CctPp})
  when Item == disc_copies orelse
       Item == disc_only_copies orelse
       Item == ram_copies ->
    tabDefs_split(Tail, {TD_Other, [TD | TD_copies], CctI, CctPp});
tabDefs_split([clh_changeCopyType_installation | Tail],
	      {TD_Other, TD_copies, _, CctPp}) ->
    tabDefs_split(Tail, {TD_Other, TD_copies, true, CctPp});
tabDefs_split([clh_changeCopyType_postpone | Tail],
	      {TD_Other, TD_copies, CctI, _}) ->
    tabDefs_split(Tail, {TD_Other, TD_copies, CctI, true});
tabDefs_split([TD | Tail], {TD_Other, TD_copies, CctI, CctPp}) ->
    tabDefs_split(Tail, {[TD | TD_Other], TD_copies, CctI, CctPp});
tabDefs_split([], Split_TabDefs) ->
    Split_TabDefs.

%%% ###########################################################################
%%% table_info
%%%
%%% ###=====================================================================###
table_info(Name) ->
    [#clhMnesiaTbl{module = Module}] = mnesia:dirty_read(clhMnesiaTbl, Name),
    {Module, lists:flatten([{table_name, Name},
			    {size, mnesia:table_info(Name, size)},
			    {memory, mnesia:table_info(Name, memory)} |
			    ets:select(clhMnesiaTbl,
				       [{#clhMnesiaTbl{name = Name,
						       copies = '$1',
						       _ = '_'},
					 [],
					 ['$1']}])])}.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
