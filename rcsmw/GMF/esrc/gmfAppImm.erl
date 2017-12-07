%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfAppImm.erl %
%%% Author:
%%% Description: Handles parsing of gmfImm metadata.
%%%
%%% Modules used: TODO, when refactoring done
%%%
%%% ----------------------------------------------------------
-module(gmfAppImm).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R5A/R12A/1').
-date('2017-10-27').
-author('etxarnu').
-shaid('66fb3d9ee9f1a3344b99d76483f055a7fb4eb9d1').
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
%%% Rev     Date       Name     What
%%% -----   ---------  -------- ------------------------
%%% R5A/1   2016-01-20 etxpeno  Refactoring
%%% R5A/3   2016-03-08 etxpeno  no mixing between ets:lookup and mnesia:write
%%% -----   ---------  -------- ------------------------
%%% R12A/1  2017-10-27 etxarnu  Corrected filename_join for wildcards
%%%% ----------------------------------------------------------


-export([appdata/3,
	 ad_complete/0,
	 filename_join/3
	]).

-export([get_imm_data/0]).

-include_lib("gmf.hrl").
-include("gmfMeta.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(MonoTime, erlang:monotonic_time()).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%% @doc Called by SWM to handle appdata for target="gmfImm".

-spec appdata(string(), string(), #xmlElement{}) -> ok.

appdata(CxpProdId, CxpRev, AppDataE) ->
    {ok, CxpPath} = swmI:get_cxp_path(CxpProdId, CxpRev),

    ImmInfos = gmfMetaLib:getAppdataImmInfos(AppDataE, CxpPath),

    Classes =
	lists:append(
	  [parseClasses(CxpProdId, CxpRev, CxpPath, AbsPath)
	   || #immInfo{type=Type, abspath=AbsPath} <- ImmInfos,
	      Type =:= namedSchema orelse Type =:= unnamedSchema]),

    Objects =
	lists:append(
	  [parseObjects(CxpProdId, CxpRev, CxpPath, AbsPath)
	   || #immInfo{type=objects, abspath=AbsPath} <- ImmInfos]),

    RootObjects =
	lists:append(
	  [case isRootInstance(Object) of true -> [Object]; _ -> [] end
	   || Object <- Objects]),


    ImmInfosObj = [X || #immInfo{type=objects}=X <- ImmInfos],

    {atomic, ok} =
	mnesia:transaction(
	  fun() ->
		  ok = update_tables(Classes,
				     RootObjects,
				     {CxpProdId, CxpRev, ImmInfosObj}),
		  ok = update_cxpRev(CxpProdId,
				     CxpRev,
				     CxpPath,
				     [[Rdn, Class] || #gmfImmObject{key={Rdn, Class, _, _}} <- RootObjects],
				     ImmInfos)
	  end),
    ok.

ad_complete() ->
    T0 = ?MonoTime,
    ok = init_safs(),
    T1 = ?MonoTime,
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  {'init_safs', sysUtil:time_to_string(T1 - T0)},
			  "-------------- Summary --------------",
			  {'TOTAL', sysUtil:time_to_string(T1 - T0)}]),

    ok.

%% @doc Returns the absolute pathname of the indicated file.
%% Wildcard expansion is performed on the RelPath part. This function
%% is used by gmfDataInit, gmfAppImm and gmfAppMim.
%%
%% TODO, an uncaught match error for [AbsDir] can occur.
%% TODO, this kind of wildcarding is done in several places.
%% TODO, where does this function belong?

filename_join(CxpPath, RelPath, FileName) ->
    [AbsFile] = filelib:wildcard(filename:join([CxpPath, RelPath, FileName])),
    AbsFile.


-spec parseClasses(string(), string(), string(), string()) -> [#gmfImmClass{}].

parseClasses(CxpProdId, CxpRev, _CxpPath, AbsPath) ->
    OverrideAbsPath = swmI:find_file(AbsPath),
    gmfImmXml:parse_classes(CxpProdId, CxpRev, OverrideAbsPath).


-spec parseObjects(string(), string(), string(), string()) -> [#gmfImmObject{}].

parseObjects(CxpProdId, CxpRev, _CxpPath, AbsPath) ->
    OverrideAbsPath = swmI:find_file(AbsPath),
    gmfImmXml:parse_objects(CxpProdId, CxpRev, OverrideAbsPath).


%% @doc Returns true if the given gmfImmObject is a
%% root instance (the distinguished name has exactly
%% one component).

-spec isRootInstance(#gmfImmObject{}) -> boolean().

isRootInstance(#gmfImmObject{key={Name, _Class, _Cxp, _Rev}}) ->
    length(string:tokens(Name, ",")) =:= 1.


%% @doc Write to the gmfImmClass, root object and gmfImmObjectFile
%% tables.

-spec update_tables([#gmfImmClass{}], [#gmfImmObject{}],
		    {string(), string(), [#immInfo{}]}) -> ok.

update_tables(Classes, RootObjects, {CxpProdId, CxpRev, ObjRefs}) ->
    [mnesia:write(Class) || Class <- Classes],
    [mnesia:write(RootObject) || RootObject <- RootObjects],
    [mnesia:write(
       #gmfImmObjectFile{key={FileName, CxpProdId, CxpRev},
			 path=RelPath})
     || #immInfo{path=RelPath, file=FileName} <- ObjRefs],
    ok.


%% @doc Save the given #immInfo{} records and root object descriptors
%% associated with the given CXP. The CXP is identifed by product id
%% and revision.
%%
%% The root object descriptor is a 2-list, TODO: define a proper record

-spec update_cxpRev(string(), string(), string(), list(),
		    [#immInfo{}]) -> ok.
update_cxpRev(CxpProdId, CxpRev, CxpPath, RootObjects, ImmInfos) ->
    Key = {CxpProdId, CxpRev},
    GmfCxpRev = mnesia:wread({gmfCxpRev, Key}),
    NewGmfCxpRev = ucr(GmfCxpRev, Key, CxpPath, RootObjects, ImmInfos),
    mnesia:write(NewGmfCxpRev).

ucr([], Key, CxpPath, RootObjects, ImmInfos) ->
    #gmfCxpRev{key      = Key,
	       imm_root = RootObjects,
	       imm_info = ImmInfos,
	       cxp_path = CxpPath};
ucr([GmfCxpRev], _Key, _CxpPath, RootObjects, ImmInfos) ->
    ImmInfo = GmfCxpRev#gmfCxpRev.imm_info,
    NewImmInfo = lists:append(ImmInfo, ImmInfos),

    ImmRoot = GmfCxpRev#gmfCxpRev.imm_root,
    NewImmRoot = lists:append(ImmRoot, RootObjects),

    GmfCxpRev#gmfCxpRev{imm_info = NewImmInfo,
			imm_root = NewImmRoot}.


init_safs() ->
    {ImmClasses, ImmObj} = get_imm_data(),

    ok = imm_db(ImmClasses, ImmObj).

%% @doc Picks up IMM related info from the gmfCxpRev table.
%% The result is a 2-tuple of lists that are
%% accumulated over all CXPs. The list elements
%% are:
%%
%%   [AbsPath, ...]          final IMM class file abspaths
%%   [AbsPath, ...]          final IMM object file abspaths
%%
%% @end
get_imm_data() ->
    F = fun(#gmfCxpRev{cxp_path = CxpPath,
		       imm_info = ImmInfos},
	    {Acc1, Acc2}) ->
		{ObjImmInfos, ClassImmInfos} =
		    lists:partition(
		      fun(#immInfo{type=Type}) -> Type =:= objects end,
		      ImmInfos),

		{[{CxpPath, ClassImmInfos}|Acc1],
		 [{CxpPath, ObjImmInfos}|Acc2]
		}
	end,

    F2 = fun() -> mnesia:foldr(F, {[], []}, gmfCxpRev) end,

    {atomic, {CxpPathClassImmInfos, CxpPathObjImmInfos}} =
	mnesia:transaction(F2),

    [warning_unnamed_schema(ClassImmInfos) ||
	{_, ClassImmInfos} <- CxpPathClassImmInfos],

    ImmClasses = [im(CxpPath, ClassImmInfos) ||
		     {CxpPath, ClassImmInfos} <- CxpPathClassImmInfos],
    ImmObj = [im(CxpPath, ObjImmInfos) ||
		 {CxpPath, ObjImmInfos} <- CxpPathObjImmInfos],

    {ImmClasses, ImmObj}.

warning_unnamed_schema(ClassImmInfos) ->
    %% This warning was introduced as an aid for system integrators
    %% who need to tell application developers to introduce named
    %% and versioned schemas.
    [logI:write_log("SwmInternal", "GMF", warning,
		    lists:flatten(io_lib:format("unnamed schema: ~s",
						[AbsPath])))
     || #immInfo{type    = unnamedSchema,
		 abspath = AbsPath} <- ClassImmInfos].

%% @doc Import the given list of class definition files (ImmClasses)
%% and the given list of instances files (ImmObj), except if the context
%% is restart as To-version in upgrade. In that case the IMM instances
%% will be restored from a backup of the From-version instances, with
%% optional conversion carried out by the owning application.
%%
%% The first clause of the function gets executed only once during
%% startup. On subsequent invocations the second (no-op) clause is
%% executed.
%%
%% ImmClasses is a list of lists of abspaths to IMM class XML files.
%%
%% ImmObj is a list of lists of abspaths to instances files.

imm_db(ImmClasses, ImmObj) ->
    T0 = ?MonoTime,
    ok = import_imm_classes(ImmClasses),
    T1 = ?MonoTime,
    ok = import_imm_objects(ImmObj, swmI:is_upgrade_ongoing()),
    T2 = ?MonoTime,
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  {'import_imm_classes', sysUtil:time_to_string(T1 - T0)},
			  {'import_imm_objects', sysUtil:time_to_string(T2 - T1)},
			  "-------------- Summary --------------",
			  {'TOTAL', sysUtil:time_to_string(T2 - T0)}]),
    ok.

import_imm_classes(ImmClasses) ->
    ClassFiles = lists:concat(lists:reverse(ImmClasses)),
    ok = safs_imm_db:import_files(ClassFiles, []).

import_imm_objects(_ImmObj, true) ->
    %% do not import instances files if the context is upgrade,
    %% instances will be restored by the gmfImmUgMaster and
    %% by applications over the ICTI interface
    ok;
import_imm_objects(ImmObj, _) ->
    ObjFiles = lists:concat(lists:reverse(ImmObj)),
    ok = safs_imm_db:import_files(ObjFiles, []).

%% @doc TODO, is it in fact the case that the absolute path
%% is looked up (by wildcarding) twice (check the abspath
%% field in the #immInfo{} record)? If so, do it ONLY HERE.

-spec im(string(), [#immInfo{}]) -> [string()].

im(CxpPath, ImmInfos) ->
    [begin
	 AbsPath = filename_join(CxpPath, Rel, File),
	 swmI:find_file(AbsPath) end
     ||#immInfo{path=Rel, file=File} <- ImmInfos].
