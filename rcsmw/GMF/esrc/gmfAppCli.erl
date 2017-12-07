%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfAppCli.erl %
%%% Author:	erarafo
%%% Description: Handles CLI Extension appdata.
%%%
%%% Modules used: gmfMetaLib
%%%
%%% ----------------------------------------------------------
-module(gmfAppCli).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/R5A/1').
-date('2016-03-08').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% -----      -------    --------    ------------------------
%%% R2A/1      2013-09-23 erarafo     First version
%%% R2A/2      2013-09-25 erarafo     Metadata parsing adjusted
%%% R2A/3      2013-09-30 erarafo     tag="shared_lib" attribute dropped
%%% R2A/4      2014-03-06 erarafo     Removed dependency towards SWM
%%% R2A/5      2014-03-18 erarafo     Typo fixed
%%% R5A/1      2016-03-08 etxpeno     no mixing between ets:lookup and mnesia:write
%%% ----------------------------------------------------------


-export([appdata/3
	]).

-include("gmf.hrl").
-include_lib("xmerl/include/xmerl.hrl").


-spec appdata(string(), string(), #xmlElement{}) -> ok.

appdata(CxpProdId, CxpRev, AppDataE) ->

    {ok, CxpPath} = swmI:get_cxp_path(CxpProdId, CxpRev),

    Context =
	lists:flatten(
	  io_lib:format("parsing appdata of: ~s", [CxpPath])),

    LoadModuleElements =
	gmfMetaLib:getSubElements(loadmodule, AppDataE),

    SharedLibs =
	getSharedLibList(LoadModuleElements, Context),

    sysInitI:info_msg(
      lists:flatten(io_lib:format(
		      "CLI extension shared libraries: ~p~n",
		      [SharedLibs]))),

    F = fun() ->
		ok = update_cxpRev(CxpProdId, CxpRev, CxpPath, SharedLibs)
	end,

    {atomic, ok} = mnesia:transaction(F),

    ok.


-spec getSharedLibList([#xmlElement{}], string()) -> [#cliSharedLib{}].

getSharedLibList(LoadModuleElements, Context) ->
    lists:flatten(
      [begin
	   FileElements = gmfMetaLib:getSubElements(file, LoadModuleElement),
	   getFileList(FileElements, Context)
       end
       || LoadModuleElement <- LoadModuleElements]).


-spec getFileList([#xmlElement{}], string()) -> [#cliSharedLib{}].

getFileList(FileElements, Context) ->
    [
     begin
	 Attr = gmfMetaLib:getRequiredAttribute(type, FileElement, Context),
	 Type = sysEnv:architectureByMetaData(Attr),
	 if
	     Type =:= false ->
		 Message =
		     lists:flatten(io_lib:format(
				     "~w:~w "
				     "unsupported target architecture in metadata: ~s, "
				     "context: ~s",
				     [?MODULE, ?LINE, Attr, Context])),
		 sysInitI:error_msg(Message),
		 erlang:error(Message, [FileElements]);
	     true ->
		 RelPath=gmfMetaLib:getRequiredAttribute(relpath, FileElement, Context),
		 #cliSharedLib{type=Type, relpath=RelPath}
	 end
     end
     || FileElement <- FileElements].


%% @doc Save the given #cliSharedLib{} records associated with
%% the given CXP. The CXP is identifed by product id and revision.

-spec update_cxpRev(string(), string(), string(), [#cliSharedLib{}]) -> ok.

update_cxpRev(CxpProdId, CxpRev, CxpPath, SharedLibs) ->
    Key = {CxpProdId, CxpRev},
    GmfCxpRev = mnesia:wread({gmfCxpRev, Key}),
    NewGmfCxpRev = ucr(GmfCxpRev, Key, CxpPath, SharedLibs),
    mnesia:write(NewGmfCxpRev).

ucr([], Key, CxpPath, SharedLibs) ->
    #gmfCxpRev{key      = Key,
	       cli_info = SharedLibs,
	       cxp_path = CxpPath};
ucr([GmfCxpRev], _Key, _CxpPath, SharedLibs) ->
    CliInfo = GmfCxpRev#gmfCxpRev.cli_info,
    NewCliInfo = lists:append(CliInfo, SharedLibs),

    GmfCxpRev#gmfCxpRev{cli_info = NewCliInfo}.
