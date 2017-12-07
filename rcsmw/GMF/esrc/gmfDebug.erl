%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfDebug.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R5A/R7A/1
%%%
%%% @doc ==GMF debug==
%%% This module contains helpful functions for debugging GMF

-module(gmfDebug).
-vsn('/main/R2A/R3A/R5A/R7A/1').
-date('2016-10-07').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% -----      -------    --------    ------------------------
%%% R2A/1      2012-04-25 etxpeno     Created
%%% ----------------------------------------------------------

-export([help/0,
         tables/1,
         tables/0,
	 mapping/0]).

%%% @doc Shows help text
help() ->
    io:format("~n"
              "=========================================================~n"
              "=== GMF debug functions                               ===~n"
              "=========================================================~n"
              " help()             This help text~n"
              "=================== GMF shortcuts =======================~n"
              " tables()           Show all GMF tables~n"
              " mapping()          Show mapping between LDNs and ObjectIds~n"
              "=========================================================~n~n"
             ).

%%% @doc Print GMF tables in short format
tables() ->
    tables(short).

%%% @doc Print GMF tables in short or long format
tables(Type) ->
    io:format("GMF tables~n"),
    Tabs = [gmfImmClass,
	    gmfImmObject,
	    gmfImmObjectFile,
	    gmfMimClass,
	    gmfMimStruct,
	    gmfMimDerivedType,
	    gmfDnMap,
	    gmfMimChildren,
	    gmfMetaBiDir,
	    gmfCxpRev,
	    gmfVars,
	    gmfMims],

    [sysDebug:tab(Type, Tab) || Tab <- Tabs].

mapping() ->
    sysDebug:tab(short, gmfTrData).
