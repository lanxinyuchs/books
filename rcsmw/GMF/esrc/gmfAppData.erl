%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfAppData.erl %
%%% Author:	etxjotj
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfAppData).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R9A/1').
-date('2017-04-27').
-author('etxberb').
-shaid('b1b28fdbb2ecc670970fb0bf1c770786130b6ca0').
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
%% R3A/2   2015-04-22 erarafo  Added no-op appdata_upgrade/3
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2016-01-13 etxberb  Parallelized appdata routines and removed some
%%                             mnesia transactions.
%% R5A/2   2016-01-20 etxpeno  refactoring after changes in R5A/1
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-09-15 etxpeno  Improvement of MIB sync
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-04-27 etxberb  Added check on 'gmfVars, ac' in ad_complete/0.
%%% ----------------------------------------------------------
%%%
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([appdata_phase/2]).
-export([appdata/3]).
-export([appdata_upgrade/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("gmf.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(GMF_MIM_CLASSES, ["Transport"]).
-define(MonoTime, erlang:monotonic_time()).

-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% @doc
%%   Handle appdata. The given XML element is trusted to have
%%   the 'tag' attribute.
%% @end
%%===========================================================================

-spec appdata_phase(Phase :: atom(),
		    Arguments :: list()) -> ok.

appdata_phase(appdata, [CxpProdId, CxpVersion, AppDataE]) ->
    ad(CxpProdId, CxpVersion, AppDataE);
appdata_phase(appdata_complete, []) ->
    ad_complete();
appdata_phase(A1, A2) ->
    throw({A1, A2}).

ad(CxpProdId, CxpVersion, AppDataE) ->
    appdata(CxpProdId, CxpVersion, AppDataE).

ad_complete() ->
    case mnesia:dirty_read(gmfVars, ac) of
	[{gmfVars, ac, true}] ->
	    ok;
	_ ->
	    ok = init_gmf_mim_classes(),
	    gmfAppImm:ad_complete(),
	    gmfAppMim:ad_complete(),
	    mnesia:dirty_write({gmfVars, ac, true}),
	    ok
    end.

-spec appdata(string(), string(), #xmlElement{}) -> ok.

appdata(CxpProdId, CxpVersion, AppDataE) ->

    mnesia:dirty_delete(gmfVars, ac),

    Tag = gmfMetaLib:getRequiredAttribute(target,
					  AppDataE,
					  CxpProdId++" "++CxpVersion),

    appdata(CxpProdId, CxpVersion, AppDataE, Tag).

appdata(CxpProdId, CxpVersion, AppDataE, ?GMFAPP_MIM) ->
    gmfAppMim:appdata(CxpProdId, CxpVersion, AppDataE);
appdata(CxpProdId, CxpVersion, AppDataE, ?GMFAPP_IMM ) ->
    gmfAppImm:appdata(CxpProdId, CxpVersion, AppDataE);
appdata(CxpProdId, CxpVersion, AppDataE, ?GMFAPP_CLI_EXTENSION) ->
    gmfAppCli:appdata(CxpProdId, CxpVersion, AppDataE).


%%% ----------------------------------------------------------
%%% @doc Callback; see swmAppData:push_appdata_upgrade/1
%%% for call details.
%%% @end
%%% ----------------------------------------------------------
-spec appdata_upgrade(string(), string(), #xmlElement{}) -> ok.

appdata_upgrade(_ProdNo, _ProdRev, _XmlElement) ->
    ok.


update_mim_imm(#gmfMimClass{key      = {MomName, Name} = Key,
			    imm_ns   = ImmNs,
			    cxp_info = {Cxp, Rev}} = MocRec, ok) ->
    ImmClass = ImmNs ++ Name,
    case mnesia:wread({gmfImmClass, ImmClass}) of
	[] ->
	    sysInitI:error_msg("~p:update_mim_imm()~n"
			       "Could not find IMM Class ~p~n"
			       "Cxp ~p~n"
			       "Rev ~p~n"
			       "MOM ~p~n",
			       [?MODULE, ImmClass, Cxp, Rev, MomName]),
	    exit("Error in MOM " ++ MomName);
	[Rec] ->
	    ok = mnesia:write(Rec#gmfImmClass{gmfMimClass = Key}),
	    {ImmRdn, _} = Rec#gmfImmClass.rdn,
	    ok = mnesia:write(MocRec#gmfMimClass{imm_rdn = ImmRdn})
    end,

    case mnesia:wread({gmfMetaBiDir, Key}) of
	[] ->
	    ok;
	[Rec1] ->
	    ok = mnesia:write(Rec1#gmfMetaBiDir{imm_class = ImmClass})
    end.


init_gmf_mim_classes() ->
    %% This is ok for the timebeing. In future, we need to see
    %% if we actually need a copy of all the MIM classes owned
    %% by us (RCS). The class and attribute definitions are needed.
    %% It is ok in case of Transport, which has only key attribute.
    %% May be it is better to parse the comTop.xml.
    [igmc(Class) || Class <- ?GMF_MIM_CLASSES],

    F = fun() ->
		ok = mnesia:foldl(fun update_mim_imm/2, ok, gmfMimClass)
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

igmc(Class = "Transport") ->
    MimName = "ECIM_Top",
    [Rec] = mnesia:dirty_read(gmfImmClass, Class),
    #gmfImmClass{key      = Class,
		 rdn      = {ImmRdn, _},
		 cxp_info = CxpInfo} = Rec,
    Key = {MimName, Class},
    mnesia:dirty_write(Rec#gmfImmClass{gmfMimClass = Key}),

    Attributes = [{"transportId",
		   [{dataType, {string, undefined, undefined}},
		    key,
		    restricted,
		    mandatory]}],
    MimClass = #gmfMimClass{key        = Key,
			    root       = true,   % This is not true
			    parent     = [{"ManagedElement","ECIM_Top"}],
			    attributes = Attributes,
			    imm_rdn    = ImmRdn,
			    cxp_info   = CxpInfo},
    mnesia:dirty_write(MimClass),
    ok;
igmc(_) ->
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
