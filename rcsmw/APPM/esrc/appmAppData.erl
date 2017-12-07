%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmAppData.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/6
%%%
%%% @doc ==Application Manager appdata registration==
%%% This module handles the registration appdata with target 'appm'.
%%% Two types of appdata is handled: loadmodule and lmlist.
%%% The loadmodule describes files related to a load module.
%%% The lmlist describes what load modules belong to a board type.
%%%
%%% ----------------------------------------------------------
-module(appmAppData).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/6').
-date('2017-11-22').
-author('etxarnu').
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
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1     20120221   etxarnu   Created
%%% R1A/18    20120820   etxarnu   Changed format for lmdata and
%%%                                added lmlist handling
%% ----    ---------- -------  ------------------------------------------------
%%% R2A/1     20130110   etxarnu   number -> id, revision -> rev
%%%                                in new loadmodule registration
%%% R2A/2     20130126   etxarnu   Added error printouts for old
%%%                                attribute tags
%%% R2A/3     20130306   etxarnu   Handling of updated xml for inc3
%%% R2A/10    20130328   etxarnu   Changed nice to rtCap
%%% R2A/11    20130417   etxarnu   Check for patched files in get_lm_files
%%% R2A/12    20130418   etxpeno   Add new clause in get_lm_files/2
%%% R2A/13    20130418   etxpeno   Add new clause in get_lm_files/2
%%% R2A/14    20130424   etxarnu   More INFO REPORTS to show registration.
%%% R2A/16    20130502   etxarnu   Added get_upi_start_data
%%% R2A/17    20130507   etxarnu   Corrected filter_for_board_type
%%% R2A/18    20130513   etxarnu   Corrected bug in do_add_symlink
%%% R2A/19    20130610   etxarnu   Improved printouts in store_appdata
%%%%                               Removed old format for appdata
%%% R2A/20    20130823   etxarnu   Also handle i386 as architecture file type.
%%% R2A/21    20130828   etxarnu   Also handle i386 as shared_library file type.
%%% R2A/25    20131011   erarafo   Recognize type="armhf" as fit for "arm" host.
%%% R2A/28    20131120   etxarnu   Added program group functions
%%% R2A/29    20131204   etxarnu   Added back "Appl" escalation for bw comp.
%%% R2A/30    20131217   etxarnu   Added get_lm_start_data_from_name/1
%%%                                Fixed bug (handle "None") in 
%%%                                get_pgm_escalation
%%% R2A/31    20140331   etxarnu   Added arm-linux-gnueabi as xml tag for arm
%%% R2A/32    20140401   etxarnu   Now possible to have many boards in lmlist
%%% R2A/34    20140417   etxberb   Added decoding of heartbeat parameters.
%%% R2A/35    20140602   etxarnu   Ignore i686 errors on target in 
%%%                                do_add_symlink
%%% R2A/36    20140603   etxarnu   Ignore ppc errors on arm in do_add_symlink
%% ----    ---------- -------  ------------------------------------------------
%%% R3A/1     20140909   etxarnu   Add cxpNo and cxpRev to appmLmData records
%%% R3A/2     20140917   etxarnu   Now possible to use lmlist if needed per 
%%%                                cxp
%%%                                Also BoardType is now board productnumber
%%% R3A/3     20140922   etxarnu   Added get_cxp_rev/1
%%% R3A/4     20140924   etxarnu   Added appdata_complete/0 and use lmlist
%%%                                when symlinking shared libraries
%%% R3A/5     20140925   etxarnu   Handle case when same NameCxc is used for
%%%                                different loadmodules
%%% R3A/6     20140926   etxarnu   Improved WARNING printout for above
%%% R3A/7     20140929   etxarnu   Added sysAdmin
%%% R3A/8     20140930   etxarnu   Strip all BoardType usage from whiteSpace
%%% R3A/9     20141030   etxarnu   Print ERROR if relpath file does not exist
%%% R3A/10-11 20141118   etxarnu   Handle preload to return new LM paths in 
%%%                                   get_lm_files
%%% R3A/12    20141203   etxarnu   Updated for rev handling of BoardType
%%%                                Added appdata_upgrade, 
%%%                                   appdata_upgrade_complete
%%% R3A/13    20150121   etxpeno   New function: make_symlinks()
%%% R3A/14    20150122   etxarnu   Call get_new_cxp_path in preload
%%% R3A/15    20150128   etxarnu   Use file:make_symlink instead of 
%%%                                   os:cmd("ln -s  ...
%%% R3A/16    20150130   etxarnu   if get_lm_files failes for {BtNo,BtRev}
%%%                                try with only BtNo
%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1     20150424   etxpejn   Added support for esi_local
%%% R4A/3     20150710   eolaand   Added attribute tmpSize in appmPgmData.
%%% R4A/4     20150818   etxarnu   Added register_warm_cb/1
%%% R4A/5     20150825   etxarnu   Don't print info_msg during upgrade
%%% R4A/7     20151103   etxarnu   HU32119: incorrect revision handling
%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1     20151214   etxarnu   Bug in get_lowest_board_data and get_lm_files
%%% R5A/2     20151222   etxarnu   Bug in make_symlinks
%%% R5A/3     20151223   etxarnu   Set default rev to P1A
%%% R5A/4     20151223   etxarnu   Fix dialyzer issue
%%% R5A/5     20160112   etxarnu   Handle arm libs in SIM UP
%%% R5A/7     20160315   etxarnu   Removed symlinking libs for target
%%% R5A/8     20160412   etxarnu   Removed some io:format
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-07-05 etxberb  Added appmHwData & appmHwData_ug.
%% R6A/2   2016-07-08 etxberb  Added handling of appmHwData & appmHwData_ug in
%%                             search functions.
%% R7A/1   2016-09-12 etxarnu  Better error printout in store_appdata if
%%                             loadmodule already stored
%% R7A/2   2016-10-03 uabesvi  HV26316 Timeout in ESI CB
%% R7A/3   2016-10-05 etxarnu  HV26316 Use default value for maxEsiCbTime
%% R7A/4   2016-10-06 etxarnu  WP6081 : Added handling of rollback_esi tag
%% ----    ---------- -------  ------------------------------------------------
%% R8A/1   2016-12-21 etxarnu  WPnnnn: Allow dynamic programs in pgm groups
%% ----    ---------- -------  ------------------------------------------------
%% R9A/2   2017-05-10 etxberb  Changed error to warning in get_file_list/2 &
%%                             get_info/2.
%% R9A/2   2017-05-16 etxberb  store_pgroup_in_lm: Equal PgName gives info
%%                             instead of error.
%% ----    ---------- -------  ------------------------------------------------
%% R10A/1  2017-05-29 etxarnu  WPnnnn: Added tag=restart_cold 
%%                             Added get_cb_apps instead of separate functions
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-08-21 etxarnu  Added possibility to have multiple files to define
%%                             LMs in one program group
%% R11A/2  2017-08-23 etxarnu  Default value for programgroup escalation is BoardWarm
%% ----    ---------- -------  ------------------------------------------------
%% R11A/3  2017-09-29 etxarnu  added dataProcessing
%% R11A/4  2017-10-04 etxarnu  Added delayedKill
%% R11A/5  2017-10-09 etxarnu  Removed export_all and unused functions
%% ----    ---------- -------  ------------------------------------------------
%% R12A/1  2017-10-23 etxarnu  Added register_pgroup_cb/1
%% R12A/2  2017-10-25 etxarnu  Only do make_symlinks for rcssim
%% R12A/3  2017-10-26 etxarnu  Still need symlinks for vrcs
%% R12A/4  2017-11-06 etxarnu  Added register_cold_cb/1
%% R12A/5  2017-11-20 etxarnu  Remove duplicates from appmPgroupData
%% R12A/6  2017-11-22 etxarnu  No symlinks for vrcs (only vrcs32)

%% ------- ---------- -------  ---END-OF-LOG-----------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%-compile(export_all).
-export([appdata/3]).
-export([appdata_complete/0]).
-export([appdata_upgrade/3]).
-export([appdata_upgrade_complete/0]).
-export([get_cb_apps/1]).
-export([get_cxp_no_rev/1]).
-export([get_rollback_esi_apps/0]).
-export([get_esi_apps/0]).
-export([get_esi_local_apps/0]).
-export([get_upi_apps/0]).
-export([get_start_data/0]).
-export([get_start_data/1]).
-export([get_pgroup_data/0]).
-export([get_lm_rev/1]).
-export([get_lm_start_data_from_name/1]).
-export([get_lm_start_data/1]).
-export([get_lm_start_data/2]).
-export([get_lm_files/4]).
-export([get_lm_paths/1]).
-export([get_max_esi_cb_time/1]).
-export([register_warm_cb/1]).
-export([register_pgroup_cb/1]).
-export([register_cold_cb/1]).

-define(DEFAULT_cardiacArrest_noOfHb, 1).
%% General
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(ELSE, true).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
-include_lib("xmerl/include/xmerl.hrl").

-include("appm.hrl").

%%% ----------------------------------------------------------
%%% -type appdata(ProdNo, ProdRev, RegData)
%%%     ok | error().                                       %#
%%% Input:
%%%
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

-spec appdata(ProdNo :: string(), ProdRev :: string(), Data :: tuple()) -> ok.

appdata(ProdNo, ProdRev, #xmlElement{content = Content}) ->
   % sysInitServer:lift_quarantine(?MODULE),
    R = parse_elements(Content,[]),
    Fun = fun() -> store_appdata( false, ProdNo, ProdRev, R) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [Content])
    end.

appdata_upgrade(ProdNo, ProdRev, #xmlElement{content = Content}) ->

    R = parse_elements(Content,[]),
    Fun = fun() -> store_appdata( true, ProdNo, ProdRev, R) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [Content])
    end.




%%% ----------------------------------------------------------
%%% -type appdata_complete()
%%%     ok | error().                                       %#
%%% Input:
%%%
%%% Output:
%%% Exceptions:
%%% Description: Is called after all appdata calls.
%%%              Now only relevant shared libraries will be
%%%              symlinked
%%% ----------------------------------------------------------

-spec appdata_complete() -> ok.

appdata_complete() ->
    case sysEnv:rcs_mode_3() of
	qemusim -> ok;
	target  -> ok;
	vrcs    -> ok;
	_ -> make_symlinks() %old rcssim and vrcs32
    end,
    HwIndepLms = get_common_cxp_lms(get_hw_indep_cxps()),
    mnesia:dirty_write(#appmDataTable{key = hw_indep_lms,
				      value = HwIndepLms}),
    mnesia:dirty_write(#appmDataTable{key = appdata_complete,
				      value = true}),
    store_pgroup_in_lm(),
    ok.

appdata_upgrade_complete() ->
    ok.

get_hw_indep_cxps() ->
    CxpTab=ets:new(allCxpTab,[set]),
    [ets:insert(CxpTab,{X#appmLmData.cxpNo,X#appmLmData.cxpRev}) ||
	X <- ets:tab2list(appmLmData)],
    BtCxpsTab=ets:new(btCxpTab,[set]),
    [ets:insert(BtCxpsTab,
		{element(2,X#appmBoardData.id),
		 element(3,X#appmBoardData.id)})
     || X <- ets:tab2list(appmBoardData)],
    [ets:insert(BtCxpsTab,
		{element(2,X#appmHwData.id),
		 element(3,X#appmHwData.id)})
     || X <- ets:tab2list(appmHwData)],
    ets:tab2list(CxpTab) --  ets:tab2list(BtCxpsTab).

get_common_cxp_lms(CpxIds) ->
    [LM#appmLmData.nameCxc || LM <- ets:tab2list(appmLmData),
			      {X,Y} <- CpxIds,
			      X == LM#appmLmData.cxpNo,
			      Y == LM#appmLmData.cxpRev].

make_symlinks() ->
    BT = board_type(),
    Libs = get_libs(BT),
    make_symlinks(Libs).

make_symlinks([]) ->
    ok;
make_symlinks([L|Libs]) ->
    [AppData] = mnesia:dirty_read(appmLmData,L),
    CxpPath = AppData#appmLmData.cxpPath,
    Files = AppData#appmLmData.files,
    add_sym_links(CxpPath,Files),
    make_symlinks(Libs).

store_pgroup_in_lm()->
    PGs = ets:tab2list(appmPgroupData),
    [store_pgroup_in_lm(PG) || PG <- PGs].

store_pgroup_in_lm(#appmPgroupData{name = Name, lms = LMs}) ->
    [store_pgroup_in_lm(Name,LM) || LM <- LMs].
    
store_pgroup_in_lm(PgName, #lmProdId{nameCxc=LmNameCxc}) ->
    case  mnesia:dirty_read(appmLmData,LmNameCxc) of
	[AppData] ->
	    case  AppData#appmLmData.tag of
		Tag when Tag == "central";
			 Tag == "local";
			 Tag == "dynamic" ->
		    case AppData#appmLmData.pgroup of
			undefined ->
			    mnesia:dirty_write(AppData#appmLmData{pgroup = PgName});
			PgName ->
			    appmLib:log(info,
					"~p:~p(~p,~p) LM already belongs to ~p~n",
					[?MODULE, ?FUNCTION_NAME,PgName, LmNameCxc,PgName]),
			    ok;
			OldPg ->
			    appmLib:log(error,
					"~p:~p(~p,~p) LM already belongs to ~p~n",
					[?MODULE, ?FUNCTION_NAME,PgName, LmNameCxc,OldPg]),

			    throw(same_lm_in_multiple_pgroup)
		    end;
		_ ->
		    ok
	    end;
	[] ->
	    appmLib:log(error,
			"~p:~p(~p,~p) LM defined in PG does not exist~n",
			[?MODULE, ?FUNCTION_NAME,PgName, LmNameCxc])

    end.

		    
    

%%% ----------------------------------------------------------
%%% -type get_lm_files(IsPreload,HwType,BoardType,Tag)
%%%
%%% Input:
%%%
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%% -spec get_lm_files(IsPreload :: boolean(), BoardType :: string(), Tag :: string()) ->
%%%  [{LmName :: string(), LmNo :: string(), LmRev :: string(),
%%%   [{FileType ::string(), FilePath :: string()}..] }..] .

get_lm_files(false, {"", ?DEFAULT_HwModel}, {"", ?DEFAULT_BoardRev}, Tag) ->
    Pattern = #appmLmData{tag = Tag,
			  _   = '_'},
    LMs = mnesia:dirty_match_object(Pattern),
    try
	lists:map(
	  fun(LM) ->
		  FileList = get_file_list(LM#appmLmData.cxpPath,
					   LM#appmLmData.files),
		  {Name, No} = LM#appmLmData.nameCxc,
		  {Name, No, LM#appmLmData.rev, FileList}
	  end, LMs)
    catch
	throw:nonexistent_file -> {error,nonexistent_file}
    end;

get_lm_files(true, {"", ?DEFAULT_HwModel}, {"", ?DEFAULT_BoardRev}, Tag) ->
    Pattern = #appmLmData_ug{tag = Tag,
			     _   = '_'},
    LMs = mnesia:dirty_match_object(Pattern),
    try
	lists:map(
	  fun(LM) ->
		  FileList = get_file_list(LM#appmLmData_ug.cxpPath,
					   LM#appmLmData_ug.files),
		  {Name, No} = LM#appmLmData_ug.nameCxc,
		  {Name, No, LM#appmLmData_ug.rev, FileList}
	  end, LMs)
    catch
	throw:nonexistent_file -> {error,nonexistent_file}
    end;

get_lm_files(IsPreload, HwType, BoardType, Tag) ->
    {BtNo, BtRev} = strip_whitespace(BoardType),
    {HtCat, HtMod} = HT = strip_whitespace(HwType),
    MsBt = make_ms(IsPreload, boardType, BtNo),
    MsHt = make_ms(IsPreload, hwType, HT),
    MsHtWc = make_ms(IsPreload, hwType, {HtCat, ?WILDCARD_HwDataTbl}),
    case HtMod of
	?DEFAULT_HwModel ->
	    %% The search and match for all HwModels in DataTbl is covered by
	    %% the MsHt match below.
	    HtWildCardMatch = [];
	_ ->
	    %% Include also wildcarded HwModel specifications in DataTbl.
	    HtWildCardMatch = mnesia:dirty_match_object(MsHtWc)
    end,
    case
	{mnesia:dirty_match_object(MsBt), (mnesia:dirty_match_object(MsHt) ++
					   HtWildCardMatch)}
	of
	{[], []} ->
	    {error, nonexistent_file};
	{BoardData, HwData} ->
	    try
		LmLists = pick_lmlist(IsPreload, BtRev, BoardData ++ HwData),
		R = [get_lm_files_for_tag(IsPreload, LMs, Tag, []) ||
			X <- LmLists,
			X =/= undef,
			LMs <- [get_lms(IsPreload, X)]],
		lists:flatten(R)
	    catch
		throw : nonexistent_file ->
		    {error, nonexistent_file}
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pick_lmlist(IsPreload,Rev,Lms) ->
    P = split_per_cxp(IsPreload,Lms),
    lists:flatten([begin
		       S = sort_boardlist(IsPreload,L),
		       pick_lmlist(IsPreload,Rev,S,undef)
		   end
		   || {_,L} <- P]).


pick_lmlist(_IsPreload,_Rev,[],Acc) ->  
    Acc;
pick_lmlist(IsPreload,Rev,[H|T], Acc) when is_record(H, appmBoardData) orelse
					   is_record(H, appmBoardData_ug) ->
    HRev = get_rev(IsPreload,H),
    case appmProdIdCheck:check_prod_rev(Rev,HRev) of
	true ->
	    pick_lmlist(IsPreload,Rev,T, H);
	false ->
	    Acc
    end;
pick_lmlist(_, _, L, _) ->
    L.
    
sort_boardlist(IsPreload,L) ->    
    Fun=fun(A,B) ->
	      ARev = get_rev(IsPreload,A),
	      BRev = get_rev(IsPreload,B),
	      appmProdIdCheck:check_prod_rev(BRev,ARev)
      end, 
    lists:sort(Fun,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_per_cxp(IsPreload,L) ->
    split_per_cxp(IsPreload,L,[]).
split_per_cxp(_IsPreload,[],Acc) ->
    Acc;
split_per_cxp(IsPreload,[H|T],Acc) ->
    Cxp = get_cxp(IsPreload,H),
    NewAcc = sort_by_cxp(Cxp,H,Acc),
    split_per_cxp(IsPreload,T,NewAcc).

sort_by_cxp(Cxp,H,Acc) ->
    case lists:keyfind(Cxp,1,Acc) of
	false ->
	    [{Cxp,[H]}|Acc];
	{Cxp,L} ->
	    lists:keyreplace(Cxp,1,Acc,{Cxp,[H|L]})
    end.
     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_ms(false, boardType, BtNo) ->
    #appmBoardData{id = {{BtNo, '_'}, '_', '_'},
		   _ = '_'};
make_ms(true, boardType, BtNo) ->
    #appmBoardData_ug{id = {{BtNo, '_'}, '_', '_'},
		      _ = '_'};
make_ms(false, hwType, HwType) ->
    #appmHwData{id = {HwType, '_', '_'},
		_ = '_'};
make_ms(true, hwType, HwType) ->
    #appmHwData_ug{id = {HwType, '_', '_'},
		   _ = '_'}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_lms(false, #appmBoardData   {lms=LMs} ) -> LMs;
get_lms(true,  #appmBoardData_ug{lms=LMs} ) -> LMs;
get_lms(false, #appmHwData   {lms=LMs} ) -> LMs;
get_lms(true,  #appmHwData_ug{lms=LMs} ) -> LMs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_rev(false, #appmBoardData   {id={{_,Rev},_,_}} ) -> Rev;
get_rev(true,  #appmBoardData_ug{id={{_,Rev},_,_}} ) -> Rev;
get_rev(_,  _) -> ?DEFAULT_BoardRev.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_cxp(false, #appmBoardData   {id={_,Cxp,_}} ) -> Cxp;
get_cxp(true,  #appmBoardData_ug{id={_,Cxp,_}} ) -> Cxp;
get_cxp(false, #appmHwData   {id={_,Cxp,_}} ) -> Cxp;
get_cxp(true,  #appmHwData_ug{id={_,Cxp,_}} ) -> Cxp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_lm_files_for_tag(_IsPreload,[],_Tag,Acc) ->
    Acc;
get_lm_files_for_tag(IsPreload,[#lmProdId{nameCxc = NameCxc = {Name,No},
				rev = Rev}|T],Tag,Acc) ->

    case find_lm_files(IsPreload,NameCxc, Rev,Tag) of
	false ->
	    get_lm_files_for_tag(IsPreload,T,Tag,
				 [{error,{lm_not_found,Name,No,Rev}}|Acc]);
	{FoundRev,Files} ->
	    get_lm_files_for_tag(IsPreload,T,Tag,
				 [{Name,No,FoundRev,Files}|Acc])

    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_lm_files(false,NameCxc,Rev,Tag) ->
    MH = #appmLmData{nameCxc = NameCxc,
		     tag=Tag,
		     _='_'},

    case mnesia:dirty_match_object(MH) of
	[] ->
	    false;
	LmDataRecs  ->
	    LMs = [ [L#appmLmData.rev,
		     L#appmLmData.cxpPath,
		     L#appmLmData.files] || L <- LmDataRecs,
					    appmProdIdCheck:check_prod_rev(
					      L#appmLmData.rev, Rev)],
	    case LMs of
		[] ->
		    false;
		LMs ->

		    {CxpPath, LmRev,Files} = find_highest_rev(LMs),
		    {LmRev, get_file_list(CxpPath, Files)}
	    end
    end;

find_lm_files(true,NameCxc,Rev,Tag) ->
    MH = #appmLmData_ug{nameCxc = NameCxc,
			tag=Tag,
			_='_'},

    case mnesia:dirty_match_object(MH) of
	[] ->
	    false;
	LmDataRecs  ->
	    LMs =  [[L#appmLmData_ug.rev,
		     L#appmLmData_ug.cxpPath,
		     L#appmLmData_ug.files] || L <- LmDataRecs,
					       appmProdIdCheck:check_prod_rev(
						 L#appmLmData_ug.rev, Rev)],
	    case LMs of
		[] ->
		    false;
		LMs ->
		    {CxpPath, LmRev,Files} = find_highest_rev(LMs),
		    {LmRev, get_file_list(CxpPath, Files)}
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_highest_rev([[LmRev,Cxp,Fil]|Rest]) ->
    find_highest_rev([[LmRev,Cxp,Fil]|Rest],[LmRev,Cxp,Fil] ).

find_highest_rev([],[OldLmRev,OldCxp,OldFil] ) ->
    {OldCxp,OldLmRev,OldFil};
find_highest_rev([[LmRev,Cxp,Fil]|Rest],[OldLmRev,OldCxp,OldFil] ) ->
    case appmProdIdCheck:check_prod_rev(LmRev, OldLmRev) of
	true ->
	    find_highest_rev(Rest,[LmRev,Cxp,Fil] );
	false ->
	    find_highest_rev(Rest,[OldLmRev,OldCxp,OldFil] )
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_file_list(CxpPath, Files) ->
    Fun=fun(F) ->
		Type = F#file.type,
		Tpath = filename:join(CxpPath, F#file.relpath),
		case filelib:wildcard(Tpath) of
		    [TmpTgt] ->
			%% Check if patched file exists
			Tgt = swmI:find_file(TmpTgt),
			[{Type, Tgt}];
		    _Other ->
			appmLib:log(warning,
				    "~p:no file found at: ~n"
				    "~p~n", [?MODULE, Tpath]),

			throw(nonexistent_file)
		end
	end,
    lists:flatmap(Fun, Files).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_warm_cb(Module) when is_atom(Module) -> 
    register_cb(warm_cb,Module).
register_cold_cb(Module) when is_atom(Module) -> 
    register_cb(cold_cb,Module).
register_pgroup_cb(Module) when is_atom(Module) -> 
    register_cb(pgroup_cb,Module).

register_cb(CB,Module) ->
    {atomic, ok} =
	mnesia:transaction(
	  fun() ->
		  [{appmDataTable, CB, Modules}] =
		      mnesia:read({appmDataTable, CB}), 

		  case lists:member(Module, Modules) of
		      true ->
			  ok;
		      false ->
			  mnesia:write({appmDataTable, CB,
					[Module|Modules]})
		  end

	  end),
    ok.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

						%
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

parse_elements([],Acc) ->
    lists:reverse(Acc);
parse_elements([#xmlElement{name = Name,
			    attributes = Attributes,
			    content = Content} | T] , Acc) ->
    parse_elements(T,[ {Name,
			parse_attributes(Attributes,[]) ,
			parse_elements(Content,[])
		       } | Acc]);
parse_elements([_H|T],Acc) ->
    parse_elements(T,Acc).



%%% ----------------------------------------------------------
parse_attributes([],Acc) ->
    lists:reverse(Acc);
parse_attributes([#xmlAttribute{ name = Name, value = Value}|T],Acc) ->
    parse_attributes(T,[{Name,Value}|Acc]).

%%% ----------------------------------------------------------
parse_lm(_CxpPath,[],Acc) ->
    lists:reverse(Acc);
parse_lm(CxpPath,[{file,Attrs,[]}|T],Acc) ->
    Type = attr(type, Attrs),
    RelPath = attr(relpath,Attrs),
    AbsPath = filelib:wildcard(filename:join(CxpPath,RelPath)),
    case file:read_file_info(AbsPath) of
	{error,enoent} ->
	    appmLib:log(error,"~p: File  ~n~p~n does not exist~n",
				   [?MODULE,CxpPath++"/"++RelPath]);
	_ ->
	    ok
    end,
    parse_lm(CxpPath,T,
	     [#file{type = Type,
		    relpath = RelPath}|Acc]);
parse_lm(CxpPath,[_|T],Acc) ->
    parse_lm(CxpPath,T,Acc).


%%% ----------------------------------------------------------
parse_lmlist([],Acc) ->
    lists:reverse(Acc);
parse_lmlist([{lm,Attrs,[]}|T],Acc) ->
    parse_lmlist(T,
		 [#lmProdId{nameCxc = {attr(name, Attrs), attr(id, Attrs)},
			    rev = attr(rev,Attrs)}|Acc]);
parse_lmlist([_|T],Acc) ->
    parse_lmlist(T,Acc).




%%% ----------------------------------------------------------
parse_lmlist_bt([],Acc) ->
    lists:reverse(Acc);
parse_lmlist_bt([{boardType,Attrs,[]}|T],Acc) ->
    case attr(rev, Attrs) of
	undefined -> % no rev given, assume P1A
	    parse_lmlist_bt(T,
			    [{attr(id, Attrs),?DEFAULT_BoardRev}|Acc]);
	Rev ->
	    parse_lmlist_bt(T,
			    [{attr(id, Attrs),Rev}|Acc])
    end;
parse_lmlist_bt([_|T],Acc) ->
    parse_lmlist_bt(T,Acc).

%%% ----------------------------------------------------------
parse_lmlist_ht([{hwType, Attrs, []} | Tail], Acc) ->
    case attr(model, Attrs) of
	undefined ->
	    Model = ?WILDCARD_HwDataTbl;
	Model ->
	    ok
    end,
    parse_lmlist_ht(Tail, [{attr(category, Attrs), Model} | Acc]);
parse_lmlist_ht([_ | Tail], Acc) ->
    parse_lmlist_ht(Tail, Acc);
parse_lmlist_ht([], Acc) ->
    lists:reverse(Acc).




%%% ----------------------------------------------------------
store_appdata(IsPreload, ProdNo, ProdRev,[{loadmodule,Attrs,Content}|Rest]) ->
    {ok,CxpPath} = case IsPreload of
		       false ->
			   swmI:get_cxp_path(ProdNo, ProdRev);
		       true ->
			   swmI:get_new_cxp_path(ProdNo, ProdRev) %to new CXP during UG
		   end,
    Name  = attr(name,Attrs),
    case attr(tag,Attrs) of
	undefined ->
	    appmLib:log(error,
			"~n~p:Missing 'tag' in loadmodule definition for ~p in ~p~n"
			"Please update to correct format ~n"
			"See LMHI IWD:~n"
			"https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/dev/"
			"RCP_CSX10179/RCS_CRX901266/APPM/APPM_CNX9012632/doc/15519/LmhIwd.doc ~n",
			[?MODULE, Name, CxpPath ]),
	    store_appdata(IsPreload, ProdNo, ProdRev,Rest);

	Tag ->
	    %%New loadmodule format
	    Files = parse_lm(CxpPath,Content,[]),
	    No = get_attr(id,Attrs,CxpPath++":"++Name),
	    Rev = get_attr(rev,Attrs,CxpPath++":"++Name),
	    XmlNameCxc = {Name, No},
	    MpInfo = get_info(CxpPath,Files),
	    LmDataTable =
		if
		    IsPreload -> appmLmData_ug;
		    true ->  appmLmData
		end,
	    NameCxc =
		case mnesia:read(LmDataTable,XmlNameCxc) of
		    [] ->
			XmlNameCxc;
		    [OldRec] when Tag == "shared_lib"->
			NewName = Name ++ "_renamed",
			NewNameCxc = {NewName, No},
			appmLib:log(warning,
				    "~p:store_appdata - loadmodule (shared_lib) for NameCxc= ~p  ~n"
				    "with files ~p ~n"
				    "in CXP ~p ~n"
				    "is  already defined for ~n~p~n"
				    "Will rename ~p to ~p ~n",
				    [?MODULE, XmlNameCxc, Files, CxpPath, OldRec , Name, NewName ]),
			NewNameCxc;
		    [OldRec] ->
			appmLib:log(error,
				    "~p:store_appdata - loadmodule (Tag=~p) for NameCxc= ~p  ~n"
				    "in CXP ~p ~n"
				    "is  already defined for ~n~p~n",
				    [?MODULE, Tag, XmlNameCxc, CxpPath, OldRec ]),
			erlang:error({aborted,loadmodule_already_defined})
			
		end,

	    R = if
		    IsPreload ->
			#appmLmData_ug{
			   nameCxc = NameCxc,
			   rev     = Rev,
			   tag     = Tag,
			   date    = attr(date,Attrs),
			   cxpNo   = ProdNo,
			   cxpRev  = ProdRev,
			   cxpPath = CxpPath,
			   files   = Files,
			   mpInfo  = MpInfo
			  };
		   true ->
			#appmLmData{
			   nameCxc = NameCxc,
			   rev     = Rev,
			   tag     = Tag,
			   date    = attr(date,Attrs),
			   cxpNo   = ProdNo,
			   cxpRev  = ProdRev,
			   cxpPath = CxpPath,
			   files   = Files,
			   mpInfo  = MpInfo
			  }
		end,
	    info_msg(
	      "~p:store_appdata - loadmodule, Tag: ~p~n"
	      "CxpPath: ~p~n"
	      "Name: ~p, No: ~p, Rev : ~p~n"
	      "Files: ~p ~n"
	      "MpInfo: ~p~n ~n",
	      [?MODULE, Tag, CxpPath, Name, No, Rev, Files, MpInfo]),
	    ok = mnesia:write(R),
	    if
		IsPreload -> ok;
		true ->  update_mp_apps(Tag,NameCxc)
	    end,
	    store_appdata(IsPreload, ProdNo, ProdRev,Rest)
    end;


store_appdata(IsPreload, ProdNo, ProdRev,[{lmlist,Attrs,Content}|Rest]) ->
    LMs = parse_lmlist(Content, []),
    BTL =
	case attr(boardType, Attrs) of
	    undefined -> % boardType as element
		parse_lmlist_bt(Content, []);
	    BT1 -> % old style with boardType as attribute
		[{BT1, ?DEFAULT_BoardRev}]
	end,
    HTL = parse_lmlist_ht(Content, []),
    store_appdata_lmlist(BTL, IsPreload, boardType, LMs, ProdNo, ProdRev),
    store_appdata_lmlist(HTL, IsPreload, hwType, LMs, ProdNo, ProdRev),
    store_appdata(IsPreload, ProdNo, ProdRev, Rest);

store_appdata(true, ProdNo, ProdRev, [_Other|Rest]) ->
    store_appdata(true, ProdNo, ProdRev, Rest);

store_appdata(IsPreload, ProdNo, ProdRev,[{programgroup,Attrs,Content}|Rest]) ->
    LMs = parse_lmlist(Content,[]),
    Name  = attr(name,Attrs),
    Esc  = attr(escalation,Attrs,"BoardWarm"),
    MaxR  = attr_int(maxRestarts,Attrs,?MAX_GRP_RST_DEF_INT),
    MaxT  = attr_int(maxTime,Attrs, ?MAX_GRP_TMO_DEF_INT ),
    R = case mnesia:read(appmPgroupData,Name) of
	    [] ->
		#appmPgroupData{ name = Name,
				 escalation = Esc,
				 maxRestarts = MaxR,
				 maxTime = MaxT,
				 lms   = LMs};
	    [PG] ->
		%% ensure that duplicate LMs are not stored in appmPgroupData
		PG#appmPgroupData{lms = (PG#appmPgroupData.lms -- LMs)++ LMs}
	end,
    info_msg(
      "~p:store_appdata - programgroup, Name: ~p~n"
      "Product: ~p ~p ~n"
      "Escalation: ~p ~n"
      "MaxR: ~p ~n"
      "MaxT: ~p ~n"
      "Added LMs: ~p ~n"
      "Total LMs: ~p ~n",
      [?MODULE, Name, ProdNo, ProdRev,Esc, MaxR, MaxT,
       LMs, R#appmPgroupData.lms]),
    ok = mnesia:write(R),
    store_appdata(IsPreload, ProdNo, ProdRev, Rest);

store_appdata(IsPreload, ProdNo, ProdRev, [_Other|Rest]) ->
    store_appdata(IsPreload, ProdNo, ProdRev, Rest);
store_appdata(_IsPreload, _ProdNo, _ProdRev, []) ->
    ok.

%%% ----------------------------------------------------------
store_appdata_lmlist([Attrs | Tail], IsPreload, Type, LMs, ProdNo, ProdRev) ->
    Id = {strip_whitespace(Attrs), ProdNo, ProdRev},
    {TblName, Obj} = tbl_obj(IsPreload, Type, Id, LMs),
    case mnesia:read(TblName, Id) of
	[] ->
	    info_rep([{?MODULE, ?FUNCTION} |
		      rec_info(TblName, Obj)]),
	    ok = mnesia:write(Obj);
	[OldObj] ->
	    appmLib:log(error,
			"~p:lmlist failed for ~p  with LMs~n~p~n"
			"lmlist already defined for LMs~n~p~n"
			"Merge them into one lmlist for this LMC~n",
			[?MODULE, Id, LMs, lms(TblName, OldObj)])
    end,
    store_appdata_lmlist(Tail, IsPreload, Type, LMs, ProdNo, ProdRev);
store_appdata_lmlist([], _, _, _, _, _) ->
    ok.

%%% ----------------------------------------------------------
lms(appmBoardData, Rec) ->
    Rec#appmBoardData.lms;
lms(appmBoardData_ug, Rec) ->
    Rec#appmBoardData_ug.lms;
lms(appmHwData, Rec) ->
    Rec#appmHwData.lms;
lms(appmHwData_ug, Rec) ->
    Rec#appmHwData_ug.lms.

%%% ----------------------------------------------------------
tbl_obj(false, boardType, Id, LMs) ->
    {appmBoardData, #appmBoardData{id  = Id, lms = LMs}};
tbl_obj(true, boardType, Id, LMs) ->
    {appmBoardData_ug, #appmBoardData_ug{id  = Id, lms = LMs}};
tbl_obj(false, hwType, Id, LMs) ->
    {appmHwData, #appmHwData{id  = Id, lms = LMs}};
tbl_obj(true, hwType, Id, LMs) ->
    {appmHwData_ug, #appmHwData_ug{id  = Id, lms = LMs}}.

%%% ----------------------------------------------------------
rec_info(appmBoardData, Record) ->
    sysUtil:record_format(record_info(fields, appmBoardData), Record);
rec_info(appmBoardData_ug, Record) ->
    sysUtil:record_format(record_info(fields, appmBoardData_ug), Record);
rec_info(appmHwData, Record) ->
    sysUtil:record_format(record_info(fields, appmHwData), Record);
rec_info(appmHwData_ug, Record) ->
    sysUtil:record_format(record_info(fields, appmHwData_ug), Record).

%%% ----------------------------------------------------------
info_msg(Str,Args) ->
    case swmI:is_upgrade_ongoing() of
	false ->
	    sysInitI:info_msg(Str,Args);
	true ->
	    ok
    end.

%%% ----------------------------------------------------------
info_rep(Args) ->
    case swmI:is_upgrade_ongoing() of
	false ->
	    sysInitI:info_report(Args);
	true ->
	    ok
    end.

%%% ----------------------------------------------------------
get_pgroup_data() ->
    case mnesia:dirty_read(appmDataTable,pgroup_data) of
	[] ->
	    L = ets:tab2list(appmPgroupData),
	    Val=
		[{X#appmPgroupData.name,
		  X#appmPgroupData.escalation,
		  X#appmPgroupData.maxRestarts,
		  X#appmPgroupData.maxTime,
		  get_valid_pgroup_lms( X#appmPgroupData.lms)} || X <- L],
	    mnesia:dirty_write(#appmDataTable{key = pgroup_data,
					      value = Val}),
	    Val;
	[#appmDataTable{value=Val}] ->
	    Val
    end.


get_valid_pgroup_lms(Lms) ->
    get_valid_pgroup_lms(Lms,[]).

get_valid_pgroup_lms([],Acc) ->
    Acc;
get_valid_pgroup_lms([#lmProdId{nameCxc=NameCxc,rev=Rev}|Rest],Acc) ->
    case mnesia:dirty_read(appmLmData,NameCxc) of
	[#appmLmData{tag="central"}] ->
	    get_valid_pgroup_lms(Rest,[{NameCxc,Rev}|Acc]);
	[#appmLmData{tag="dynamic"}] ->
	    get_valid_pgroup_lms(Rest,[{NameCxc,Rev}|Acc]);
	[#appmLmData{tag="local"}] ->
	    get_valid_pgroup_lms(Rest,[{NameCxc,Rev}|Acc]);
	[#appmLmData{tag=Tag}]  ->
	    appmLib:log(error,
			"~p:get_valid_pgroup_lms: ~p with tag = ~p  cannot be part of a "
			"MP Program Group, ignoring~n",
			[?MODULE,NameCxc,Tag]),
	    get_valid_pgroup_lms(Rest,Acc);
	_ ->
	    appmLib:log(error,
			"~p:get_valid_pgroup_lms: ~p does not exist in database~n",
			[?MODULE,NameCxc]),
	    get_valid_pgroup_lms(Rest,Acc)
    end.



%%% ----------------------------------------------------------
get_attr(Attr,Attrs,Name) ->
    case attr(Attr,Attrs) of
	undefined ->
	    missing_attribute(Name, Attr),
	    undefined;
	AttrVal  ->
	    AttrVal
    end.

missing_attribute(Name,Attr) ->
     appmLib:log(error,
      "~n~p:Missing attribut (~p) in loadmodule definition for ~p~n"
      "Please update the loadmodule appdata definition xml file.~n"
      "See LMHI IWD:~n"
      "https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/APPM/APPM_CNX9012632/doc/15519/LmhIwd.doc ~n",
      [?MODULE, Attr, Name]).



%%% ----------------------------------------------------------
add_sym_links(_,[]) ->
    ok;
add_sym_links(CxpPath,[#file{type=Arch,relpath=Lib}|T]) ->
    do_add_symlink(CxpPath, Arch, Lib),
    add_sym_links(CxpPath,T).

%%% ----------------------------------------------------------
%%% Handle one symlink

do_add_symlink(CxpPath, ArchExternal, Lib) ->
    Arch = map_arch_appdata_to_internal(ArchExternal),
    
    Tpath = filename:join(CxpPath,Lib),
    LibName = filename:basename(Lib),
    case filelib:wildcard(Tpath) of
	[Tgt] ->
	    Src = filename:join([sysEnv:library_root(), Arch, LibName]),
	    case file:make_symlink(Tgt, Src) of
		ok ->
		    info_msg("~p:  Added symlink ~p~n to ~p~n",
					  [?MODULE,Tgt,Src]),
		    ok;
		{error,LnError} ->
		    {HwArch,_} =  sysEnv:architecture(),
		    case Arch of
			HwArch ->
			    {ok,OldLink} = file:read_link(Src),
		
			    appmLib:log(error,
					"~p:Cannot link library:~n~p~n"
					"due to : ~p~n"
					"conflicting file : ~p~n",
					[?MODULE,Tpath,LnError, OldLink] );
			_ -> ok %ignore links for other architectures.
		    end
	    end;

	[] ->
	     appmLib:log(error,"~p:no lib found at: ~n~p~n",
				   [?MODULE,Tpath]);
	TgtList ->
	     appmLib:log(error,"~p:multiple libs found at: ~n~p~n~p~n",
				   [?MODULE, Tpath, TgtList])
    end.

%%% ----------------------------------------------------------

update_mp_apps(Tag,App) ->
    case lists:member(Tag,?MP_TAGS) of
	true ->
	    [#appmDataTable{value=OldApps}] = mnesia:read(appmDataTable,Tag),
	    mnesia:write(#appmDataTable{key=Tag,
					value = OldApps ++ [App]});
	false ->
	    ok
    end.



%%% ----------------------------------------------------------
attr(Key,List) ->
    attr(Key,List,undefined).

attr(Key,List,DefVal) ->
    case lists:keyfind(Key,1,List) of
	{Key,Val} ->
	    Val;
	_ ->
	    DefVal
    end.

attr_int(Key,List) ->
    attr_int(Key,List,undefined).

attr_int(Key,List,DefVal) ->
    case lists:keyfind(Key,1,List) of
	{Key,Val} ->
	    list_to_integer(Val);
	_ ->
	    DefVal
    end.

%%% ----------------------------------------------------------
get_start_data() ->
    case sysEnv:role() of
	regular ->
	    get_start_data(regular);
	_ ->
	    get_start_data(core)
    end.


get_start_data(core) ->
    BT = board_type(),
    case mnesia:dirty_read(appmDataTable, {core,BT}) of
	[] ->
	    SS = get_core_apps(BT),
	    Val = [{LM,get_lm_start_data(LM)} || LM <- SS],
	    mnesia:dirty_write(#appmDataTable{key={core,BT},
					      value =Val}),
	    Val;
	[#appmDataTable{value = Val}] -> Val
    end;
get_start_data(regular) ->
    BT = board_type(),
    case mnesia:dirty_read(appmDataTable, {regular,BT}) of
	[] ->
	    SS = get_regular_apps(board_type()),
	    Val = [{LM,get_lm_start_data(LM)} || LM <- SS],
	    mnesia:dirty_write(#appmDataTable{key={regular,BT},
					      value =Val}),
	    Val;
	[#appmDataTable{value = Val}] -> Val
    end;

get_start_data(Pgms) when is_list(Pgms) ->
    [{LM,get_lm_start_data(LM)} || LM <- Pgms,
				      is_dynamic(LM) == false].

board_type() ->
    PI = eqs_pri_service:get_product_information_data(),
    No =  proplists:get_value(productNumber,PI),
    Rev =  proplists:get_value(productRevision,PI),
    {No,Rev}.

%% %%% ----------------------------------------------------------
get_regular_apps(BT) ->
    [LA] = mnesia:dirty_read(appmDataTable, "local"),
    Progs = LA#appmDataTable.value,
    F = filter_for_board_type(Progs, BT),
    F -- (F -- Progs).

%% %%% ----------------------------------------------------------
get_core_apps(BT) ->
    [CA] = mnesia:dirty_read(appmDataTable, "central"),
    [LA] = mnesia:dirty_read(appmDataTable, "local"),
    Progs = LA#appmDataTable.value ++ CA#appmDataTable.value,
    F = filter_for_board_type(Progs, BT),
    F -- (F -- Progs).


get_libs(BT) ->
    [SL] = mnesia:dirty_read(appmDataTable, "shared_lib"),
    Libs = SL#appmDataTable.value,
    F = filter_for_board_type(Libs, BT),
    F -- (F -- Libs).

%% %%% ----------------------------------------------------------
filter_for_board_type(Progs,BoardType) when is_tuple(BoardType) ->
    {BtNo,BtRev} = strip_whitespace(BoardType),
    Pattern=#appmBoardData{id={{BtNo,'_'},'_','_'}, _='_'},
    case mnesia:dirty_match_object( Pattern) of
	[] -> % If no BoardData defined assume all (backward compatible)
	    Progs;
	BoardData ->
	    All =
		[begin
		     LmProdIds = BD#appmBoardData.lms,
		     BoardProgs = [X#lmProdId.nameCxc || X <- LmProdIds],
		     {{BtNo,Rev},CxpNo,_} = BD#appmBoardData.id,
		     case  appmProdIdCheck:check_prod_rev(BtRev,Rev) of
			 true -> {CxpNo, BoardProgs};
			 false -> {"",""}
		     end
		 end
		 || BD <- BoardData],
	    {Cxps,BoardLms} = lists:unzip(All),
	    GlobalCxpLms = remove_lms(Cxps,Progs),
	    lists:flatten(GlobalCxpLms ++ BoardLms)
    end.

remove_lms([],Progs) ->
    Progs;
remove_lms([Cxp|Rest],Progs) ->
    RProgs =
	[NameCxc || NameCxc <- Progs,
		    AP <- mnesia:dirty_read(appmLmData,NameCxc),
		    AP#appmLmData.cxpNo /= Cxp],
    remove_lms(Rest,RProgs).


get_rollback_esi_apps() ->
    get_cb_apps("rollback_esi").
get_esi_apps() ->
    get_cb_apps("esi").
get_esi_local_apps() ->
    get_cb_apps("esi_local").
get_upi_apps() ->
    get_cb_apps("upi").

get_cb_apps(Tag) ->
    [X] = mnesia:dirty_read(appmDataTable, Tag),
    X#appmDataTable.value.



%%% ----------------------------------------------------------
get_lm_paths(NameCxc) ->
    {ok, CxpPath, RelPath, _Info, _} = get_lm_start_data(NameCxc),
    {ok, CxpPath, RelPath}.

%% %%% ----------------------------------------------------------
get_lm_start_data_from_name(Name) when is_list(Name) ->
    MH = #appmLmData{nameCxc = {'$1', '$2'},
		     cxpPath = '$3',
		     files   = '$4',
		     mpInfo  = '$5',
		     pgroup  = '$6',
                     _       = '_'},
    Result = {{{{'$1', '$2'}}, '$3', '$4', '$5', '$6'}},
    case mnesia:dirty_select(appmLmData,
			     [{MH, [{'=:=','$1',Name}], [Result]}]) of
	[{NameCxc, CxpPath, Files, Info, Pgroup}] ->
	    case get_bin_file(Files) of
		error ->
		    {error, NameCxc, binfile_not_found};

		BinFile ->
		    {ok, NameCxc, CxpPath, BinFile, Info, Pgroup}
	    end;
	_ ->
	    {error, Name, loadmodule_not_found}
    end.


%%% ----------------------------------------------------------
%%% keep for bw compatibility for a while
get_lm_start_data(Name)  when is_list(Name) ->
    get_lm_start_data_from_name(Name);

%%% ----------------------------------------------------------
get_lm_start_data(NameCxc) ->
    case mnesia:dirty_read(appmLmData, NameCxc) of
	[#appmLmData{cxpPath = CxpPath,
		     files = Files,
		     mpInfo = Info,
		     pgroup = Pgroup} ] ->
	    case get_bin_file(Files) of
		error ->
		    {error, NameCxc, {binfile_not_found,{files,Files}}};

		BinFile ->
		    {ok, CxpPath, BinFile, Info, Pgroup}
	    end;
	_ ->
	    {error, NameCxc, loadmodule_not_found}
    end.


%%% ----------------------------------------------------------
get_lm_start_data(NameCxc, Tag) ->
    Pattern = #appmLmData{nameCxc = NameCxc,
			  tag     = Tag,
			  _       = '_'},
    case mnesia:dirty_match_object(Pattern) of
	[#appmLmData{cxpPath = CxpPath,
		     files   = Files,
		     mpInfo  = Info,
		     pgroup = Pgroup} ] ->
	    case get_bin_file(Files) of
		error ->
		    {error, NameCxc, binfile_not_found};

		BinFile ->
		    {ok,CxpPath,BinFile,Info,Pgroup}
	    end;
	_ ->
	    {error, NameCxc, loadmodule_not_found}
    end.
get_lm_rev(NameCxc) ->
    case mnesia:dirty_read(appmLmData, NameCxc) of
	[#appmLmData{rev = undefined} ] ->
	    {ok,""};
	[#appmLmData{rev = Rev} ] ->
	    {ok,Rev};
	_ ->
	    {error, NameCxc, loadmodule_not_found}
    end.



get_cxp_no_rev(NameCxc) ->
    case mnesia:dirty_read(appmLmData, NameCxc) of
	[#appmLmData{cxpNo=undefined, cxpRev = undefined} ] ->
	    {ok,"",""};
	[#appmLmData{cxpNo=No, cxpRev = undefined} ] ->
	    {ok,No,""};
	[#appmLmData{cxpNo=No,cxpRev = Rev} ] ->
	    {ok,No,Rev};
	_ ->
	    {error, NameCxc, loadmodule_not_found}
    end.





get_max_esi_cb_time(NameCxc) ->
    case mnesia:dirty_read(appmLmData, NameCxc) of
	[#appmLmData{mpInfo = MpInfo}] ->
	    case MpInfo of
		#appmPgmData{maxEsiCbTime=Time} ->
		    Time;
		_ ->
		    undefined
	    end;
	_ ->
	    {error, NameCxc, loadmodule_not_found}
    end.

get_bin_file(Files) ->
    {Arch,_} = sysEnv:architecture(),

    case get_arch_file(Arch,Files) of
	error ->
	    NewArch = map_arch_internal_to_appdata(Arch),
	    get_arch_file(NewArch,Files);
	File ->
	    File
    end.

get_arch_file(false,_Files) ->
    error;
get_arch_file([Arch1,Arch2],Files)  ->
    case lists:keyfind(Arch1,#file.type,Files) of
	false ->
	    case lists:keyfind(Arch2,#file.type,Files) of
		false ->
		    error;
		R2 ->
		    R2#file.relpath
	    end;
	R1 ->
	    R1#file.relpath
    end;
get_arch_file(Arch,Files) ->
    case lists:keyfind(Arch,#file.type,Files) of
	false ->
	    error;
	R ->
	    R#file.relpath
    end.


%% @doc Map architecture (internal identifier produced by sysEnv:architecture())
%% to type defined in LMH IWD.

map_arch_internal_to_appdata("i686_32") ->
    "i386_32";
map_arch_internal_to_appdata("i686") ->
    "i386";
map_arch_internal_to_appdata("x86_64") ->
    "x86_64";
map_arch_internal_to_appdata("arm") ->
    ["arm-linux-gnueabi","armhf"];
map_arch_internal_to_appdata(_) ->
    false.


%% @doc Map architecture type as defined in LMH IWD to internal identifier
%% produced by sysEnv:architecture().

map_arch_appdata_to_internal("i386_32") ->
    "i686_32";
map_arch_appdata_to_internal("i386") ->
    "i686";
map_arch_appdata_to_internal("x86_64") ->
    "x86_64";
map_arch_appdata_to_internal("arm-linux-gnueabi") ->
    "arm";
map_arch_appdata_to_internal("armhf") ->
    "arm";
map_arch_appdata_to_internal(Other) ->
    Other.


%%% ----------------------------------------------------------
%%% Files with type="config" define extra variables
%%%
get_info(CxpPath,Files) ->
    case lists:keyfind("config",#file.type,Files) of
	false ->
	    undefined;
	R when is_record(R,file)->
	    Tpath = filename:join(CxpPath,R#file.relpath),
	    case filelib:wildcard(Tpath) of
		[CfgFile] ->
		    parse_config_file(CfgFile);
		_Other ->
		    appmLib:log(warning,
		      "~p:no file found at: ~n~p~n",
		      [?MODULE,Tpath]),
		    undefined
	    end;
	Err ->
	     appmLib:log(error,
	      "~p:Only one 'config' type allowed in loadmodule definition for ~p~n Found these ~p~n"
	      "Please update the loadmodule appdata definition xml file.~n"
	      "See LMHI IWD:~n"
	      "https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/APPM/APPM_CNX9012632/doc/15519/LmhIwd.doc ~n",
 	      [?MODULE, CxpPath,Err]),
	    undefined

    end.

parse_config_file(File) ->
    ActualFile = swmI:find_file(File), %use patched version if exist
    case xmerl_scan:file(ActualFile) of
	{#xmlElement{attributes=Attr, content = Content},_}  ->
	    Attrs = parse_attributes(Attr,[]),
	    Elements = parse_elements(Content,[]),
	    Escalation = get_pgm_escalation(attr(escalation,Attrs),File),
	    {HeartbeatInterval, CardiacArrest_noOfHb} =
		get_heartbeatParams(attr(heartbeatInterval, Attrs),
				    attr(cardiacArrest_noOfHb, Attrs),
				    File),
	    #appmPgmData{escalation=Escalation,
			 maxMem=attr(maxMem,Attrs),
			 maxRestarts=list_to_integer(
				       attr(maxRestarts, Attrs,?MAX_RST_DEF)),
			 maxTime=list_to_integer(
				       attr(maxTime, Attrs, ?MAX_TMO_DEF)),
			 tmpSize=attr_int(tmpSize, Attrs),
			 rtCap=attr(rtCap,Attrs),
			 softRt=attr(softRt,Attrs),
			 netAdmin=attr(netAdmin,Attrs),
			 sysAdmin=attr(sysAdmin,Attrs),
			 dataProcessing=attr(dataProcessing,Attrs,"false"),
			 delayedKill=list_to_integer(
				       attr(delayedKill,Attrs,"0")),
			 env=parse_env(Elements, []),
			 exportedEnv=parse_exported_env(Elements),
			 heartbeatInterval = HeartbeatInterval,
			 cardiacArrest_noOfHb = CardiacArrest_noOfHb,
			 maxEsiCbTime=list_to_integer(
					attr(maxEsiCbTime,
					     Attrs, ?MAX_ESI_CB_TMO_DEF))
			};
	Other ->
	    appmLib:log(
	      error,
	      "~p:error ( ~p )in configfile found at: ~n~p~n",
	      [?MODULE,Other, ActualFile])
    end.

get_pgm_escalation(undefined,_) ->
    undefined;
get_pgm_escalation("Appl",_) ->
    "None"; % for backward compatibility
get_pgm_escalation("Board",_) ->
    "BoardWarm"; % for backward compatibility
get_pgm_escalation(Esc,_) when Esc =:= "None";
			       Esc =:= "PgmGrp";
			       Esc =:= "BoardWarm";
			       Esc =:= "BoardCold"
			     ->
    Esc;
get_pgm_escalation(Esc,File) ->
    appmLib:log(error,
		"Unknown escalation ~p in file ~p~n"
		"Using BoardWarm ~n",
		[Esc,File]),
    "BoardWarm".

get_heartbeatParams(Val_HbInterval, Val_NoOfHb, File) ->
    HbInterval = get_heartbeatParam(Val_HbInterval, "heartbeatInterval", File),
    NoOfHb = get_heartbeatParam(Val_NoOfHb, "cardiacArrest_noOfHb", File),
    case HbInterval of
	_ when is_integer(HbInterval) ->
	    case NoOfHb of
		_ when is_integer(NoOfHb) ->
		    {HbInterval, NoOfHb};
		undefined ->
		    {HbInterval, ?DEFAULT_cardiacArrest_noOfHb}
	    end;
	undefined ->
	    case NoOfHb of
		undefined ->
		    {HbInterval, NoOfHb};
		_ ->
		    appmLib:log(error,
				"cardiacArrest_noOfHb = ~p in file ~p~n"
				"heartbeatInterval is missing ~n",
				[NoOfHb, File]),
		    {HbInterval, undefined}
	    end
    end.

get_heartbeatParam(undefined, _, _) ->
    undefined;
get_heartbeatParam(Val, Param, File) ->
    try list_to_integer(Val) of
	Integer when Integer >= 1 ->
	    Integer
    catch
	_ : _ ->
	    appmLib:log(error,
			"Unknown " ++ Param ++ " ~p in file ~p~n"
			"Using undefined ~n",
			[Val, File]),
	    undefined
    end.

parse_env([],Acc) ->
    lists:reverse(Acc);
parse_env([{env,Attrs,[]}|T],Acc) ->
    case attr(var, Attrs) of
	"LD_LIBRARY_PATH" -> % LD_LIBRARY_PATH ignored
	    parse_env(T,Acc);
	Var ->
	    parse_env(T,
		      [{Var,
			attr(value,Attrs)}|Acc])
    end;
parse_env([_|T],Acc) ->
    parse_env(T,Acc).

parse_exported_env(Elements) ->
    lists:foldl(
      fun({exportedenv,Attrs,[]}, Acc) ->
	      case attr(var, Attrs) of
		  "LD_LIBRARY_PATH" -> % LD_LIBRARY_PATH ignored
		      Acc;
		  Var ->
		      [Var|Acc]
	      end;
	 (_, Acc) ->
	      Acc
      end, [], Elements).

%%% ----------------------------------------------------------
strip_whitespace({S1, S2})  ->
   {strip_whitespace(S1), strip_whitespace(S2)};
strip_whitespace(Str) when is_list(Str) ->
    lists:filter(fun($ ) -> false;
		    (_)  -> true
		 end,
		 Str);
strip_whitespace(Other) ->
    Other.

%%% ----------------------------------------------------------
is_dynamic(NameCxc) ->
    case mnesia:dirty_read(appmLmData, NameCxc) of
	[#appmLmData{tag="dynamic"} ] -> true;
	_ -> false
    end.



%%% 4     CODE FOR TEMPORARY CORRECTIONS
%%% ---------------------------------------------------------

    
