%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmAppData.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/R5A/2
%%%
%%% @doc ==App data management==
%%% This module contains the support for reading app data files registered
%%% to each loadmodule, and distributing them to the registered appdata 
%%% receiver.

-module(swmAppData).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/2').
-date('2016-03-22').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-02-14   etxjotj     Created
%%% R2A/1      2012-12-19   etxarnu     Dont't crash if appdata file missing
%%% R2A/3      2013-04-05   etxarnu     Also log what appdata file that has been parsed
%%% R2A/5      2013-09-09   erarafo     Edoc
%%% R2A/8      2014-05-05   erarafo     Eliminated possibility of invoking
%%%                                     non-existent function throw/2
%%% R3A/1      2014-12-19   etxjotj     Appdata at upgrade
%%% R3A/2      2015-01-07   etxjotj     Removed appdata from collect file 
%%% R3A/3      2015-01-13   etxjotj     No ets table at startup
%%% R3A/4      2015-02-20   etxjotj     Handle erroneous appdata
%%% R3A/5      2015-03-06   etxjotj     HT54425 Clear appdata storage after read
%% ----    ---------- -------  ------------------------------------------------
%%% R4A/2   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/3   2015-07-15 etxjotj  Appdata efficiency
%%% R4A/4   2015-07-16 etxjotj  Index handling improved
%%% R4A/5   2015-07-16 etxjotj  Bugfix
%%% R4A/6   2015-07-20 etxjotj  Make_zip addition
%%% R4A/7   2015-07-21 etxjotj  Various dialyer fixes
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-13 etxberb  Parallelized appdata routines and removed some
%%%                             mnesia transactions.
%%% R5A/2   2016-03-22 etxjotj  Retrieve all appdata files from a UP
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([register_appdata_receiver/2]).
-export([init/1]).
-export([push_appdata/0,
	 push_appdata_upgrade/1]).
-export([prepare_appdata/0,
	 appdata_complete/2]).
-export([make_zip/0]).

-export([get_appdata_files/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-export([appdata/3,
	 parse_appdata/3,
	 init_prepare_appdata/4,
	 push_appdata_file/1]).

-include_lib("xmerl/include/xmerl.hrl").

-define(tmp_swm_dir, filename:join(sysEnv:tmp_dir(), "swm")).
-define(tmp_appdata_dir, filename:join(?tmp_swm_dir, "appdata")).
-define(tmp_appdata_cxp_dir, filename:join(?tmp_swm_dir, "appdata_cxp")).
-define(MonoTime, erlang:monotonic_time()).
-define(prepAppdata_proc, swmAppData_prepare_appdata).

-define(ELSE, true).
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).
-record(swmAppData, {target, module}).
-record(swmAppParsed, {key,
		       adPhase,
		       prodId,
		       version,
		       appDataE,
		       actualFile}).

-compile(export_all).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Register an appdata receiver
%%% See swmI for more info
%%% @end
%%% ----------------------------------------------------------

register_appdata_receiver(Tag, Module) ->
    Fun = fun() -> mnesia:write(#swmAppData{target=Tag, module=Module}) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [Tag, Module])
    end.


%%% ----------------------------------------------------------
%%% @doc Create tables etc
%%% Called by: swmDataInit:init/1
%%% @end
%%% ----------------------------------------------------------

init(DbNodes) ->
    {atomic, ok} = 
	clhI:mnesia_create_table(swmAppData, 
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   swmAppData)} |
				  swmDataInit:add_clh_option(swmAppData)]),
    ok.

%%% ----------------------------------------------------------
%%% @doc Collect all appdata files for read
%%% @end
%%% ----------------------------------------------------------

prepare_appdata() ->
    AdPhase = appdata,
    SwFolder = swmLib:software_dir(),
    ToFolder = ?tmp_appdata_dir,
    prepare_appdata(AdPhase, SwFolder, ToFolder).

%%% ----------------------------------------------------------
%%% @doc During upgrade, collect all appdata files in the new up for read
%%% @end
%%% ----------------------------------------------------------

prepare_appdata_upgrade(HomeOther) ->
    AdPhase = appdata_upgrade,
    SwFolder = swmLib:software_dir(HomeOther),
    ToFolder = swmLib:appdata_dir(),
    prepare_appdata(AdPhase, SwFolder, ToFolder).

prepare_appdata(AdPhase, SwFolder, ToFolder) ->
    erlang:spawn(?MODULE,
		 init_prepare_appdata,
		 [AdPhase, SwFolder, ToFolder, self()]),
    receive
	{?MODULE, init_prepare_appdata_done} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% #           push_appdata_upgrade()
%%% Input: -
%%% Output: ok
%%% Exceptions: 
%%% Description: Find all cxp metadata files named cxp*.xml and 
%%%              parse them for appdata information
%%% ----------------------------------------------------------

push_appdata_upgrade(HomeOther) ->
    prepare_appdata_upgrade(HomeOther),
    case get(make_zip) of
	true ->
	    catch (whereis(?prepAppdata_proc) ! {?MODULE, ignore_push_appdata});
	_ ->
	    push_appdata()
    end.

%%% This seems not to be in use?

make_zip() ->
    put(make_zip, true),
    push_appdata_upgrade(sysEnv:home_dir()),
    erase(make_zip).

%%% ----------------------------------------------------------
%%% @doc Return paths to all appdata files or a subset
%%% @end
%%% ----------------------------------------------------------

get_appdata_files(Up, all) ->
    get_appdata_files(Up);
get_appdata_files(Up, TargetType) ->
    Paths = get_appdata_files(Up, all),
    filter_appdata_files(TargetType, Paths).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init_prepare_appdata(AdPhase, SwFolder, ToFolder, Parent) ->
    register(?prepAppdata_proc, self()),
    supervise_prepare_appdata(AdPhase,
			      SwFolder,
			      ToFolder,
			      Parent,
			      ?MonoTime).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           get_appdata_files(Up)
%%% Input: current|other|PathToUpDir
%%% Output: 
%%% Exceptions: 
%%% Description:Get appdata files for a UP 
%%% ----------------------------------------------------------


get_appdata_files(current) ->
    get_appdata_files(swmLib:software_dir());
get_appdata_files(other) ->
    get_appdata_files(swmLib:software_dir_other());
get_appdata_files(Path) when is_list(Path) ->
    Pattern = filename:join([Path, "*", "cxp*.xml"]),
    CxpMetadataFiles = filelib:wildcard(Pattern),

    lists:foldl(fun get_product/2, [], CxpMetadataFiles).

get_product(Path, Accu) ->
    {ConfigurationE, []} = xmerl_scan:file(Path),
    ContentinfoE = find_element(contentinfo, ConfigurationE),
    NewAppDataFiles = 
	lists:foldl(fun get_appdata/2, [], ContentinfoE#xmlElement.content),
    CxpDir = filename:dirname(Path),
    Accu++[filename:join(CxpDir, RelPath)|| RelPath<-NewAppDataFiles].

get_appdata(ProductE, Accu) when ProductE#xmlElement.name == product->
    lists:foldl(fun get_appdata_path/2, Accu, ProductE#xmlElement.content);
get_appdata(_, Accu) ->
    Accu.

get_appdata_path(AppDataE, Accu) when AppDataE#xmlElement.name == appdata ->
    RelPath = find_attribute(relpath, AppDataE),
    File = find_attribute(file, AppDataE),
    Accu ++ [filename:join(RelPath, File)];
get_appdata_path(_, Accu) ->
    Accu.

filter_appdata_files(TargetType, Paths) ->
    [Path||Path<-Paths, is_correct_target_type(TargetType, Path)].

is_correct_target_type(TargetType, ActualFile) ->
    %% This can perhaps be more efficient than parsing the whole file
    %% for the first xml line
    {AppDataE, []} = xmerl_scan:file(ActualFile),
    case get_target(ActualFile, AppDataE) of
	TargetType ->
	    true;
	_ ->
	    false
    end.


supervise_prepare_appdata(AdPhase, SwFolder, ToFolder, Parent, T0) ->
    Parent ! {?MODULE, init_prepare_appdata_done},
    try
	begin
	    ets:new(appdataindex,
		    [public, ordered_set, named_table, {keypos, 2}]),
	    file:make_dir(?tmp_swm_dir),
	    file:make_dir(?tmp_appdata_dir),
	    file:make_dir(?tmp_appdata_cxp_dir),
	    do_prepare_appdata(AdPhase, SwFolder, ToFolder, T0),
	    wait4push(AdPhase, ToFolder)
	end
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {adPhase, AdPhase},
				   {swFolder, SwFolder},
				   {toFolder, ToFolder},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    Parent ! {?MODULE, error, prepare_appdata, ErrReason}
    end,
    exit(normal).

do_prepare_appdata(appdata_upgrade, SwFolder, ToFolder, T0) ->
    IndexPath = filename:join(ToFolder, "index.dat"),
    os:cmd(["rm -rf ", swmLib:appdata_dir()]),
    filelib:ensure_dir(IndexPath),
    {ok, Fd} = file:open(IndexPath,[write]),
    put(fd, Fd),
    parseCxp_copyCxc_appdata(SwFolder, ToFolder, T0),
    file:close(Fd),
    erase(fd);
do_prepare_appdata(appdata, SwFolder, ToFolder, T0) ->
    IndexFile = filename:join(swmLib:appdata_dir(), "index.dat"),
    case file:consult(IndexFile) of
	{ok, [{_, _, _, _} | _] = Data} ->
	    info_msg("Index file found. Copying data.~n"),
	    [ets:insert(appdataindex, X) || X <- Data],
	    move_appdata(?MonoTime, T0);
	{ok, [{_, _} | _]} ->
	    info_msg("Old index file found. Reverting to standard.~n"),
	    parseCxp_copyCxc_appdata(SwFolder, ToFolder, T0);
	{error, enoent} ->
	    info_msg("No index file found. Reverting to standard.~n"),
	    parseCxp_copyCxc_appdata(SwFolder, ToFolder, T0);
	Other ->
	    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				     {file, consult, [IndexFile]},
				     Other]),
	    parseCxp_copyCxc_appdata(SwFolder, ToFolder, T0)
    end.

wait4push(AdPhase, Folder) ->
    T1 = ?MonoTime,
    receive
	{?MODULE, push_appdata, Tpush, SenderPid} ->
	    T2 = ?MonoTime,
	    {T3, T4} = push_appdata(AdPhase, Folder),
	    T5 = ?MonoTime,
	    sysInitI:info_report([{?MODULE, ?FUNCTION},
				  {waiting4push, sysUtil:time_to_string(Tpush -
									T1)},
				  {no_of_files, ets:info(appdataindex, size)},
				  {parsing, sysUtil:time_to_string(T3 - T2)},
				  {AdPhase, sysUtil:time_to_string(T4 - T3)},
				  {ad_compl_phase(AdPhase),
				   sysUtil:time_to_string(T5 - T4)}
				 ]),
	    SenderPid ! {?MODULE, push_appdata_done};
	{?MODULE, ignore_push_appdata} ->
	    ok
    end.

move_appdata(T1, T0) ->
    SourceDir = swmLib:appdata_dir(),
    {ok, Files} = file:list_dir(SourceDir),
    [begin
	 Source = filename:join(SourceDir, File),
	 Destination = filename:join(?tmp_appdata_dir, File),
	 {ok, _} = file:copy(Source, Destination)
     end
     || File <- Files],
    T2 = ?MonoTime,
    CxcCnt = length(Files),
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  {indexFile2ets, sysUtil:time_to_string(T1 - T0)},
			  {{move_rcs2tmp, CxcCnt}, sysUtil:time_to_string(T2 -
									  T1)},
			  {'TOTAL', sysUtil:time_to_string(T2 - T0)}]),
    ok.

parseCxp_copyCxc_appdata(SwFolder, ToFolder, T0) ->
    put(counter, 1),
    file:make_dir(ToFolder),
    CxpPattern = filename:join([SwFolder, "*", "cxp*.xml"]),
    CxpFiles = filelib:wildcard(CxpPattern),
    T1 = ?MonoTime,
    copy_cxpAppdata_tmp(CxpFiles),
    T2 = ?MonoTime,
    copy_cxcAppdata(CxpFiles, ToFolder),
    T3 = ?MonoTime,
    CxpCnt = length(CxpFiles),
    CxcCnt = erase(counter) - 1,
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  {prepare, sysUtil:time_to_string(T1 - T0)},
			  {{copy2tmp_cxp, CxpCnt}, sysUtil:time_to_string(T2 -
									  T1)},
			  {{copy2tmp_cxc, CxcCnt}, sysUtil:time_to_string(T3 -
									  T2)},
			  {'TOTAL', sysUtil:time_to_string(T3 - T0)}]),
    ok.

copy_cxpAppdata_tmp([CxpFile | Tail]) ->
    ActualFile = swmLib:find_file(CxpFile),
    try
	{ok, _} = file:copy(ActualFile,
			    filename:join(?tmp_appdata_cxp_dir,
					  filename:basename(ActualFile)))
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {cxpFile, CxpFile},
				   {actualFile, ActualFile},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}])
    end,
    copy_cxpAppdata_tmp(Tail);
copy_cxpAppdata_tmp([]) ->
    ok.

copy_cxcAppdata([CxpFile | Tail], ToFolder) ->
    CxpFileName = filename:basename(CxpFile),
    {Configuration, ""} =
	xmerl_scan:file(filename:join(?tmp_appdata_cxp_dir, CxpFileName)),
    ConfigurationE = xmlElement_cwd(Configuration),
    ProductE = find_element(product, ConfigurationE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    ContentE = find_element(contentinfo, ConfigurationE),
    copy_cxcAppdata_file(ContentE#xmlElement.content,
			 ProdId,
			 Version,
			 filename:dirname(CxpFile),
			 ToFolder),
    copy_cxcAppdata(Tail, ToFolder);
copy_cxcAppdata([], _) ->
    ok.

copy_cxcAppdata_file([ProductE | Tail], ProdId, Version, CxpDir, ToFolder)
  when ProductE#xmlElement.name == product ->
    [begin
	 RelPath = find_attribute(relpath, AppDataE),
	 try find_attribute(file, AppDataE) of
	     File ->
		 ActualFile =
		     swmLib:find_file(filename:join([CxpDir, RelPath, File])),
		 store_zip(ActualFile, ToFolder, ProdId, Version)
	 catch
	     _ : _ ->
		 DataDir = filename:join([CxpDir, RelPath]),
		 {ok, Files} = file:list_dir(DataDir),
		 [begin
		      ActualFile =
			  swmLib:find_file(filename:join([CxpDir,
							  RelPath,
							  File])),
		      store_zip(ActualFile, ToFolder, ProdId, Version)
		  end
		  || File <- Files],
		 ok
	 end
     end
     || AppDataE <- ProductE#xmlElement.content,
	  AppDataE#xmlElement.name == appdata],
    copy_cxcAppdata_file(Tail, ProdId, Version, CxpDir, ToFolder);
copy_cxcAppdata_file([_ | Tail], ProdId, Version, CxpDir, ToFolder) ->
    copy_cxcAppdata_file(Tail, ProdId, Version, CxpDir, ToFolder);
copy_cxcAppdata_file([], _, _, _, _) ->
    ok.

push_appdata() ->
    try
	whereis(?prepAppdata_proc) ! {?MODULE, push_appdata, ?MonoTime, self()}
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    erlang:ErrClass(ErrReason)
    end,
    receive
	{?MODULE, push_appdata_done} ->
	    ok;
	{?MODULE, error, prepare_appdata, _ErrReason} ->
	    nok
    end.

push_appdata(AdPhase, Folder) ->
    ets:new(swmAppParsed, [public, ordered_set, named_table, {keypos, 2}]),
    T0 = ?MonoTime,
    {ok, ParseAppDataResults} =
	sysUtil:parallel_call([{?MODULE, parse_appdata, [AdPhase,
							 Folder,
							 Index]}
			       || Index
				      <- ets:select(appdataindex,
						    [{{'_', '$1', '_', '_'},
						      [],
						      ['$1']}])]),
    ParseAppDataInfoList =
	[case ParseAppDataResult of
	     {ok, {LogInfo, FileAndModuleInfo}} ->
		 logI:write_log("SwmInternal",
				"swmAppData",
				info,
				binary_to_list(LogInfo)),
		 FileAndModuleInfo
	 end
	 || ParseAppDataResult <- ParseAppDataResults],
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      "------- Parse Info -------:" |
			      format_moduleInfo(ParseAppDataInfoList, T0)]),
    AppDataModules = swmAppData_modules(),
    T1 = ?MonoTime,
    {ok, AppDataResults} =
	sysUtil:parallel_call([{?MODULE, push_appdata_file, [Module]}
			       || Module <- AppDataModules]),
    AppDataInfoList =
	lists:flatten([[case AppDataSubResult of
			    {ok, ModuleInfo} ->
				ModuleInfo
			end
			|| AppDataSubResult <- AppDataSubResults]
		       || AppDataSubResults <- AppDataResults]),
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      {"Callback Function", AdPhase},
			      "------- Callback Modules -------:" |
			      format_moduleInfo(AppDataInfoList, T1)]),
    AdComplPhase = ad_compl_phase(AdPhase),
    T2 = ?MonoTime,
    {ok, AppDataComplResults} =
	sysUtil:parallel_call([{?MODULE, appdata_complete, [Module,
							    AdComplPhase]}
			       || Module <- AppDataModules]),
    AppDataComplInfoList =
	[case AppDataComplResult of
	     {ok, AppDataComplInfo} ->
		 AppDataComplInfo
	 end
	 || AppDataComplResult <- AppDataComplResults],
    error_logger:info_report([{?MODULE, ?FUNCTION},
			      {"Callback Function", AdComplPhase},
			      "------- Callback Modules -------:" |
			      AppDataComplInfoList]),
    {T1, T2}.

format_moduleInfo(ModInf, T0) ->
    [{Module, {integer_to_list(Cnt) ++ " appdata file(s):",
	       sysUtil:time_to_string(T1 - T0)}}
     || {Module, Cnt, T1}
	    <- merge_moduleInfo(ModInf)].

merge_moduleInfo([{Module, _, T} | Tail]) ->
    {Cnt, LatestT, NewTail} = merge_moduleInfo(Tail, Module, 1, T, []),
    [{Module, Cnt, LatestT} | merge_moduleInfo(NewTail)];
merge_moduleInfo([]) ->
    [].

merge_moduleInfo([{Module, _, T} | Tail], Module, Cnt, PrevT, AccTail) ->
    LatestT = if
		  T > PrevT ->
		      T;
		  ?ELSE ->
		      PrevT
	      end,
    merge_moduleInfo(Tail, Module, Cnt + 1, LatestT, AccTail);
merge_moduleInfo([ModInf | Tail], Module, Cnt, T, AccTail) ->
    merge_moduleInfo(Tail, Module, Cnt, T, [ModInf | AccTail]);
merge_moduleInfo([], _, Cnt, LatestT, AccTail) ->
    {Cnt, LatestT, AccTail}.

ad_compl_phase(appdata) ->
    appdata_complete;
ad_compl_phase(appdata_upgrade) ->
    appdata_upgrade_complete.

swmAppData_modules() ->
    swmAppData_modules([Module || #swmAppData{module = Module}
				      <- ets:tab2list(swmAppData)]).

swmAppData_modules([Module | Tail]) ->
    case lists:member(Module, Tail) of
	false ->
	    [Module | swmAppData_modules(Tail)];
	true ->
	    swmAppData_modules(Tail)
    end;
swmAppData_modules([]) ->
    [].


xmlElement_cwd(#xmlElement{content = C} = E) ->
    Content = xmlElement_cwd(C),
    %% Setting xmlbase to current working directory is to avoid the full path
    %% to the file in the xml data since that would upset the hash
    %% value count in PMS
    E#xmlElement{content = Content,
		 xmlbase = "."};
xmlElement_cwd([E | Tail]) ->
    [xmlElement_cwd(E) | xmlElement_cwd(Tail)];
xmlElement_cwd(Other) ->
    Other.

%%% ----------------------------------------------------------
%% @doc Processes one CXP appdata file. Arguments are:
%%
%% ProdId    CXP product id, e g "CXP9021691_2"
%% Version   CXP version, e g "R2A188"
%% CxpDir    absolute directory path: RCS_ROOT/home/USER/software/CxpName_CxpId_CxpVer,
%%           where CxpName is e g "DUMMY-SIM"
%% RelPath   path relative to CxpDir, e g "FAKE2_CXC1734197_2/fake-R2A51/priv/appdata"
%% File      file name, e g "TESTMOM_appdata_imm.xml"
%%
%% If the patches directory contains a file named File then it
%% will override the regular appdata file.
%% @end
%%% ----------------------------------------------------------

parse_appdata(AdPhase, Folder, Index) ->
    [{ActualFile, _, ProdId, Version}] = ets:lookup(appdataindex, Index),
    ArchivePath = filename:join(Folder,  integer_to_list(Index) ++ ".zip"),
    {ok, [{_, Bin}]} = zip:unzip(ArchivePath, [memory]),
    String = binary_to_list(Bin),
    LogInfo =
  	"Registering appdata from " ++
  	ProdId ++
  	" " ++
  	Version ++
  	" " ++
  	ActualFile,
    case xmerl_scan:string(String) of
	{AppDataE, ""} ->
	    Target = get_target(ActualFile, AppDataE),
	    Module = get_module(Target),
	    ets:insert(swmAppParsed,
		       #swmAppParsed{key = {Module, Index},
				     adPhase = AdPhase,
				     prodId = ProdId,
				     version = Version,
				     appDataE = AppDataE,
				     actualFile = ActualFile}),
	    {ok, {list_to_binary(LogInfo),
		  {Module, ActualFile, ?MonoTime}}};
	Other ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {method, AdPhase},
				   {folder, Folder},
				   {file_missing_or_faulty, ActualFile},
				   {xmerl_scan, Other}]),
	    Other
    end.

push_appdata_file(Module) ->
    push_appdata_file(ets:next(swmAppParsed, {Module, -1}), Module, []).

push_appdata_file({Module, _Index} = Key, Module, AccResult) ->
    [#swmAppParsed{adPhase = AdPhase,
 		   prodId = ProdId,
 		   version = Version,
 		   appDataE = AppDataE,
 		   actualFile = ActualFile}] = ets:lookup(swmAppParsed, Key),
    CbReturn = call_appdata_func(Module,
 				 AdPhase,
 				 [ProdId, Version, AppDataE],
 				 ActualFile),
    Result = {CbReturn, {Module, ActualFile, ?MonoTime}},
    push_appdata_file(ets:next(swmAppParsed, Key),
 		      Module,
 		      [Result | AccResult]);
push_appdata_file(_, _, AccResult) ->
    AccResult.


store_zip(ActualFile, ToFolder, ProdId, Version) ->
    {ok, B} = file:read_file(ActualFile),
    Counter = get(counter),
    put(counter, Counter + 1),
    ets:insert(appdataindex, {ActualFile, Counter, ProdId, Version}),
    case get(fd) of
	undefined ->
	    ok;
	Fd ->
	    io:format(Fd, "~p.~n",[{ActualFile, Counter, ProdId, Version}])
    end,
    ZipName = integer_to_list(Counter),
    Files = [{filename:basename(ActualFile), B}],
    {ok, {_, OutBin}} = zip:create(ZipName, Files, [memory]),
    OutPath = filename:join(ToFolder, ZipName ++ ".zip"),
    file:write_file(OutPath, OutBin).
	

get_target(ActualFile, AppDataE) ->
    try find_attribute(target, AppDataE) of
	Tgt -> Tgt
    catch error:{badmatch, false} ->
	    error_msg("Target not specified in ~p~n~s~n",
		      [ActualFile, os:cmd(["cat ", ActualFile])]),
	    erlang:error(no_target_in_appdata);
	  T:E ->
	    erlang:T(E, [ActualFile, AppDataE])
		
    end.
	    
call_appdata_func(Module, Func, Args, ActualFile) ->
    try
	apply(Module, Func, Args)
    catch 
	error:undef ->
	    case Func of
		appdata ->
		    error_msg("No ~w:appdata func~n",[Module]),
		    erlang:error(undef, [Module, Func, args, ActualFile]);
		appdata_upgrade ->
		    Msg = "No appdata_upgrade func for "++atom_to_list(Module),
		    logI:write_log("SwmInternal", "swmAppData", warning, Msg)
	    end;
	T:E ->
	    sysInitI:error_report(
	      [{?MODULE, call_appdata_func},
	       {mfa, {Module, Func, Args}},
	       {appdata_file, ActualFile},
	       {T,E},
	       erlang:get_stacktrace()]),
	    if
		T =:= throw ->
		    throw({E, [Module, Func, Args, ActualFile]});
		true ->
		    erlang:T(E, [Module, Func, Args, ActualFile])
	    end
    end.

%%% ----------------------------------------------------------
%%% #           get_module(Target)
%%% Input: Target:string()
%%% Output: Modulde:atom()
%%% Exceptions: 
%%% Description: Find the callback module associated with the target
%%%              Return this module as default
%%%
%%% TODO, is the default action really of any use? This module
%%% has no appdata/3 function so the default action will cause
%%% a crash.
%%% ----------------------------------------------------------
get_module(Target) ->
    Fun = fun() ->
		  case mnesia:dirty_read({swmAppData, Target}) of
		      [Obj] ->
			  Obj#swmAppData.module;
		      [] ->
			  ?MODULE
		  end
	  end,
    {atomic, Module} = mnesia:transaction(Fun),
    Module.

%%% ----------------------------------------------------------
%%% #           appdata(Element)
%%% Input: Element:#xmlElement{}
%%% Output: 
%%% Exceptions: 
%%% Description: Default handling of appdata, namely to report and throw away
%%% ----------------------------------------------------------
appdata(ProdId, Version, Element) ->
    Target = find_attribute(target, Element),
    case ets:info(swm_appdata_collect) of
	undefined ->
	    error_msg("Unknown target ~p in ~p ~p ~p~n",
		      [Target, Element#xmlElement.xmlbase, ProdId, Version]);
	_ ->
	    ok
    end.
		 


%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
        lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

%% find_text(Element) when is_record(Element, xmlElement) ->
%%     [Text] = Element#xmlElement.content,
%%     Text#xmlText.value.

info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format) ->
    warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).


appdata_complete(Module, Phase) ->
    case
	ad_compl_cb(erlang:function_exported(Module, appdata_phase, 2),
		    Module,
		    Phase)
	of
	{ok, undefined} ->
	    Transaction =
		fun() ->
			ad_compl_cb_old(erlang:function_exported(Module,
								 Phase,
								 0),
					Module,
					Phase)
		end,
	    case mnesia:transaction(Transaction) of
		{atomic, {CbResult, CbInfo}} ->
		    {CbResult, {Module, CbInfo}};
		{aborted, Reason} ->
		    erlang:error({aborted, Reason}, [])
	    end;
	{CbResult, CbInfo} ->
	    {CbResult, {Module, CbInfo}}
    end.

ad_compl_cb(false, _, _) ->
    {ok, undefined};
ad_compl_cb(true, Module, Phase) ->
    Arg1 = Phase,
    Arg2 = [],
    T0 = ?MonoTime,
    try
	Result = Module:appdata_phase(Arg1, Arg2),
	{Result, sysUtil:time_to_string(?MonoTime - T0)}
    catch
	throw : {Arg1, Arg2} ->
	    {ok, undefined};
	  ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {module, Module},
				   {phase, Phase},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    {ErrClass, ErrReason}
    end.

ad_compl_cb_old(false, _, _) ->
    {ok, undefined};
ad_compl_cb_old(true, Module, Func) ->
    T0 = ?MonoTime,
    try
	Result = Module:Func(),
	{Result, sysUtil:time_to_string(?MonoTime - T0)}
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {module, Module},
				   {phase, Func},
				   {ErrClass, ErrReason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    erlang:ErrClass(ErrReason)
    end.
		    
			   

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

