%49%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmBackupFile.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/1

%%% @doc ==Backup file managmement==
%%% This module contains backup file handling, including import, export, and 
%%% delete operations

-module(swmBackupFile).
-vsn('/main/R11A/R12A/1').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-09-15 etxjotj  Created
%% R11A/4  2017-10-02 etxjotj  Fixed error handling in open_remote_file
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([delete_backup/1,
	 import_backup/3,
	 export_backup/3]).

%%% For swmBackup
-export([get_export_prefix/1, 
	 do_handle_import_backup/3,
	 do_remove_backup_dir/1,
	 delete_backup_internal/2,
	 get_next_index/0,
	 get_metadata/1,
	 store_metadata/2,
	 convert_metadata/1]).

%%% For swmBackupScheduler
-export([compress_file/2]).

%%% For swmServer (certificate revocation)
-export([delete_all_backups/0]).

%%% For swmBackup vRAN
%%% also compress_file/2
-export([start_channel/5,
	 file_size/3,
	 open_remote_file/5,
	 download_file/3,
	 format_product_data/1,
	 handle_delete_backup/1,
	 handle_export_backup/4,
	 parse_uri/1]).
	 
%%% For swmBackupModel
-export([remove_backup_dir/1, cleanup/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #2.3   IMPORTED FUNCTIONS
%%% ----------------------------------------------------------
-import(swmBackupModel, [update_progress/2,
			 get_backup_by_name/1,
			 default_idle_pr/0,
			 make_dn/1]).
-import(swmBackup, [is_protected_backup/1]).
%%% ----------------------------------------------------------
%%% #2.4   DEFINES
%%% ----------------------------------------------------------


-record(transfer,
	{scheme,
	 user,
	 host,
	 port,
	 remoteFile,
	 bu_key,
	 pwd,
	 fd,
	 pid,
	 handle,
	 rp_key,
	 total_size,
	 path}).


%%% ----------------------------------------------------------
%%% Includes
-include("RcsBrM.hrl").
-include("SwmInternal.hrl").
-include_lib("kernel/include/file.hrl").
%% -include("comte_types.hrl").

%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Delete backup
%%% @end
%%% ----------------------------------------------------------

-spec delete_backup(Name::binary()) -> pid().
delete_backup(Name) ->
    Fun = fun() -> handle_delete_backup(Name) end,
    proc_lib:spawn(
      fun() -> swmBackupModel:action_handler(Fun, mgr), 
	       ok = swmLib:unlock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID)
      end).
			    
%%% ----------------------------------------------------------
%%% @doc Delete backup
%%% @end
%%% ----------------------------------------------------------

-type one()::string().
-type bu_key()::{one(), one(), one(), one(), string()}.
-spec export_backup(BuKey::bu_key(), Passwd::string(), Uri::string()) -> pid().
export_backup(BuKey, Passwd, Uri) ->
    Fun = fun() -> handle_export_backup(BuKey, BuKey, Passwd, Uri) end,
    proc_lib:spawn(
      fun() -> 
	      swmBackupModel:action_handler(Fun, BuKey),
	      ok = swmLib:unlock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID) 
      end).


			    
%%% ----------------------------------------------------------
%%% @doc Delete backup
%%% @end
%%% ----------------------------------------------------------
-spec import_backup(DnRev::term(), Uri::string(), Passwd::string()) -> pid().
import_backup(DnRev, Uri, Passwd) ->
    Fun = fun() -> handle_import_backup(DnRev, Uri, Passwd) end,
    proc_lib:spawn(
      fun() -> 
	      swmBackupModel:action_handler(Fun, mgr),
	      ok = swmLib:unlock_action_capable(?IMPORT_BACKUP_ACTION_CAPABLE_ID) 
      end).
			    
		   
		   
%%% ----------------------------------------------------------
%%% @doc Parse an http uri or return an exception

parse_uri(Uri) ->
    parse_uri(Uri, mgr).

parse_uri(Uri, ReportKey) ->
    parse_uri(Uri, ReportKey, []).

parse_uri(RawUri, ReportKey, ParseOpts) ->
    Uri = http_uri:decode(RawUri), %Handle HTTP encoded URIs; HV58153

    case http_uri:parse(Uri, ParseOpts) of
	{ok, {file = Scheme, [], [], _Port, Path, _Query} = Parsed} ->
	    case parse_uri_verify_filepath(Path) of
		ok ->
		    {ok, Parsed};
		{error, Reason} ->
		    update_progress(ReportKey,
				    [{resultInfo, Path ++ ": " ++ Reason}]),
		    throw({invalid_url_path, Scheme, Path})
	    end;
	{ok, {file = Scheme, _UserInfo, _Host, _Port, Path, _Query}} ->
	    update_progress(ReportKey,
			    [{resultInfo, "Malformed uri: " ++ Path}]),
	    throw({malformed_url, Scheme, Path});
	{ok, Parsed} ->
	    {ok, Parsed};
	{error, {no_default_port, file, Uri}} ->
	    %% Reported to support@erlang.ericsson.se: [seq13257].
	    %% TODO: Remove this case clause when OTP fixed!
	    parse_uri(Uri, ReportKey, [{scheme_defaults, [{file, 9999}]}]);
    {error, {no_default_port, ftpes, Uri}} ->
        parse_uri(Uri, ReportKey, [{scheme_defaults, [{ftpes, 21}]}]);
	{error, no_scheme} -> 
	    update_progress(
	      ReportKey,
	      [{resultInfo, "The uri has no scheme or it is unknown"}]),
	    throw({no_scheme, Uri});
	{error, {malformed_url, Scheme, AbsURI}} -> 
	    update_progress(
	      ReportKey,
	      [{resultInfo, "Malformed uri: "++AbsURI}]),
	    throw({malformed_url, Scheme, AbsURI});
	{error, {no_default_port, Scheme, AbsURI}} ->
	    update_progress(
	      ReportKey,
	      [{resultInfo, "The system has no default port for "++
		    atom_to_list(Scheme)}]),
	    throw({no_default_port, Scheme, AbsURI})
    end.

%%% ----------------------------------------------------------
parse_uri_verify_filepath(Path) ->
    case file:read_file_info(Path) of
	{ok, #file_info{type = Type}} when Type /= directory ->
	    {error, "not a directory"};
	{ok, #file_info{access = Access}} when Access /= read_write andalso
					       Access /= write ->
	    {error, "no write access"};
	{ok, #file_info{}} ->
	    ok;
	{error, Reason} ->
	    {error, file:format_error(Reason)}
    end.

%%% ----------------------------------------------------------
%%% @doc Find the next free index for creating a backup dir, and model object
%%%

get_next_index() ->
    BuDir = swmLib:backup_dir(),
    case file:list_dir(BuDir) of
	{ok, Dirs} ->
	    case [D||D<-Dirs, D/=".", D/=".."] of
		[] ->
		    "1";
		IndexDirs ->
		    Max = lists:max([list_to_integer(IxD)||IxD<-IndexDirs]),
		    get_next_index(Max+1)
	    end;
	{error, enoent} ->
	    ok = file:make_dir(BuDir),
	    "1";
	{error, Reason} ->
	    erlang:error(Reason, [])
    end.

get_next_index(IndexN) ->
    Index = integer_to_list(IndexN),
    try swmLib:lock_backup(Index) of
	ok ->
	    Index
    catch error:set_lock_failed ->
	    get_next_index(IndexN+1)
    end.

store_metadata(Index, MetadataM) ->
    BuDir = swmLib:backup_dir(Index),
    store_metadata_in_dir(BuDir, MetadataM).

store_metadata_in_dir(BuDir, MetadataM) ->
    BuMeta = filename:join(BuDir, "metadata.v3"),
    {ok, Fd} = file:open(BuMeta, [write]),
    [ok = io:format(Fd, "~p.~n",[Meta])||Meta<-maps:to_list(MetadataM)],
    ok = file:close(Fd),
    
    %% Legacy metadata
    BuMeta2 = filename:join(BuDir, "metadata"),
    #{backupName := BackupName,
      creationTime := CreationTime,
      swVersion := SwVersion,
      creationType := CreationType} = MetadataM,
    M2 = #metadataV2{backupName = BackupName,
		     creationTime = CreationTime,
		     swVersion = SwVersion,
		     creationType = CreationType},
    {ok, Fd2} = file:open(BuMeta2,[write]),
    ok = io:format(Fd2, "~p.~n" ,[M2]),
    ok = file:close(Fd2).
    
%%% ----------------------------------------------------------
%%% #           get_metadata(Index)
%%% Input: Index:string()
%%% Output: #metadataV2{}
%%% Exceptions:
%%% Description: This function is supposed to return a compatible
%%%              metadata version, even if the metadata record changes
%%%              I.e. if a metadataV1 is found in the file and the latest
%%%              version is metadataV2 this function should convert to V2
%%% ----------------------------------------------------------

get_metadata(Index_or_Dir) ->
    MetaDir = 
	try list_to_integer(Index_or_Dir) of
	    _ -> swmLib:backup_dir(Index_or_Dir)
	catch  error:badarg -> Index_or_Dir
	end,
    case file:consult(filename:join(MetaDir, "metadata.v3")) of
	{ok, Metadata} ->
	    convert_metadata(Metadata);
	{error, enoent} ->
	    case file:consult(filename:join(MetaDir, "metadata")) of
		{ok, [Metadata]} -> 
		    convert_metadata(Metadata);
		{error, Reason} ->
		    erlang:error({metadata_read_error, Reason}, [Index_or_Dir])
	    end;
	{error, Reason} ->
	    erlang:error({metadata_read_error, Reason}, [Index_or_Dir])
    end.

convert_metadata(Md) when is_record(Md, metadataV2) ->
    KeyValues = lists:zip(record_info(fields, metadataV2),
			  tl(tuple_to_list(Md))),
    maps:from_list([{metadata,v3}|KeyValues]);
convert_metadata(Md) when is_record(Md, metadataV1) ->
    #metadataV1{backupName = Name,
		creationTime = Time,
		software = Software, 
		creationType = CType} = Md,
    Md2 = 
	#metadataV2{backupName = Name,
		    creationTime = Time,
		    swVersion = 
			[#'ProductData'{productName = element(1,S),
					productNumber = element(2,S),
					productRevision = element(3,S),
					description = "",
					type = ""
				       }
			 ||S<-Software],
		    creationType=CType},
    convert_metadata(Md2);
convert_metadata(Metadata) ->
    maps:from_list(Metadata).


delete_all_backups() ->
    [delete_backup_internal(BrmBackup#brmBackup.backupName, forced)||
	BrmBackup<-ets:tab2list(brmBackup)],
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Delete a backup
%%%

handle_delete_backup(Name) ->
    receive
	cancel_mgr -> throw(cancelled)
    after 5000 -> 
	    swmLib:set_ram_variable(mgr_point_of_no_return, true)	    
    end,
    case delete_backup_internal(Name, normal) of
	{ok, Indices} ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    case Indices of
		[] ->
		    update_progress(mgr, [{result, ?ActionResultType_FAILURE},
					  {resultInfo, "Backup not found"},
					  {progressPercentage, 0},
					  {state, ?ActionStateType_FINISHED},
					  {timeActionCompleted, CompleteTime}]);
		_ ->
		    update_progress(mgr, [{result, ?ActionResultType_SUCCESS},
					  {resultInfo, "Backup removed"},
					  {progressPercentage, 100},
					  {state, ?ActionStateType_FINISHED},
					  {timeActionCompleted, CompleteTime}])
	    end;
	{error, protected_backup} ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    Msg = "Backup "++binary_to_list(Name)++" is protected",
	    update_progress(mgr, [{result, ?ActionResultType_FAILURE},
				  {resultInfo, Msg},
				  {progressPercentage, 0},
				  {state, ?ActionStateType_FINISHED},
				  {timeActionCompleted, CompleteTime}])
    end,
    swmLib:erase_ram_variable(mgr_point_of_no_return).


-spec delete_backup_internal(Name::string()|binary(), 
			     Operation::normal|forced) ->
				    {ok, [string()]} | {error, protected_backup}.


delete_backup_internal(NameB, Operation) when is_binary(NameB) ->
    Name = binary_to_list(NameB),
    delete_backup_internal(Name, Operation);
delete_backup_internal(Name, Operation) ->
    Msg = "Deleting backup "++Name,
    swmLib:write_swm_log("BrmBackupManager", info, Msg),
    Fun = delete_backup_internal_trans(Name, Operation),
    case mnesia:transaction(Fun) of
	{atomic, Indices} -> 
	    [proc_lib:spawn(fun() -> remove_backup_dir(Index) end)||
		Index<-Indices],
	    {ok, Indices};
	{aborted, protected_backup} ->
	    {error, protected_backup}
    end.

delete_backup_internal_trans(Name, Operation) ->
    fun() ->
	    %% While this code can handle multiple instance with the same 
	    %% backup name, according to ECIM that should not happen
	    Matched = get_backup_by_name(Name),
	    do_delete_backup_internal_trans(Operation, Matched)
    end.

do_delete_backup_internal_trans(forced, [Bu|Matched]) ->
    FullIndex = tuple_to_list(Bu#brmBackup.brmBackupId),
    mnesia:delete_object(Bu),
    [lists:last(FullIndex)|do_delete_backup_internal_trans(forced, Matched)];
do_delete_backup_internal_trans(normal, [Bu|Matched]) ->
    case is_protected_backup(Bu#brmBackup.backupName) of
	true -> mnesia:abort(protected_backup);
	false ->
	    FullIndex = tuple_to_list(Bu#brmBackup.brmBackupId),
	    mnesia:delete_object(Bu),
	    [lists:last(FullIndex)|
	     do_delete_backup_internal_trans(normal, Matched)]
    end;
do_delete_backup_internal_trans(_, []) -> [].
    

-spec remove_backup_dir(Index::string()) -> ok | {error, backup_busy}.

remove_backup_dir(Index) ->
    try swmLib:lock_backup(Index) of
	ok ->
	    try do_remove_backup_dir(Index) 
	    after 
		swmLib:unlock_backup(Index)
	    end
    catch error:set_lock_failed ->
	    {error, backup_busy}
    end.

do_remove_backup_dir(Index) ->
    BuDir = swmLib:backup_dir(Index),
    cmd("rm -rf "++BuDir),
    ok.

%%% ----------------------------------------------------------
%%% @doc Exports a backup
%%%

handle_export_backup(BuKey, Report, Passwd, Uri) ->
    Index = element(5, BuKey),
    BuDir = swmLib:backup_dir(Index),
    ETime = os:timestamp(),
    Info = "Preparing backup "++Index++" for transport",
    update_progress(Report, [{additionalInfoClear, Info}]),

    TmpPath = compress_file(BuDir, ETime),

    %% Upload file
    InfoStart =
	case Passwd of
	    "" ->
		"Starting transport without password";
	    _ ->
		"Starting transport"
	end,
    update_progress(Report, [{additionalInfo, InfoStart}]),
    {ok, {Scheme, User, Host, Port, RemoteDir, _Query}} = parse_uri(Uri, BuKey),
    %% Use basic format in order not to introduce any unwanted characters
    %% in the timestamp part of the filename
    BasicET = comsaI:iso_time(ETime, basic),
    RemoteFileName = make_remote_name(BuKey, BasicET),
    RemoteFile = filename:join(RemoteDir, RemoteFileName),
    upload_file(#transfer{scheme = Scheme,
			  user = User,
			  host = Host,
			  port = Port,
			  remoteFile = RemoteFile,
			  bu_key = BuKey,
			  pwd = Passwd,
			  rp_key = Report,
			  total_size = filelib:file_size(TmpPath), 
			  path = TmpPath}),
    Fun = 
	fun() ->
		[BrmBackup] = mnesia:read({brmBackup, BuKey}),
		Name = BrmBackup#brmBackup.backupName,
		LsKey = list_to_tuple(
			  lists:reverse(
			    ["1"|tl(lists:reverse(tuple_to_list(BuKey)))])),
		[LS] = mnesia:read({brmBackupLabelStore, LsKey}),
		mnesia:write(LS#brmBackupLabelStore{lastExportedBackup=Name}),
		Name
	end,
    {atomic, Name} = mnesia:transaction(Fun),
    cleanup(),
    swmLib:write_swm_log("BrmBackup="++Index, info, 
			 "Backup exported: "++Name),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(Report, [{additionalInfo, "Transfer complete"},
			     {result, ?ActionResultType_SUCCESS},
			     {resultInfo, RemoteFile},
			     {progressPercentage, 100},
			     {state, ?ActionStateType_FINISHED},
			     {timeActionCompleted, CompleteTime}]),
    ok.

%%% ----------------------------------------------------------
-spec compress_file(BuDir::string(), 
		    ETime::{integer(), integer(), integer()}) ->
			   OutPath::string().

compress_file(BuDir, ETime) ->
    %% Cleanup in case of previous sw crash
    Exp_tgz = "export.tgz",
    Exp_bup = "export.bup",
    Exp_zip = "export.zip",
    cleanup_file(filename:join(BuDir, Exp_tgz)), 
    cleanup_file(filename:join(BuDir, Exp_bup)), 
    %% Create bu package
    TmpPath = filename:join(swmLib:swm_dir(), Exp_zip),
    {ok, Files} = file:list_dir(BuDir),
    cmd(["cd ", BuDir, " ; tar cfz ", BuDir, "/"++Exp_tgz]++
	    [[" ", File]||File<-Files--["backupinfo.xml"]]),
    {ok, Bin} = file:read_file(filename:join(BuDir, Exp_tgz)),
    Crypto = crypto:block_encrypt(aes_cfb128,
				  <<"1234567890ABCDEF">>, 
				  <<"1234567890ABCDEF">>, Bin),
    ok = file:write_file(filename:join(BuDir, Exp_bup), Crypto),
    file:delete(filename:join(BuDir, Exp_tgz)),

    %% Adjust export time
    MetaDataPath = filename:join(BuDir, "backupinfo.xml"),
    {ok, BI} = file:read_file(MetaDataPath),
    ExportTime = comsaI:iso_time(ETime, extended),
    NewBI = re:replace(BI, "<exportTime>.*</exportTime>", 
		       "<exportTime>"++ExportTime++"</exportTime>", 
		       [{return, binary}]),
    ok = file:write_file(MetaDataPath, NewBI),

    cmd("cd "++BuDir++" ; zip "++TmpPath++" "++Exp_bup++" backupinfo.xml"),
    file:delete(filename:join(BuDir, Exp_bup)),
    TmpPath.


cleanup_file(File) ->
    case filelib:is_file(File) of
	true ->
	    Result = file:delete(File),
	    info_msg(
	      "Cleaned up rest from interupted backup export: ~p ~p~n", 
	      [File, Result]);
	false ->
	    ok
    end. 

%%% ----------------------------------------------------------
%%% @doc Generate the external file name from configuration
%%% In the action uri parameter the string "null" is interpreted as no uri
%%% (at least until COM support optional parameters)

make_remote_name(BuKey, ExportTime) ->
    [BrmBackup] = mnesia:dirty_read({brmBackup, BuKey}),

    MeData = comsaI:get_managed_element_data(),
    ExportUserLabel = case get_export_prefix(MeData) of
			  "" -> "";
			  EUL -> EUL++"_"
		      end,

    Name = BrmBackup#brmBackup.backupName,
    %% Time = lists:append(string:tokens(ExportTime, "-:.")),

    %% Fix for HS77233
    %% File names according to ECIM BrM UCD
    MeId = case proplists:get_value(networkManagedElementId, MeData) of
	       undefined -> "1";
	       Id -> Id
	   end,
    MeType = case proplists:get_value(managedElementType, MeData) of
		 undefined -> "";
		 ET -> ET
	     end,

    noslash(ExportUserLabel++Name++"_"++MeId++"_"++MeType)++"_"++
	ExportTime++".zip".


get_export_prefix(MeData) ->
    {atomic, [Brm]} = mnesia:transaction(
			fun() -> mnesia:read({brM, {"1","1","1"}}) end),
    NMEI = proplists:get_value(networkManagedElementId, MeData),
    case {Brm#brM.exportPackageLabelPrefix, NMEI} of
	{undefined, undefined} -> "";
	{undefined, _} -> NMEI;
	{Label, _} -> Label
    end.


noslash(Str) ->
    [case X of
	 $/ -> $_;
	 _ -> X
     end||X<-Str].

%%% ----------------------------------------------------------
%%% @doc Open an sftp channel or starts ftpes client

start_channel(Proto, Host, Port, User, Passwd) ->
    start_channel(Proto, Host, Port, User, Passwd, undefined).

start_channel(Proto, Host, Port, User, Passwd, BuKey) ->
    case ftpI:start_channel_with_alt(Proto, Host, Port, User, Passwd) of
    {ok, SP, C} -> 
        case Proto of
            ftpes ->
               info_msg("Started ftpes client : ~p~n", [SP]);
            sftp ->
	          info_msg("~p~n",[ssh:connection_info(
			       C, [client_version, server_version, peer])]),
	          put(connectionRef, C)
        end,
        put(protocol, Proto),
        put(channelPid, SP),
	    {ok, SP, C};
	{error, E1} ->
	    %% HU71354 Handle unexpected errors
	    Progress = case BuKey of 
			   undefined -> mgr;
			   BuKey -> BuKey
		       end,
	    case E1 of
		etimedout -> 
		    Msg = "No response from the remote server",
		    update_progress(Progress, [{additionalInfo, Msg},
					       {resultInfo, Msg}]),
		    throw(etimedout);
		econnrefused ->
		    Msg = "The remote server refused the connection",
		    update_progress(Progress, [{additionalInfo, Msg},
					       {resultInfo, Msg}]),
		    throw(econnrefused);
		nxdomain ->
		    Msg = "Cannot find the remote server",
		    update_progress(Progress, [{additionalInfo, Msg},
					       {resultInfo, Msg}]),
		    throw(nxdomain);
		einval ->
		    Msg = "Invalid parameters",
		    update_progress(Progress, [{additionalInfo, Msg},
					       {resultInfo, Msg}]),
		    throw(einval);
		String when is_list(String) ->
		    update_progress(Progress, [{additionalInfo, String},
					       {resultInfo, String}]),
		    throw(String);
		{options, _} ->
		    Msg = "Faulty internet parameters",
		    update_progress(Progress, [{additionalInfo, Msg},
					       {resultInfo, Msg}]),
		    throw(options);
		_ ->
		    Msg = "Software error",
		    update_progress(Progress, [{resultInfo, Msg}]),
		    sysInitI:error_report(
		      [{mfa, [{ftpI, start_channel_with_alt,
			       [Proto, Host, Port, User, Passwd]}]},
		       {error, E1}]),
		    erlang:error(E1, [Proto, Host, Port, User, Passwd, BuKey])		    
	    end
    end.

%%% ----------------------------------------------------------
%%% @doc Upload the backup file and update progress at the same time

open_remote_file(Proto, BuKey, ChannelPid, RemotePath, Opts) ->
    %% case verify_path(ChannelPid, filename:dirname(RemotePath)) of
    %% 	ok -> ok;
    %% 	{error, Reason, Dir} ->
    %% 	    Info= lists:flatten(io_lib:format("~p for ~s",[Reason, Dir])),
    %% 	    update_progress(BuKey, [{resultInfo, Info}]),
    %% 	    erlang:error(Reason)
    %% end,
    RemoteDir = filename:dirname(RemotePath),
    case ftpI:list_dir(Proto, ChannelPid, RemoteDir) of
	{ok, _} -> ok;
	{error, Reason} ->
	    Info = manage_open_error(Proto, Reason, RemoteDir),
	    update_progress(BuKey, [{resultInfo, Info}]),
	    throw({fail, "Problem accessing directory"})
    end,

    case ftpI:open(Proto, ChannelPid, RemotePath, Opts) of
	{ok, Handle} -> 
	    put(handle, Handle),
	    {ok, Handle};
	{error, Reason2} ->
	    Info2 = manage_open_error(Proto, Reason2, RemotePath),
	    update_progress(BuKey, [{resultInfo, Info2}]),
	    throw({fail, "Problem acessing file"})
    end.

manage_open_error(ftpes, Reason, Path) ->
    case ftp:formaterror(Reason) of
	"Unknown"++_ ->
	    error_msg("~p ~p~n",[Reason, Path]),
	    "Software error";
	Msg -> 
	    Msg ++ " " ++ Path
    end;
manage_open_error(sftp, no_such_file, Path) ->
    "No such file or direcotry "++Path;
manage_open_error(sftp, Reason, Path) ->
    error_msg("~p ~p~n",[Reason, Path]),
    "Software error".


    

    


%% verify_path(ChannelPid, RemotePath="/") ->
%%     case ftpI:list_dir(sftp, ChannelPid, RemotePath) of
%% 	{ok, _} ->
%% 	    ok;
%% 	{error,Reason} ->
%% 	    {error, Reason, RemotePath}
%%     end;
%% verify_path(ChannelPid, RemotePath) ->
%%     case verify_path(ChannelPid, filename:dirname(RemotePath)) of
%% 	ok ->
%% 	    case ftpI:list_dir(sftp, ChannelPid, RemotePath) of
%% 		{ok, _} ->
%% 		    ok;
%% 		{error,Reason} ->
%% 		    {error, Reason, RemotePath}
%% 	    end;
%% 	{error, Reason, RemotePath} ->
%% 	    {error, Reason, RemotePath}
%%     end.

%% c
%% fault_search_path(_, "/") ->
%%     "/";
%% fault_search_path(ChannelPid, Path) ->
%%     Dir = filename:dirname(Path),
%%     case ftpI:list_dir(sftp, ChannelPid, Dir) of
%% 	{ok, _} ->
%% 	    Path;
%% 	{error, Reason} ->
%% 	    info_msg("ftpI:list_dir(sftp, ~p, ~p) = {error, ~p}~n",
%% 		     [ChannelPid, Dir, Reason]),
%% 	    fault_search_path(ChannelPid, Dir)
%%     end.

%%% ----------------------------------------------------------
%%% @doc Upload the backup file and update progress at the same time
upload_file(#transfer{scheme = Proto,
		      user = User,
		      host = Host,
		      port = Port,
		      remoteFile = RemoteFile,
		      bu_key = BuKey,
		      pwd = Passwd,
		      rp_key = Report,
		      %total_size = TotalSize, 
		      path = TmpPath} = Params) when Proto =:= sftp orelse Proto =:= ftpes ->
    {ok, Pid, _CRef} = start_channel(Proto, Host, Port, User, Passwd, Report),
    case file:open(TmpPath, [read, raw, binary]) of
	{ok, Fd} ->
	    {ok, Handle} = open_remote_file(Proto, BuKey, Pid, RemoteFile, [write]),
	    NewParams = 
		Params#transfer{fd = Fd,
				pid = Pid,
				handle = Handle},
	    upload_file(file:read(Fd, 65536), 0, NewParams),
	    file:close(Fd),
	    file:delete(TmpPath),
	    ok;
	{error, Reason} ->
	    file:delete(TmpPath),
	    Info = file:format_error(Reason),
	    update_progress(Report, [{additionalInfo, Info}]),
	    erlang:error(Reason)        
    end;

upload_file(#transfer{scheme = file, 
		      remoteFile = RemoteFile,
		      rp_key = Report,
		      total_size = TotalSize, 
		      path = TmpPath}) ->
    case file:copy(TmpPath, RemoteFile) of
	{ok, TotalSize} ->
	    file:delete(TmpPath),
	    update_progress(Report, [{progressPercentage, 100}]),
	    ok;
	{ok, BytesCopied} ->
	    file:delete(TmpPath),
	    Info = "File copy incomplete",
	    sysInitI:error_report([Info,
				   {totalSize, TotalSize},
				   {bytesCopied, BytesCopied},
				   {file, TmpPath},
				   {remoteFile, RemoteFile}]),
	    update_progress(Report, [{additionalInfo, Info}]),
	    erlang:error(Info);
	{error, Reason} ->
	    file:delete(TmpPath),
	    Info = file:format_error(Reason),
	    update_progress(Report, [{additionalInfo, Info}]),
	    erlang:error(Reason)
    end.


%%% ----------------------------------------------------------
upload_file({ok, Data}, AccuSize, #transfer{scheme = Proto, fd = Fd,
					    pid = Pid,
					    handle = Handle,
					    rp_key = Report,
					    total_size = TotalSize} = Params) ->
    garbage_collect(),
    NewAccuSize = AccuSize + size(Data),
    ok = ftpI:write(Proto, Pid, Handle, Data, 10000),
    update_progress(Report, [{progressPercentage, AccuSize*100 div TotalSize}]),
    receive
	{cancel,Key} when Params#transfer.bu_key == Key->
	    %% Local cleanup, ssh side will be handled in cleanup
	    file:close(Fd),
	    file:delete(Params#transfer.path),
	    throw(cancelled)
    after
	0 ->
	    ok
    end,
    upload_file(file:read(Fd, 65536), NewAccuSize, Params);
upload_file(eof, _, _) ->
    ok;
upload_file({error, Reason}, _, Params) ->
    Info = file:format_error(Reason),
    update_progress(Params#transfer.rp_key, [{additionalInfo, Info}]),
    erlang:error(Reason).

%%% ----------------------------------------------------------
%%% @doc Import a backup
%%%

handle_import_backup(DnRev, Uri, Passwd) ->

    case swmBackupModel:housekeeping() of
	ok ->
	    ok;
	max_reached ->
	    update_progress(mgr, 
			    [{resultInfo,"Maximum number of backups reached"}]),
	    throw(max_reached);
	{cleaned, _} ->
	    ok
    end,
    swmLib:write_swm_log("BrmBackupManager", info, "Importing backup"),

    %% Download file
    update_progress(mgr, [{additionalInfoClear, "Starting transport"}]),
    update_progress(mgr, [{additionalInfo, "Locating "++Uri}]),
    {ok, {Proto, User, Host, Port, RemoteDir, _Query}} = parse_uri(Uri),
    {ok, Pid, _CRef} = start_channel(Proto, Host, Port, User, Passwd),
    TotalSize = file_size(Proto, Pid, RemoteDir),
    TmpPath = filename:join(swmLib:swm_dir(), "import.zip"),
    ok = filelib:ensure_dir(TmpPath),
    case file:open(TmpPath, [write, raw, binary]) of
	{ok, Fd} ->
	    {ok, Handle} = open_remote_file(Proto, mgr, Pid, RemoteDir, [read,binary]),
	    Params = 
		#transfer{scheme = Proto, fd = Fd, pid = Pid, handle = Handle, 
			  rp_key = mgr, total_size = TotalSize, path=TmpPath},
	    download_file(Params, 0, ftpI:read(Proto, Pid, Handle, 65536)),
 	    ftpI:close(Proto, Pid, Handle),
	    file:close(Fd);
	{error, Reason} ->
	    Info = file:format_error(Reason),
	    update_progress(mgr, [{additionalInfo, Info}]),
	    erlang:error(Reason)	    
    end,

    %% Create bu package
    update_progress(mgr, [{additionalInfo, "Transport complete. Unpacking."}]),
    Index = get_next_index(),
    Fun = fun() -> do_handle_import_backup(DnRev, TmpPath, Index) end,
    case mnesia:transaction(Fun) of
	{atomic, {MoRef, Metadata}} -> 
	    #{backupName := Name} = Metadata,
	    store_metadata(Index, Metadata),
	    swmLib:unlock_backup(Index),
	    swmLib:write_swm_log("BrmBackup="++Index, info, 
				 "Backup imported: "++Name),
	    file:delete(TmpPath),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    update_progress(mgr, [{additionalInfo, "Import complete"},
				  {result, ?ActionResultType_SUCCESS},
				  {resultInfo, MoRef},
				  {progressPercentage, 100},
				  {state, ?ActionStateType_FINISHED},
				  {timeActionCompleted, CompleteTime}]),
	    ok;
	{aborted, {duplicate_name, Name}} ->
	    file:delete(TmpPath),
	    swmLib:unlock_backup(Index),
	    update_progress(mgr, [{resultInfo, "Duplicate name "++Name}]),
	    swmLib:write_swm_log("BrmBackupManager", error, 
				 "Duplicate name "++Name),
	    remove_backup_dir(Index),
	    throw({duplicate_name, Name});
	{aborted, {inconsistent_software, SwVersions}} ->
	    file:delete(TmpPath),
	    swmLib:unlock_backup(Index),
	    [begin
		 Msg = "Required software: "++format_product_data(Sw),
		 swmLib:write_swm_log("BrmBackupManager", error, Msg),
		 update_progress(mgr,[{additionalInfo, Msg}])
	     end||Sw<-SwVersions],
	    update_progress(mgr, [{resultInfo, "The applicable software is not installed"}]),
	    remove_backup_dir(Index),
	    throw({inconsistent_software, SwVersions});
	{aborted, MnesiaReason} ->
	    file:delete(TmpPath),
	    do_remove_backup_dir(Index),
	    swmLib:unlock_backup(Index),
	    erlang:error(MnesiaReason)
    end.

format_product_data(Pd) ->
    Pd#'ProductData'.productName++" "++
	Pd#'ProductData'.productNumber++" "++
	Pd#'ProductData'.productRevision.

do_handle_import_backup(DnRev, TmpPath, Index) ->
    BuDir = swmLib:backup_dir(Index),
    cmd(["mkdir -p ", BuDir]),
    cmd(["cd ", BuDir, " ; unzip ", TmpPath]),
    {ok, Bin} = file:read_file(filename:join(BuDir, "export.bup")),
    Cleartext = crypto:block_decrypt(aes_cfb128,
				     <<"1234567890ABCDEF">>,
				     <<"1234567890ABCDEF">>, Bin),
    ok = file:write_file(filename:join(BuDir, "export.tgz"), Cleartext),
    cmd(["cd ", BuDir, " ; tar xfz export.tgz"]),
    file:delete(filename:join(BuDir, "export.bup")),
    file:delete(filename:join(BuDir, "export.tgz")),

    swmLib:sync(),
    Metadata = get_metadata(Index),

    #{backupName := Name,
      creationTime := CreationTime,
      swVersion := SwVersion} = Metadata,

    case swmBackupModel:validate_name(Name) of
	ok ->
	    ok;
	duplicate_name ->
	    mnesia:abort({duplicate_name, Name})
    end,

    case swmModel:get_matching_up(SwVersion) of
	nomatch ->
	    case sysEnv:rcs_mode_2() of
		vrcs ->
		    ok;
		_ ->
		    mnesia:abort({inconsistent_software, SwVersion})
	    end;
	_ ->
	    ok
    end,

    %% Add backup in BrM
    BrmBackupManagerKey = comsaGeneric:dnrev_to_key(DnRev), 
    BrmBackupKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++[Index]),

    BrmBackup = #brmBackup{brmBackupId = BrmBackupKey,
			   backupName = Name,
			   creationTime = CreationTime,
			   status = ?BrmBackupStatus_BRM_BACKUP_COMPLETE,
			   progressReport = default_idle_pr(),
			   swVersion = SwVersion,
			   %% Imported backups are always considered manual
			   creationType = ?BrmBackupCreationType_MANUAL},
    mnesia:write(BrmBackup),

    LsKey = list_to_tuple(tuple_to_list(BrmBackupManagerKey)++["1"]),
    [LS] = mnesia:read({brmBackupLabelStore, LsKey}),
    mnesia:write(LS#brmBackupLabelStore{lastImportedBackup=Name}),
    {_, _, BrM, BMgr} = BrmBackupManagerKey,
    {make_dn(["BrM",BrM,
	      "BrmBackupManager",BMgr,
	      "BrmBackup",Index]), 
     Metadata#{creationType => ?BrmBackupCreationType_MANUAL}}.

%%% ----------------------------------------------------------
%%% @doc Read the size of the remote file
%%% @end


file_size(sftp, Pid, Path) ->
    case ssh_sftp:read_file_info(Pid, Path, 10000) of
	{error, Reason} ->
	    throw({read_file_info, {Reason, Path}});
	{ok, FIO} ->
	    case FIO#file_info.type of
		regular ->
		    FIO#file_info.size;
		_ ->
		    0
	    end
    end;

file_size(ftpes, Pid, Path) ->
    case ftpesI:is_directory(Pid, Path) of
        file-> 
            case ftpesI:size(Pid, Path) of
                {error, _Reason} -> 0;
                Size-> Size
            end;
        _-> 0
    end.

%%% ----------------------------------------------------------
%%% @doc Download the backup file and update progress at the same time

download_file(Params, AccuSize, {ok, Data}) ->
    #transfer{scheme = Proto, 
          fd = Fd, 
	      pid = Pid, 
	      handle = Handle, 
	      rp_key = RpKey, 
	      total_size = TotalSize} = Params,
    receive
	cancel_mgr -> 
	    file:close(Fd),
	    file:delete(Params#transfer.path),
	    throw(cancelled)
    after 0 -> ok
    end,
    garbage_collect(),
    NewAccuSize = AccuSize + size(Data),
    ok = file:write(Fd, Data),
    update_progress(RpKey, [{progressPercentage, AccuSize*100 div TotalSize}]),

    download_file(Params, NewAccuSize, ftpI:read(Proto, Pid, Handle, 65536));
download_file(_, _, eof) ->
    ok;
download_file(Params, _, {error, Reason}) ->
    Info = case Reason of
	       no_such_file -> "no such file or directory on remote side";
	       _ -> lists:flatten(io_lib:format("~p",[Reason]))
	   end,
    update_progress(Params#transfer.rp_key, [{additionalInfo, Info}]),
    erlang:error(Reason).


cleanup() ->
    cleanup(get(protocol)).

cleanup(sftp) ->
    case get(channelPid) of
        Pid when is_pid(Pid) ->
            case get(handle) of
                undefined ->
                    ok;
                _-> 
                    ftpI:close(sftp, Pid, get(handle))
            end,
            ftpI:stop_channel(sftp, Pid);
        undefined -> ok
    end,
    case get(connectionRef) of
    CRef when is_pid(CRef) -> ssh:close(CRef);
    undefined -> ok
    end;
cleanup(ftpes) ->
    case get(channelPid) of
    Pid when is_pid(Pid) ->
            %% close file if opened
            ftpI:close(ftpes, Pid, []),
            ftpI:stop_channel(ftpes, Pid);
    undefined -> ok
    end;
cleanup(_Other) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Execute a shell command and print the result in the erlang shell

cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.

%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    try
	sysInitI:info_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:info_msg("~w: "++Format, [?MODULE|Args])
    end.

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     try
%% 	sysInitI:warning_msg("~w: "++Format, [?MODULE|Args])
%%     catch
%% 	_ : _ ->
%% 	    %% During startup, before sysInitI has started!
%% 	    error_logger:warning_msg("~w: "++Format, [?MODULE|Args])
%%     end.

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    try
	sysInitI:error_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:error_msg("~w: "++Format, [?MODULE|Args])
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
