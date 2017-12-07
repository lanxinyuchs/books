%%% ----------------------------------------------------------
%%% %CCaseFile:	logLib.erl %
%%% Author:	eolaand
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(logLib).
-id('Updated by CCase').
-vsn('/main/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-11-16').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name     What
%%% -----      -------    -------- ---------------------------
%%% R8A/1      2016-11-10 uabesvi  Created
%%% R8A/2      2016-11-24 uabesvi  Encrypted logs
%%% R9A/2-3    2017-03-27 uabesvi  Added code for compress
%%% R10A/1     2017-06-07 uabesvi  Added new functions
%%% R11A/2     2017-09-04 etxjotj  Replaced swmLib with swmI
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% MODULE INTERFACE
%%% ----------------------------------------------------------
%%%-export([start/0]).


%%% ----------------------------------------------------------
%%% OTHER EXPORTED FUNCTIONS
%%% ----------------------------------------------------------

-export([is_encrypted/1,
	 is_local/1,
	 is_compressed/1,
	 is_internal/1,
	 use_milli_sec/1,
	 get_log_dir/1,
	 get_encryption_type/1,
	 log_dir/1,
	 log_storage_mode_create/4,
	 log_storage_mode_update/2,
	 log_storage_mode_delete/1,
	 log_storage_mode_exists/1,
	 decrypt_file/2,
	 log_data_update/2,
	 get_all_objects/1,
	 open_disk_log/3,
	 get_attr/2,
	 choose/3
	]).


%%% ----------------------------------------------------------
%%% HEADER FILES AND MACRO DEFINITIONS
%%% ----------------------------------------------------------
-include("log.hrl").
-include("om.hrl").


-define(SAF_VSN, #safsVersion{releaseCode  = $A,
			      majorVersion = 2,
			      minorVersion = 11}).


%%% ----------------------------------------------------------
%%% TYPES AND RECORDS
%%% ----------------------------------------------------------
%%%-type foo() :: bar().
%%%-record(state, {qwe, rty}).

-type logId()   :: any().


%%% ----------------------------------------------------------
%%% FUNCTIONS
%%% ----------------------------------------------------------


%%=======================================================================
%% log_storage_mode_create
%%=======================================================================
-spec log_storage_mode_create(logId(), boolean(), boolean(), boolean()) ->
	  ok | {error, exists}.

log_storage_mode_create(LogId, IsLocal, IsEncrypted, IsCompressed) ->
    case mnesia:dirty_read(logStorageMode, LogId) of
	[] ->
	    sysInitI:info_msg("Create logStorageMode DB for ~p:~n"
			      "local      = ~p~n"
			      "encrypted  = ~p~n"
			      "compressed = ~p~n", 
			      [LogId, IsLocal, IsEncrypted, IsCompressed]),
	    mnesia:dirty_write(#logStorageMode{logId      = LogId,
					       local      = IsLocal,
					       encrypted  = IsEncrypted,
					       compressed = IsCompressed});
	_Other ->
	    {error, exists}
   end.


%%=======================================================================
%% log_storage_mode_update
%%=======================================================================
-type compressedLog() :: {compressed, boolean()}.
-type update_opt() :: compressedLog().
-type update_opt_list() :: [update_opt()].

-spec log_storage_mode_update(logId(), update_opt_list()) ->
	  ok | {error, not_existing} | {error, illegal_parameter_value}.

log_storage_mode_update(_LogId, []) ->
    ok;
log_storage_mode_update(LogId, Options) ->
    NewCompr = proplists:get_value(compressed, Options),
    case mnesia:dirty_read(logStorageMode, LogId) of
	_ when NewCompr == undefined ->
	    ok;
	[Rec] ->
	    lsmu(LogId, Rec, NewCompr);
	_Other ->
	    {error, not_existing}
   end.

lsmu(LogId, Rec, Compressed) when is_boolean(Compressed) ->
    sysInitI:info_msg("Updated log storage mode entry for ~p:~n"
		      "compressed = ~p~n",
		      [LogId, Compressed]),
    mnesia:dirty_write(Rec#logStorageMode{compressed = Compressed});
lsmu(LogId, _Rec, Compressed) ->
    sysInitI:warning_msg("Illegal parameter value (not boolean)~n"
			 "Log storage mode entry not updated for ~p:~n"
			 "  compressed = ~p~n",
			 [LogId, Compressed]),
    {error, illegal_parameter_value}.


%%=======================================================================
%% log_storage_mode_delete
%%=======================================================================
-spec log_storage_mode_delete(LogId::string()) -> ok.
log_storage_mode_delete(LogId) ->
    mnesia:dirty_delete(logStorageMode, LogId).


%%=======================================================================
%% log_storage_mode_exists
%%=======================================================================
-spec log_storage_mode_exists(LogId::string()) -> true | false.
log_storage_mode_exists(LogId) ->
    case mnesia:dirty_read(logStorageMode, LogId) of
	[#logStorageMode{}] ->
	    true;
	_ ->
	    false
    end.


%%=======================================================================
%% log_data_update
%%=======================================================================
-type public() :: {public, boolean()}.
-type log_dataupdate_opt() :: public().
-type update_log_data_opt_list() :: [log_dataupdate_opt()].

-spec log_data_update(logId(), update_log_data_opt_list()) ->
	  ok | {error, not_existing} | {error, illegal_parameter_value}.

log_data_update(LogId, Options) ->
    NewPublic = proplists:get_value(public, Options),
    NewMS     = proplists:get_value(milliSec, Options),
    case logDb:log_data_get_dirty({"1","1","1",LogId}) of
	_ when NewPublic == undefined andalso
	       NewMS     == undefined ->
	    ok;
	[#logData{internal = OldInter,
		  milliSec = OldMS} = Rec] ->
	    Public = choose(NewPublic == undefined, not OldInter, NewPublic),
	    MS     = choose(NewMS     == undefined, OldMS,        NewMS),
	    ldu(LogId, Rec, Public, MS);
	_Other ->
	    {error, not_existing}
   end.

ldu(LogId, Rec, Public, MS)
  when is_boolean(Public) andalso
       is_boolean(MS) ->
    sysInitI:info_msg("Updated log data entry for ~p:~n"
		      "  public   = ~p~n"
		      "  milliSec = ~p~n", 
		      [LogId, Public, MS]),
    logDb:log_data_set_dirty(Rec#logData{internal = not Public,
					 milliSec = MS});
ldu(LogId, _Rec, Public, MS) ->
    sysInitI:warning_msg("Illegal parameter value (not boolean)~n"
			 "Log data entry not updated for ~p:~n"
			 "  public   = ~p~n"
			 "  milliSec = ~p~n", 
			 [LogId, Public, MS]),
    {error, illegal_parameter_value}.
   

%%=======================================================================
%% is_local
%%=======================================================================
-spec is_local(LogId::string()) -> boolean().
is_local(LogId) ->
    case mnesia:dirty_read(logStorageMode, LogId) of
	[#logStorageMode{local=true}] ->
	    true;
	_ ->
	    false
    end.
    

%%=======================================================================
%% is_encrypted
%%=======================================================================
-spec is_encrypted(LogId::string()) -> boolean().
is_encrypted(LogId) ->
    case mnesia:dirty_read(logStorageMode, LogId) of
	[#logStorageMode{encrypted=true}] ->
	    true;
	_ ->
	    false
    end.
    

%%=======================================================================
%% is_compressed
%%=======================================================================
-spec is_compressed(LogId::string()) -> boolean().
is_compressed(LogId) ->
    case mnesia:dirty_read(logStorageMode, LogId) of
	[#logStorageMode{compressed = true}] ->
	    true;
	_ ->
	    false
    end.

    
%%=======================================================================
%% is_internal
%%=======================================================================
-spec is_internal(LogId::string()) -> boolean().
is_internal(LogId) ->
    case mnesia:dirty_read(logData, {"1","1","1",LogId}) of
	[#logData{internal = true}] ->
	    true;
	_ ->
	    false
    end.

    
%%=======================================================================
%% use_milli_sec
%%=======================================================================
-spec use_milli_sec(LogId::string()) -> boolean().
use_milli_sec(LogId) ->
    case mnesia:dirty_read(logData, {"1","1","1",LogId}) of
	[#logData{milliSec = true}] ->
	    true;
	_ ->
	    false
    end.
    


%%=======================================================================
%% get_log_dir
%%=======================================================================
-spec get_log_dir(logId()) -> {ok, string()} | {error, any()}.
get_log_dir(LogId) ->
    case mnesia:dirty_read(logStorageMode, LogId) of
	[#logStorageMode{local = true}] ->
	    Dir = log_dir_local(),
	    {ok, Dir};
	[#logStorageMode{local = false}] ->
	    Dir = log_dir_central(),
	    {ok, Dir};
	[] ->
	    {error, {not_exist, LogId}}
    end.


%%=======================================================================
%% get_encryption_type
%%=======================================================================
-spec get_encryption_type(logId()) -> {ok, cleartext|encrypted} | 
				      {error, any()}.
get_encryption_type(LogId) ->
    case mnesia:dirty_read(logStorageMode, LogId) of
	[#logStorageMode{encrypted=true}] ->
	    {ok, encrypted};
	[#logStorageMode{encrypted=false}] ->
	    {ok, cleartext};
	[] ->
	    {error, {not_exist, LogId}}
    end.


%%=======================================================================
%% log_dir
%%=======================================================================
-spec log_dir(logId()) -> string().
log_dir(LogId) ->
    case get_log_dir(LogId) of
	{ok, Dir} ->
	    filename:join([Dir, LogId]);
	_ ->
	    filename:join([log_dir_central(), LogId])
    end.


log_dir_central() ->
    filename:join([sysEnv:vnf_dir(), "log"]).


log_dir_local() ->
    filename:join([sysEnv:rcs_dir(), "log"]).

get_all_objects(Tab) ->
    gao(swmI:is_old_table(Tab), Tab).

gao(true, Tab)   -> swmI:all_objects(Tab);
gao(false, _Tab) -> [].



%%=======================================================================
%% open_disk_log
%%=======================================================================
open_disk_log(StreamName, MaxKb, MaxNoFiles) ->
    LogDir  = log_dir(StreamName),
    LogFile = filename:join(LogDir, StreamName),
    MaxSize = ?GET_LOG_SIZE_BYTES(MaxKb),
    filelib:ensure_dir(LogFile),

    Opts = [{name,   StreamName},
	    {file,   LogFile},
	    {format, odl_format(StreamName)},
	    {notify, true},
	    {mode,   read_write}] ++ odl_options(MaxNoFiles, MaxSize),
    
    info_msg("Opening log ~p ~n",[StreamName]),
    disk_log:open(Opts).


odl_format(StreamName) ->
    Bool = logLib:is_encrypted(StreamName) andalso  
	certI:is_encryption_enabled(), 
   choose(Bool, internal, external).


odl_options(undefined, MaxSize) ->
    [{type, halt},
     {size, MaxSize}];
odl_options(MaxNoFiles, MaxSize) ->
    [{type, wrap},
     {size, {MaxSize, MaxNoFiles}}].






%%========================================================================
%% decrypt_file(FromFile, ToFile) -> ok | {error, {Where, Reason}}
%% 
%% decrypt FromFile and write the result in ToFile
%% 
%% Where = file_open | file_write |  
%%         wrap_log_reader_open | wrap_log_reader_read | decrypt
%%========================================================================
decrypt_file(FromFile, ToFile) ->
    df(file:open(ToFile, [write, raw, binary]),
       open_encrypted_log(FromFile),
       ToFile).


df({ok, FD}, {ok, Cont}, ToFile) ->
    Res = df_loop({cont, Cont},  FD),
    df_close(file:close(FD), ToFile),
    Res;
df({error, Error}, _, _ToFile) ->
    {error, {file_open, Error}};
df(_, {error, Error}, _ToFile) ->
    {error, {wrap_log_reader_open, Error}}.


df_loop({cont, Cont}, FD) ->
    Res = df_read(wrap_log_reader:chunk(Cont),  FD),
    df_loop(Res, FD);
df_loop(ok, _) ->
    ok;
df_loop(Error, _) ->
    Error.


df_read({error, Reason}, _ToFile) ->
    {error, {wrap_log_reader_read, Reason}};
df_read({Cont, Data}, _ToFile)
  when Data == eof;
       Data == [] ->
    wrap_log_reader:close(Cont),
    ok;
df_read({Cont, _, NoBytes}, _ToFile) ->
    wrap_log_reader:close(Cont),
    {error, {wrap_log_reader_read, {corrupt, NoBytes}}};
df_read({Cont, EncryptedData}, ToFile) ->
    Res = [df_decrypt(certI:decrypt_data(D), ToFile) || D <- EncryptedData],
    case [E || {error, _} = E <- Res] of
	[] ->
	    {cont, Cont};
	[Error | _] ->
	    wrap_log_reader:close(Cont),
	    Error
    end.

df_decrypt({ok, BinData}, ToFile) ->
    case file:write(ToFile, BinData) of
	ok             -> ok;
	{error, Error} -> {error, {file_write, Error}}
    end;
df_decrypt({error, Error}, _) ->
    {error, {decrypt, Error}}.

df_close(ok, _) -> 
    ok;
df_close({error, Error}, ToFile) -> 
    sysInitI:info_msg("Could not close file when decrypting ~p:~n"
		      "  File  = ~p~n"
		      "  Error = ~p~n", 
		      [ToFile, Error]).



open_encrypted_log(LogFile) ->
    {No, [_ | Elif]} = lists:splitwith(fun(C) -> [C] /= "." end, r(LogFile)),
    wrap_log_reader:open(r(Elif), list_to_integer(No)).




%%========================================================================
%% IMM function calls
%% 
%% Read imm to get the fix record length for the logs
%%========================================================================
get_attr(ObjDN, AttrName) ->
    case get_attrs(ObjDN, [AttrName]) of
	{ok, [{_Key, Val}]} ->
	    {ok, Val};
	Error ->
	    Error
    end.


get_attrs(ObjDN, Attrs) ->
    H = initialize_om_ah(),
    Res = get_attrs(H, ObjDN, Attrs),
    finalize_om_ah(H),
    Res.

get_attrs({_OH, AH}, ObjDN, Attrs) ->
    get_attrs(AH, ObjDN, Attrs);
get_attrs(AH, ObjDN, Attrs) ->
    case safs_imm_om:accessor_get_2(AH, ObjDN, Attrs) of
	{ok, AttrVals} ->
	    Vals = [{Name, Val} || {Name, _Type, Val} <- AttrVals],
	    {ok, Vals};
	Error ->
	    Error
    end.


initialize_om() ->
    {ok, OmHandle, _} = safs_imm_om:initialize(undefined, ?SAF_VSN),
    OmHandle.

finalize_om(OmHandle) ->
    ok = safs_imm_om:finalize(OmHandle).


initialize_ah(OH) ->
    {ok, AH} = safs_imm_om:accessor_initialize(OH),
    AH.

finalize_ah(AH) ->
    ok = safs_imm_om:accessor_finalize(AH).


initialize_om_ah() ->
    OH = initialize_om(),
    AH = initialize_ah(OH),
    {OH, AH}.

finalize_om_ah({OmHandle, AH}) ->
    finalize_ah(AH),
    finalize_om(OmHandle).




choose(true,  T, _) -> T;
choose(false, _, F) -> F.

r(L) ->
    lists:reverse(L).


info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

