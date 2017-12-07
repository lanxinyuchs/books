%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysFi.erl %
%%% @author eivomat
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1

%%% @doc ==File interface==
%%% This module implements a server process for the File interface.
%%% All times are in milliseconds unless otherwise stated.
%%% @end

-module(sysFi).
-behaviour(gen_server).
-vsn('/main/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-11-14').
-author('eivomat').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% Rev        Date         Name        What
%%% -----      -------      --------    ------------------------
%%% R3A/1      2014-09-11   etxpejn     Created
%%% R3A/2      2014-09-16   etxpejn     Changed dir to fullpath
%%% R3A/3      2014-09-17   etxpejn     Removed FiDir for export
%%% R3A/5      2014-10-14   etxpejn     Added call to sysSftp:start_channel
%%% R3A/7      2015-01-09   erarafo     PM Event ROP files, work in progress
%%% R3A/8      2015-01-09   erarafo     Dialyzer complaint fixed
%%% R3A/9      2015-01-12   erarafo     Settable parameters for volatile files
%%% R3A/10     2015-01-14   erarafo     Set up symlinks from SFTP directory
%%% R3A/11     2015-01-15   erarafo     Local time vs UTC issue fixed
%%% R3A/12     2015-01-15   erarafo     Filespace limit adjusted
%%% R3A/13     2015-01-20   erarafo     Separation of PMS and PES SFTP services
%%% R3A/14     2015-01-20   erarafo     Bug fixed
%%% R3A/15     2015-01-21   erarafo     Robustness, Edoc
%%% R3A/16     2015-01-21   erarafo     Parameter setting the COLI way
%%% R3A/17     2015-01-21   erarafo     Edoc; trigger a build
%%% R3A/18     2015-01-22   erarafo     Cleanup after R3A/16 
%%% R3A/19     2015-01-26   erarafo     Functions for creating persistent files
%%% R3A/25     2015-02-23   eolaand     Disable reclaimFiles, done by LRAT 
%%% R3A/26     2015-02-25   eolaand     Remove symlink before create
%%% R3A/29     2015-04-02   eolaand     Store sftp dirs in mnesia and cleanup
%%%                                     obsolete code
%%% R3A/30     2015-04-16   eolaand     Set write permission for all on ram dir
%%% R3A/31     2015-04-20   eolaand     Ensure dir before creating softlink 
%%% R3A/31     2015-04-22   eolaand     Check that sftp dir is a basename in
%%%                                     register_sftp_dir/1. More cleaning up.
%%% R4A/1      2015-07-07   etxberb     Changed mnesia:create_table to
%%%                                     clhI:mnesia_create_table.
%%% R4A/3      2015-07-24   eolaand     Handle root_dir for RAM dir.
%%% R4A/5      2015-09-29   eolaand     Add init_data function for reservation
%%%                                     of disc space in sysServer. 
%%%                                     Hardcoded for now.
%%% R4A/6      2015-10-16   eolaand     Increase gen_server:call timeout to 
%%%                                     30 sec in register_sftp_dir.
%%% R4A/7      2015-11-02   eolaand     Remove /tmp/sys_fi dir that has been
%%%                                     kept for backwards compatibility
%%% R4A/10     2015-11-11   eolaand     Check directory max size in write_file.
%%%                                     Add fcn delete_oldest. Add fcns for 
%%%                                     registration of sftp dir in data init.
%%% R4A/11     2015-11-12   eolaand     Remove obsolete and commented out code
%%% R5A/1      2016-02-11   eolaand     Add registration of sftp dir with size
%%%                                     in init_data phase.
%%% R5A/2      2016-02-12   eolaand     Remove hardcoded call to reserve_disk.
%%% R7A/1      2016-10-17   etxpejn     Added IMPORT_FILE_2 & EXPORT_FILE_2
%%% R7A/2      2016-10-18   etxpejn     Corrected call to comsaI:decrypt_password/1
%%% R8A/2      2016-12-13   eolaand     Use sysEnv:vnf_dir() instead of rcs_dir
%%% R8A/3      2017-01-13   estjako     Added get_session_info/1, get_session_info/2,
%%%                                     get_session_info/3, get_session_info/4 functions
%%% R9A/1      2017-01-13   eivmiha     switched ssh_sftp and sysSftp to ftpI
%%% R9A/2      2017-02-07   eivmiha     Added support for FTPES
%%% R10A/1     2017-05-29   ejinfeg	Added IMPORT_FILE_3 
%%% R10A/3     2017-06-02   ejinfeg	Added get_file_size/2
%%% R10A/4     2017-06-29   ejinfeg	Removed get_file_size/2, added get_file_size/3
%%% R10A/5     2017-07-07   ejinfeg	fix the error in function get_file_size/3
%%% R10A/6     2017-07-11   ejinfeg	check free disk size before starting to load ftp file
%%% R11A/1     2017-08-10   etxpejn	Cor HW13739, support of multiple users of FI API 
%%% R12A/1     2017-11-14   eivomat     HW43425 Delete file through cast
%%% ----------------------------------------------------------

-export([start/0,
	 start/1,
	 start_link/0,
	 start_link/1,
	 stop/0]).

-export([cec_setup/1]).

-export([init_tables/1,
	 init_data/0]).

-export([register_sftp_dir/1,
	 register_sftp_dir/3,
	 register_sftp_ram_dir/3,
	 register_sftp_ram_dir/4,
	 get_sftp_reg_dir/1,
	 get_sftp_reg_dirs/0,
	 sftp_root/0,
	 get_reg_dir_root/1]).

-export([get_sftp_session_info/1,
	 get_sftp_session_info/2,
	 get_sftp_session_info/3,
	 get_sftp_session_info/4,
     get_session_info/1,
     get_session_info/2,
     get_session_info/3,
     get_session_info/4]).

-export([list_dir/1,
	 write_file/3, 
	 write_file_info/4, 
	 delete/2, 
	 delete_older/2, 
	 delete_oldest/1, 
	 get_oldest/1, 
	 read_file/2, 
	 read_file_info/3, 
	 absname/1,
	 sftp_absname/1,
	 sftp_absname/2,
	 reg_absname/1,
	 reg_absname/2,
	 clean_dir/1,
         get_file_size/3]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([setFiParams/1]).

-export([handle_file/5]).
-export([import_file/5]).


-include("sys.hrl").
-include_lib("kernel/include/file.hrl").


-define(SERVER, ?MODULE).

-define(IMPORT_FILE, 0).
-define(EXPORT_FILE, 1).
-define(SPACE_AVAILABLE, 2).
-define(IMPORT_FILE_2, 3).
-define(EXPORT_FILE_2, 4).
-define(IMPORT_FILE_3, 5).

%% Must match codes in csrc/fi.h
-define(FI_OK, 0).
-define(FI_INVALID_URI, 1).
-define(FI_FAILED_TO_CONNECT_TO_SFTP_SERVER, 2).
-define(FI_FAILED_TO_IMPORT_OR_EXPORT_FILE, 3).
-define(FI_SFTP_SYMLINK_INVALID_NAME, 12).
-define(FI_SFTP_SYMLINK_FAILED_TO_CREATE, 13).
-define(FI_SFTP_SYMLINK_FAILED_TO_DELETE, 14).
-define(FI_SFTP_SYMLINK_TOO_MANY_ATTEMPTS, 15).
-define(FI_SFTP_SYMLINK_UNSPECIFIED_ERROR, 16).
-define(FI_FAILED_TO_DECRYPT_PASSWORD, 17).
-define(FI_FILE_SIZE_EXCEED_LIMITATION, 18).
-define(FI_NO_ENOUGH_DISK_SPACE, 19).

-define(VOLATILE_SPACE_LIMIT, 264*1024*1024).
-define(VOLATILE_STALE_LIMIT, 3600*1000).

-define(VOLATILE, 0).
-define(PERSISTENT, 1).

-define(DEFAULT(X, D), if X =:= 0 -> D; true -> X end).

-record(state, 
	{sockets=[]                                      :: list(),
	 volSpaceLimit=?VOLATILE_SPACE_LIMIT             :: non_neg_integer(),
	 volStaleLimit=?VOLATILE_STALE_LIMIT             :: non_neg_integer(),
	 volDir=""                                       :: string(),
	 sftpRegDirs=[]                                  :: list()
	}).


%%===========================================================================
%% Init functions
%%===========================================================================
init_tables(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(sysSftpRegDirs,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   sysSftpRegDirs)} |
				  sysDataInit:add_clh_option(sysSftpRegDirs)]),
    ok.


init_data() ->
    ok.


%%% ----------------------------------------------------------
%%% @doc 
%%% Register SFTP directory and reserve disc size in KB for the directory.
%%% This function must only be called during the init data phase.<br/> 
%%% Module is a file_handler callback module as defined in the subsystem spec
%%% for OTP sftpd.
%%% @end
%%% ----------------------------------------------------------
-spec register_sftp_dir(Dir::string(), Size::integer(), Mod::atom()) -> 
			       ok | {error, any()}.
register_sftp_dir(Dir, Size, Mod) 
  when is_list(Dir), is_atom(Mod), is_integer(Size) ->
    Mod =/= undefined andalso ({module, _Mod} = code:ensure_loaded(Mod)),
    case put_sftp_reg_dir(sftp_root(), clean_dir(Dir), disc, Size, Mod) of
	ok ->
	    FileHandler = file_handler(Mod),
	    sysServer:reserve_disk(sftp_absname(Dir), FileHandler, Size);
	Error ->
	    Error
    end.
	    

%%% ----------------------------------------------------------
%%% @doc 
%%% Register SFTP RAM directory.<br/>
%%% Root is a tmpfs RAM directory where Dir will be created.<br/> 
%%% Module is a file_handler callback module as defined in the subsystem spec
%%% for OTP sftpd.
%%% @end
%%% ----------------------------------------------------------
-spec register_sftp_ram_dir(Dir::string(), Root::string(), Mod::atom()) -> 
				   ok | {error, any()}.
register_sftp_ram_dir(Dir, Root, Mod) ->
    register_sftp_ram_dir(Dir, Root, undefined, Mod). 


register_sftp_ram_dir(Dir, Root, Size, Mod) 
  when is_list(Dir), 
       is_list(Root), 
       is_integer(Size) orelse Size =:= undefined,
       is_atom(Mod) ->
    Mod =/= undefined andalso ({module, _Mod} = code:ensure_loaded(Mod)),
    put_sftp_reg_dir(Root, clean_dir(Dir), ram, Size, Mod).


file_handler(undefined) ->
    sysSshSftpd:file_handler();

file_handler(Mod) ->
    Mod.


cec_setup(Socket) ->
    try
	gen_server:call(?SERVER, {cec_setup, Socket}, 30000)
    catch exit:{timeout, _} = Reason ->
            sysInitI:error_report([{mfa, {?MODULE, cec_setup, []}},
				       {caught, {exit, Reason}},
				       {socket, Socket}
				      ]),
	    throw(Reason)
    end.


%%===========================================================================
%% API functions
%%===========================================================================
get_session_info({ftpes, UserData})-> 
    ftpesI:get_ftpes_session_info(UserData);
get_session_info(UserData) ->
    get_sftp_session_info(UserData).


get_session_info({ftpes, UserData}, KeyOrSession) ->
    ftpesI:get_ftpes_session_info(UserData, KeyOrSession);
get_session_info(UserData, KeyOrSession) ->
    get_sftp_session_info(UserData, KeyOrSession).


get_session_info({ftpes, UserData}, Key, Default) ->
    ftpesI:get_ftpes_session_info(UserData, Key, Default);
get_session_info(UserData, Key, Default) ->
    get_sftp_session_info(UserData, Key, Default).


get_session_info({ftpes, UserData}, _Session, Key, Default) ->
    ftpesI:get_ftpes_session_info(UserData, Key, Default);
get_session_info(UserData, Session, Key, Default) ->
    get_sftp_session_info(UserData, Session, Key, Default).


get_sftp_session_info(Tab) ->
    sysSshSftpd:get_session_info(Tab).


get_sftp_session_info(Tab, KeyOrSession) ->
    sysSshSftpd:get_session_info(Tab, KeyOrSession).


get_sftp_session_info(Tab, Key, Default) ->
    sysSshSftpd:get_session_info(Tab, Key, Default).


get_sftp_session_info(Tab, Session, Key, Default) ->
    sysSshSftpd:get_session_info(Tab, Session, Key, Default).
    

register_sftp_dir({Root, Dir, Type, Size, Mod}) 
  when is_list(Dir) andalso is_atom(Mod) andalso
       (Type =:= disc orelse Type =:= ram) andalso 
       (Type =:= disc orelse is_list(Root)) andalso
       (Type =:= ram orelse Root =:= undefined) andalso
       (Size =:= undefined orelse is_integer(Size)) ->
    try
	Mod =/= undefined andalso ({module, _Mod} = code:ensure_loaded(Mod)),
	%% Dir must be a basename and not a pathname. Leading ./ or / is 
	%% however ignored.
	CleanDir = clean_dir(Dir),
	gen_server:call(?SERVER, {register_sftp_dir, 
				  {Root, CleanDir, Type, Size, Mod}}, 30000)
    catch 
	_E:Reason ->
	    sysInitI:error_msg("~p: Failed to register SFTP dir ~s~n~p~n",
				   [?MODULE, Dir, Reason]),
	    {error, invalid_sftp_dir}
    end;

register_sftp_dir({Root, Dir, ram = Type, Mod}) ->
    register_sftp_dir({Root, Dir, Type, undefined, Mod});

register_sftp_dir({Dir, disc = Type, Size, Mod}) ->
    register_sftp_dir({undefined, Dir, Type, Size, Mod});

register_sftp_dir({Dir, disc = Type, Mod}) ->
    register_sftp_dir({Dir, Type, 30000, Mod});

register_sftp_dir({Dir, Type}) ->
    register_sftp_dir({Dir, Type, undefined});

register_sftp_dir([Ch | _] = Dir) when is_integer(Ch) ->
    register_sftp_dir({Dir, disc, undefined});

register_sftp_dir(Dirs) when is_list(Dirs) ->
    lists:foreach(fun(Dir) ->
			  register_sftp_dir(Dir)
		  end, Dirs). 


get_sftp_reg_dir(Dir) ->
    gen_server:call(?SERVER, {get_sftp_reg_dir, Dir}).

%%% ----------------------------------------------------------
%%% @doc COLI command for test purposes. The given argument list
%%% must contain exactly 4 numeral strings.
%%% @end
%%% ----------------------------------------------------------

setFiParams(Args) ->
    gen_server:cast(
      ?SERVER,  
      list_to_tuple([set_parameters|[list_to_integer(A)||A <- Args]])).


%%% ----------------------------------------------------------
%%% @doc List all files the directory Dir. The Dir argument specifies a 
%%% directory that has been registered with the sysFi:register_sftp_dir() 
%%% function.
%%% @end
%%% ----------------------------------------------------------
-spec list_dir(string()) -> {ok, Files::list()} | {error, any()}.

list_dir(Dir) -> 
    case reg_absname(Dir) of
	{ok, AbsName} ->
	    file:list_dir(AbsName);
	Error ->
	    Error
    end.

%%% ----------------------------------------------------------
%%% @doc The Dir argument specifies a directory that has been registered 
%%% with the sysFi:register_sftp_dir() function. Filename is a slash-free
%%% non-empty string. Bytes is an iodata() item. 
%%% Returns ok if successful. The error reason 'eexist' indicates that a
%%% file, directory or symlink already exists.
%%% @end
%%% ----------------------------------------------------------
-spec write_file(string(), string(), iodata()) -> ok | {error, any()}.

write_file(Dir, Filename, Bytes) -> 
    case check_total_file_size(Dir, Filename, Bytes) of
	{ok, AbsName} ->
	    file:write_file(AbsName, Bytes);
	Error ->
	    Error
    end.
    
%%% ----------------------------------------------------------
%%% @doc The file specified by the given Dir and Filename
%%% strings is deleted. 
%%% @end
%%% ----------------------------------------------------------
-spec delete(string(), string()) -> ok | {error, any()}.

delete(Dir, Filename) -> 
    file:delete(sftpObjectName(Dir, Filename)).
%%% ----------------------------------------------------------
%%% @doc All files in the given Dir that are older than Time
%%% are deleted. 
%%% @end
%%% ----------------------------------------------------------
-spec delete_older(string(), integer() | tuple()) -> ok.
delete_older(Dir, Seconds) ->
    gen_server:cast(?SERVER, {handle_delete_older, Dir, Seconds}).

-spec handle_delete_older(string(), integer() | tuple()) -> ok | {error, any()}.
handle_delete_older(Dir, Seconds) 
  when is_list(Dir), Dir =/= "", is_integer(Seconds) ->
    Precipice = time_minus(now_ut(), Seconds),
    {ok, Files} = list_dir(Dir),
    FileInfo = [{FileName, read_file_info(Dir, FileName, [{time, universal}])} 
		|| FileName <- Files],
    lists:foreach(fun({File, #file_info{mtime = MTime}}) 
			when MTime < Precipice ->
			  delete(Dir, File);
		     (_F) ->
			  ok
		  end, [{FileN, FileI} || {FileN, {ok, FileI}} <- FileInfo]);

handle_delete_older(Dir, {seconds, S}) ->
    handle_delete_older(Dir, S);

handle_delete_older(Dir, {minutes, M}) ->
    handle_delete_older(Dir, timer:minutes(M) div 1000);

handle_delete_older(Dir, {hours, H}) ->
    handle_delete_older(Dir, timer:hours(H) div 1000).


%%% ----------------------------------------------------------
%%% @doc Delete the oldest file in the given Dir. Dir has to be a registered
%%%      sftp directory.
%%% @end
%%% ----------------------------------------------------------
-spec delete_oldest(Dir::string()) -> ok.
delete_oldest(Dir) ->
    gen_server:cast(?SERVER, {handle_delete_oldest, Dir}).

-spec handle_delete_oldest(Dir::string()) -> ok.
handle_delete_oldest(Dir) ->
    {ok, AbsDir} = reg_absname(Dir),
    case get_oldest_file(AbsDir) of
	[] ->
	    ok;
	File ->
	    AbsName = filename:absname(File, AbsDir),
	    ok = file:delete(AbsName)
    end.


%%% ----------------------------------------------------------
%%% @doc 
%%% Get the name of the oldest file in the given Dir. Dir has to be a 
%%% registered sftp directory.
%%% @end
%%% ----------------------------------------------------------
-spec get_oldest(Dir::string()) -> FileName::string().
get_oldest(Dir) ->
    {ok, AbsDir} = reg_absname(Dir),
    get_oldest_file(AbsDir).


%%% ----------------------------------------------------------
%%% @doc The contents of the specified file is returned.
%%% See the OTP file:read_file/1 function.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec read_file(string(), string()) -> {ok, binary()}|{error, any()}.

read_file(Dir, Filename) -> 
    file:read_file(sftpObjectName(Dir, Filename)).


%%% ----------------------------------------------------------
%%% @doc Information about the specified file is returned.
%%% Opts is a list of file_info_option() as specified for the OTP
%%% file:read_file_info/2 function.
%%%
%%% If the Filename is specified as 'none' then info about the
%%% directory is provided.
%%% @end
%%% ----------------------------------------------------------
-spec read_file_info(string(), string()|none, [any()]) -> {ok|error, any()}.

read_file_info(Dir, none, Opts) ->
    file:read_file_info(sftp_absname(Dir), Opts);
    
read_file_info(Dir, Filename, Opts) -> 
    file:read_file_info(sftpObjectName(Dir, Filename), Opts).


%%% ----------------------------------------------------------
%%% @doc Write information about the specified file.
%%% See the OTP file:write_file_info/2 function.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec write_file_info(string(), string(), tuple(), [any()]) -> 
			     ok|{error, any()}.

write_file_info(Dir, Filename, FileInfo, Opts) -> 
    file:write_file_info(sftpObjectName(Dir, Filename), FileInfo, Opts).


%%% ----------------------------------------------------------
%%% @doc Returns the absolute filesystem path for a given
%%% sftp directory. The directory does not need to exist. Unlike
%%% filename:absname/1 this function does not depend on the
%%% current directory. 
%%% @end
%%% ----------------------------------------------------------
-spec sftp_absname(string()) -> string().

sftp_absname(Dir) ->
    filename:absname(clean_path(Dir), sftp_root()).


-spec sftp_absname(FieName::string(), Dir::string()) -> Path::string().

sftp_absname(FileName, Dir) ->
    filename:absname(FileName, sftp_absname(Dir)).

%% @private
%% Kept for backwards compatibilty
absname(Dir) ->
    sftpObjectName([clean_path(Dir)]).
		

%%% ----------------------------------------------------------
%%% @doc Returns the absolute filesystem path for a registered sftp
%%% directory. The directory has to be a registered sftp dir. 
%%% Unlike filename:absname/1 this function does not depend on the
%%% current directory. 
%%% @end
%%% ----------------------------------------------------------
-spec reg_absname(Dir::string()) -> 
			 {ok, AbsName::string()} | {error, Reason::term()}.

reg_absname(Dir) ->
    CleanDir = clean_dir(Dir),
    case get_clean_reg_dir_root(CleanDir) of
	RootDir when is_list(RootDir) ->
	    {ok, filename:absname(CleanDir, RootDir)};
	_Undef ->
	    {error, not_registered_sftp_dir}
    end.


-spec reg_absname(FileName::string(), Dir::string()) -> 
			 {ok, AbsName::string()} | {error, Reason::term()}.

reg_absname(FileName, Dir) ->
    case reg_absname(Dir) of
	{ok, AbsDir} ->
	    {ok, filename:absname(FileName, AbsDir)};
	Error ->
	    Error
    end.


%%% ----------------------------------------------------------
%%% @doc 
%%% Get root directory for a registered sftp directory.
%%% @end
%%% ----------------------------------------------------------
-spec get_reg_dir_root(Dir::string()) -> Root::string() | undefined.

get_reg_dir_root(Dir) ->
    CleanDir = clean_dir(Dir),
    get_clean_reg_dir_root(CleanDir).


%%% ----------------------------------------------------------
%%% @doc Returns an absolute directory of file name for the
%%% given list of name components. The returned name is prefixed
%%% with the SFTP directory. 
%%% @end
%%% ----------------------------------------------------------
sftpObjectName(Names) ->
    filename:join([sftp_root()|Names]).


sftpObjectName(Dir, FileName) ->
    filename:join(sftp_absname(Dir), FileName).


sftp_root() ->
    filename:join(sysEnv:vnf_dir(), sysSshSftpd:sftp_dir()).


%% ram_files_root() ->
%%     filename:join([sysEnv:tmp_dir(), ?RAMFILES_DIR]).
    

%%===========================================================================
%% gen_server start functions
%%===========================================================================

start() ->
    start([]).


start(Opts) ->
    ok = cec:register(<<"FI">>, sysFi),
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).


start_link() ->
    start_link([]).


start_link(Opts) ->
    ok = cec:register(<<"FI">>, sysFi),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).


stop() ->
    ok = cec:unregister(<<"FI">>),
    gen_server:cast(?SERVER, stop).


init(_Opts) ->
    Dirs = get_sftp_reg_dirs(),
    F = fun({Dir, {Root, Type, Size, Mod}}, Acc) ->
		{ok, NewAcc} = 
		    handle_reg_sftp_dir(Root, Dir, Type, Size, Mod, Acc),
		NewAcc
	end,
    RegDirs = lists:foldl(F, [], Dirs),
    S = #state{sftpRegDirs = RegDirs},
    {ok, S}.


code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


terminate(_Reason, S) ->
    [gen_tcp:close(Socket) || Socket <- S#state.sockets],
    ok.


%%===========================================================================
%% gen_server message functions
%%===========================================================================
%%---------------------------------------------------------------------
%% call
%%---------------------------------------------------------------------
handle_call({get_sftp_reg_dir, Dir}, _From, #state{sftpRegDirs=Dirs}=S) ->
    case  proplists:get_value(Dir, Dirs) of
	{Mod, Type, _Root} ->
	    {reply, {Mod, Type}, S};
	DirData ->
	    {reply, DirData, S}
	end;

handle_call({register_sftp_dir, {Root, Dir, Type, Size, Mod}}, _From,
	    #state{sftpRegDirs=DirData}=S) ->
    try
	RealRoot = get_root_dir(Root, Type),
	{ok, NewDirData} = 
	    handle_reg_sftp_dir(RealRoot, Dir, Type, Size, Mod, DirData),
	NewState = S#state{sftpRegDirs=NewDirData}, 
	{reply, ok, NewState}
    catch 
	_:Error ->
	    sysInitI:warning_msg("~p: Failed to register sftp dir.~n~p",
				     [?MODULE, Error]),
	    {reply, {error, Error}, S}
    end;
	
handle_call({unregister_sftp_dir, Dir}, _From,
	    #state{sftpRegDirs=DirData}=S) ->
    NewDirData = handle_unreg_sftp_dir(Dir, DirData),
    NewState = S#state{sftpRegDirs=NewDirData}, 
    {reply, ok, NewState};

handle_call({cec_setup, Socket}, _From, #state{sockets = Sockets} = S) ->
    {reply, self(), S#state{sockets = [Socket | Sockets]}};

handle_call(Command, _From, S) ->
    {reply, Command, S}.

%%---------------------------------------------------------------------
%% cast
%%---------------------------------------------------------------------
%% This function should be removed but is needed for now by the test suite.
%% It is not used in live environment.
handle_cast({set_parameters, 
	     Persistence, 
	     SpaceLimit, 
	     StaleLimit, 
	     _TimeoutMillis}, S) ->
    if
	Persistence =:= ?VOLATILE -> 
	    NewState = 
		S#state{volSpaceLimit=
			    ?DEFAULT(SpaceLimit, ?VOLATILE_SPACE_LIMIT), 
			volStaleLimit=
			    ?DEFAULT(StaleLimit, ?VOLATILE_STALE_LIMIT)
			%% timeout=?DEFAULT(TimeoutMillis, ?TIMEOUT)
		       },
	    
	    % when stable, consider commenting this log entry, TODO
	    sysInitI:info_msg(
	      "parameters set: Persistence: ~w, "
	      "SpaceLimit: ~w, "
	      "StaleLimit: ~w~n ",
	      %% "TimeoutMillis: ~w~n",		  
	      [Persistence,
	       NewState#state.volSpaceLimit,
	       NewState#state.volStaleLimit]),   
	       %% NewState#state.timeout]),
	    
	    {noreply, NewState};
	true ->
	    {noreply, S}
    end;

handle_cast({handle_delete_older, Dir, Seconds}, S) ->
    try handle_delete_older(Dir, Seconds)
    catch
        _:Error ->
            sysInitI:error_msg("~p: Failed to delete oldest ~n~p",
                                 [?MODULE, Error])
    end,
    {noreply, S};

handle_cast({handle_delete_oldest, Dir}, S) ->
    try handle_delete_oldest(Dir)
    catch
        _:Error ->
            sysInitI:error_msg("~p: Failed to delete oldest ~n~p",
                                 [?MODULE, Error])
    end,
    {noreply, S};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_Msg, S) ->
    {noreply, S}.

%%---------------------------------------------------------------------
%% info
%%---------------------------------------------------------------------
handle_info({tcp, Socket, <<?IMPORT_FILE:4/native-unsigned-integer-unit:8,
			    PasswordSize:4/native-unsigned-integer-unit:8,
			    UriSize:4/native-unsigned-integer-unit:8,
			    Password:PasswordSize/binary,
			    Uri:UriSize/binary,
			    FileNameOnNode/binary>>}, S) ->
    spawn(?MODULE, handle_file, [Socket, Password, Uri, FileNameOnNode, import]), 
    {noreply, S};

handle_info({tcp, Socket, <<?IMPORT_FILE_2:4/native-unsigned-integer-unit:8,
			    PasswordSize:4/native-unsigned-integer-unit:8,
			    UriSize:4/native-unsigned-integer-unit:8,
			    DepryptPassword:PasswordSize/binary,
			    Uri:UriSize/binary,
			    FileNameOnNode/binary>>}, S) ->
    case comsaI:decrypt_password(to_string(DepryptPassword)) of
	{error, Reason} ->
	    sysInitI:error_msg("~w: "++"Failed to decrypt password: ~p~n", 
			       [?MODULE, Reason]),
	    gen_tcp:send(Socket, <<?FI_FAILED_TO_DECRYPT_PASSWORD:4/native-integer-unit:8>>),
	    inet:setopts(Socket, [{active, once}, {nodelay, true}]);
	Password ->
	    spawn(?MODULE, handle_file, [Socket, Password, Uri, FileNameOnNode, import]) 
    end,
    {noreply, S};

handle_info({tcp, Socket, <<?IMPORT_FILE_3:4/native-unsigned-integer-unit:8,
			    PasswordSize:4/native-unsigned-integer-unit:8,
			    UriSize:4/native-unsigned-integer-unit:8,
			    FileSize:4/native-unsigned-integer-unit:8,
			    Password:PasswordSize/binary,
			    Uri:UriSize/binary,
			    FileNameOnNode:FileSize/binary,
			    MaxSize:8/native-unsigned-integer-unit:8,
			    EncrptedPwd:1/native-unsigned-integer-unit:8>>}, S) ->
    case EncrptedPwd of
	0 ->
	    spawn(?MODULE, import_file, [Socket, Password, Uri, FileNameOnNode, MaxSize]); 
	_ -> 
	    case comsaI:decrypt_password(to_string(Password)) of
		{error, Reason} ->
		    sysInitI:error_msg("~w: "++"Failed to decrypt password: ~p~n", 
				       [?MODULE, Reason]),
		    gen_tcp:send(Socket,<<?FI_FAILED_TO_DECRYPT_PASSWORD:4/native-integer-unit:8>>),
		    inet:setopts(Socket, [{active, once}, {nodelay, true}]);
		DecryptPassword ->
		    spawn(?MODULE, import_file, [Socket, DecryptPassword, Uri, FileNameOnNode, 
						 MaxSize]) 
	    end
    end,
    {noreply, S};

handle_info({tcp, Socket, <<?EXPORT_FILE:4/native-unsigned-integer-unit:8,
			    PasswordSize:4/native-unsigned-integer-unit:8,
			    UriSize:4/native-unsigned-integer-unit:8,
			    Password:PasswordSize/binary,
			    Uri:UriSize/binary,
			    FullPath/binary>>}, S) ->
    spawn(?MODULE, handle_file, [Socket, Password, Uri, FullPath, export]), 
    {noreply, S};

handle_info({tcp, Socket, <<?EXPORT_FILE_2:4/native-unsigned-integer-unit:8,
			    PasswordSize:4/native-unsigned-integer-unit:8,
			    UriSize:4/native-unsigned-integer-unit:8,
			    DepryptPassword:PasswordSize/binary,
			    Uri:UriSize/binary,
			    FullPath/binary>>}, S) ->
    case comsaI:decrypt_password(to_string(DepryptPassword)) of
	{error, Reason} ->
	    sysInitI:error_msg("~w: "++"Failed to decrypt password: ~p~n", 
			       [?MODULE, Reason]),
	    gen_tcp:send(Socket, <<?FI_FAILED_TO_DECRYPT_PASSWORD:4/native-integer-unit:8>>),
	    inet:setopts(Socket, [{active, once}, {nodelay, true}]);
	Password ->
	    spawn(?MODULE, handle_file, [Socket, Password, Uri, FullPath, export])
    end,
    {noreply, S};

handle_info({tcp, Socket, <<?SPACE_AVAILABLE:32/native-unsigned-integer,
			    Persistence:32/native-unsigned-integer,
	                    Dir/binary>>}, S) ->
    % calculate available file space in bytes; a return value of 0 indicates
    % that no more files may be written; also returns a root string

    DirS = binary_to_list(Dir),

    {Avail, RootS} = 
	if
	    Persistence =:= ?VOLATILE ->
		RamFilesAbsDir = get_ram_dir_root(DirS),
		SpaceUsed = getVolatileFilesSize(RamFilesAbsDir),
		{max(0, S#state.volSpaceLimit - SpaceUsed), RamFilesAbsDir};
	    Persistence =:= ?PERSISTENT ->
		{0, "/nosuch"}
	end,
    RootSize = length(RootS),
    RootB = list_to_binary(RootS),

    DirValid = check_ram_dir_valid(RootS, DirS),
    if
    	DirValid =/= ?FI_OK ->
    	    sysInitI:warning_report(
    	      [{"DirValid", DirValid},
    	       {"Dir", Dir},
    	       {"Avail", Avail},
    	       {"Root", RootB}]);
    	true ->
    	    ok
    end,

    Reply = <<Avail:32/native-unsigned-integer,
              DirValid:32/native-unsigned-integer,
	      RootSize:32/native-unsigned-integer,
	      RootB/binary>>,
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]),
    {noreply, S};

handle_info({tcp_closed, Socket}, #state{sockets = Sockets} = S) ->
    {noreply, S#state{sockets = lists:delete(Socket, Sockets)}};

handle_info(_Info, S) ->
    {noreply, S}.



handle_file(Socket, Password, Uri, FileOrPath, Dir) ->
    Reply = case ftpI:parse_uri(to_string(Uri)) of
		{ok, {Proto, User, Host, Port, File, _}} ->
		    start_channel(Proto, Host, Port, User, Password, File, 
				  to_string(FileOrPath), Dir);
		{error, UriReason} ->
		    sysInitI:error_msg("~w: "++"Failed to parse URI: ~p~n", 
					   [?MODULE, UriReason]),
		    <<?FI_INVALID_URI:4/native-integer-unit:8>>
	    end,
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]).


import_file(Socket, Password, Uri, FileNameOnNode, MaxSize) ->
    Reply = case ftpI:parse_uri(to_string(Uri)) of
		{ok, {Proto, User, Host, Port, File, _}} ->
		    start_channel_2(Proto, Host, Port, User, Password, File, 
				    to_string(FileNameOnNode),
				    MaxSize);
		{error, UriReason} ->
		    sysInitI:error_msg("~w: "++"Failed to parse URI: ~p~n", 
				       [?MODULE, UriReason]),
		    <<?FI_INVALID_URI:4/native-integer-unit:8>>
	    end,
    gen_tcp:send(Socket, Reply),
    inet:setopts(Socket, [{active, once}, {nodelay, true}]).

%%% ----------------------------------------------------------
%%% #           start_channel(Proto, Host, Port, User, Password, File)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Starting the channel towards the SFTP server
%%% ----------------------------------------------------------
start_channel(Proto, Host, Port, User, Password, File, FullPathOrFile, DownOrUpload) ->
    %% Create the fi dir
    FiDir = filename:join(sysEnv:rcs_dir(), "fi"),

    case ftpI:start_channel(Proto, Host,  Port, User, to_string(Password)) of
	{ok, Pid, ConnectionRef} ->
	    try
		case DownOrUpload of
		    import ->
			{ok, FileData} = ftpI:read_file(Proto, Pid, File),	
			FileOnNode =  
			    case lists:last(FullPathOrFile) of
				$/ ->
				    %% No file name is included in the fullpath, 
				    %% keep the name from URI
				    %% Join fi dir with the dir from the application
				    filename:join(filename:join(FiDir, FullPathOrFile), 
						  filename:basename(File));
				_ ->
				    %% Join fi dir with the dir from the application
				    filename:join(FiDir, FullPathOrFile)
			    end,
			ok = filelib:ensure_dir(FileOnNode),
			file:write_file(FileOnNode, FileData);
		    export ->
			{ok, FileData} = file:read_file(FullPathOrFile),
			case lists:last(File) of
			    $/ ->
				%% No file name is included in the uri, keep the name from node
				FileOnServer = filename:join(File, filename:basename(FullPathOrFile)),
				ok = ftpI:write_file(Proto, Pid, FileOnServer, FileData);
			    _Else ->
				ok = ftpI:write_file(Proto, Pid, File, FileData)
			end
		end
	    of
		ok ->
		    <<?FI_OK:4/native-integer-unit:8>>		   		   
	    catch
		error:{badmatch, {error, Reason}} ->
		    sysInitI:error_msg("~w: "++"Failed to ~p file, reason: ~p~n", 
					   [?MODULE, DownOrUpload, Reason]),
		    <<?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE:4/native-integer-unit:8>>
	    after
		ok = ftpI:stop_channel(Proto, Pid),
		ok = ssh:close(ConnectionRef)
	    end;
	{error, Reason} ->
	    %% Failed to establish a connection to remote server
	    sysInitI:error_msg("~w: "++"Failed to connect to SFTP server: ~p~n", 
				   [?MODULE, Reason]),
	    <<?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER:4/native-integer-unit:8>>
    end.



%%% ----------------------------------------------------------
%%% #           start_channel_2(Proto, Host, Port, User, Password, File, MaxSize)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Starting the channel towards the SFTP server with limitation of file size
%%% ----------------------------------------------------------

start_channel_2(Proto, Host, Port, User, Password, File, FullPathOrFile, MaxSize) ->
    %% Create the fi dir
    FiDir = filename:join(sysEnv:rcs_dir(), "fi"),
    %% Get free disk size in bytes
    FreeDiskSize = sysServer:get_free_disk() * 1000,

    case ftpI:start_channel(Proto, Host,  Port, User, to_string(Password)) of
      {ok, Pid, ConnectionRef} ->
      case get_file_size(Proto, Pid, File) of
        {ok, FileSize}->
	  if
	    FileSize > MaxSize ->
              sysInitI:error_msg("~w: "++"Failed to import file, reason: ~s~p~n", 
                                [?MODULE,  "File size exceeds the limitation : ", MaxSize]),
              <<?FI_FILE_SIZE_EXCEED_LIMITATION:4/native-integer-unit:8>>;
            FileSize > FreeDiskSize ->
              sysInitI:error_msg("~w: "++"Failed to import file, reason: ~s~p~n", 
                                [?MODULE,  "File size exceeds the free disk size : ", FreeDiskSize]),
              <<?FI_NO_ENOUGH_DISK_SPACE:4/native-integer-unit:8>>; 
            true->
		try
				{ok, FileData} = ftpI:read_file(Proto, Pid, File),	
				FileOnNode =  
				case lists:last(FullPathOrFile) of
					$/ ->
					%% No file name is included in the fullpath, 
					%% keep the name from URI
					%% Join fi dir with the dir from the application
					filename:join(filename:join(FiDir, FullPathOrFile), 
							filename:basename(File));
					_ ->
					%% Join fi dir with the dir from the application
					filename:join(FiDir, FullPathOrFile)
				end,
				ok = filelib:ensure_dir(FileOnNode),
				file:write_file(FileOnNode, FileData)
		of
			ok ->
			<<?FI_OK:4/native-integer-unit:8>>		   		   
		catch
			error:{badmatch, {error, Reason}} ->
			sysInitI:error_msg("~w: "++"Failed to ~p file, reason: ~p~n", 
						[?MODULE,  Reason]),
			<<?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE:4/native-integer-unit:8>>
		after
			ok = ftpI:stop_channel(Proto, Pid),
			ok = ssh:close(ConnectionRef)
		end
	end;
        {error, Reason} ->
          %% Failed to get file size on remote sftp server
          sysInitI:error_msg("~w: "++"Failed to ~p file, reason: ~p~n", 
                        [?MODULE,  Reason]),
          <<?FI_FAILED_TO_IMPORT_OR_EXPORT_FILE:4/native-integer-unit:8>>
        end;
      {error, Reason} ->
        %% Failed to establish a connection to remote server
        sysInitI:error_msg("~w: "++"Failed to connect to SFTP server: ~p~n", 
                          [?MODULE, Reason]),
        <<?FI_FAILED_TO_CONNECT_TO_SFTP_SERVER:4/native-integer-unit:8>>
    end.



%%% ----------------------------------------------------------
%%%    # size/2
%%% @doc 
%%% ----------------------------------------------------------
-spec get_file_size(ftpes | sftp,
           Pid :: pid(),
           File :: string()) ->
          {ok, integer()} | {error,  any()}.

get_file_size(sftp, Pid, File) ->
    case ssh_sftp:read_file_info(Pid, File) of
       {ok, FileInfo}->
            {ok, FileInfo#file_info.size};
       {error, Reason}->
            {error, Reason}
    end;

get_file_size(ftpes, Pid, File) ->
    case ftpesI:size(Pid, File) of
       {error, Reason}->
            {error, Reason};
       FileSize when is_integer(FileSize)->
            {ok, FileSize};
       Other ->
            {error, Other}
    end.

%%% ----------------------------------------------------------
%%% @doc Computes the number of bytes in files under the given
%%% directory. The overhead caused by directories is not included
%%% in the returned result.
%%% @end
%%% ----------------------------------------------------------

-spec getVolatileFilesSize(string()) -> non_neg_integer().

getVolatileFilesSize(Dir) ->
    filelib:fold_files(
      Dir, 
      "..*", 
      true, 
      fun(File, Acc) -> filelib:file_size(File) + Acc end, 
      0).


%%% ----------------------------------------------------------
%%% @doc 
%%% Check if the directory is a registered sftp RAM directory.
%%% @end
%%% ----------------------------------------------------------
check_ram_dir_valid("", Dir) ->
    sysInitI:error_msg("~p: Application tried to write to invalid"
		       " directory ~s~n", [?MODULE, Dir]),
    ?FI_SFTP_SYMLINK_INVALID_NAME;

check_ram_dir_valid(_Root, _Dir) ->
    ?FI_OK.

%%% ----------------------------------------------------------
%%% @doc 
%%% Get root directory for a registered sftp RAM directory.
%%% @end
%%% ----------------------------------------------------------
get_ram_dir_root(Dir) ->
    RegDir = clean_dir(Dir),
    case mnesia:dirty_read({sysSftpRegDirs, RegDir}) of
	[#sysSftpRegDirs{root_dir = Root, type = ram}] ->
	    Root;
	_ -> 
	    ""
    end.

%%% ----------------------------------------------------------
%%% @doc 
%%% Get root directory for a registered sftp directory.
%%% @end
%%% ----------------------------------------------------------
get_clean_reg_dir_root(CleanDir) ->
    case mnesia:dirty_read({sysSftpRegDirs, CleanDir}) of
	[#sysSftpRegDirs{root_dir = Root, type = ram}] ->
	    Root;
	[#sysSftpRegDirs{root_dir = undefined, type = disc}] ->
	    sftp_root();
	[#sysSftpRegDirs{root_dir = Root, type = disc}] ->
	    Root;
	_ -> 
	    undefined
    end.


get_clean_reg_dir_root_and_size(CleanDir) ->
    case mnesia:dirty_read({sysSftpRegDirs, CleanDir}) of
	[#sysSftpRegDirs{root_dir = Root, type = ram}] ->
	    {Root, undefined};
	[#sysSftpRegDirs{root_dir = undefined, type = disc, size = Size}] ->
	    {sftp_root(), Size};
	[#sysSftpRegDirs{root_dir = Root, type = disc, size = Size}] ->
	    {Root, Size};
	_ -> 
	    {undefined, undefined}
    end.

%%% ----------------------------------------------------------
%%% @doc 
%%% Get root directory for a sftp directory to register.
%%% @end
%%% ----------------------------------------------------------
get_root_dir(undefined, disc) ->
    sftp_root();

get_root_dir(Dir, _Type) ->
    Dir.


%%% ----------------------------------------------------------
%%% @doc Register and create sftp directory
%%% @end
%%% ----------------------------------------------------------
handle_reg_sftp_dir(Root, Dir, Type, Size, Mod, DirData) ->
    RelDir = filename:join("/", Dir),
    case proplists:get_value(RelDir, DirData) of
	undefined ->
	    RegDirData = [{sftp_absname(Dir), {Type, Mod}},
			  {RelDir, {Type, Mod}}],
	    NewDirData = lists:foldl(fun({K, _V} = KV, Acc) ->
					     lists:keystore(K, 1, Acc, KV)
				     end, DirData, RegDirData), 
	    ok = handle_reg_sftp_dir_create(Type, Root, Dir),
	    put_sftp_reg_dir(Root, Dir, Type, Size, Mod),
	    {ok, NewDirData};
	{Type, Mod} ->
	    {ok, DirData};
	Data ->
	    sysInitI:error_msg("~p: Failed to register SFTP dir ~s due"
				   " to name conflict:~n~p ~p~n",
				   [?MODULE, Dir, Data, {Type, Mod}]),
	    {error, name_conflict}
    end.


handle_reg_sftp_dir_create(disc, _Undef, Dir) ->
    AbsDir = sftp_absname(Dir),
    ok = filelib:ensure_dir(filename:join(AbsDir, "dum")),
    sysInitI:info_msg("~p: Register SFTP dir ~s~nCreate dir ~s~n",
			  [?MODULE, Dir, AbsDir]),
    ok;

handle_reg_sftp_dir_create(ram, Root, Dir) ->
    LinkAbsPath = sftp_absname(Dir), 
    LinkTarget = filename:join(Root, Dir),
    sysInitI:info_msg("~p: Register SFTP RAM dir ~s~n",
			  [?MODULE, Dir]),
    ok = filelib:ensure_dir(LinkAbsPath),
    case {file:list_dir(LinkTarget), file:read_link_all(LinkAbsPath)} of
	{{ok, _}, {ok, LinkTarget}} ->
	    ok;
	{{ok, _}, {error, enoent}} ->
	    sysInitI:info_msg("~p: Create symlink ~s~n",
				  [?MODULE, LinkAbsPath]),
	    ok = file:make_symlink(LinkTarget, LinkAbsPath);
	{{error, enoent}, {error, enoent}} ->
	    create_sftp_ram_dir(LinkTarget, LinkAbsPath);
	_Error ->
	    sysInitI:info_msg("~p: Deleting existing directory/symlink ~s~n",
			      [?MODULE, LinkAbsPath]),
	    create_sftp_ram_dir(LinkTarget, LinkAbsPath)
    end.


create_sftp_ram_dir(LinkTarget, LinkAbsPath) ->
    file:del_dir(LinkAbsPath),
    file:delete(LinkAbsPath),
    sysInitI:info_msg("~p: Create dir ~s~n"
			  "Create symlink ~s~n",
			  [?MODULE, LinkTarget, LinkAbsPath]),
    ok = filelib:ensure_dir(filename:join(LinkTarget, "dum")),
    {ok, FileInfo} = file:read_file_info(LinkTarget),
    Mode = FileInfo#file_info.mode bor 8#22,
    ok = file:write_file_info(LinkTarget, FileInfo#file_info{mode = Mode}),
    ok = file:make_symlink(LinkTarget, LinkAbsPath).

%%% ----------------------------------------------------------
%%% @doc Store a registered sftp dir in db
%%% @end
%%% ----------------------------------------------------------
put_sftp_reg_dir(Root, Dir, Type, Size, Mod) ->
    Rec = #sysSftpRegDirs{root_dir = Root, 
			  dir = Dir, 
			  type = Type,
			  size = Size, 
			  mod = Mod},

    case mnesia:transaction(fun() ->
				    put_sftp_reg_dir(Dir, Rec)
			    end) of
	{atomic, Val} ->
	    Val;
	Error ->
	    Error
    end.

put_sftp_reg_dir(Dir, Rec) ->
    case mnesia:read(sysSftpRegDirs, Dir) of
	[] ->
	    mnesia:write(Rec);
	[Rec] ->
	    ok;
	[#sysSftpRegDirs{} = RegDir] ->
	    sysInitI:error_msg("~p: Failed to register SFTP dir ~s due"
			       " to name conflict:~nOld = ~p~nNew = ~p~n",
			       [?MODULE, Dir, RegDir, Rec]),
	    {error, name_conflict}
    end.
    

%%% ----------------------------------------------------------
%%% @doc Uregister sftp dirs from db. Only for testing.
%%% @end
%%% ----------------------------------------------------------
handle_unreg_sftp_dir(Dir, DirData) ->
    CleanDir = clean_dir(Dir),
    RelDir = filename:join("/", CleanDir),
    case proplists:get_value(RelDir, DirData) of
	undefined ->
	    DirData;
	_Val ->
	    {ok, AbsDir} = reg_absname(CleanDir),
	    mnesia:dirty_delete(sysSftpRegDirs, CleanDir),
	    NewDirData = lists:keydelete(RelDir, 1, DirData),
	    lists:keydelete(AbsDir, 1, NewDirData)
    end.


%%% ----------------------------------------------------------
%%% @doc Fetch the registered sftp dirs from db
%%% @end
%%% ----------------------------------------------------------
get_sftp_reg_dirs() ->
    [{Rec#sysSftpRegDirs.dir,
      {Rec#sysSftpRegDirs.root_dir,
       Rec#sysSftpRegDirs.type, 
       Rec#sysSftpRegDirs.size,
       Rec#sysSftpRegDirs.mod}} || 
	Rec <- ets:tab2list(sysSftpRegDirs)].
    
%%% ----------------------------------------------------------
%%% @doc Converts a binary or string to a well-formed
%%% Erlang string. A terminating NULL character, if
%%% present in the given argument, is dropped.
%%% @end
%%% ----------------------------------------------------------
to_string(Text) when is_binary(Text) ->
    to_string(binary_to_list(Text));

to_string(Text) when is_list(Text) ->
    lists:takewhile(fun(B) -> B =/= 0 end, Text).


clean_path(Dir) ->
    case filename:split(Dir) of
	[H | Rest] when H =:= "/"; H =:= "." ->
	    filename:join(Rest);
	Parts ->
	    filename:join(Parts)	    
    end.


clean_dir(Dir) ->
    case filename:split(Dir) of
	[H, CleanDir] when H =:= "/"; H =:= "." ->
	    CleanDir;
	[Dir] ->
	    Dir;
	_ ->
	    throw("Directory is a pathname and not a basename")
    end.


now_ut() ->
    calendar:now_to_universal_time(os:timestamp()).
    

time_minus(Time, Seconds) ->
    TSecs = calendar:datetime_to_gregorian_seconds(Time),
    calendar:gregorian_seconds_to_datetime(TSecs - Seconds).


check_total_file_size(Dir, Filename, Bytes) ->
    CleanDir = clean_dir(Dir),
    case get_clean_reg_dir_root_and_size(CleanDir) of
	{RootDir, MaxSize} when is_list(RootDir), is_integer(MaxSize) ->
	    AbsDir = filename:absname(CleanDir, RootDir),
	    check_total_file_size(AbsDir, Filename, Bytes, MaxSize);
	{RootDir, undefined} when is_list(RootDir) ->
	    {ok, filename:absname(CleanDir, RootDir)};
	_Undef ->
	    {error, not_registered_sftp_dir}
    end.
    

check_total_file_size(AbsDir, Filename, Bytes, MaxSize) ->
    TotSize = get_total_file_size(AbsDir) + size(Bytes),
    if
	TotSize =< MaxSize * 1000 ->
	    {ok, filename:absname(Filename, AbsDir)};
	true ->
	    {error, enospc}
    end.


get_total_file_size(Dir) ->
    {ok, Files} = file:list_dir(Dir),    
    lists:foldl(fun(File, AccSize) ->
			AbsName = filename:absname(File, Dir),
			AccSize + filelib:file_size(AbsName)
		end, 0, Files).


get_oldest_file(AbsDir) ->
    case file:list_dir(AbsDir) of
	{ok, Files} when Files =/= [] ->
	    get_oldest_of_files(AbsDir, Files);
	_ ->
	    []
    end.


get_oldest_of_files(AbsDir, Files) ->
    F = fun(error_reading, Acc) -> Acc;
           ({_File, Rec}, {_OldFile, OldRec} = Acc) 
          when Rec#file_info.mtime > OldRec#file_info.mtime ->
        Acc;
       (NewAcc, _) ->
        NewAcc
    end,
    {OldestFile, _} = 
    lists:foldl(F, [], [{File, get_file_info(File, AbsDir)} || 
                   File <- Files]),
    OldestFile.

get_file_info(File, AbsDir) ->
    AbsName = filename:absname(File, AbsDir),
    case file:read_file_info(AbsName) of
        {error, Reason} ->
            sysInitI:warning_msg(
              "~p: Failed to read info for ROP file ~p:~n~p",
              [?MODULE, File, Reason]),
            error_reading;
        {ok, Info} -> Info
    end.

