%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certSecStore.erl %
%%% @author eivomat
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1
%%% 
%%% @doc ==Certficate revocation list handling==
%%% This module implements the certificate revocation list handling
-module(certSecStore).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-11-23').
-author('eivomat').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% -----   ---------- -------  ------------------------------
%%% R2A/1   2013-11-14 etxasta  Created
%%% R3A/4   2015-02-08 etxtory  Added ~n in info_msg
%%% R6A/1   2016-07-20 etomist  HU99273, increased timeout
%%%                             for get_next_chain_key
%%% R6A/2   2016-09-15 etxasta  Added get_device to handle dus53  
%%% R7A/1   2016-09-30 etxasta  Added get_cred  
%%% R7A/2   2016-10-19 etxasta  Fixed read timeout  
%%% R8A/1   2016-10-28 etxasta  More for share secret
%%% R8A/2   2016-11-14 emariad  Added new read for early phace in node startup
%%% R8A/3   2016-11-18 etxasta  More for shared secret 
%%% R8A/4   2017-01-20 etxasta  Removed VC for vrcs, cloud usage
%%% R8A/5   2017-01-24 etxasta  Temporary put back VC for vrcs
%%% R9A/1   2017-04-06 etxasta  Removed VC for vrcs, cloud usage
%%% R10A/1  2017-05-08 ebabmat  HV85346 crc algorythm fixed
%%% R10A/2  2017-05-17 etxasta  Changed rcs_mode to rcs_mode_2
%%% R10A/3  2017-05-23 etomist  HV90099
%%% R11A/1  2017-08-31 ebabmat  HW12922 and HW21881
%%% R11A/2  2017-09-04 ebabmat  revert for HW12922 and HW21881
%%% R11A/3  2017-09-08 ebabmat  reimplementing fix for HW12922 and HW21881
%%% R12/1   2017-11-23 eivomat  HW42211
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%% Called from supervisor
-export([start/0]).

-export([
        read/2,
        write/3,
        delete/2,
        install_vc/0,
        put_nc_key/3,
        get_nc_key/2,
        remove_nc_key/2,
        remove_nc_dir/1,
        get_vc/0,
        get_next_chain_key/1,
        get_chain_cc/1,
        get_cred/1,
        try_call/1
    ]).

-export([get_device/0]).
-export([read_early_phase/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).


-include("RcsCertM.hrl").

%these are from rhai-sdoi.h
-define(RHAI_SDO_MBOX_NAME, "sdo_mbox").
-define(RHAI_MSG_SDO_GET_REQUEST, 16#01900502).
-define(RHAI_MSG_SDO_STORE_REQUEST, 16#01900500).
-define(RHAI_MSG_SDO_REMOVE_REQUEST, 16#01900504).
-define(RHAI_SDO_MAX_PATH_LENGHT, 1000).
-define(SDO_RC_OK, 0).
-define(SDO_RC_COULD_NOT_START, 1).
-define(SDO_RC_HW_ERROR, 2).
-define(SDO_RC_FILE_ERROR, 3).
-define(SDO_RC_FILE_EXISTS, 4).
-define(SDO_RC_FILE_DOES_NOT_EXIST, 5).
-define(SDO_RC_AUTH_FAILED, 6).

-define(POLY, 16#4C11DB7).
-define(MAGIC_LENGTH, 4).
-define(MAGIC, "vecr").
-define(NOMAGIC, "novc").
-define(PASW, "Abandon hope all ye who enter here").
%version changed 1->2 to indicate that the  blob is SDOD
%-define(VERSION, 1).
-define(VERSION, 2).
-define(STORAGESIZE, (64 * 1024)). %64K
-define(VCNAME, "Ericsson-vc-1").

-define(VCDEVICE, "/dev/mtd").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the certSecStore server process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).


%%% ----------------------------------------------------------
%%% #          read(Id, Index) ->
%%% Input: Id    - string used for identification, and space
%%%                there Index is uniqe.
%%%        Index - Index for the file name.
%%% Output: {ok, Data} | {error, Reason}
%%%          Reason - not_found
%%% Exceptions: 
%%% Description: Generic read function for storage of data
%%%              in secure storage.
%%% ----------------------------------------------------------
read(Id, Index) ->
    gen_server:call(?MODULE, {read, Id, Index}, 10000).

%%% ----------------------------------------------------------
%%% #          write(Id, Index, Data) ->
%%% Input: Id    - string used for identification, and space
%%%                there Index is uniqe.
%%%        Index - Index for the file name.
%%%        Data  - Data to store in secure storage
%%% Output: ok | {error, Reason}
%%% Exceptions: 
%%% Description: Generic write function for storage of data
%%%              in secure storage.
%%% ----------------------------------------------------------
write(Id, Index, Data) ->
    gen_server:cast(?MODULE, {write, Id, Index, Data}),
    ok.

%% write_sec_stor()->
%%     write_sec().

%%% ----------------------------------------------------------
%%% #          delete(Id, Index) ->
%%% Input: Id    - string used for identification, and space
%%%                there Index is uniqe.
%%%        Index - Index for the file name.
%%% Output: ok | {error, Reason}
%%% Exceptions: 
%%% Description: Generic delete function for storage of data
%%%              in secure storage.
%%% ----------------------------------------------------------
delete(Id, Index) ->
    gen_server:cast(?MODULE, {delete, Id, Index}),
    ok.

%%% ----------------------------------------------------------
%%% #          install_vc() ->
%%% Input: -  
%%% Output: Cert | not_found
%%% Exceptions: 
%%% Description: Fetch and set correct vc. Can be from
%%% Secure Storage or for test a inserted test VC
%%% ----------------------------------------------------------
install_vc() ->
    gen_server:call(?MODULE, install_vc).

%%% ----------------------------------------------------------
%%% #          put_nc_key(Type, Index, Data, Filename) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Write entry to Secure Storage.
%%% ----------------------------------------------------------
put_nc_key(Index, Data, Filename) ->
    gen_server:cast(?MODULE, {put_nc_key, Index, Data, Filename}),
    ok.

%%% ----------------------------------------------------------
%%% #          get_nc_key(Index, Filename) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Read entry from Secure Storage.
%%% ----------------------------------------------------------
get_nc_key(Index, Filename) ->
    try_call([{get_nc_key, Index, Filename}, 15000]). % 15000 is timeout for underlying gen_server:call
    
%%% ----------------------------------------------------------
%%% #          get_vc() ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Get vendor credential.
%%% ----------------------------------------------------------
get_vc() ->
    try_call([get_vc]).

%%% ----------------------------------------------------------
%%% #          get_cred(Index) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Get credentials for CMPv2, can be VC, OTP or
%%%              some other certificate credential.
%%% ----------------------------------------------------------
get_cred(Index) ->
    try_call([{get_cred, Index}]).


%%% ----------------------------------------------------------
%%% #          get_next_chain_key(ChainId) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Get next VC chain certificate key.
%%% ----------------------------------------------------------
get_next_chain_key(ChainId) ->
    gen_server:call(?MODULE, {get_next_chain_key, ChainId}, _Timeout = 15000).


%%% ----------------------------------------------------------
%%% #          get_chain_cc(Nth) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Get nth VC chain certificate.
%%% ----------------------------------------------------------
get_chain_cc(Nth) ->
    try_call([{get_chain_cc, Nth}, 15000]).

%%% ----------------------------------------------------------
%%% #          remove_nc_dir(Index) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Remove entry in Secure Storage.
%%% ----------------------------------------------------------
remove_nc_dir(Index) -> % Take whole directory
    gen_server:cast(?MODULE, {remove_nc_dir, Index}),
    ok.

%%% ----------------------------------------------------------
%%% #          remove_nc_key(Index, Filename) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Remove entry in Secure Storage.
%%% ----------------------------------------------------------
remove_nc_key(Index, Filename) ->
    gen_server:cast(?MODULE, {remove_nc_key, Index, Filename}),
    ok.

%%% ----------------------------------------------------------
%%% #          read_early_phase(Index, Filename) ->
%%% Input: Id - string used for identification, and space
%%%                there Index is uniqe.
%%%        Index - Index for the file name.
%%% Output: 
%%% Exceptions: 
%%% Description: Read from disk in an early phase during rcs startup when 
%%%              certSecStore gen_server has not been initiated yet. 
%%%              Function used by certCrypto.
%%% ----------------------------------------------------------
read_early_phase(Id, Index) ->
    Path = certLib:ex_dir(Id),
    File = filename:join(Path, Index),
    case read_vc_state(os:find_executable("cup")) of
         vc ->
            case catch read_sec(File) of
                {ok, Bin} ->
                    {ok, Bin};
                _ ->
                    {error, not_found}
            end;
        _ -> % just for test and simulated environmnet
            case file:read_file(File) of
                {ok, Bin} ->
                    {ok, Bin};
                _ ->
                    {error, not_found}
            end
    end.

try_call(Args) ->
    case catch apply(gen_server, call, [?MODULE] ++ Args) of
        {'EXIT', {timeout, _}} -> 
            info_msg("Problem reading secure storage while trying: ~p, REASON: call timeout!~n", [Args]),
            {error, timeout};
        {'EXIT', Reason} ->
            error_msg("Problem reading secure storage while trying: ~p, REASON: ~p~n", [Args, Reason]),
            {error, Reason};
        Return ->
            Return
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(_Args) ->
    %% Copy Vendor Certificate (NOT credentials) to vendorCerdential table.
    case sysEnv:vrcs() of
        false -> % Support Vendor Credential
            case mnesia:dirty_read(vendorCredential, {"1","1","1","1","1"}) of
                [] ->
                    gen_server:cast(?MODULE, install_vc);
                _ ->
                    ok
            end;
        true -> % Cloud, do not support Vendor Credential
           ok
    end,
    {ok, up}.

handle_call({read, Id, Index}, _From, State) ->
    Result = handle_read(Id, Index), 
    {reply, Result, State};
handle_call(install_vc, _From, State) ->
    Result = handle_install_vc(),
    {reply, Result, State};
handle_call({get_nc_key, Index, Filename}, _From, State) ->
    Result = handle_get_nc_key(Index, Filename),
    {reply, Result, State};
handle_call(get_vc, _From, State) ->
    Result = handle_get_vc(),
    {reply, Result, State};
handle_call({get_cred, Index}, _From, State) ->
    Result = handle_get_cred(Index),
    {reply, Result, State};
handle_call({get_next_chain_key, ChainId}, _From, State) ->
    Result = handle_get_next_chain_key(ChainId),
    {reply, Result, State};
handle_call({get_chain_cc, Nth}, _From, State) ->
    Result = handle_get_chain_cc(Nth),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({remove_nc_dir, Index}, State) ->
    handle_remove_nc_dir(Index),
    {noreply, State};
handle_cast({remove_nc_key, Index, Filename}, State) ->
    handle_remove_nc_key(Index, Filename),
    {noreply, State};
handle_cast({put_nc_key, Index, Data, Filename}, State) ->
    handle_put_nc_key(Index, Data, Filename),
    {noreply, State};
handle_cast({delete, Id, Index}, State) ->
    handle_delete(Id, Index), 
    {noreply, State};
handle_cast({write, Id, Index, Data}, State) ->
    handle_write(Id, Index, Data),
    {noreply, State};
handle_cast(install_vc, State) ->
    handle_install_vc(),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #          handle_read(Id, Index) ->
%%% Input: Id    - string used for identification, and space
%%%                there Index is uniqe.
%%%        Index - Index for the file name.
%%% Output: {ok, Data} | {error, Reason}
%%%          Reason - not_found
%%% Exceptions: 
%%% Description: Generic read function for storage of data
%%%              in secure storage.
%%% ----------------------------------------------------------
handle_read(Id, Index) ->
    Path = certLib:ex_dir(Id),
    File = filename:join(Path, Index),
    case secure_node() of
        true ->
            case catch read_sec(File) of
                {ok, Bin} ->
                    {ok, Bin};
                _ ->
                    {error, not_found}
            end;
        false -> % just for test and simulated environmnet
            case file:read_file(File) of
                {ok, Bin} ->
                    {ok, Bin};
                _ ->
                    {error, not_found}
            end
    end.



%%% ----------------------------------------------------------
%%% #          handle_write(Id, Index, Data) ->
%%% Input: Id    - string used for identification, and space
%%%                there Index is uniqe.
%%%        Index - Index for the file name.
%%%        Data  - Data to store in secure storage
%%% Output: ok | {error, Reason}
%%% Exceptions: 
%%% Description: Generic write function for storage of data
%%%              in secure storage.
%%% ----------------------------------------------------------
handle_write(Id, Index, Data) ->
    Path = certLib:ex_dir(Id),
    File = filename:join(Path, Index),
    ok = filelib:ensure_dir(File),
    case secure_node() of
        true ->
            case write_sec(File, Data) of
                ok ->
                    ok;
                {error, file_exist} ->
                    delete_sec(File),
                    case write_sec(File, Data) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            error_msg("Write to secure store failed, ~p~n",
                                [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    error_msg("Write to secure store failed, ~p~n", [Reason]),
                    {error, Reason}
            end;
        false ->
            certLib:cmd(["chmod a+rwx ", Path]),
            ok = file:write_file(File, Data, [sync])
    end.

%%% ----------------------------------------------------------
%%% #          handle_delete(Id, Index) ->
%%% Input: Id    - string used for identification, and space
%%%                there Index is uniqe.
%%%        Index - Index for the file name.
%%% Output: ok | {error, Reason}
%%% Exceptions: 
%%% Description: Generic delete function for storage of data
%%%              in secure storage.
%%% ----------------------------------------------------------
handle_delete(Id, Index) ->
    Path = certLib:ex_dir(Id),
    File = filename:join(Path, Index),
    case secure_node() of
        true ->
            delete_sec(File);
        false ->
            file:delete(File)
    end,
    ok.


%%% ----------------------------------------------------------
%%% #          handle_install_vc() ->
%%% Input: -  
%%% Output: Cert | not_found
%%% Exceptions: 
%%% Description: Fetch and set correct vc. Can be from
%%% Secure Storage or for test a inserted test VC
%%% ----------------------------------------------------------
handle_install_vc() ->

    case sysEnv:rcs_mode_2() of
        simulated -> % just for simulated environmnet  
            dummy_vc();
        vrcs -> % Not using Vendor Credential
            ok;
        _ ->
            case catch real_vc() of % if not exist dummy will be used, if any
                {'EXIT', Reason} ->
                    info_msg("Get real vc exploded: ~p~n", [Reason]);
                _ -> 
                    ok
            end
    end.

%%% ----------------------------------------------------------
%%% #          handle_put_nc_key(Type, Index, Data, Filename) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Write entry to Secure Storage.
%%% ----------------------------------------------------------
handle_put_nc_key(Index, Data, Filename) ->
    Path = certLib:nc_dir(Index),
    File = filename:join(Path, Filename),
    ok = filelib:ensure_dir(File),
    case secure_node() of
        true ->
            case write_sec(File, Data) of
                ok ->
                    ok;
                {error, file_exist} ->
                    delete_sec(File),
                    case write_sec(File, Data) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            error_msg("Write to secure store failed, ~p~n",
                                [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    error_msg("Write to secure store failed, ~p~n", [Reason]),
                    {error, Reason}
            end;
        false ->
            certLib:cmd(["chmod a+rwx ", Path]),
            ok = file:write_file(File, Data, [sync])
    end.

%%% ----------------------------------------------------------
%%% #          handle_get_nc_key(Index, Filename) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Read entry from Secure Storage.
%%% ----------------------------------------------------------
handle_get_nc_key(Index, Filename) ->
    Path   = certLib:nc_dir(Index),
    File   = filename:join(Path, Filename),
    Secure = secure_node(),
    case do_handle_get_nc_key(Secure, File) of
        {ok, Bin} ->
            {ok, Bin};
        {error, Reason} ->
            case do_handle_get_nc_key(Secure, File) of
                {ok, Bin} ->
                    {ok, Bin};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

do_handle_get_nc_key(true, File) ->
    % On secure board
    case read_sec(File) of
        {ok, Bin} ->
            {ok, Bin};
        {error, Reason} ->
            info_msg("Get file from Secure Storage failed, ~p~n", [Reason]),
            {error, Reason}
    end;
do_handle_get_nc_key(false, File) ->
    % Just for test on unsecure board  and simulated environmnet
    file:read_file(File).

%%% ----------------------------------------------------------
%%% #          handle_get_vc() ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Get Vendor Credential.
%%% ----------------------------------------------------------
handle_get_vc() ->
    %% If real vc not exist dummy vc will be used, if exist
    case sysEnv:rcs_mode_2() of
        simulated -> %% just for simulated environmnet  
            read_dummy_vc();
        vrcs ->
            {error, "No VC support in virtual nodes"};
        _ ->
            case catch read_real_vc() of
                {'EXIT', Reason} ->
                    info_msg("Get real vc exploded: ~p~n", [Reason]);
                Res -> 
                    Res
            end
    end.


%%% ----------------------------------------------------------
%%% #          handle_get_cred(Index) ->
%%% Input:       -
%%% Output:      {cert, {Cert, Key}} | {shared, Key}
%%% Exceptions: 
%%% Description: Get credential e.g. CMPv2. Can be VC,
%%%              shared secret or some other certifcate credential.
%%%
%%%              At the moment it only gives the vc.
%%% ----------------------------------------------------------
handle_get_cred(Index) ->
    case sysEnv:vrcs() of
        false ->
            case handle_get_vc() of
                {ok, VcCertPem, VcKeyPem} -> %% Real or dummy VC found
                    {cert, {VcCertPem, VcKeyPem}};
                _ -> %% No VC found, check if there is a OTP
                    case handle_get_nc_key(Index, "nc.c") of
                        {ok, SharedSecret} ->
                            {shared, binary_to_list(SharedSecret)};
                        _ ->
                            {error, "Found no VC or shared secret"}
                    end
            end;
        true ->
            case handle_get_nc_key(Index, "nc.c") of
                {ok, SharedSecret} ->
                    {shared, binary_to_list(SharedSecret)};
                _ ->
                    {error, "Found no shared secret"}
            end
    end.


%%% ----------------------------------------------------------
%%% #          handle_remove_nc_dir(Index) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Remove entry in Secure Storage.
%%% ----------------------------------------------------------
handle_remove_nc_dir(Index) -> % Take whole directory
    Path = certLib:nc_dir(Index),
    case secure_node() of
        true ->
            %% FIXME remove all sec store first if any
            lists:foreach(                
                fun(Filename) ->
                        File = filename:join(Path, Filename),
                        delete_sec(File)
                end, string:tokens(os:cmd(lists:append("ls ", Path)), "\n"));
        false ->
            ok
    end,
    certLib:cmd(["rm -rf ", Path]),
    ok.


%%% ----------------------------------------------------------
%%% #          handle_remove_nc_key(Index, Filename) ->
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Remove entry in Secure Storage.
%%% ----------------------------------------------------------
handle_remove_nc_key(Index, Filename) ->
    Path = certLib:nc_dir(Index),
    File = filename:join(Path, Filename),
    case secure_node() of
        true ->
            delete_sec(File);
        false ->
            file:delete(File)
    end,
    ok.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

read_vc_state(false) ->
    unknown;
read_vc_state(CupBinary) ->
    Port = open_port({spawn_executable, CupBinary},
            [stream, {cd, "/tmp"}, {args, ["-c"]}, binary,
             stderr_to_stdout, exit_status]),
    read_port_result(Port).

read_port_result(Port) ->
    receive
    {Port,{exit_status, 0}} -> vc;
    {Port,{exit_status, _}} -> no_vc;
    {Port, _} -> read_port_result(Port)
    after
    5000 ->
        catch port_close(Port),
        info_msg("~s~n", ["Timeout reading VC status"]),
        unknown
    end.

secure_node() ->
    case get(secure_node) of
        true ->
            true;
        false ->
            false;
        undefined ->
            case sysInitI:is_secure() of
                true ->
                    put(secure_node, true),
                    true;
                false ->
                    info_msg("unsecure node~n", []),
                    put(secure_node, false),
                    false
            end
    end.

read_sec(Filename) ->
    {Sdod_pid, Sdod_port} = get_sdod(),
    Index     = mk_index(),
    Read_req  = mk_sdod_get_req(Index, Filename),
    flush(Sdod_port, Sdod_pid),
    ok = itc:send(Sdod_port, Sdod_pid, ?RHAI_MSG_SDO_GET_REQUEST, Read_req),
    receive
        {message, Sdod_port, {Sdod_pid, _To_box, _Msg_no, Data}} ->
            decode_sdod_get_answer(Data)
    after
        6000 ->
            {error, "timedout waiting for sdod decrypt"}
    end.


write_sec(Filename, StoreData) ->
    {Sdod_pid, Sdod_port} = get_sdod(),
    Index     = mk_index(),
    Read_req  = mk_sdod_store_req(Index, Filename, StoreData),
    flush(Sdod_port, Sdod_pid),
    ok = itc:send(Sdod_port,Sdod_pid,?RHAI_MSG_SDO_STORE_REQUEST,Read_req),
    receive
        {message, Sdod_port, {Sdod_pid, _To_box, _Msg_no, Data}} ->
            decode_sdod_store_answer(Data)
    after
        8000 ->
            ok =
            itc:send(Sdod_port,Sdod_pid,?RHAI_MSG_SDO_STORE_REQUEST,Read_req),
            receive
                {message, Sdod_port, {Sdod_pid, _To_box, _Msg_no, Data}} ->
                    decode_sdod_store_answer(Data)
            after
                8000 ->
                    {error, "timedout waiting for sdod decrypt"}
            end
    end.

flush(Sdod_port, Sdod_pid) ->
    receive
        {message, Sdod_port, {Sdod_pid, _To_box, _Msg_no, _Data}} ->
            flush(Sdod_port, Sdod_pid)
    after
        0 ->
            ok
    end.

delete_sec(Filename) ->
    {Sdod_pid, Sdod_port} = get_sdod(),
    Index     = mk_index(),
    Read_req  = mk_sdod_remove_req(Index, Filename),
    flush(Sdod_port, Sdod_pid),
    ok = itc:send(Sdod_port,Sdod_pid,?RHAI_MSG_SDO_REMOVE_REQUEST,Read_req),
    receive
        {message, Sdod_port, {Sdod_pid, _To_box, _Msg_no, Data}} ->
            decode_sdod_remove_answer(Data)
    after
        8000 ->
            {error, "timedout waiting for sdod decrypt"}
    end.


%%typedef struct rhai_msg_sdo_get_ans{
%%	uint32_t msgno;  <- stripped by eitc
%%	uint32_t result_code;
%%	uint16_t object_info;
%%	uint16_t pad;
%%	uint32_t data_length;
%%	/* data */
%%} rhai_msg_sdo_get_ans_t
decode_sdod_get_answer(<<(?SDO_RC_OK):32/native,_:16, _:16, L:32/native,
			 Rest/binary>>) ->
    if
	size(Rest) =:= L ->
	    {ok, Rest};
	true ->
	    {error, "size error in decoding from sdod"}
    end;
decode_sdod_get_answer(<<(?SDO_RC_COULD_NOT_START):32/native,_/binary>>) ->
    {error, cound_not_start};
decode_sdod_get_answer(<<(?SDO_RC_HW_ERROR):32/native,_/binary>>) ->
    {error, hw_error};
decode_sdod_get_answer(<<(?SDO_RC_FILE_ERROR):32/native,_/binary>>) ->
    {error, file_error};
decode_sdod_get_answer(<<(?SDO_RC_FILE_DOES_NOT_EXIST):32/native,_/binary>>) ->
    {error, no_file_exist};
decode_sdod_get_answer(<<(?SDO_RC_AUTH_FAILED):32/native,_/binary>>) ->
    {error, auth_failed}.


%%typedef struct rhai_msg_sdo_store_ans{
%%	uint32_t msgno;  <- stripped by eitc
%%	uint32_t result_code;
%%} rhai_msg_sdo_store_ans_t;
decode_sdod_store_answer(<<(?SDO_RC_OK):32/native,_/binary>>) ->
    ok;
decode_sdod_store_answer(<<(?SDO_RC_COULD_NOT_START):32/native,_/binary>>) ->
    {error, cound_not_start};
decode_sdod_store_answer(<<(?SDO_RC_HW_ERROR):32/native,_/binary>>) ->
    {error, hw_error};
decode_sdod_store_answer(<<(?SDO_RC_FILE_ERROR):32/native,_/binary>>) ->
    {error, file_error};
decode_sdod_store_answer(<<(?SDO_RC_FILE_EXISTS):32/native,_/binary>>) ->
    {error, file_exist};
decode_sdod_store_answer(<<(?SDO_RC_AUTH_FAILED):32/native,_/binary>>) ->
    {error, auth_failed}.

%%typedef struct rhai_msg_sdo_remove_ans{
%%	uint32_t msgno;  <- stripped by eitc
%%	uint32_t result_code;
%%} rhai_msg_sdo_remove_ans_t;
decode_sdod_remove_answer(<<(?SDO_RC_OK):32/native,_/binary>>) ->
    ok;
decode_sdod_remove_answer(<<(?SDO_RC_COULD_NOT_START):32/native,_/binary>>) ->
    {error, cound_not_start};
decode_sdod_remove_answer(<<(?SDO_RC_HW_ERROR):32/native,_/binary>>) ->
    {error, hw_error};
decode_sdod_remove_answer(<<(?SDO_RC_FILE_ERROR):32/native,_/binary>>) ->
    {error, file_error};
decode_sdod_remove_answer(<<(?SDO_RC_FILE_DOES_NOT_EXIST):32/native,_/binary>>) ->
    {error, no_file_exist};
decode_sdod_remove_answer(<<(?SDO_RC_AUTH_FAILED):32/native,_/binary>>) ->
    {error, auth_failed}.


read_dummy_vc() ->
    Path = certLib:vc_dir(),
    CertFile = filename:join(Path, "vc.crt"),
    KeyFile  = filename:join(Path, "vc.key"),
    case file:read_file(CertFile) of
        {ok, CertBin} ->
            case file:read_file(KeyFile) of
                {ok, KeyBin} ->
                    {ok, CertBin, KeyBin};
                _ ->
                    not_found
            end;
        _ ->
            not_found
    end.

read_real_vc() ->
    case get_real_vc() of
        novc ->
            %info_msg("No Vendor Credential found, get dummy~n", []),
            read_dummy_vc();
        {error, _Reason} ->
            %info_msg("Getting Vendor Credential failed: ~p~n", [Reason]),
            %info_msg("Get Vendor Credential dummy~n", []),
            read_dummy_vc();
        {vc, Bin} ->
            case public_key:pem_decode(Bin) of
                [K, {'Certificate', _Cert,_} = C|T] ->
                    CertBin = public_key:pem_encode(lists:append([C], T)),
                    KeyBin  = public_key:pem_encode([K]),
                    {ok, CertBin, KeyBin};
                _ ->
                    info_msg("Reading Vendor Credential file failed~n", []),
                    not_found
            end
    end.


real_vc() ->
    case get_real_vc() of
        novc ->
            %info_msg("No Vendor Credential found, get dummy~n", []),
            dummy_vc();
        {error, _Reason} ->
            %info_msg("Getting Vendor Credential failed: ~p~n", [Reason]),
            %info_msg("Get Vendor Credential dummy~n", []),
            dummy_vc();
        {vc, Bin} ->
            case public_key:pem_decode(Bin) of
                [_K, {'Certificate', _Cert,_} = C|_] ->
                    CertBin = public_key:pem_encode([C]),
                    update_vc(CertBin);
                _ ->
                    info_msg("Reading Vendor Credential file failed~n", []),
                    not_found
            end
    end.


dummy_vc() ->
    VcKeyFile1  = filename:join(certLib:vc_dir(), "vc.key"),
    VcCertFile1 = filename:join(certLib:vc_dir(), "vc.crt"),
    VcKeyFile2  = filename:join(sysEnv:dev_patches_dir(), "vc.key"),
    VcCertFile2 = filename:join(sysEnv:dev_patches_dir(), "vc.crt"),
    ok = filelib:ensure_dir(VcKeyFile1),
    case {file:read_file_info(VcKeyFile2),
            file:read_file_info(VcCertFile2)} of
        {{ok,_}, {ok,_}} -> % Patch VC
            file:copy(VcKeyFile2, VcKeyFile1),
            file:copy(VcCertFile2, VcCertFile1),
            {ok, CertBin} = file:read_file(VcCertFile1),
            update_vc(CertBin);
        _ ->
            case {file:read_file_info(VcKeyFile1),
                    file:read_file_info(VcCertFile1)} of
                {{ok,_}, {ok,_}} -> % VC exist
                    {ok, CertBin} = file:read_file(VcCertFile1),
                    update_vc(CertBin);
                _ ->
                    ok
            end
    end.

update_vc(CertBin) ->
    case public_key:pem_decode(CertBin) of
        [{'Certificate',Cert,_}|_] ->
            CC = certLib:read_cert_metadata(Cert),
            VC = #vendorCredential{
                vendorCredentialId = {"1","1","1","1","1"},
                certificateContent = CC,
                certificateState   = ?CertificateState_VALID},
            mnesia:dirty_write(VC),
            ok;
        _ ->
            info_msg("Reading Vendor Certificate failed~n", []),
            error
    end.

handle_get_next_chain_key(undefined) ->
    %% First chain certificate
    do_handle_get_next_chain_key(1);
handle_get_next_chain_key({"1","1","1","1","1",ChainId}) ->
    do_handle_get_next_chain_key(list_to_integer(ChainId) + 1);
handle_get_next_chain_key(ChainId) ->
    do_handle_get_next_chain_key(ChainId + 1).

do_handle_get_next_chain_key(Nth) ->
    case handle_get_vc() of
        {ok, CertPemBin,_} ->
            List = public_key:pem_decode(CertPemBin),
            case length(List) of
                Value when Value > Nth ->
                    {ok, {"1","1","1","1","1",integer_to_list(Nth)}};
                _ ->
                    {ok, undefined}
            end;
        _ ->
            {ok, undefined}
    end.

handle_get_chain_cc(Nth) ->
    {ok, CertPemBin,_} =  handle_get_vc(),
    List = public_key:pem_decode(CertPemBin),
    %% +1 since the first is vendor cert, not chain cert
    {'Certificate',ChainCert,_} = lists:nth(Nth+1,List),
    certLib:read_cert_metadata(ChainCert).
            


%%% ----------------------------------------------------------
%%% #           get_real_vc()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
get_real_vc() ->
    get_vc(get_serialnumber()).

get_serialnumber() ->
    List = eqs_pri_service:get_product_information_data(),
    case proplists:get_value(serialNumber, List) of
        undefined ->
            "";
        Serial_nr ->
            Serial_nr
    end.


%% ===========================================================================
%% @doc
%%  Read the VC-flash container and return the section named
%% "Ericsson-vc-1"
%% @end
%% ===========================================================================
-spec get_vc(Serial_number :: string()) ->
    {vc, binary()} | novc | {error, any()}.
get_vc("") ->
    info_msg("No serial number found!~n", []),
    {error, "No serial number found!"};
get_vc(Serial_number) when is_list(Serial_number),
length(Serial_number) =:= 10 ->
    try read_and_disassemble_vc() of
	{ok, ?MAGIC, VC_list} ->
	    Name = list_to_binary(?VCNAME),
	    case lists:keyfind(Name, 1, VC_list) of
		{Name, Blob} ->
		    case undodify(Serial_number, Blob) of
			{ok, VC_blob} ->
			    {vc, VC_blob};
			Err ->
			    Err
		    end;
		false	->
		    {error, ?VCNAME " not found in VC container"}
	    end;
	{ok, ?NOMAGIC, VC_list} ->
	    Name = mk_name(Serial_number),
	    case lists:keyfind(Name, 1, VC_list) of
		{Name, Blob} ->
		    case undodify(Serial_number, Blob) of
			{ok, Name} ->
			    novc;
			{ok, _} ->
			    {error, "NOVC-flag not found in sdod"};
			Err ->
			   Err
		    end;
		false	->
		    {error, "NOVC-flag not found in VC container"}
	    end;
	Error ->
	   Error
    catch
	A:B ->
	    io:format("~p: catched: ~p:~p~nbacktrace: ~p~n",
			[?MODULE, A, B, erlang:get_stacktrace()]),
	   {error, "crash in read_and_disassemble_vc()"}
    end.


-spec read_and_disassemble_vc() -> [{binary(), binary()}] | {error, any()}.
read_and_disassemble_vc() ->
    Device = get_device(),
    case read_nor(Device) of
	{ok, Container} ->
	    disassemble_container(Container);
	{error, _} = Err ->
	    Err
    end.


get_device() ->
    String    = os:cmd("cat /proc/mtd | grep vendor_credentials"),
    [Mtd|_]   = string:tokens(String, ":"),
    [_,MtdNr] = string:tokens(Mtd, "d"),
    ?VCDEVICE ++ MtdNr.


undodify(Serial_number, Blob) ->
    {Sdod_pid, Sdod_port} = get_sdod(),
    Index     = mk_index(Serial_number),
    File_name = mk_fname(),
    Read_req  = mk_sdod_get_req(Index, File_name),
    ok = file:write_file(File_name, Blob, [sync]),
    ok = itc:send(Sdod_port, Sdod_pid, ?RHAI_MSG_SDO_GET_REQUEST, Read_req),
    receive
        {message, Sdod_port, {Sdod_pid, _To_box, _Msg_no, Data}} ->
            file:delete(File_name),
            decode_sdod_get_answer(Data)
    after
        5000 ->
            file:delete(File_name),
            {error, "timedout waiting for sdod decrypt"}
    end.


get_sdod() ->
    case get(sdod_pid) of
        undefined ->
            %% Open ITC port
            {ok, P, Pid} = open_sdod_port(atom_to_list(?MODULE)),
            put(sdod_pid, Pid),
            put(sdod_port, P),
            {Pid, P};
        Pid ->
            {Pid, get(sdod_port)}
    end.


open_sdod_port(NameString) ->
    Port = itc:open(NameString),
    ok = itc:listen(Port),
    Huntref = itc:hunt(Port, ?RHAI_SDO_MBOX_NAME),
    receive
	{mailbox_up, Port, Huntref, Sdod_pid} ->
	    {ok, Port, Sdod_pid}
    after
	5000 ->
	    {error, "timedout hunting for sdod"}
    end.

% typedef struct rhai_msg_sdo_get_req{
%         uint32_t msgno;
%         uint64_t owner_info;
%         char path[RHAI_SDO_MAX_PATH_LENGHT];
% } rhai_msg_sdo_get_req_t;
mk_sdod_get_req(Index, Filename) ->
    <<0:32/native, Index:64/native,
	(pad_to_x(Filename, ?RHAI_SDO_MAX_PATH_LENGHT))/binary>>.

% typedef struct rhai_msg_sdo_store_req{
%         uint32_t msgno;
%         uint64_t owner_info;
%         uint16_t object_info;
%         uint16_t pad;
%         char path[RHAI_SDO_MAX_PATH_LENGHT];
%         uint32_t data_length;
%         /* data */
mk_sdod_store_req(Index, Filename, StoreData) ->
    <<0:32/native, Index:64/native,0:16/native,0:16/native,
     (pad_to_x(Filename, ?RHAI_SDO_MAX_PATH_LENGHT))/binary,
     (byte_size(StoreData)):32/native,StoreData/binary>>.

% typedef struct rhai_msg_sdo_remove_req{
%         uint32_t msgno;
%         uint64_t owner_info;
%         char path[RHAI_SDO_MAX_PATH_LENGHT];
mk_sdod_remove_req(Index, Filename) ->
     <<0:32/native, Index:64/native,
     (pad_to_x(Filename, ?RHAI_SDO_MAX_PATH_LENGHT))/binary>>.



disassemble_container(Container) when size(Container) =:= ?STORAGESIZE ->
    Header_size = ?MAGIC_LENGTH + 4 + 4, %(i.e. =12)
    <<Header:Header_size/binary, Sections_and_pad/binary >> = Container,
    case Header of
	<<Magic:?MAGIC_LENGTH/binary, ?VERSION:32/big, Tsize:32/big>> ->
	   case binary_to_list(Magic) of
		Mag when Mag =:= ?MAGIC;
			 Mag =:= ?NOMAGIC ->
		    <<Sections:Tsize/binary, Crc:32/big, _/binary >> = Sections_and_pad,
		    Calc_crc = crc(<<Header/binary, Sections/binary>>),
		    case Calc_crc of
			Crc ->
			    {ok, Mag, extract_vc(Mag, Sections, [])};
			_ ->
			    {error, "CRC error, read: 0x" ++ integer_to_list(Crc, 16) ++
			    " calculated: 0x" ++ integer_to_list(Calc_crc, 16)}
		    end
	    end;
	_ ->
	    {error, "cannot make sense storage content"}
    end.

% take the container without initial part and return a
% list_of_vc_tuples
% each VC in the returned list is a tuple containing two
% binaries {<<Name of VC>>, <<Data blob>>}
extract_vc(Magic, Sections, Found_vcs) ->
    <<Size:32/big, Rest/binary>> = Sections,
    if
	Size =:= 0 ->
	    Found_vcs;
	true ->
	    Reminder = Size rem 4,
	    Actual_size = if
			Reminder =:= 0 -> Size - 4;
				true -> Size + (4 - Reminder) - 4
			end,
	    Container_size = Size - 4,
	    <<Padded_cont:Actual_size/binary, Further_vc/binary >> = Rest,
	    <<Actual_container:Container_size/binary, _/binary >> = Padded_cont,
	    <<Name_bin:32/binary, VC_data/binary>> = Actual_container,
	    Normalized_name =
		case Magic of
		    ?MAGIC ->
			%get rid of nulls
			[Name] = binary:split(Name_bin, <<0>>, [global, trim]),
			Name;
		    ?NOMAGIC ->
			%Name is sha256 encoding may hold multiple nulls
			Name_bin
		end,
	    extract_vc(Magic,
		       Further_vc, [{Normalized_name, VC_data } | Found_vcs])
    end.

pad_to_x(List, Final_size) when is_list(List) ->
    pad_to_x(list_to_binary(List), Final_size);
pad_to_x(Binary, Final_size) when is_binary(Binary),
				  size(Binary) =< Final_size ->
    Size = size(Binary),
    <<Binary/binary, 0:(Final_size - Size)/unit:8>>.

read_nor(Device) ->
    Port = open_port({spawn_executable, os:find_executable("dd")},
	[{args, ["if=" ++ Device, "status=none"]},
	stream, binary, exit_status]),
    read_vc_from_port(Port, <<>>).

read_vc_from_port(Port, Sofar) when is_port(Port)->
    receive
        {Port,{data, Data}} ->
            read_vc_from_port(Port, <<Sofar/binary, Data/binary>>);
        {Port,{exit_status, Status}} when
                Status =:= 0, size(Sofar) =:= ?STORAGESIZE ->
            {ok, Sofar};
        {Port,{exit_status, Status}}  ->
        {error, "read_vc_from_port exit: " ++ integer_to_list(Status)}
    after 4000 ->
        catch port_close(Port),
        {error, "read_vc_from_port timed out"}
    end.

mk_name(Serial) ->
    crypto:hmac(sha256, ?PASW, Serial).


mk_index() -> %% Used for everything else, than VC
    %a blank added to get exactly 24 bytes
    Idx = (<<"1                       ">>),
    0 = (size(Idx) rem 4), %assert
    xorro(Idx, 0).

mk_index(Serial) -> %% Only used for VC
    %a blank added to get exactly 24 bytes
    Idx = (<<(list_to_binary(?VCNAME))/binary,  " ", 
	(list_to_binary(Serial))/binary>>),
    0 = (size(Idx) rem 4), %assert
    xorro(Idx, 0).

xorro(<<>>, R) -> R;
xorro(<<V:32, Rest/binary>>, R) ->
    xorro(Rest, R bxor V).

mk_fname() ->
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    Dt = io_lib:format("~4.10.0b-~2.10.0b-~2.10.0b_~2.10.0b:~2.10.0b:~2.10.0b",
	[Y, M, D, H, Min, S]),
    Name = atom_to_list(?MODULE) ++ "." ++ lists:flatten(Dt) ++ "." ++
	   os:getpid(),
    filename:join(["/tmp", Name]).


%% NOTE! This needs full octets, i.e. if the input is viewed as a bitstream the
%% length of it must be a multiple of 8, it also fails if every single bit is 0.
crc(Binary) ->
%%io:format("crc on binary size: ~b~n", [size(Binary)]),
	calc(<< >>, <<Binary/binary, 0:32>>).

calc(<<A:8, B:8, C:8, D:8>>, <<>>) ->
%%This one is CPP compatibe on ppc but not on x86
    <<Done:32>> = <<(rb(<<D:8>>)), (rb(<<C:8>>)),
        (rb(<<B:8>>)), (rb(<<A:8>>))>>,
%This one is CPP compatible on x86 not on ppc
%%  <<Third:32>> = <<(rb(<<A:8>>)), (rb(<<B:8>>)),
%%      (rb(<<C:8>>)), (rb(<<D:8>>))>>,
%%io:format("Done crc: 0x~.16b~n", [Done]),
%%io:format("Third (returned) crc: 0x~.16b~n", [Third]),
    Done;
calc(<<0:1, H:32, Rest/bits>>, TT) -> % HV85346
    calc(<<H:32, Rest/bits>>, TT);
calc(<<_:1, H:32,  Rest/bits>>, TT) ->
    calc(<<(H bxor ?POLY):32, Rest/bits>>, TT);
calc(To_short, <<B:8, Rest/bits>>) ->
    calc(<<To_short/bits, (rb(<<B:8>>))>>, Rest).

rb(<<B1:1, B2:1, B3:1, B4:1, B5:1, B6:1, B7:1, B8:1>>) ->
	<<Byte:8>> = <<B8:1, B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1>>,
	Byte.

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).



%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

