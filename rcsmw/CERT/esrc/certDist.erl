%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certDist.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R11A/R12A/2
%%% 
%%% @doc ==Handle certificate distrubution of certificates.==
%%% Will be used for VRCS and BPU
-module(certDist).
-behaviour(gen_server).
-vsn('/main/R9A/R10A/R11A/R12A/2').
-date('2017-11-25').
-author('etxarnu').
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
%%% Rev     Date       Name     What
%%% -----   ---------- -------  ------------------------------
%%% R9A/1   2017-02-15 etxasta  Created
%%% R9A/12  2017-04-06 emariad  Added dist_vnf_log_key for vrcs
%%% R9A/13  2017-04-11 emariad  Updated dist_vnf_log_key for vrcs
%%% R10A/1  2017-05-09 emariad  Changed secSerer call read in dist_vnf_log_key
%%% R10A/3  2017-05-17 emariad  Small change in handle_dist_vnf_log_key
%%% R10A/5  2017-06-15 etxasta  Bug fix for HV93930
%%% R10A/6  2017-08-21 enenteo  Updated for key backup SP531/SP419
%%% R11A/1  2017-08-21 enenteo Delivery to R11
%%% R12A/2  2017-11-25 etxarnu Use comsaI:has_consul()
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%% Called from supervisor
-export([start/0]).

-export([get_cert_dist_dir/0,
         update_cert_dist/1]).

%% VNFM distributes VNFC internal credential,
%% used between VNFM and VNFCs
-export([dist_vnfc_certs/3]).
-export([get_vnfc_certs/0]).

%VNF distributes decryption log keys to VNFM
-export([dist_vnf_key/1]).
-export([get_vnf_log_key_from_vnfm/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).


-define(SHARED, [229,23,242,121,107,96,160,211,79,240,66,
        40,176,139,225,173,101,160,158,192]).


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% FIXME remove when redirected in vnfcHttps.erl
get_vnfc_certs() ->
    certVnfCert:get_cert().

%%% ----------------------------------------------------------
%%% @doc Starts the certDist server process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% #         get_cert_dist_dir() ->
%%% Input:  -
%%% Output: string(PathDir)
%%% Exceptions: 
%%% Description: Returns the directory there the files
%%%              are stored for distribution.
%%% ----------------------------------------------------------
get_cert_dist_dir() ->
    certLib:tmp_cert_dir(). 

%%% ----------------------------------------------------------
%%% #          update_cert_dist(internal|{NS, NcMoRef, TcMoRef}|
%%%                  {nc, NcIndex}|{tcat, TcatIndex}|NS) ->
%%% Input: NS      - string(NameSpace)
%%%        NcMoRef - string(NcMoRef) 
%%%        TcMoRef - string(TcMoRef)
%%% Output: ok
%%% Exceptions: 
%%% Description: Update the certificate distribution files for chosen
%%%              NameSpace. Only NS will remove certificate
%%%              files for that NS.
%%%              Internal will only be used internally
%%% ----------------------------------------------------------
update_cert_dist(Info) ->
    case {comsaI:has_consul(), sysEnv:vrcs(), swmI:node_type()} of
        {false, false,_} -> %% No BPU or VRCS
            ok;
        {_, true, "R-VNFM"} -> %% Not on R-VNFM
            ok;
        {_,_,_} -> %% This is VRCS or BPU
            info_msg("update_cert_dist, ~p", [Info]),
            gen_server:cast(?MODULE, {update_cert_dist, Info})
    end,
    ok.

%%% ----------------------------------------------------------
%%% #          dist_vnf_key() ->|
%%% Input: KeyIvecBin
%%% Output: ok
%%% Exceptions: 
%%% Description: Distribute the encryption/decryption
%%% key for this vnf to ran-vnfm
%%% ----------------------------------------------------------
dist_vnf_key(KeyIvecBin)->
    
    case {sysEnv:vrcs(), swmI:node_type()} of
        {true, "R-VNFM"} ->
            info_msg("Key not distributed on R-VNFM", []),
            ok;
        {true, _} ->
            info_msg("Distribute key to R-VNFM", []),
            gen_server:cast(?MODULE, {dist_vnf_key, KeyIvecBin});
        _ ->
            ok
    end,
    ok.

%%% ----------------------------------------------------------
%%% #          get_vnf_log_key_from_vnfm(VnfcId) -> 
%%% Input: VnfcId
%%% Output: {ok, Key, Ivec}|{error, Reason}
%%% Exceptions: 
%%% Description: Distribute the decryption log key for this vnf to ran-vnfm
%%% ----------------------------------------------------------
get_vnf_log_key_from_vnfm(VnfcId)->
    info_msg("get_vnf_log_key_from_vnfm, VnfcId=~p", [VnfcId]),
    case {sysEnv:vrcs(), swmI:node_type()} of
            {true, "R-VNFM"} -> % Should not be called on R-VNFM
                {error, "Node is R-VNFM"};
            {true, _} ->
                gen_server:call(?MODULE, {get_vnf_log_key_from_vnfm, VnfcId});
            {_,_}->
                {error, "Not able to read node type"}
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(_Args) ->
    {ok, up}.

%handle_call
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%handle_cast
handle_cast({update_cert_dist, Info}, State) ->
    handle_update_cert_dist(Info),
    {noreply, State};
handle_cast({dist_vnf_key,KeyIvecBin}, State) ->
    handle_dist_vnf_key(KeyIvecBin),
    {noreply, State};
handle_cast({get_vnf_log_key_from_vnfm, VnfcId}, State) ->
    handle_get_vnf_log_key_from_vnfm(VnfcId),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%handle_info
handle_info({handle_dist_vnf_key, KeyIvecBin}, State) ->
    handle_dist_vnf_key(KeyIvecBin),
    {noreply, State};
handle_info({handle_get_vnf_log_key_from_vnfm, VnfcId}, State) ->
    handle_get_vnf_log_key_from_vnfm(VnfcId),
    {noreply, State};
handle_info({send_data_to_vnfm, DataToSend}, State) ->
    send_data_to_vnfm(DataToSend),
    {noreply, State};
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
%%% #          handle_update_cert_dist(internal|
%%%             {NS, NcMoRef, TcMoRef}|{nc, NcIndex}|
%%%             {tcat, TcatIndex}|NS) ->
%%% Input: NS      - string(NameSpace)
%%%        NcMoRef - string(NcMoRef)
%%%        TcMoRef - string(TcMoRef)
%%% Output: ok
%%% Exceptions: 
%%% Description: Update the certificate files for chosen
%%%              NameSpace.
%%%              Only NS will remove certificate files for that NS.
%%%              Internaly cert will use {nc, NcIndex} and
%%%              {tcat, TcatIndex} to update files when
%%% ----------------------------------------------------------
handle_update_cert_dist(internal) ->
    %% Create/update internal certificate files
    do_update_cert_dist(certVnfCert:get_cert());
handle_update_cert_dist({nc, NcIndex}) ->
    handle_update_cert_dist(get_dist_info({nc, NcIndex}));
handle_update_cert_dist({tcat, TcatIndex}) ->
    handle_update_cert_dist(get_dist_info({tcat, TcatIndex}));
handle_update_cert_dist({NS, NcMoRef, TcMoRef}) when is_list(NS) ->
    %% Create/update Node Credential and Trust Category certificate files
    do_update_cert_dist(certLib:decode_moref(NcMoRef),
        certLib:decode_moref(TcMoRef), NcMoRef, TcMoRef, NS);
handle_update_cert_dist(NS) when is_list(NS) ->
    %% Remove certificate files
    rm_tmp_file("cert_" ++ NS ++ ".pem"),
    rm_tmp_file("key_" ++ NS ++ ".pem"),
    rm_tmp_file("tc_" ++ NS ++ ".pem"),
    %% Remove distribution information
    del_dist_info(NS),
    ok;
handle_update_cert_dist(_) ->
    ok.

do_update_cert_dist({error, Reason1}, {error, Reason2}, NcMoRef, TcMoRef,_) ->
    info_msg("NC(~p) failed, reason: ~p,~nTC(~p) failed, reason: ~p",
        [NcMoRef, Reason1, TcMoRef, Reason2]),
    {error, "Not found"};
do_update_cert_dist({error, Reason}, _, NcMoRef, _,_) ->
    info_msg("NC(~p) failed, reason: ~p", [NcMoRef, Reason]),
    {error, "Not found"};
do_update_cert_dist(_, {error, Reason}, _, TcMoRef,_) ->
    info_msg("TC(~p) failed, reason: ~p", [TcMoRef, Reason]),
    {error, "Not found"};
do_update_cert_dist({nc, NcKey}, {tcat, TcKey}, NcMoRef, TcMoRef, NS) ->
    %% Store distribution info
    put_dist_info(NcKey, TcKey, NcMoRef, TcMoRef, NS),
    %% Check if the certificate files can be created or updated
    case certNcServer:get_nc_der(NcKey) of
        {ok, CertDerList, {KeyType, DerPrivKey}} -> %% NC
            case certServer:get_tcat_and_crlcheck(TcKey) of
                {ok, TcatList, _CrlCheck} ->
                    %% Later on if crl check should pass on CrlCheck aswell
                    NodeCert = certLib:der_to_pem(CertDerList, 'Certificate'),
                    PrivKey  = certLib:der_to_pem([DerPrivKey], KeyType),
                    TcCerts  = certLib:der_to_pem(TcatList, 'Certificate'),
                    mk_tmp_file("cert_" ++ NS ++ ".pem", NodeCert),
                    mk_tmp_file("key_" ++ NS ++ ".pem", PrivKey),
                    mk_tmp_file("tc_" ++ NS ++ ".pem", TcCerts),
                   ok;
                Error ->
                    info_msg("TC(~p) failed, ~p", [TcMoRef, Error]),
                    {error, "Not found"}
            end;
        Error ->
            info_msg("NC(~p) failed, ~p", [NcMoRef, Error]),
            {error, "Not found"}
    end;
do_update_cert_dist(_,_, NcMoRef, TcMoRef,_) ->
    info_msg("NC(~p) and TC(~p) not supported", [NcMoRef, TcMoRef]),
    {error, "Not supported"}.


do_update_cert_dist({ok, [Cert, SubCa], {Format, Key},
        [_RootCa, _VnfmSubCa], _Type}) ->
    %% Exclude the RootCa, to prevent connection to other VNFs
    %% Exclude the VnfmSubCa, to prevent connection to the VNFM
    CertPem       = certLib:der_to_pem([Cert], 'Certificate'),
    KeyPem        = certLib:der_to_pem([Key],  Format),
    TrustCertsPem = certLib:der_to_pem([SubCa], 'Certificate'),
    mk_tmp_file("cert.pem", CertPem),
    mk_tmp_file("key.pem", KeyPem),
    mk_tmp_file("tc.pem", TrustCertsPem),
    info_msg("Fixed internal certificates", []),
    ok;
do_update_cert_dist({NcKey, TcKey, NcMoRef, TcMoRef, NS}) ->
    do_update_cert_dist({nc, NcKey}, {tc, TcKey}, NcMoRef, TcMoRef, NS);
do_update_cert_dist(_) ->
    ok.


mk_tmp_file(Filename, Bin) when is_binary(Bin) ->
    Dir  = certLib:tmp_cert_dir(),
    File = filename:join(Dir, Filename),
    case file:read_file(File) of
        {ok, _} -> % Already exist
            file:delete(File);
        _  ->
            ok
    end,
    % Make file
    ok = filelib:ensure_dir(File),
    certLib:cmd(["chmod a+rwx ", Dir]),
    ok = file:write_file(File, Bin, [sync]).

rm_tmp_file(Filename) ->
    Dir  = certLib:tmp_cert_dir(),
    File = filename:join(Dir, Filename),
    file:delete(File).


put_dist_info(NcIndex, TcatIndex, NcMoRef, TcMoRef, NS) ->
    put({nc, NcIndex}, {NS, NcMoRef, TcMoRef}),
    put({tcat, TcatIndex}, {NS, NcMoRef, TcMoRef}),
    put(NS, {NcIndex, TcatIndex, NcMoRef, TcMoRef}),
    ok.

get_dist_info({nc, NcIndex}) ->
    get({nc, NcIndex});
get_dist_info({tcat, TcatIndex}) ->
    get({tcat, TcatIndex});
get_dist_info(NS) when is_list(NS) ->
    get(NS).
   
del_dist_info(NS) when is_list(NS) ->
    case get_dist_info(NS) of
        {NcIndex, TcatIndex, _NcMoRef, _TcMoRef} ->
            put({nc, NcIndex}, undefined),
            put({tcat, TcatIndex}, undefined),
            put(NS, undefined),
            ok;
        _ ->
            ok
    end.


%%% ----------------------------------------------------------
%%% #          handle_dist_vnf_key(KeyIvecBin)
%%% Input:  KeyIvecBin key and Initialization Vector in binary format
%%% Output: ok | {error, string(Reason)} 
%%% Exceptions: 
%%% Description: Distribution of  the decryption key used for logs.
%%%              To be stored in VNFM to be used when VNF/VNFC shows
%%%              faulty behaviour.
%%% ----------------------------------------------------------
handle_dist_vnf_key(KeyIvecBin) ->    
    case  apply(vnfcI, vnfc_id, []) of
        false ->
            info_msg("VnfcId not defined while distributing the key, key not distributed",
                     []),
            ok;
        VnfcId ->
            try
                do_handle_dist_vnf_key(VnfcId, KeyIvecBin)
            catch _Type:_Reason ->
                info_msg("Send dist vnf key message again Reason:~p", [_Reason]),

                erlang:send_after(10000, self(), {handle_dist_vnf_key, KeyIvecBin})
            end
    end.



%%% ----------------------------------------------------------
%%%           do_handle_dist_vnf_key
%%% ----------------------------------------------------------
do_handle_dist_vnf_key(VnfcId, KeyIvecBin)->
    
    {ok, KeyIvecBinEnc} = certCrypto:encrypt_shared(?SHARED, KeyIvecBin),
    DataToSend=term_to_binary({store_key, VnfcId, KeyIvecBinEnc}),
    
    case send_data_to_vnfm(DataToSend) of
        {ok, ReturnValue} ->
            info_msg("Key succesfully stored in RVNFM ~nVnfcId:~p~nReturn value:~p",
                      [VnfcId, ReturnValue]),
            ok
    end.



%%% ----------------------------------------------------------
%%% #          handle_get_vnf_log_key_from_vnfm()
%%% Input:  
%%% Output: {ok, Key, Ivec} | {error, string(Reason)} 
%%% Exceptions: 
%%% Description: Dtribution of  the decryption key used for logs.
%%%              To be stored in VNFM to be used when VNF/VNFC shows
%%%              faulty behaviour.
%%% ----------------------------------------------------------
handle_get_vnf_log_key_from_vnfm(VnfcId) ->
    info_msg("handle_get_vnf_log_key_from_vnfm, VnfcId:~p", [VnfcId]),
    try
        do_handle_get_vnf_log_key_from_vnfm(VnfcId)
    catch _Type:_Reason ->
        info_msg("Fetching key for encryption/decrypyion from VNFM failed, Reason:~p",
                  [_Reason]),

        erlang:send_after(10000, self(), {handle_get_vnf_log_key_from_vnfm, VnfcId})
    end.
    

%%% ----------------------------------------------------------
%%%           do_handle_get_vnf_log_key_from_vnfm
%%% ----------------------------------------------------------
do_handle_get_vnf_log_key_from_vnfm(VnfcId) ->
    
    DataToSend = term_to_binary({get_key, VnfcId}),
    
    case send_data_to_vnfm(DataToSend) of
        {ok,  DataReceived}->
            extract_key_ivec(DataReceived)
    end.


%%% ----------------------------------------------------------
%%%           extract_key_ivec
%%%           Received data is encrypted binary of key and ivec tuple
%%% ----------------------------------------------------------
extract_key_ivec(Data) ->
    case certCrypto:decrypt_shared(?SHARED, Data) of
        {ok, KeyIvecBin} ->  
            case binary_to_term(KeyIvecBin) of
                {Key, Ivec}->
                     {ok,Key,Ivec};
                _->
                    info_msg("Corruption occured ~p", [Data]),
                    {error, "Corruption occured"}
             end;
              
        {error, Message}->
            {error, Message}
    end.


%%% ----------------------------------------------------------
%%%                   send_data_to_vnfm
%%% ----------------------------------------------------------
send_data_to_vnfm(DataToSend) ->
    
    info_msg("send_data_to_vnfm",[]),
    %fetch vnfm ip
    {ok, {Ip, Port, _Id}} = apply(vnfcI, get_vnfm_server, []),

    Header = [],
    ContentType = "text/html",
    
    IpString = case is_integer(Ip) of
                   true ->
                       integer_to_list(Ip);
                   false ->
                       Ip
               end,
    IpPort = case is_integer(Port) of
                   true ->
                       integer_to_list(Port);
                   false ->
                       Port
               end,
    
    Uri = "https://"++IpString++":"++IpPort++"/ranvnfm/ksd/handle",
    
    %default timeout cab  3s what is ok
    %default method is post so that is ok as well
    Map = maps:put(data, {Uri, Header, ContentType, DataToSend }, maps:new()),
    
    case apply(vnfcHttps, send_receive, [Map]) of
        {error, Reason} ->
            info_msg("Sending data to VNFM resulted with failing, reason:~p",
                     [Reason]),
            erlang:send_after(60000, self(), {send_data_to_vnfm, DataToSend});
        {ok, ReceivedDataBin} ->
            {ok, ReceivedDataBin}
    end.



%%% ----------------------------------------------------------
%%% #          dist_vnfc_cert(Pkcs12, CredPwd, Path)
%%% Input:  Pkcs12     - binary()
%%%         CredPwd    - string()
%%%         
%%% Output: ok | {error, string(Reason)} 
%%% Exceptions: 
%%% Description: VNFM fetches the p12 file and encrypt it and
%%%              store it to be used later by VNFC.
%%% ----------------------------------------------------------
dist_vnfc_certs(Pkcs12, CredPwd, Path) when is_list(CredPwd) ->
    {ok, P12EncBin}  = certCrypto:encrypt_shared(?SHARED, Pkcs12),
    {ok, CredEncBin} = certCrypto:encrypt_shared(?SHARED,
        list_to_binary(CredPwd)),
    ok = file:write_file(Path++"/vnfc.pfx", P12EncBin),
    ok = file:write_file(Path++"/vnfc.sec", CredEncBin).

info_msg(Format, Args) ->
    certLib:info_msg(?MODULE, Format, Args).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

