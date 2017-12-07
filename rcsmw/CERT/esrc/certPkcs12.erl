%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certPkcs12.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/1
%%% 
%%% @doc ==Pkcs12 module==
%%% 

-module(certPkcs12).
%%%---------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% Rev      Date        Name      What
%%% -----    ----------  --------  -------------------------
%%% R9A/1    2017-02-09  etxasta   Created
%%% ----------------------------------------------------------
%%%

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([unpack_offline_data/2,
         decode_pkcs12/2]).

-include_lib("public_key.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% #          unpack_offline_data(Data, CredPwd)
%%% Input:  Downloaded offline enrollment data
%%% Output: {pkcs12, Cert, PriKey}|{cert, Cert}| {error, Reason}
%%% Exceptions: 
%%% Description: Unpack the downloaded data at install from uri.
%%% NOTE: Pkcs#12 and pem is supported, der it not!!!
%%% NOTE: The rootCA in the cert chain is not in the CertWithChain,
%%%       it is added to the TcDerList
%%% ----------------------------------------------------------
unpack_offline_data(Data, CredPwd) ->
    case check_cert(Data) of
        {error, failed_decode_cert_file} ->
            case decode_pkcs12(Data, CredPwd) of
                {error, Reason} ->
                    info_msg("Failed to unpack pkcs12 file, ~p", [Reason]),
                    {error, failed_decode_pkcs12};
                {ok, C, P} ->
                    case check_key(P) of
                        {error,_} ->
                            {error, failed_decode_key_file};
                        {Format, PrivDer} ->
                            case check_cert(C) of
                                {error, _} ->
                                    {error, failed_decode_cert_file};
                                CertDerList ->
                                    {CertWithChain, TcDerList} =
                                    split_nc_chain_and_tc(CertDerList),
                                    {pkcs12, CertWithChain, {Format, PrivDer},
                                        TcDerList}
                            end
                    end
            end;
        CertDerList ->
            {CertWithChain, TcDerList} = split_nc_chain_and_tc(CertDerList),
            {cert, CertWithChain, TcDerList}
    end.

%%% ----------------------------------------------------------
%%% #          decode_pkcs12(Pkcs12, CredPwd)
%%% Input:  Pkcs12  - pkcs12 container binary
%%%         CredPwd - Password used on the pkcs12 container
%%% Output: Cert
%%%         Key 
%%% Exceptions: 
%%% Description: Unpack a pkcs12 container.
%%% ----------------------------------------------------------
decode_pkcs12(Pkcs12, CredPwd) when is_list(CredPwd) ->
    decode_pkcs12(Pkcs12, CredPwd, get(key));
decode_pkcs12(_,_) ->
    {error, "Failed to decode pkcs12 file"}.

decode_pkcs12(Pkcs12, CredPwd, undefined) -> % internal certificate for vnfc
    File = filename:join(certLib:ex_dir("internal"), "container.pkcs12"),
    do_decode_pkcs12(Pkcs12, CredPwd, File);
decode_pkcs12(Pkcs12, CredPwd,_) -> % Normal Node Credential
    File = filename:join(certLib:nc_dir(get(key)), "container.pkcs12"),
    do_decode_pkcs12(Pkcs12, CredPwd, File).

do_decode_pkcs12(Pkcs12, CredPwd, File) ->
    ok   = filelib:ensure_dir(File),
    ok   = file:write_file(File, Pkcs12),
    Cert = certLib:cmd(["openssl pkcs12 -nokeys -passin pass:",
            CredPwd, " -in ", File]),
    Key  = certLib:cmd(["openssl pkcs12 -nocerts -passin pass:",
            CredPwd, " -nodes -in ", File]),
    ok = file:delete(File),
    {ok, Cert, Key}.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
check_cert(CertData) when is_list(CertData) ->
    check_cert(list_to_binary(CertData));
check_cert(CertData) ->
    case catch public_key:pem_decode(CertData) of
        {'EXIT', _} ->
            {error, failed_decode_cert_file};
        [] -> % Might be der format
            case catch public_key:der_decode('Certificate', CertData) of
                C1 when is_record(C1, 'Certificate') -> % Just for check
                    [CertData];
                _ ->
                    {error, failed_decode_cert_file}
            end;
        Pems -> % pem format
            lists:filtermap(
                fun({'Certificate',NewCert,_}) ->
                        {true, NewCert};
                    (_) ->
                        false
                end, Pems)
    end.

check_key(KeyData) when is_list(KeyData) ->
    check_key(list_to_binary(KeyData));
check_key(KeyData) ->
    case catch public_key:pem_decode(KeyData) of
        [{'ECPrivateKey', PrivDer, _}|_] ->
            %% PEM CURVE
            {'ECPrivateKey', PrivDer};
        [_EcpkParam, {'ECPrivateKey', PrivDer, _}|_] ->
            %% PEM CURVE
            {'ECPrivateKey', PrivDer};
        [{Format, PrivDer,_}|_] ->
            %% PEM RSA
            {Format, PrivDer};
        [] -> % Might be DER
            case catch public_key:der_decode('PrivateKeyInfo', KeyData) of
                K when is_record(K, 'PrivateKeyInfo') -> % Just for check
                    {'PrivateKeyInfo', KeyData};
                _ ->
                    case catch public_key:der_decode('RSAPrivateKey',
                            KeyData) of
                        K when is_record(K, 'RSAPrivateKey') ->% Just for check
                            {'RSAPrivateKey', KeyData};
                        _ ->
                            %% Check it might be CURVE
                            case catch public_key:der_decode('ECPrivateKey',
                                    KeyData) of
                                [_,K] when is_record(K, 'ECPrivateKey') ->
                                    {'ECPrivateKey', KeyData};

                                K when is_record(K, 'ECPrivateKey') ->
                                    {'ECPrivateKey', KeyData};

                                _ ->
                                    {error, failed_decode_key_file}
                            end
                    end
            end;
        _ ->
            {error, failed_decode_key_file}
    end.

split_nc_chain_and_tc(CertDerList) ->
    do_split_nc_chain_and_tc(CertDerList, []).

do_split_nc_chain_and_tc([], [{Issuer, _Subject, CertDer}|T]) ->
    sort_out_certs(Issuer, T, [CertDer]);
do_split_nc_chain_and_tc([CertDer|T], List) ->
    OTPCert = public_key:pkix_decode_cert(CertDer, otp),
    TBS  = OTPCert#'OTPCertificate'.tbsCertificate,
    Issuer = format_name(TBS#'OTPTBSCertificate'.issuer),
    Subject = format_name(TBS#'OTPTBSCertificate'.subject),
    do_split_nc_chain_and_tc(T, List ++ [{Issuer, Subject, CertDer}]).

sort_out_certs(Name, Certs, Chain) ->
    case lists:keyfind(Name, 2, Certs) of
        false -> % Done
            {Chain, clean_list(Certs)};
        {Name, Name, _CertDer} -> % Self signed, done
            {Chain, clean_list(Certs)};
        {Issuer, Name, CertDer} -> % Not self signed, build chain
            NewCerts = lists:keydelete(Name, 2, Certs),
            sort_out_certs(Issuer, NewCerts, Chain ++ [CertDer])
    end.

format_name({rdnSequence, List}) ->
    certLib:format_rdn({rdnSequence, lists:sort(List)});
format_name({directoryName,{rdnSequence, List}}) ->
    certLib:format_rdn({rdnSequence, lists:sort(List)}).


clean_list(List_to_clean) ->
    clean_list(List_to_clean, []).

clean_list([], List) ->
    List;
clean_list([{_,_,Cert}|T], List) ->
    clean_list(T, List ++ [Cert]).






%%% ----------------------------------------------------------
%%% INFO, WARNING and ERROR MESSAGE HANDLING
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

