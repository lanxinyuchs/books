%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certCrl.erl %
%%% @author emajkaa
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R9A/R10A/R11A/1
%%% 
%%% @doc ==Certficate revocation list handling==
%%% This module implements the certificate revocation list handling
-module(certCrl).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R9A/R10A/R11A/1').
-date('2017-10-06').
-author('emajkaa').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-08-08 etxjotj     Created
%%% R6A/2      2016-08-30 emariad     CSUC feature, cipher configuration
%%% R9A/1      2017-01-30 etomist     HV59927 
%%% R10A/1     2017-05-15 ebabmat     HV87843
%%% R10A/2     2017-05-24 emajkaa     HV90577
%%% R11A/1     2017-10-06 emajkaa     Improvement: added function dump_cached_crls
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([download_crl/0,
         get_crl/1,
         get_cached_crl/1,
         clean_crls/0,
         remove_cached_crl/1,
         remove_cached_crl_by_key/1,
         dump_cached_crls/1]).

%% Only for test
-export([install_crl_file/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsCertM.hrl").
-include("cert.hrl").
-include_lib("public_key.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

download_crl() ->
    StartTime = comsaI:iso_time(os:timestamp(), extended),
    certServer:update_progress([{additionalInfoClear, "Download all CRLs"},
            {progressPercentage, 0},
            {result, ?ActionResultType_NOT_AVAILABLE},
            {resultInfo, ""},
            {state, ?ActionStateType_RUNNING},
            {timeActionStarted, StartTime},
            {timeActionCompleted, StartTime}]),

    lists:foreach(
        fun(Path) ->
                case download(Path) of
                    {error, Reason} ->
                        info_msg("Download of CRL failed, reason:~w",[Reason]),
                        certLib:sec_log("",
                            "Download of CRL failed, " ++ Path),
                        certServer:update_progress(
                            [{resultInfo, "Failed to download crl: " ++Path}]),
                        ok;
                    Crl ->
                        certServer:update_progress(
                            [{resultInfo, "Downloaded CRL: " ++ Path}]),
                        [Obj] = mnesia:dirty_read(certCrl, Path),
                        ThisUpdate = get_this_update(),
                        NextUpdate = get_next_update(Crl),
                        NewObj =
                        Obj#certCrl{
                            crl = Crl,
                            this_update = ThisUpdate,
                            next_update = NextUpdate},
                        mnesia:dirty_write(NewObj)
                end
        end, mnesia:dirty_all_keys(certCrl)),

    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    certServer:update_progress([{result, ?ActionResultType_SUCCESS},
                                {state, ?ActionStateType_FINISHED},
                                {progressPercentage, 100},
                                {timeActionCompleted, CompleteTime}]),
    ok.


download(Url) ->
    %io:format("Url: ~p~n", [Url]),
    case wget(Url, 5000) of
        {ok, CrlDer} ->
            %% Make sure that this file is correct and not faulty
            case catch public_key:der_decode('CertificateList', CrlDer) of
                {'EXIT', Reason} ->
                    Msg = "Downloaded CRL file can not be decoded, " ++ Url,
                    info_msg(Msg, []),
                    certLib:sec_log("", Msg),
                    {error, Reason};
                _ ->
                    CrlDer
            end;
        {error, Reason} ->
            Msg = "Download of CRL file failed, " ++ Url,
            info_msg(Msg, []),
            certLib:sec_log("", Msg),
            {error, Reason}
    end.


wget(Url, Timeout) ->
    case certLib:resolve_uri(Url) of
        {ok, NewUrl} ->
            Method  = get,
            Request = {NewUrl, []},
            HTTPOptions = [
                {timeout, Timeout},
                {connect_timeout, Timeout},
                {ssl,[{verify,0}, {ciphers, comsaI:get_tls_cipher_suites()}]},
                {autoredirect, false}],
            Options = [{body_format, binary},{ipv6_host_with_brackets, true}],
            httpc:set_options([{ipfamily, certLib:get_inet()},
                    {socket_opts, ootI:get_all_oam_opt_list()}], cert),
            case catch httpc:request(Method, Request, HTTPOptions,
                    Options, cert) of
                {ok, {{_,200,_}, Body}} ->
                    {ok, Body};
                {ok, {{_,200,_}, _Header, Body}} ->
                    {ok, Body};
                {'EXIT', Reason} ->
                    info_msg("Reason: ~p~n", [Reason]),
                    {error, not_found};
                A ->
                    info_msg("A: ~p~n", [A]),
                    {error, not_found}
            end;
        {error, Reason} ->
            info_msg("Resolve URI ~p Failed, reason: ~p~n", [Url, Reason]),
            {error, not_found}
    end.



get_crl(Path) ->
    Fun =
    fun() ->
            case mnesia:read(certCrl, Path) of
                [] ->
                    case download(Path) of
                        {error, Reason} ->
                            %% FIXME what to do???
                            {error, Reason};
                        Crl ->
                            ThisUpdate = get_this_update(),
                            NextUpdate = get_next_update(Crl),
                            NewObj =
                            #certCrl{
                                path = Path,
                                crl = Crl,
                                this_update = ThisUpdate,
                                next_update = NextUpdate},
                            mnesia:write(NewObj),
                            Crl
                    end;
                [Obj] ->
                    case Obj#certCrl.next_update of
                        undefined ->
                            Obj#certCrl.crl;
                        NextUpdate ->
                            ThisUpdate = get_this_update(),
                            {D, T} =
                            calendar:time_difference(ThisUpdate, NextUpdate),
                            case D*86400 + calendar:time_to_seconds(T) of
                                TimeLeft when TimeLeft > 0 ->
                                    Obj#certCrl.crl;
                                _ ->
                                    case download(Path) of
                                        {error, _Reason} ->
                                            %% FIXME what to do???
                                            Obj#certCrl.crl;
                                        Crl ->
                                            NewNextUpdate =
                                            get_next_update(Crl),
                                            NewObj =
                                            Obj#certCrl{
                                                crl  = Crl,
                                                this_update = ThisUpdate,
                                                next_update = NewNextUpdate},
                                            mnesia:write(NewObj),
                                            Crl
                                    end
                            end
                    end
            end
    end,
    {atomic, Crl} = mnesia:transaction(Fun),
    Crl.

get_cached_crl(Path) ->
    Fun =
    fun() ->
            case mnesia:read(certCrl, Path) of
                [] ->
                    crl_missing;
                [Obj] ->
                    case Obj#certCrl.next_update of
                        undefined ->
                            Obj#certCrl.crl;
                        NextUpdate ->
                            ThisUpdate = get_this_update(),
                            {D, T} =
                            calendar:time_difference(ThisUpdate, NextUpdate),
                            case D*86400 + calendar:time_to_seconds(T) of
                                TimeLeft when TimeLeft > 0 ->
                                    Obj#certCrl.crl;
                                _ ->
                                    crl_missing
                            end
                    end
            end
    end,
    {atomic, Crl} = mnesia:transaction(Fun),     
    Crl.

dump_cached_crls(_Args) ->
    Dump = mnesia:dirty_match_object({certCrl,'_','_','_','_'}),
    io:format("~p~n", [Dump]).

remove_cached_crl_by_key(DbKey) ->
    [TC] = mnesia:dirty_read(certTC, DbKey),
    case TC#certTC.cert of
        undefined -> 
            ok;
        _ ->
            Cert = public_key:pkix_decode_cert(TC#certTC.cert, otp),

            DeletePathFun =
            fun(URL) ->
                case URL of
                    {uniformResourceIdentifier, Path} ->
                        remove_cached_crl(Path);
                    _ ->
                        ok
                end
            end,

            DeleteCachedURLs =
            fun(DP) ->
                case DP#'DistributionPoint'.distributionPoint of
                    {fullName, URLs} ->
                        lists:foreach(DeletePathFun, URLs);
                    _ ->
                        ok
                end
            end,

            case public_key:pkix_dist_points(Cert) of 
                [] ->
                    ok;
                DPs ->
                    lists:foreach(DeleteCachedURLs, DPs)
            end,
            ok
    end.

remove_cached_crl(Path) ->
    mnesia:dirty_delete(certCrl, Path).

get_this_update() ->
    calendar:universal_time().

get_next_update(CrlDer) ->
    Crl = public_key:der_decode('CertificateList', CrlDer),
    TBS = Crl#'CertificateList'.tbsCertList,
    case TBS#'TBSCertList'.nextUpdate of
        {utcTime, UTC} ->
            certLib:convert_cert_date({utcTime, UTC});
        {generalTime, UTC} ->
            certLib:convert_cert_date({generalTime, UTC});
        _ ->
            undefined
    end.


clean_crls() ->
    info_msg("Removing old CRLs...if any", []),
    clean_crls(mnesia:dirty_first(certCrl)).

clean_crls('$end_of_table') ->
    ok;
clean_crls(Index) ->
    Next = mnesia:dirty_next(certCrl, Index),
    case mnesia:dirty_read(certCrl, Index) of
        [] ->
            ok;
        [Obj] ->
            case Obj#certCrl.next_update of
                undefined ->
                    mnesia:dirty_delete(certCrl, Index);
                NextUpdate ->
                    ThisUpdate = get_this_update(),
                    {D, T} = calendar:time_difference(ThisUpdate, NextUpdate),
                    case D*86400 + calendar:time_to_seconds(T) of
                        TimeLeft when TimeLeft > 0 ->
                            ok;
                        _ ->
                            mnesia:dirty_delete(certCrl, Index),
                            info_msg("Old crl removed, ~p~n", [Index]),
                            certLib:sec_log("",
                                "Old CRL removed, " ++ Index)
                    end
            end
    end,
    clean_crls(Next).

%% Only used for test
install_crl_file(File, Path) ->
    info_msg("Test function, install crl file, ~p", [File]),
    case file:read_file(File) of
        {ok, CrlDer} ->
            %% Make sure that this file is correct and not faulty
            case catch public_key:der_decode('CertificateList', CrlDer) of
                {'EXIT', Reason} ->
                    {error, Reason};
                _ ->
                    ThisUpdate = get_this_update(),
                    NextUpdate = get_next_update(CrlDer),
                    NewObj =
                    #certCrl{
                        path = Path,
                        crl = CrlDer,
                        this_update = ThisUpdate,
                        next_update = NextUpdate},
                    mnesia:dirty_write(NewObj)
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).




%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

