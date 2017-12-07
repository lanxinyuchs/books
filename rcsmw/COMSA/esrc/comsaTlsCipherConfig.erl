%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaTlsCipherConfig.erl %
%%% @author emariad
%%% @copyright Ericsson AB 2016
%%% @version /main/R6A/3
%%%
%%% @doc ==A implementation for TLS cipher configuration==
%%% This is the implementation for TLS cipher filter and configuration


-module(comsaTlsCipherConfig).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R1A/1      2016-05-13 emariad     Created
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_enabled_ciphers/2]).
-export([convert_ciphers_to_otp_ssl_format/1]).
-export([convert_ciphers_from_otp_ssl_format/1]).

-include("RcsSecM.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Description: Generate enabled ciphers
%%% ----------------------------------------------------------
generate_enabled_ciphers(Filter, SupportedCiphers)->
    FilterToList = string:tokens(Filter,  ":"),
    SortedFilter = sort_filter(FilterToList),
%%     check_and_verify_DEFAULT_keyword_ok(SortedFilter),    
    
    case verify_positive_filter(SortedFilter) of 
        true ->
            Ciphers = do_generate(SortedFilter,
                                  SupportedCiphers),
            Ciphers;
        false ->
            {error, "Invalid value for attribute 'cipherFilter'.
No positive cipher string."}
    end.

%%% ----------------------------------------------------------
%%% Description: Convert to Tls cipher struct format from the 
%%%              OTP SSL cipher suite format.
%%% ----------------------------------------------------------
convert_ciphers_from_otp_ssl_format(Cipher_suite) when size(Cipher_suite) == 3 ->
    Kx_A = string:to_upper(atom_to_list(element(1, Cipher_suite))),
    Encryption = string:to_upper(atom_to_list(element(2, Cipher_suite))),
    EncryptionFormat = string:join(string:tokens(Encryption, "_"), "-"),
    Hash = string:to_upper(atom_to_list(element(3, Cipher_suite))),
    case string:str(Kx_A, "_") of
        0 ->
            KeyExchange = Kx_A,
            Authentication = Kx_A; 
        _ ->
            Nr = string:str(Kx_A, "_"),
            KeyExchange = string:substr(Kx_A, 1, Nr-1),
            Authentication = string:substr(Kx_A, Nr+1) 
    end,
    case KeyExchange =:= Authentication of
        true ->
            Name = string:join(["TLS",
                                KeyExchange,
                                "WITH",
                                Encryption,
                                Hash],
                                "_");
        false ->
            Name = string:join(["TLS",
                                KeyExchange,
                                Authentication,
                                "WITH",
                                Encryption,
                                Hash],
                                "_")
    end,
    #'Cipher'{protocolVersion="TLS",
              keyExchange=      KeyExchange,
              authentication=   Authentication,
              encryption=       EncryptionFormat,
              mac=              Hash,
              export=           "",
              name=             Name};
convert_ciphers_from_otp_ssl_format(Cipher_suite) when size(Cipher_suite) == 4 ->
    Kx_A = string:to_upper(atom_to_list(element(1, Cipher_suite))),
    Encryption = string:to_upper(atom_to_list(element(2, Cipher_suite))),
    EncryptionFormat = string:join(string:tokens(Encryption, "_"), "-"),
    Mac = string:to_upper(atom_to_list(element(3, Cipher_suite))),
    Prf = string:to_upper(atom_to_list(element(4, Cipher_suite))),
    Hash = Mac ++"-"++ Prf,
    case string:str(Kx_A, "_") of
        0 ->
            KeyExchange = Kx_A,
            Authentication = Kx_A;
        _ ->
            Nr = string:str(Kx_A, "_"),
            KeyExchange = string:substr(Kx_A, 1, Nr-1),
            Authentication = string:substr(Kx_A, Nr+1)
    end,
    case KeyExchange =:= Authentication of
        true ->
            Name = string:join(["TLS",
                                KeyExchange,
                                "WITH",
                                Encryption,
                                Hash],
                                "_");
        false ->
            Name = string:join(["TLS",
                                KeyExchange,
                                Authentication,
                                "WITH",
                                Encryption,
                                Hash],
                                "_")
    end,
    #'Cipher'{protocolVersion="TLS",
              keyExchange=      KeyExchange,
              authentication=   Authentication,
              encryption=       EncryptionFormat,
              mac=              Hash,
              export=           "",
              name=             Name};
convert_ciphers_from_otp_ssl_format(_Cipher_suite) ->
    sysInitI:info_msg("~n No supported cipher suite size ").

%%% ----------------------------------------------------------
%%% Description: Convert to OTP SSL cipher suite format from the 
%%%              Tls cipher struct format.
%%% ----------------------------------------------------------
convert_ciphers_to_otp_ssl_format(CipherList) ->
    convert_ciphers_to_otp_ssl_format(CipherList, []).
convert_ciphers_to_otp_ssl_format([H | T], CipherOtpTlsFormat)
  when size(H) == 5 ->
    KeyExchange_Authentication =
        merge_keyexchange_authentication(element(1, H), element(2, H)),
    Cipher = {KeyExchange_Authentication,
              list_to_atom(string:to_lower(element(3, H))),
              list_to_atom(string:to_lower(element(4, H))),
              list_to_atom(string:to_lower(element(5, H)))},
    convert_ciphers_to_otp_ssl_format(T, CipherOtpTlsFormat ++ [Cipher]);
convert_ciphers_to_otp_ssl_format([H | T], CipherOtpTlsFormat)
  when size(H) == 4 ->
    KeyExchange_Authentication =
        merge_keyexchange_authentication(element(1, H), element(2, H)),
    Cipher = {KeyExchange_Authentication,
              list_to_atom(string:to_lower(element(3, H))),
              list_to_atom(string:to_lower(element(4, H)))},
    convert_ciphers_to_otp_ssl_format(T, CipherOtpTlsFormat ++ [Cipher]);
convert_ciphers_to_otp_ssl_format([H | _], _) ->
    sysInitI:info_msg("Can not format this size ~p" ,[size(H)]);
convert_ciphers_to_otp_ssl_format([], CipherOtpTlsFormat)->
    CipherOtpTlsFormat.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Description: Generate enabled ciphers by applying filter on 
%%%              supported cipher suites.
%%% ----------------------------------------------------------
do_generate(Filter, SupportedCipherSuites) ->
    case check_and_verify_keyword_DEFAULT_ok(Filter) of
        true ->
            do_generate(Filter, [],  SupportedCipherSuites);
        false ->
            {error, "'DEFAULT' cannot be used together with other cipher algorithms."}
     end.

do_generate([H | _], _, SupportedCipherSuites) when H =:= "DEFAULT" ->
            SupportedCipherSuites;
do_generate([H | T], _, SupportedCipherSuites)  when H =:= "ALL" ->
    do_generate(T, SupportedCipherSuites, SupportedCipherSuites);
do_generate([H | T], FilteredCipherSuites,  SupportedCipherSuites) ->
    case is_positive(H) of
        true ->
            CipherSuites = apply_positive_filter(H, SupportedCipherSuites),
            UniqueCipherSuites = remove_duplicates(FilteredCipherSuites, CipherSuites),
            do_generate(T,
                                 FilteredCipherSuites ++ UniqueCipherSuites,
                                 SupportedCipherSuites);
        false ->
            CipherSuites = apply_negative_filter(H, FilteredCipherSuites),
            do_generate(T,
                                 CipherSuites,
                                 SupportedCipherSuites)
    end;
do_generate([], FilteredCipherSuites,  _) ->
    case FilteredCipherSuites  of
        [] ->
            {error, "Invalid value for attribute 'cipherFilter'. No ciphers selected after applying the filter"};
        _ ->
            FilteredCipherSuites
    end.

apply_negative_filter([_ | T], TmpCipherList) ->
    case string:str(T, "+") of
        0 ->
            exclude_ciphers(T, TmpCipherList);
        _ ->
            exclude_combined_ciphers(string:tokens(T, "+"), TmpCipherList)
    end.

apply_positive_filter(Filter, SupportedCipherList) ->
    case string:str(Filter, "+") of
        0 ->
            include_ciphers(Filter, SupportedCipherList);
        _ ->
            include_combined_ciphers(string:tokens(Filter, "+"),
                                     SupportedCipherList)
    end.

%%% ----------------------------------------------------------
%%% Description: Exclude cipher functions
%%% ----------------------------------------------------------
exclude_combined_ciphers(CombinedFilter, Ciphers )->
    %%generate list of Ciphers that contain all parts of combined filter
     CiphersToRemove=include_combined_ciphers(CombinedFilter, Ciphers),
     Ciphers--CiphersToRemove.

exclude_ciphers([H | T], TmpCipherList) when  [H] =:= "a" -> 
    Ciphers = lists:filter(fun({_,_,_,X,_,_,_,_})when X =:= T -> false;
                              (_) -> true end, TmpCipherList),
    Ciphers;
exclude_ciphers([H | T], TmpCipherList) when  [H] =:= "k" ->
    Ciphers = lists:filter(fun({_,_,X,_,_,_,_,_}) when X =:= T -> false;
                              (_) -> true end, TmpCipherList),
    Ciphers;
exclude_ciphers(Filter, TmpCipherList) ->	
    List1 = lists:filter(fun({_,_,X,_,_,_,_,_}) when X =:= Filter -> false;
                            (_) -> true end, TmpCipherList),
    List2 = lists:filter(fun({_,_,_,X,_,_,_,_}) when X =:= Filter -> false;
                            (_) -> true end, List1),
    List3 = lists:filter(fun({_,_,_,_,X,_,_,_}) when X =:= Filter -> false;
                            (_) -> true end, List2),
    
    %Need to handle that encryption ciphers can have several algorithms and 
    %it is possible to filter on each algorithm.
    List4 = exclude_encryption_ciphers(Filter, List3),
    
    List5 = lists:filter(fun({_,_,_,_,_,X,_,_}) when X =:= Filter -> false;
                            (_) -> true end, List4),
    
    %Need to handle that a filter can be a whole cipher suite, 
    %ex ECDH-RSA-AES-256-GCM-SHA256
    case string:str(Filter, "-") of
        0 ->
            List5;
        _ ->
            Ciphers = exclude_whole_cipher(Filter, List5),
            Ciphers
    end.

exclude_encryption_ciphers(Filter, TmpCipherList) ->
    exclude_encryption_ciphers(Filter, TmpCipherList, []).
exclude_encryption_ciphers(Filter,[H | T], NewCipherList)->
    ExtractedEncryptionCiphers = string:tokens(element(5, H), "-"),
    case is_encryption_matching_filter(Filter, ExtractedEncryptionCiphers) of
        true ->
            exclude_encryption_ciphers(Filter, T, NewCipherList);
        false ->
            exclude_encryption_ciphers(Filter, T, NewCipherList ++ [H])
    end;
exclude_encryption_ciphers(_, [], NewCipherList)-> NewCipherList.

exclude_whole_cipher(Filter, TmpCipherList) ->
    exclude_whole_cipher(Filter, TmpCipherList, []).
exclude_whole_cipher(Filter, [H | T], NewCipherList) ->
    Cipher = H#'Cipher'.keyExchange ++"-"++
             H#'Cipher'.authentication ++"-"++
             H#'Cipher'.encryption ++"-"++
             H#'Cipher'.mac,
    case Cipher =:= Filter of
        true ->
            exclude_whole_cipher(Filter, T, NewCipherList);
        false ->
            exclude_whole_cipher(Filter, T, NewCipherList ++ [H])
    end;
exclude_whole_cipher(_, [], NewCipherList)-> NewCipherList.

%%% ----------------------------------------------------------
%%% Description: Include cipher functions
%%% ----------------------------------------------------------
include_combined_ciphers([H | T], SupportedCipherList)->
    NewCipherList = include_ciphers(H, SupportedCipherList),
    include_combined_ciphers(T, NewCipherList);
include_combined_ciphers([], NewCipherList) -> NewCipherList.

include_ciphers([H | T], SupportedCipherList) when [H] =:= "a" ->
    Ciphers = lists:filter(fun({_,_,_,X,_,_,_,_}) when X =:= T -> true;
                              (_) -> false end,	SupportedCipherList),
    Ciphers;
include_ciphers([H | T], SupportedCipherList) when [H] =:= "k" ->
    Ciphers = lists:filter(fun({_,_,X,_,_,_,_,_}) when X =:= T -> true;
                              (_) -> false end, SupportedCipherList),
    Ciphers;
include_ciphers(Filter, SupportedCipherList) ->
    List1 = lists:filter(fun({_,_,X,_,_,_,_,_}) when X =:= Filter -> true;
                            (_) -> false end, SupportedCipherList),
    List2 = lists:filter(fun({_,_,_,X,_,_,_,_}) when X =:= Filter -> true;
                            (_) -> false end,	SupportedCipherList),
    List3 = lists:filter(fun({_,_,_,_,X,_,_,_}) when X =:= Filter -> true;
                            (_) -> false end,	SupportedCipherList),
    
    %Need to handle that encryption ciphers can have several algorithms and 
    %it is possible to filter on each algorithm.
    List4 = include_encryption_ciphers(Filter, SupportedCipherList),
    
    List5 = lists:filter(fun({_,_,_,_,_,X,_,_}) when X =:= Filter -> true;
                            (_) -> false end, SupportedCipherList),
    
    %Need to handle that a filter can be a whole cipher suite, 
    %ex ECDH-RSA-AES-256-GCM-SHA256
    List6 = include_whole_cipher(Filter, SupportedCipherList),
    
    RemovedDuplicates1 = remove_duplicates(List1, List2),
    RemovedDuplicates2 = remove_duplicates(List3, List4),
    Ciphers = List1 ++
              RemovedDuplicates1 ++
              List3 ++
              RemovedDuplicates2 ++
              List5 ++
              List6,
    Ciphers.

include_encryption_ciphers(Filter, TmpCipherList) ->
    include_encryption_ciphers(Filter, TmpCipherList, []).
include_encryption_ciphers(Filter,[H | T], NewCipherList)->
    ExtractedEncryptionCiphers = string:tokens(element(5, H), "-"),
    case is_encryption_matching_filter(Filter, ExtractedEncryptionCiphers) of
        true ->
            include_encryption_ciphers(Filter, T, NewCipherList ++ [H]);
        false ->
            include_encryption_ciphers(Filter, T, NewCipherList)
    end;
include_encryption_ciphers(_, [], NewCipherList)-> NewCipherList.

include_whole_cipher(Filter, SupportedCipherList) ->
    include_whole_cipher(Filter, SupportedCipherList, []).
include_whole_cipher(Filter, [H | T], NewCipherList) ->
    Cipher = H#'Cipher'.keyExchange ++"-"++
             H#'Cipher'.authentication ++"-"++
             H#'Cipher'.encryption ++"-"++
             H#'Cipher'.mac,
    case Cipher =:= Filter of
        true ->
            include_whole_cipher(Filter, T, NewCipherList ++ [H]);
        false ->
            include_whole_cipher(Filter, T, NewCipherList)
    end;
include_whole_cipher(_, [], NewCipherList)-> NewCipherList.

%%% ----------------------------------------------------------
%%% INTERNAL HELP FUNCTIONS
%%% ----------------------------------------------------------

%%Sort filter so "!" is last, easier to filter on ciphers.
sort_filter(Filter)->
    sort_filter(Filter, [], []).
sort_filter([H | T], Sorted, AddToLast)->
    case string:str(H, "!") of
        0 ->
            sort_filter(T, Sorted ++ [H], AddToLast);
        _ ->
            sort_filter(T, Sorted, AddToLast ++ [H])
    end;
sort_filter([], Sorted, AddToLast)->
    Sorted ++ AddToLast.

%%Check if keyword DEFAULT is used and verify that it is used correctly.
%%That is:
%%DEFAULT can't be used as an exclusive filter 
%%DEFAULT can't be used with other cipher filter strings
%%DEFAULT can only be used once in one filter
check_and_verify_keyword_DEFAULT_ok(Filter)->
    ConcatFilterToString = lists:concat(Filter),
    case string:str(ConcatFilterToString, "DEFAULT") of
        0 ->
            true;
        _ ->
            case string:len(ConcatFilterToString) of
                7->
                    true;
                _->
                    false
            end
    end.

%%Verify filter OK.
verify_positive_filter([H | _]) when H =:= "DEFAULT" -> true;
verify_positive_filter([H | _]) when H =:= "ALL" -> true;
verify_positive_filter([H | T]) ->    
    case is_positive(H) of
         true ->
             true;
         false ->
            verify_positive_filter(T)
     end;
verify_positive_filter([])-> false.

is_positive([H | _]) when [H] =:= "!" -> false;
is_positive([H | _]) when [H] =:= "-" -> false;
is_positive([_ |_]) -> true.

%% Merge key exchange and atuhentication
merge_keyexchange_authentication(KeyExchange, Authentication) ->
    case KeyExchange =:= Authentication of
        true ->
            Kex = KeyExchange,
            list_to_atom(string:to_lower(Kex));
        false ->
            KX_Au = string:to_lower(KeyExchange ++ "_" ++ Authentication),
            list_to_atom(KX_Au)
    end.

%%Check if encrytption matching filter
is_encryption_matching_filter(Filter, [H | _]) when Filter =:= H -> true;
is_encryption_matching_filter(Filter, [_ | T]) ->
    is_encryption_matching_filter(Filter, T);
is_encryption_matching_filter(_, []) -> false.

remove_duplicates(List1, List2)->
    lists:filter(fun(X) -> not lists:member(X, List1) end, List2).
