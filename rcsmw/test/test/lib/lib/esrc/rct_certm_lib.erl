%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_certm_lib.erl %
%%% Author: 
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(rct_certm_lib).
-vsn('/main/R10A/R11A/1').
-author('ekurnik').

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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R10A/1   2017-06-12   ekurnik      Created
%%% R10A/2   2017-06-14   ekurnik      Moved parts of code to mo handler
%%% R11A/1   2017-08-03   ekurnik      Added cleanup function
%%%--------------------------------------------------------------------

%% main interfaces functions
-export([get_node_credential_dn/1, 
         get_trusted_certificate_dn/1,
         get_trust_category_dn/1,
         
         create_node_credential/2,
         create_node_credential/3,
         install_node_credential_from_uri/6,
         delete_node_credential/2,
         
         install_trusted_certificate_from_uri/4,
         set_trusted_certificate_managed_state/3,
         get_all_trusted_certificates/1,
         delete_trusted_certificate/2,
         
         create_trust_category/3,
         get_trust_category_trusted_certs/2,
         set_trust_category_trusted_certs/3,
         delete_trust_category/2]).

-export([clear_certm_config/1]).

%% for test:
-export([suite/0, all/0, test/1]).

-define(CERTM_DN, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1").

-define(NODE_CREDENTIAL_DN(NcId), ?CERTM_DN ++ ",NodeCredential=" ++ NcId).
-define(TRUSTED_CERTIFICATE_DN(TcId), ?CERTM_DN ++ ",TrustedCertificate=" ++ TcId).
-define(TRUST_CATEGORY_DN(TcatId), ?CERTM_DN ++ ",TrustCategory=" ++ TcatId).

suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
     {rct_netconf,{nc1, html}},
     {cth_conn_log,[]},
     {rct_core,[]},
     {rct_logging,
         {all,
             [{erlang,
                     {["ERROR REPORT", "CRASH REPORT"], 
                      []}}]}}
    ]}].

all() ->
    [test].

%% Used for testing the library
test(_Config) ->
    
    Handler = {netconf, nc1},
    
    ct:pal("NC: ~p~n", [get_node_credential_dn("snmp_cert")]),
    ct:pal("TC: ~p~n", [get_trusted_certificate_dn("1")]),
    ct:pal("Tcat: ~p~n", [get_trust_category_dn("Tcat_1")]),
    
    ok = create_node_credential(Handler, "snmp_cert"),
    
    ok = install_node_credential_from_uri(Handler, "snmp_cert", 
                                     "sftp://labuser@10.68.101.131/home/labuser/FTP_TLS_SETUP/tls_nodecert_pkcs12",
                                     "labuser", "idiocy", "17:8b:19:ef:57:e1:12:62:67:33:f5:bd:bd:8c:28:8e:bd:4b:c2:ce"),
    
    {ok, TcId} = install_trusted_certificate_from_uri(Handler, 
                                         "sftp://labuser@10.68.101.131/home/labuser/FTP_TLS_SETUP/user-ca.crt", 
                                         "labuser", "DB:59:94:FB:BE:E6:1B:83:D4:77:88:BF:F8:27:9B:B9:BC:A0:5D:23"),
    ct:pal("TcId: ~p~n", [TcId]),
    
    ok = set_trusted_certificate_managed_state(Handler, TcId, disabled),
    ok = set_trusted_certificate_managed_state(Handler, TcId, enabled),
    
    {ok, TrustedCertIds} =  get_all_trusted_certificates(Handler),
    ct:pal("TrustedCerts: ~p~n", [TrustedCertIds]),
    
    ok = create_trust_category(Handler, "tcat_1", TrustedCertIds),

    ct:pal("TrustedCerts: ~p~n", [get_trust_category_trusted_certs(Handler, "tcat_1")]),

    ok = set_trust_category_trusted_certs(Handler, "tcat_1", [TcId]),

    {ok, [TcId]} = get_trust_category_trusted_certs(Handler, "tcat_1"),
    
    ok = rct_mo_handler_lib:set_mo(Handler, "ManagedElement=1,SystemFunctions=1,SysM=1,FileTPM=1,FtpTls=1", 
                                   [{trustCategory, get_trust_category_dn("tcat_1")}]),
    
    ok = rct_mo_handler_lib:set_mo(Handler, "ManagedElement=1,SystemFunctions=1,SysM=1,NetconfTls=1", 
                                   [{nodeCredential, get_node_credential_dn("snmp_cert")}]),
    ok = rct_mo_handler_lib:set_mo(Handler, "ManagedElement=1,SystemFunctions=1,SysM=1,NetconfTls=1", 
                                   [{trustCategory, get_trust_category_dn("tcat_1")}]),
    
    ok = clear_certm_config(Handler),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns NodeCredential MO DN based on Id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_node_credential_dn(NcId :: string()) -> string().
get_node_credential_dn(NcId) ->
    ?NODE_CREDENTIAL_DN(NcId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns TrustedCertificate MO DN based on Id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_trusted_certificate_dn(TcId :: string()) -> string().
get_trusted_certificate_dn(TcId) ->
    ?TRUSTED_CERTIFICATE_DN(TcId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns TrustCategory MO DN based on Id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_trust_category_dn(TcatId :: string()) -> string().
get_trust_category_dn(TcatId) ->
    ?TRUST_CATEGORY_DN(TcatId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates NodeCredential with NcId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_node_credential(Handler :: rct_mo_handler_lib:handler(), 
                             NcId :: string()) -> ok | {error, term()}.
create_node_credential(Handler, NcId) ->
    create_node_credential(Handler, NcId, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates NodeCredential with NcId and initial attributes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_node_credential(Handler :: rct_mo_handler_lib:handler(), 
                             NcId :: string(), 
                             Attrs :: [{atom(), term()}] | map()) -> 
                                ok | {error, term()}.
create_node_credential(Handler, NcId, Attrs) ->
    OperResult = rct_mo_handler_lib:create_mo(Handler, ?NODE_CREDENTIAL_DN(NcId), Attrs),
    format_crud_operation(create, ?NODE_CREDENTIAL_DN(NcId), OperResult).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Invokes installCredentialFromUri action on existing NodeCredential MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec install_node_credential_from_uri(Handler :: rct_mo_handler_lib:handler(),
                                       NcId :: string(), 
                                       Uri :: string(), 
                                       UriPassword :: string(), 
                                       CredentialPassword :: string(),
                                       Fingerprint :: string() | undefined) -> 
                                            ok | {error, term()}.
install_node_credential_from_uri(Handler, NcId, Uri, UriPassword, CredentialPassword, Fingerprint) ->
    Params = [{uri, Uri}, {uriPassword, UriPassword}, 
              {credentialPassword, CredentialPassword}, 
              {fingerprint, Fingerprint}],
    %% filter out undefined
    FilteredParams = lists:filter(fun({_, undefined}) -> false; 
                                     (_) -> true end, Params),
    ActionResult = rct_mo_handler_lib:async_action_mo(Handler, 
                                            ?NODE_CREDENTIAL_DN(NcId), 
                                            installCredentialFromUri,
                                            FilteredParams,
                                            enrollmentProgress),
    {ok, [{enrollmentProgress, ProgressReport}]} = rct_mo_handler_lib:get_mo(Handler, ?NODE_CREDENTIAL_DN(NcId), enrollmentProgress),
    format_action_result(installCredentialFromUri, ?NODE_CREDENTIAL_DN(NcId), ActionResult, ProgressReport).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Deletes NodeCredential with NcId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec delete_node_credential(Handler :: rct_mo_handler_lib:handler(), 
                             NcId :: string()) -> ok | {error, term()}.
delete_node_credential(Handler, NcId) ->
    OperResult = rct_mo_handler_lib:delete_mo(Handler, ?NODE_CREDENTIAL_DN(NcId)),
    format_crud_operation(delete, ?NODE_CREDENTIAL_DN(NcId), OperResult).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Invokes installTrustedCertFromUri action on CertM MO and returns 
%%      TcId of created TrustedCertificate MO if successful
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec install_trusted_certificate_from_uri(Handler :: rct_mo_handler_lib:handler(),
                                           Uri :: string(), 
                                           UriPassword :: string(), 
                                           Fingerprint :: string() | undefined) -> 
                                            {ok, TcId :: string()} | {error, term()}.
install_trusted_certificate_from_uri(Handler, Uri, UriPassword, Fingerprint) ->
    Params = [{uri, Uri}, {uriPassword, UriPassword}, {fingerprint, Fingerprint}],
    %% filter out undefined
    FilteredParams = lists:filter(fun({_, undefined}) -> false; 
                                     (_) -> true end, Params),
    ActionResult = rct_mo_handler_lib:async_action_mo(Handler, 
                                            ?CERTM_DN,
                                            installTrustedCertFromUri,
                                            FilteredParams),
    {ok, [{reportProgress, ProgressReport}]} = rct_mo_handler_lib:get_mo(Handler, ?CERTM_DN, reportProgress),
    case format_action_result(installTrustedCertFromUri, ?CERTM_DN, ActionResult, ProgressReport) of
        ok ->
            {ok, get_trusted_cert_id_from_progress(ProgressReport)};
        Other ->
            Other
    end.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sets managedState ENABLED/DISABLED for TrustedCertificate MO 
%%      (will be ENABLED after creation)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_trusted_certificate_managed_state(Handler :: rct_mo_handler_lib:handler(),
                                            TcId :: string(), 
                                            ManagedState :: enabled | disabled) -> 
                                             ok | {error, term()}.
set_trusted_certificate_managed_state(Handler, TcId, ManagedState) ->
    
    Attrs = [{managedState, managed_state_to_string(ManagedState)}],
    OperResult = rct_mo_handler_lib:set_mo(Handler, ?TRUSTED_CERTIFICATE_DN(TcId), Attrs),
    format_crud_operation(set, ?TRUSTED_CERTIFICATE_DN(TcId), OperResult).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets IDs of all TrustedCertificate MOs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_all_trusted_certificates(Handler :: rct_mo_handler_lib:handler()) -> 
                                      {ok, TcIds :: [string()]} | {error, term()}.
get_all_trusted_certificates(Handler) ->
    case rct_mo_handler_lib:get_children_mo(Handler, ?CERTM_DN, "TrustedCertificate") of
        {ok, TcDns} ->
            TcIds = lists:map(fun(TcDn) -> rct_mo_handler_lib:get_mo_id(TcDn) end, TcDns),
            {ok, TcIds};
        Other ->
            Other
    end.
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Invokes removeTrustedCert action on CertM MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec delete_trusted_certificate(Handler :: rct_mo_handler_lib:handler(),
                                 TcId :: string()) -> ok | {error, term()}.
delete_trusted_certificate(Handler, TcId) ->
    Params = [{trustedCert, ?TRUSTED_CERTIFICATE_DN(TcId)}],
    ActionResult = rct_mo_handler_lib:async_action_mo(Handler, 
                                            ?CERTM_DN,
                                            removeTrustedCert,
                                            Params),
    {ok, [{reportProgress, ProgressReport}]} = rct_mo_handler_lib:get_mo(Handler, ?CERTM_DN, reportProgress),
    format_action_result(removeTrustedCert, ?CERTM_DN, ActionResult, ProgressReport).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates TrustCategory with initial list of TrustedCertificates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_trust_category(Handler :: rct_mo_handler_lib:handler(), 
                            TcatId :: string(), TcIds :: [string()]) -> 
                                ok | {error, term()}.
create_trust_category(Handler, TcatId, TcIds) ->
    Attrs = [{trustedCertificates, lists:map(fun(TcId) -> ?TRUSTED_CERTIFICATE_DN(TcId) end, TcIds)}],
    OperResult = rct_mo_handler_lib:create_mo(Handler, ?TRUST_CATEGORY_DN(TcatId), Attrs),
    format_crud_operation(create, ?TRUST_CATEGORY_DN(TcatId), OperResult).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets IDs of all TrustedCertificate MOs in trustedCertificates list of TrustCategory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_trust_category_trusted_certs(Handler :: rct_mo_handler_lib:handler(), TcatId :: string()) -> 
                                        {ok, [string()]} | {error, term()}.
get_trust_category_trusted_certs(Handler, TcatId) ->
    case rct_mo_handler_lib:get_mo(Handler, ?TRUST_CATEGORY_DN(TcatId), [trustedCertificates]) of
        {ok, [{trustedCertificates, [L | _] = TrustedCert}]} when is_integer(L) -> %% single TrustCert
            {ok, lists:map(fun(TcDn) -> rct_mo_handler_lib:get_mo_id(TcDn) end, [TrustedCert])};
        {ok, [{trustedCertificates, TrustedCerts}]} -> %% list of trusted certs
            {ok, lists:map(fun(TcDn) -> rct_mo_handler_lib:get_mo_id(TcDn) end, TrustedCerts)};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sets a new list of TrustedCertificates in TrustCategory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_trust_category_trusted_certs(Handler :: rct_mo_handler_lib:handler(), 
                                       TcatId :: string(), TcIds :: [string()]) -> 
                                        ok | {error, term()}.
set_trust_category_trusted_certs(Handler, TcatId, TcIds) ->
    Attrs = [{trustedCertificates, lists:map(fun(TcId) -> ?TRUSTED_CERTIFICATE_DN(TcId) end, TcIds)}],
    OperResult = rct_mo_handler_lib:set_mo(Handler, ?TRUST_CATEGORY_DN(TcatId), Attrs),
    format_crud_operation(set, ?TRUST_CATEGORY_DN(TcatId), OperResult).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Deletes TrustCategory with TcatId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec delete_trust_category(Handler :: rct_mo_handler_lib:handler(), TcatId :: string()) -> 
                                ok | {error, term()}.
delete_trust_category(Handler, TcatId) ->
    OperResult = rct_mo_handler_lib:delete_mo(Handler, ?TRUST_CATEGORY_DN(TcatId)),
    format_crud_operation(delete, ?TRUST_CATEGORY_DN(TcatId), OperResult).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Function which clears CertM MO config
%%      Current implementation removes:
%%          - NodeCredential MOs
%%          - TrustCategory MOs
%%          - TrustedCertificate MOs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec clear_certm_config(Handler :: rct_mo_handler_lib:handler()) -> ok | {error, term()}.
clear_certm_config(Handler) ->
    try
        %% NodeCredential
        {ok, NcDNs} = rct_mo_handler_lib:get_children_mo(Handler, ?CERTM_DN, "NodeCredential"),
        NcIds = lists:map(fun(NcDn) -> rct_mo_handler_lib:get_mo_id(NcDn) end, NcDNs),
        
        deref_node_credentials(Handler, NcDNs),
        lists:foreach(fun(NcId) -> delete_node_credential(Handler, NcId) end, NcIds),
        
        %% TrustCategory
        {ok, TcatDNs} = rct_mo_handler_lib:get_children_mo(Handler, ?CERTM_DN, "TrustCategory"),
        TcatIds = lists:map(fun(TcatDn) -> rct_mo_handler_lib:get_mo_id(TcatDn) end, TcatDNs),
        
        deref_trust_categories(Handler, TcatDNs),
        lists:foreach(fun(TcatId) -> delete_trust_category(Handler, TcatId) end, TcatIds),
        
        %% TrustedCertificates
        {ok, TcIds} = get_all_trusted_certificates(Handler),
        lists:foreach(fun(TcId) -> delete_trusted_certificate(Handler, TcId) end, TcIds),
        
        ok
    catch A:B ->
        ct:pal("~p:~p~n", [A, B]),
        {error, "Failed to clean CertM"}
    end.
    

deref_node_credentials(Handler, DNs) ->
    lists:foreach(fun(DN) -> deref_mo(Handler, DN, reservedByUser, nodeCredential) end, DNs).

deref_trust_categories(Handler, DNs) ->
    lists:foreach(fun(DN) -> deref_mo(Handler, DN, reservedByUser, trustCategory) end, DNs).

deref_mo(Handler, DN, ReservedBy, RefAttr) ->
    %% Get all MOs which reserve (reference) DN
    {ok, [{ReservedBy, RefDNs}]} = rct_mo_handler_lib:get_mo(Handler, DN, [ReservedBy]),
    %% Set reference attribute to undefined (remove reference)
    case RefDNs of 
        [OneRefDN | _] when is_list(OneRefDN) ->
            lists:foreach(fun(RefDN) -> ok = rct_mo_handler_lib:set_mo(Handler, RefDN, [{RefAttr, undefined}]) end, RefDNs);
        RefDNs when is_list(RefDNs) ->
            lists:foreach(fun(RefDN) -> ok = rct_mo_handler_lib:set_mo(Handler, RefDN, [{RefAttr, undefined}]) end, [RefDNs]);
        _ ->
            ok
    end.


%% Helper functions for logging operations
-spec format_crud_operation(create | delete | set, string, ok | {error, term()}) -> 
                            ok | {error, term()}.
format_crud_operation(Operation, Mo, Result) ->
    case Result of
        ok ->
            ct:log("Successful ~p: ~p~n", [Operation, Mo]),
            ok;
        {error, Reason} ->
            ct:log("Failed ~p: ~p~nReason:~n~p~n", [Operation, Mo, Reason]),
            {error, Reason}
    end.

format_action_result(Action, Mo, ActionResult, ProgressReport) ->
    case ActionResult of
        {ok, "SUCCESS"} ->
            ct:log("Successfully finished action [~p] on: [~p]~nProgressReport: ~p~n", 
                   [Action, Mo, ProgressReport]),
            ok;
        {ok, Result} ->
            ct:log("Finished action [~p] on: [~p]~nResult: [~p]~nProgressReport: ~p~n", 
                   [Action, Mo, Result, ProgressReport]),
            {error, Result};
        {error, Reason} ->
            ct:log("Failed action [~p] on: [~p]~nReason: ~p~nProgressReport: ~p~n", 
                   [Action, Mo, Reason, ProgressReport]),
            {error, Reason}
    end.

managed_state_to_string(enabled) ->
    "ENABLED";
managed_state_to_string(disabled) ->
    "DISABLED".

get_trusted_cert_id_from_progress(ProgressReport) ->
    case proplists:get_value(resultInfo, ProgressReport, undefined) of
        undefined ->
            ct:fail("TrustedCertificate DN missing from resultInfo:~n~p~n", [ProgressReport]);
        TcDN ->
            rct_mo_handler_lib:get_mo_id(TcDN)
    end.
