%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certDataInit.erl %
%%% @author edartop
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/2
%%% 
%%% @doc ==Initialization of certificate management==
%%% This module contains the necessary initialization of certifikate management.

-module(certDataInit).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/2').
-date('2017-09-18').
-author('edartop').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	xxxDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% Rev     Date       Name       What
%%% -----   ----------  -------  -----------------------------
%%% R2A/1   2013-08-08  etxbjca  Created
%%% R3A/4   2015-02-05  etxtory  ensure_dir for vc dir
%%% -----   ----------  -------  -----------------------------
%%% R4A/1   2015-07-07  etxberb  Changed mnesia:create_table to
%%%                              clhI:mnesia_create_table.
%%% -----   ----------  -------  -----------------------------
%%% R5A/1   2015-10-29  etxtory  dummy vc on all boards
%%% R5A/2   2016-01-07  etxberb  Changed installation phases in all blocks.
%%% R5A/5   2016-01-11  etxberb  Added old installation phase functions for
%%%                              backwards compatibility reasons (explicit
%%%                              calls from $RDE_TOP/tools/mkcpi/mkcpi.escript)
%%% -----   ----------  -------  -----------------------------
%%% R6A/1   2016-05-31 etxasta   Added certAlarm process
%%% -----   ----------  -------  -----------------------------
%%% R8A/1   2017-01-20 etxasta   Removed VC for vrcs, cloud usage 
%%% R8A/2   2017-01-23 etxasta   Put back the VC table to please COM 
%%% R8A/3   2017-01-24 etxasta   Temporary put back the VC for vrcs 
%%% R9A/1   2017-02-03 etxasta   VRCS: Added dummy internal certs 
%%% R9A/4   2017-02-15 etxasta   VRCS, BPU: Added the certDist process 
%%% R9A/8   2017-02-28 etomist   ECIM CertM 2.7 uplift
%%% R9A/9   2017-03-02 etomist   HV57826 (RAM table support)
%%% R9A/10  2017-04-06 etxasta   Added certVnfCert process for vrcs
%%% R9A/11  2017-04-06 etxasta   Removing VC for vrcs
%%% R10A/1  2017-05-04 etxasta   Support of R-VNFM
%%% R10A/2  2017-05-10 etxasta   Removed pre_install of nc
%%% R11A/1  2017-09-18 edartop   Added certSecCredu process
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_init_board/0,
	 instPhParallel_post_init/0]).
-export([init/1,
	 init_data/0]).
-export([children/0,
	 activate/0]).

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

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    Tables =
       [{certM, ?certM_types},
        {trustedCertificate, ?trustedCertificate_types},
        {nodeCredential, ?nodeCredential_types},
        {vendorCredential, ?vendorCredential_types},
        {enrollmentServerGroup, ?enrollmentServerGroup_types},
        {enrollmentAuthority, ?enrollmentAuthority_types},
        {trustCategory, ?trustCategory_types},
        {certMCapabilities, ?certMCapabilities_types},
        {enrollmentServer, ?enrollmentServer_types},
        {certNC, ?certNC_types},    % cert internal table
        {certTC, ?certTC_types},    % cert internal table
        {certSub, ?certSub_types},  % cert internal table
        {certCrl, ?certCrl_types}], % cert internal table
    [create_table(TableDef, DbNodes)||TableDef<-Tables],
    create_ram_table({certNcState, ?certNcState_types}, DbNodes),
    certLib:cmd(["mkdir -p ", certLib:cert_dir()]),
    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(DbNodes) ->
    instPhParallel_init(DbNodes).

children() ->
    {ok, [
            {certSecStore, {certSecStore, start, []},
                permanent, 1000, worker, [certSecStore]},
            {certVnfCert, {certVnfCert, start, []},
                permanent, 1000, worker, [certVnfCert]},
            {certServer, {certServer, start, []},
                permanent, 1000, worker, [certServer]},
            {certAlarm, {certAlarm, start, []},
                permanent, 1000, worker, [certAlarm]},
            {certSeci, {certSeci, start, []},
                permanent, 1000, worker, [certSeci]},
            {certDist, {certDist, start, []},
                permanent, 1000, worker, [certDist]},
            {certSecCredu, {certSecCredu, start_link, []},
                permanent, 1000, worker, [certSecCredu]}
                |certNcServer:children()
        ]}.



%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data() ->
    %% Create Top MOCs
    case swmI:is_upgrade_ongoing() of
        true ->
            upg_tab(certTC),
            upg_tab(certM),
            upg_tab(certMCapabilities),
            upg_tab(trustedCertificate),
            upg_tab(nodeCredential),
            upg_tab(enrollmentServerGroup),
            upg_tab(enrollmentAuthority),
            upg_tab(trustCategory),
            upg_tab(enrollmentServer),
            upg_tab(certNC),
            upg_tab(certSub),
            upg_tab(certCrl);
        false ->
            CertM =
            case sysEnv:vrcs() of
                false ->
                    #certM{certMId = {"1","1","1","1"},
                        activeVendorCredential =
                        <<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,VendorCredential=1">>};
                _ ->
                    #certM{certMId = {"1","1","1","1"}}
            end,
            Capabilities = #certMCapabilities{
                certMCapabilitiesId = {"1","1","1","1","1"},
                enrollmentSupport =
                [?EnrollmentSupport_OFFLINE_CSR,
                    ?EnrollmentSupport_OFFLINE_PKCS12,
                    %%?EnrollmentSupport_ONLINE_SCEP, / no support
                    ?EnrollmentSupport_ONLINE_CMP],
                fingerprintSupport = ?FingerprintSupport_SHA_1,
                keySupport = [
                    ?KeyInfo_RSA_1024,
                    ?KeyInfo_RSA_2048,
                    ?KeyInfo_RSA_3072,
                    ?KeyInfo_RSA_4096,
                    ?KeyInfo_ECDSA_160,
                    ?KeyInfo_ECDSA_224,
                    ?KeyInfo_ECDSA_256,
                    ?KeyInfo_ECDSA_384,
                    ?KeyInfo_ECDSA_512,
                    %%?KeyInfo_ECDSA_521  No support in OTP
                    ?KeyInfo_ECDSA_BRAINPOOL_256,
                    ?KeyInfo_ECDSA_BRAINPOOL_320,
                    ?KeyInfo_ECDSA_BRAINPOOL_384,
                    ?KeyInfo_ECDSA_BRAINPOOL_512
                ]},
            Fun = 
            fun() ->
                    mnesia:write(CertM),
                    mnesia:write(Capabilities)
            end,
            {atomic, ok} = mnesia:transaction(Fun)
    end,
    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init_data() ->
    instPhParallel_init_data().

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_board() ->
    PrivDir = code:priv_dir(cert),
    case {sysEnv:vrcs(), swmI:node_type()} of
        {false, _} ->
            %% environment and for target without VC
            TestCrt = filename:join([PrivDir, "vc", "test_vc.pem"]),
            TestKey = filename:join([PrivDir, "vc", "test_vc.key"]),
            VcDir = certLib:vc_dir(),
            VcCrt = filename:join([VcDir, "vc.crt"]),
            VcKey = filename:join([VcDir, "vc.key"]),
            filelib:ensure_dir(VcCrt),
            file:copy(TestCrt, VcCrt),
            file:copy(TestKey, VcKey);
        {true, "R-VNFM"} -> % R-VNFM
            ok;
        {_,_} -> % Only for VRC
            %% Only VRCS uses this dummy internal certificate
            TestP12 = filename:join([PrivDir, "dint", "vnfc1.pfx"]),
            TestPwd = filename:join([PrivDir, "dint", "vnfc1.sec"]),
            Dir     = certLib:tmp_cert_dir(),
            IntP12  = filename:join([Dir, "vnfc.pfx"]),
            IntPwd  = filename:join([Dir, "vnfc.sec"]),
            filelib:ensure_dir(IntP12),
            file:copy(TestP12, IntP12),
            file:copy(TestPwd, IntPwd)
    end,
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_post_init() ->
    %% Register Netconf notifications (AVC)
    comsaI:register_subscriptions("RcsCertM", [
            {"CertM",                 certM},
            {"VendorCredential",      vendorCredential},
            {"TrustedCertificate",    trustedCertificate},
            {"NodeCredential",        nodeCredential},
            {"EnrollmentServerGroup", enrollmentServerGroup},
            {"EnrollmentAuthority",   enrollmentAuthority},
            {"TrustCategory",         trustCategory},
            {"EnrollmentServer",      enrollmentServer},
            {"CertMCapabilities",     certMCapabilities}]),
    ok.

activate() ->
    certSecCredu:activate(),
    ok.

upg_tab(nodeCredential) ->
    try
        begin
                [{nodeCredential, Ets}] = ets:lookup(olddb, nodeCredential),
                lists:foreach(
                    fun({nodeCredential,ID, UL, SN, RBU, ET, ESG,
                                KI, EP, RM, EA, EAT, CC, CS}) ->
                            mnesia:dirty_write(
                                #nodeCredential{
                                    nodeCredentialId      = ID,
                                    userLabel             = UL,
                                    subjectName           = SN,
                                    reservedByUser        = RBU,
                                    enrollmentTimer       = ET,
                                    enrollmentServerGroup = ESG,
                                    keyInfo               = KI,
                                    enrollmentProgress    = EP,
                                    renewalMode           = RM,
                                    enrollmentAuthority   = EA,
                                    expiryAlarmThreshold  = EAT,
                                    subjectAltName        = undefined,
                                    certificateContent    = CC, 
                                    certificateState      = CS});
                        (Obj) ->
                            mnesia:dirty_write(Obj)
                    end, ets:tab2list(Ets)),
                ok
        end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, copy_old_table, [nodeCredential]},
                    Err]),
            exit(ErrReason) 
    end;
upg_tab(trustedCertificate) ->
    try
        begin
                [{trustedCertificate, Ets}] =
                ets:lookup(olddb, trustedCertificate),
                lists:foreach(
                    fun(Obj) ->
                            mnesia:dirty_write(Obj),
                            Index =
                            Obj#trustedCertificate.trustedCertificateId,
                            case mnesia:dirty_read(certTC, Index) of
                                [] ->
                                    case read_old_tc(Index, "tc.crt") of
                                        {ok, Bin} ->
                                            Hash =
                                            certLib:get_fingerprint_support(),
                                            Fingerprint =
                                            crypto:hash(Hash, Bin),
                                            NewTC =
                                            #certTC{
                                                index       = Index,
                                                fingerprint = Fingerprint,
                                                cert        = Bin},
                                            mnesia:dirty_write(NewTC),
                                            info_msg("Upg TC, ~p~n", [Index]);
                                        _ ->
                                            info_msg("Failed to upg TC, ~p~n",
                                                [Index])
                                    end,
                                    delete_old_tc(Index, dir);
                                [{certTC, Index, Timeout}] ->
                                    mnesia:dirty_delete(certTC, Index),
                                    case read_old_tc(Index, "tc.crt") of
                                        {ok, Bin} ->
                                            Hash =
                                            certLib:get_fingerprint_support(),
                                            Fingerprint =
                                            crypto:hash(Hash, Bin),
                                            NewTC =
                                            #certTC{
                                                index       = Index,
                                                fingerprint = Fingerprint,
                                                cert        = Bin,
                                                timeout     = Timeout},
                                            mnesia:dirty_write(NewTC),
                                            info_msg("Upg TC, ~p~n", [Index]);
                                        _ ->
                                            NewTC =
                                            #certTC{
                                                index       = Index,
                                                timeout     = Timeout},
                                            mnesia:dirty_write(NewTC),
                                            info_msg("Failed to upg TC, ~p~n",
                                                [Index])
                                    end,
                                    delete_old_tc(Index, dir);
                                _ ->
                                    info_msg("No need to upg TC, ~p~n",
                                        [Index])
                            end
                    end, ets:tab2list(Ets)),
                ok
        end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, copy_old_table,
                        [trustedCertificate]},
                    Err]),
            exit(ErrReason) 
    end;
upg_tab(certNC) ->
    try
        begin
                [{certNC, Ets}] = ets:lookup(olddb, certNC),
                lists:foreach(
                    fun(Obj) ->
                            NewObj =
                            case Obj#certNC.cert of
                                [_|_] -> %% No need to convert, correct format
                                    Obj;
                                undefined -> %% No need to convert,
                                             %% no cert existing
                                    Obj;
                                Cert -> %% Convert to new 16B format
                                    Obj#certNC{cert = [Cert]}
                            end,
                            mnesia:dirty_write(NewObj)
                    end, ets:tab2list(Ets)),
                ok
        end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, copy_old_table, [certNC]},
                    Err]),
            exit(ErrReason) 
    end;
upg_tab(trustCategory) ->
    try
        begin
                [{trustCategory, Ets}] = ets:lookup(olddb, trustCategory),
                lists:foreach(
                    fun({trustCategory,ID,UL,TCs,RBU}) ->
                            mnesia:dirty_write(
                                #trustCategory{
                                    trustCategoryId      = ID,
                                    userLabel            = UL,
                                    trustedCertificates  = TCs,
                                    reservedByUser       = RBU,
                                    crlCheck = ?FeatureState_DEACTIVATED});
                        (Obj) ->
                            mnesia:dirty_write(Obj)
                    end, ets:tab2list(Ets)),
                ok
        end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, copy_old_table, [trustCategory]},
                    Err]),
            exit(ErrReason) 
    end;
upg_tab(certMCapabilities) ->
    try
        begin
                [{certMCapabilities, Ets}] =
                ets:lookup(olddb, certMCapabilities),
                lists:foreach(
                    fun({certMCapabilities,ID,ES,FS}) ->
                            mnesia:dirty_write(
                                #certMCapabilities{
                                    certMCapabilitiesId = ID,
                                    enrollmentSupport   = ES,
                                    fingerprintSupport  = FS,
                                    keySupport = [
                                        ?KeyInfo_RSA_1024,
                                        ?KeyInfo_RSA_2048,
                                        ?KeyInfo_RSA_3072,
                                        ?KeyInfo_RSA_4096,
                                        ?KeyInfo_ECDSA_160,
                                        ?KeyInfo_ECDSA_224,
                                        ?KeyInfo_ECDSA_256,
                                        ?KeyInfo_ECDSA_384,
                                        ?KeyInfo_ECDSA_512,
                                        %%?KeyInfo_ECDSA_521  No support in OTP
                                        ?KeyInfo_ECDSA_BRAINPOOL_256,
                                        ?KeyInfo_ECDSA_BRAINPOOL_320,
                                        ?KeyInfo_ECDSA_BRAINPOOL_384,
                                        ?KeyInfo_ECDSA_BRAINPOOL_512
                                    ]});
                        (Obj) ->
                            mnesia:dirty_write(Obj)
                    end, ets:tab2list(Ets)),
                ok
        end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, copy_old_table,
                        [certMCapabilities]}, Err]),
            exit(ErrReason) 
    end;

upg_tab(certM) ->
    try
        begin
            [{certM, Ets}] = ets:lookup(olddb, certM),
            lists:foreach(
            fun({certM, ID, LFSP, RP, UL}) ->
                mnesia:dirty_write(
                    #certM{certMId = ID,
                           localFileStorePath = LFSP,
                           reportProgress = RP,
                           userLabel = UL,
                           activeVendorCredential =
                           <<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,VendorCredential=1">>});
                (Obj) -> mnesia:dirty_write(Obj)
                end,
                ets:tab2list(Ets)),
            ok
        end
    catch
        ErrClass : ErrReason ->
            Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
            sysInitI:error_report([{?MODULE, copy_old_table, [certM]}, Err]),
            exit(ErrReason)
    end;

upg_tab(Tab) ->
    swmI:copy_old_table(Tab).

read_old_tc(Index, Filename) ->
    Path = certLib:tc_dir(Index),
    File = filename:join(Path, Filename),
    file:read_file(File).

delete_old_tc(Index, dir) ->
    Path = certLib:tc_dir(Index),
    certLib:cmd(["rm -rf ", Path]),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

create_table({Name, Types}, DbNodes) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]).

create_ram_table({Name, Types}, DbNodes) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
    clhI:mnesia_create_table(Name, [{type, set},
                    {ram_copies, DbNodes},
                    {attributes, Fields} |
                    add_clh_option(Name)]).


%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).
