%%% --------------------------------------------------------
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
%%% --------------------------------------------------------

-hrl_id({"RcsCertM","2.8.0","/main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R12A/0"}).


%% -------------- CLASS CertM -------------------------

%% Description:
%% The top class of the Certificate Management model. 

-record(certM, {certMId,
                userLabel,
                reportProgress,
                localFileStorePath,
                activeVendorCredential}).

-define(certM_types,
        [{certMId, string},
         {userLabel, string},
         {reportProgress, {struct,'AsyncActionProgress'}},
         {localFileStorePath, string},
         {activeVendorCredential, moRef}]).

-define(CertM_restricted, [certMId]).


%% -------------- CLASS TrustedCertificate -------------------------

%% Description:
%% Represents a trusted certificate.

-record(trustedCertificate, {trustedCertificateId,
                             reservedByCategory,
                             managedState,
                             certificateContent,
                             certificateState}).

-define(trustedCertificate_types,
        [{trustedCertificateId, string},
         {reservedByCategory, {sequence,moRef}},
         {managedState, 'RcsCertM.ManagedCertificateState'},
         {certificateContent, {struct,'CertificateContent'}},
         {certificateState, 'RcsCertM.CertificateState'}]).

-define(trustedCertificate_managedState_default, 'ENABLED').
-define(TrustedCertificate_restricted, [trustedCertificateId]).


%% -------------- CLASS NodeCredential -------------------------

%% Description:
%% Represents the node credential and contains information about the corresponding certificate.

-record(nodeCredential, {nodeCredentialId,
                         userLabel,
                         subjectName,
                         reservedByUser,
                         enrollmentTimer,
                         enrollmentServerGroup,
                         keyInfo,
                         enrollmentProgress,
                         renewalMode,
                         enrollmentAuthority,
                         expiryAlarmThreshold,
                         subjectAltName,
                         certificateContent,
                         certificateState}).

-define(nodeCredential_types,
        [{nodeCredentialId, string},
         {userLabel, string},
         {subjectName, string},
         {reservedByUser, {sequence,moRef}},
         {enrollmentTimer, uint16},
         {enrollmentServerGroup, moRef},
         {keyInfo, 'RcsCertM.KeyInfo'},
         {enrollmentProgress, {struct,'AsyncActionProgress'}},
         {renewalMode, 'RcsCertM.RenewalMode'},
         {enrollmentAuthority, moRef},
         {expiryAlarmThreshold, 'RcsCertM.ExpiryAlarmThresholdRange'},
         {subjectAltName, string},
         {certificateContent, {struct,'CertificateContent'}},
         {certificateState, 'RcsCertM.CertificateState'}]).

-define(nodeCredential_enrollmentTimer_default, 60).
-define(nodeCredential_renewalMode_default, 'MANUAL').
-define(nodeCredential_expiryAlarmThreshold_default, 30).
-define(NodeCredential_restricted,
        [nodeCredentialId,
         subjectName]).


%% -------------- CLASS VendorCredential -------------------------

%% Description:
%% Represents the vendor credential pre-installed on the ME.

-record(vendorCredential, {vendorCredentialId,
                           certificateContent,
                           certificateState}).

-define(vendorCredential_types,
        [{vendorCredentialId, string},
         {certificateContent, {struct,'CertificateContent'}},
         {certificateState, 'RcsCertM.CertificateState'}]).

-define(VendorCredential_restricted, [vendorCredentialId]).


%% -------------- CLASS EnrollmentServerGroup -------------------------

%% Description:
%% Maintains a group of enrollment servers for load balancing.

-record(enrollmentServerGroup, {enrollmentServerGroupId,
                                userLabel}).

-define(enrollmentServerGroup_types,
        [{enrollmentServerGroupId, string},
         {userLabel, string}]).

-define(EnrollmentServerGroup_restricted, [enrollmentServerGroupId]).


%% -------------- CLASS EnrollmentAuthority -------------------------

%% Description:
%% Represents a Certification or Registration Authority for certificate enrollment.

-record(enrollmentAuthority, {enrollmentAuthorityId,
                              enrollmentCaCertificate,
                              enrollmentCaFingerprint,
                              authorityType,
                              userLabel,
                              enrollmentAuthorityName}).

-define(enrollmentAuthority_types,
        [{enrollmentAuthorityId, string},
         {enrollmentCaCertificate, moRef},
         {enrollmentCaFingerprint, 'RcsCertM.Fingerprint'},
         {authorityType, 'RcsCertM.AuthorityType'},
         {userLabel, string},
         {enrollmentAuthorityName, 'RcsCertM.DistinguishedName'}]).

-define(EnrollmentAuthority_restricted, [enrollmentAuthorityId]).


%% -------------- CLASS TrustCategory -------------------------

%% Description:
%% Represents a group of trusted certificates that can be referenced by Credential Users on the ME.

-record(trustCategory, {trustCategoryId,
                        userLabel,
                        trustedCertificates,
                        reservedByUser,
                        crlCheck}).

-define(trustCategory_types,
        [{trustCategoryId, string},
         {userLabel, string},
         {trustedCertificates, {sequence,moRef}},
         {reservedByUser, {sequence,moRef}},
         {crlCheck, 'RcsCertM.FeatureState'}]).

-define(trustCategory_crlCheck_default, 'DEACTIVATED').
-define(TrustCategory_restricted, [trustCategoryId]).


%% -------------- CLASS CertMCapabilities -------------------------

%% Description:
%% Contains the certificate management capabilities of the ME.

-record(certMCapabilities, {certMCapabilitiesId,
                            enrollmentSupport,
                            fingerprintSupport,
                            keySupport}).

-define(certMCapabilities_types,
        [{certMCapabilitiesId, string},
         {enrollmentSupport, {sequence,'RcsCertM.EnrollmentSupport'}},
         {fingerprintSupport, 'RcsCertM.FingerprintSupport'},
         {keySupport, {sequence,'RcsCertM.KeyInfo'}}]).

-define(CertMCapabilities_restricted, [certMCapabilitiesId]).


%% -------------- CLASS EnrollmentServer -------------------------

%% Description:
%% Represents an enrollment server.

-record(enrollmentServer, {enrollmentServerId,
                           userLabel,
                           enrollmentAuthority,
                           uri,
                           protocol}).

-define(enrollmentServer_types,
        [{enrollmentServerId, string},
         {userLabel, string},
         {enrollmentAuthority, moRef},
         {uri, string},
         {protocol, 'RcsCertM.EnrollmentProtocol'}]).

-define(EnrollmentServer_restricted, [enrollmentServerId]).


%% -------------- CLASS ChainCertificate -------------------------

%% Description:
%% Chain certificate belonging to the credential.

-record(chainCertificate, {chainCertificateId,
                           certificateContent,
                           certificateState}).

-define(chainCertificate_types,
        [{chainCertificateId, string},
         {certificateContent, {struct,'CertificateContent'}},
         {certificateState, 'RcsCertM.CertificateState'}]).

-define(ChainCertificate_restricted, [chainCertificateId]).


%% ------------------ ENUM KeyInfo ----------------------
-ifndef('KeyInfo').
-define('KeyInfo', 1).

-define(KeyInfo_RSA_1024, 0).
-define(KeyInfo_RSA_2048, 1).
-define(KeyInfo_RSA_3072, 2).
-define(KeyInfo_RSA_4096, 3).
-define(KeyInfo_ECDSA_160, 4).
-define(KeyInfo_ECDSA_224, 5).
-define(KeyInfo_ECDSA_256, 6).
-define(KeyInfo_ECDSA_384, 7).
-define(KeyInfo_ECDSA_512, 8).
-define(KeyInfo_ECDSA_521, 9).
-define(KeyInfo_ECDSA_BRAINPOOL_256, 10).
-define(KeyInfo_ECDSA_BRAINPOOL_320, 11).
-define(KeyInfo_ECDSA_BRAINPOOL_384, 12).
-define(KeyInfo_ECDSA_BRAINPOOL_512, 13).
-define(KeyInfo_ECDSA_SECP_256_R1, 14).
-define(KeyInfo_ECDSA_SECP_384_R1, 15).
-define(KeyInfo_ECDSA_SECP_521_R1, 16).

-endif. % KeyInfo

%% ------------------ ENUM CertificateState ----------------------
-ifndef('CertificateState').
-define('CertificateState', 1).

-define(CertificateState_NOT_VALID_YET, 1).
-define(CertificateState_VALID, 0).
-define(CertificateState_EXPIRED, 2).
-define(CertificateState_REVOKED, 3).

-endif. % CertificateState

%% ------------------ ENUM ActionStateType ----------------------
-ifndef('ActionStateType').
-define('ActionStateType', 1).

-define(ActionStateType_CANCELLING, 1).
-define(ActionStateType_RUNNING, 2).
-define(ActionStateType_FINISHED, 3).
-define(ActionStateType_CANCELLED, 4).

-endif. % ActionStateType

%% ------------------ ENUM FingerprintSupport ----------------------
-ifndef('FingerprintSupport').
-define('FingerprintSupport', 1).

-define(FingerprintSupport_SHA_1, 0).
-define(FingerprintSupport_SHA_224, 1).
-define(FingerprintSupport_SHA_256, 2).
-define(FingerprintSupport_SHA_384, 3).
-define(FingerprintSupport_SHA_512, 4).

-endif. % FingerprintSupport

%% ------------------ ENUM AuthorityType ----------------------
-ifndef('AuthorityType').
-define('AuthorityType', 1).

-define(AuthorityType_CERTIFICATION_AUTHORITY, 0).
-define(AuthorityType_REGISTRATION_AUTHORITY, 1).

-endif. % AuthorityType

%% ------------------ ENUM RenewalMode ----------------------
-ifndef('RenewalMode').
-define('RenewalMode', 1).

-define(RenewalMode_MANUAL, 0).
-define(RenewalMode_AUTOMATIC, 1).

-endif. % RenewalMode

%% ------------------ ENUM EnrollmentProtocol ----------------------
-ifndef('EnrollmentProtocol').
-define('EnrollmentProtocol', 1).

-define(EnrollmentProtocol_SCEP, 0).
-define(EnrollmentProtocol_CMP, 1).

-endif. % EnrollmentProtocol

%% ------------------ ENUM ManagedCertificateState ----------------------
-ifndef('ManagedCertificateState').
-define('ManagedCertificateState', 1).

-define(ManagedCertificateState_ENABLED, 0).
-define(ManagedCertificateState_DISABLED, 1).

-endif. % ManagedCertificateState

%% ------------------ ENUM FeatureState ----------------------
-ifndef('FeatureState').
-define('FeatureState', 1).

-define(FeatureState_ACTIVATED, 0).
-define(FeatureState_DEACTIVATED, 1).

-endif. % FeatureState

%% ------------------ ENUM ActionResultType ----------------------
-ifndef('ActionResultType').
-define('ActionResultType', 1).

-define(ActionResultType_SUCCESS, 1).
-define(ActionResultType_FAILURE, 2).
-define(ActionResultType_NOT_AVAILABLE, 3).

-endif. % ActionResultType

%% ------------------ ENUM EnrollmentSupport ----------------------
-ifndef('EnrollmentSupport').
-define('EnrollmentSupport', 1).

-define(EnrollmentSupport_OFFLINE_CSR, 0).
-define(EnrollmentSupport_OFFLINE_PKCS12, 1).
-define(EnrollmentSupport_ONLINE_SCEP, 2).
-define(EnrollmentSupport_ONLINE_CMP, 3).

-endif. % EnrollmentSupport

%% ------------------ STRUCT AsyncActionProgress ----------------------
-ifndef(_ASYNC_ACTION_PROGRESS).
-define(_ASYNC_ACTION_PROGRESS, 1).

-record('AsyncActionProgress', {actionName,
                                additionalInfo,
                                progressInfo,
                                progressPercentage,
                                result,
                                resultInfo,
                                state,
                                actionId,
                                timeActionStarted,
                                timeActionCompleted,
                                timeOfLastStatusUpdate}).

-define('AsyncActionProgress_types',
        [{actionName, string},
         {additionalInfo, {sequence,string}},
         {progressInfo, string},
         {progressPercentage, uint8},
         {result, 'RcsCertM.ActionResultType'},
         {resultInfo, string},
         {state, 'RcsCertM.ActionStateType'},
         {actionId, uint16},
         {timeActionStarted, 'RcsCertM.DateTime'},
         {timeActionCompleted, 'RcsCertM.DateTime'},
         {timeOfLastStatusUpdate, 'RcsCertM.DateTime'}]).


-endif. % _ASYNC_ACTION_PROGRESS


%% ------------------ STRUCT CertificateContent ----------------------
-ifndef(_CERTIFICATE_CONTENT).
-define(_CERTIFICATE_CONTENT, 1).

-record('CertificateContent', {version,
                               serialNumber,
                               signatureAlgorithm,
                               issuer,
                               validFrom,
                               validTo,
                               publicKey,
                               publicKeyAlgorithm,
                               keyUsage,
                               extensionContent,
                               subject}).

-define('CertificateContent_types',
        [{version, string},
         {serialNumber, string},
         {signatureAlgorithm, string},
         {issuer, string},
         {validFrom, 'RcsCertM.DateTime'},
         {validTo, 'RcsCertM.DateTime'},
         {publicKey, string},
         {publicKeyAlgorithm, string},
         {keyUsage, string},
         {extensionContent, {sequence,string}},
         {subject, string}]).


-endif. % _CERTIFICATE_CONTENT

