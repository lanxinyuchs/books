%% Generated by the Erlang ASN.1 compiler version:4.0.4
%% Purpose: Erlang record definitions for each named and unnamed
%% SEQUENCE and SET, and macro definitions for each value
%% definition,in module PKIX1Explicit88



-ifndef(_PKIX1EXPLICIT88_HRL_).
-define(_PKIX1EXPLICIT88_HRL_, true).

-record('Attribute',{
type, values}).

-record('AttributeTypeAndValue',{
type, value}).

-record('Certificate',{
tbsCertificate, signatureAlgorithm, signature}).

-record('TBSCertificate',{
version = asn1_DEFAULT, serialNumber, signature, issuer, validity, subject, subjectPublicKeyInfo, issuerUniqueID = asn1_NOVALUE, subjectUniqueID = asn1_NOVALUE, extensions = asn1_NOVALUE}).

-record('Validity',{
notBefore, notAfter}).

-record('SubjectPublicKeyInfo',{
algorithm, subjectPublicKey}).

-record('Extension',{
extnID, critical = asn1_DEFAULT, extnValue}).

-record('CertificateList',{
tbsCertList, signatureAlgorithm, signature}).

-record('TBSCertList',{
version = asn1_NOVALUE, signature, issuer, thisUpdate, nextUpdate = asn1_NOVALUE, revokedCertificates = asn1_NOVALUE, crlExtensions = asn1_NOVALUE}).

-record('TBSCertList_revokedCertificates_SEQOF',{
userCertificate, revocationDate, crlEntryExtensions = asn1_NOVALUE}).

-record('AlgorithmIdentifier',{
algorithm, parameters = asn1_NOVALUE}).

-record('ORAddress',{
'built-in-standard-attributes', 'built-in-domain-defined-attributes' = asn1_NOVALUE, 'extension-attributes' = asn1_NOVALUE}).

-record('BuiltInStandardAttributes',{
'country-name' = asn1_NOVALUE, 'administration-domain-name' = asn1_NOVALUE, 'network-address' = asn1_NOVALUE, 'terminal-identifier' = asn1_NOVALUE, 'private-domain-name' = asn1_NOVALUE, 'organization-name' = asn1_NOVALUE, 'numeric-user-identifier' = asn1_NOVALUE, 'personal-name' = asn1_NOVALUE, 'organizational-unit-names' = asn1_NOVALUE}).

-record('PersonalName',{
surname, 'given-name' = asn1_NOVALUE, initials = asn1_NOVALUE, 'generation-qualifier' = asn1_NOVALUE}).

-record('BuiltInDomainDefinedAttribute',{
type, value}).

-record('ExtensionAttribute',{
'extension-attribute-type', 'extension-attribute-value'}).

-record('TeletexPersonalName',{
surname, 'given-name' = asn1_NOVALUE, initials = asn1_NOVALUE, 'generation-qualifier' = asn1_NOVALUE}).

-record('UnformattedPostalAddress',{
'printable-address' = asn1_NOVALUE, 'teletex-string' = asn1_NOVALUE}).

-record('PDSParameter',{
'printable-string' = asn1_NOVALUE, 'teletex-string' = asn1_NOVALUE}).

-record('ExtendedNetworkAddress_e163-4-address',{
number, 'sub-address' = asn1_NOVALUE}).

-record('PresentationAddress',{
pSelector = asn1_NOVALUE, sSelector = asn1_NOVALUE, tSelector = asn1_NOVALUE, nAddresses}).

-record('TeletexDomainDefinedAttribute',{
type, value}).

-define('id-pkix', {1,3,6,1,5,5,7}).
-define('id-pe', {1,3,6,1,5,5,7,1}).
-define('id-qt', {1,3,6,1,5,5,7,2}).
-define('id-kp', {1,3,6,1,5,5,7,3}).
-define('id-ad', {1,3,6,1,5,5,7,48}).
-define('id-qt-cps', {1,3,6,1,5,5,7,2,1}).
-define('id-qt-unotice', {1,3,6,1,5,5,7,2,2}).
-define('id-ad-ocsp', {1,3,6,1,5,5,7,48,1}).
-define('id-ad-caIssuers', {1,3,6,1,5,5,7,48,2}).
-define('id-ad-timeStamping', {1,3,6,1,5,5,7,48,3}).
-define('id-ad-caRepository', {1,3,6,1,5,5,7,48,5}).
-define('id-at', {2,5,4}).
-define('id-at-name', {2,5,4,41}).
-define('id-at-surname', {2,5,4,4}).
-define('id-at-givenName', {2,5,4,42}).
-define('id-at-initials', {2,5,4,43}).
-define('id-at-generationQualifier', {2,5,4,44}).
-define('id-at-commonName', {2,5,4,3}).
-define('id-at-localityName', {2,5,4,7}).
-define('id-at-stateOrProvinceName', {2,5,4,8}).
-define('id-at-organizationName', {2,5,4,10}).
-define('id-at-organizationalUnitName', {2,5,4,11}).
-define('id-at-title', {2,5,4,12}).
-define('id-at-dnQualifier', {2,5,4,46}).
-define('id-at-countryName', {2,5,4,6}).
-define('id-at-serialNumber', {2,5,4,5}).
-define('id-at-pseudonym', {2,5,4,65}).
-define('id-domainComponent', {0,9,2342,19200300,100,1,25}).
-define('common-name', 1).
-define('teletex-common-name', 2).
-define('teletex-organization-name', 3).
-define('teletex-personal-name', 4).
-define('teletex-organizational-unit-names', 5).
-define('pds-name', 7).
-define('physical-delivery-country-name', 8).
-define('postal-code', 9).
-define('physical-delivery-office-name', 10).
-define('physical-delivery-office-number', 11).
-define('extension-OR-address-components', 12).
-define('physical-delivery-personal-name', 13).
-define('physical-delivery-organization-name', 14).
-define('extension-physical-delivery-address-components', 15).
-define('unformatted-postal-address', 16).
-define('street-address', 17).
-define('post-office-box-address', 18).
-define('poste-restante-address', 19).
-define('unique-postal-name', 20).
-define('local-postal-attributes', 21).
-define('extended-network-address', 22).
-define('terminal-type', 23).
-define('teletex-domain-defined-attributes', 6).
-define('ub-name', 32768).
-define('ub-common-name', 64).
-define('ub-locality-name', 128).
-define('ub-state-name', 128).
-define('ub-organization-name', 64).
-define('ub-organizational-unit-name', 64).
-define('ub-title', 64).
-define('ub-serial-number', 64).
-define('ub-match', 128).
-define('ub-emailaddress-length', 255).
-define('ub-common-name-length', 64).
-define('ub-country-name-alpha-length', 2).
-define('ub-country-name-numeric-length', 3).
-define('ub-domain-defined-attributes', 4).
-define('ub-domain-defined-attribute-type-length', 8).
-define('ub-domain-defined-attribute-value-length', 128).
-define('ub-domain-name-length', 16).
-define('ub-extension-attributes', 256).
-define('ub-e163-4-number-length', 15).
-define('ub-e163-4-sub-address-length', 40).
-define('ub-generation-qualifier-length', 3).
-define('ub-given-name-length', 16).
-define('ub-initials-length', 5).
-define('ub-integer-options', 256).
-define('ub-numeric-user-id-length', 32).
-define('ub-organization-name-length', 64).
-define('ub-organizational-unit-name-length', 32).
-define('ub-organizational-units', 4).
-define('ub-pds-name-length', 16).
-define('ub-pds-parameter-length', 30).
-define('ub-pds-physical-address-lines', 6).
-define('ub-postal-code-length', 16).
-define('ub-pseudonym', 128).
-define('ub-surname-length', 40).
-define('ub-terminal-id-length', 24).
-define('ub-unformatted-address-length', 180).
-define('ub-x121-address-length', 16).
-endif. %% _PKIX1EXPLICIT88_HRL_
