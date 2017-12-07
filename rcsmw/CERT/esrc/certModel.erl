%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certModel.erl %
%%% @author eivirad
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/3
%%%
%%% @doc ==ECIM model callback module for CertM==
%%% This module implements the callback functions for the CertM model
-module(certModel).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/3').
-date('2017-11-09').
-author('eivirad').
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-08-08   etxjotj     Created
%%% R2A/15     2013-12-19   etxarnu     Corrected action/4
%%% R2A/22     2014-03-19   etxlg       Allow empty Atts in setMoAttributes
%%% R3A/1      2014-11-28   etxberb     Added values/1.
%%% R3A/2-3    2014-12-11   etxberb     Bug correction.
%%% R3A/4      2015-01-16   etxberb     Bug correction; Added 'case comsaGeneric:
%%%                                     existsMo..' in getMoAttributes.
%%% R5A/2      2016-04-25   ehsake      Validate EnrollmentServer:uri,HU76338
%%% R7A/1      2016-10-04   etomist     HU24184, removed custom reservedBy check
%%% R8A/1      2016-11-15   etomist     HV30513, support for escape sequences in URIs
%%% R9A/5      2017-02-16   etomist     HV64748, IPv6 address fix
%%% R9A/6      2017-02-23   ebabmat     HV66821, attribute keyInfo set fix
%%% R9A/7      2017-03-02   etomist     HV57826
%%% R9A/9      2017-03-20   ebabmat     HV71517, check if escape sequences valid
%%%                                     in enrollmentAuthority name
%%% R9A/11     2017-03-20   eivomat     HV70387
%%% R11A/1     2017-10-13   emarnek     HW33598
%%% R12A/1     2017-11-04   eivirad     HW39506
%%% R12A/2     2017-11-04   eivirad     Revert HW39506 due to faulty TCs in certm suite
%%% R12A/3     2017-11-06   eivirad     HW39506
%%% ----------------------------------------------------------

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).
-export([action/3]).

-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         action/4,
         createMo/5]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%-export([internal_function1/2]).
%%%-export([internal_function2/3]).

-include("RcsCertM.hrl").
-include("cert.hrl").
-define(FTPES_PORT, 21).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
existsMo([<<"1">>,<<"ChainCertificate">>,<<"1">>,<<"VendorCredential">>|_],_) ->
    case certSecStore:get_next_chain_key(undefined) of
        {ok, undefined} ->
            false;
        _ ->
            true
    end;
existsMo([Id,<<"ChainCertificate">>,<<"1">>,<<"VendorCredential">>|_],_) ->
    ChainId = list_to_integer(binary_to_list(Id)) - 1,
    case certSecStore:get_next_chain_key(ChainId) of
        {ok, undefined} ->
            false;
        _ ->
            true
    end;
existsMo([<<"1">>,<<"ChainCertificate">>,NcId,<<"NodeCredential">>|_], _) ->
    NcKey    = {"1","1","1","1",binary_to_list(NcId)},
     case certNcServer:get_next_chain_key(NcKey, undefined) of
        {ok, undefined} ->
            false;
        _ ->
            true
    end;
existsMo([Id,<<"ChainCertificate">>,NcId,<<"NodeCredential">>|_], _) ->
    ChainId = list_to_integer(binary_to_list(Id)) - 1,
    NcKey    = {"1","1","1","1",binary_to_list(NcId)},
     case certNcServer:get_next_chain_key(NcKey, ChainId) of
        {ok, undefined} ->
            false;
        _ ->
            true
    end;
existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

getMoAttributes(AttrNames, DnRev, TxHandle) ->
    case existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true ->
	    [getMoAttribute([AN | DnRev], TxHandle) || AN <- AttrNames];
	false ->
	    []
    end.

%getMoAttribute([Attribute|DnRev], TxHandle) ->
getMoAttribute([<<"certificateContent">>,ChainId,<<"ChainCertificate">>,
        <<"1">>,<<"VendorCredential">>|_]=[_|DnRev], _) ->
    CC = certSecStore:get_chain_cc(list_to_integer(binary_to_list(ChainId))),
    StructFields =
    comsaEcimModelAdaptor:get_struct_fields(DnRev,'CertificateContent'),
    comsaGeneric:format_struct(CC, StructFields);
getMoAttribute([<<"certificateState">>,_,<<"ChainCertificate">>,
        <<"1">>,<<"VendorCredential">>|_], _) ->
   [12, ?CertificateState_VALID];
getMoAttribute([<<"certificateContent">>,ChainId,<<"ChainCertificate">>,
        NcId,<<"NodeCredential">>|_]=[_|DnRev], _) ->
    NcKey = {"1","1","1","1",binary_to_list(NcId)},
    CC =
    certNcServer:get_chain_cc(NcKey, list_to_integer(binary_to_list(ChainId))),
    StructFields =
    comsaEcimModelAdaptor:get_struct_fields(DnRev,'CertificateContent'),
    comsaGeneric:format_struct(CC, StructFields);
getMoAttribute([<<"certificateState">>,_,<<"ChainCertificate">>,
        _,<<"NodeCredential">>|_], _) ->
    [12, ?CertificateState_VALID];
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

%nextMo(Dn, Key, TxHandle) -
% ChainCertificate for Vendor Credential
nextMo([<<"ChainCertificate">>,<<"1">>,<<"VendorCredential">>|_], Key, _) ->
    case certSecStore:get_next_chain_key(Key) of
        {ok, undefined} ->
            {ok, undefined};
        {ok, {"1","1","1","1","1",NextIndex}} ->
            {ok,{{9,list_to_binary(NextIndex)},{"1","1","1","1","1",NextIndex}}}
    end;
% ChainCertificate for Node Credential
nextMo([<<"ChainCertificate">>,Id,<<"NodeCredential">>|_], Key, _) ->
    Index = binary_to_list(Id),
    case certNcServer:get_next_chain_key({"1","1","1","1",Index}, Key) of
        {ok, undefined} ->
            {ok, undefined};
        {ok, {"1","1","1","1","1",NextIndex}} ->
            {ok,{{9,list_to_binary(NextIndex)},{"1","1","1","1","1",NextIndex}}}
    end;
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

moref_to_key(MoRef) when is_list(MoRef) ->
    DnRev = lists:reverse([list_to_binary(X)||
			      X<-string:tokens(MoRef, "=,")]),
    comsaGeneric:dnrev_to_key(DnRev).

dnrev_to_moref([Index,Class]) ->
    binary_to_list(Class)++"="++binary_to_list(Index);
dnrev_to_moref([Index, Class|DnRev]) ->
    dnrev_to_moref(DnRev)++","
	++binary_to_list(Class)++"="++binary_to_list(Index).

setMoAttributes([], DnRev, _TransId) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), []);
setMoAttributes([{AttrName, TypeAndValue}|Rest], DnRev, TransId) ->
    R = setMoAttribute([AttrName|DnRev], TypeAndValue, undefined, TransId),
    case Rest of
	[] -> R;
	_  -> setMoAttributes(Rest, DnRev, TransId)
    end.

setMoAttribute([Attribute= <<"uri">>|DnRev],{Type, Value},_,_) ->
    %% HV64748, fix for IPv6 addresses
    case http_uri:parse(http_uri:decode(binary_to_list(Value)), 
                        [{ipv6_host_with_brackets, true}, 
                         {scheme_defaults, default_ports()}]) of 
       {ok, _Result} ->
        Table = table(comsaGeneric:class(DnRev)),
        comsaGeneric:set(DnRev, Attribute, Table, types(Table), {Type, Value});
    {error,_} ->
        mnesia:abort("Invalid URI")
    end;

setMoAttribute([Attribute= <<"protocol">> |
                [_Id, <<"EnrollmentServer">> | _] = DnRev],
               {Type, EnrollmentProtocol}, _, _) ->
    [#certMCapabilities{enrollmentSupport = EnrollmentSupport}] =
        mnesia:dirty_read(certMCapabilities, {"1","1","1","1","1"}),
    {SupportForProtocol, ProtocolString} =
        case EnrollmentProtocol of
            ?EnrollmentProtocol_SCEP ->
                {?EnrollmentSupport_ONLINE_SCEP, "SCEP"};
            ?EnrollmentProtocol_CMP  ->
                {?EnrollmentSupport_ONLINE_CMP, "CMP"};
            _                        ->
                {unsupported, "UNSUPPORTED"}
        end,

    case lists:member(SupportForProtocol, EnrollmentSupport) of
        true  ->
            Table = table(comsaGeneric:class(DnRev)),
            comsaGeneric:set(DnRev, Attribute, Table, types(Table),
                             {Type, EnrollmentProtocol});
        false ->
            Reason =
                "Unsupported enrollment protocol " ++ ProtocolString,
            mnesia:abort(Reason)
    end;

setMoAttribute([Attribute= <<"keyInfo">> |
                [_Id, <<"NodeCredential">> | _] = DnRev],
               {Type, KeyInfoValue}, _, _) ->
    [#certMCapabilities{keySupport = SupportedKeys}] =
        mnesia:dirty_read(certMCapabilities, {"1","1","1","1","1"}),
    case lists:member(KeyInfoValue, SupportedKeys) of
        true  ->
            Table = table(comsaGeneric:class(DnRev)),
            comsaGeneric:set(DnRev, Attribute, Table, types(Table),
                             {Type, KeyInfoValue});
        false ->
            Reason =
                "Unsupported keyInfo " ++ integer_to_list(KeyInfoValue),
            mnesia:abort(Reason)
    end;

setMoAttribute([Attribute= <<"enrollmentAuthorityName">> |
                [_Id, <<"EnrollmentAuthority">> | _] = DnRev],
               {Type, Name}, _, _) ->
    IsValid = check_RDN_valid(binary_to_list(Name)),
    case IsValid of
        true  ->
            Table = table(comsaGeneric:class(DnRev)),
            comsaGeneric:set(DnRev, Attribute, Table, types(Table),
                             {Type, Name});
        false ->
            Reason =
                "Badly formed Enrollment Authority name, name contains illegal escape sequence: " ++ Name,
            mnesia:abort(Reason)
    end;

%% HV70387
setMoAttribute([Attribute = <<"enrollmentCaCertificate">> | DnRev],
               undefined, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    CaCertificate = comsaGeneric:get(DnRev, Attribute, Table, types(Table)),
    case CaCertificate of
        [11] -> %% ComOamSpiDatatype_REFERENCE = 11
            do_nothing; %% caCert already empty, don't clear fingerprint
        _ ->
            %% If CaCertificate was set, when unsetting also clear fingerprint
            comsaGeneric:set(DnRev, <<"enrollmentCaFingerprint">>,
                             Table, types(Table), undefined)
    end,
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), undefined);
setMoAttribute([Attribute = <<"enrollmentCaCertificate">> | DnRev],
               {Type, TrustedCertificateMoRef}, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    TrustedCertificateString = binary:bin_to_list(TrustedCertificateMoRef),
    TCIndex = lists:last(string:tokens(TrustedCertificateString, "=,")),
    Fingerprint =
        case mnesia:dirty_read({certTC, {"1","1","1","1",TCIndex}}) of
            [CertTC] ->
                BinFingerprint = CertTC#certTC.fingerprint,
                certLib:encode_fingerprint(BinFingerprint);
            _ ->
                undefined
        end,
    comsaGeneric:set(DnRev, <<"enrollmentCaFingerprint">>, Table, types(Table),
                     {9, Fingerprint}), %% ComOamSpiDatatype_STRING = 9
    comsaGeneric:set(DnRev, Attribute, Table, types(Table),
                     {Type, TrustedCertificateMoRef});

%% HV70387
setMoAttribute([Attribute = <<"enrollmentCaFingerprint">> | DnRev],
               TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    CaCertificate = comsaGeneric:get(DnRev, <<"enrollmentCaCertificate">>,
                                     Table, types(Table)),
    case CaCertificate of
        [11] -> %% ComOamSpiDatatype_REFERENCE = 11
            %% Value undefined, can set Fingerprint
            comsaGeneric:set(DnRev, Attribute, Table, types(Table),
                             TypeAndValue);
        _ ->
            mnesia:abort("Can't set fingerprint if enrollmentCaCertificate "
                         "is set")
    end;

setMoAttribute([Attr= <<"trustedCertificates">>|DnRev], TypeAndValue, _, _) ->
    %% Update TrustedCertficiate reservedByCategory bi directional reference
    %% with added
    %% TAV = this attribute is a seuqence of MoRef, however if only one
    %%       MoRef is given, comte will not return a list of one MoRef, but
    %%       the MoRef itself
    TAV =
	case TypeAndValue of
	    {_, _} ->
		[TypeAndValue];
	    TypeAndValue when is_list(TypeAndValue) ->
		TypeAndValue
	end,

    %% Check that Trusted Certificates exist, if not abort
    lists:foreach(
        fun({_,BinRef}) ->
                ["ManagedElement","1","SystemFunctions","1","SecM","1",
                    "CertM","1","TrustedCertificate",Index] =
                string:tokens(binary_to_list(BinRef), ",="),
                case mnesia:dirty_read(trustedCertificate, {"1","1","1","1",Index}) of
                    [] ->
                        Msg = "TrustedCertificate=" ++ Index ++ " is missing",
                        mnesia:abort(list_to_binary(Msg));
                    _ ->
                        ok
                end
        end, TAV),

    %% This is kind of a brute force solution to keeping check on all the
    %% references, but calculating deltas is not what I'd like to do right
    %% now. If this becomes inefficient, please rewrite. jotj 2013-08-12

    %% The following is based on that the TypeAndValue parameter contains
    %% the complete list of references when called

    %% Remove all references to this trust category
    %% [begin
    %% 	 [TC] = mnesia:read({trustedCertificate, Key}),
    %% 	 CurrentReserved = TC#trustedCertificate.reservedByCategory,
    %% 	 case lists:member(ThisRef, CurrentReserved) of
    %% 	     true ->
    %% 		 NewR = lists:delete(ThisRef, CurrentReserved),
    %% 		 mnesia:write(TC#trustedCertificate{reservedByCategory=NewR});
    %% 	     false ->
    %% 		 ok
    %% 	 end
    %%  end||Key<-mnesia:all_keys(trustedCertificate)],

    %% Standard table op
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attr, Table, types(Table), TAV);

setMoAttribute([Attribute = <<"renewalMode">> |
               [_Id, <<"NodeCredential">>|_] = DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue),
    certNcServer:restart_expiration_timer(DnRev);

setMoAttribute([Attribute = <<"expiryAlarmThreshold">> |
               [_Id, <<"NodeCredential">>|_] = DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue),
    certNcServer:restart_expiration_timer(DnRev);

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

check_RDN_valid([]) -> true; 
check_RDN_valid([Char|NameTail]) ->
    case [Char] of
        "\\" ->
            [H|T] = NameTail,
            case lists:member([H], [",", "=", "+", "<", ">", "#", ";", "\\", "\""]) of % special chars that can be escaped directly
                true -> check_RDN_valid(T);
                _ -> 
                     MatchFirstChar = re:run([H], "[0-9|a-f|A-F]"),
                     case MatchFirstChar of
                         {match, _} -> 
                             [H2|T2] = T,
                             Match2NdChar = re:run([H2], "[0-9|a-f|A-F]"),
                             case Match2NdChar of
                                 {match, _} -> check_RDN_valid(T2);
                                 nomatch -> false
                             end;
                         nomatch -> false
                     end
            end;
        _ ->
            check_RDN_valid(NameTail)
    end.

default_ports() ->
    [{http, sysEnv:get_port_conf(http)},
     {cmp, sysEnv:get_port_conf(cmp)}, 
     {ftpes, ?FTPES_PORT}].
    %% In case of supported scep protocol add default port for scep
    %% well known port for scep is 1640, update port.conf by changing file:
    %% SYS/esrc/sys/make_release.escript

createMo([Class | ParentDnRev], _KeyAttrName, KeyValue, InitAttrs, _TransId) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

%deleteMo(DnRev, TxHandle) ->
deleteMo([_,<<"TrustCategory">>|_] = DnRev, _) ->
    %% Standard table op
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo([_,<<"NodeCredential">>|_] = DnRev, _) ->
    ThisRef = dnrev_to_moref(DnRev),
    Key = moref_to_key(ThisRef),

    [NC] = mnesia:read({nodeCredential, Key}),
    case NC#nodeCredential.enrollmentProgress of
	undefined ->
	    ok;
	Progress ->
	    case Progress#'AsyncActionProgress'.state of
		?ActionStateType_RUNNING -> mnesia:abort(<<"Enrollment is running">>);
		?ActionStateType_CANCELLING -> mnesia:abort(<<"Enrollment is cancelling">>);
		_ -> ok
	    end
    end,
    %% Standard Table op
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table),
    ok;
deleteMo([_,<<"EnrollmentAuthority">>|_] = DnRev, _) ->
    %% Standard table op
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo([_,<<"EnrollmentServerGroup">>|_] = DnRev, _) ->
    ThisRef = dnrev_to_moref(DnRev),
    lists:foreach(
        fun(ES) ->
                {_,_,_,_,_,Index} = ES#enrollmentServer.enrollmentServerId,
                ES_DnRev = [list_to_binary(Index)] ++ [<<"EnrollmentServer">>] ++ DnRev,
                Table = table(comsaGeneric:class(ES_DnRev)),
                comsaGeneric:delete(ES_DnRev, Table)
        end, certNcServer:get_enrollment_servers(ThisRef)),
    %% Standard Table op
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table),
    ok;
deleteMo(DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table).


table("CertM") -> certM;
table("TrustedCertificate") -> trustedCertificate;
table("NodeCredential") -> nodeCredential;
table("VendorCredential") -> vendorCredential;
table("EnrollmentServerGroup") -> enrollmentServerGroup;
table("EnrollmentAuthority") -> enrollmentAuthority;
table("TrustCategory") -> trustCategory;
table("ChainCertificate") -> chainCertificate;
table("CertMCapabilities") -> certMCapabilities;
table("EnrollmentServer") -> enrollmentServer.

types(certM) -> ?certM_types;
types(trustedCertificate) -> ?trustedCertificate_types;
types(nodeCredential) -> ?nodeCredential_types;
types(vendorCredential) -> ?vendorCredential_types;
types(enrollmentServerGroup) -> ?enrollmentServerGroup_types;
types(enrollmentAuthority) -> ?enrollmentAuthority_types;
types(trustCategory) -> ?trustCategory_types;
types(chainCertificate) -> ?chainCertificate_types;
types(certMCapabilities) -> ?certMCapabilities_types;
types(enrollmentServer) -> ?enrollmentServer_types.

values([{<<"trustedCertificates">> = Name, {_, Value}} | Tail]) ->
    [{Name, [Value]} | values(Tail)];
values([{"trustedCertificates" = Name, {_, Value}} | Tail]) ->
    [{Name, [Value]} | values(Tail)];
values([{Name, {9, Value}} | Tail]) ->
    [{Name, sysUtil:term_to_string(Value)} | values(Tail)];
values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

%%% Actions on CertM MOC
action(Name, DnRev, NamedParams, TransId) ->
    Parameters =
        case Name of
            <<"installTrustedCertFromUri">> ->
                [getTypeValue(<<"uri">>, NamedParams),
                 getTypeValue(<<"uriPassword">>, NamedParams),
                 getTypeValue(<<"fingerprint">>, NamedParams)];
            <<"installCredentialFromUri">> ->
                [getTypeValue(<<"uri">>, NamedParams),
                 getTypeValue(<<"uriPassword">>, NamedParams),
                 getTypeValue(<<"credentialPassword">>, NamedParams),
                 getTypeValue(<<"fingerprint">>, NamedParams)];
            <<"startOfflineCsrEnrollment">> ->
                [getTypeValue(<<"uri">>, NamedParams),
                 getTypeValue(<<"uriPassword">>, NamedParams)];
	    <<"removeTrustedCert">> ->
		[getTypeValue(<<"trustedCert">>, NamedParams)];
	    <<"startOnlineEnrollment">> ->
		NamedParams;
            _ ->
                []
        end,
    action([Name|DnRev], Parameters, TransId).

getTypeValue(Name, NamedParams) ->
    case lists:keyfind(Name, 1, NamedParams) of
        false ->
            erlang:error(no_such_parameter, [Name, NamedParams]);
        {_, TypeValue} when is_tuple(TypeValue) ->
            TypeValue;
        {_, [TypeValue]} ->
            TypeValue;
        {_, undefined} ->
            undefined;
        {_, [_|_]=TypeValues} ->
            erlang:error(multiple_parameter_values, [Name, TypeValues])
    end.

action([<<"installTrustedCertFromUri">>|_], Parameters, _) ->
    {Uri, Passwd, Fingerprint} =
    case Parameters of
        [{9, UriBin}, {9, PasswdBin}, FingerprintBin] ->
            {binary_to_list(UriBin), binary_to_list(PasswdBin),
                format_fingerprint(FingerprintBin)};
        [undefined, undefined, FingerprintBin] ->
            {undefined, undefined, format_fingerprint(FingerprintBin)};
        [undefined, {9, PasswdBin}, FingerprintBin] ->
            {undefined, binary_to_list(PasswdBin),
                format_fingerprint(FingerprintBin)};
         [{9, UriBin}, undefined, FingerprintBin] ->
            {binary_to_list(UriBin), undefined,
                format_fingerprint(FingerprintBin)}
    end,
    certServer:install_certificate(Uri, Passwd, Fingerprint),
    {10, true};
action([<<"cancel">>|_], _, _) ->
    certServer:cancel(),
    {10, true};
action([<<"downloadCrl">>|_], _, _) ->
    certServer:download_crl(),
    {10, true};
action([<<"removeTrustedCert">>|_], Parameters, _) ->
    [{11, MoRef}] = Parameters,
    certServer:remove_certificate(MoRef),
    {10, true};

%%% Actions on NodeCredential MOC
action([<<"cancelEnrollment">>|DnRev], _, _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    certNcServer:cancel_enrollment(Key),
    {10, true};
action([<<"startOnlineEnrollment">>|DnRev], Parameters, _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    [NC] = read_nc(Key),
    Start =
    case NC#nodeCredential.enrollmentProgress of
        undefined ->
            ok;
        Progress ->
            case Progress#'AsyncActionProgress'.state of
                ?ActionStateType_RUNNING ->
                    nok;
                ?ActionStateType_CANCELLING ->
                    nok;
                _ ->
                    ok
            end
    end,

    case Start of
        ok ->
            Challenge =
            case Parameters of
                [{<<"challengePassword">>,undefined}] ->
                    undefined;
                [{<<"challengePassword">>,{9,ChallengeBin}}] ->
                    binary_to_list(ChallengeBin);
                [undefined] ->
                    undefined
            end,
            certNcServer:start_online_enrollment(Key, Challenge),
            {10, true};
        _ ->
            {error, <<"Enrollment already ongoing. Wait until it is ready.">>}
    end;

action([<<"startOfflineCsrEnrollment">>|DnRev], Parameters, _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    [NC] = read_nc(Key),
    Start =
    case NC#nodeCredential.enrollmentProgress of
        undefined ->
            ok;
        Progress ->
            case Progress#'AsyncActionProgress'.state of
                ?ActionStateType_RUNNING ->
                    nok;
                ?ActionStateType_CANCELLING ->
                    nok;
                _ ->
                    ok
            end
    end,

    case Start of
        ok ->
            [{9, UriBin}, {9, PasswordBin}] = Parameters,
            certNcServer:start_offline_enrollment(Key, binary_to_list(UriBin),
                                                  binary_to_list(PasswordBin)),
            {10, true};
        _ ->
            {error, <<"Enrollment already ongoing. Wait until it is ready.">>}
    end;

action([<<"installCredentialFromUri">>|DnRev], Parameters, _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    [NC] = read_nc(Key),
    Start =
    case NC#nodeCredential.enrollmentProgress of
        undefined ->
            ok;
        Progress ->
            case Progress#'AsyncActionProgress'.state of
                ?ActionStateType_RUNNING ->
                    nok;
                ?ActionStateType_CANCELLING ->
                    nok;
                _ ->
                    ok
            end
    end,

    case Start of
        ok ->
            {Uri, UriPwd, CredPwd, Fingerprint} =
            case Parameters of
                [{9, UriBin}, {9, PasswdBin}, {9, CredPwdBin}, FingerprintBin] ->
                    {binary_to_list(UriBin), binary_to_list(PasswdBin),
                    binary_to_list(CredPwdBin), format_fingerprint(FingerprintBin)};
                [{9, UriBin}, _, {9, CredPwdBin}, FingerprintBin] ->
                    {binary_to_list(UriBin), undefined, binary_to_list(CredPwdBin),
                    format_fingerprint(FingerprintBin)}
            end,
            certNcServer:install_credential(Key, Uri, UriPwd, CredPwd, Fingerprint),
            {10, true};
         _ ->
            {error, <<"Enrollment already ongoing. Wait until it is ready.">>}
    end;

action(_DnRev, _Parameters, _TransId) ->
    {10, false}.


format_fingerprint(undefined) ->
    undefined;
format_fingerprint({_,<<"NULL">>}) ->
    undefined;
format_fingerprint({_,Bin}) ->
    binary_to_list(Bin).


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

read_nc(Key) ->
    Fun = fun() -> mnesia:read(nodeCredential, Key) end,
    Ret =
    case mnesia:is_transaction() of
        true -> Fun();
        false -> mnesia:transaction(Fun)
    end,
    NC =
    case Ret of
        {atomic, Cred} -> Cred;
        _ -> Ret
    end,
    NC.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
