%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaLkf.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R4A/2
%%% 
%%% @doc ==Verification module for certificates==
%%% This module implements the verify part of the License Key File 

-module(lmaLkf).
-vsn('/main/R2A/R4A/2').
-date('2015-06-15').
-author('etxpejn').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% Rev        Date        Name      What
%%% -----      ----------  --------  -------------------------
%%% R2A/0      2014-03-18  etxpejn   Created
%%% R2A/4      2014-03-25  etxpejn   Changed case catch to try
%%% R4A/2      2015-06-15  etxpejn   Added load_test_mom
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([verify_lkf_file/1]).
-export([load_test_mom/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("public_key/include/public_key.hrl").
-include("lma.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
load_test_mom() ->
    case mnesia:dirty_read(lma_KFLocation, 1) of
	[Obj] ->
	    case Obj#lma_KFLocation.testMom of
		undefined ->
		    false;
		TrueOrFalse ->
		    TrueOrFalse
	    end;
	[] ->
	    false
    end.
	
%%% ----------------------------------------------------------
%%% @doc verify certificates
%%% @end
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type verify_lkf_file(Path)->        %#
%%%     valid | {failed, Reason} | {error, Reason}                        %#
%%% Input: 
%%% Output: 
%%% @private
%%% Exceptions: 
%%% Description: Fetch a LKF-file and verify its signature
%%% ----------------------------------------------------------
verify_lkf_file(Path) ->
    case file:read_file(Path) of
	{ok, LicenseData} ->
            try verify_lkf_file0(LicenseData) of
		{Type, Reason} ->
                    {Type, Reason};
                valid ->
                    valid
	    catch error:Reason ->
		    {error, Reason}
	    end;
	{error, _Reason} ->
            {error, file_not_found}
    end.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
verify_lkf_file0(LicenseData) ->
    ProdCert = get_cert(LicenseData, "prodcert"),
    CaCert   = get_cert(LicenseData, "cacert"),
    verify_lkf_file1(ProdCert, CaCert, LicenseData).


%% verify_lkf_file1({error, faulty_cert},_,_) ->
%%     {error, faulty_prod_cert};
%% verify_lkf_file1(_,{error, faulty_cert},_) ->
%%     {error, faulty_ca_cert};
verify_lkf_file1(ProdCert, CaCert, LicenseData) ->
    PublicKey = validate_path([CaCert, ProdCert]),
    verify_lkf_file2(PublicKey, LicenseData).

verify_lkf_file2({error, Reason}, _LicenseData) ->
    {error, Reason};
verify_lkf_file2(PublicKey, LicenseData) ->
    Body = get_body(LicenseData),
    Signature = get_signature(LicenseData),
    case public_key:verify(Body, 'sha', Signature, PublicKey) of
        true ->
            valid;
        _ ->
            {failed, sign_faulty}
    end.

  
get_cert(LicenseData, Tag) ->
    StartTag = list_to_binary(["<", Tag, ">"]),
    EndTag = list_to_binary(["</", Tag, ">"]),
    {Start0, Length0} = binary:match(LicenseData, StartTag),
    PemStart = Start0+Length0, {Start1, _} =
    binary:match(LicenseData, EndTag,
        [{scope, {PemStart, byte_size(LicenseData)-PemStart}}]),
    PemEnd = Start1-1,
    Pem = binary:part(LicenseData, {PemStart, PemEnd-PemStart}),
    [{'Certificate', Cert, not_encrypted}] = public_key:pem_decode(Pem),
    Cert.


validate_path(CertList) ->
    case public_key:pkix_path_validation(rootCert(), CertList,
        [{verify_fun, validationFunState()}]) of
        {ok, {{_Algorithm, PublicKey, _PublicKeyParams}, _PolicyTree}} ->
            PublicKey;
        {error, Reason} ->
            {error, Reason}
    end.


validationFunState() ->
    Fun =
    fun(_, {bad_cert, missing_basic_constraint}, UserState) ->
            %% The Basic Constrains extension is not mandatory
            {valid, UserState};
        (_, {bad_cert, _} = Reason, _) ->
            {fail, Reason};
        (_, {extension, #'Extension'{extnID = ?'id-ce-subjectKeyIdentifier'}},
            UserState) ->
            {valid, UserState};
        (_, {extension, #'Extension'{extnID = ?'id-ce-basicConstraints'}},
            UserState) ->
            {valid, UserState};
        (_, {extension, #'Extension'{extnID = ?'id-ce-nameConstraints'}},
            UserState) ->
            {valid, UserState};
        (_, {extension, _Ext}, UserState) ->
            %% FIXME check for high prio not supported
            %io:format("Ext: ~p~n", [Ext]),
            {unknown, UserState};
        (OtpCert, valid, UserState) ->
            Version = get_version(OtpCert),
            if Version =/= v3 ->
                    {fail, wrong_version};
                true ->
                    {valid, UserState}
            end;
        (OtpCert, valid_peer, UserState) ->
            Version = get_version(OtpCert),
            if Version =/= v3 ->
                    {fail, wrong_version};
                true ->
                    {valid, UserState}
            end
    end,
    State = [],
    {Fun, State}.

get_version(#'OTPCertificate'{tbsCertificate = TBSCert}) ->
    TBSCert#'OTPTBSCertificate'.version.


get_body(LicenseData) ->
    {Start0, _} = binary:match(LicenseData, <<"<body">>),
    PemStart = Start0,
    {Start1, Length1} = binary:match(LicenseData, <<"</body>">>),
    Pem = binary:part(LicenseData, {PemStart, Start1 + Length1 - PemStart}),
    remove_whitespace(Pem).


get_signature(LicenseData) ->
    Size = byte_size(LicenseData),
    {Start0, Length0} = binary:match(LicenseData, <<"<PKIsignature">>),
    Next = Start0+Length0,
    {Start1, Length1} = binary:match(LicenseData, <<">">>,
        [{scope, {Next, Size-Next}}]),
    PemStart = Start1+Length1,
    {Start2, _} = binary:match(LicenseData, <<"</PKIsignature>">>,
        [{scope, {PemStart, Size-PemStart}}]),
    base64:decode(binary:part(LicenseData, {PemStart, Start2-PemStart})).


remove_whitespace(Binary) ->
    <<<<X>> || <<X>> <= Binary, X>32>>.


%% This is the real root certificate
rootCert() ->
    <<16#30,16#82,16#03,16#b9,16#30,16#82,16#02,16#a1,16#a0,16#03,16#02,16#01,
      16#02,16#02,16#01,16#01,16#30,16#0d,16#06,16#09,16#2a,16#86,16#48,16#86,
      16#f7,16#0d,16#01,16#01,16#05,16#05,16#00,16#30,16#81,16#82,16#31,16#0b,
      16#30,16#09,16#06,16#03,16#55,16#04,16#06,16#13,16#02,16#53,16#45,16#31,
      16#0b,16#30,16#09,16#06,16#03,16#55,16#04,16#07,16#13,16#02,16#4c,16#49,
      16#31,16#14,16#30,16#12,16#06,16#03,16#55,16#04,16#0a,16#13,16#0b,16#45,
      16#72,16#69,16#63,16#73,16#73,16#6f,16#6e,16#20,16#41,16#42,16#31,16#17,
      16#30,16#15,16#06,16#03,16#55,16#04,16#0b,16#13,16#0e,16#4c,16#69,16#63,
      16#65,16#6e,16#73,16#65,16#20,16#43,16#65,16#6e,16#74,16#65,16#72,16#31,
      16#37,16#30,16#35,16#06,16#03,16#55,16#04,16#03,16#13,16#2e,16#52,16#6f,
      16#6f,16#74,16#20,16#66,16#6f,16#72,16#20,16#53,16#69,16#67,16#6e,16#69,
      16#6e,16#67,16#20,16#6c,16#69,16#63,16#65,16#6e,16#73,16#65,16#20,16#66,
      16#69,16#6c,16#65,16#73,16#20,16#43,16#41,16#58,16#20,16#31,16#30,16#36,
      16#20,16#30,16#30,16#38,16#34,16#2f,16#33,16#30,16#30,16#22,16#18,16#0f,
      16#32,16#30,16#30,16#37,16#30,16#32,16#32,16#36,16#30,16#30,16#30,16#30,
      16#30,16#30,16#5a,16#18,16#0f,16#32,16#30,16#33,16#37,16#30,16#32,16#32,
      16#36,16#30,16#30,16#30,16#30,16#30,16#30,16#5a,16#30,16#75,16#31,16#0b,
      16#30,16#09,16#06,16#03,16#55,16#04,16#06,16#13,16#02,16#53,16#45,16#31,
      16#14,16#30,16#12,16#06,16#03,16#55,16#04,16#0a,16#13,16#0b,16#45,16#72,
      16#69,16#63,16#73,16#73,16#6f,16#6e,16#20,16#41,16#42,16#31,16#17,16#30,
      16#15,16#06,16#03,16#55,16#04,16#0b,16#13,16#0e,16#4c,16#69,16#63,16#65,
      16#6e,16#73,16#65,16#20,16#43,16#65,16#6e,16#74,16#65,16#72,16#31,16#37,
      16#30,16#35,16#06,16#03,16#55,16#04,16#03,16#13,16#2e,16#52,16#6f,16#6f,
      16#74,16#20,16#66,16#6f,16#72,16#20,16#53,16#69,16#67,16#6e,16#69,16#6e,
      16#67,16#20,16#6c,16#69,16#63,16#65,16#6e,16#73,16#65,16#20,16#66,16#69,
      16#6c,16#65,16#73,16#20,16#43,16#41,16#58,16#20,16#31,16#30,16#36,16#20,
      16#30,16#30,16#38,16#34,16#2f,16#33,16#30,16#30,16#82,16#01,16#22,16#30,
      16#0d,16#06,16#09,16#2a,16#86,16#48,16#86,16#f7,16#0d,16#01,16#01,16#01,
      16#05,16#00,16#03,16#82,16#01,16#0f,16#00,16#30,16#82,16#01,16#0a,16#02,
      16#82,16#01,16#01,16#00,16#c8,16#65,16#b8,16#a3,16#f0,16#ba,16#34,16#d4,
      16#2a,16#04,16#63,16#67,16#90,16#a6,16#c5,16#27,16#d1,16#17,16#c4,16#3d,
      16#06,16#c3,16#ee,16#48,16#35,16#80,16#66,16#0a,16#ae,16#96,16#75,16#1c,
      16#25,16#61,16#e6,16#74,16#8a,16#0d,16#db,16#da,16#f7,16#e2,16#b1,16#6e,
      16#94,16#3e,16#fd,16#cd,16#89,16#a8,16#1e,16#b4,16#e6,16#df,16#8a,16#de,
      16#7e,16#87,16#94,16#3b,16#23,16#91,16#3e,16#6e,16#eb,16#de,16#e2,16#b9,
      16#33,16#81,16#3c,16#25,16#c4,16#9d,16#ab,16#4a,16#9a,16#ea,16#4a,16#68,
      16#5a,16#5e,16#34,16#32,16#23,16#0c,16#a3,16#81,16#2b,16#f5,16#94,16#51,
      16#52,16#07,16#3e,16#f8,16#e4,16#59,16#2d,16#06,16#ea,16#64,16#f8,16#0b,
      16#2b,16#82,16#7b,16#46,16#75,16#a7,16#91,16#6d,16#77,16#96,16#ef,16#bc,
      16#53,16#83,16#43,16#e2,16#06,16#28,16#dd,16#df,16#94,16#ff,16#fc,16#76,
      16#0e,16#86,16#7b,16#6c,16#d7,16#40,16#45,16#09,16#7f,16#c5,16#e9,16#c0,
      16#ac,16#1f,16#3c,16#5b,16#54,16#3f,16#0d,16#73,16#6a,16#ae,16#b7,16#a3,
      16#43,16#35,16#d3,16#02,16#a6,16#8a,16#77,16#8b,16#8c,16#2d,16#ba,16#98,
      16#5f,16#97,16#0f,16#8e,16#8a,16#8a,16#f1,16#02,16#96,16#41,16#aa,16#c0,
      16#f9,16#95,16#de,16#3c,16#0a,16#9d,16#9d,16#76,16#e4,16#ad,16#6f,16#cf,
      16#21,16#3b,16#a7,16#3e,16#0f,16#8d,16#5a,16#ba,16#f7,16#92,16#b5,16#80,
      16#94,16#a3,16#e7,16#fc,16#34,16#b4,16#b7,16#09,16#06,16#4e,16#be,16#56,
      16#98,16#fd,16#ee,16#aa,16#7e,16#12,16#c2,16#ef,16#0e,16#59,16#97,16#94,
      16#a8,16#ad,16#5a,16#9a,16#ee,16#c7,16#b2,16#a4,16#73,16#6f,16#13,16#c5,
      16#08,16#2f,16#b9,16#fb,16#b5,16#55,16#c0,16#8d,16#e1,16#85,16#41,16#ea,
      16#f3,16#bd,16#76,16#d2,16#83,16#90,16#93,16#e5,16#02,16#03,16#01,16#00,
      16#01,16#a3,16#42,16#30,16#40,16#30,16#1d,16#06,16#03,16#55,16#1d,16#0e,
      16#04,16#16,16#04,16#14,16#32,16#54,16#16,16#8f,16#d8,16#09,16#54,16#7a,
      16#99,16#77,16#27,16#34,16#4c,16#5a,16#a0,16#a0,16#59,16#34,16#44,16#b8,
      16#30,16#12,16#06,16#03,16#55,16#1d,16#13,16#01,16#01,16#ff,16#04,16#08,
      16#30,16#06,16#01,16#01,16#ff,16#02,16#01,16#01,16#30,16#0b,16#06,16#03,
      16#55,16#1d,16#0f,16#04,16#04,16#03,16#02,16#01,16#06,16#30,16#0d,16#06,
      16#09,16#2a,16#86,16#48,16#86,16#f7,16#0d,16#01,16#01,16#05,16#05,16#00,
      16#03,16#82,16#01,16#01,16#00,16#6f,16#95,16#2e,16#f3,16#c3,16#97,16#c7,
      16#b2,16#96,16#5d,16#61,16#d1,16#c6,16#9a,16#8e,16#c4,16#5a,16#8b,16#f0,
      16#ce,16#50,16#59,16#66,16#6b,16#d8,16#a4,16#bf,16#6d,16#7d,16#84,16#61,
      16#17,16#4c,16#df,16#7b,16#3c,16#50,16#7b,16#a2,16#b1,16#56,16#a3,16#81,
      16#06,16#c9,16#98,16#72,16#1b,16#5c,16#e0,16#75,16#29,16#30,16#6d,16#49,
      16#7e,16#eb,16#e3,16#b7,16#ae,16#94,16#85,16#14,16#e7,16#30,16#c8,16#71,
      16#9d,16#2b,16#10,16#d4,16#68,16#3a,16#24,16#45,16#c7,16#4c,16#15,16#96,
      16#82,16#21,16#d5,16#69,16#5a,16#be,16#86,16#62,16#1f,16#2b,16#50,16#3f,
      16#b8,16#34,16#ed,16#4f,16#6d,16#69,16#68,16#c7,16#93,16#42,16#81,16#44,
      16#68,16#3e,16#17,16#60,16#e7,16#80,16#57,16#2c,16#c8,16#3b,16#e5,16#36,
      16#67,16#a0,16#86,16#ca,16#62,16#a1,16#0e,16#be,16#05,16#5f,16#8b,16#4b,
      16#f7,16#86,16#e7,16#a9,16#d9,16#c9,16#e6,16#b0,16#83,16#bd,16#91,16#1f,
      16#9a,16#d2,16#00,16#5a,16#6f,16#58,16#c3,16#95,16#1f,16#95,16#5f,16#39,
      16#6c,16#ba,16#c7,16#4a,16#0e,16#a2,16#72,16#30,16#42,16#7d,16#74,16#fb,
      16#f4,16#2c,16#dc,16#17,16#4f,16#ff,16#e8,16#e3,16#77,16#02,16#9d,16#c0,
      16#f7,16#55,16#4c,16#37,16#ec,16#8c,16#1c,16#99,16#fd,16#f5,16#f2,16#dc,
      16#80,16#76,16#e0,16#d1,16#6a,16#44,16#39,16#d6,16#53,16#7d,16#74,16#a0,
      16#5c,16#6e,16#40,16#d3,16#fa,16#38,16#bb,16#8e,16#9c,16#97,16#7f,16#5e,
      16#9b,16#ef,16#34,16#94,16#0d,16#cb,16#89,16#8f,16#af,16#60,16#00,16#89,
      16#f9,16#8b,16#66,16#2a,16#65,16#a7,16#7c,16#1e,16#83,16#4e,16#a5,16#4d,
      16#76,16#bd,16#ab,16#ad,16#b5,16#f4,16#48,16#a7,16#d4,16#8d,16#bd,16#d5,
      16#35,16#4f,16#88,16#e7,16#b3,16#e6,16#9d,16#43,16#34>>.


