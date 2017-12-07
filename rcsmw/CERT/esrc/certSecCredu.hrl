%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certSecCredu.hrl %
%%% @author emirbos
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/1

%%% @doc ==sec_credu_api.hrl==
%%% Header file for certSecCredu.erl
%%% @end

-vsn('/main/R11A/R12A/1').
-date('2017-10-25').
-author('emirbos').

%%% ----------------------------------------------------------
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
%%% ------------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% ------------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    --------------------------------------
%%% R11A/1     2017-08-28 ekurnik     Created
%%% R11A/2     2017-08-31 enekdav     Small changes to directory definitions
%%% R11A/4     2017-09-01 enekdav     Separated mo_ref and mo_id
%%% R11A/5     2017-09-04 edartop     Added macros for Data serilization
%%% R11A/6     2017-09-04 ekurnik     Added DN macros
%%% R11A/7     2017-09-07 emirbos     Added macros for handle_sec_credu funs
%%% R11A/9     2017-09-08 evadumb     Added RESP_STAT_ERR_UNKNOWN_REQ_TYPE,
%%%                                   SEC_CREDU_RESP_SIG
%%% R11A/10    2017-09-15 emirbos     Added macros for handle_cert_event
%%% R11A/11    2017-10-18 evadumb     Updated subscriber record
%%% R11A/12    2017-10-19 enekdav     LOG cleanup 
%%% R12A/1     2017-10-24 emirbos     Added REQ_UNKNOWN
%%% ------------------------------------------------------------------------


%%%===================================================================
%%% Macros
%%%===================================================================

-define(TMP_DIR, sysEnv:tmp_dir()).
-define(SEC_CREDU_DIR,   ?TMP_DIR ++ "/sec_credu_api").
-define(NC_CERT_DIR,     ?SEC_CREDU_DIR ++ "/certs").
-define(NC_KEY_DIR,      ?SEC_CREDU_DIR ++ "/private").
-define(TCAT_ROOT_DIR,   ?SEC_CREDU_DIR ++ "/cacerts").
-define(DIRS, [?SEC_CREDU_DIR, 
               ?NC_CERT_DIR, 
               ?NC_KEY_DIR, 
               ?TCAT_ROOT_DIR]).

-define(NC_CERT_FILENAME(NcId), "nc" ++ NcId ++ ?NC_CERT_EXT).
-define(NC_KEY_FILENAME(NcId), "nc" ++ NcId ++ ?NC_KEY_EXT).
-define(TCAT_DIRNAME(TcatId), "/tcat" ++ TcatId).
-define(TC_CERT_FILENAME(TcId), "tc" ++ TcId ++ ?TC_CERT_EXT).

-define(NC_CERT_EXT, ".pem").
-define(NC_KEY_EXT, ".key").
-define(TC_CERT_EXT, ".pem").

-define(SEC_CREDU_REQ_SIG, 16#18A4ED0).
-define(SEC_CREDU_RESP_SIG, 16#18A4ED1).
-define(SEC_CREDU_EVENT_SIG, 16#18A4ED2).

-define(REQ_INITIALIZE, 1).
-define(REQ_FINALIZE, 2).
-define(REQ_SELECTION_OBJECT_GET, 3).
-define(REQ_NC_SUBSCRIBE, 4).
-define(REQ_NC_UNSUBSCRIBE, 5).
-define(REQ_NC_CERT_GET, 6).
-define(REQ_NC_KEY_GET, 7).
-define(REQ_TCAT_SUBSCRIBE, 8).
-define(REQ_TCAT_UNSUBSCRIBE, 9).
-define(REQ_TCAT_GET, 10).
-define(REQ_UNKNOWN, 11).

-define(RESP_STAT_OK, 0).
-define(RESP_STAT_ERR_ID_NOT_FOUND, 1).
-define(RESP_STAT_ERR_SUB_ID_NOT_FOUND, 2).
-define(RESP_STAT_ERR_MO_REF_NOT_FOUND, 3).
-define(RESP_STAT_ERR_NC_NOT_INSTALLED, 4).
-define(RESP_STAT_ERR_TCAT_EMPTY, 5).
-define(RESP_STAT_ERR_UNKNOWN_REQ_TYPE, 6).

-define(NC_EVENT, 1).
-define(TCAT_EVENT, 2).
-define(FINALIZE_EVENT, 3).

-define(CERTM_DN, "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1").

-define(NODE_CREDENTIAL_DN(NcId), ?CERTM_DN ++ ",NodeCredential=" ++ NcId).
-define(TRUSTED_CERTIFICATE_DN(TcId), ?CERTM_DN ++ ",TrustedCertificate=" ++ TcId).
-define(TRUST_CATEGORY_DN(TcatId), ?CERTM_DN ++ ",TrustCategory=" ++ TcatId).

-define(LOG(Msg), ?LOG(Msg, [])).
-define(LOG(_Format, _Args), ok).

-define(SECCREDUVERSION, "A11").

-define(CERT_EVENT_OFFSET, 100).

%%%===================================================================
%%% Type definitions
%%%===================================================================

-type id() :: integer().
-type socket() :: port().
-type mo_ref() :: binary().
-type mo_id() :: string().
-type error_reason() :: id_not_found | sel_obj_not_found | sub_id_not_found | 
                        mo_ref_not_found | nc_not_installed | tcat_empty | 
                        unknown_request.
-type sec_credu_format() :: pem | filepath.
-type sec_credu_event_type() :: nc_event | tcat_event | final_event.
-type sec_credu_resp_status() :: ok | error_reason().

%%%===================================================================
%%% Records
%%%===================================================================

-record (state, {subscribers = []                %% list of subscribers
        }).

-record (subscriber, {id,                        %% unique client id
                      select_object = undefined, %% socket used for async messages
                      subscriptions = [],        %% subscribe_id to dn mapping
                      communication_socket = undefined %% socket used for identification
}).

-record (subscription, {subscribe_id,            %% unique integer
                        mo_ref                   %% NC or Tcat DN
        }).
