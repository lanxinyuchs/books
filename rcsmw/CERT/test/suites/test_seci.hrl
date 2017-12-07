%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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

%% must match definitions in cert_seci.h
-define(SECI_OK,                        0).
-define(SECI_NOT_FOUND,                 1).
-define(SECI_ERROR,                     2).
-define(SECI_INVALID_PARAMETER_RESULTP, 3).
-define(SECI_SEND_ERROR,                4).
-define(SECI_RECEIVE_ERROR,             5).
-define(SECI_DELETE_FAILED_WRONG_ID,    6).
-define(SECI_SECI_UNINITIALIZED_SUB,    7).
-define(SECI_VERIFY_PEER_VALID,         8).
-define(SECI_VERIFY_PEER_NOT_VALID,     9).
-define(SECI_VERIFY_PEER_UNKNOWN,      10).

%% must match definition in ift_app: master.h
-define(SECI, 19).

%% must match definitions in ift_app: test_seci.c
-define(SECI_GET_CERT,        1).
-define(SECI_READ,            2).
-define(SECI_WRITE,           3).
-define(SECI_DELETE,          4).
-define(SECI_LOG,             5).
-define(SECI_SUB_INIT,        6).
-define(SECI_ADD_SUB,         7).
-define(SECI_DEL_SUB,         8).
-define(SECI_GET_SUB_EVENT,   9).
-define(SECI_GET_VC,         10).
-define(SECI_GET_NC,         11).
-define(SECI_GET_TCAT,       12).
-define(SECI_VERIFY_PEER,    13).
-define(SECI_GET_VCERT,      14).
-define(SECI_VERIFY_PEER_VC, 15).
-define(SECI_ENCODE,         16).
-define(SECI_DECODE,         17).

-define(SECI_SECURITY_LOG,    0).
-define(SECI_AUDIT_TRAIL_LOG, 1).

-define(SECI_SEVERITY_EMERGANCY, 0).
-define(SECI_SEVERITY_ALERT,     1).
-define(SECI_SEVERITY_CRITICAL,  2).
-define(SECI_SEVERITY_ERROR,     3).
-define(SECI_SEVERITY_WARNING,   4).
-define(SECI_SEVERITY_NOTICE,    5).
-define(SECI_SEVERITY_INFO,      6).
-define(SECI_SEVERITY_DEBUG,     7).


