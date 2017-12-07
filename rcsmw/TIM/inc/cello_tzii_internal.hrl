%%% Generated file; do not edit
%%% Source: csrc/cello_tzii_internal.h
-define(SERVER_UNAVAILABLE, 0).
-define(SERVER_INITIATING, 1).
-define(SERVER_AVAILABLE, 2).
-define(SERVER_TERMINATING, 3).
-define(SERVER_SUSPENDED, 4).
-define(CEC_SIGNATURE, "TZII").
-define(CEC_RESPONSE_OK, "ok").
-define(CEC_RESPONSE_UNKNOWN, "unknown").
-define(CEC_OK, 0).
-define(CEC_UNKNOWN, 1).
-define(CEC_UNEXPECTED, 2).
-define(CEC_ERROR_OPEN, 3).
-define(CEC_ERROR_SEND, 4).
-define(CEC_ERROR_RECEIVE, 5).
-define(CEC_ERROR_CLOSE_A, 6).
-define(CEC_ERROR_CLOSE_B, 7).
-define(CEC_ERROR_CLOSE_C, 8).
-define(CEC_REQUEST_INIT_SERVICE, 100).
-define(CEC_REQUEST_TERM_SERVICE, 101).
-define(CEC_REQUEST_SUBSCRIBE_DST, 102).
-define(CEC_REQUEST_SUBSCRIBE_LEAP_SEC, 103).
-define(CEC_REQUEST_CHANGE_FEEDBACK, 104).
-define(CEC_REQUEST_PROCEED, 105).
-define(CEC_REQUEST_SET_MAILBOX, 106).
