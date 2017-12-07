%% must match definition in master.h of ift_app
-define(BUTI, 15).

%% must match definitions in test_buti.c
-define(CelloButi_initiateMemory,         1).
-define(CelloButi_initiateService,        2).
-define(CelloButi_internal,               3).
-define(CelloButi_freeMemory,             4).
-define(CelloButi_terminateService,       5).
-define(CelloButi_subscribeButtonEvent,   6).
-define(CelloButi_unsubscribeButtonEvent, 7).
-define(CelloButi_changeFeedBackMode,     8).

%% must match values in cello_buti.h
-define(CELLO_BUTI_NO_PV,                        0).
-define(CELLO_BUTI_PV1,                          1).

-define(CELLO_BUTI_NO_FEEDBACK_BLINK_MODE,       0).
-define(CELLO_BUTI_FEEDBACK_BLINK_MODE,          1).

-define(CELLO_BUTI_OK,                           0).
-define(CELLO_BUTI_ERROR_SERVER_NOT_UNAVAILABLE, 1).
-define(CELLO_BUTI_SERVER_NOT_FOUND,             2).
-define(CELLO_BUTI_UNKNOWN_SIGNAL,               3).
-define(CELLO_BUTI_ERROR_SERVER_NOT_AVAILABLE,   4).
-define(CELLO_BUTI_INTERNAL_ERROR,               5).
-define(CELLO_BUTI_INVALID_PV,                   6).
-define(CELLO_BUTI_MEMORY_NOT_INITIATED,         7).
-define(CELLO_BUTI_SERVICE_BUSY,                 8).

-define(CELLO_BUTI_BUTTON_PRESSED,               0).
-define(CELLO_BUTI_BUTTON_SHORT_RELEASE,         1).
-define(CELLO_BUTI_BUTTON_MEDIUM_RELEASE,        2).

%% must match definitions in test_buti.sig
-define(CELLO_BUTI_INITIATE_SERVICE_CFM,         16#60D36).
-define(CELLO_BUTI_INITIATE_SERVICE_REJ,         16#60D37).
-define(CELLO_BUTI_SUBSCRIBE_BUTTON_EVENT_CFM,   16#60D38).
-define(CELLO_BUTI_UNSUBSCRIBE_BUTTON_EVENT_CFM, 16#60D39).
-define(CELLO_BUTI_CHANGE_FEEDBACK_MODE_CFM,     16#60D3A).
-define(CELLO_BUTI_CHANGE_FEEDBACK_MODE_REJ,     16#60D3B).
-define(CELLO_BUTI_TERMINATE_SERVICE_CFM,        16#60D3C).
-define(CELLO_BUTI_EVENT_IND,                    16#60D3D).
