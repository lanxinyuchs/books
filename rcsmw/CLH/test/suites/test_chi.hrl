%%% %CCaseFile:	test_chi.hrl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R3A/R4A/R5A/1

%% must match definition in master.h of ift_app
-define(CHI, 21).

%% must match definitions in test_chi.c
-define(Chi_get_state,    1).   % Deprecated
-define(Chi_subscribe,    2).   % Deprecated
-define(Chi_unsubscribe,  3).   % Deprecated
-define(Chi_subscribeOpState,    4).
-define(Chi_unsubscribeOpState,  5).
-define(Chi_associateMp,         6).
-define(Chi_disassociateMp,      7).
-define(Chi_restartCluster,      8).

%% must match values in clh_chi.h
-define(CHI_RESULT_OK,                         0).
-define(CHI_RESULT_ERROR_SERVER_NOT_AVAILABLE, 1).
-define(CHI_RESULT_NO_EXIST,                   2).
-define(CHI_RESULT_BAD_PARM,                   3).
-define(CHI_RESULT_NO_ITC_MBOX,                4).

-define(CHI_MP_ROLE_CORE,    0).
-define(CHI_MP_ROLE_REGULAR, 1).

-define(CHI_OPSTATE_DISABLED, 0).
-define(CHI_OPSTATE_ENABLED,  1).

-define(CHI_CORE_RANK_PRIMARY,   0).
-define(CHI_CORE_RANK_SECONDARY, 1).
-define(CHI_CORE_RANK_UNDEFINED, 2).

-define(CHI_RESTART_TYPE_HARD, 0).
-define(CHI_RESTART_TYPE_SOFT, 1).

-define(CHI_RESTART_RANK_COLD,           0).
-define(CHI_RESTART_RANK_COLD_WITH_TEST, 1).
-define(CHI_RESTART_RANK_WARM,           2).

-define(CHI_ROLE_ACTIVE,  0).   % Deprecated
-define(CHI_ROLE_STANDBY, 1).   % Deprecated
-define(CHI_ROLE_REGULAR, 2).   % Deprecated

-define(CHI_CORE_ROLE_PRIMARY,   0).   % Deprecated
-define(CHI_CORE_ROLE_SECONDARY, 1).   % Deprecated
-define(CHI_CORE_ROLE_REGULAR,   2).   % Deprecated

%% must match values in clh_chi.sig
-define(CHI_OP_STATE_CHANGE_IND, 16#0180003).
-define(CHI_CHANGE_STATE_IND, 16#0180001).   % Deprecated

%% Miscellaneous
-define(MP_ID_OWN,               1).
-define(MP_ID_STANDBY,           2).
-define(MP_ID_OTHER,             3).
-define(MP_ID_NONEXISTENT,       9876).
