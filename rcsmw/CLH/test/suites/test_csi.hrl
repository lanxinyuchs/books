%%% %CCaseFile:	test_csi.hrl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R3A/R4A/5

%% must match definition in master.h of ift_app
-define(CSI, 3).


%% must match definitions in test_csi.c
-define(Csi_get_state,        1).   % Deprecated
-define(Csi_subscribe,        2).   % Deprecated
-define(Csi_unsubscribe,      3).   % Deprecated
-define(Csi_get_role,         4).   % Deprecated
-define(Csi_subscribeRole,    5).   % Deprecated
-define(Csi_unsubscribeRole,  6).   % Deprecated
-define(Csi_subscribeCoreState,          7).
-define(Csi_unsubscribeCoreState,        8).
-define(Csi_getOwnMpid,                  9).
-define(Csi_getHuntPathPrefix,          10).
-define(Csi_subscribeClusterRestart,    18).
-define(Csi_unsubscribeClusterRestart,  19).
-define(Csi_clusterRestartReply,        20).

%% must match values in clh_csi.h
-define(CSI_CORE_STATE_ACTIVE,    0).
-define(CSI_CORE_STATE_STANDBY,   1).
-define(CSI_CORE_STATE_UNDEFINED, 2).

-define(CSI_OK,                            0).
-define(CSI_ERROR_SERVER_NOT_AVAILABLE,    1).
-define(CSI_NO_EXIST,                      2).
-define(CSI_NO_ITC_MBOX,                   3).

-define(CSI_DISABLED, 0).   % Deprecated
-define(CSI_ENABLED,  1).   % Deprecated

-define(CSI_ROLE_ACTIVE,  0).   % Deprecated
-define(CSI_ROLE_STANDBY, 1).   % Deprecated
-define(CSI_ROLE_REGULAR, 2).   % Deprecated

%% must match values in clh_csi.sig
-define(CSI_CHANGE_OPER_STATE_IND, 16#0180000).   % Deprecated
-define(CSI_CHANGE_ROLE_IND,       16#0180002).   % Deprecated
-define(CSI_CORE_STATE_CHANGE_IND, 16#0180004).
-define(CSI_CLUSTER_RESTART_IND,   16#0180006).

-define(CSI_RESTART_TYPE_HARD, 0).
-define(CSI_RESTART_TYPE_SOFT, 1).

-define(PIU_INSTANCE_ID_OWN,               1).
-define(PIU_INSTANCE_ID_OTHER,             3).
-define(PIU_INSTANCE_ID_NONEXISTENT,       9876).

