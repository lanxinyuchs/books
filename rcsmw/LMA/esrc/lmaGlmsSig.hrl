%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaGlmsSig.hrl %
%%% Author:	etxbjca
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R3A/R4A/R5A/R9A/R11A/2').
-hrl_date('2017-09-28').
-hrl_author('qselert').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% -----      -------    --------    ------------------------
%%% R2A/1      2013-09-12   etxpejn     Created
%%% R2A/2      2013-09-20   etxpejn     Updated defines due to updates in glms_adpi.sig
%%% R2A/5      2013-11-13   etxpejn     Updated to master branch signals
%%% R2A/6      2013-11-21   etxpejn     Added alarm defines
%%% R2A/7      2013-11-25   etxpejn     Added LM states defines
%%% R2A/8      2013-12-09   etxpejn     Added GLMS_ACTION_STATE
%%% R2A/9      2014-01-09   etxpejn     Added AM signals
%%% R2A/10     2014-03-12   etxpejn     Changes made for new glms adpi 
%%% R3A/1      2014-10-06   etxpejn     Added LIHI signals
%%% R3A/2      2014-11-26   etxpejn     Added capacity signals
%%% R3A/3      2014-12-16   etxpejn     Added UPDATE_GRACE_PERIOD_ATTRIBUTES signals
%%% R4A/1      2015-07-26   etxpejn     Added READ_MO_GRACE_PERIOD signals
%%% R5A/1      2016-01-25   etxpejn     Added *_ALARM_RSP signals
%%% R9A/1      2017-04-11   etxpejn     Added LICENSE_KEY_EXPIRATION_ALARM
%%% R11A/1     2017-07-13   qselert     Added signals for SP086
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: Values from glms_adpi.sig
%%% ----------------------------------------------------------

-define(GLMS_OK, 1).
-define(GLMS_NOK, 0).

-define(GLMS_LOG_DISABLED, 0).
-define(GLMS_LOG_LOW, 4).
-define(GLMS_LOG_MEDIUM, 7).
-define(GLMS_LOG_HIGH, 10).

-define(GLMS_ALARM_CEASED, 0).
-define(GLMS_ALARM_ACTIVATED, 1).

-define(GLMS_FEATURE_KEY, 0).
-define(GLMS_CAPACITY_KEY, 1).

-define(GLMS_ALARM_NONE, 0).
-define(GLMS_ALARM_MINOR, 1).
-define(GLMS_ALARM_WARNING, 2).
-define(GLMS_ALARM_MAJOR, 3).

-define(GLMS_LM_LOCKED, 0).
-define(GLMS_LM_NORMAL, 1).
-define(GLMS_LM_EMERGENCY_UNLOCK, 2).
-define(GLMS_LM_INTEGRATION_UNLOCK, 3).
-define(GLMS_LM_AUTONOMOUS_MODE, 4).

-define(GLMS_ACTION_STATE_CANCELLING, 0).
-define(GLMS_ACTION_STATE_RUNNING, 1).
-define(GLMS_ACTION_STATE_FINISHED, 2).
-define(GLMS_ACTION_STATE_CANCELLED, 3).

-define(GLMS_FAILURE, 0).
-define(GLMS_SUCCESS, 1).
-define(GLMS_NOT_AVAILABLE, 2).

-define(GLMS_ALARM_REASON_CEASED, 0).
-define(GLMS_ALARM_REASON_KEY_NOT_AVAILABLE, 1).	
-define(GLMS_ALARM_REASON_KEY_EXPIRED, 2).

%% Signals

%% Adapter -> GLMS
-define(GLMS_ADPI_ACTIVATE_REQ,                  (16#1900100)).
-define(GLMS_ADPI_SET_FINGERPRINT_REQ,           (16#1900102)).
-define(GLMS_ADPI_INSTALL_KEY_FILE_REQ,          (16#1900104)).
-define(GLMS_ADPI_DOWNLOAD_KEY_FILE_RSP,         (16#1900108)).
-define(GLMS_ADPI_STORE_KEY_FILE_RSP,            (16#190010a)).
-define(GLMS_ADPI_CREATE_FEATURE_KEY_MO_RSP,     (16#190010c)).
-define(GLMS_ADPI_SET_FEATURE_STATE_REQ,         (16#190010d)).
-define(GLMS_ADPI_GET_KEY_FILE_LOCATION_RSP,     (16#1900110)).
-define(GLMS_ADPI_DEACTIVATE_REQ,                (16#1900111)).
-define(GLMS_ADPI_DELETE_FEATURE_KEY_MO_RSP,     (16#1900114)).
-define(GLMS_ADPI_PKI_VERIFICATION_RSP,          (16#1900116)).
-define(GLMS_ADPI_HEARTBEAT_REQ,                 (16#1900117)).
-define(GLMS_ADPI_ACTIVATE_EU_REQ,               (16#1900119)).

-define(GLMS_ADPI_REFRESH_LICENSE_INVENTORY_REQ, (16#190011b)).
-define(GLMS_ADPI_DUMP_GLMS_STATE_DATA_REQ,      (16#190011e)).
-define(GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_REQ,     (16#1900120)).
-define(GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_REQ,     (16#1900122)).
-define(GLMS_ADPI_DUMP_FEATURE_STATE_DATA_REQ,   (16#1900124)).
-define(GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_REQ,  (16#1900126)).
-define(GLMS_ADPI_ACTIVATE_IU_REQ,               (16#1900128)).
-define(GLMS_ADPI_ACTIVATE_PU_REQ,               (16#190012a)).
-define(GLMS_ADPI_DEACTIVATE_PU_REQ,             (16#190012c)).

-define(GLMS_ADPI_CREATE_FEATURE_STATE_MO_RSP,   (16#190012f)).
-define(GLMS_ADPI_DELETE_FEATURE_STATE_MO_RSP,   (16#1900131)).

-define(GLMS_ADPI_CREATE_CAPACITY_KEY_MO_RSP,    (16#1900133)).
-define(GLMS_ADPI_DELETE_CAPACITY_KEY_MO_RSP,    (16#1900135)).
-define(GLMS_ADPI_CREATE_CAPACITY_STATE_MO_RSP,  (16#1900137)).
-define(GLMS_ADPI_DELETE_CAPACITY_STATE_MO_RSP,  (16#1900139)).
-define(GLMS_ADPI_CREATE_GRACE_PERIOD_MO_RSP,    (16#190013b)).
-define(GLMS_ADPI_DELETE_GRACE_PERIOD_MO_RSP,    (16#190013d)).
-define(GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_REQ,            (16#190013e)).

-define(GLMS_ADPI_FEATURE_CONFIGURATION_LIST_RSP, (16#1900141)). 

-define(GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_REQ,              (16#1900150)).
-define(GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_REQ,  (16#1900152)).
-define(GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_RSP,  (16#1900153)).
-define(GLMS_ADPI_READ_MO_LM_REQ,                                (16#1900154)).
-define(GLMS_ADPI_READ_MO_FEATURE_KEY_REQ,                       (16#1900156)).

-define(GLMS_ADPI_READ_EU_MO_REQ,                                (16#1900158)).
-define(GLMS_ADPI_SUBSCRIBE_MO_UPDATES_REQ,                      (16#190015a)).
-define(GLMS_ADPI_READ_IU_MO_REQ,                                (16#1900165)).
-define(GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_REQ,                   (16#1900167)).
-define(GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_REQ,                  (16#1900169)).
-define(GLMS_ADPI_READ_AM_MO_REQ,                                (16#190016b)).

-define(GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_REQ,                  (16#190016d)).
-define(GLMS_ADPI_READ_MO_FEATURE_STATE_REQ,                      (16#190016f)).

-define(GLMS_ADPI_READ_MO_CAPACITY_KEY_REQ,                       (16#1900171)).
-define(GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_REQ,                 (16#1900173)).
-define(GLMS_ADPI_READ_MO_CAPACITY_STATE_REQ,                     (16#1900175)).
-define(GLMS_ADPI_READ_MO_GRACE_PERIOD_REQ,                       (16#1900177)).
-define(GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_REQ,                  (16#1900146)).
-define(GLMS_ADPI_UPDATE_AREA_ID_REQ,                             (16#1900144)).
-define(GLMS_ADPI_READ_MO_LICENSE_SUPPORT_REQ,                    (16#190017d)).
-define(GLMS_ADPI_STATE_MO_AUDIT_REQ,                             (16#1900142)).

%% GLMS -> Adapter
-define(GLMS_ADPI_ACTIVATE_RSP,                  (16#1900101)).
-define(GLMS_ADPI_SET_FINGERPRINT_RSP,           (16#1900103)).
-define(GLMS_ADPI_INSTALL_KEY_FILE_RSP,          (16#1900105)).
-define(GLMS_ADPI_INSTALL_KEY_FILE_IND,          (16#1900106)).
-define(GLMS_ADPI_DOWNLOAD_KEY_FILE_REQ,         (16#1900107)).
-define(GLMS_ADPI_STORE_KEY_FILE_REQ,            (16#1900109)).
-define(GLMS_ADPI_CREATE_FEATURE_KEY_MO_REQ,     (16#190010b)).
-define(GLMS_ADPI_SET_FEATURE_STATE_RSP,         (16#190010e)).
-define(GLMS_ADPI_GET_KEY_FILE_LOCATION_REQ,     (16#190010f)).
-define(GLMS_ADPI_DEACTIVATE_RSP,                (16#1900112)).
-define(GLMS_ADPI_DELETE_FEATURE_KEY_MO_REQ,     (16#1900113)).
-define(GLMS_ADPI_PKI_VERIFICATION_REQ,          (16#1900115)).
-define(GLMS_ADPI_HEARTBEAT_RSP,                 (16#1900118)).
-define(GLMS_ADPI_LOG_IND,                       (16#190011d)).
-define(GLMS_ADPI_ACTIVATE_EU_RSP,               (16#190011a)).

-define(GLMS_ADPI_REFRESH_LICENSE_INVENTORY_RSP, (16#190011c)).
-define(GLMS_ADPI_DUMP_GLMS_STATE_DATA_RSP,      (16#190011f)).
-define(GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_RSP,     (16#1900121)).
-define(GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_RSP,     (16#1900123)).
-define(GLMS_ADPI_DUMP_FEATURE_STATE_DATA_RSP,   (16#1900125)).
-define(GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_RSP,  (16#1900127)).
-define(GLMS_ADPI_ACTIVATE_IU_RSP,               (16#1900129)).
-define(GLMS_ADPI_ACTIVATE_PU_RSP,               (16#190012b)).
-define(GLMS_ADPI_DEACTIVATE_PU_RSP,             (16#190012d)).

-define(GLMS_ADPI_CREATE_FEATURE_STATE_MO_REQ,   (16#190012e)).
-define(GLMS_ADPI_DELETE_FEATURE_STATE_MO_REQ,   (16#1900130)).

-define(GLMS_ADPI_CREATE_CAPACITY_KEY_MO_REQ,    (16#1900132)).
-define(GLMS_ADPI_DELETE_CAPACITY_KEY_MO_REQ,    (16#1900134)).
-define(GLMS_ADPI_CREATE_CAPACITY_STATE_MO_REQ,  (16#1900136)).
-define(GLMS_ADPI_DELETE_CAPACITY_STATE_MO_REQ,  (16#1900138)).
-define(GLMS_ADPI_CREATE_GRACE_PERIOD_MO_REQ,    (16#190013a)).
-define(GLMS_ADPI_DELETE_GRACE_PERIOD_MO_REQ,    (16#190013c)).
-define(GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_RSP,    (16#190013f)).

-define(GLMS_ADPI_FEATURE_CONFIGURATION_LIST_REQ, (16#1900140)). 

-define(GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_IND,  (16#1900163)).


-define(GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_RSP,              (16#1900151)).
-define(GLMS_ADPI_READ_MO_LM_RSP,                                (16#1900155)).
-define(GLMS_ADPI_READ_MO_FEATURE_KEY_RSP,                       (16#1900157)).

-define(GLMS_ADPI_READ_EU_MO_RSP,                                (16#1900159)).
-define(GLMS_ADPI_SUBSCRIBE_MO_UPDATES_RSP,                      (16#190015b)).
-define(GLMS_ADPI_MO_UPDATE_EU_IND,                              (16#190015c)).
-define(GLMS_ADPI_MO_UPDATE_IU_IND,                              (16#190015d)).
-define(GLMS_ADPI_MO_UPDATE_AM_IND,                              (16#190015e)).
-define(GLMS_ADPI_MO_UPDATE_KEY_FILE_IND,                        (16#190015f)).
-define(GLMS_ADPI_MO_UPDATE_LM_IND,                              (16#1900160)).
-define(GLMS_ADPI_MO_UPDATE_FEATURE_STATE_IND,                   (16#1900161)).
-define(GLMS_ADPI_MO_UPDATE_FEATURE_KEY_NAME_IND,                (16#1900162)).
-define(GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_IND,                    (16#1900163)).
-define(GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_NAME_IND,               (16#1900164)).
-define(GLMS_ADPI_READ_IU_MO_RSP,                                (16#1900166)).
-define(GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_RSP,                   (16#1900168)).
-define(GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_RSP,                  (16#190016a)).
-define(GLMS_ADPI_READ_AM_MO_RSP,                                (16#190016c)).
   
-define(GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_RSP,                 (16#190016e)).
-define(GLMS_ADPI_READ_MO_FEATURE_STATE_RSP,                     (16#1900170)).

-define(GLMS_ADPI_READ_MO_CAPACITY_KEY_RSP,                      (16#1900172)).
-define(GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_RSP,                (16#1900174)).
-define(GLMS_ADPI_READ_MO_CAPACITY_STATE_RSP,                    (16#1900176)).
-define(GLMS_ADPI_READ_MO_GRACE_PERIOD_RSP,                      (16#1900178)).

-define(GLMS_ADPI_MO_UPDATE_GRACE_PERIOD_IND,                    (16#1900179)).
-define(GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_CAPACITY_UNIT_IND,      (16#190017a)).
-define(GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_CAPACITY_UNIT_IND,    (16#190017b)).
-define(GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_RSP,                 (16#1900147)).
-define(GLMS_ADPI_UPDATE_AREA_ID_RSP,                            (16#1900145)).
-define(GLMS_ADPI_READ_MO_LICENSE_SUPPORT_RSP,                   (16#190017e)).
-define(GLMS_ADPI_STATE_MO_AUDIT_RSP,                            (16#1900143)).

%%% #---------------------------------------------------------
%% Alarms

%% GLMS -> Adapter
-define(GLMS_ADPI_KEY_FILE_FAULT_ALARM_REQ,                      (16#19001a0)).
-define(GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_REQ,           (16#19001a1)).
-define(GLMS_ADPI_EU_ALARM_IND,                                  (16#19001a2)).
-define(GLMS_ADPI_AM_ALARM_IND,                                  (16#19001a4)).
-define(GLMS_ADPI_GP_ALARM_IND,                                  (16#19001a5)).
-define(GLMS_ADPI_KEY_FILE_FAULT_ALARM_RSP,                      (16#19001a6)).
-define(GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_RSP,           (16#19001a7)).
-define(GLMS_ADPI_LICENSE_KEY_EXPIRATION_ALARM_IND,              (16#19001a8)).
-define(GLMS_ADPI_MO_UPDATE_LICENSE_SUPPORT_IND,                 (16#190017c)).

%%% #---------------------------------------------------------
%% PERSISTENT PARAMETERS INTERFACE

%% Adapter -> GLMS
-define(GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP,       (16#19001b1)).
-define(GLMS_ADPI_PERSISTENT_PARAMETER_SET_RSP,              (16#19001b3)).
-define(GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP,              (16#19001b5)).
-define(GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_RSP,     (16#19001b7)).

%% GLMS -> Adapter
-define(GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_REQ,       (16#19001b0)).
-define(GLMS_ADPI_PERSISTENT_PARAMETER_SET_REQ,              (16#19001b2)).
-define(GLMS_ADPI_PERSISTENT_PARAMETER_GET_REQ,              (16#19001b4)).
-define(GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_REQ,     (16#19001b6)).


%%% #---------------------------------------------------------
%%  SOFTWARE PARAMETERS INTERFACE

%% Adapter -> GLMS
-define(GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP,        (16#19001c1)).
-define(GLMS_ADPI_SOFTWARE_PARAMETER_SET_RSP,               (16#19001c3)).
-define(GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP,               (16#19001c5)).
-define(GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_RSP,      (16#19001c7)).

%% GLMS -> Adapter
-define(GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_REQ,        (16#19001c0)).
-define(GLMS_ADPI_SOFTWARE_PARAMETER_SET_REQ,               (16#19001c2)).
-define(GLMS_ADPI_SOFTWARE_PARAMETER_GET_REQ,               (16#19001c4)).
-define(GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_REQ,      (16#19001c6)).

%%% #---------------------------------------------------------
%%% LIHI
%%% #---------------------------------------------------------
-define(NS_HUNT_REQUEST, 31617).
-define(LICENSE_FEATURE_CTRL_SIGBASE, (16#1741d00)).
-define(LFCI_HUNT, (?LICENSE_FEATURE_CTRL_SIGBASE + 11)).
-define(LFCI_ATTACH, (?LICENSE_FEATURE_CTRL_SIGBASE + 10)).

-define(LFCI_CONN_TO_SERVER_REQ, (?LICENSE_FEATURE_CTRL_SIGBASE + 0)).
-define(LFCI_CONN_TO_SERVER_CFM, (?LICENSE_FEATURE_CTRL_SIGBASE + 1)).
-define(LFCI_CONN_TO_SERVER_REJ, (?LICENSE_FEATURE_CTRL_SIGBASE + 2)).
-define(LFCI_FEATURE_LICENSE_SUBSCRIBE_REQ, (?LICENSE_FEATURE_CTRL_SIGBASE + 3)).
-define(LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM, (?LICENSE_FEATURE_CTRL_SIGBASE + 4)).
-define(LFCI_FEATURE_LICENSE_CHANGE_IND, (?LICENSE_FEATURE_CTRL_SIGBASE + 7)).
-define(LFCI_FEATURE_LICENSE_DISCONNECT_IND, (?LICENSE_FEATURE_CTRL_SIGBASE + 8)).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------



