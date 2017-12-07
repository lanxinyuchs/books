-define(PRI, 2).

-define(CelloPri_initiateMemory,           1).
-define(CelloPri_initiateService,          2).
-define(CelloPri_internal,                 3).
-define(CelloPri_freeMemory,               4).
-define(CelloPri_terminateService,         5).
-define(CelloPiu3_getHuntPath,             6).
-define(CelloPiu10_getPid,                 7).
-define(CelloPiu3_getOwnIdentity,          8).
-define(CelloPri9_getOwnIdentity,          9).
-define(CelloPri8_getOwnIdentity,         10).
-define(CelloPri9_getPiuOrDeviceIdentity, 11).
-define(CelloPri8_getPiuOrDeviceIdentity, 12).
-define(CelloPiu4_restartOwnPiu,          13).
-define(CelloPiu4_restartOtherPiu,        14).
-define(CelloPiu3_getLinkHandlerName,     15).

%% CelloPriResult
-define(CELLO_PRI_OK,                         1).
-define(CELLO_PRI_MEMORY_NOT_INITIATED,       2).
-define(CELLO_PRI_SERVICE_NOT_INITIATED,      3).
-define(CELLO_PRI_ERROR_SERVER_NOT_AVAILABLE, 4).
-define(CELLO_PRI_UNKNOWN_PV,                 5).
-define(CELLO_PRI_UNKNOWN_SIGNAL,             6).
-define(CELLO_PRI_WRONG_HW_ADDRESS,           7).
-define(CELLO_PRI_FAILURE_TO_GET_OWN_SMN,     8).


-define( CELLO_PRI_INITIATE_SERVICE_CFM, (16#105C9)).
-define( CELLO_PRI_INITIATE_SERVICE_SUS, (16#105CA)).
-define( CELLO_PRI_INITIATE_SERVICE_REJ, (16#105CB)).
-define( CELLO_PRI_SERVER_UP_IND,        (16#105CC)).
-define( CELLO_PRI_SERVER_DOWN_IND,      (16#105CD)).
-define( CELLO_PRI_SERVER_UNPUBLISH_IND, (16#105CE)).
-define( CELLO_PRI_TERMINATE_SERVICE_CFM,(16#105D0)).
-define( CELLO_PIU3_GET_HUNT_PATH_CFM,   (16#10738)).
-define( CELLO_PIU3_GET_LH_NAME_CFM,     (16#1069B)).
-define( CELLO_PIU3_GET_OWN_ID_CFM,      (16#1073C)).
-define( CELLO_PIU4_RESTART_PIU_CFM,     (16#1077C)).
-define( CELLO_PIU4_RESTART_PIU_REJ,     (16#1077D)).
-define( CELLO_PRI8_GET_IDENTITY_CFM,    (16#10A5F)).
-define( CELLO_PRI9_GET_IDENTITY_CFM,    (16#10B0A)).
-define( CELLO_PIU10_OPERATIONAL_PID_CFM,(16#10B63)).

%%  CelloPiuRoResult
-define(CELLO_PIU_RO_OK,                          1).
-define(CELLO_PIU_RO_WRONG_HW_ADDRESS,            2).
-define(CELLO_PIU_RO_ERROR_PIU_NOT_FOUND,         3).
-define(CELLO_PIU_RO_ERROR_IN_OSPI,               4).
-define(CELLO_PIU_RO_RESTART_NOT_ALLOWED,         5).
-define(CELLO_PIU_RO_NO_SUCH_SERVICE,             6).
-define(CELLO_PIU_RO_SERVICE_NOT_FOUND,           7).
-define(CELLO_PIU_RO_PROPERTIES_NOT_FOUND,        8).
-define(CELLO_PIU_RO_TEMP_FUNC_NOT_SUPPORTED,     9).
-define(CELLO_PIU_RO_TEMP_CLIENT_NOT_SUBSCRIBED,  10).
-define(CELLO_PIU_RO_TEMP_ALREADY_SUBSCRIBED,     11).
-define(CELLO_PIU_RO_TEMP_UNIT_NOT_FOUND,         12).
-define(CELLO_PIU_RO_ERROR_SMN_NOT_FOUND,         13).
-define(CELLO_PIU_RO_HW_PID_NOT_FETCHED,          14).
-define(CELLO_PIU_RO_ERROR_PROGRAM_NOT_FOUND,     15).
