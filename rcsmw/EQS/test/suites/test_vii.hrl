%% must match definition in master.h of ift_app
-define(VII, 4).

%% must match definitions in test_vii.c
-define(Vii_visualIndRequest, 1).
-define(Vii_visualIndGet,     2).

%% must match values in cello_vii.h

%%% enum CelloViiResult_e
-define(CELLO_VII_FAILED, -1).
-define(CELLO_VII_SUCCESS, 0).

%%% enum CelloViiCommand_e
-define(CELLO_VII_FAULT,                     -1).
-define(CELLO_VII_NO_FAULT,                   0).
-define(CELLO_VII_LOADTEST_START,             1).
-define(CELLO_VII_LOADTEST_END,               2).
-define(CELLO_VII_NO_POWER,                   3).
-define(CELLO_VII_POWER,                      4).
-define(CELLO_VII_BOOTTEST_START,             5).
-define(CELLO_VII_BOOTTEST_END,               6).
-define(CELLO_VII_MISSING_RESOURCE_START,     7).
-define(CELLO_VII_MISSING_RESOURCE_END,       8).
-define(CELLO_VII_BOARD_LOCKED,               9).
-define(CELLO_VII_BOARD_UNLOCKED,            10).
-define(CELLO_VII_BOARD_BLOCKED,             11).
-define(CELLO_VII_BOARD_UNBLOCKED,           12).
-define(CELLO_VII_DISC_SYNC_START,           13).
-define(CELLO_VII_DISC_SYNC_STOP,            14).
-define(CELLO_VII_BOARD_BUSY_START,          15).
-define(CELLO_VII_BOARD_BUSY_STOP,           16).
-define(CELLO_VII_SHUTDOWN_START,            17).
-define(CELLO_VII_SHUTDOWN_END,              18).
-define(CELLO_VII_BACKUP_START,              19).
-define(CELLO_VII_BACKUP_END,                20).
-define(CELLO_VII_MEDIUM_BUTTON_PRESS_START, 21).
-define(CELLO_VII_MEDIUM_BUTTON_PRESS_END,   22).
-define(CELLO_VII_SHORT_BUTTON_PRESS_START,  23).
-define(CELLO_VII_SHORT_BUTTON_PRESS_END,    24).
-define(CELLO_VII_ALARM_SUPPRESS_START,      25).
-define(CELLO_VII_ALARM_SUPPRESS_END,        26).
-define(CELLO_VII_NODE_FAULT_START,          27).
-define(CELLO_VII_NODE_FAULT_END,            28).
-define(CELLO_VII_REMOTE_UNIT_FAULT_START,   29).
-define(CELLO_VII_REMOTE_UNIT_FAULT_END,     30).

%%% enum CelloViiLed_e
-define(CELLO_VII_LED_OPERATIONAL, 1).
-define(CELLO_VII_LED_FAULT,       2).
-define(CELLO_VII_LED_STATUS,      3).
-define(CELLO_VII_LED_MAINTENANCE, 4).

%%% enum CelloViiLedState_e
-define(CELLO_VII_LED_OFF,              1).
-define(CELLO_VII_LED_ON,               2).
-define(CELLO_VII_LED_SLOW_BLINK,       3).
-define(CELLO_VII_LED_FAST_BLINK,       4).
-define(CELLO_VII_LED_DOUBLE_FLASH_OFF, 5).
-define(CELLO_VII_LED_DOUBLE_FLASH_ON,  6).
