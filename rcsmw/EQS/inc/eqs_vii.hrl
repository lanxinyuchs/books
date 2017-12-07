%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	eqs_vii.hrl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R2A/2').
-hrl_date('2014-02-18').
-hrl_author('etxpeno').

%%% %CCaseTemplateFile: module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% R2A/1      2014-01-20 etxpeno     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

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
-define(CELLO_VII_DISC_SYNC_END,             14).
-define(CELLO_VII_BOARD_BUSY_START,          15).
-define(CELLO_VII_BOARD_BUSY_END,            16).
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

-define(CELLO_VII_FAILED, -1).
-define(CELLO_VII_SUCCESS, 0).

-define(CELLO_VII_LED_OPERATIONAL, 1).
-define(CELLO_VII_LED_FAULT,       2).
-define(CELLO_VII_LED_STATUS,      3).
-define(CELLO_VII_LED_MAINTENANCE, 4).

-define(CELLO_VII_LED_OFF,              1).
-define(CELLO_VII_LED_ON,               2).
-define(CELLO_VII_LED_SLOW_BLINK,       3).
-define(CELLO_VII_LED_FAST_BLINK,       4).
-define(CELLO_VII_LED_DOUBLE_FLASH_OFF, 5).
-define(CELLO_VII_LED_DOUBLE_FLASH_ON,  6).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
