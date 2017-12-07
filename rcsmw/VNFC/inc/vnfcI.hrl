%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile: vnfcs.hrl %
%%% Author:     etxaldu
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('').
-hrl_vsn('').
-hrl_date('').
-hrl_author('').
%%% %CCaseTemplateFile: module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%%            2016-10-19 etxaldu     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description:
%%% ----------------------------------------------------------
-define(RESTART_USR,  manual).
-define(RESTART_APP,  automatic).
-define(RESTART_OSMW, recovery).

-define(RESTART_USR_STR,  "MANUAL").
-define(RESTART_APP_STR,  "AUTOMATIC").
-define(RESTART_OSMW_STR, "RECOVERY").

-define(HB_DETAIL_LEN,           255).
-define(HB_ERROR_DETAIL_LEN,     255).
-define(HB_ERROR_ADD_DETAIL_LEN, 255).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
