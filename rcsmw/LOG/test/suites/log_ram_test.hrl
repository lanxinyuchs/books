%%% --------------------------------------------------------
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
%%% --------------------------------------------------------


%% =======================================================
%% General defines
%% =======================================================
-define(LOG_NAME,        "RamLogTest").
-define(LOG_COPY_PATH,   "RamLogTest").
-define(COPIED_DIR_NAME, "RamLogFiles").
-define(LOG_LINK_TPL ,   "<a href={path}>{name} ({size} bytes)</a>").

-define(LOG_NAME_1,      "RamLogTestFirst").
-define(LOG_NAME_2,      "RamLogTestSecond").

-define(LOG_DEFAULTS,    [{zip, false}]).
-define(TESTNODE,        testnode).
-define(LOG_RAM,         logRamI).

%% =======================================================
%% Time interval defines
%% =======================================================
-define(SECOND,               1000).
-define(MINUTE,               60 * ?SECOND).
-define(RPC_TIMEOUT,          10 * ?SECOND).
-define(NODE_PING_INTERVAL,    5 * ?SECOND).
-define(NODE_RESTART_TIMEOUT,  3 * ?MINUTE).


%% =======================================================
%% file:file_info record definition
%% =======================================================
-record(file_info, {
    size, type, access, atime, mtime, ctime, mode,
    links, major_device, minor_device, inode, uid, gid
}).

%% =======================================================
%% logRamI:write_log handling macros
%%
%% Used for handling blacklist tag when writing to RAM Log
%% without having to manually specify tag parameters.
%% =======================================================
-define(
  LOG_RAM_WRITE(Data),
  log_ram_test_lib:write_log(
    Data, 
    {?MODULE, ?LINE, self()}
  )
).

-define(
  LOG_RAM_WRITE_MULTI(Data),
  log_ram_test_lib:write_log_multi(
    Data, 
    {?MODULE, ?LINE, self()}
  )
).
