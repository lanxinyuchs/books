%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	tim.hrl %
%%% Author:	erarafo
%%% Description:
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R4A/R9A/R10A/1').
-hrl_date('2017-07-19').
-hrl_author('etomist').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R4A/1      2015-10-16 erarafo     First version
%%% R4A/2      2015-10-18 erarafo     Type specs added
%%% R4A/3      2015-10-26 erarafo     Adjusted constants
%%% R4A/4      2015-10-27 erarafo     Removed TRUE and FALSE
%%% R4A/5      2015-11-03 erarafo     Cleanup
%%% R4A/6      2015-11-30 erarafo     Adjusted "prewarn overdue"
%%% R10A/1     2017-07-19 etomist     HW12725
%%% ----------------------------------------------------------

-include("RmeTimeSettings.hrl").

%% the one and only supported protocol version so far
-define(SUPPORTED_PVS, [?CELLO_TZII_PV1]).

%% signal revision
-define(SIGNAL_REV, 1).

%% allow an IND to be sent even if current time is
%% slightly past the DST or leap change time; usually
%% this is no issue anyway since the prewarn time
%% is likely to be a lot greater than this
-define(PREWARN_OVERDUE_LIMIT, 500).

%% signal records are kept for at most 23 hours.
-define(SIGNAL_REC_STALE_LIMIT, 23*3600*1000).


%% constants
-define(MAX_PREWARN_MILLIS, 23*3600*1000).
-define(TIMEOUT_24_H, 24*3600*1000).
-define(TIMEOUT_NOW, 0).
-define(ITC_PORT_NAME, <<"TZII">>).
-define(SERVER, timServer).


-define(
   INFO(FORMAT, DATA),
   sysInitI:info_msg("~w:~w " FORMAT "~n", [?MODULE, ?LINE|DATA])).

-define(
   WARNING(FORMAT, DATA),
   sysInitI:warning_msg("~w:~w " FORMAT "~n", [?MODULE, ?LINE|DATA])).

-define(
   ERROR(FORMAT, DATA),
   sysInitI:error_msg("~w:~w " FORMAT "~n", [?MODULE, ?LINE|DATA])).

%% to be removed when no longer needed
-define(
   TRACE(FORMAT, DATA),
   sysInitI:info_msg("tzii:trace ~w:~w " FORMAT "~n", [?MODULE, ?LINE|DATA])).

%% to be removed when no longer needed
-define(coverage(), timLib:doCoverage(?MODULE, ?LINE)).


-type uint32()     :: 0..4294967295.
-type spid()       :: uint32().
-type clientInfo() ::  uint32().

-type millis()     :: integer().

-type year()       :: non_neg_integer().
-type gregDay()    :: non_neg_integer().
-type dayInWeek()  :: 1..7.
-type tzType()     :: utc|local.

-type leapSeconds()  :: -1000..1000.
-type increment()  :: -1|1.

-type cause()     :: 0|1|2.

-type protocolVersion() :: non_neg_integer().

-type dstStatus() :: on|off|undefined.

-type dstRule()   :: #'DstRule'{}.

-type eventType() :: dst|leapSec.

-type dstMode()   :: dstStart|dstEnd|undefined.


%% itime() represents points in time, in milliseconds
%% relative to UTC 00:00:00 of the day that timServer
%% was started
-type itime()   :: integer().

%% C/P from MOM:
%% dayRule
%% The rule for the day of the month for which this rule applies. The rule can 
%% be formatted in any of three ways:
%% - 1..31 (the change will be applied on the given day)
%% - lastDDD, where DDD = Mon, Tue, Wed, Thu, Fri, Sat, Sun
%%           (the change will be applied on the last given weekday of the month)
%% - DDD >= N where DDD= Mon, Tue, Wed, Thu, Fri, Sat, Sun and N = 1..31
%%           (the change will be applied on the weekday with the given date or
%%            the given weekday following the given date)

-define(RE_DAY, "([1-9]|[1-2][0-9]|3[0-1])").
-define(RE_DAY_NAMES, "([Mm][Oo][Nn]|[Tt][Uu][Ee]|[Ww][Ee][Dd]|[Tt][Hh][Uu]|"
                      "[Ff][Rr][Ii]|[Ss][Aa][Tt]|[Ss][Uu][Nn])").
-define(RE_LAST, "([Ll][Aa][Ss][Tt])").
-define(RE_DAY_RULE_1, list_to_binary("^\\s*" ++ ?RE_DAY ++ "\\s*$")).
-define(RE_DAY_RULE_2, list_to_binary("^\\s*" ++ ?RE_LAST ++ 
                                      ?RE_DAY_NAMES ++ "\\s*$")).
-define(RE_DAY_RULE_3, list_to_binary("^\\s*" ++ ?RE_DAY_NAMES ++ "\\s*" ++
                                      ">=" ++ "\\s*" ++ ?RE_DAY ++ "\\s*$")).
