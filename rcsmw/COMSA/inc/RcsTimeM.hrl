%%% --------------------------------------------------------
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% --------------------------------------------------------

-hrl_id({"RcsTimeM","1.2.1","/main/R2A/R3A/R4A/3"}).


%% -------------- CLASS TimeM -------------------------

%% Description:
%% Time management concerns the calendar date and local time of the node.

-record(timeM, {timeMId,
                dummy}).

-define(timeM_types,
        [{timeMId, string},
         {dummy, atom}]).

-define(TimeM_restricted, [timeMId]).


%% -------------- CLASS DateAndTime -------------------------

%% Description:
%% Local time and time zone values.

-record(dateAndTime, {dateAndTimeId,
                      timeZone,
                      localDateTime,
                      dateTimeOffset,
                      tzRevision}).

-define(dateAndTime_types,
        [{dateAndTimeId, string},
         {timeZone, string},
         {localDateTime, 'RcsTimeM.DateTime'},
         {dateTimeOffset, 'RcsTimeM.DifferenceFromUTC'},
         {tzRevision, string}]).

-define(DateAndTime_restricted, [dateAndTimeId]).

