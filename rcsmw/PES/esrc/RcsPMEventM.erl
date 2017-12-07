%%% --------------------------------------------------------
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
%%% --------------------------------------------------------
-module('RcsPMEventM').


-export([string/1]).
-export([int16/1]).
-export([int32/1]).
-export([uint16/1]).
-export([uint32/1]).
-export([boolean/1]).
-export([moRef/1]).
-export(['RcsPMEventM.FilterMethod'/1]).
-export(['RcsPMEventM.TimePeriod'/1]).
-export(['RcsPMEventM.SessionState'/1]).
-export(['RcsPMEventM.JobControl'/1]).
-export(['RcsPMEventM.JobState'/1]).
-export(['RcsPMEventM.CompressionTypes'/1]).
-export(['RcsPMEventM.IpDNSAddress'/1]).

%% default functions START
string(Val)  -> Val.
int16(Val)   -> list_to_integer(Val).
int32(Val)   -> list_to_integer(Val).
uint16(Val)  -> list_to_integer(Val).
uint32(Val)  -> list_to_integer(Val).
boolean(Val) -> list_to_atom(Val).
moRef(Val)   -> list_to_atom(Val).
%% default functions END




%% FilterMethodSTART
%% SINGLE_SELECT<====>0
'RcsPMEventM.FilterMethod'(0)->"SINGLE_SELECT";
'RcsPMEventM.FilterMethod'("SINGLE_SELECT")->0;
%% MULTI_SELECT<====>1
'RcsPMEventM.FilterMethod'(1)->"MULTI_SELECT";
'RcsPMEventM.FilterMethod'("MULTI_SELECT")->1;
%% INTERVAL<====>2
'RcsPMEventM.FilterMethod'(2)->"INTERVAL";
'RcsPMEventM.FilterMethod'("INTERVAL")->2;
%% DISTINGUISHED_NAMES<====>3
'RcsPMEventM.FilterMethod'(3)->"DISTINGUISHED_NAMES";
'RcsPMEventM.FilterMethod'("DISTINGUISHED_NAMES")->3;
%% REGEXP<====>4
'RcsPMEventM.FilterMethod'(4)->"REGEXP";
'RcsPMEventM.FilterMethod'("REGEXP")->4.

%% FilterMethodEND


%% TimePeriodSTART
%% ONE_MIN<====>3
'RcsPMEventM.TimePeriod'(3)->"ONE_MIN";
'RcsPMEventM.TimePeriod'("ONE_MIN")->3;
%% FIVE_MIN<====>4
'RcsPMEventM.TimePeriod'(4)->"FIVE_MIN";
'RcsPMEventM.TimePeriod'("FIVE_MIN")->4;
%% FIFTEEN_MIN<====>5
'RcsPMEventM.TimePeriod'(5)->"FIFTEEN_MIN";
'RcsPMEventM.TimePeriod'("FIFTEEN_MIN")->5;
%% THIRTY_MIN<====>6
'RcsPMEventM.TimePeriod'(6)->"THIRTY_MIN";
'RcsPMEventM.TimePeriod'("THIRTY_MIN")->6;
%% ONE_HOUR<====>7
'RcsPMEventM.TimePeriod'(7)->"ONE_HOUR";
'RcsPMEventM.TimePeriod'("ONE_HOUR")->7;
%% TWELVE_HOUR<====>8
'RcsPMEventM.TimePeriod'(8)->"TWELVE_HOUR";
'RcsPMEventM.TimePeriod'("TWELVE_HOUR")->8;
%% ONE_DAY<====>9
'RcsPMEventM.TimePeriod'(9)->"ONE_DAY";
'RcsPMEventM.TimePeriod'("ONE_DAY")->9;
%% TEN_SECONDS<====>1
'RcsPMEventM.TimePeriod'(1)->"TEN_SECONDS";
'RcsPMEventM.TimePeriod'("TEN_SECONDS")->1;
%% THIRTY_SECONDS<====>2
'RcsPMEventM.TimePeriod'(2)->"THIRTY_SECONDS";
'RcsPMEventM.TimePeriod'("THIRTY_SECONDS")->2.

%% TimePeriodEND


%% SessionStateSTART
%% ACTIVATING<====>0
'RcsPMEventM.SessionState'(0)->"ACTIVATING";
'RcsPMEventM.SessionState'("ACTIVATING")->0;
%% ACTIVE<====>1
'RcsPMEventM.SessionState'(1)->"ACTIVE";
'RcsPMEventM.SessionState'("ACTIVE")->1;
%% DEACTIVATING<====>2
'RcsPMEventM.SessionState'(2)->"DEACTIVATING";
'RcsPMEventM.SessionState'("DEACTIVATING")->2;
%% STOPPED<====>3
'RcsPMEventM.SessionState'(3)->"STOPPED";
'RcsPMEventM.SessionState'("STOPPED")->3;
%% FAILED<====>4
'RcsPMEventM.SessionState'(4)->"FAILED";
'RcsPMEventM.SessionState'("FAILED")->4.

%% SessionStateEND


%% JobControlSTART
%% FULL<====>0
'RcsPMEventM.JobControl'(0)->"FULL";
'RcsPMEventM.JobControl'("FULL")->0;
%% STARTSTOP<====>1
'RcsPMEventM.JobControl'(1)->"STARTSTOP";
'RcsPMEventM.JobControl'("STARTSTOP")->1;
%% VIEWONLY<====>2
'RcsPMEventM.JobControl'(2)->"VIEWONLY";
'RcsPMEventM.JobControl'("VIEWONLY")->2.

%% JobControlEND


%% JobStateSTART
%% ACTIVE<====>1
'RcsPMEventM.JobState'(1)->"ACTIVE";
'RcsPMEventM.JobState'("ACTIVE")->1;
%% STOPPED<====>2
'RcsPMEventM.JobState'(2)->"STOPPED";
'RcsPMEventM.JobState'("STOPPED")->2.

%% JobStateEND


%% CompressionTypesSTART
%% GZIP<====>0
'RcsPMEventM.CompressionTypes'(0)->"GZIP";
'RcsPMEventM.CompressionTypes'("GZIP")->0.

%% CompressionTypesEND

%% IpDNSAddressSTART
%% GZIP<====>0
%%'RcsPMEventM.IpDNSAddress'(0)->"GZIP";
'RcsPMEventM.IpDNSAddress'(IpAddr)->IpAddr.

%% IpDNSAddressEND


