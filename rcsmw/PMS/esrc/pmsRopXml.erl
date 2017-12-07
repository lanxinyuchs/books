%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsRopXml.erl %
%%% @private
%%% Author:	
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(pmsRopXml).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/1').
-date('2016-06-14').
-author('uabesvi').
-shaid('3a1dbeefaccc37b433209e829984e387755c382b').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% R1A/1      2012-01-18 etxjotj     Created
%%% R5A/1      2015-11-19 uabesvi     HU37017
%%% R6A/1      2016-06-14 uabesvi     changed name
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([measResultFileName/3, 
	 measResultFileName/4, 
	 measResultFileName/5]).
-export([measCollecFile/3]).
-export([fileHeader/5]).
-export([fileFooter/1]).
-export([fileSender/2]).
-export([measCollecBegin/1]).
-export([measCollecEnd/1]).
-export([measData/2]).
-export([managedElement/3]).
-export([measInfo/6]).
-export([job/1]).
-export([granPeriod/2]).
-export([repPeriod/1]).
-export([measTypes/1]).
-export([measValues/1, measValues/2]).
-export([measValue/3, measValue/4]).
-export([measResults/1, measResults/2]).
-export([suspect/1]).

%% -compile(export_all).

-include("RcsPm.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-define(XML_HEADER,
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"<?xml-stylesheet type=\"text/xsl\" href=\"MeasDataCollection.xsl\"?>\n"
	"<measCollecFile xmlns="
	"\"http://www.3gpp.org/ftp/specs/archive/32_series/32.435#measCollec\""
	"\n                xmlns:xsi="
	"\"http://www.w3.org/2001/XMLSchema-instance\""
	"\n                xsi:schemaLocation="
	"\"http://www.3gpp.org/ftp/specs/archive/32_series/32.435#measCollec\""
	">\n").



-define(IND(_Level),          f(lists:duplicate(2 * _Level, " "))).
-define(IND(_Level, _Offset), f(lists:duplicate(2 * _Level + _Offset, " "))).
	

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%========================================================================
%% 
%%========================================================================
measResultFileName(Type, StartTime, EndTime, JobId, UniqueId) 
  when JobId =:= undefined, UniqueId =:= undefined ->
    measResultFileName(Type, StartTime, EndTime);

measResultFileName(Type, StartTime, EndTime, JobId, UniqueId) 
  when UniqueId =:= undefined ->
    measResultFileName(Type, StartTime, EndTime, "-" ++ itl(JobId));

measResultFileName(Type, StartTime, EndTime, JobId, UniqueId) 
  when JobId =:= undefined ->
    measResultFileName(Type, StartTime, EndTime, UniqueId);

measResultFileName(Type, StartTime, EndTime, JobId, UniqueId) ->
    Id = a(["-",itl(JobId),"_",UniqueId]),
    measResultFileName(Type, StartTime, EndTime, Id).


measResultFileName(Type, StartTime, EndTime, Id) ->
    a([measResultFileName(Type, StartTime, EndTime), "_", Id]).


measResultFileName(Type, StartTime, EndTime) 
  when Type =:= "A"; Type =:= "B" ->
    a([Type, f_datetime(StartTime), "-", f_endtime(EndTime)]);

measResultFileName(Type, StartTime, EndTime)
  when Type =:= "C"; Type =:= "D" ->
    a([Type, f_datetime(StartTime), "-", f_datetime(EndTime)]).
 

%%========================================================================
%% 
%%========================================================================
measCollecFile(Head, MeasData, Footer) -> 
    a([?XML_HEADER, 
       Head, 
       MeasData,
       Footer,
       "</measCollecFile>\n"]).

%%========================================================================
%% 
%%========================================================================
fileHeader(Vsn, Vendor, DnPrefix, Sender, MeasCollec) -> 
    Offset = length(?IND(1)) + 12,
    a([?IND(1),
       "<fileHeader fileFormatVersion=", 
       f_str(Vsn),
       optional(f_str(Vendor),   "vendorName=",   Offset),
       optional(f_str(DnPrefix), "dnPrefix=", Offset),
       ">\n",
       Sender,
       MeasCollec,
       ?IND(1),
       "</fileHeader>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
fileFooter(MeasCollec) -> 
    a([?IND(1),
       "<fileFooter>\n", 
       MeasCollec,
       ?IND(1),
       "</fileFooter>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
fileSender(LocalDn, ElemType) -> 
    Offset = choose(LocalDn == undefined, 0, length(?IND(2, 11))),
    a([?IND(2),
       "<fileSender", 
       optional(f_str(LocalDn),  " localDn=",     0),
       optional(f_str(ElemType), " elementType=", Offset),
       "/>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
measCollecBegin(Time) -> 
    a([?IND(2),
       "<measCollec beginTime=", 
       f_time(Time),
       "/>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
measCollecEnd(Time) -> 
    a([?IND(2),
       "<measCollec endTime=", 
       f_time(Time),
       "/>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
measData(undefined, undefined) -> 
    "";
measData(ME, MI) -> 
    a([?IND(1),
       "<measData>\n", 
       ME, 
       MI, 
       ?IND(1),
       "</measData>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
managedElement(LocalDn, UserLabel, SwVsn) -> 
    Offset1 = choose(LocalDn == undefined, 0, length(?IND(2, 15))),
    Offset2 = choose(LocalDn == undefined andalso UserLabel == undefined,
		     0,
		     length(?IND(2, 15))),
    a([?IND(2),
       "<managedElement",
       optional(f_str(LocalDn),   " localDn=",   0),
       optional(f_str(UserLabel), " userLabel=", Offset1),
       optional(f_str(SwVsn),     " swVersion=", Offset2),
       "/>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
measInfo(undefined, undefined, undefined, undefined, undefined, undefined) -> 
    "";
measInfo(MiId, Job, GranP, RepP, Types, Values) -> 
    a([?IND(2),
       "<measInfo",
       optional(f_str(MiId), " measInfoId=", 0),       
       ">\n",
       Job,
       GranP, 
       RepP, 
       Types, 
       Values, 
       ?IND(2),
       "</measInfo>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
job(undefined) -> 
    "";
job(Id) -> 
    a([?IND(3),
       "<job jobId=",
       f_str(Id),
       "/>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
granPeriod(Duration, EndTime) -> 
    a([?IND(3),
       "<granPeriod duration=", 
       f_duration(Duration), 
       "\n",
       f(lists:duplicate(length(?IND(3, 11)), " ")),
       " endTime=", 
       f_time(EndTime), 
       "/>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
repPeriod(undefined) -> 
    "";
repPeriod(Duration) -> 
    a([?IND(3),
       "<repPeriod duration=", 
       f_duration(Duration), 
       "/>\n"
      ]).

%%========================================================================
%% 
%%========================================================================
measTypes(Types) ->
    a([a([?IND(3),"<measType p=", f_str(f_int(N)), ">", Type, 
	  "</measType>\n"]) || 
	  {N, Type} <- Types]).

%%========================================================================
%% 
%%========================================================================
measValues(Vals) ->
    measValues(undefined, Vals).

measValues(Format, Vals) ->
    a([measValue(Format, Ldn, Suspect, Results) || 
	  {Ldn, Suspect, Results} <- Vals]).


measValue(Ldn, Suspect, Results) ->
    measValue(undefined, Ldn, Suspect, Results).


measValue(Format, Ldn, Suspect, Results) ->
    BSuspect = f_bool(Suspect),
    a([?IND(3),
       "<measValue measObjLdn=",
       f_str(Ldn),
       ">\n",
       measResults(Format, Results),
       choose(BSuspect == "true" andalso Results /= undefined, 
	      suspect(BSuspect),
	      ""),
       ?IND(3),
       "</measValue>\n"
      ]).

%%========================================================================
%% use ecim format for now
%%========================================================================
%% measResults(Format, Res) when Format =:= "3gpp"; Format =:= '3gpp' ->
%%     NewRes = lists:flatten([[{N, [Val]} || Val <- Vals] || {N, Vals} <- Res]),
%%     measResults(NewRes);

measResults(_Format, Res) ->
    measResults(Res).


measResults(Res) -> 
    a([a([?IND(4),"<r p=",f_str(f_int(N)),">",f_ints(Vals),"</r>\n"]) ||
	  {N, Vals} <- Res]).

    %% a([?IND(4), 
    %%    "<measResults>", 
    %%    f_meas_res(Res), 
    %%    "</measResults>\n"
    %%   ]).

%%========================================================================
%% 
%%========================================================================
suspect(Bool) -> 
    a([?IND(4), "<suspect>", f_bool(Bool), "</suspect>\n"]).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%==============================================
%% ROP help functions
%%==============================================
optional(undefined, _,   _)   -> "";
optional(Val,       Str, 0)   -> a([Str, Val]);
optional(Val,       Str, Off) -> a(["\n", d(Off), Str, Val]).

%%==============================================
%% ROP format functions
%%==============================================
f_int(Int) ->
    itl(Int).


f_ints(Ints) ->
    %% [_ | IntList] = a([[$, | itl(Int)] || Int <- Ints]),
    [_ | IntList] = lists:append([[$, | itl(Int)] || Int <- Ints]),
    IntList.


f_bool(true)  -> "true";
f_bool(false) -> "false";
f_bool("true")  -> "true";
f_bool("false") -> "false".


f_duration(?TimePeriod_TEN_SECONDS)    -> "\"PT10S\"";
f_duration(?TimePeriod_THIRTY_SECONDS) -> "\"PT30S\"";
f_duration(?TimePeriod_ONE_MIN)        -> "\"PT60S\"";
f_duration(?TimePeriod_FIVE_MIN)       -> "\"PT300S\"";
f_duration(?TimePeriod_FIFTEEN_MIN)    -> "\"PT900S\"";
f_duration(?TimePeriod_THIRTY_MIN)     -> "\"PT1800S\"";
f_duration(?TimePeriod_ONE_HOUR)       -> "\"PT3600S\"";
f_duration(?TimePeriod_TWELVE_HOUR)    -> "\"PT43200S\"";
f_duration(?TimePeriod_ONE_DAY)        -> "\"PT86400S\"".

%% f_meas_res(undefined) -> f_str("NIL");
%% f_meas_res(Str)       -> Str.

f_str(undefined) -> undefined;
f_str(String)    -> a(["\"", String, "\""]).

f_time(Time) ->
    a(["\"", f_time_nq(Time), "\""]).

f_time_nq(Secs) when is_integer(Secs) ->
    %% LT = calendar:now_to_local_time(secs_to_now(Secs)),
    LT = calendar:now_to_universal_time(secs_to_now(Secs)),
    f_time_nq(LT);
f_time_nq(Time) -> 
    %% LT    = {{Y, Mo, D}, {H, Mi, S}} = Now, 
    %% [UTC] = calendar:local_time_to_universal_time_dst(Now),
    %% {Sign, DH, DM} = time_diff(LT, UTC),
    {{Y, Mo, D}, {H, Mi, S}} = Time, 
    Sign = "+", 
    DH = 0, 
    DM = 0,
    a([f_int(Y), 
       "-", 
       padzero(Mo),
       "-", 
       padzero(D),
       "T", 
       padzero(H),
       ":",
       padzero(Mi),
       ":",
       padzero(S),
       Sign,
       padzero(DH),
       ":",
       padzero(DM)]).


f_datetime(Secs) when is_integer(Secs) ->
    %% LT = calendar:now_to_local_time(secs_to_now(Secs)),
    UTC = calendar:now_to_universal_time(secs_to_now(Secs)),
    f_datetime(UTC);
f_datetime(Now) ->
    %% LT    = {{Y, Mo, D}, {H, Mi, S}} = Now, 
    %% [UTC] = calendar:local_time_to_universal_time_dst(Now),
    %% {Sign, DH, DM} = time_diff(LT, UTC),
    {{Y, Mo, D}, {H, Mi, S}} = Now, 
    a([f_int(Y), 
       padzero(Mo),
       padzero(D),
       ".",
       padzero(H),
       padzero(Mi),
       null_if_zero(S)]).
       %% Sign,
       %% padzero(DH),
       %% padzero(DM)]).
    

f_endtime(Secs) when is_integer(Secs) ->
    %% LT = calendar:now_to_local_time(secs_to_now(Secs)),
    UTC = calendar:now_to_universal_time(secs_to_now(Secs)),
    f_endtime(UTC);
f_endtime({{_Y, _Mo, _D}, {H, Mi, S}} = _LT) ->
    %% [UTC] = calendar:local_time_to_universal_time_dst(LT),
    %% {Sign, DH, DM} = time_diff(LT, UTC),
    a([padzero(H),
       padzero(Mi),
       null_if_zero(S)]).
       %% Sign,
       %% padzero(DH),
       %% padzero(DM)]).

padzero(N) when N < 10 -> "0" ++ f_int(N);
padzero(N) -> f_int(N).

%% time_diff(LT, UTC) ->
%%     DiffSecs = calendar:datetime_to_gregorian_seconds(LT) - 
%%                calendar:datetime_to_gregorian_seconds(UTC),
%%     Sign = choose(DiffSecs >= 0, "+", "-"),
%%     {0, {H, M,_}} = calendar:seconds_to_daystime(DiffSecs),
%%     {Sign, H, M}.
    
null_if_zero(0) ->
    "";
null_if_zero(N) ->
    padzero(N).

secs_to_now(Secs) ->
    {Secs div 1000000, Secs rem 1000000, 0}.

%%==============================================
%% Misc functions
%%==============================================
a(List) -> iolist_to_binary([List]).
%%a(List) -> lists:append(List).
f(List) -> lists:flatten(List).
d(N)    -> [lists:duplicate(N, " ")].

choose(true,  T, _) -> T;
choose(false, _, F) -> F.


itl(Int) when is_integer(Int) ->
    integer_to_list(Int);
itl(L) when is_list(L) ->
    L.

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

