%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_lib.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_lib).
-vsn('/main/R1A/R2A/1').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2013 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/6      2012-03-07 etxpeno     Dialyzer fixes
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([
	 convert_id/1,
	 pad/1,
	 get_lih_dir/0,
	 get_LKF_path/1,
	 get_parameter_file_path/0,
	 seconds_to_midnight/1
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici.hrl").

-define(BLOCK, "lih").

-define(LICENSE_FILE, "licensekey.xml").
-define(LICENSE_FILE_BACKUP, ".licensekey.xml").
-define(LICENSE_FILE_TMP, "tmp.xml").

-define(PARAMETER_FILE, "parameters_01.lic").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec convert_id(Id) -> Result when
      Id :: string() | binary(),
      Result :: string().
%% Removes all characters except letters, digits and '/'
%% All letters should be capital
convert_id(Id) when is_binary(Id) ->
    convert_id(binary_to_list(Id));
convert_id(Id) when is_list(Id) ->
    convert_id(Id, []).

convert_id([],      A)                       -> lists:reverse(A);
convert_id([C|Cs],  A) when C >= $a, C =< $z -> convert_id(Cs, [(C+$A-$a)|A]);
convert_id([C|Cs],  A) when C >= $A, C =< $Z -> convert_id(Cs, [C|A]);
convert_id([$/|Cs], A)                       -> convert_id(Cs, [$/|A]);
convert_id([$_|Cs], A)                       -> convert_id(Cs, [$_|A]);
convert_id([C|Cs],  A) when C >= $0, C =< $9 -> convert_id(Cs, [C|A]);
convert_id([_|Cs],  A)                       -> convert_id(Cs, A).

-spec pad(Id) -> Result when
      Id :: string(),
      Result :: string().
%% Pad Id to 24 characters
pad(Id) ->
    Length = length(Id),
    Id ++ lists:duplicate(?CELLO_LICI_MAX_ID_LENGTH - Length, 0).

get_lih_dir() ->
    filename:join([sysEnv:rcs_dir(), ?BLOCK]).

-spec get_LKF_path(Type) -> Result when
      Type :: 'license' | 'backup' | 'tmp',
      Result :: string().
%% Return the path to the LKF
get_LKF_path(license) ->
    filename:join([get_lih_dir(), ?LICENSE_FILE]);
get_LKF_path(backup) ->
    filename:join([get_lih_dir(), ?LICENSE_FILE_BACKUP]);
get_LKF_path(tmp) ->
    filename:join([get_lih_dir(), ?LICENSE_FILE_TMP]).

-spec get_parameter_file_path() -> Result when
      Result :: string().
%% Return the path to the LICI parameter file
get_parameter_file_path() ->
    filename:join([get_lih_dir(), ?PARAMETER_FILE]).

-spec seconds_to_midnight(UTC) -> Result when
      UTC :: calendar:datetime(),
      Result :: 0..86400.
seconds_to_midnight(UTC) ->
    {LocalDate, LocalTime} = calendar:universal_time_to_local_time(UTC),
    NextDate = get_next_day(LocalDate),
    MidnightTime = {0, 0, 0},

    case calendar:local_time_to_universal_time_dst({NextDate, MidnightTime}) of
	[] ->
	    %% There is no local time 00:00:00 (23:59:59 -> 01:00:00)
	    %% Use a fictionary local time 00:00:00
	    24*60*60-calendar:time_to_seconds(LocalTime);
	Result ->
	    %% Result is a list containing one or two UTC
	    %% Use the first element in the list
	    MidnightUTC = hd(Result),
	    calendar:datetime_to_gregorian_seconds(MidnightUTC) -
		calendar:datetime_to_gregorian_seconds(UTC)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

get_next_day({Y, 12, 31}) ->
    {Y+1, 1, 1};
get_next_day({Y, M, D}) ->
    LastDay = calendar:last_day_of_the_month(Y, M),

    if D =:= LastDay ->
	    {Y, M+1, 1};
       D < LastDay ->
	    {Y, M, D+1}
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
