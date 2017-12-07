%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	sysUtil.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R10A/R11A/3

%%% @doc == System Utility Functions ==
%%% This module contains miscellaneous utility functions.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(sysUtil).
-vsn('/main/R3A/R4A/R5A/R6A/R7A/R10A/R11A/3').
-date('2017-10-17').
-author(etxberb).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%% Rev     Date       Name     What
%% R3A/    ---------- -------  ------------------------------------------------
%%% 1      2014-10-08 etxberb  Created.
%%% 3      2014-11-07 etxberb  Added pairElems/2.
%%% 4      2014-11-17 etxberb  Added timediff*/* and micrSecs_to_string/1.
%%% 5      2014-11-18 etxberb  Added timediff_before/1.
%%% 6      2014-11-19 etxberb  Added get_previous_module/0, etc.
%%% 7      2014-11-25 etxberb  Added timediff_before_skip/0.
%%% 8      2014-11-25 etxberb  Corrected get_previous_module/0.
%% R3A/9   2014-12-04 etxberb  Added timediff_after_and_previous/1, now_decr1/1
%% R3A/10  2015-02-26 etxberb  Added record_format/1 & record_format/2.
%% R3A/11  2015-04-02 etxberb  Added 'Report' option in timediff_enable.
%% R3A/12  2015-04-15 etxberb  Added pid_info/1, pid_info/2 & pid_name/1.
%% R4A/1   2015-06-10 etxberb  Enhanced timediff_XXX for managing XXXXL
%%                             measurements and added {details,{top,N}} option.
%% R4A/6   2015-10-15 etxberb  Added time_to_timestamp/1.
%% R4A/7   2015-10-16 etxberb  Added time_to_string/1 & time_to_string/2.
%% R4A/11  2015-11-06 etxberb  Added str_tokens/2.
%% R4A/12  2015-11-27 etxarnu  Added print_port_info/0,/1.
%% R5A/8   2015-11-20 etxpeno  Using erlang:monotonic_time()
%% R5A/9   2015-11-30 etxarnu  Added print_port_info/0,/1.
%% R5A/10  2015-12-17 etxberb  Added parallel_call/1,/2 & parallel_call_parent/0
%% R5A/11  2016-01-20 etxberb  Added remove_chars/2.
%% R5A/12  2016-02-10 etxberb  Added is_rev_inRange/2 & keyfind_all/3
%%                             is_rev_EqualOrGreater/2.
%% R5A/13  2016-02-16 etxberb  * Added rev_split/1, is_rev_equiv/2,
%%                               is_rev_higher/2
%%                             * Refactored is_rev_EqualOrGreater/2 algorithm
%%                               and changed name to is_rev_eqOrHi/2.
%% R5A/14  2016-02-17 etxberb  Further development of is_rev_XXX functions.
%% R5A/15  2016-02-29 etxberb  Enhancement of validate_rev/3.
%% R5A/16  2016-03-11 etxberb  Update of 'is_rev' algorithm regarding relation
%%                             between 'ordinary' & 'special'-with alpha-suffix.
%%                             According to hardware revision handling.
%% R5A/17  2016-03-14 etxberb  * Enhanced is_rev_equiv/2, is_rev_inRange/2 &
%%                               is_letters_higher/2.
%%                             * Added is_rev_range_equiv/2.
%% R5A/18  2016-05-25 uabesvi  HU87420 undefined port
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-05-18 etxberb  Added option results_order to parallel_call/X.
%% R6A/2   2016-05-25 uabesvi  merge from R15A/18 HU87420 undefined port
%% R6A/3   2016-08-11 etxberb  Bugfix in is_letters_higher/2.
%% ----    ---------- -------  ------------------------------------------------
%% R7A/1   2016-10-03 etxberb  Same handling for p_state as for r_state in
%%                             validate_rev/2.
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-06-20 etxberb  Added reason_phrase/1 & reason_phrase/2.
%% R9A/2   2017-06-21 etxberb  Added get_previous_modFun/0.
%% ----    ---------- -------  ------------------------------------------------
%% R10A/1  2017-06-20 etxberb  Merge from R9A/1.
%% R10A/3  2017-06-21 etxberb  Merge from R9A/2.
%% R10A/4  2017-06-27 eolaand  Add parallel_call/3 with fun and list of items.
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-10-11 etxarnu  OTP20: removed type def for exception_throw(_) 
%% R11A/3  2017-10-17 etxberb  Adaptions to OTP20.
%% ------  ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
-export([get_previous_modFun/0,
	 get_previous_module/0,
	 is_rev_equiv/2,
	 is_rev_eqOrHi/2,
	 is_rev_higher/2,
	 is_rev_inRange/2,
	 keyfind_all/3,
	 micrSecs_to_atom/1,
	 micrSecs_to_string/1,
	 now_decr1/1,
	 pairElems/2,
	 parallel_call/1,
	 parallel_call/2,
	 parallel_call/3,
	 parallel_call_parent/0,
	 pid_info/1,
	 pid_info/2,
	 pid_name/1,
	 reason_phrase/1,
	 reason_phrase/2,
	 record_format/1,
	 record_format/2,
	 remove_chars/2,
	 rev_split/1,
	 str_tokens/2,
	 term_to_string/1,
	 time_to_string/1,
	 time_to_string/2,
	 time_to_timestamp/1,
	 timediff_after/1,
	 timediff_after_and_previous/1,
	 timediff_before/1,
	 timediff_before_skip/0,
	 timediff_cancel/0,
	 timediff_disable/0,
	 timediff_enable/0,
	 timediff_enable/1,
	 timediff_enable/2,
	 timediff_enable/3,
	 timediff_start/0,
	 timediff_start/1,
	 timediff_status/0,
	 timediff_stop/0]).

-export([print_port_info/0]).
-export([print_port_info/1]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([activate/0]).
-export([init_parallel_call/2]).
-export([timediff_timeout/0]).

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(Tbl_timediff,             sysUtil_timediff).
-define(Tbl_timediff_enabled,     sysUtil_timediff_enabled).
-define(Tbl_timediff_sumInfo,     sysUtil_timediff_sumInfo).
-define(Tbl_timediff_top,         sysUtil_timediff_top).
-define(Tbl_timediff_sumInfo_pos_cnt,   2).
-define(Tbl_timediff_sumInfo_pos_tdiff, 3).
-define(Timediff_default_maxTime, 1800000).   % 30 minutes.
-define(Timediff_default_reportOpts, [{details, {top, 5}},
				      {summary, {tagLevel, 1}}]).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(ReportInfo),
	sysInitI:error_report(?RepInfo(ReportInfo))).
-define(LOG_ERR_ALL(ReportInfo),
	error_logger:error_report(?RepInfo(ReportInfo))).
-define(RepInfo(RepInfo),
	[{?MODULE, ?FUNCTION} | RepInfo]).

%% Usecase Error
-define(UC_ERR_3(Reason, ReportInfo), {uc_error, Reason, ?RepInfo(ReportInfo)}).

%% Reason for Usecase Error
-define(UC_ERR_InvalidRevFormat, "Invalid revision format").
-define(UC_ERR_InvalidRevRange, "Invalid revision range").
-define(UC_ERR_RevLengthOutOfRange, "Revision length out of range").
-define(UC_ERR_RevNotAString, "Revision is not a string").

%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(STACKTRACE_C,   % Current stacktrace
	element(2, process_info(self(), current_stacktrace))).
-define(STACKTRACE_E,   % Stacktrace at Exception
	erlang:get_stacktrace()).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(?Tbl_timediff, {key,
			data,
			report_opts}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###
-type rev_state() :: r_state | p_state.
-type rev_type()  :: ordinary | verification_state | special.
-type split_rev() :: {RevisionState     :: rev_state(),
		      Type              :: rev_type(),
		      Number            :: string(),
		      Letter            :: string(),
		      AmendmentOrSuffix :: string()}.

-type time_unit() :: seconds | milli_seconds | micro_seconds | nano_seconds.

%% -type uc_err_reason() :: string().
%% -type uc_err_3()      :: {uc_error,
%% 			  Reason :: uc_err_reason(),
%% 			  ReportInfo :: list()}.
%-type exception_throw(_) :: none().   % erlang:throw(uc_err_3())

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Get the previous module and function from call stack.
%%%
%%% @end
%%% ###=====================================================================###
get_previous_modFun() ->
    StackTrace = ?STACKTRACE_C,
    AskingModule = get_previous_module(StackTrace, ?MODULE),
    get_previous_modFun(StackTrace, AskingModule).

%%% ###---------------------------------------------------------------------###
get_previous_modFun([{Module, Function, _, _} | _], AskingModule)
  when Module /= AskingModule andalso
       Module /= ?MODULE ->
    {Module, Function};
get_previous_modFun([_ | Tail], AskingModule) ->
    get_previous_modFun(Tail, AskingModule);
get_previous_modFun([], _) ->
    undefined.

%%% ###########################################################################
%%% @doc Get the previous module from call stack.
%%%
%%% @end
%%% ###=====================================================================###
get_previous_module() ->
    StackTrace = ?STACKTRACE_C,
    AskingModule = get_previous_module(StackTrace, ?MODULE),
    get_previous_module(StackTrace, AskingModule).

%%% ###---------------------------------------------------------------------###
get_previous_module(AskingModule) ->
    get_previous_module(?STACKTRACE_C, AskingModule).

%%% ###---------------------------------------------------------------------###
get_previous_module([{Module, _, _, _} | _], AskingModule)
  when Module /= AskingModule andalso
       Module /= ?MODULE ->
    Module;
get_previous_module([_ | Tail], AskingModule) ->
    get_previous_module(Tail, AskingModule);
get_previous_module([], _) ->
    undefined.

%%% ###########################################################################
%%% @doc Compares a revision against another revision. Returns true
%%%   if the Revision compares equivalent with the CompareRevision.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec is_rev_equiv(Revision        :: string() | split_rev(),
		   CompareRevision :: string() | split_rev()) ->
			  boolean() |
			  none().
		      %	exception_throw(uc_err_3()).
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_rev_equiv(IsRev, CompRev) when not is_tuple(IsRev) andalso
				  not is_tuple(CompRev) ->
    is_rev_equiv(rev_split(IsRev), rev_split(CompRev));
is_rev_equiv(IsRev, {_, _, _, _, _} = CompRev) when not is_tuple(IsRev) ->
    is_rev_equiv(rev_split(IsRev), CompRev);
is_rev_equiv({_, _, _, _, _} = IsRev, CompRev) when not is_tuple(CompRev) ->
    is_rev_equiv(IsRev, rev_split(CompRev));
%%% ------- Algorithm below ------- Type conversion above -------
is_rev_equiv({_, _, _, _, _} = Rev, Rev) ->
    true;
is_rev_equiv({RevState, ordinary, RevNr, "", _},
	     {RevState, ordinary, RevNr, [_ | _], _}) ->
    false;
is_rev_equiv({RevState, ordinary, RevNr, [_ | _], _},
	     {RevState, ordinary, RevNr, "", _}) ->
    false;
is_rev_equiv({RevState, ordinary, RevNr, "", _},
	     {RevState, CompRevType, RevNr, _, _})
  when (CompRevType == ordinary orelse
	CompRevType == verification_state) ->
    true;
is_rev_equiv({RevState, IsRevType, RevNr, _, _},
	     {RevState, ordinary, RevNr, "", _})
  when (IsRevType == ordinary orelse
	IsRevType == verification_state) ->
    true;
is_rev_equiv({RevState, IsRevType, RevNr, RevLett, _},
	     {RevState, CompRevType, RevNr, RevLett, _})
  when ((IsRevType == ordinary orelse
	 IsRevType == verification_state) andalso
	(CompRevType == ordinary orelse
	 CompRevType == verification_state)) ->
    true;
is_rev_equiv({RevState, special, RevNr, _, [Char | _]},
	     {RevState, ordinary, RevNr, "", _})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    true;
is_rev_equiv({RevState, ordinary, RevNr, "", _},
	     {RevState, special, RevNr, _, [Char | _]})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    true;
is_rev_equiv({RevState, special, RevNr, RevLett, [Char | _]},
	     {RevState, ordinary, RevNr, RevLett, _})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    true;
is_rev_equiv({RevState, ordinary, RevNr, RevLett, _},
	     {RevState, special, RevNr, RevLett, [Char | _]})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    true;
is_rev_equiv(_, _) ->
    false.

%%% ###########################################################################
%%% @doc Compares a revision against another revision. Returns true
%%%   if the Revision compares equivalent or higher than the CompareRevision.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec is_rev_eqOrHi(Revision        :: string() | split_rev(),
		    CompareRevision :: string() | split_rev()) ->
			   boolean() |
			   none().
		      %	exception_throw(uc_err_3()).
	
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_rev_eqOrHi({_, _, _, _, _} = IsRev, {_, _, _, _, _} = CompRev) ->
    is_rev_equiv(IsRev, CompRev) orelse is_rev_higher(IsRev, CompRev);
%%% ------- Algorithm above ------- Type conversion below -------
is_rev_eqOrHi(IsRev, CompRev) when not is_tuple(IsRev) andalso
				   not is_tuple(CompRev) ->
    is_rev_eqOrHi(rev_split(IsRev), rev_split(CompRev));
is_rev_eqOrHi(IsRev, {_, _, _, _, _} = CompRev) when not is_tuple(IsRev) ->
    is_rev_eqOrHi(rev_split(IsRev), CompRev);
is_rev_eqOrHi({_, _, _, _, _} = IsRev, CompRev) when not is_tuple(CompRev) ->
    is_rev_eqOrHi(IsRev, rev_split(CompRev)).

%%% ###########################################################################
%%% @doc Compares a revision against another revision. Returns true
%%%   if the Revision compares higher than the CompareRevision.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec is_rev_higher(Revision        :: string() | split_rev(),
		    CompareRevision :: string() | split_rev()) ->
			   boolean() |
			   none().
                       %%  exception_throw(uc_err_3()).
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_rev_higher(IsRev, CompRev) when not is_tuple(IsRev) andalso
				   not is_tuple(CompRev) ->
    is_rev_higher(rev_split(IsRev), rev_split(CompRev));
is_rev_higher(IsRev, {_, _, _, _, _} = CompRev) when not is_tuple(IsRev) ->
    is_rev_higher(rev_split(IsRev), CompRev);
is_rev_higher({_, _, _, _, _} = IsRev, CompRev) when not is_tuple(CompRev) ->
    is_rev_higher(IsRev, rev_split(CompRev));
%%% ------- Algorithm below ------- Type conversion above -------
is_rev_higher({RevState, ordinary, RevNr, IsRevLett, _},
	      {RevState, ordinary, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, ordinary, IsRevNr, _, _},
	      {RevState, ordinary, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, ordinary, RevNr, RevLett, _},
	      {RevState, verification_state, RevNr, RevLett, _}) ->
    true;
is_rev_higher({RevState, ordinary, RevNr, IsRevLett, _},
	      {RevState, verification_state, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, ordinary, IsRevNr, _, _},
	      {RevState, verification_state, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, verification_state, RevNr, IsRevLett, _},
	      {RevState, ordinary, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, verification_state, IsRevNr, _, _},
	      {RevState, ordinary, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, verification_state, RevNr, RevLett, IsRevAmt},
	      {RevState, verification_state, RevNr, RevLett, CompRevAmt}) ->
    is_chars_higher(IsRevAmt, CompRevAmt);
is_rev_higher({RevState, verification_state, RevNr, IsRevLett, _},
	      {RevState, verification_state, RevNr, CompRevLett, _}) ->
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, verification_state, IsRevNr, _, _},
	      {RevState, verification_state, CompRevNr, _, _}) ->
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, special, RevNr, RevLett, IsRevSuff},
	      {RevState, special, RevNr, RevLett, CompRevSuff}) ->
    case is_same_charType(IsRevSuff, CompRevSuff) of
	true ->
	    is_chars_higher(IsRevSuff, CompRevSuff);
	false ->
	    false
    end;
is_rev_higher({RevState, special, RevNr, IsRevLett, IsRevSuff},
	      {RevState, special, RevNr, CompRevLett, CompRevSuff}) ->
    case is_same_charType(IsRevSuff, CompRevSuff) of
	true ->
	    is_chars_higher(IsRevLett, CompRevLett);
	false ->
	    false
    end;
is_rev_higher({RevState, special, IsRevNr, _, IsRevSuff},
	      {RevState, special, CompRevNr, _, CompRevSuff}) ->
    case is_same_charType(IsRevSuff, CompRevSuff) of
	true ->
	    is_chars_higher(IsRevNr, CompRevNr);
	false ->
	    false
    end;
is_rev_higher({RevState, ordinary, RevNr, RevLett, _},
	      {RevState, special, RevNr, RevLett, [Char | _]})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    true;
is_rev_higher({RevState, ordinary, RevNr, IsRevLett, _},
	      {RevState, special, RevNr, CompRevLett, [Char | _]})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, ordinary, IsRevNr, _, _},
	      {RevState, special, CompRevNr, _, [Char | _]})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher({RevState, special, RevNr, IsRevLett, [Char | _]},
	      {RevState, ordinary, RevNr, CompRevLett, _})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    is_chars_higher(IsRevLett, CompRevLett);
is_rev_higher({RevState, special, IsRevNr, _, [Char | _]},
	      {RevState, ordinary, CompRevNr, _, _})
  when Char >= $A andalso Char =< $Z ->
    %% Requirement / practice from hardware revision handling.
    is_chars_higher(IsRevNr, CompRevNr);
is_rev_higher(_, _) ->
    false.

%%% ###########################################################################
%%% @doc Compares a revision against a specified revision range. Returns true
%%%   if the Revision compares compatible with the specified RevisionRange.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec is_rev_inRange(Revision      :: string(),
		     RevisionRange :: string()) ->
			    boolean() |
			    none().
                         %% exception_throw(uc_err_3()).
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_rev_inRange(Rev, RevRange) ->
    case string:tokens(RevRange, "-") of
	[RawLowRev, RawHighRev] ->
	    %% Closed range
	    LowRev_split = rev_split(string:strip(RawLowRev)),
	    HighRev_split = rev_split(string:strip(RawHighRev)),
	    validate_rev(range, LowRev_split, RevRange),
	    validate_rev(range, HighRev_split, RevRange),
	    Rev_split = rev_split(Rev),
	    ((is_rev_range_equiv(Rev_split, LowRev_split) orelse
	      is_rev_higher(Rev_split, LowRev_split)) andalso
	     (is_rev_range_equiv(Rev_split, HighRev_split) orelse
	      is_rev_higher(HighRev_split, Rev_split)));
	[RawLimitRev] ->
	    LimitRev = string:strip(RawLimitRev),
	    case string:chr(RevRange, $-) of
		0 ->
		    %% Explicit R-state, absolute revision
		    LimitRev_split = rev_split(LimitRev),
		    validate_rev(explicit, LimitRev_split, RevRange),
		    is_rev_range_equiv(rev_split(Rev), LimitRev_split);
		1 ->
		    %% Open ended range, to revision
		    LimitRev_split = rev_split(LimitRev),
		    validate_rev(range, LimitRev_split, RevRange),
		    Rev_split = rev_split(Rev),
		    (is_rev_range_equiv(Rev_split, LimitRev_split) orelse
		     is_rev_higher(LimitRev_split, Rev_split));
		_ ->
		    %% Open ended range, from revision
		    LimitRev_split = rev_split(LimitRev),
		    validate_rev(range, LimitRev_split, RevRange),
		    Rev_split = rev_split(Rev),
		    (is_rev_range_equiv(Rev_split, LimitRev_split) orelse
		     is_rev_higher(Rev_split, LimitRev_split))
	    end;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevRange, [{rev_range, RevRange}]))
    end.

%%% ###########################################################################
%%% @doc Equivalent with 'lists:keyfind/3', but returns a list of all tuples
%%%   found in the TupleList rather than only the first found.
%%%
%%% @end
%%% ###=====================================================================###
keyfind_all(Key, N, TupleList) ->
    case keyfind_all_loop(TupleList, Key, N) of
	[] ->
	    false;
	FoundTuples ->
	    FoundTuples
    end.

keyfind_all_loop([Tuple | Tail], Key, N) when element(N, Tuple) == Key ->
    [Tuple | keyfind_all_loop(Tail, Key, N)];
keyfind_all_loop([_ | Tail], Key, N) ->
    keyfind_all_loop(Tail, Key, N);
keyfind_all_loop([], _, _) ->
    [].

%%% ###########################################################################
%%% @doc Transform an integer time difference in micro seconds to an atom in seconds.
%%%
%%% @end
%%% ###=====================================================================###
micrSecs_to_atom(Time) ->
    list_to_atom(micrSecs_to_string(Time)).

%%% ###########################################################################
%%% @doc Transform an integer time difference in micro seconds to a string in seconds.
%%%
%%% @end
%%% ###=====================================================================###
micrSecs_to_string(Time) ->
    MicrSecs = integer_to_list(Time rem 1000000),
    integer_to_list(Time div 1000000) ++
	"," ++
	lists:nthtail(length(MicrSecs), "000000" ++ MicrSecs) ++
	" seconds".

%%% ###########################################################################
%%% @doc Decrease a now-formatted timestamp by 1.
%%%
%%% @end
%%% ###=====================================================================###
now_decr1({MegSec, 0, 0}) ->
    {MegSec - 1, 999999, 999999};
now_decr1({MegSec, Sec, 0}) ->
    {MegSec, Sec - 1, 999999};
now_decr1({MegSec, Sec, MicrSec}) ->
    {MegSec, Sec, MicrSec - 1};
now_decr1(Other) ->
    sysInitI:warning_report([{?MODULE, now_decr1},
				 {faulty_argument, Other},
				 {module, get_previous_module()}]),
    Other.

%%% ###########################################################################
%%% @doc Pair each element in <i>List1</i> with the element from <i>List2</i>
%%%   at corresponding list position into a tuple.
%%%   If the two lists are of different size, the atom 'undefined' replaces
%%%   the missing position of the other list.
%%%
%%%   Example:
%%%   > sysUtil:pairElems([1, 2, 3], [a, b]).
%%%   [{1,a},{2,b},{3,undefined}]
%%%
%%% @end
%% -spec pairElems(List1 :: list(any()),
%% 		List2 :: list(any())) ->
%%     list(tuple(ElementXList1::any(), ElementXList2::any())).
%%% ###=====================================================================###
pairElems([E1 | Tail1], [E2 | Tail2]) ->
    [{E1, E2} | pairElems(Tail1, Tail2)];
pairElems([], [E2 | Tail2]) ->
    [{undefined, E2} | pairElems([], Tail2)];
pairElems([E1 | Tail1], []) ->
    [{E1, undefined} | pairElems(Tail1, [])];
pairElems([], []) ->
    [].

%%% ###########################################################################
%%% @doc Execute a list of tasks in separate processes and wait for all tasks
%%%   to finish before returning.
%%%
%%% @end
%%% ###=====================================================================###
parallel_call(Tasks) ->
    parallel_call(Tasks, [{spawn_opt, []},
			  {timeout, infinity},
			  {results_order, chrono_reverse}]).

parallel_call(Tasks, Options) ->
    SpawnOpts = parallel_call_spawnOpts(Options),
    PidsTasks = parallel_call_spawn(Tasks, SpawnOpts),
    {ok, TimerRef} =
	case lists:keyfind(timeout, 1, Options) of
	    {timeout, infinity} ->
		{ok, undefined};
	    {timeout, Time} ->
		timer:send_after(Time, {?MODULE, parallel_call_timeout});
	    false ->
		{ok, undefined}
	end,
    parallel_call_receive(parallel_call_resultsOrder(Options),
			  PidsTasks,
			  [],
			  TimerRef).


parallel_call(Fun, Items, Options) ->
    parallel_call({Fun, Items}, Options).


parallel_call_parent() ->
    get({?MODULE, parent}).

parallel_call_spawnOpts([{spawn_opt, SpawnOpts} | _]) ->
    SpawnOpts;
parallel_call_spawnOpts([_ | Tail]) ->
    parallel_call_spawnOpts(Tail);
parallel_call_spawnOpts([]) ->
    [].

parallel_call_spawn({Fun, Items}, SpawnOpts) ->
    [begin
	 Task = fun() -> Fun(Item) end,
	 {erlang:spawn_opt(?MODULE,
			   init_parallel_call,
			   [Task, self()],
			   SpawnOpts),
	  Task} 
     end
     || Item <- Items];
parallel_call_spawn(Tasks, SpawnOpts) ->
    [{erlang:spawn_opt(?MODULE,
		       init_parallel_call,
		       [Task, self()],
		       SpawnOpts),
      Task}
     || Task <- Tasks].

parallel_call_resultsOrder([{results_order, chrono_reverse} | _]) ->
    {chrono, reverse};
parallel_call_resultsOrder([{results_order, chrono} | _]) ->
    {chrono, upright};
parallel_call_resultsOrder([{results_order, match_task_reverse} | _]) ->
    {match_task, reverse};
parallel_call_resultsOrder([{results_order, match_task} | _]) ->
    {match_task, upright};
parallel_call_resultsOrder([_ | Tail]) ->
    parallel_call_resultsOrder(Tail);
parallel_call_resultsOrder([]) ->
    %% Default
    {chrono, reverse}.

parallel_call_receive({match_task, Direction} = ResultsOrder,
		      [{Pid, _} | Tail],   % Pid match against message below
		      Results,
		      TimerRef) ->
    receive
	%% Matching Pid - the first element in the PidsTask list above,
	%% consuming results in the order specified by input TaskList:
	{?MODULE, parallel_call_done, Result, Pid} ->
	    parallel_call_receive(ResultsOrder,
				  Tail,
				  elem_add(Direction, Result, Results),
				  TimerRef);
	{?MODULE, parallel_call_timeout = Error} ->
	    parallel_call_error(Error, Tail)
    end;
parallel_call_receive({chrono, Direction} = ResultsOrder,
		      [_ | _] = PidsTasks,
		      Results,
		      TimerRef) ->
    receive
	%% Consuming results in the order they are received - chronological:
	{?MODULE, parallel_call_done, Result, Pid} ->
	    parallel_call_receive(ResultsOrder,
				  lists:keydelete(Pid, 1, PidsTasks),
				  elem_add(Direction, Result, Results),
				  TimerRef);
	{?MODULE, parallel_call_timeout = Error} ->
	    parallel_call_error(Error, PidsTasks)
    end;
parallel_call_receive(_, [], Results, TimerRef) ->
    timer:cancel(TimerRef),
    {ok, Results}.

parallel_call_error(Error, PidsTasks) ->
    [begin
	 ?LOG_ERR([Error, Task | pid_info(Pid)]),
	 exit(Pid, kill)
     end
     || {Pid, Task} <- PidsTasks],
    {error, {timeout, [Task || {_, Task} <- PidsTasks]}}.

init_parallel_call(Task, CallerPid) ->
    put({?MODULE, parent}, CallerPid),
    supervise_parallel_call(Task, CallerPid).

supervise_parallel_call(Task, CallerPid) ->
    Result =
	try
	    case Task of
		{M, F, A} ->
		    erlang:apply(M, F, A);
		_ when is_function(Task) ->
		    Task()
	    end
	catch
	    ErrClass : ErrReason ->
		Stacktrace = ?STACKTRACE_E,
		?LOG_ERR([{task, Task},
			  {ErrClass, ErrReason},
			  {stacktrace, Stacktrace}]),
		{ErrClass, ErrReason}
	end,
    CallerPid ! {?MODULE, parallel_call_done, Result, self()},
    exit(normal).

%%% ###########################################################################
%%% @doc Collect information about a process and return in a
%%%   format that fits well in e.g. sysInitI:info_report'.
%%%
%%%   Note 1: Item 'registered_name' is always included first in the list
%%%   regardless of the 'IncludedItems' and 'ExcludedItems' arguments.
%%%
%%%   Note 2: Items 'error_handler' and 'messages' are excluded for
%%%   'IncludedItems' = 'all'.
%%%
%%% @end
-spec pid_info(Pid :: pid() | integer() | {pid(), any()}) ->
    list().
%%% ###=====================================================================###
pid_info(Pid) ->
    pid_info(Pid, all).

%%% ###########################################################################
-spec pid_info(Pid :: pid() | integer() | {pid(), any()},
	       IncludedItems :: all | {all, ExcludedItems :: list(atom())} | list(atom())) ->
    list().
%%% ###=====================================================================###
pid_info(Pid, InclItems) when is_pid(Pid) ->
    case InclItems of
	{all, ExclItems} when is_list(ExclItems) ->
	    MyExclItems = [registered_name | ExclItems],
	    MyInclItems = all;
	all ->
	    MyExclItems = [registered_name, messages, error_handler],
	    MyInclItems = all;
	_ when is_list(InclItems) ->
	    MyExclItems = [registered_name],
	    MyInclItems = InclItems;
	_ ->
	    MyExclItems = not_applicable,
	    MyInclItems = not_applicable,
	    erlang:error({badarg, InclItems})
    end,
    pid_info_tuples(MyInclItems, Pid, [{process_type, erlang}], MyExclItems);
pid_info(Pid, _) when is_integer(Pid) ->
    RegName =
	case cec:get_program_name(Pid) of
	    String when is_list(String) ->
		list_to_atom(String);
	    Atom when is_atom(Atom) ->
		Atom
	end,
    [{terms_to_atom(Pid, " ", process_type), linux},
     {terms_to_atom(Pid, " ", name), RegName}];
pid_info({Pid, _}, Items) ->
    pid_info(Pid, Items);
pid_info(Other, _) ->
    [{bad_process, Other}].

%%% ###=====================================================================###
pid_info_name(Pid) ->
    try erlang:process_info(Pid, registered_name) of
	[] ->
	    {name, undefined};
	{_, Name} ->
	    {name, Name}
    catch
	_ : _ ->
	    {bad_process, Pid}
    end.

%%% ###=====================================================================###
pid_info_tuples(all, Pid, HeadInfoTuples, ExclItems) ->
    InfoTuples =
	HeadInfoTuples ++
	case erlang:is_process_alive(Pid) of
	    true ->
		[pid_info_name(Pid) | erlang:process_info(Pid)];
	    false ->
		[{process_status, not_alive}]
	end,
    [{terms_to_atom(Pid, " ", Item), Value} ||
	{Item, Value} <- lists_delete(ExclItems, InfoTuples)];
pid_info_tuples(Items, Pid, HeadInfoTuples, ExclItems) when is_list(Items) ->
    InfoTuples =
	HeadInfoTuples ++
	case erlang:is_process_alive(Pid) of
	    true ->
		[pid_info_name(Pid) |
		 [try
		      erlang:process_info(Pid, Item)
		  catch
		      _ : _ ->
			  {Item, 'ERROR: invalid_item'}
		  end ||
		     Item <- Items]];
	    false ->
		[{process_state, not_alive}]
	end,
    [{terms_to_atom(Pid, " ", Item), Value} ||
	{Item, Value} <- lists_delete(ExclItems, InfoTuples)].

%%% ###########################################################################
%%% @doc Get the registered name of a process and return in a
%%%   format that fits well in e.g. sysInitI:info_report'.
%%%
%%% @end
-spec pid_name(Pid :: pid() | integer()) ->
    list().
%%% ###=====================================================================###
pid_name(Pid) ->
    pid_info(Pid, []).

%%% ###########################################################################
%%% @doc Format an error reason to a readable string.
%%%
%%% @end
-spec reason_phrase(Reason :: term()) ->
    string().
%%% ###=====================================================================###
reason_phrase(Reason) when is_integer(Reason) ->
    httpd_util:reason_phrase(Reason);
reason_phrase(eof) ->
    "End of file";
reason_phrase(Reason) when is_atom(Reason) ->
    case file:format_error(Reason) of
	"unknown POSIX error" ->
	    ReasonStrList = string:tokens(atom_to_list(Reason), "_"),
	    lists:droplast(lists:flatten([E ++ " " || E <- ReasonStrList]));
	ReasonPhrase ->
	    ReasonPhrase
    end;
reason_phrase(Reason) ->
   term_to_string(Reason).

-spec reason_phrase(Reason :: term(),
		    Item   :: string()) ->
    string().
%% ###=======================================================================###
reason_phrase(Reason, Item) ->
    reason_phrase(Reason) ++ ": " ++ Item.

%%% ###########################################################################
%%% @doc Combine 'record_info(fields, 'record')' and the 'record' Data into a
%%%   format that fits well in e.g. sysInitI:info_report'.
%%%
%%% @end
-spec record_format(RecordData :: tuple()) ->
    list().
%%% ###=====================================================================###
record_format(Rec) when is_tuple(Rec) ->
    record_format(record_info_default(length(tuple_to_list(Rec))), Rec).

%%% ###########################################################################
-spec record_format(RecordInfo :: list(),
		    RecordData :: tuple()) ->
    list().
%%% ###=====================================================================###
record_format(RecInfo, Rec) when is_list(RecInfo) andalso is_tuple(Rec) ->
    try
	begin
	    [RecordName | RecordElements] = tuple_to_list(Rec),
	    RecHead = list_to_atom("#" ++ atom_to_list(RecordName)),
	    pairElems([terms_to_atom(RecHead, ".", Item) || Item <- RecInfo],
		      RecordElements)
	end
    catch
	Class : Reason ->
	    [{Class, Reason}]
    end;
record_format(OtherRecInfo, OtherRec) ->
    [{OtherRecInfo, OtherRec}].

%%% ###########################################################################
%%% @doc Remove specified character / characters from a string.
%%%
%%% @end
-spec remove_chars(CharsToBeRemoved :: integer() | list(integer()),
		   String :: string()) ->
    string().
%%% ###=====================================================================###
remove_chars(RemoveChar, String) when is_integer(RemoveChar) ->
    lists:filter(fun(Char) when Char == RemoveChar ->
			 false;
		    (_) ->
			 true
		 end,
		 String);
remove_chars([RemoveChar | Tail], String) ->
    remove_chars(Tail, remove_chars(RemoveChar, String));
remove_chars([], String) ->
    String.

%%% ###########################################################################
%%% @doc Validates and splits a Revision string into the different parts it
%%%   consists of.
%%%
%%% @end
-spec rev_split(Revision :: string()) ->
		       split_rev() |
		       none().
              %%	exception_throw(uc_err_3()).
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
rev_split(Rev) ->
    validate_rev(Rev),
    {RevState, Tail1} = rev_state(Rev),
    {Number, Tail2} = rev_digits(Tail1),
    validate_revNumberLen(Number, Rev),
    {Letter, Tail3} = rev_letters(Tail2, Rev),
    validate_revLetterLen(Letter, Tail3, Rev),
    {Type, AmendmentOrSuffix} = rev_type(Tail3, Rev),
    {RevState, Type, Number, Letter, AmendmentOrSuffix}.

%%% ###########################################################################
%%% @doc Returns a list of tokens in String, separated by the characters in
%%%      SeparatorList.
%%%
%%%   Same as string:tokens/2 except that this function will treat two or more
%%%   adjacent separator characters in String differently; An empty string will
%%%   be inserted between such adjacent separators in the resulting list of
%%%   tokens. An empty string will also be inserted as first / last token when a
%%%   separator appears first / last respectively in String.
%%%
%%%   Example:
%%%   > sysUtil:str_tokens("abc defxxghix jkl", "x ").
%%%   ["abc", "def", [], "ghi", [], "jkl"]
%%%
%%%   ((( Compare with the --almost same-- function in module string )))
%%%   > string:tokens("abc defxxghix jkl", "x ").
%%%   ["abc", "def", "ghi", "jkl"]
%%%
%%% @end
-spec str_tokens(String     :: string(),
		 Separators :: string()) ->
    [Token :: string()].
%%% ###=====================================================================###
str_tokens([_ | _] = String, Separators) ->
    case str_token(String, Separators) of
	{Token, Tail} ->
	    [Token | str_tokens(Tail, Separators)];
	{Token, LastToken, Tail} ->
	    [Token, LastToken | str_tokens(Tail, Separators)]
    end;
str_tokens([], _) ->
    [].

%%% ###########################################################################
%%% @doc Transform any Erlang term to a string.
%%%
%%% @end
%%% ###=====================================================================###
term_to_string(Term) when is_atom(Term) ->
    atom_to_list(Term);
term_to_string(Term) when is_integer(Term) ->
    integer_to_list(Term);
term_to_string(Term) when is_float(Term) ->
    float_to_list(Term);
term_to_string(Term) when is_list(Term) ->
    try io_lib:format("~s", [Term]),
	Term
    catch
	_ : _ ->
	    "[" ++ list_to_string(Term) ++ "]"
    end;
term_to_string(Term) when is_tuple(Term) ->
    "{" ++ list_to_string(tuple_to_list(Term)) ++ "}";
term_to_string(Term) when is_binary(Term) ->
    binary_to_list(Term);
term_to_string(Term) when is_pid(Term) ->
    pid_to_list(Term);
term_to_string(Term) when is_reference(Term) ->
    erlang:ref_to_list(Term);
term_to_string(Term) when is_function(Term) ->
    erlang:fun_to_list(Term);
term_to_string(Term) when is_port(Term) ->
    erlang:port_to_list(Term);
term_to_string(_) ->
    "unrecognized_term".

%%% ###########################################################################
%%% @doc Transform a native time integer to a string.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec time_to_string(NativeTime :: integer()) ->
    string().
%%% ###=====================================================================###
time_to_string(NativeTime) ->
    time_to_string(NativeTime, nano_seconds).

%%% ###########################################################################
%%% @equiv time_to_string(NativeTime, nano_seconds)
-spec time_to_string(NativeTime :: integer(),
		     TimeUnit :: time_unit()) ->
    string().
%%% ###=====================================================================###
time_to_string(NativeTime, TimeUnit) ->
    {ConvertedTime, Filler, Divider} =
	try
	    Time = erlang:convert_time_unit(NativeTime, native, TimeUnit),
	    case TimeUnit of
		seconds ->
		    {Time, "", 1};
		milli_seconds ->
		    {Time, "000", 1000};
		micro_seconds ->
		    {Time, "000000", 1000000};
		nano_seconds ->
		    {Time, "000000000", 1000000000}
	    end
	catch
	    ErrClass : ErrReason ->
		Stacktrace = ?STACKTRACE_E,
		?LOG_ERR([{'TimeUnit', TimeUnit},
			  {assumed_TimeUnit, native},
			  {ErrClass, ErrReason},
			  {stacktrace, Stacktrace}]),
		{NativeTime, "000000000", 1000000000}
	end,
    TimeString = integer_to_list(ConvertedTime rem Divider),
    integer_to_list(ConvertedTime div Divider) ++
	if
	    TimeUnit == seconds ->
		"";
	    ?ELSE ->
		","
	end ++
	add_us(lists:nthtail(length(TimeString), Filler ++ TimeString)) ++
	" seconds".

add_us([E1, E2, E3]) ->
    [E1, E2, E3];
add_us([E1, E2, E3 | Tail]) ->
    [E1, E2, E3, $_ | add_us(Tail)];
add_us(Tail) ->
    Tail.

%%% ###########################################################################
%%% @doc Convert a native time integer to erlang:timestamp format.
%%%
%%% @end
%%% ###=====================================================================###
time_to_timestamp(NativeTime) ->
    NativeTimeMs = erlang:convert_time_unit(NativeTime, native, micro_seconds),
    MegaSecs = NativeTimeMs div 1000000000000,
    Secs = NativeTimeMs div 1000000 - MegaSecs * 1000000,
    MicroSecs = NativeTimeMs rem 1000000,
    {MegaSecs, Secs, MicroSecs}.

%%% ###########################################################################
%%% @doc Add a time difference object to a measurement if the measurement is
%%%   started.
%%%
%%% @end
%%% ###=====================================================================###
timediff_after(disabled) ->
    disabled;
timediff_after(Before) ->
    timediff_after(Before, erlang:monotonic_time()).

timediff_after_and_previous(disabled) ->
    disabled;
timediff_after_and_previous(Before) ->
    timediff_previous(Before),
    timediff_after(Before, erlang:monotonic_time()).

timediff_after(disabled, _) ->
    disabled;
timediff_after({Tag, TsBefore, Key}, TsAfter) ->
    ets:insert(?Tbl_timediff, #?Tbl_timediff{key = Key,
					     data = {Tag, TsBefore, TsAfter}}).

timediff_previous(disabled) ->
    disabled;
timediff_previous({Tag, TsBefore, {Id, StartTime, _} = Key}) ->
    case ets:prev(?Tbl_timediff, Key) of
	KeyPrev when KeyPrev /= '$end_of_table' ->
	    case ets:lookup(?Tbl_timediff, KeyPrev) of
		[#?Tbl_timediff{data = {sysUtil_between, _, _}}] ->
		    case ets:lookup(?Tbl_timediff,
				    ets:next(?Tbl_timediff, KeyPrev))
			of
			[#?Tbl_timediff{key = {Id, StartTime, _},
					data = {_, _, TsPrev}}]  ->
			    TsPrev;
			_ ->
			    TsPrev = StartTime
		    end,
		    KeyBetween = {Id, StartTime, TsBefore-1},
		    DataBetween = {sysUtil_between, TsPrev, TsBefore},
		    ets:insert(?Tbl_timediff,
			       #?Tbl_timediff{key = KeyBetween,
					      data = DataBetween});
		[#?Tbl_timediff{key = {Id, StartTime, _}}] ->
		    timediff_previous({Tag, TsBefore, KeyPrev});
		_ ->
		    KeyBetween = {Id, StartTime, TsBefore-1},
		    DataBetween = {sysUtil_between, StartTime, TsBefore},
		    ets:insert(?Tbl_timediff,
			       #?Tbl_timediff{key = KeyBetween,
					      data = DataBetween})
	    end;
	'$end_of_table' ->
	    KeyBetween = {Id, StartTime, TsBefore-1},
	    DataBetween = {sysUtil_between, StartTime, TsBefore},
	    ets:insert(?Tbl_timediff,
		       #?Tbl_timediff{key = KeyBetween,
				      data = DataBetween})
    end;
timediff_previous(_) ->
    ok.

%%% ###########################################################################
%%% @doc Take a timestamp. Return value to be used in timediff_after.
%%%
%%% @end
%%% ###=====================================================================###
timediff_before(Tag) ->
    timediff_before(Tag, self()).

timediff_before(Tag, Id) ->
    case timediff_status() of
	disabled ->
	    disabled;
	_ ->
	    case ets:lookup(?Tbl_timediff, {ongoing, Id}) of
		[#?Tbl_timediff{data = {_, _, StartTime}}] ->
		    TsBefore = erlang:monotonic_time(),
		    Key = {Id, StartTime, TsBefore},
		    {Tag, TsBefore, Key};
		[] ->
		    timediff_start(),
		    timediff_before(Tag)
	    end
    end.

%%% ###########################################################################
%%% @doc Skip a timestamp. Return value to be used in timediff_after.
%%%
%%% @end
%%% ###=====================================================================###
timediff_before_skip() ->
    disabled.

%%% ###########################################################################
%%% @doc Cancel time difference measurements. No results are reported.
%%%
%%% @end
%%% ###=====================================================================###
timediff_cancel() ->
    mnesia:delete_table(?Tbl_timediff_enabled),
    catch ets:delete(?Tbl_timediff_sumInfo),
    timediff_timer_cancel(),
    mnesia:delete_table(?Tbl_timediff).

%%% ###########################################################################
%%% @doc Disable time difference measurements and report the result.
%%%
%%% @end
%%% ###=====================================================================###
timediff_disable() ->
    Key =
	try
	    timediff_stop_all(ets:next(?Tbl_timediff, {before_ongoing, id})),
	    ets:next(?Tbl_timediff, {finished, erlang:system_info(start_time), id})
	catch
	    _ : _ ->
		'$end_of_table'
	end,
    mnesia:delete_table(?Tbl_timediff_enabled),
    timediff_report(Key),
    timediff_cancel().

%%% ###########################################################################
%%% @doc Enable time difference measurements.
%%%
%%% @end
%%% ###=====================================================================###
timediff_enable() ->
    timediff_enable(?Timediff_default_reportOpts).

%%% ###=====================================================================###
timediff_enable(ReportOpts) ->
    timediff_enable(ReportOpts, mnesia:system_info(db_nodes)).

%%% ###=====================================================================###
timediff_enable(ReportOpts, DbNodes) ->
    timediff_enable(ReportOpts, DbNodes, ?Timediff_default_maxTime).

%%% ----------------------------------------------------------
-spec timediff_enable(Opts:: all | list({details, {top, integer()} | all} |
					{summary, {tagLevel, integer()} | all}),
		      DbNodes :: list(atom()),
		      MaxTime :: integer()) ->
    {ok, UsedOpts :: list()} | {aborted, Reason :: term()}.
%%% ###=====================================================================###
timediff_enable(ReportOpts, DbNodes, MaxTime) ->
    MyReportOpts = validate_ReportOpts(ReportOpts),
    case
	mnesia:create_table(?Tbl_timediff,
			    [{type, ordered_set},
			     {ram_copies, DbNodes},
			     {attributes, record_info(fields, ?Tbl_timediff)}])
	of
	{atomic, ok} ->
	    {ok, TimerRef} =
		timer:apply_after(MaxTime, ?MODULE, timediff_timeout, []),
	    mnesia:dirty_write(#?Tbl_timediff{key = enabled,
					      data = {os:timestamp(),
						      TimerRef},
					      report_opts = MyReportOpts}),
	    mnesia:create_table(?Tbl_timediff_enabled,
				[{type, ordered_set}, {ram_copies, DbNodes}]),
	    {ok, MyReportOpts};
	Aborted ->
	    Aborted
    end.

%%% ###########################################################################
%%% @doc Check status of the timediff functionality.
%%%
%%% @end
%%% ###=====================================================================###
timediff_status() ->
    case ets:info(?Tbl_timediff_enabled) of
	undefined ->
	    disabled;
	_ ->
	    [#?Tbl_timediff{data = {Since, _TimerRef}}] =
		mnesia:dirty_read(?Tbl_timediff, enabled),
	    {enabled, {since, calendar:now_to_local_time(Since)}}
    end.

%%% ###########################################################################
%%% @doc Start a timediff measurement if enabled.
%%%
%%% @end
%%% ###=====================================================================###
timediff_start() ->
    timediff_start(undefined).

timediff_start(Tag) ->
    timediff_start(Tag, self()).

timediff_start(Tag, Id) ->
    case timediff_status() of
	disabled ->
	    disabled;
	_ ->
	    StartTime = erlang:monotonic_time(),
	    Data = {get_previous_module(?MODULE), Tag, StartTime},
	    ets:insert(?Tbl_timediff, #?Tbl_timediff{key = {ongoing, Id},
						     data = Data}),
	    Id
    end.

%%% ###########################################################################
%%% @doc Stop a timediff measurement if started.
%%%
%%% @end
%%% ###=====================================================================###
timediff_stop() ->
    timediff_stop(self()).

timediff_stop(Id) ->
    StopTime = erlang:monotonic_time(),
    try ets:lookup(?Tbl_timediff, {ongoing, Id}) of
	[#?Tbl_timediff{data = {_, _, StartTime} = Data}] ->
	    timediff_after({'======= total =======',
			    StartTime,
			    {Id, StartTime, StopTime}},
			   StopTime),
	    ets:insert(?Tbl_timediff, #?Tbl_timediff{key = {finished,
							    Data,
							    Id},
						     data = StopTime}),
	    ets:delete(?Tbl_timediff, {ongoing, Id})
    catch
	_ : _ ->
	    ok
    end.




%%% ###=====================================================================###
%%% @doc Print info about all ports and owner processes
%%% @end
%%%
print_port_info() ->
    print_port_info(standard_io).

print_port_info(File) when is_list(File) ->
    {ok,FD} = file:open(File,[write]),
    print_port_info(FD),
    file:close(FD);
print_port_info(FD) ->
    Ports = erlang:ports(),
    io:format(FD,"~nInfo for ~p  ports~n"
	      "#####################~n",[length(Ports)]),
    [handle_port(FD, erlang:port_info(P), P) || P <- Ports].


%%% ###=====================================================================###
handle_port(FD,undefined,Port) ->
    io:format(FD,"~ninfo for ~p : port not found~n",[Port]);
handle_port(FD,PortInfo,_) ->
    {_,Type} = proplists:lookup(name,PortInfo),
    {_,Pid} = proplists:lookup(connected,PortInfo),
    Info = erlang:process_info(Pid),
    io:format(FD,"~ninfo for ~p :  ~p~n",[Type,Info]).

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% Start phase.
%%%
%%% ###=====================================================================###
activate() ->
    case timediff_status() of
	disabled ->
	    ok;
	_ ->
	    timediff_cancel(),
	    ok
    end.

%%% ###########################################################################
%%%
%%% ###=====================================================================###
timediff_timeout() ->
    timediff_disable(),
    timediff_cancel().   % Just to be really really safe!

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.3.1 Help functions for is_rev_inRange/2 and is_rev_eqOrHi/2
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% is_chars_higher
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_chars_higher([_ | _], "") ->
    true;
is_chars_higher("", [_ | _]) ->
    false;
is_chars_higher(Chars1, Chars2) ->
    try
	list_to_integer(Chars1) > list_to_integer(Chars2)
    catch
	error : badarg ->
	    is_letters_higher(Chars1, Chars2)
    end.

%%% ###########################################################################
%%% is_letters_higher
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_letters_higher(Chars1, Chars2) when length(Chars1) < length(Chars2) ->
    false;
is_letters_higher(Chars1, Chars2) when length(Chars1) > length(Chars2) ->
    true;
is_letters_higher(Chars1, Chars2) ->
    Chars1 > Chars2.

%%% ###########################################################################
%%% is_rev_range_equiv
%%% Compares a revision with a revision range or part of a range.
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_rev_range_equiv({RevState, ordinary, RevNr, [_ | _], _},
		   {RevState, ordinary, RevNr, "", _}) ->
    %% Overrides normal equivalence between two revisions.
    true;
is_rev_range_equiv(Rev, RevRange) ->
    is_rev_equiv(Rev, RevRange).

%%% ###########################################################################
%%% is_same_charType
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
is_same_charType([Char1 | _], [Char2 | _])
  when (Char1 >= $0 andalso
	Char1 =< $9 andalso
	Char2 >= $0 andalso
	Char2 =< $9) ->
    true;
is_same_charType([Char1 | _], [Char2 | _])
  when (Char1 >= $A andalso
	Char1 =< $Z andalso
	Char2 >= $A andalso
	Char2 =< $Z) ->
    true;
is_same_charType(_, _) ->
    false.

%%% ###########################################################################
%%% rev_state
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
rev_state([$R | Tail]) ->
    {r_state, Tail};
rev_state([$P | Tail]) ->
    {p_state, Tail};
rev_state(Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision neither 'R'- nor 'P'-state",
		     {revision, Rev}])).

%%% ###########################################################################
%%% rev_digits
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
rev_digits(RevTail) ->
    rev_digits(RevTail, []).

rev_digits([Char | Tail], Acc) when Char >= $0 andalso Char =< $9 ->
    rev_digits(Tail, Acc ++ [Char]);
rev_digits(Tail, Acc) ->
    {Acc, Tail}.

%%% ###########################################################################
%%% rev_letters
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
rev_letters(RevTail, Rev) ->
    rev_letters(RevTail, [], Rev).

rev_letters([Char | Tail], Acc, Rev) when Char >= $A andalso Char =< $Z ->
    validate_revLetter(Char, Rev),
    rev_letters(Tail, Acc ++ [Char], Rev);
rev_letters(Tail, Acc, Rev) ->
    validate_revLetters(Acc, Rev),
    {Acc, Tail}.

%%% ###########################################################################
%%% rev_type
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
rev_type([$/ | Suffix], Rev) ->
    validate_revSuffix(Suffix, Rev),
    {special, Suffix};
rev_type([Char | _] = Amendment, Rev) when Char >= $0 andalso Char =< $9 ->
    validate_revAmendment(Amendment, Rev),
    {verification_state, Amendment};
rev_type([], _) ->
    {ordinary, []};
rev_type(Chars, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision contains illegal character(s)",
		     {illegal_characters, Chars},
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_rev
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_rev(Rev) ->
    validate_revString(Rev),
    validate_revLength(Rev).

%%% ###=====================================================================###
validate_rev(explicit, {_, ordinary, [_ | _], _, ""}, _) ->
    ok;
validate_rev(explicit, {_, special, [_ | _], [_ | _], [_ | _]}, _) ->
    ok;
validate_rev(explicit, {p_state, _, _, _, _}, _) ->
    ok;
validate_rev(range, {_, ordinary, [_ | _], "", ""}, _) ->
    ok;
validate_rev(range, {_, special, [_ | _], [_ | _], [_ | _]}, _) ->
    ok;
validate_rev(_, _, RevRange) ->
    sysInitI:warning_report(?RepInfo(["Revision range not supported",
				      "Result might be other than expected",
				      {rev_range, RevRange}])).

%%% ###########################################################################
%%% validate_revAmendment
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revAmendment(String, Rev) ->
    case rev_digits(String) of
	{[_, _], []} ->
	    ok;
	{[Char1, _, _] = String, []} when Char1 /= $0 ->
	    ok;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
			    ["Verification State Amendment consists of",
			     "2 digits (01-99) or 3 digits (100-999)",
			     {faulty_revision_part, String},
			     {revision, Rev}]))
    end.

%%% ###########################################################################
%%% validate_revLength
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revLength(Rev) ->
    validate_revLength(Rev, string:len(Rev)).

validate_revLength(_, Len) when Len >= 2 andalso Len =< 7 ->
    ok;
validate_revLength(Rev, _) ->
    throw(?UC_ERR_3(?UC_ERR_RevLengthOutOfRange,
		    [{revision, Rev},
		     {"Revision length limits", "2 - 7 characters"}])).

%%% ###########################################################################
%%% validate_revLetter
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revLetter(Char, _) when Char /= $I andalso
				 Char /= $O andalso
				 Char /= $P andalso
				 Char /= $Q andalso
				 Char /= $R andalso
				 Char /= $W ->
    ok;
validate_revLetter(_, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["The letters I, O, P, Q, R and W must not be used",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revLetters
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revLetters([], _) ->
    ok;
validate_revLetters([_], _) ->
    ok;
validate_revLetters([_, _], _) ->
    ok;
validate_revLetters([_, Char2, Char3 | _], _) when Char2 /= $A andalso
						   Char2 /= $E andalso
						   Char2 /= $U andalso
						   Char2 /= $Y andalso
						   Char3 /= $A andalso
						   Char3 /= $E andalso
						   Char3 /= $U andalso
						   Char3 /= $Y ->
    ok;
validate_revLetters([_, _, _ | _], Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Vowels (A, E, U, Y) are forbidden in letter position",
		     "2 and 3 for R-states with 3 and 4 letters",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revLetterLen
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revLetterLen([_ | _] = String, _, _) when length(String) =< 4 ->
    ok;
validate_revLetterLen("", [], _) ->
    ok;
validate_revLetterLen("", _, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision letter is missing",
		     {revision, Rev}]));
validate_revLetterLen(_, _, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision letters exceeds 4 characters",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revNumberLen
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revNumberLen([_ | _] = String, _) when length(String) =< 4 ->
    ok;
validate_revNumberLen("", Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["2nd position is not a digit",
		     {revision, Rev}]));
validate_revNumberLen(_, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
		    ["Revision number exceeds 4 digits",
		     {revision, Rev}])).

%%% ###########################################################################
%%% validate_revString
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revString(Rev) ->
    validate_revString(Rev, Rev).

validate_revString([Char | Tail], Rev) when is_integer(Char) ->
    validate_revString(Tail, Rev);
validate_revString([], _) ->
    ok;
validate_revString(_, Rev) ->
    throw(?UC_ERR_3(?UC_ERR_RevNotAString, [{revision, Rev}])).

%%% ###########################################################################
%%% validate_revSuffix
%%%
%%% ###=====================================================================###
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This function is copied in these places:
%%% - $RCS_TOP block SYS
%%% - $OS_TOP block NL
%%% - $RDE_TOP/tools/ gupmaker / halmaker
%%% When updating this function, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################
validate_revSuffix(String, Rev) ->
    case rev_digits(String) of
	{[_], []} ->
	    ok;
	{[_, _], []} ->
	    ok;
	{[], String} ->
	    ok;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
			    ["Special revision suffix may consist of",
			     "maximum 2 digits or one letter",
			     {faulty_revision_part, String},
			     {revision, Rev}]))
    end,
    case rev_letters(String, Rev) of
	{[_], []} ->
	    ok;
	{[], String} ->
	    ok;
	_ ->
	    throw(?UC_ERR_3(?UC_ERR_InvalidRevFormat,
			    ["Special revision suffix may consist of",
			     "maximum 2 digits or one letter",
			     {faulty_revision_part, String},
			     {revision, Rev}]))
    end.

%%% ###---------------------------------------------------------------------###
%%% # 3.3.2 Help functions for timediff_XXX/n
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report({finished, {_, _, StartTime}, Id} = Key) ->
    timediff_report_summary(Key),
    Before_TDiffKey = {Id, StartTime, StartTime},
    timediff_report_tdiff(ets:next(?Tbl_timediff, Before_TDiffKey), Key),
    timediff_report(ets:next(?Tbl_timediff, Key));
timediff_report(_) ->
    timediff_report_summaries().

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_summary({_, {PrevModule, Tag, StartTime}, Id} = Key) ->
    [#?Tbl_timediff{data = StopTime}] = ets:lookup(?Tbl_timediff, Key),
    TDiff = StopTime-StartTime,
    case ets:lookup(?Tbl_timediff, summary) of
	[#?Tbl_timediff{data = Data} = Sum] ->
	    ets:insert(?Tbl_timediff,
		       Sum#?Tbl_timediff{data = [{PrevModule,
						  Tag,
						  Id,
						  TDiff} | Data]});
	[] ->
	    ets:insert(?Tbl_timediff,
		       #?Tbl_timediff{key = summary,
				      data = [{PrevModule,
					       Tag,
					       Id,
					       TDiff}]})
    end.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_summaries() ->
    try ets:lookup(?Tbl_timediff, summary) of
	[#?Tbl_timediff{data = Data} = Sum] ->
	    timediff_report_summaries(Data),
	    Sum;
	[] ->
	    ok
    catch
	_ : _ ->
	    ok
    end.

timediff_report_summaries([{PrevModule, Tag, _Id, TDiff} | Tail]) ->
    timediff_report_summaries(Tail, PrevModule, Tag, 1, TDiff);
timediff_report_summaries([]) ->
    sysInitI:info_report([{?MODULE, timediff_report_summaries},
			      no_summary_found]).

timediff_report_summaries([{PrevModule, Tag, _Id, TDiff} | Tail],
			  PrevModule, Tag, Cnt, TDiffSum) ->
    timediff_report_summaries(Tail, PrevModule, Tag, Cnt + 1, TDiffSum + TDiff);
timediff_report_summaries([{PrevModule_Next, Tag_Next, _Id, TDiff_Next} | Tail],
			  PrevModule, Tag, Cnt, TDiffSum) ->
    io:format("~n=BEGIN MEASUREMENT SUMMARY====~n"
	      "  =MEASUREMENT ID====~n"
	      "    module: ~p~n"
	      "    mainTag: ~p~n"
	      "  ===================~n"
	      "    cnt: ~p~n"
	      "    total: ~p~n"
	      "    average: ~p~n"
	      "=END MEASUREMENT SUMMARY====~n",
	      [PrevModule,
	       Tag,
	       Cnt,
	       time_to_string(TDiffSum),
	       time_to_string(TDiffSum div Cnt)]),
    timediff_report_summaries(Tail, PrevModule_Next, Tag_Next, 1, TDiff_Next);
timediff_report_summaries([], PrevModule, Tag, Cnt, TDiffSum) ->
    io:format("~n=BEGIN MEASUREMENT SUMMARY====~n"
	      "  =MEASUREMENT ID====~n"
	      "    module: ~p~n"
	      "    mainTag: ~p~n"
	      "  ===================~n"
	      "    cnt: ~p~n"
	      "    total: ~p~n"
	      "    average: ~p~n"
	      "=END MEASUREMENT SUMMARY====~n",
	      [PrevModule,
	       Tag,
	       Cnt,
	       time_to_string(TDiffSum),
	       time_to_string(TDiffSum div Cnt)]),
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff(Key, {_, {PrevModule, MainTag, StartTime}, Id}) ->
    [#?Tbl_timediff{report_opts = ReportOpts}] =
	mnesia:dirty_read(?Tbl_timediff, enabled),
    io:format("~n=BEGIN MEASUREMENT REPORT====~n"
	      "  =MEASUREMENT ID====~n"
	      "    module: ~p~n"
	      "    mainTag: ~p~n"
	      "  ===================~n",
	      [PrevModule, MainTag]),
    timediff_report_tdiff_details_all(Key, Id, StartTime, ReportOpts),
    timediff_report_tdiff_details_top(Key, Id, StartTime, ReportOpts),
    timediff_report_tdiff_sumInfo(Key, Id, StartTime, ReportOpts),
    io:format("=END MEASUREMENT REPORT====~n").

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_all(Key, Id, StartTime, ReportOpts) ->
    StartKey =
	case lists:member({details, all}, ReportOpts) of
	    true ->
		Key;
	    false ->
		Key_AfterLast = {Id, StartTime, "after last"},
		ets:prev(?Tbl_timediff, Key_AfterLast)
	end,
    timediff_report_tdiff_details_all_loop(StartKey, Id, StartTime).

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_all_loop({Id, StartTime, _} = Key,
				       Id,
				       StartTime) ->
    [#?Tbl_timediff{data = Data}] = ets:lookup(?Tbl_timediff, Key),
    timediff_report_tdiff_details_io(Data, StartTime),
    timediff_report_tdiff_details_all_loop(ets:next(?Tbl_timediff, Key),
					   Id,
					   StartTime);
timediff_report_tdiff_details_all_loop(_, _, _) ->
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_io({Tag, TsBefore, TsAfter}, StartTime) ->
    TimeInfo =
	{list_to_atom("Started " ++
		       time_to_string(TsBefore-StartTime) ++
		      " from measStart"),
	 list_to_atom("Took: " ++
			  time_to_string(TsAfter-TsBefore))},
    io:format("    ~p:~n      ~p~n", [Tag, TimeInfo]).

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_top(Key, Id, StartTime, ReportOpts) ->
    case [TopCnt || {details, {top, TopCnt}} <- ReportOpts] of
	[TopCnt | _] ->
	    io:format("  =BEGIN {DETAILS, {TOP, ~p}}====~n", [TopCnt]),
	    ets:new(?Tbl_timediff_top,
		    [named_table, public, ordered_set, {keypos, 2}]),
	    timediff_report_tdiff_details_top_loop(Key, Id, StartTime, TopCnt),
	    timediff_report_tdiff_details_top_io(ets:first(?Tbl_timediff_top),
						 StartTime),
	    ets:delete(?Tbl_timediff_top),
	    io:format("  =END {DETAILS, {TOP, ~p}}====~n", [TopCnt]);
	[] ->
	    ok
    end.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_top_io(Key, StartTime)
  when Key /= '$end_of_table' ->
    [#?Tbl_timediff{data = Data}] = ets:lookup(?Tbl_timediff_top, Key),
    timediff_report_tdiff_details_io(Data, StartTime),
    timediff_report_tdiff_details_top_io(ets:next(?Tbl_timediff_top, Key),
					 StartTime);
timediff_report_tdiff_details_top_io('$end_of_table', _) ->
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_top_loop({Id, StartTime, _} = Key,
				       Id,
				       StartTime,
				       TopCnt) ->
    case ets:lookup(?Tbl_timediff, Key) of
	[#?Tbl_timediff{data = {Tag, TsBefore, TsAfter} = Data}]
	when is_atom(Tag) ->
	    TopTag = Tag;
	[#?Tbl_timediff{data = {Tag, TsBefore, TsAfter} = Data}]
	when is_tuple(Tag) ->
	    TopTag = element(1, Tag)
    end,
    Tdiff = TsAfter-TsBefore,
    ets:insert(?Tbl_timediff_top, #?Tbl_timediff{key = {TopTag, Tdiff},
						 data = Data}),
    timediff_report_tdiff_details_topCnt(TopTag, TopCnt),
    timediff_report_tdiff_details_top_loop(ets:next(?Tbl_timediff, Key),
					   Id,
					   StartTime,
					   TopCnt);
timediff_report_tdiff_details_top_loop(_, _, _, _) ->
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_topCnt(TopTag, TopCnt) ->
    BeforeHi = {TopTag, -1},
    AfterLo = {TopTag, after_last},
    StartKey = timediff_report_tdiff_details_topCnt_stepDown(TopCnt, BeforeHi),
    EndKey = timediff_report_tdiff_details_topCnt_stepUp(TopCnt, AfterLo),
    timediff_report_tdiff_details_topCnt_remove(StartKey, EndKey).

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_topCnt_stepDown(TopCnt, Key)
  when TopCnt >= 0 andalso Key /= '$end_of_table' ->
    timediff_report_tdiff_details_topCnt_stepDown(TopCnt - 1,
						  ets:next(?Tbl_timediff_top,
							   Key));
timediff_report_tdiff_details_topCnt_stepDown(_, Key) ->
    Key.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_topCnt_stepUp(TopCnt, Key)
  when TopCnt >= 0 andalso Key /= '$end_of_table' ->
    timediff_report_tdiff_details_topCnt_stepUp(TopCnt - 1,
						ets:prev(?Tbl_timediff_top,
							 Key));
timediff_report_tdiff_details_topCnt_stepUp(_, Key) ->
    Key.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_details_topCnt_remove(Key, EndKey)
  when Key =< EndKey andalso
       Key /= '$end_of_table' ->
    ets:delete(?Tbl_timediff_top, Key),
    timediff_report_tdiff_details_topCnt_remove(ets:next(?Tbl_timediff_top,
							 Key),
						EndKey);
timediff_report_tdiff_details_topCnt_remove(_, _) ->
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_sumInfo(Key, Id, StartTime, ReportOpts) ->
    catch ets:new(?Tbl_timediff_sumInfo,
		  [named_table, public, ordered_set, {keypos, 1}]),
    case lists:keyfind(summary, 1, ReportOpts) of
	{summary, {tagLevel, TagLevel}} ->
	    TagLevel;
	{summary, all} ->
	    TagLevel = infinity;
	_ ->
	    TagLevel = 1
    end,
    timediff_report_tdiff_sumInfo_loop(Key, Id, StartTime, TagLevel),
    timediff_report_tdiff_sumInfo_io(ets:first(?Tbl_timediff_sumInfo)),
    ets:delete(?Tbl_timediff_sumInfo).

%%% ###=====================================================================###
timediff_report_tdiff_sumInfo_loop({Id, StartTime, _} = Key,
				   Id,
				   StartTime,
				   MaxTagLevel) ->
    [#?Tbl_timediff{data = {Tag, TsBefore, TsAfter}}] =
	ets:lookup(?Tbl_timediff, Key),
    Tdiff = TsAfter-TsBefore,
    timediff_report_tdiff_sumInfo_w(Tag, 1, MaxTagLevel, Id, StartTime, Tdiff),
    timediff_report_tdiff_sumInfo_loop(ets:next(?Tbl_timediff, Key),
				       Id,
				       StartTime,
				       MaxTagLevel);
timediff_report_tdiff_sumInfo_loop(_, _, _, _) ->
    ok.

%%% ###=====================================================================###
timediff_report_tdiff_sumInfo_w(Tag,
				_,
				MaxTagLevel,
				Id,
				StartTime,
				Tdiff) when is_atom(Tag) andalso
					    MaxTagLevel >= 0 ->
    ets_update_cnt(?Tbl_timediff_sumInfo,
		   {Id, StartTime, Tag},
		   {?Tbl_timediff_sumInfo_pos_cnt, 1}),
    ets_update_cnt(?Tbl_timediff_sumInfo,
		   {Id, StartTime, Tag},
		   {?Tbl_timediff_sumInfo_pos_tdiff, Tdiff});
timediff_report_tdiff_sumInfo_w(Tag,
				TagLevel,
				MaxTagLevel,
				Id,
				StartTime,
				Tdiff) when is_tuple(Tag) andalso
					    TagLevel =< MaxTagLevel ->
    try
	begin
	    SubTag_List =
		[element(ElemPos, Tag) || ElemPos <- lists:seq(1, TagLevel)],
	    SubTag_Tuple =
		case size(Tag) of
		    TagSize when TagSize > TagLevel ->
			list_to_tuple(SubTag_List ++ ['...']);
		    _ ->
			list_to_tuple(SubTag_List)
		end,
	    ets_update_cnt(?Tbl_timediff_sumInfo,
			   {Id, StartTime, SubTag_Tuple},
			   {?Tbl_timediff_sumInfo_pos_cnt, 1}),
	    ets_update_cnt(?Tbl_timediff_sumInfo,
			   {Id, StartTime, SubTag_Tuple},
			   {?Tbl_timediff_sumInfo_pos_tdiff, Tdiff}),
	    timediff_report_tdiff_sumInfo_w(Tag,
					    TagLevel + 1,
					    MaxTagLevel,
					    Id,
					    StartTime,
					    Tdiff)
	end
    catch
	_ : _ ->
	    ok
    end;
timediff_report_tdiff_sumInfo_w(_, _, _, _, _, _) ->
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_report_tdiff_sumInfo_io({_, _, Tag} = Key) ->
    [{_, Cnt, TDiffSum}] = ets:lookup(?Tbl_timediff_sumInfo, Key),
    io:format("  =TAG SUMMARY====~n"
	      "    ~p: ~p~n"
	      "    ~p: ~p~n"
	      "    ~p: ~p~n"
	      "    ~p: ~p~n",
	      [tag, Tag,
	       cnt, Cnt,
	       total, time_to_string(TDiffSum),
	       average, time_to_string(TDiffSum div Cnt)]),
    timediff_report_tdiff_sumInfo_io(ets:next(?Tbl_timediff_sumInfo, Key));
timediff_report_tdiff_sumInfo_io(_) ->
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
ets_update_cnt(TblName, Key, UpdateOp) ->
    try
	ets:update_counter(TblName, Key, UpdateOp)
    catch
	_ : _ ->
	    ets:insert(TblName, {Key, 0, 0}),
	    ets_update_cnt(TblName, Key, UpdateOp)
    end.

%%% ###########################################################################
%%% timediff_stop_all
%%%
%%% ###=====================================================================###
timediff_stop_all({ongoing, Id}) ->
    Before = timediff_before('======= Stopped by timediff_disable =======', Id),
    timediff_after(Before),
    timediff_stop(Id),
    timediff_stop_all(ets:next(?Tbl_timediff, {ongoing, Id}));
timediff_stop_all(_) ->
    ok.

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
timediff_timer_cancel() ->
    try mnesia:dirty_read(?Tbl_timediff, enabled) of
	[#?Tbl_timediff{data = {_Since, TimerRef}}] ->
	    timer:cancel(TimerRef);
	_ ->
	    ok
    catch
	_ : _ ->
	    ok
    end.

%%% ###---------------------------------------------------------------------###
%%% # 3.3.3 Miscellaneous
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% elem_add
%%%
%%% ###=====================================================================###
elem_add(reverse, Elem, List) ->
    [Elem | List];
elem_add(upright, Elem, List) ->
    List ++ [Elem].

%%% ###########################################################################
%%% lists_delete
%%%
%%% ###=====================================================================###
lists_delete([Key | Tail], List) ->
    List1 = lists:keydelete(Key, 1, List),
    lists_delete(Tail, lists:delete(Key, List1));
lists_delete([], List) ->
    List.

%%% ###########################################################################
%%% list_to_string
%%%
%%% ###=====================================================================###
list_to_string(List) ->
    Content = lists:flatten(list_to_strings(List)),
    string:strip(Content, right, $,).

%%% ###########################################################################
%%% list_to_strings
%%%
%%% ###=====================================================================###
list_to_strings([Elem | Tail]) ->
    [term_to_string(Elem) ++ "," | list_to_strings(Tail)];
list_to_strings([]) ->
    [].

%%% ###########################################################################
%%% record_info_default
%%%
%%% ###=====================================================================###
record_info_default(Len) ->
    record_info_default(Len - 1, 1).

record_info_default(Len, Pos) when Len > 0 ->
    [list_to_atom("e" ++ integer_to_list(Pos)) |
     record_info_default(Len - 1, Pos + 1)];
record_info_default(0, _) ->
    [].

%%% ###########################################################################
%%% str_token
%%%
%%% ###=====================================================================###
str_token(String, Separators) ->
    str_token(String, Separators, []).

%%% ###=====================================================================###
str_token([E | Tail], Separators, AccToken) ->
    case lists:member(E, Separators) of
	false ->
	    str_token(Tail, Separators, AccToken ++ [E]);
	true ->
	    case Tail of
		[] ->
		    LastToken = [],
		    {AccToken, LastToken, Tail};
		_ ->
		    {AccToken, Tail}
	    end
    end;
str_token([], _, AccToken) ->
    {AccToken, []}.

%%% ###########################################################################
%%% terms_to_atom
%%%
%%% ###=====================================================================###
terms_to_atom(Term1, Term2, Term3) ->
    list_to_atom(term_to_string(Term1) ++
		 term_to_string(Term2) ++
		 term_to_string(Term3)).

%%% ###########################################################################
%%%
%%%
%%% ###=====================================================================###
validate_ReportOpts(all) ->
    [{details, all}, {summary, all}];
validate_ReportOpts([{details, {top, TopCnt}} = RO | Tail])
  when is_integer(TopCnt) ->
    [RO | validate_ReportOpts(Tail)];
validate_ReportOpts([{details, all} = RO | Tail]) ->
    [RO | validate_ReportOpts(Tail)];
validate_ReportOpts([{summary, {tagLevel, TL}} = RO | Tail])
  when is_integer(TL) ->
    [RO | validate_ReportOpts(Tail)];
validate_ReportOpts([{summary, all} = RO | Tail]) ->
    [RO | validate_ReportOpts(Tail)];
validate_ReportOpts([RO | Tail]) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 unknown_report_option_in_timediff_enable,
				 {report_opt, RO}]),
    validate_ReportOpts(Tail);
validate_ReportOpts([]) ->
    [];
validate_ReportOpts(ReportOpts) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				 unknown_report_option_in_timediff_enable,
				 {report_opts, ReportOpts}]),
    ?Timediff_default_reportOpts.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
