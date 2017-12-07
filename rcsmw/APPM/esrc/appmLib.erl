%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmLib.erl %
%%% Author:	etxarnu
%%% Description:Library functions 
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(appmLib).
-author(etxarnu).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R4A/R12A/1').
-date('2017-12-04').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R4A/1     20150520   etxpejn      Removed SystemLog and log_both & syslog
%%% R12A/1    20171204   etxarnu      Made log/r3 crash safe
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([log/3]).

-include("appm.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc Log string in erlang log
%%% ===Arguments===
%%% Level - Logging level according to LOG IWD
%%% String - Format string, same syntax as io:format
%%% Args - Arguments to format string

-spec log(Level :: atom(), Str :: string(), Args :: list(term())) -> ok |{error,term()}.

log(Level, Str,Args) ->
    try
	NewStr = lists:flatten(
		   io_lib:format(Str,Args)),
	case Level of
	    info ->
		sysInitI:info_msg(NewStr);
	    notice ->
		sysInitI:info_msg("notice:"  ++ NewStr);
	    warning ->
		sysInitI:warning_msg(NewStr);
	    error ->
		sysInitI:error_msg(NewStr);
	    Other ->
		sysInitI:error_msg(atom_to_list(Other) ++ ":" ++  NewStr)
	end,
	ok
    catch
	Error:Reason ->
	    sysInitI:error_msg("appmLib:log(~p,~p,~p) failed, reason:~p~n",
		[Level, Str,Args,{Error,Reason}]),
	    {error, Reason} 
    end.
