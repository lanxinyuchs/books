%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaI.erl %
%%% Author:	etxpejn
%%% @author etxpejn
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R5A/R6A/R7A/R11A/1
%%%
%%% @doc ==License management interface==
%%% This module contains the erlang internal interface for license 
%%% management.

-module(lmaI).
-vsn('/main/R5A/R6A/R7A/R11A/1').
-date('2017-09-18').
-author('etxpejn').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ------------------------
%%% R5A/1      2015-12-03 etxpejn     Created
%%% R5A/3      2016-01-12 etxpejn     Added clean_disk
%%% R5A/4      2016-02-05 etxpejn     Added encrypt_esi_log
%%% R6A/1      2016-05-10 etxpejn     Do not encrypt ESI for 5G
%%% R6A/2      2016-06-10 etxpejn     Do not encrypt ESI for sim
%%% R6A/3      2016-09-14 etxpejn     Workaround for vRCS to always load the test MOM
%%% R7A/1      2016-09-28 etxpejn     vRCS to handle encrypt_esi_log
%%% R11A/1     2017-09-18 etxpejn     Added dual support for encrypt_esi_log
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([load_test_mom/0,
	 clean_disk/1,
	 encrypt_esi_log/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%%-----------------------------------------------------------
%%% @doc
%%% This function is used by GMF to see if the test MOM should be loaded
%%% or not.
%%% @end
%%% ----------------------------------------------------------

-spec load_test_mom() -> true | false.

load_test_mom() ->
    case sysEnv:vrcs() of
	false ->
	    lmaLkf:load_test_mom();
	true ->
	    %% This is a workaround since there is currently no AI in vRCS 
	    true
    end.
 

%%%-----------------------------------------------------------
%%% @doc Clean disk
%%% Callback for sysServer when there is a need to clean the disk
%%% @end 
%%%-----------------------------------------------------------

-spec clean_disk(Severity::minor|major) -> any().

clean_disk(_Severity) ->
    %% Only the LKF stored under /rcs/lma/ and that one can not be removed.
    ok.

%%%-----------------------------------------------------------
%%% @doc 
%%% This function is used by LOG to see if the ESI log should be encrypted
%%% or not.
%%% @end
%%% ----------------------------------------------------------

-spec encrypt_esi_log() -> true | false.

encrypt_esi_log() ->
    case sysEnv:rcs_mode_2() of
	target ->
	    case sysEnv:role() of
		active ->
		    lmaGlms:encrypt_esi_log();
		regular ->
		    ErlangNode = get_core_node(),
		    rpc:call(ErlangNode, lmaGlms, encrypt_esi_log,[])
	    end;
	simulated ->
	    %% Do not encrypt ESI for sim regardless of LKF
	    false;
	vrcs ->
	    %% Currently do not encrypt ESI for vRCS since there is no AI
	    false
	    %% lmaGlms:encrypt_esi_log()	
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
get_core_node() ->
    case  clhI:erlang_nodes(active) of
    	[] ->
    	    clhI:erlang_node();
    	[Node]->
    	   Node
    end.
%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

