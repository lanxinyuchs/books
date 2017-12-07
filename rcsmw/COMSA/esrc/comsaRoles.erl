%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaRoles.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R9A/1
%%%
%%% @doc == Callback interface for roles ==
%%% This module holds the collback interface for role enquiries from COM

-module(comsaRoles).
-vsn('/main/R1A/R2A/R3A/R4A/R9A/1').
-date('2017-04-07').
-author('etxarnu').
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
%%% R1A/1      2012-01-10 etxbjca     Created
%%% R2A/13     2013-03-18 etxjotj     Removed special handling of sim env
%%%                                   That will be done in SYS instead
%%% R2A/16     2014-01-29 etxlg       sysSshCli -> omc_server
%%% R2A/17     2014-04-25 etxjotj     Possiblity to test different roles in SIM
%%% R3A/1      2014-09-04 etxarnu     Added start/1 for comte-1.0   
%%% R9A/1      2017-04-06 etxarnu     Handle vrcs as target in getRoles
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1]).
-export([getRoles/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%-export([internal_function1/2]).
%%%-export([internal_function2/3]).

%-include("template.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start(_) ->
    ok.

%%% ----------------------------------------------------------
%%% -type getRoles(User) ->                                 %#
%%%    [Bin]                                                %#
%%% Input: User - [bin()]
%%% Output: Roles - [bin()]
%%% Exceptions: 
%%% Description: Fetch user role data from the ldap cache
%%% ----------------------------------------------------------


getRoles(User) ->
    case sysEnv:rcs_mode_2() of
	simulated ->
	    case comsaLib:get_variable(test_roles) of
		undefined ->
		    [<<"EricssonSupport">>];
		Roles when is_list(Roles)-> 
		    sysInitI:info_msg(
		      "~w:User ~p has roles ~p~n",[?MODULE,User,Roles]),
		    Roles
	    end;
	_ ->
	    UserStr = binary_to_list(User),
	    Roles = omc_server:authorize_user(UserStr),
	    [list_to_binary(Role)||Role<-Roles]
    end.

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%some_method(Parameter)->
%   nn.

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

