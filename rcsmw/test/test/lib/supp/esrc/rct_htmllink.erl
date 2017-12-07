%% #0.    BASIC INFORMATION
%% -----------------------------------------------------------------------------
%% %CCaseFile:	rct_htmllink.erl %
%% @author etxkols
%% @copyright Ericsson AB 2012-2014
%% @version /main/R1A/R2A/2
%% @doc Prints a HTML link to current common test run (useful in Jenkins).
%% 
%% This module is intended to be used from a Common Test suite, where the module is called as a ct_hook at the beginning of each test suite.<br/>
%% This hook prints a HTML link to current common test run. This is useful in Jenkins to directly access the result of a test<br/>
%% The ct_hook must look as below. and is run in post_end_per_suite<br/>
%% ```suite() -> 
%%        [{ct_hooks, [{rct_htmllink, []}]}].'''
%% @end
-module(rct_htmllink).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/2').
-date('2014-04-24').
-author('etxkols').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2014 All rights reserved.
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
%%% R1A/1      2012-06-27 etxkols     Created
%%% R1A/1      2012-06-28 etxkols     \n added
%%% R2A/1      2014-03-26  etxkols    Faulty init/2 return value
%%% R2A/2      2014-04-24  etxkols    Add printout of testpath in pre_init_per_suite
%%% ----------------------------------------------------------
-export([init/2]).
-export([pre_init_per_suite/3,
	 post_end_per_suite/4]).

%% @hidden
init(_Id, Opts) ->
    {ok,Opts}.

%% @hidden
pre_init_per_suite(_Suite,Config,States) ->
    {ok,Dir} = file:get_cwd(),
    io:format(user,"Test run log: https://rbs-rde.rnd.ki.sw.ericsson.se/" ++ 
                  filename:join(Dir,"index.html\n"), []),
    {Config, States}.

%% @hidden
post_end_per_suite(_Suite,_Config,Return,States) ->
    {ok,Dir} = file:get_cwd(),
    io:format(user,"Test run log: https://rbs-rde.rnd.ki.sw.ericsson.se/" ++ 
                  filename:join(Dir,"index.html\n"), []),
    {Return, States}.


