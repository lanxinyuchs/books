%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cchDataInit.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/1
%%%
%%% @doc == Initialization of CCH ==
%%% This module initializes CCH through the various callbacks to SYS etc

-module(cchDataInit).
-vsn('/main/R5A/1').
-date('2016-02-17').
-author('etomist').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R5A/1      2016-01-27 etomist     Created
%%% ----------------------------------------------------------
-export([children/0, activate/0]).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
-spec activate() -> 'ok'.
activate() ->
    ok.

-spec children() -> {'ok', [supervisor:child_spec()]}.
children() ->
    {ok, [{cch_service, {cch_service, start_link, []},
		   permanent, 1000, worker, [cch_service]}]}.
