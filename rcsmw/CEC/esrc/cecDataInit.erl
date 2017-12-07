%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cecDataInit.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R2A/R5A/1
%%%
%%% @doc == Initialization of CEC ==
%%% This module initializes CEC through the various callbacks to SYS etc

-module(cecDataInit).
-vsn('/main/R2A/R5A/1').
-date('2016-01-07').
-author('etxberb').
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
%%% R1A/1      2012-10-19 etxpeno     Created
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% ----------------------------------------------------------
-export([instPhParallel_init/1]).
-export([children/0, activate/0]).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
-spec instPhParallel_init(DbNodes::[node()]) -> 'ok'.
instPhParallel_init(DbNodes) ->
    ok = cec_db:init(DbNodes).

-spec activate() -> 'ok'.
activate() ->
    ok.

-spec children() -> {'ok', [supervisor:child_spec()]}.
children() ->
    {ok, [{cec_service, {cec_service, start_link, []},
           permanent,
           1000,
           worker,
           [cec_service]}]}.
