%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile: vnfcDataInit.erl %
%%% @author etxaldu
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/1
%%%
%%% @doc == Initialization of VNFC ==
%%% This module initializes VNFC through the various callbacks to SYS etc

-module(vnfcDataInit).
-date('2016-09-13').
-author('etxaldu').
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
%%%            2016-09-13 etxaldu     Created
%%%            2016-10-13 etxaldu     Mnesia Table
%%%            2017-10-20 emariad     Added vnfTestLicense table
%%%            2017-11-30 emariad     Backed out vnfTestLicense table
%%%            2017-12-01 etxaldu     Added vnfcData table
%%% ----------------------------------------------------------
-include("vnfcs.hrl").
-export([instPhParallel_init/1]).
-export([init/1]).
-export([children/0, activate/0]).



%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(Db_nodes) ->
    {atomic, ok} =
        clhI:mnesia_create_table(vnfcs,
                                 [{attributes, record_info(fields, vnfcs)},
                                  {disc_copies, Db_nodes}]),
    {atomic, ok} =
        clhI:mnesia_create_table(vnfc,
                                 [{attributes, record_info(fields, vnfc)},
                                  {ram_copies, Db_nodes}]),
    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(Db_nodes) ->
    instPhParallel_init(Db_nodes).


%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
-spec activate() -> 'ok'.
activate() ->
    ok.

-spec children() -> {'ok', [supervisor:child_spec()]}.
children() ->
    {ok, [{vnfcs, {vnfcs, start_link, []},
                permanent, 1000, worker, [vnfcs]},
          {vnfcc, {vnfcc, start_link, []},
              permanent, 1000, worker, [vnfcc]},
         {vnfcHttps, {vnfcHttps, start, []},
             permanent, 1000, worker, [vnfcHttps]}
     ]}.
