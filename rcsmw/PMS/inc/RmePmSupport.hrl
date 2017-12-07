%%% --------------------------------------------------------
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% --------------------------------------------------------

-hrl_id({"RmePmSupport","1.0.0","/main/R5A/3"}).


%% -------------- CLASS PmSupport -------------------------

%% Description:
%% Holds control for multi ROP file behaviour.

-record(pmSupport, {pmSupportId,
                    ropFileHandling}).

-define(pmSupport_types,
        [{pmSupportId, string},
         {ropFileHandling, 'RmePmSupport.FileHandlingMethod'}]).

-define(pmSupport_ropFileHandling_default, 'SINGLE_ROP_FILE').
-define(PmSupport_restricted, [pmSupportId]).


%% ------------------ ENUM FileHandlingMethod ----------------------
-ifndef('FileHandlingMethod').
-define('FileHandlingMethod', 1).

-define(FileHandlingMethod_SINGLE_ROP_FILE, 1).
-define(FileHandlingMethod_MULTIPLE_ROP_FILES, 2).

-endif. % FileHandlingMethod
