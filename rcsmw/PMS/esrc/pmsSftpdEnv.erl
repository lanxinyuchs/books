%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsSftpdEnv.erl %
%%% @private
%%% Author:	 eolaand
%%% Description: Functions for retrieving Spec and paths for PMS sftpd.
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pmsSftpdEnv).
-vsn('/main/R2A/R3A/R5A/1').
-date('2016-02-11').
-author('eolaand').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R1A/R2A/1  2013-01-31 eolaand     First version
%%% R3A/4      2015-01-20 erarafo     Removed subsystem_spec/0
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API
-export([file_handler/0,
	 rop_dir/0,
	 rop_dir_path/0]).


-include("pms.hrl").

-define(FILE_HANDLER, pmsSshSftpdFile).

%%%===================================================================
%%% API
%%%===================================================================
%% Get PMS sftpd file handler callback module
file_handler() ->
    ?FILE_HANDLER.  


%% Get ROP file location.
rop_dir() ->
    case pmsDb:meas_capabilities_get(fileLocation) of
	{ok, RopDir}->
	    RopDir;
	_Nada ->
	    ?DEFAULT_ROP_DIR
    end.


%% Get full path to rop sftp directory.
rop_dir_path() ->
    sysFi:absname(rop_dir()).


%%%===================================================================
%%% Internal functions
%%%===================================================================
