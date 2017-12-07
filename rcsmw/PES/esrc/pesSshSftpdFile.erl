%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pesSshSftpdFile.erl %
%%% @hidden
%%% Author:	erarafo
%%% 
%%% Description: This module contains functions that intercept
%%% SFTP callbacks according to the (OTP) ssh_sftpd_file_api
%%% behaviour.
%%%
%%% Only a subset of the ssh_sftpd_file_api callbacks need to be
%%% declared here. The purpose of declaring a callback is to allow
%%% some special handling of that particular callback.
%%% 
%%% A function declared here may decide to not do any special
%%% handling, based e g on an inspection of a path argument or
%%% such. In that case the function should return 'ignore', giving
%%% other interceptor modules opportunity to handle the callback.
%%% Otherwise the function must return a term of the same format as
%%% the functions in (OTP) ssh_sftpd_file return.
%%% 
%%% This module must be registered with sysFi, using the
%%% sysFi:register_sftp_handler_module/1 function.
%%%
%%% The sysFi process maintains a list of registered interceptor
%%% modules. For each callback all interceptor modules will be tried
%%% in sequence until some module defines a matching callback function
%%% and this function returns a non-'ignore' result. If the list of
%%% modules is exhausted the default handling defined in
%%% sysSshSftpdFile will take place.
%%% 
%%% Interceptor functions should be written so that module order is
%%% not significant. This may not always be possible; for that reason
%%% the modules list is built in a well-defined order where order of
%%% registration does not matter. It is also possible to prescribe
%%% modules order by associating an ordering weight to known module
%%% names in sysFi:moduleOrder/1.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(pesSshSftpdFile).
-vsn('/main/R3A/3').
-date('2015-01-21').
-author('erarafo').
-shaid('9b8478e4c23c2c0ed73a28e8d78b956ed5e47d23').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% R3A/1      2015-01-20 erarafo     First version
%%% R3A/2      2015-01-20 erarafo     Cleanup
%%% R3A/3      2015-01-21 erarafo     Edoc
%%% ----------------------------------------------------------


% This module needs to know the directory where
% PMS stores ROP files. The macro will be set to
% /rcs/sftp/rop (on target) or something like
% /local/scratch/SIGNUM/RCS_ROOT/rcs/sftp/rop
% on the simulator.
-define(PMS_ROP_DIR, pmsSftpdEnv:rop_dir()).


-export([read_link/2
	]).


read_link(Path, State) ->
    case sftpPathType(Path) of
	pms ->
	    ignore;
	other ->
	    ParentPath = filename:dirname(Path),
	    RootDir = filename:join(sysEnv:rcs_dir(), sysSshSftpd:sftp_dir()),
	    if 
		ParentPath =/= RootDir ->
		    % unless we have the special case described below
		    ssh_sftpd_file:read_link(Path, State);
		true ->
		    % special case: we might have a symlink pointing
		    % into the ramdisk file tree; if that is the case
		    % pretend that the link is just a directory
		    LinkTarget = file:read_link(Path),
		    case LinkTarget of
			{error, _Reason}=E ->
			    % not a symlink after all
			    {E, State};
			{ok, _Target} ->
			    % symlink!
			    {{error, einval}, State}
		    end
	    end
    end.


sftpPathType(Path) ->
    PmsRopDir = ?PMS_ROP_DIR,
    Ult = filename:basename(PmsRopDir),
    PenUlt = filename:basename(filename:dirname(PmsRopDir)),
    Pattern = filename:join(PenUlt, Ult),
    % the pattern typically boils down to "sftp/rop"
    case re:run(Path, Pattern, []) of
	{match, _} ->
	    pms;
	nomatch ->
	    other
	end.
