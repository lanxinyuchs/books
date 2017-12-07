%%% ----------------------------------------------------------
%%% %CCaseFile:	sftpd.erl %
%%% Author:	erarafo
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(sftpd).
%behaviour(_).
-id('Updated by CCase').
-vsn('/main/R5A/1').
-date('2016-01-18').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R5A/1      2016-01-18 erarafo     First version
%%% ----------------------------------------------------------

-export(
   [sftpd/1, 
    info_msg/2, 
    warning_msg/2, 
    error_msg/2]).


%%% ----------------------------------------------------------
%%% @doc Launch an SFTP daemon.
%%% @end
%%% ----------------------------------------------------------
sftpd([Port, User, Password, PublishedDir, SystemDir, Verbose]=Args) ->
    VerboseB = if Verbose =:= "true" -> true; true -> false end,
    if
	VerboseB ->
	    info_msg("arguments are: ~p", [Args]);
	true ->
	    ok
    end,
    
    PortN = list_to_integer(Port),
    ssh:start(permanent),
    SubSysSpec = 
	ssh_sftpd:subsystem_spec(
	  [{root, PublishedDir},
	   {cwd, "/"},
	   {file_handler, ssh_sftpd_wrapper},
	   {max_files, 0}]),
    
    try
	ssh:daemon(
	  PortN,
	  [{system_dir, SystemDir},
	   {subsystems, [SubSysSpec]},
	   {auth_methods, "password"},
	   {user_passwords, [{User, Password}]}]) of
	
	{error, {{shutdown,
		  {failed_to_start_child,
		   {ssh_acceptor_sup, any, _, default},
		   {shutdown,
		    {failed_to_start_child,
		     {ssh_acceptor_sup, any, _, default},
		     eaddrinuse}}}},
		 _}} ->
	    error_msg("probably port in use: ~p", [PortN]),
	    init:stop();
	
	{error,{{shutdown,
		 {failed_to_start_child,
		  {ssh_acceptor_sup, _, PortN},
		  {shutdown,
		   {failed_to_start_child,
		    {ssh_acceptor_sup, _, PortN},
		    eaddrinuse}}}},
		_}} ->
	    	    
	    error_msg("probably port in use: ~p", [PortN]),
	    init:stop();

	{ok, Sshd} ->
	    info_msg("SFTP daemon started: ~p", [Sshd]),
	    idle()
	
    catch 
	ExType:ExData ->
	    error_msg("exception caught: ~p", [{ExType, ExData}]),
	    init:stop()
    end.


idle() ->
    timer:sleep(60000),
    idle().


error_msg(Format, Data) ->
  io:format(standard_error, "ERROR: "++Format++"~n", Data).   

warning_msg(Format, Data) ->
  io:format(standard_error, "WARNING: "++Format++"~n", Data). 

info_msg(Format, Data) ->
  io:format(standard_error, "INFO: "++Format++"~n", Data).  
