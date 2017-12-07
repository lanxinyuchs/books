%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli.hrl %
%%% Author:	etxbjca
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R3A/R4A/R5A/R8A/R9A/1').
-hrl_date('2017-03-27').
-hrl_author('uabesvi').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/1      2013-07-10   etxbjca   Created
%%% R2A/2      2013-09-05   etxlg     Add INTERNAL_DIR
%%% R2A/3      2014-02-02   etxlg     Stuff to limit input line length
%%% R2A/4      2014-02-24   etxlg     Prompt changed
%%% R3A/1      2015-03-19   etxlg     New proxy-program
%%% R4A/10     2015-07-26   uabesvi   fru_type -> cli_scope, local default
%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------

-define(DEFAULT_MP_ID,          "'$default_fru_id' MpId = 1").
-define(DEFAULT_MP_ID_REVERSED, "1 = dIpM 'di_urf_tluafed$'").

-define(HISTORYLENGTH, 50).
-define(MAXLINELEN, 1000).
-define(ETX, 3). %what you get from ssh when the user does CTRL-C
-define(EOT, 4). %what you get from ssh when the user does CTRL-D
-define(BEL, 7). %ring the terminal bell
-define(BS, 8). %backspace

-define(SPACE,        32).
-define(SINGLE_QUOTE, 39).
-define(DOUBLE_QUOTE, 34).

-define(EOF_PROXY, "port_proxy").
%-define(EOF_PROXY, "std_eof_proxy").
-define(DEFAULT_EOF, "#inband-eof-marker#").

-define(INTERNAL_DIR,      "/misc").
-define(INTERNAL_ALL_CMDS, ["cd", "exit", "help", "ls", "pwd", ".."]).
-define(INTERNAL_LAB_CMDS, ["authlevel"]).
-define(INTERNAL_DIR_CMDS, ["cmds", 
			    "grep", 
			    "tail", 
			    "info", 
			    "prompt", 
			    "reportrc", 
			    "timeout"]).

%% Different FRU TYPES 
-define(FRU_CENTRAL, central).
-define(FRU_LOCAL,   local).
-define(FRU_RU,      ru).
-define(FRU_XMU,     xmu).

-define(COLI_INTERFACE_REGISTRATION, 16#0190001).
-define(COLI_ADD_FRU,                16#0190002).
-define(COLI_DELETE_FRU,             16#0190003).
-define(COLI_COMMAND_REQUEST,        16#0190004).
-define(COLI_COMMAND_REPLY,          16#0190005).


-define(MAX_FRU_LENGTH,      32).
-define(MAX_LDN_LENGTH,     512).
-define(MAX_COMMAN_LENGTH,  256).
-define(MAX_REPLY_LENGTH,  1024).

-define(COLI_RESULT_OK,              0).
-define(COLI_RESULT_NO_FRU_EXISTS,   1).
-define(COLI_RESULT_OTHER_ERROR,    99).
-define(COLI_RESULT_OK_CONTINUE,   100).





-define(ECOLI_LOG, "RcsEcoli").

-define(SEV_ERROR,   error).
-define(SEV_WARNING, warning).
-define(SEV_1, 1).
-define(SEV_2, 2).
-define(SEV_3, 3).
-define(SEV_4, 4).
-define(SEV_5, 5).
-define(SEV_6, 6).
-define(SEV_7, 7).
-define(SEV_8, 8).
-define(SEV_9, 9).


-define(LOG_RAM(__MultiMsg),
	begin
	    logRamI:write_log(?ECOLI_LOG,
			      {?MODULE, ?LINE, self()}, 
			      __MultiMsg)
	end).

-define(LOG_RAM(__Sev, __Msg),
	begin
	    logRamI:write_log(?ECOLI_LOG,
			      {?MODULE, ?LINE, self()}, 
			      {__Sev, __Msg})
	end).

-define(LOG_RAM(__Sev, __Msg, __BL),
	begin
	    logRamI:write_log(?ECOLI_LOG,
			      {?MODULE, ?LINE, self()}, 
			      {__Sev, __Msg, __BL})
	end).




%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           record_name
%%% Description: 
%%% ----------------------------------------------------------
%% record used in mnesia table and by moduls ecoli_DataInit,
%%	ecoli_cmd_shell, ecoli_internal_cmd, and ecoli_ssh_channel.
%%
%% The record is also used in 
%% /vobs/rcs/tools/RDE_LXA119945/tools/mkcpi/mkcpi.escript. 
%% However, the hrl file is not included in the script, 
%% so if this record is changed the script must also be updated.  
-record(coli_cmd,       {cli_pname,
			 cli_scope     = ?FRU_LOCAL,  %% central, local, fruacc
			 cli_type,
			 type,
			 %% authorization:  0:no_auth, 1:expert, 2:advanced, 3:basic
			 authorization = 0,
			 cxp_path,
			 relpath,
			 filepath,
			 subcommand,
			 module,
			 function,
			 args = [],
			 usage = "",
			 description = <<>>}).

%% intermediate format used in modules ecoli_cmd_shell, ecoli_internal_cmd,
%% and ecoli_exec
-record(cmd,	{name, path, args, remote_node}).

-record(current_fru, {role      = local,  %% Role     = central | local | fruacc
		      fru_id    = "",     %% FruId    = string()
		      mp_id,              %% MpId     = integer()
		      cli_types = [],     %% CliType  = undefined | ru | xmu
		      paths     = []      %% CliPaths = [Path]
		     }).

-record(tab_fru, {role   = fruacc,  %% Role     = central | local | fruacc
		  fru_id = "",      %% FruId    = string()
		  mp_id,            %% MpId     = integer()
		  type   = "",     %% CliType  = undefined | ru | xmu
		  paths   = []      %% CliPaths = [Path]
		 }).

%% record used in ecoli_cmd_shell, ecoli_tab_expand, and ecoli_internal_cmd
-record(cst, 
	{
	  auth         = 4,                 %% meaning "no valid role"
	  prompt       = "coli [\\h\\w]-> ",
%%	  prompt       = "coli [\\w]-> ",
	  acc          = [],
	  all_paths,
	  history      = {0, [], [], queue:new()},
	  level        = "/",
	  start_fru,                        %% {FruId, CliTypes, Paths}
	  current_fru  = #current_fru{},
	  fru_names    = [],                %% [Name]
	  fruacc_names = [],                %% [Name]
	  count        = 0
	 }).


-define(OLD_PROMPT, "coli [\\w]-> ").

-record(fruacc, {
	  key,       %% {CliType, CliPrefix, Mbox}
	  ldns = []  %% [Ldn]
	  }).



%%--------------------------------------------------
%% list of all PMS modules (used for debugging)
%%--------------------------------------------------
-define(ECOLI_MODS, [
		     ecoli_cmd_shell,
		     ecoli_datainit,
		     ecoli_debug,
		     ecoli_exec,
		     ecoli_internal_cmd,
		     ecoli_io,
		     ecoli_itc_server,
		     ecoli_lib,
		     ecoli_register,
		     ecoli_ssh_channel,
		     ecoli_tab_expand,
		     ecoli_transport_adaptor
		    ]).




