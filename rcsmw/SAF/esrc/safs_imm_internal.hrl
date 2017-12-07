%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2012. All Rights Reserved.
%% 
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%% 
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_internal.hrl
%% 
%% Description:
%%    Internal include file for IMM
%%
%%--------------------------------------------------------------------
-ifndef(safs_imm_internal_hrl).
-define(safs_imm_internal_hrl, true).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(IMM_RELEASE_CODE, $A).
-define(IMM_MAJOR_VERSION, 2).
-define(IMM_MINOR_VERSION, 13).

-define(IMM_CB_DEFAULT_TIMEOUT, 10000).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(safs_imm_om_search_data, {
	  search_handle, 
	  imm_handle,
	  rootname,
	  scope,
	  options,
	  classes,
	  params,
	  attributes,
	  pid
	 }).

-record(safs_imm_om_adm_owner, {
	  owner_handle, 
	  imm_handle,
	  name,
	  release_on_finalize
	 }).

-record(safs_imm_om_administrated_objects, {
	  key	  
	 }).

-endif.
