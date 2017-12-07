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
%% File: safs_imm_om.hrl
%% 
%% Description:
%%    Internal include file for IMM OM
%%
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(safs_imm_om_search_data, {
	  search_handle, 
	  imm_handle,
	  rootname,
	  scope,
	  options,
	  params,
	  attributes,
	  pid
	 }).
