%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_db.hrl
%%
%% Description:
%%    This file contains definitions for the SAFS LOG Service implementation
%%
%%--------------------------------------------------------------------
-ifndef(safs_log_db_hrl).
-define(safs_log_db_hrl, true).
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(safs_log_stream_config, {stream_name, 
				 attributes_cfg, %% not used, delete?
				 create_time,
				 create_attributes, 
				 filters         %% not used, delete?
				}).

-record(safs_log_user, {handle, 
			version, 
			callbacks,
			proxy, 
			cb_proxy}).


-record(file_data, {disk_log_id,
		    name,
		    path,
		    file_names = [],
		    type,
		    rec_format,
		    max_file_size,
		    max_rec_size,
		    max_rotated,
		    cfg_file}).

-record(cfg_attributes, {version,
			 format,
			 max_file_size,
			 rec_fixed_size,
			 full_action,
			 max_rotated
			 }).

-endif.
