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
%% File: safs_ntf_db_db.hrl
%%
%% Description:
%%    This file contains definitions for the SAFS NTF implementation
%%
%%--------------------------------------------------------------------
-ifndef(safs_ntf_db_hrl).
-define(safs_ntf_db_hrl, true).
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(ntf_sent_notification, {time_id, notification}).
-record(ntf_notification_id, {id, value}).

-record(safs_ntf_reader, {read_handle, pid, handle, search_criteria,
			  notification_type_filters, last_event_time,
			  last_notification_id}).

-record(safs_ntf_subscriber, {id, notificationtype_filters}).

-endif.
