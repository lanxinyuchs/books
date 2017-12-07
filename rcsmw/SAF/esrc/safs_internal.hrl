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
%%----------------------------------------------------------------------
%% File    : safs_internal.hrl
%%
%% Description : 
%%   Internal data types and constants for SAFS
%%
%%----------------------------------------------------------------------

-ifndef(safs_internal_hrl).
-define(safs_internal_hrl, true).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(APPLICATION, safs).

-define(INFO(Format, Data),
	io:format("\n~p(~p): " ++ Format, 
		  [?MODULE, ?LINE | Data])).

-define(WARNING(Format, Data),
	io:format("\n~p(~p): <WARNING> " ++ Format,
		  [?MODULE, ?LINE | Data])).

-define(ERROR(Format, Data),
	io:format("\n~p(~p): <ERROR> " ++ Format,
		  [?MODULE, ?LINE | Data])).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------


-endif.
