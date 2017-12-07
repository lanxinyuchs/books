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
%% File: safs.erl
%%
%% Description:
%%    This file contains definitions for the SAFS IMM database
%%
%%--------------------------------------------------------------------
-ifndef(safs_imm_db_hrl).
-define(safs_imm_db_hrl, true).
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(imm_class,  {name, rdn,  category, attrs=[], oi, oi_appliers=[]}).
-record(imm_rdn,    {name, type, category, flags=[]}).
-record(imm_attr,   {name, type, category, flags=[], default, values=[]}).
-record(imm_object, {dn, class, attrs=[]}).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SAFS_SEARCH_RESULT_LIMIT, 200).

-endif.
