#! /usr/bin/env escript
%%% ----------------------------------------------------------
%%% %CCaseFile:	app_info.escript %
%%% Author:	erarafo
%%% Description: Invoke as follows:
%%%
%%%   app_info.escript APPFILE...
%%%
%%% The given files are assumed to be Erlang .app files. Info
%%% from each file is listed to standard output.
%%%
%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% R2A/1      2014-05-20 erarafo     First version
%%% R2A/2      2014-05-22 erarafo     Improved version
%%% R2A/3      2014-05-22 erarafo     Formatting tweaked
%%% ----------------------------------------------------------


fileToTerm(File) ->
    {ok, Stream} = file:open(File, [read]),
    {ok, {application, AppName, Props}} = io:read(Stream, ""),
    file:close(Stream),
    {application, AppName, Props}.


%% Some entries appear to be doubled. This is because tgt_i686 and
%% tgt_i686_32 versions are both delivered for the simulator.

displayList(Terms) ->
    [
     begin
         Id = proplists:get_value(id, Props, ""),
         Vsn = proplists:get_value(vsn, Props, "unknown"),
         Descr = proplists:get_value(description, Props, ""),
         io:format("~15w ~12s  ~-9s ~s~n", [AppName, Id, Vsn, Descr])
     end
    || {_, AppName, Props} <- ordsets:to_list(ordsets:from_list(Terms))].


main(Args) ->
    Terms = [fileToTerm(AppFile) || AppFile <- Args],
    {HasId, HasNoId} = lists:partition(
        fun({_, _, Props}) -> proplists:is_defined(id, Props) end,
        Terms),
    displayList(HasNoId),
    displayList(HasId).
