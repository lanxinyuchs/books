#! /usr/bin/env escript
%%
%% %CCaseFile:	decompile.escript %
%% %CCaseRev:	/main/R4A/1 %
%% %CCaseDate:	2015-05-19 %
%% Author: <name>, <e-mail address>
%%
%% Purpose: Decompile a BEAM file to standard output.
%%
%% Dependencies:
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2015 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the
%% receiver of this document shall keep the information contained
%% herein confidential and shall protect the same in whole or in
%% part from disclosure and dissemination to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%
%%
%% ----------------------------------------------------------------------
%%
%% Revision history:
%%
%% Rev        Date       Name        What
%% -----      -------    --------    ------------------------------------
%% R4A/1      2015-05-19 erarafo     First version
%% ----------------------------------------------------------------------

main([BeamFile]) ->
  {ok, {_, [{abstract_code, {_, AC}}]}} =
    beam_lib:chunks(BeamFile, [abstract_code]),
  io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).
