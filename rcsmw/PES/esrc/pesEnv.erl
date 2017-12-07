%% ===========================================================================
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%% ===========================================================================
%% @hidden
%% @author uabesvi
%% @copyright Ericsson AB 2014-2016
%% @doc
%% This module provides interfaces for using the PES Design Debug Application,
%% in short PES.
%% 
%%
%%
%% == Examples ==
%% A chronological example of usage
%%
%% Add a filter called testing
%% <pre>
%% pesEnv:add_filter(pesX).
%% </pre>
%%
%% A log without the filter tag will not be printed
%% <pre>
%% pesEnv:message({format, {"Hello World~n", []}}).
%% </pre>
%%
%% A log with the filter tag testing is printed
%% <pre>
%% pesEnv:message({format, pesX, {"PES X Hello World~n", []}}).
%% </pre>
%%
%% A log with the filter tag pesY is not printed
%% <pre>
%% pesEnv:message({format, pesY, {"PES Y Hello World~n", []}}).
%% </pre>
%%
%% Remove a filter called testing
%% <pre>
%% pesEnv:remove_filter(pesX).
%% </pre>
%%
%% If there are no filters defined, the following will be printed 
%% regardless of the presence of a tag in the format message.
%% <pre>
%% pesEnv:message({format, pesZ, {"Hello World~n", []}}).
%%
%% pesEnv:message({format, {"normal Hello World~n", []}}).
%% </pre>
%%
%%
%% @end

-module(pesEnv).
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-04-05').
-author('uabesvi').
-shaid('5ec403a880dd59bfb32e21ae7245dc2143f26d9a').
