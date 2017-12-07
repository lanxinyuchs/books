%%% %CCaseFile:	pms_c_util.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/2

%%% @doc == Utility functions for tests of the PM C interface ==

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
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/3      2013-04-08 erarafo     Counter values larger than 2^31
%%% R2A/4      2014-04-22 uabesvi     show counters
%%% ----------------------------------------------------------

-module(pms_c_util).


-export([
		 createGroups/1,
		 createGroupSpecs/1,
		 createValueBundles/3,
		 tuplifyBundles/1,
		 tuplifyMTs/1,
		 listifyBundles/1
		]).


%% @doc Create some groups, using the given string
%% as the first group.

-spec createGroups(string()) -> [string()].

createGroups(Name) ->
	[Name, "G", "H", "U"].


%% @doc Turns the given groups into a list of GroupSpec.

createGroupSpecs(Groups) ->
	[{G, [G++"_c1", G++"_c2", G++"_c3"]} || G <- Groups].

%% @doc Turns the given non-empty list of groups
%% into a list of value bundles.
%%
%% If the group name is "U" then insert a measurement
%% value in the 0-255 range. The purpose is to test
%% handling of very small integers through the C interface.

createValueBundles([G1 | _]=Groups, Timespec, Bias) ->
    UserCode = userCode(G1),
    GroupSpecs = createGroupSpecs(Groups),
    [
     case G of
	 "U" ->
	     {G, [
		  case MTNames of
		      [MTName|_] ->
			  {MTName, 1, {0}};
		      [_, MTName | _] ->
			  {MTName, 1, {127}};
		      _ ->
			  {MTName, 1, {255}}
		  end
		  || MTName <- MTNames]};
	 _ ->
	     {G, values(MTNames, Timespec+UserCode+Bias)}
     end
     ||
     {G, MTNames} <- GroupSpecs].





%% Returns a number that is the sum of codes in the
%% given string.

userCode("") ->
	0;

userCode([X|Y]) ->
	X + userCode(Y).


%% Returns a list of measurement values formed from
%% the given list of names and the given bias.

values(MTNames, Bias) ->
    [begin 
	 Value = tweakValue(length(MTName) + Bias),
	 %%ct:print("v: ~p", [{MTName, 1, {Value}}]),
	 {MTName, 1, {Value}}
     end
     || MTName <- MTNames].


%% @doc Tweak the given value so as to produce the edge values 2147483647
%% and 2147483648 when X is close enough.

tweakValue(X) ->
    if
	X < 2147483648, (2147483648 - X) < 100 ->
	    2147483647;
	X > 2147483647, (X - 2147483647) < 100 ->
	    2147483648;
	true ->
	    X
    end.


tuplifyBundles(Bundles) ->
    [{Group, tuplifyMTs(MTs)} || {Group, MTs} <- Bundles].

tuplifyMTs(MTs) ->
    [{MT, length(Elements), list_to_tuple(Elements)} || {MT, Elements} <- MTs].

listifyBundles(Bundles) ->
    [{Group, listifyMTs(MTs)} || {Group, MTs} <- Bundles].

listifyMTs(MTs) ->
    [{MT, tuple_to_list(Elements)} || {MT, _Length, Elements} <- MTs].


