%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfI.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R8A/2

%%% @doc ==GMF Interface Module==
%%% This module is an interface module for miscelaneous GMF functions.
%%% end

-module(gmfI).
-vsn('/main/R3A/R8A/2').
-date('2016-12-15').
-author('etxpeno').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date         Name        What
%%% -----      -------      --------    ------------------------
%%% R1A/1      2014-09-08   uabesvi     Created
%%% R8A/1      2016-12-13   etxpeno     Add to_coi/2
%%% R8A/2      2016-12-15   etxpeno     Add imm_to_mim/1, mim_to_imm/1, to_coi/2
%%% ----------------------------------------------------------

-export([check_integer/1]).
-export([check_string/1]).

-export([get_integer/1]).
-export([get_string/1]).

-export([to_coi/2]).

-export([imm_to_mim/1, mim_to_imm/1]).
-export([get_attribute_names/1]).

%%===========================================================================
%% Interface functions for transforming integers to strings, and vice versa.
%% The integers can be seen as aliases for the strings.
%%===========================================================================

%% check_integer(Integer) -> {ok, String} | {error, not_found}
check_integer(Integer) when is_integer(Integer) ->
    gmfTrService:check_integer(Integer).

%% check_string(String) -> {ok, Integer} | {error, not_found}
check_string(String) when is_list(String) ->
    gmfTrService:check_string(String).


%% get_integer(String) -> {ok, Integer}
get_integer(String) when is_list(String) ->
    gmfTrService:get_integer(String).

%% get_string(Integer) -> {ok, String} | {error, not_found}
get_string(Integer) when is_integer(Integer) ->
    gmfTrService:get_string(Integer).

%%===========================================================================
%% Interface function for convert an IMM DN to an ECIM DN and mapping
%% a list of values to a format defined by COI.
%%===========================================================================
-spec to_coi(ImmDn, Values) -> {ok, EcimDn, CoiValueList} | {error, Reason} when
      ImmDn :: binary(),
      Values :: list(Value),
      Value :: {AttributeName, AttributeValue},
      AttributeName :: binary(),
      AttributeValue :: term(),
      EcimDn :: binary(),
      CoiValueList :: list(),
      Reason :: term().
to_coi(ImmDn, Values) ->
    gmfSALib:to_coi(ImmDn, Values).

%%===========================================================================
%% Interface function for convert an ECIM DN to an IMM DN.
%%===========================================================================
-spec mim_to_imm(MimDn) -> {ok, ImmDn} | {error, Reason} when
      MimDn  :: binary(),
      ImmDn  :: binary(),
      Reason :: term().
mim_to_imm(MimDn) ->
    gmfSALib:mim_to_imm(MimDn).

%%===========================================================================
%% Interface function for convert an IMM DN to an ECIM DN.
%%===========================================================================
-spec imm_to_mim(ImmDn) -> {ok, MimDn} | {error, Reason} when
      ImmDn  :: binary(),
      MimDn  :: binary(),
      Reason :: term().
imm_to_mim(ImmDn) ->
    gmfSALib:imm_to_mim(ImmDn).

%%===========================================================================
%% Interface function for get all attribute names from an IMM DN.
%% The name of the attribute used as the key is not included.
%%===========================================================================
-spec get_attribute_names(ImmDn) -> {ok, AttrNames} | {error, Reason} when
      ImmDn  :: binary(),
      AttrNames :: list(AttrName),
      AttrName :: binary(),
      Reason :: term().
get_attribute_names(ImmDn) ->
    gmfSALib:get_attribute_names(ImmDn).
