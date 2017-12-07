%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%% R2A/1      2014-02-10 etxpeno     Created
%%% ----------------------------------------------------------
-module(eqs_mmi).

-vsn('/main/R2A/2').
-date('2014-02-18').
-author('etxpeno').

-export([set_led_behavior/2]).
-export([reset_led_behavior/1]).
-export([get_led_behavior/1]).

-type indicator() :: fault       |
		     operational |
		     status      |
		     maintenance.

-type led_behavior() :: off              |
			steady_on        |
			fast_blink       |
			slow_blink       |
			double_flash_off |
			double_flash_on .

-spec set_led_behavior(Indicator :: indicator(),
		       LedBehavior :: led_behavior()) -> ok | {error, term()}.
set_led_behavior(Indicator, LedBehavior) ->
    eqs_mmi_service:set_led_behavior(Indicator, LedBehavior).

-spec reset_led_behavior(Indicator :: indicator()) -> ok.
reset_led_behavior(Indicator) ->
    eqs_mmi_service:reset_led_behavior(Indicator).

-spec get_led_behavior(Indicator :: indicator()) -> led_behavior() | {error, term()}.
get_led_behavior(Indicator) ->
    eqs_mmi_service:get_led_behavior(Indicator).
