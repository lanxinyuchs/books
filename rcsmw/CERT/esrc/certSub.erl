%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certSub.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R9A/1
%%% 
%%% @doc ==Subscription module for CERT==
%%% This module contains functions event subscription for CERT
-module(certSub).
-vsn('/main/R2A/R3A/R4A/R9A/1').
-date('2017-02-16').
-author('etxasta').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	certSub.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% Rev        Date        Name       What
%%% -----      ----------  --------   ------------------------
%%% R2A/1      2014-02-05  etxasta    Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([
	 subscribe/2,
	 unsubscribe/2,
	 unsubscribe/1,
	 trig/2
    ]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("cert.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Subscribe for certificate changes events
%%% @end
%%% ----------------------------------------------------------
subscribe(MoRef, CbModule) ->
    info_msg("Subscribe, MoRef: ~p, Callback ~p:cert_nc_event/2~n",
        [MoRef, CbModule]),
    case certLib:decode_moref(MoRef) of
        error ->
            error;
        Index ->
            case mnesia:dirty_read(certSub, Index) of
                [] ->
                    New =
                    #certSub{
                        index = Index,
                        moref = MoRef,
                        cbModules = [CbModule]
                    },
                    mnesia:dirty_write(New);
                [Obj] ->
                    List1 = Obj#certSub.cbModules,
                    List2 =
                    lists:filtermap(
                        fun(X) ->
                                case X of
                                    CbModule ->
                                        false;
                                    _ ->
                                        {true, X}
                                end
                        end, List1),
                    NewList = lists:append(List2, [CbModule]),
                    mnesia:dirty_write(Obj#certSub{cbModules = NewList})
            end
    end.

%%% ----------------------------------------------------------
%%% @doc Unsubscribe for certificate changes events
%%% @end
%%% ----------------------------------------------------------
unsubscribe(MoRef, CbModule) ->
    info_msg("Unsubscribe MoRef: ~p, Callback ~p:cert_event/2~n",
	     [MoRef, CbModule]),
    case certLib:decode_moref(MoRef) of
        error ->
            error;
        Index ->
            case mnesia:dirty_read(certSub, Index) of
                [] ->
                    ok;
                [Obj] ->
                    List1 = Obj#certSub.cbModules,
                    List2 =
                    lists:filtermap(
                        fun(X) ->
                                case X of
                                    CbModule ->
                                        false;
                                    _ ->
                                        {true, X}
                                end
                        end, List1),
                    case List2 of
                        [] ->
                            mnesia:dirty_delete(certSub, Index);
                        _ ->
                            mnesia:dirty_write(Obj#certSub{cbModules = List2})
                    end
            end
    end.

unsubscribe(CbModule) ->
    info_msg("Unsubscribe Callback ~p:cert_event/2~n", [CbModule]),
    do_unsubscribe(CbModule, mnesia:dirty_all_keys(certSub)).

do_unsubscribe(_CbModule, []) ->
    ok;
do_unsubscribe(CbModule, [Key | T]) ->
    case mnesia:dirty_read(certSub, Key) of
	[Obj] ->
	    case lists:member(CbModule, Obj#certSub.cbModules) of
		true ->
		    case Obj#certSub.cbModules -- [CbModule] of
			[] ->
			    mnesia:dirty_delete(certSub, Obj#certSub.index);
			CbList ->
                            mnesia:dirty_write(Obj#certSub{cbModules = CbList})
		    end;
		false ->
		    ok
	    end;
	_ ->
	   ok
    end, 
    do_unsubscribe(CbModule, T).

%%% ----------------------------------------------------------
%%% @doc For the cert block to trig and possible event 
%%% @end
%%% ----------------------------------------------------------
trig(Type, Index) ->
    info_msg("Trig an possible events of cert change, ~p, ~p~n",
        [Type, Index]),
    %% Update distributed certificate files for VRCS and BPU
    certDist:update_cert_dist({Type, Index}),
    %% Check for subscription to trig
    case mnesia:dirty_read(certSub, {Type, Index}) of
        [] ->
            ok;
        [Obj] ->
            MoRef = Obj#certSub.moref,
            lists:foreach(
                fun({clib, Id}) ->
                        %% Event for the c-lib
                        certSeci:event(Id, MoRef);
                    (Module) ->
                        apply(Module, cert_event, [MoRef])
                end, Obj#certSub.cbModules)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).




%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

