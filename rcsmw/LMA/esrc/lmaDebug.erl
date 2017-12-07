%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaDebug.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/1
%%% @doc ==LMA Debug module==
%%% This module is implemented for debug reason. It shall be possible 
%%% to fetch LMA and GLMS information during runtime.
%%% @end
%%%
%%% ----------------------------------------------------------

-module(lmaDebug).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/1').
-date('2016-04-05').
-author('etxpejn').

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
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1     2014-02-18   etxpejn    Created
%%% R2A/2     2014-03-27   etxpejn    Updates
%%% R2A/3     2014-03-28   etxpejn    Added edoc
%%% R4A/1     2015-08-18   etxpejn    Changed debug prints to files 
%%% R5A/1     2016-04-05   etxpejn    Corrected printing to file
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([glms_info/0]).
-export([glms_info/1]).
-export([help/0]).
-export([lma_info/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ---------------------------------------------
-export([print/2]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Prints information of all debug functions. 
%%% @end
%%% ----------------------------------------------------------
-spec help() -> ok.

help() ->
    io:format("~n"
              "=========================================================~n"
              "===                 LMA debug functions               ===~n"
              "=========================================================~n"
              " help()                          This help text~n"
              "=================== LMA shortcuts =======================~n"
              " glms_info()                     Dumps all GLMS info~n"
              " glms_info(capacity_key_data)    Dumps CapacityKey runtime data, including subscriber status~n"
	      " glms_info(feature_key_data)     Dumps FeatureKey runtime data, including subscriber status~n"
	      " glms_info(lfci_client)          Dumps runtime data of the LFCI clients~n"
	      " glms_info(lcci_client)          Dumps runtime data of the LCCI clients~n"
	      " glms_info(glms_state_data)      Dumps runtime data of GLMS~n~n"
              " lma_info()                      Shows the status of the LMA server~n"

              "=========================================================~n~n"
             ).

%%% ----------------------------------------------------------
%%% @doc Fetches all information from GLMS. 
%%% @end
%%% ----------------------------------------------------------
-spec glms_info() -> ok.

glms_info() ->
    glms_info(capacity_key_data),
    glms_info(feature_key_data),
    glms_info(lfci_client),
    glms_info(lcci_client),
    glms_info(glms_state_data).
       
%%% ----------------------------------------------------------
%%% @doc Fetches specific information from GLMS. 
%%% @end
%%% ----------------------------------------------------------
-spec glms_info(atom()) -> ok.

glms_info(capacity_key_data) ->
    lmaGlms:dump_capacity_key_data_req();
glms_info(feature_key_data) ->
    lmaGlms:dump_feature_key_data_req();
glms_info(lfci_client) ->
    lmaGlms:dump_lfci_client_data_req();
glms_info(lcci_client) ->
    lmaGlms:dump_lcci_client_data_req();
glms_info(glms_state_data) ->
    lmaGlms:dump_glms_state_data_req().

%%% ----------------------------------------------------------
%%% @doc Function called from lmaGlms to print information
%%% @private
%%% @end
%%% ----------------------------------------------------------
-spec print(list(), atom()) -> ok.

print("No info from glms", Function) ->
    io:format("Not able to fetch information from GLMS for ~p~n", [Function]);
print(Dump, FileType) ->
    File = filename:join([lmaLib:get_lma_dir(), atom_to_list(FileType)]),
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, Dump, [append]).
    

%%% ----------------------------------------------------------
%%% @doc Fetches and prints information from LMA.
%%% @end
%%% ----------------------------------------------------------
-spec lma_info() -> ok.

lma_info() ->
    State = lmaGlms:get_state(),
    io:format("Port:            ~p~n" 
	      "Pid:             ~p~n"
	      "S Pid:           ~p~n"
	      "ITC port:        ~p~n"
	      "Heartbeat:       ~p~n"
	      "GLMS restarted:  ~p~n"
	      "ECIM Pid:        ~p~n"
	      "Action Id:       ~p~n"
	      "Alarms Keys:     ~p~n",
	      [element(2, State),
	       element(3, State),
	       element(4, State),
	       element(5, State),
	       element(6, State),
	       element(7, State),
	       element(8, State),
	       element(9, State),
	       element(11, State)]).

