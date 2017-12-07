%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cert.hrl %
%%% @author etomist
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R4A/R9A/1
%%% 
%%% @doc ==hrl-file for common internal data==
%%% This module defines CERT internal data 

-vsn('/main/R2A/R4A/R9A/1').
-date('2017-03-02').
-author('etomist').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% Rev        Date        Name      What
%%% -----      ----------  --------  -------------------------
%%% R2A/1      2013-12-10  etxasta   Created
%%% R9A/1      2017-03-02  etomist   HV57826 (certNcState record)
%%% ----------------------------------------------------------


-record(certNC,
        {index,     % Same index as for nodeCredential table
         action   = no_action, % Ongoing between actions
         timeout,
         cert,
         csr,
         keyStore = [],
         ai = false,
         offline_p12}).

-define(certNC_types,
        [{index,       string},
         {action,      atom},
         {timeout,     atom},
         {cert,        term},
         {csr,         term},
         {keyStore,    string},
         {ai,          atom},
         {offline_p12, term}]).

-record(certTC,
        {index,     % Same index as for trustedCertificate table
         timeout,
         fingerprint,
         cert}).

-define(certTC_types,
        [{index,   string},
         {timeout, atom},
         {fingerprint, string},
         {cert, binary}]).

%% Used to store certificate change subscriptions 
-record(certSub,
        {index,      % {Type, Index}
         moref,      % MoRef of nodeCredential or TrustCategory
         cbModules}).% List of callback modules

-define(certSub_types,
        [{index,    term},
         {moref,    string},
         {cbModule, list}]).

-record(certCrl,
        {path, % Used as index
         crl,
         this_update,
         next_update}). 

-define(certCrl_types,
        [{path, string},
         {crl,  binary},
         {this_update, term},
         {next_update, term}]).

-record(certNcState,
        {key,
         alarm_timer_value}).

-define(certNcState_types,
        [{key, term},
         {alarm_timer_value, term}]).
