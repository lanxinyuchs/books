%%% --------------------------------------------------------
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% --------------------------------------------------------

-hrl_id({"RcsHttpM","1.0.2","/main/R3A/R4A/R12A/0"}).


%% -------------- CLASS HttpM -------------------------

%% Description:
%% HTTP management.

-record(httpM, {httpMId,
                dummy}).

-define(httpM_types,
        [{httpMId, string},
         {dummy, atom}]).

-define(HttpM_restricted, [httpMId]).


%% -------------- CLASS Https -------------------------

%% Description:
%% Configuration for HTTP secure service.
%% The HTTPS service allows secure communication over TLS with the web applications of the system. Basic web-service (port 443) is only using certificates for secure transport. Extended web-service (port 8443) is using certificates for secure transport and login validation.

-record(https, {httpsId,
                nodeCredential,
                trustCategory}).

-define(https_types,
        [{httpsId, string},
         {nodeCredential, moRef},
         {trustCategory, moRef}]).

-define(Https_restricted, [httpsId]).

