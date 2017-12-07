%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_httpd_custom.erl %
%%% @author etxtory
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/1

%%% @doc == https header filtering for request and response ==
%%% @end
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(omc_httpd_custom).
-vsn('/main/R3A/R4A/1').
-date('2015-10-10').
-author('etxtory').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% Rev     Date     Name      What
%%% -----   -------  --------  ------------------------
%%% R3A/1   150603   etxtory   HT74963: Removed etag header
%%% R4A/1   151010   etxtory   HU20190: Added xframe header
%%% ----------------------------------------------------------

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([response_header/1,
	 request_header/1,
	 response_default_headers/0]).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Filters headers in https-response.
%%% Header "etag" is filtered (removed) from https-response.
%%% @end
%%% ----------------------------------------------------------
response_header({"etag", _}) ->
    false;
response_header(Header) ->
    {true, Header}.

%%% ----------------------------------------------------------
%%% @doc Filters headers in https-request.
%%% Currently no filter.
%%% @end
%%% ----------------------------------------------------------
request_header(Header) ->
    {true, Header}.

%%% ----------------------------------------------------------
%%% @doc Adds headers in https-response.
%%% @end
%%% ----------------------------------------------------------
response_default_headers() ->
    [{"X-Frame-Options", "SAMEORIGIN"}]. %% NESSUS plugin: 85582
