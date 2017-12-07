%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_mod_dir.erl %
%%% @author etxtory
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/1
%%%
%%% @doc RBS own variant of mod_dir for http/https server
%%% ----------------------------------------------------------
-module(omc_mod_dir).
-vsn('/main/R3A/R4A/1').
-date('2015-06-18').
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R3A/1      15-05-26   etxtory     HT73324; Own mod_dir
%%% R4A/1      15-06-18   etxtory     404 for files not found 
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([do/1]).

-include_lib("inets/include/httpd.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
do(Info) ->
    case Info#mod.method of
	"GET" ->
	    case proplists:get_value(status, Info#mod.data) of
		%% A status code has been generated!
		{_StatusCode, _PhraseArgs, _Reason} ->
		    {proceed,Info#mod.data};
		%% No status code has been generated!
		undefined ->
		    case proplists:get_value(response, Info#mod.data) of
			%% No response has been generated!
			undefined ->
			    do_dir(Info);
			%% A response has been generated or sent!
			_Response ->
			    {proceed,Info#mod.data}
		    end
	    end;
	_ ->
	    %% Not a GET method!
	    {proceed, Info#mod.data}
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% do_dir
%%% Do not allowed listing of directory; reload main-page
%%% ----------------------------------------------------------
do_dir(Info) ->
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			  Info#mod.request_uri),
    DefaultPath = mod_alias:default_index(Info#mod.config_db,Path),
    case file:read_file_info(DefaultPath) of
	{ok, FileInfo} when FileInfo#file_info.type == directory ->
	    main_page(Info);
	{ok, _FileInfo} ->
	    {proceed, Info#mod.data};
	{error, Reason} ->
	    {proceed,
	     [{status,{404, Info#mod.request_uri, Reason}} | Info#mod.data]}
    end.

main_page(Info) ->
    Dir = ["<html><head><title>Welcome to RBS</title></head>\n<body><h1>Welcome to RBS</h1></body>\n</html>\n"],
    Head = [{content_type,"text/html"},
	    {content_length,
	     integer_to_list(httpd_util:flatlength(Dir))},
	    {code,200}],
    {proceed,[{response,{response, Head, Dir}},
	      {mime_type,"text/html"} | Info#mod.data]}.
