%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmXml.erl %
%%% Author:
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfImmXml).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R3A/R6A/1').
-date('2016-09-15').
-author('etxpeno').
-shaid('261bf70bb09a5c3fb06ea80d13cf8b3b9d208b15').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R2A/2      2013-09-09 erarafo     Refactoring, edoc
%%% R2A/3      2013-09-17 erarafo     Edoc only
%%% R2A/4      2013-09-17 erarafo     Types specs only
%%% R6A/1      2016-09-15 etxpeno     MIB sync improvements
%%% ----------------------------------------------------------
%%%
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([parse_classes/3,
	 parse_objects/3,
	 %% Callback to SAX Parser
	 parse/3]).



%%% NOTE: NO ERROR HANDLING HAS BEEN IMPLEMENTED YET.
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("gmf.hrl").

-include_lib("xmerl/include/xmerl.hrl").



-record(gmfIX, {cxpProdId,
		cxpRev,
		file,
		type,
		classes = [],
		objects = [],
		prev,
		data}).
%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% Same code:: Keeping it separate for the time being.


-spec parse_classes(string(), string(), string()) -> [#gmfImmClass{}].

parse_classes(CxpProdId, CxpRev, File) ->
    ParseOps = [skip_external_dtd,
		{event_fun, fun parse/3},
		{event_state, #gmfIX{cxpProdId = CxpProdId,
				     cxpRev    = CxpRev,
				     file      = File,
				     type      = class}}],
    {ok, #gmfIX{classes = Classes}, _} = xmerl_sax_parser:file(File, ParseOps),
    lists:reverse(Classes).


-spec parse_objects(string(), string(), string()) -> [#gmfImmObject{}].

parse_objects(CxpProdId, CxpRev, File) ->
    ParseOps = [skip_external_dtd,
		{event_fun, fun parse/3},
		{event_state, #gmfIX{cxpProdId = CxpProdId,
				     cxpRev    = CxpRev,
				     file      = File,
				     type      = object}}],
    {ok, #gmfIX{objects = Objects}, _} = xmerl_sax_parser:file(File, ParseOps),
    lists:reverse(Objects).



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% @doc Callback function, required by xmerl_sax_parser:file/2

parse(Event, _LineNo, State) ->
    parse(Event, State).

%% The parsing is simplified to handle only what is needed.
%% For example object attributes are ignored.
%% The file are handled separately for classes and objects.
%% If both exist in the same, then GO TO HELL

parse({ignorableWhitespace, _}, State) ->
    State;

%% CLASS PARSING START
%% Class
parse({startElement, _, "class", _, [{_, _, "name", Name}]},
      State = #gmfIX{type      = class,
		     cxpProdId = CxpProdId,
		     cxpRev    = CxpRev,
		     file      = File}) ->
    CxpInfo = {CxpProdId, CxpRev},
    Data = #gmfImmClass{key           = Name,
			imm_file_name = File,
			cxp_info      = CxpInfo},
    State#gmfIX{data = Data};
parse({endElement, _, "class", _},
      State = #gmfIX{type    = class,
		     classes = Classes,
		     data    = #gmfImmClass{} = Class}) ->
    State#gmfIX{classes = [Class|Classes], data = undefined};

%% Category
parse({startElement, _, "category", _, _},
      State = #gmfIX{type = class,
		     prev = undefined,
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = category};
parse({characters, Chars},
      State = #gmfIX{type = class,
		     prev = category,
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = {category, Chars}};
parse({endElement, _, "category", _},
      State = #gmfIX{prev ={category, _} = Cat,
		     type = class,
		     data = #gmfImmClass{} = Class}) ->
    State#gmfIX{prev = undefined,
		data = Class#gmfImmClass{category = Cat}};

%% RDN
parse({startElement, _, "rdn", _, _},
      State = #gmfIX{type = class,
		     prev = undefined,
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = {rdn, undefined, []}};
parse({startElement, _, "name", _, _},
      State = #gmfIX{type = class,
		     prev = {rdn, undefined, []},
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = {rdn, name, []}};
parse({characters, Chars},
      State = #gmfIX{type = class,
		     prev = {rdn, name, Prop},
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = {rdn, Chars, Prop}};
parse({startElement, _, Element, _, _},
      State = #gmfIX{type = class,
		     prev = {rdn, Name, Prop},
		     data = #gmfImmClass{}}) ->
    State#gmfIX{type = class,
		prev = {rdn, Name, [?L2A(Element)|Prop]}};
parse({characters, Chars},
      State = #gmfIX{type = class,
		     prev = {rdn, Name, [P|Prop]},
		     data = #gmfImmClass{}}) when is_atom(P) ->
    State#gmfIX{type = class,
		prev = {rdn, Name, [{P,Chars}|Prop]}};
parse({endElement, _, Element, _},
      State = #gmfIX{type = class,
		     prev = {rdn, _, _},
		     data = #gmfImmClass{}}) when Element /= "rdn" ->
    State;
parse({endElement, _, "rdn", _},
      State = #gmfIX{type = class,
		     prev = {rdn, Name, Props},
		     data = #gmfImmClass{} = Data}) ->
    State#gmfIX{prev = undefined,
		data = Data#gmfImmClass{rdn = {Name, Props}}};

%% Attr (ALMOST A COPY OF RDN we can optimise it later on)
parse({startElement, _, "attr", _, _},
      State = #gmfIX{type = class,
		     prev = undefined,
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = {attr, undefined, []}};
parse({startElement, _, "name", _, _},
      State = #gmfIX{type = class,
		     prev = {attr, undefined, []},
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = {attr, name, []}};
parse({characters, Chars},
      State = #gmfIX{type = class,
		     prev = {attr, name, Prop},
		     data = #gmfImmClass{}}) ->
    State#gmfIX{prev = {attr, Chars, Prop}};
parse({startElement, _, Element, _, _},
      State = #gmfIX{type = class,
		     prev = {attr, Name, Prop},
		     data = #gmfImmClass{}}) ->
    State#gmfIX{type = class,
		prev = {attr, Name, [?L2A(Element)|Prop]}};
parse({characters, Chars},
      State = #gmfIX{type = class,
		     prev = {attr, Name, [P|Prop]},
		     data = #gmfImmClass{}}) when is_atom(P) ->
    State#gmfIX{type = class,
		prev = {attr, Name, [{P,Chars}|Prop]}};
parse({endElement, _, Element, _},
      State = #gmfIX{type = class,
		     prev = {attr, _, _},
		     data = #gmfImmClass{}}) when Element /= "attr" ->
    State;
parse({endElement, _, "attr", _},
      State = #gmfIX{type = class,
		     prev = {attr, Name, Props},
		     data = #gmfImmClass{attributes = Attrs} = Data}) ->
    State#gmfIX{prev = undefined,
		data = Data#gmfImmClass{attributes = [{Name, Props}|Attrs]}};
%% CLASS PARSING END



%% OBJECT PARSING START
%% Object
parse({startElement, _, "object", _, [{_, _, "class", Name}]},
      State = #gmfIX{type      = object,
		     cxpProdId = CxpProdId,
		     cxpRev    = CxpRev,
		     file      = File}) ->
    Data = #gmfImmObject{key          = {undefined, Name, CxpProdId, CxpRev},
			 imm_file_name = File},
    State#gmfIX{data = Data};
parse({endElement, _, "object", _},
      State = #gmfIX{type    = object,
		     objects = Objects,
		     data    = #gmfImmObject{} = Object}) ->
    State#gmfIX{objects = [Object|Objects], data = undefined};

%% dn
parse({startElement, _, "dn", _, _},
      State = #gmfIX{type = object,
		     prev = undefined,
		     data = #gmfImmObject{}}) ->
    State#gmfIX{prev = dn};
parse({characters, Chars},
      State = #gmfIX{type = object,
		     prev = dn,
		     data = #gmfImmObject{}}) ->
    State#gmfIX{prev = {dn, Chars}};
parse({endElement, _, "dn", _},
      State = #gmfIX{prev ={dn, DN},
		     type = object,
		     data = #gmfImmObject{key = {undefined, C1, C2, C3}} = Object}) ->
    State#gmfIX{prev = undefined,
		data = Object#gmfImmObject{key = {DN, C1, C2, C3}}};

%% OBJECT PARSING END



parse(_, State) ->
    State.
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
