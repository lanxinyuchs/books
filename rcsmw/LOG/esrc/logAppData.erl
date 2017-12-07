%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logAppData.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(logAppData).
-vsn('/main/R1A/R2A/R9A/1').
-date('2017-02-08').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-02-16 etxjotj     Created
%%% R2A/6      2013-12-12 uabesvi     changed LogM to RcsLogM
%%% R9A/1      2017-02-08 uabesvi     Added recordSize to appdata
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([appdata/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsLogM.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

appdata(_CxpProdId, _CxpVersion, AppDataE) ->
    parse_log(AppDataE#xmlElement.content).

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

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

parse_log([LogE|Content]) 
  when LogE#xmlElement.name == log ->
    Name = find_attribute(name, LogE),
    Options = get_options(LogE, [maxSize, rotatingSegments, recordSize]),
    logServer:create_app_log(Name, Options),
    parse_log(Content);
parse_log([_|Content]) ->
    parse_log(Content);
parse_log([]) -> ok.


get_options(LogE, [Opt | Options]) 
 when Opt == maxSize;
      Opt == rotatingSegments;
      Opt == recordSize ->
    try find_attribute(Opt, LogE) of
	Str -> [{Opt, list_to_integer(Str)} | 
		get_options(LogE, Options)]
    catch _:_ -> 
	    get_options(LogE, Options)
    end;		    
get_options(LogE, [_ | Options]) ->
    get_options(LogE, Options);
get_options(_, []) ->
    [].


%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

%% find_element(ElementName, Element) when is_record(Element, xmlElement) ->
%%     find_element(ElementName, Element#xmlElement.content);
%% find_element(ElementName, ContentList) ->
%%     {value, Element} =
%%         lists:keysearch(ElementName, #xmlElement.name, ContentList),
%%     Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

%% find_text(Element) when is_record(Element, xmlElement) ->
%%     [Text] = Element#xmlElement.content,
%%     Text#xmlText.value.

		      
		       

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
