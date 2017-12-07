%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaAuth.erl %
%%% @author uabhgma
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R4A/R6A/1

%%% @doc ==Local authorization==
%%% @end

-module(comsaAuth).
-vsn('/main/R3A/R4A/R6A/1').
-date('2016-05-10').
-author('uabhgma').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R3A/1      2015-03-02 etxjotj     Created
%%% R3A/2      2015-03-24 etxjxotj    Added protection agains double regs
%%% R6A/1      2016-05-04 uabhgma     ComSecM -> RcsSecM
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

-include("RcsLocalAuthorization.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Parse authorization appdata
%%% @end
%%% ----------------------------------------------------------

appdata(_, _, AppdataE) ->
    [parse_role(RoleE)||RoleE<-AppdataE#xmlElement.content,
			RoleE#xmlElement.name == role],
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

parse_role(RoleE) ->
    RoleId = find_attribute(id, RoleE),
    UserLabel = try find_attribute(userLabel, RoleE) of
		    UL -> UL
		catch _:_ ->
			undefined
		end,
    Rules = [parse_rule(RuleE)||RuleE<-RoleE#xmlElement.content,
				RuleE#xmlElement.name == rule],
    {atomic, ok} = mnesia:transaction(
		     fun() -> add_data(RoleId, UserLabel, Rules) end).

parse_rule(RuleE) ->
    Id = find_attribute(id, RuleE),
    Permission = find_attribute(permission, RuleE),
    UserLabel = try find_attribute(userLabel, RuleE) of
		    UL -> UL
		catch _:_ ->
			undefined
		end,
    RuleData = [{find_attribute(index, RuleDataE), find_text(RuleDataE)}||
		   RuleDataE<-RuleE#xmlElement.content,
		   RuleDataE#xmlElement.name == ruleData],
    {Id, Permission, UserLabel, RuleData}.

add_data(RoleId, RoleUserLabel, Rules) ->
    Key = {"1","1","1","1","1",RoleId},
    case lists:member(Key, mnesia:all_keys(role)) of
	true ->
	    sysInitI:error_msg("Role already exists: ~p~n",[RoleId]);
	false ->
	    mnesia:write(#role{roleId = Key,
			       roleName = RoleId,
			       userLabel = RoleUserLabel}),
	    [[begin
		  RuleId = Id++"_"++Ix,
		  mnesia:write(
		    #rule{ruleId = {"1","1","1","1","1",RoleId, RuleId},
			  ruleName = RuleId,
			  permission = permission(Permission),
			  ruleData = Rule,
			  userLabel = UserLabel})
	      end||{Ix,Rule} <-RuleData]
	     ||{Id, Permission, UserLabel, RuleData}<-Rules]
    end,
    ok.

permission("R") -> ?PermissionType_R;
permission("RW") -> ?PermissionType_RW;
permission("X") -> ?PermissionType_X;
permission("RX") -> ?PermissionType_RX;
permission("RWX") -> ?PermissionType_RWX;
permission("NO_ACCESS") -> ?PermissionType_NO_ACCESS.

					       
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

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value.
			       
    


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

