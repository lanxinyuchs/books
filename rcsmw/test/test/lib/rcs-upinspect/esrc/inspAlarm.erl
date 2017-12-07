%%% ----------------------------------------------------------
%%% %CCaseFile:	inspAlarm.erl %
%%% Author:	erarafo
%%% Description: Inspects 'alarm' appdata.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(inspAlarm).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R5A/R6A/R8A/2').
-date('2016-12-01').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R5A/1      2016-02-11 erarafo     First version.
%%% R5A/2      2016-02-12 erarafo     Inconsistent defaultSeverity flagged as ERROR
%%% R5A/3      2016-02-15 erarafo     "Inspector" behaviour
%%% R5A/4      2016-02-15 erarafo     Extended checking of variants
%%% R5A/6      2016-02-22 erarafo     Support for "verbose" action
%%% R8A/1      2016-11-29 erarafo     Refactoring
%%% R8A/2      2016-12-01 erarafo     Fix dialyzer warning
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc ...
%%% @end
%%% ----------------------------------------------------------

-export([inspect/2, summary/0]).

-import(upLib, [err/2,
		warn/2, 
		info/2,
		progress/1,
		valDictBind/3,
		varDictStore/3,
		varDictGet/3,
		varDictGetAll/1,
		getRequiredAttribute/2, 
		getAttribute/2, 
		getSubElements/2]).

-include("upInspect.hrl").

inspect(Appdata, #inspContext{cxpInfo=#cxpInfo{id=CxpProdId}, 
			      cxcInfo=#cxcInfo{},
			      resolvedFile=File,
			      options=Opts}) ->
    % info("alarm appdata: ~p", [Appdata]),

    {ok, ReportFilenames} = orddict:find(inspAlarmsVerbose, Opts),

    AlarmTypes = getSubElements(alarmtype, Appdata),
    
    lists:foldl(
      fun(AlarmType, Acc) ->
	      Maj = getRequiredAttribute(majorType, AlarmType),
	      Min = getRequiredAttribute(minorType, AlarmType),
	      MoClasses = getRequiredAttribute(moClasses, AlarmType),
	      SpecificProblem = getRequiredAttribute(specificProblem, AlarmType),
	      ProbableCause = getRequiredAttribute(probableCause, AlarmType),
	      IsStateful = getRequiredAttribute(isStateful, AlarmType),
	      AdditionalText = getRequiredAttribute(additionalText, AlarmType),
	      Name = getAttribute(name, AlarmType),
	      EventType = getAttribute(eventType, AlarmType),
	      DefaultSeverity = getAttribute(defaultSeverity, AlarmType),
	      % info("al type: ~p", [{Maj, Min, MoClasses, SpecificProblem, ProbableCause, IsStateful, AdditionalText, Name, EventType, DefaultSeverity}]),
	      
	      MajI = list_to_integer(Maj),
	      MinI = list_to_integer(Min),
	      
	      % TODO, warn against malformed Maj/Min (leading zeros e g)
	      
	      OrderedMoClasses = orderClasses(MoClasses),
	      
	      AlarmTypeD = 
		  orddict:from_list(
		    [{majorType, MajI},
		     {minorType, MinI},
		     {moClasses, OrderedMoClasses},
		     {specificProblem, SpecificProblem},
		     {probableCause, ProbableCause},
		     {isStateful, IsStateful},
		     {additionalText, AdditionalText},
		     {name, Name},
		     {eventType, EventType},
		     {defaultSeverity, DefaultSeverity}] ++
			if ReportFilenames -> [{file, File}]; true -> [] end),
	      
	      AlarmId = {MajI, MinI},
	      
	      try
		  valDictBind(alarmTypes, AlarmId, AlarmTypeD)
	      catch
		  throw:{bad_bind, {alarmTypes, AlarmId, _Old, _New}} when CxpProdId =:= ?RCS_MW_ARM ->
		      err("alarm variants not acceptable within: ~s, alarm: ~p", [CxpProdId, AlarmId]);
		  throw:{bad_bind, {alarmTypes, AlarmId, Old, New}} ->
		      info("    collecting variant info for alarm: major=~w, minor=~w", [MajI, MinI]),
		      OldVariants = varDictGet(alarmVariants, AlarmId, ordsets:new()),
		      New1 = ordsets:add_element(Old, OldVariants),
		      New2 = ordsets:add_element(New, New1),
		      varDictStore(alarmVariants, AlarmId, New2)
	      end,
	      
	      AlarmIdAndClasses = {AlarmId, OrderedMoClasses},
	      case ordsets:is_element(AlarmIdAndClasses, Acc) of
		  false ->
		      ordsets:add_element(AlarmIdAndClasses, Acc);
		  true ->
		      err("repeated alarm declaration ~p in file: ~s", [AlarmIdAndClasses, File]),
		      Acc
	      end   
      end,
      ordsets:new(),
      AlarmTypes),
    ok.

summary() ->
    
    progress("report alarm types that are defined repeatedly (if any)"),
    
    AlarmVariants = varDictGetAll(alarmVariants),
    
    case orddict:is_empty(AlarmVariants) of
	true ->
	    ok;
	false ->
	    Abbreviations = 
		[{specificProblem, sp}, 
		 {eventType, et}, 
		 {probableCause, pc}, 
		 {additionalText, at}, 
		 {name, nm}, 
		 {moClasses, cc}, 
		 {defaultSeverity, ds},
		 {file, f}],
	    
	    AbbreviationsAsString = 
		string:join(
		  [atom_to_list(Y)++" -> "++atom_to_list(X)||{X, Y} <- Abbreviations], 
		  ", "),

	    info("abbreviations: ~s", [AbbreviationsAsString]),
	    orddict:fold(
	      fun({MajorId, MinorId}, Variants, _Acc) ->
		      info("multiple definitions found for alarm type: majorId=~w, minorId=~w, variants:", 
			   [MajorId, MinorId]),
		      Strings = 
			  [pairsToString(
			     abbrevPairs(
			       Abbreviations, 
			       orderPairs(
				 dropMajorMinor(Pairs))))||Pairs <- ordsets:to_list(Variants)],
		      info("  ~p", [Strings]),
		      % No need to check major/minor of course;
		      % no need to check moClasses since allowed to differ
		      checkUniformAttribute(defaultSeverity, Variants, MajorId, MinorId),
		      checkUniformAttribute(specificProblem, Variants, MajorId, MinorId),
		      checkUniformAttribute(eventType, Variants, MajorId, MinorId),
		      checkUniformAttribute(probableCause, Variants, MajorId, MinorId),
		      checkUniformAttribute(additionalText, Variants, MajorId, MinorId),
		      checkUniformAttribute(isStateful, Variants, MajorId, MinorId)
	      end,
	      [],
	      AlarmVariants)
    end.


orderClasses(S) ->
    Tokens = lists:sort(string:tokens(S, ",")),
    string:join(Tokens, ",").




alarmAttributeOrdering() ->
    [{majorType, 1},
     {minorType, 2},
     {specificProblem, 10},
     {eventType, 20},
     {probableCause, 30}, 
     {isStateful, 40},
     {additionalText, 50}, 
     {name, 60},
     {moClasses, 70}, 
     {defaultSeverity, 80},
     {file, 90}].



-spec dropMajorMinor([{atom(), any()}]) -> [{atom(), any()}]. 

dropMajorMinor(Pairs) ->
    lists:foldr(
      fun({minorType, _}, Acc) ->
	      Acc;
	 ({majorType, _}, Acc) ->
	      Acc;
	 ({isStateful, "true"}, Acc) ->
	      Acc;
	 ({defaultSeverity, undefined}, Acc) ->
	      Acc;
	 (P, Acc) ->
	      [P|Acc]
      end,
      [],
      Pairs).


orderPairs(Pairs) ->
    AAO = alarmAttributeOrdering(),
    Tagged = [begin V = proplists:get_value(X, AAO), {{V, X}, Y} end||{X, Y} <- Pairs],
    Sorted = lists:sort(Tagged),
    Result = [{X, Y}||{{_, X}, Y} <- Sorted],
    Result.

abbrevPairs(Abbreviations, Pairs) ->
    lists:foldr(
      fun({X, Y}, Acc) ->
	      R = proplists:get_value(X, Abbreviations, X),
	      [{R, Y}|Acc]
      end,
      [],
      Pairs).


pairsToString(Pairs) ->
    string:join(
      [if 
	   is_list(Any) -> 
	       lists:flatten(io_lib:format("~w=~s", [Atom, Any]));
	   
	   true -> 
	       lists:flatten(io_lib:format("~w=~p", [Atom, Any])) 
       end
       ||{Atom, Any} <- Pairs], " ").


checkUniformAttribute(Attribute, Variants, MajorId, MinorId) ->
    case isUniformAttribute(Attribute, Variants) of
	true ->
	    ok;
	false ->
	    err("inconsistent ~w, major=~w, minor=~w",
		[Attribute, MajorId, MinorId])
    end.


%%% ----------------------------------------------------------
%%% @doc Returns true if all set members have the same
%%% value for the given attribute.
%%% 
%%% Members of the given set are pair-lists.
%%% @end
%%% ----------------------------------------------------------
-spec isUniformAttribute(atom(), ordsets:ordset(list())) -> boolean().

isUniformAttribute(Attribute, Variants) ->
    ValueSet =
	ordsets:fold(
	  fun(PairList, Acc) ->
		  Value = proplists:get_value(Attribute, PairList),
		  ordsets:add_element(Value, Acc)
	  end,
	  ordsets:new(),
	  Variants),
    case ValueSet of
	[_CommonValue] ->
	    true;
	_ -> 
	    false
    end.
