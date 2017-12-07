%%% ----------------------------------------------------------
%%% %CCaseFile:	modump.erl %
%%% Author:	erarafo
%%% Description: Parse NETCONF output and produce a human-friendly
%%% listing.
%%%
%%% Modules used: OTP only.
%%%
%%% ----------------------------------------------------------
-module(modump).
-id('Updated by CCase').
-vsn('/main/R4A/R5A/2').
-date('2016-03-17').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R4A/1      2014-06-01 erarafo     Created
%%% R4A/2      2014-06-24 erarafo     RDN recognition improved
%%% R5A/1      2016-03-15 erarafo     Force recompile
%%% R5A/2      2016-03-17 erarafo     Output to file done properly
%%% ----------------------------------------------------------

-export([modump/1]).

-include_lib("xmerl/include/xmerl.hrl").

-record(exception,
	{key     :: atom(),
	 data    :: any()
	 }).

%%% ----------------------------------------------------------
%%% @doc Parses MO content and dumps it to standard output.
%%% @end
%%% ----------------------------------------------------------
modump([XmlFile, SubtreesSpec, [$=|OutFile]]) ->
    try
	{TopElement, _} = xmerl_scan:file(XmlFile),
	#xmlElement{name=N}=SubElement = getOneSubElement(TopElement, "check for valid reply"),
	if
	    N =:= 'rpc-error' ->
		throw(#exception{key=netconf_error, data={SubtreesSpec}});
	    N =/= 'data' ->
		throw(#exception{key=unspecific_error, data={SubtreesSpec, SubElement}});
	    true ->
		OutStream = 
		    if 
			OutFile =:= "-" ->
			    standard_io;
			true ->
			    case file:open(OutFile, [write]) of
				{error, Reason} ->
				    throw(#exception{key=cannot_write, data={OutFile, Reason}});
				{ok, Stream} ->
				    Stream
			    end
		    end,
		ME = getOneSubElement('ManagedElement', SubElement, "none"),
		io:format("=== end_of_prelude ===~n", []),
		writeTree(OutStream, ME, []),
		
		if
		    OutFile =/= "-" ->
			file:close(OutStream);
		    true ->
			ok
		end
	end
    catch
	throw:#exception{key=netconf_error, data={Spec}} ->
	    io:format(standard_error, "could not dump MO data for: ~s~n",
		      [Spec]),
	    init:stop();
	throw:#exception{key=Key, data=Data} ->
	    Stack = erlang:get_stacktrace(),
	    io:format(standard_error, "caught: ~w,~ndata: ~p,~nstack: ~p~n",
		      [Key, Data, Stack]),
	    init:stop();
	ExType:ExData ->
	    Stack = erlang:get_stacktrace(),
	    io:format(standard_error, "caught: ~p,~nstack: ~p~n",
		      [{ExType, ExData}, Stack]),
	    init:stop()
    end.


%%% ----------------------------------------------------------
%%% @doc Writes the given MO and its children recursively.
%%% @end
%%% ----------------------------------------------------------

writeTree(OutStream,
	  #xmlElement{parents=ParentsInfo, name=Name, content=Content},
	  ParentRdnValues) ->
    Classified = classifyMoContent(Content, Name),
    case lists:keyfind(attribute, 1, Classified) of
	false ->
	    throw(#exception{key=rdn_missing, data={Name, Classified}});
	{_, RdnAttr} ->
	    % it is trusted that the RDN attribute is presented
	    % before all other attributes
	    RdnValue = attributeValue(RdnAttr),
	    Parents = [atom_to_list(P)|| {P, _} <- tl(tl(lists:reverse(ParentsInfo)))],
	    ParentZip = lists:zip(Parents, ParentRdnValues),
	    EcimDn =
		lists:foldl(
		  fun({RdnN, RdnV}, Acc) ->
			  Acc++RdnN++"="++RdnV++","
		  end,
		  "",
		  ParentZip)++atom_to_list(Name)++"="++RdnValue,
	    io:format(OutStream, "~n~s~n", [EcimDn]),
	    TypedAttrs = [U|| {Indicator, _}=U <- Classified, Indicator =/= instance],
	    lists:foreach(
	      fun({attribute, A}) ->
		      io:format(OutStream, "~s.~s: ~s~n", [EcimDn, attributeName(A), attributeValue(A)]);
		 ({structAttribute, #xmlElement{content=Con}=S}) ->
		      StructType = structType(S),
		      io:format(OutStream, "~s.~s: ~s", [EcimDn, attributeName(S), StructType]),
		      lists:foldl(
			fun(#xmlText{}, Separator) ->
				Separator;
			   (Z, Separator) ->
				io:format(OutStream, "~s~s: ~s",
					  [Separator, attributeName(Z), attributeValue(Z)]),
				", "
			end,
			"{",
			Con),
		      io:format(OutStream, "}~n", []);
		 (_) ->
		      ok
	      end,
	      TypedAttrs),
	    [writeTree(OutStream, U, ParentRdnValues++[RdnValue])||{instance, U} <- Classified]
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the subelement with specified name from the given
%%% element; it is expected that there is exactly one.
%%% ----------------------------------------------------------
-spec getOneSubElement(atom(), #xmlElement{}, string()) -> #xmlElement{}.

getOneSubElement(Name, #xmlElement{content=Content}=E, Context) ->
    case [S || #xmlElement{name=N}=S <- Content, N =:= Name] of
	[Result] ->
	    Result;
	_ ->
	    throw(#exception{key=nof_elements, data={Context, Name, E}})
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the subelement from the given element;
%%% it is expected that there is exactly one.
%%% ----------------------------------------------------------
-spec getOneSubElement(#xmlElement{}, string()) -> #xmlElement{}.

getOneSubElement(#xmlElement{content=Content}=E, Context) ->
    case [S || #xmlElement{}=S <- Content] of
	[Result] ->
	    Result;
	_ ->
	    throw(#exception{key=nof_elements, data={Context, E}})
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the XML attributes of the given XML element.
%%% @end
%%% ----------------------------------------------------------
-spec attributes(#xmlElement{}) -> [#xmlAttribute{}].

attributes(#xmlElement{attributes=Attributes}) ->
    lists:filter(
      fun(#xmlAttribute{}) ->
	      true;
	 (_) ->
	      false
      end,
      Attributes).


%%% ----------------------------------------------------------
%%% @doc Returns true if the given XML element is an attribute.
%%% @end
%%% ----------------------------------------------------------
-spec isAttribute(#xmlElement{}) -> boolean().

isAttribute(#xmlElement{content=[]}) ->
    % the case of an attribute with no value
    true;

isAttribute(#xmlElement{content=[#xmlText{}]}) ->
    % content is a single #xmlText{} element,
    % assume this is a scalar attribute
    true;

isAttribute(#xmlElement{content=Items}) ->
    % accept also the case with multiple
    % #xmlText{} elements, which may appear
    % if the text contains certain strong
    % characters
    lists:all(
      fun(#xmlText{}) -> 
	      true;
	 (_) -> 
	      false 
      end,
      Items);

isAttribute(_) ->
    false.


%%% ----------------------------------------------------------
%%% @doc Returns true if the given XML element is a
%%% struct attribute.
%%% @end
%%% ----------------------------------------------------------
-spec isStructAttribute(#xmlElement{}) -> boolean().

isStructAttribute(#xmlElement{}=Element) ->
    Attrs = attributes(Element),
    case lists:keyfind(struct, #xmlAttribute.name, Attrs) of
	false ->
	    false;
	_ ->
	    true
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the struct type (struct class name) of a
%%% struct attribute value.
%%% @end
%%% ----------------------------------------------------------
-spec structType(#xmlElement{}) -> string().

structType(#xmlElement{}=Element) ->
    Attrs = attributes(Element),
    #xmlAttribute{value=Value} = lists:keyfind(struct, #xmlAttribute.name, Attrs),
    lists:flatten(Value).


%%% ----------------------------------------------------------
%%% @doc Returns the name of an attribute.
%%% @end
%%% ----------------------------------------------------------
-spec attributeName(#xmlElement{}) -> string().

attributeName(#xmlElement{name=Name}) ->
    atom_to_list(Name).


%%% ----------------------------------------------------------
%%% @doc Returns the value of a scalar attribute or a
%%% struct member.
%%% @end
%%% ----------------------------------------------------------
-spec attributeValue(#xmlElement{}) -> string().

attributeValue(#xmlElement{content=[]}) ->
    "<no value>";

attributeValue(#xmlElement{content=[#xmlText{value=Value}]}) ->
    lists:flatten(Value);

attributeValue(#xmlElement{content=Texts}=E) ->
    lists:foldr(
      fun(#xmlText{value=Value}, Acc) ->
	      lists:flatten(Value)++Acc;
	 (_Other, _Acc) ->
	      throw({unexpected_element, E})
      end,
      "",
      Texts).


%%% ----------------------------------------------------------
%%% @doc Classifies the XML elements in the given list.
%%% An element is considered an instance if it is not
%%% an attribute.
%%%
%%% The first of the attributes in the given content is
%%% believed to be an RDN attribute and it will be kept
%%% before the others regardless of its name.
%%% ----------------------------------------------------------
-spec classifyMoContent(list(), atom()) ->
	  [{attribute|structAttribute|instance, string(), #xmlElement{}}].

classifyMoContent(Content, NameA) ->
    NameS = atom_to_list(NameA),
    {KeyListReversed, FoundRdn} =
	lists:foldl(
	  fun(#xmlElement{}=E, {Acc, FR}) ->
		  case isAttribute(E) of
		      true ->
			  AttrName = attributeName(E),
			  case re:run(AttrName, "^(.)(.*)Id$", []) of
			      nomatch ->
				  {[{attribute, "2"++AttrName, E}|Acc], FR};
			      {match, [_, {0, 1}, {1, M}]} ->
				  ClassNamePartHead = string:substr(AttrName, 1, 1),
				  ClassNamePartTail = string:substr(AttrName, 2, M),
				  case string:to_upper(ClassNamePartHead)++ClassNamePartTail of
				      U when U =:= NameS ->
					  % well-formed RDN attribute found
					  {[{attribute, "0"++AttrName, E}|Acc], true};
				      _ ->
					  {[{attribute, "2"++AttrName, E}|Acc], FR}
				  end
			  end;
		      _ ->
			  case isStructAttribute(E) of
			      true ->
				  SortKey = "3"++attributeName(E),
				  {[{structAttribute, SortKey, E}|Acc], FR};
			      _ ->
				  SortKey = "4"++instanceRdn(E),
				  {[{instance, SortKey, E}|Acc], FR}
			  end
		  end;
	     (_, Acc) ->
		  Acc
	  end,
	  {[], false},
	  Content),
    
    KeyList = lists:reverse(KeyListReversed),
    
    if
	not(FoundRdn) ->
	    % If a well-formed RDN was not found then hope that
	    % the first element is an attribute and give it a
	    % lower sort key than everything else.
	    KeyListFixed =
		case KeyList of
		    [{attribute, [_|T], E}|Tail] ->
			[{attribute, [$0|T], E}|Tail]  ;
		    Other ->
			Other
		end,
	    SortedList = lists:keysort(2, KeyListFixed),
	    [{A, C} || {A, _, C} <- SortedList];
	FoundRdn ->
	    SortedList = lists:keysort(2, KeyList),
	    [{A, C} || {A, _, C} <- SortedList]
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the instance RDN of the given MO instance.
%%% @end
%%% ----------------------------------------------------------
-spec instanceRdn(#xmlElement{}) -> string().

instanceRdn(#xmlElement{content=Content}=X) ->
    Rdn =
	lists:foldl(
	  fun(_, #xmlElement{}=Acc) ->
		  Acc;
	     (#xmlElement{}=E, false) ->
		  case isAttribute(E) of
		      false ->
			  false;
		      true ->
			  E
		  end;
	     (_, false) ->
		  false
	  end,
	  false,
	  Content),
    case Rdn of
	false ->
	    throw(#exception{key=instance_has_no_rdn, data={X}});
	_ ->
	    attributeName(Rdn)++"="++attributeValue(Rdn)
    end.
