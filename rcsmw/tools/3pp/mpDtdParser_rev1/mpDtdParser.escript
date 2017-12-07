#!/usr/bin/env escript
%% -*- erlang -*-

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	mpDtdParser.escript %
%%% Author:	etxjotj
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------

-module(mpDtdParser).
-vsn('/main/24'). 
-mode(compile).
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
%%% -----      ---------- --------    ------------------------
%%% main/9     2012-07-18 etxjotj     Fixed a problem with subtyping
%%% main/13    2013-04-03 etxjotj     Fixed problem with struct names
%%% main/16    2013-05-22 etxjotj     Fixed a problem with DX ET 2.6
%%% main/17    2013-05-22 etxjotj     Further parser problems
%%% main/24    2015-10-08 etxjotj     Fixed a problem with default values
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% Command line function
-include_lib("xmerl/include/xmerl.hrl").
-export([main/1]).

-export([upper_case/1]). %% Only to aviod compiler printouts
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% Usage
%%% mpDtdParser.escript -I IncDir -out Outdir -vsn VersionTag

main(Args) ->
    main(Args, []).

main(["-debug"|Args], Opts) ->
    put(debug, true),
    main(Args, Opts);
main(["-"++_, Flag="-"++_ |Args], Opts) ->
    %% Remove empty arg
    main([Flag|Args], Opts);
main(["-I", IncDir|Args], Opts) ->
    IncDirs = proplists:get_value(inc, Opts, [IncDir]),
    main(Args, [{inc, IncDirs}|Opts]);
main(["-out", OutDir|Args], Opts) ->
    main(Args, [{out, OutDir}|Opts]);
main(["-vsn", Vsn|Args], Opts) ->
    main(Args, [{vsn, Vsn}|Opts]);
main(Paths, Opts) ->
    start([{paths, Paths}|Opts]).

start(Opts) ->
    %% Get a hold of your self
    Paths = proplists:get_value(paths, Opts),
    Out = proplists:get_value(out, Opts,
			      begin {ok, Cwd} = file:get_cwd(), Cwd end),
    OutDir = normalize_path(Out),
    Includes = proplists:get_value(inc, Opts, []),
    IncDirs = [normalize_path(IncDir)||IncDir<-Includes],
    Vsn = proplists:get_value(vsn, Opts, os:getenv("USER")++
				  " does not include -vsn"),
    [do_file(Path, Vsn, OutDir, IncDirs)||Path<-Paths],
    ok.

do_file(Src, Vsn, OutDir, IncDirs) ->
    SrcPath = normalize_path(Src),
    case filename:extension(SrcPath) of
	".xml" ->
	    continue_do_file(SrcPath, Vsn, OutDir, IncDirs);
	_ ->
	    error_logger:error_msg("Unknown file type: ~p~n",[SrcPath])
    end.

continue_do_file(SrcPath, Vsn, OutDir, IncDirs) ->

    %% Read xml file
    {TopE, _ } =
	case xmerl_scan:file(SrcPath, [{validation, off},
				       {fetch_path, IncDirs}]) of
	    {error, Reason} -> erlang:error(Reason, [SrcPath, OutDir, IncDirs]);
	    Result -> Result
	end,
    MimE = find_element(mim, TopE),

    %% Confirm existence of noteworthy things like classes, enums, structs and
    %% relationships before initiating a file

    ClassEs = find_classes(MimE),
    EnumEs = find_enums(MimE),
    StructEs = find_structs(MimE),
    %% If there are no data attributes - don't make a record
    RecClasses =
	[ClassE||ClassE<-ClassEs,
		 begin
		     {_, _, OtherAttributes } = get_class_attributes(ClassE),
		     case OtherAttributes of
			 [] -> false;
			 _ -> true
		     end
		 end],
    case {RecClasses, EnumEs, StructEs} of
	{[], [], []} -> ok;
	{[], _, _} ->
	    Mim = get_mim_data(MimE, Vsn),
	    put(local_mim_name, element(1, Mim)),
	    {ok, Fd} = initiate_header_file(SrcPath, OutDir, Mim),
	    [compile_enum(Fd, EnumE)||EnumE<-EnumEs],
	    [compile_struct(Fd, StructE)||StructE<-StructEs],
	    file:close(Fd);
	_ ->
	    Mim = get_mim_data(MimE, Vsn),
	    put(local_mim_name, element(1, Mim)),
	    {ok, Fd} = initiate_header_file(SrcPath, OutDir, Mim),
	    [compile_class(Fd, ClassE)||ClassE<-ClassEs],
	    [compile_enum(Fd, EnumE)||EnumE<-EnumEs],
	    [compile_struct(Fd, StructE)||StructE<-StructEs],
	    file:close(Fd)
    end,
    ok.

get_mim_data(MimE, Vsn) ->
    ModelVersion = find_attribute(version, MimE)++"."++
	find_attribute(release, MimE)++"."++
	find_attribute(correction, MimE),
    {find_attribute(name, MimE), ModelVersion, Vsn}.


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

initiate_header_file(Path, OutDir, Mim) ->
    Basename = filename:basename(Path, ".xml"),
    HrlFile = Basename++".hrl",

    %% CompileTime = proplists:get_value(time, ?MODULE:module_info(compile),
    %% 				      failed_to_get_compile_time),
    {ok, Fd} = file:open(filename:join(OutDir, HrlFile), [write, unicode]),

    %% Write header

    %% Don't add compile time to header. It confuses clearcase.
    io:format(Fd,
	      "%%% --------------------------------------------------------~n",
	      []),
    Year = element(1, element(1, calendar:local_time())),
    io:format(
      Fd,
      "%%% Copyright (c) Ericsson AB ~w All rights reserved.~n"
      "%%%~n"
      "%%% The information in this document is the property of Ericsson.~n"
      "%%%~n"
      "%%% Except as specifically authorized in writing by Ericsson, the~n"
      "%%% receiver of this document shall keep the information contained~n"
      "%%% herein confidential and shall protect the same in whole or in~n"
      "%%% part from disclosure and dissemination to third parties.~n"
      "%%%~n"
      "%%% Disclosure and disseminations to the receivers employees shall~n"
      "%%% only be made on a strict need to know basis.~n",[Year]),


    io:format(Fd,
	      "%%% --------------------------------------------------------~n",
	      []),
    io:format(Fd, "~n", []),
    io:format(Fd, "-hrl_id(~p).~n",[Mim]),
%    io:format(Fd, "-hrl_vsn('~s').~n",[Vsn]),
    io:format(Fd, "~n", []),
    {ok, Fd}.

%%% We assume that class and enum names are globally unique


compile_class(Fd, ClassE) ->
    debug("class: ~p~n",[find_attribute(name, ClassE)]),
    ClassName = case find_attribute(name, ClassE) of
		    "Schema" -> "SysMSchema"; % Special fix for SysM
		    CN -> CN
		end,
    Description = find_text_content(description, ClassE),
    {Attributes, Keys, _} = get_class_attributes(ClassE),

    io:format(Fd, "~n%% -------------- CLASS ~ts -------------------------~n",
	      [ClassName]),

    %% Print description
    case Description of
	undefined ->
	    ok;
	_ ->
	    Lines = string:tokens(Description, "\n"),
	    io:format(Fd, "~n%% Description:~n",[]),
	    [io:format(Fd, "%% ~ts~n",[Line])||Line<-Lines],
	    io:format(Fd, "~n",[])
    end,

    %% Write record

    case length(Keys) of
	0 -> ok;
	1 -> ok;
	_ ->
	    io:format(Fd, "%%% Multidimensional key:~n",[]),
	    [io:format(Fd, "%%%  ~ts~n",[find_attribute(name,Key)])||Key<-Keys],
	    io:format(Fd, "~n",[])
    end,

    IndentStr = [$ ||_<-lists:seq(1, length(ClassName)+11)],
    io:format(Fd, "-record(~ts, {", [lower_case(ClassName)]),

    %% Design rule

    %% Singleton objects that have parameter automatically gets an
    %% index field called "id". That field will not be known in the
    %% model, so it will not be shown in netconf/cli, but it is
    %% necessary in order to differentiate between different instances
    %% of the parent object

    IsSingleton =
	case lists:keysearch(singleton,
			     #xmlElement.name,
			     ClassE#xmlElement.content) of
	    {value, _} ->
		true;
	    false ->
		false
	end,
    if IsSingleton ->
	    io:format(Fd, "id,~n~ts", [IndentStr]);
       true ->
	    ok
    end,
    io:format(Fd, "~ts",[fnuttify(find_attribute(name, hd(Attributes)))]),
    [io:format(Fd, ",~n~ts~ts",[IndentStr, fnuttify(find_attribute(name, AttributeE))])||
	AttributeE<-tl(Attributes)],
    %% Objects with only one attribute gets a dummy value because mnesia
    %% needs it.
    case Attributes of
	[_] when IsSingleton ->
	    ok;
	[_] ->
	    io:format(Fd, ",~n~tsdummy",[IndentStr]);
	_ ->
	    ok
    end,
    io:format(Fd, "}).~n",[]),

    io:format(Fd, "~n",[]),

    %% Write datatypes

    io:format(Fd, "-define(~ts_types,~n        [",[lower_case(ClassName)]),

    IndentStr2 = [$ ||_<-lists:seq(1, 9)],
    if IsSingleton ->
	    io:format(Fd, "{id, term},~n~ts{",[IndentStr2]);
       true ->
	    io:format(Fd, "{", [])
    end,
    FirstName = fnuttify(find_attribute(name, hd(Attributes))),
    debug("attribute: ~p~n",[FirstName]),
    FirstDataType = find_datatype(hd(Attributes)),
    io:format(Fd, "~ts, ~w",
	      [FirstName,
	       format_datatype(FirstDataType)]),
    FirstDataTypeE = find_element(dataType, hd(Attributes)),
    DefaultValues =
	[{FirstName, find_default_value(FirstDataTypeE)}|
	 [begin
	      ThisName = fnuttify(find_attribute(name, AttributeE)),
  	      debug("attribute: ~p~n",[ThisName]),
	      ThisDataType = find_datatype(AttributeE),

	      io:format(Fd, "},~n~ts{~ts, ~w",
			[IndentStr2,
			 ThisName,
			 format_datatype(ThisDataType)]),
	      ThisDataTypeE = find_element(dataType, AttributeE),
	      {ThisName, find_default_value(ThisDataTypeE)}
	  end||
	     AttributeE<-tl(Attributes)]],
    case Attributes of
	[_] when IsSingleton ->
	    ok;
	[_] ->
	    io:format(Fd, "},~n~ts{dummy, atom", [IndentStr2]);
	_ ->
	    ok
    end,
    io:format(Fd, "}]).~n",[]),
    io:format(Fd, "~n",[]),


    %% Write default values

    [io:format(Fd, "-define(~ts_~ts_default, ~ts).~n",
	       [lower_case(ClassName), Name, format_default_value(Value)])||
 	{Name, Value} <- DefaultValues,
	Value /= undefined],


    %% Write restricted
    RestrictedAttributes = [AttributeE||AttributeE<-Attributes,
					is_restricted(AttributeE)],
    case RestrictedAttributes of
	[] ->
	    ok;
	[AttributeE] ->
	    io:format(Fd, "-define(~ts_restricted, [~ts]).~n~n",
		      [ClassName, fnuttify(find_attribute(name, AttributeE))]);
	_ ->
	    io:format(Fd,  "-define(~ts_restricted,~n        [~ts",
		      [ClassName,
		       find_attribute(name, hd(RestrictedAttributes))]),
	    [io:format(Fd,",~n         ~ts",[fnuttify(find_attribute(name,AttributeE))])||
		AttributeE<-tl(RestrictedAttributes)],
	    io:format(Fd, "]).~n~n", [])
    end.


get_class_attributes(ClassE) ->
    {Keys, OtherAttributes} =
	lists:foldl(
	  fun(AttributeE, {K,A}) when AttributeE#xmlElement.name==attribute ->
		  case is_key(AttributeE) of
		      true ->
			  {K++[AttributeE],A};
		      false ->
			  {K,A++[AttributeE]}
		  end;
	     (_, {K,A}) ->
		  {K,A}
	  end, {[],[]}, ClassE#xmlElement.content),

    Attributes =
	case length(Keys) of
	    0 ->
		OtherAttributes;
	    1 ->
		Keys++OtherAttributes;
	    _  ->
		KeyAttr = fake_key(),
		[KeyAttr|OtherAttributes]
	end,
    {Attributes, Keys, OtherAttributes}.

find_datatype(AttributeE) ->
    DataTypeE = find_element(dataType, AttributeE),
    try find_element(sequence, DataTypeE) of
	SequenceE ->
	    {sequence, find_subtypes(SequenceE)}
    catch _:_ ->
	    find_subtypes(DataTypeE)
    end.

find_subtypes(DataTypeE) ->
    find_subtypes(DataTypeE, [enumRef, derivedDataTypeRef, structRef]).

find_subtypes(DataTypeE, [RefType|RefTypes]) ->
   %% io:format("find_subtypes(~p, ~p)~n",[DataTypeE,  [RefType|RefTypes]]),
    try find_element(RefType, DataTypeE) of
	RefE ->
	    ReferencedType = find_attribute(name, RefE),
	    case RefType of
		structRef -> {RefType, ReferencedType};
		enumRef ->
		    try find_element(mimName, RefE) of
			MimNameE ->
			    MimName = find_text(MimNameE),
			    {enumRef, MimName++"."++ReferencedType}
		    catch _:_ ->
			    info_msg("Could not find a mimName element "
				     "for ~s. Referring to this mim.~n",
				     [ReferencedType]),
			    {enumRef, get(local_mim_name)++"."++ReferencedType}
		    end;

		_ -> 
		    MimNameE = find_element(mimName, RefE),
		    MimName = find_text(MimNameE),
		    {RefType, MimName++"."++ReferencedType}
	    end
    catch _:_ ->
	    find_subtypes(DataTypeE, RefTypes)
    end;
	     
find_subtypes(DataTypeE, []) ->
    Element = find_first_element(DataTypeE#xmlElement.content),
    {element, Element}.


format_datatype({sequence, {element, SequenceE}}) when SequenceE#xmlElement.name == sequence ->
    Element = find_first_element(SequenceE#xmlElement.content),
    {sequence, format_datatype({element, Element})};
format_datatype({sequence, DataType}) ->
    {sequence, format_datatype(DataType)};

format_datatype({element, Element}) -> Element#xmlElement.name;
format_datatype({derivedDataTypeRef, NameRef}) -> list_to_atom(NameRef);
format_datatype({enumRef, NameRef}) -> list_to_atom(NameRef);
format_datatype({struct, NameRef}) -> {struct, list_to_atom(NameRef)};
format_datatype({structRef, NameRef}) -> {struct, list_to_atom(NameRef)};
format_datatype({moRef, _}) -> moRef.


basic_find_default_value(Element) ->
    try find_element(defaultValue, Element) of
	DefaultValueE ->
	    case DefaultValueE#xmlElement.content of
		[TextE] -> TextE#xmlText.value;
		_ -> "\"\""
	    end
    catch _:_ -> 
	    undefined
    end.    
   

find_default_value(DerivedDataTypeE) 
  when DerivedDataTypeE#xmlElement.name == derivedDataType ->
    basic_find_default_value(DerivedDataTypeE);
find_default_value(StructRefE) 
  when StructRefE#xmlElement.name == structRef ->
    %% Structs cannot have default values. Only struct members have that
    undefined;
find_default_value(EnumRefE) 
  when EnumRefE#xmlElement.name == enumRef ->
    basic_find_default_value(EnumRefE);
find_default_value(SequenceE)
  when SequenceE#xmlElement.name == sequence ->
    try find_element(seqDefaultValue, SequenceE) of
	SeqDefaultValue ->
	    find_default_value(SeqDefaultValue)
    catch _:_ ->
	    %% We assume that the basic doesn't have default values
	    undefined
    end;
find_default_value(SeqDefaultValueE) 
  when SeqDefaultValueE#xmlElement.name == seqDefaultValue ->
    DefaultValues = 
	[case DefaultValueE#xmlElement.content of
	     [TextE] -> TextE#xmlText.value;
	     _ -> "\"\""
	 end||DefaultValueE<-SeqDefaultValueE#xmlElement.content,
	      DefaultValueE#xmlElement.name == defaultValue],
    {sequence, DefaultValues};
%%% Primitive types, structRef, derivedDataTypeRef or a sequence
find_default_value(Element) ->
    case basic_find_default_value(Element) of
	undefined ->
	    try find_first_element(Element) of
		FirstElement ->
		    find_default_value(FirstElement)
	    catch _:_ ->
		    undefined
	    end;
	DefaultValue ->
	    DefaultValue
    end.

format_default_value(Value=[H|_]) when H >= $A, H=<$Z ->
    [$'|Value]++[$'];
format_default_value({sequence, Value}) -> Value;
format_default_value(Value) -> Value.

%%% This function is used to find the first content that is an xml element
%%% when the name of that element is unknown

find_first_element(Element) when is_record(Element, xmlElement) ->
    find_first_element(Element#xmlElement.content);

find_first_element([Element|_]) when is_record(Element, xmlElement) ->
    Element;
find_first_element([_|Elements]) ->
    find_first_element(Elements);
find_first_element([]) -> erlang:error(no_element, []).


%%% Convert a string abcdEfghIjkl to ABCD_EFGH_IJKL

define([H|Tail]) when H>= $A, H =< $Z ->
    [$_,H|define(Tail)];
define([H|Tail]) when H>= $a, H =< $z ->
    [H-$a+$A|define(Tail)];
define([H|Tail]) ->
    [H|define(Tail)];
define([]) -> [].

%%% *************************** STRUCTS ***********************

compile_struct(Fd, StructE) ->
    StructName = find_attribute(name, StructE),
    Members = [Member||Member<-StructE#xmlElement.content,
		       Member#xmlElement.name==structMember],
    format_struct(Fd, StructName, Members).

format_struct(Fd, StructName, Members) ->

    debug("format_struct ~p~n",[StructName]),
    %% Write record

    RecordName = StructName, %% Dummy assignment for clarity
    IndentStr = [$ ||_<-lists:seq(1, length(RecordName)+13)],
    Define = define(RecordName),
    io:format(Fd, "~n%% ------------------ STRUCT ~ts ----------------------~n",[StructName]),
    io:format(Fd, "-ifndef(~ts).~n",[Define]),
    io:format(Fd, "-define(~ts, 1).~n~n",[Define]),
    io:format(Fd, "-record('~ts', {", [RecordName]),
    io:format(Fd, "~ts",[find_attribute(name, hd(Members))]),
    [io:format(Fd, ",~n~ts~ts",[IndentStr, find_attribute(name, MemberE)])||
	MemberE<-tl(Members)],
    io:format(Fd, "}).~n",[]),
    io:format(Fd, "~n",[]),

    %% Write datatypes

    io:format(Fd, "-define('~ts',~n        [",[RecordName++"_types"]),
%%%    io:format("~p~n",[hd(Members)]),
    FirstName = find_attribute(name, hd(Members)),
    FirstDataType = find_struct_datatype(hd(Members)),
%%%    io:format("~ts, ~w ~p~n",[FirstName, format_datatype(FirstDataType), FirstDataType]),
    io:format(Fd, "{~ts, ~w", [FirstName,
			       format_datatype(FirstDataType)]),
    IndentStr2 = [$ ||_<-lists:seq(1, 9)],
    DefaultValues =
	[{FirstName, find_default_value(FirstDataType)}|
	 [begin
	      ThisName = find_attribute(name, MemberE),
	      ThisDataType = find_struct_datatype(MemberE),
	      io:format(Fd, "},~n~ts{~ts, ~w",
			[IndentStr2,
			 ThisName,
			 format_datatype(ThisDataType)]),
	      {ThisName, find_default_value(ThisDataType)}
	  end||
	     MemberE<-tl(Members)]],
    io:format(Fd, "}]).~n",[]),
    io:format(Fd, "~n",[]),

    %% Write default values

    [io:format(Fd, "-define(~ts_~ts_default, ~ts).~n",[lower_case(RecordName),
						       Name,Value])||
 	{Name, Value} <- DefaultValues,
	Value /= undefined],

    io:format(Fd, "~n-endif. % ~ts~n",[Define]),
    io:format(Fd, "~n",[]).


find_struct_datatype(StructMemberE) ->
%    io:format("find_struct_datatype(~p)~n",[StructMemberE]),
    Types = availableTypes(),
    Elements = [Element||Element<-StructMemberE#xmlElement.content,
			 is_record(Element, xmlElement) andalso
			 lists:member(Element#xmlElement.name, Types)],
    case Elements of
	[TypeE] ->
	    case TypeE#xmlElement.name of
		derivedDataTypeRef ->
		    Name = find_attribute(name, TypeE),
		    try find_element(mimName, TypeE) of
			MimNameE ->
			    MimName = find_text(MimNameE),
			    {derivedDataTypeRef, MimName++"."++Name}
		    catch _:_ ->
			    info_msg("Could not find a mimName element "
				     "for ~s. Referring to this mim.~n",[Name]),
			    
			    {derivedDataTypeRef, get(local_mim_name)++"."++Name}
		    end;
		sequence ->
		    SeqTypes = [structRef, derivedDataTypeRef|primitiveTypes()],
		    SeqType = find_sequence_type(TypeE, SeqTypes),
		    {sequence, SeqType};
		enumRef ->
		    Name = find_attribute(name, TypeE),
		    try find_element(mimName, TypeE) of
			MimNameE ->
			    MimName = find_text(MimNameE),
			    {enumRef, MimName++"."++Name}
		    catch _:_ ->
			    info_msg("Could not find a mimName element "
				     "for ~s. Referring to this mim.~n",[Name]),
			    {enumRef, get(local_mim_name)++"."++Name}
		    end;
		moRef ->
		    {element, TypeE};
		_ ->
		    {element, TypeE}
	    end;
	_ ->
	    erlang:error(multiple_typedefs_in_structmember, [StructMemberE])
    end.

find_sequence_type(SequenceE, [structRef|SeqTypes]) ->
    case catch find_element(structRef, SequenceE) of
	{'EXIT', _ } ->
	    find_sequence_type(SequenceE, SeqTypes);
	StructRefE ->
	    RefName = find_attribute(name, StructRefE),
	    {struct, RefName}
    end;
find_sequence_type(SequenceE, [SeqType|SeqTypes]) ->
    case catch find_element(SeqType, SequenceE) of
	{'EXIT', _ } ->
	    find_sequence_type(SequenceE, SeqTypes);
 	TypeE->
	    {element, TypeE}
    end;
find_sequence_type(SE, []) ->
    erlang:error(could_not_find_sequence_type, [SE, []]).



%%% *************************** TYPES ***********************
%%% Definitions from mp.dtd

availableTypes() ->
    primitiveTypes() ++ [structRef, derivedDataTypeRef, sequence].

primitiveTypes() ->
    corbaTypes() ++ yangTypes() ++ [enumRef, moRef].

yangTypes() ->
    [int8, int16, int32, int64, uint8, uint16, uint32, uint64].

corbaTypes() ->
    [boolean, octet, char, double, float, long, longlong, short, string, wstring].

%%% *************************** ENUMS ***********************


compile_enum(Fd, EnumE) ->

    EnumName = find_attribute(name, EnumE),
    debug("compile_enum ~p~n",[EnumName]),
    MemberEs = [E||E<-EnumE#xmlElement.content,
		   element(#xmlElement.name, E)==enumMember],
    io:format(Fd, "~n%% ------------------ ENUM ~ts ----------------------~n",
	      [EnumName]),

    io:format(Fd, "-ifndef('~ts').~n",[EnumName]),
    io:format(Fd, "-define('~ts', 1).~n~n",[EnumName]),
    [format_enum_member(Fd, EnumName, MemberE)||MemberE<-MemberEs],
    io:format(Fd, "~n",[]),
    io:format(Fd, "-endif. % ~ts~n",[EnumName]),
    ok.

format_enum_member(Fd, EnumName, MemberE) ->
    MemberName = find_attribute(name, MemberE),
    Value = find_text_content(value, MemberE),
    io:format(Fd, "-define(~ts_~ts, ~ts).~n",[EnumName, MemberName, Value]),
    ok.

find_text_content(Name, Element) ->
    case catch find_element(Name, Element) of
	{'EXIT', _} ->
	    undefined;
	E ->
	    [TextE|_] = E#xmlElement.content,
	    TextE#xmlText.value
    end.



normalize_path(Path) ->
    case hd(Path) of
	$/ ->
	    Path;
	$~ ->
	    filename:join(os:getenv("HOME"), Path);
	_ ->
	    {ok, Cwd} = file:get_cwd(),
	    filename:join(Cwd, Path)
    end.

find_classes(MimE) ->
    [Element||Element<-MimE#xmlElement.content,
	      element(#xmlElement.name, Element)==class].

find_enums(MimE) ->
    [Element||Element<-MimE#xmlElement.content,
	      element(#xmlElement.name, Element)==enum].

find_structs(MimE) ->
    [Element||Element<-MimE#xmlElement.content,
	      element(#xmlElement.name, Element)==struct].

is_restricted(AttributeE) ->
    case catch find_element(restricted, AttributeE) of
	{'EXIT', _} -> false;
	_ -> true
    end.

is_key(AttributeE) ->
    case catch find_element(key, AttributeE) of
	{'EXIT', _} ->
	    false;
	_ ->
	    true
    end.

%%% Description: Construct the following fake xml
%%%  <attribute name="key">
%%%    <dataType>
%%%       <string/>
%%%    </dataType>
%%%  </attribute>

fake_key() ->
    DataType = #xmlElement{name=dataType,
			   content=[#xmlElement{name=string}]},
    #xmlElement{name=attribute,
		attributes= [#xmlAttribute{name=name,
					   value="key"}],
		content= [DataType]}.

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

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    debug("find_element(~p, ~p)~n",
	  [ElementName, [case element(1,X) of
			     xmlElement -> 
				 element(#xmlElement.name,X);
			     E -> E
			 end||X<-ContentList]]),
    {value, Element} =
	lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

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

fnuttify(String) ->
    case hd(String) of
	H when H >= $A, H =< $Z ->
	    [$'|String]++[$'];
	_ ->
	    String
    end.


lower_case(String) ->
    case is_all_capital(String) of
	true ->
	    [X-$A+$a||X<-String];
	false when hd(String) >= $A, hd(String) =< $Z ->
	    [hd(String)-$A+$a|tl(String)];
	_ ->
	    String
    end.

is_all_capital(String) ->
    lists:foldl(fun capital/2, true, String).

capital(X, true) when X >= $A, X =< $Z -> true;
capital(_, _) -> false.

upper_case([H|T]) when H >= $a, H =< $z ->
    [H-$a+$A|T];
upper_case(String) -> String.


info_msg(Format, Args) ->
    error_logger:info_msg("~w: "++Format, [?MODULE|Args]).

debug(Format, Args) ->
    case get(debug) of
	true ->
	    io:format(Format, Args);
	_ ->
	    ok
    end.
