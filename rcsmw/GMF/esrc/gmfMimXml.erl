%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfMimXml.erl %
%%% Author:
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfMimXml).
-vsn('/main/R1A/R2A/R3A/R9A/R10A/R11A/R12A/1').
-date('2017-11-14').
-author('elarrun').
-shaid('b1b28fdbb2ecc670970fb0bf1c770786130b6ca0').
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
%%% R9A/1      2017-03-27 etxpeno     Support for uniDirectionalAssociation
%%% R11A/1     2017-08-15 etxpeno     improve handling of parent-child relations
%%% ----------------------------------------------------------
%%%
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([%%test/0,
	 parse_mim/3,
	 %% Callback to SAX Parser
	 parse/3]).

-export([get_all_mom_info/1,
	 get_domain_extension/2,
         get_all_classes/2,
         get_all_relations/1,
	 get_all_structs/2,
	 get_all_derivedtypes/2,
	 get_all_bidirectionalassociations/2]).

%%% NOTE: NO ERROR HANDLING HAS BEEN IMPLEMENTED YET.
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("gmf.hrl").

-include_lib("xmerl/include/xmerl.hrl").



-record(gmfMX, {cxpProdId,
		cxpRev,
		file,
		mim,
		type,
		tag, %% class, struct, mim, interMim are only tags which are handled at this level
		prev,
		ignore = false,
		table,
		data = []}).
%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%% Same code:: Keeping it separate for the time being.
%% test() ->
%%     Tab = ets:new(mim, [public, {keypos, 2},ordered_set]),
%%     File = "TESTMOM_mp.xml",
%%     ParseOps = [skip_external_dtd,
%% 		{event_fun, fun parse/3},
%% 		{event_state, #gmfMX{file = File,
%% 				     table = Tab,
%% 				     cxpProdId = "XTE",
%% 				     cxpRev = "R2A"}}],
%%     po(xmerl_sax_parser:file(File, ParseOps)).

parse_mim(CxpProdId, CxpRev, File) ->
    Tab = ets:new(mim, [public, {keypos, 2},ordered_set]),
    ParseOps = [skip_external_dtd,
		{event_fun, fun parse/3},
		{event_state, #gmfMX{file = File,
				     table = Tab,
				     cxpProdId = CxpProdId,
				     cxpRev = CxpRev}}],
    po(xmerl_sax_parser:file(File, ParseOps)).


po({ok, #gmfMX{table = T} = _X, _}) ->
    {ok, T}.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

parse(Event, _LineNo, State) ->
    parse(Event, State).

%% The parsing is simplified to handle only what is needed.
%% For example object attributes are ignored.
%% The file are handled separately for classes and objects.
%% If both exist in the same, then GO TO HELL

parse({ignorableWhitespace, _}, State) ->
    State;
parse({startElement, _, "description", _, _}, State) ->
    State#gmfMX{ignore = true};
parse({endElement, _, "description", _},
      State = #gmfMX{ignore = true}) ->
    State#gmfMX{ignore = false};
parse(_, State = #gmfMX{ignore = true}) ->
    State;
parse({startElement, _, Element, _, _},
      State) when Element == "models";
		  Element == "dtdVersion";
		  Element == "interMim";
		  Element == "mib" ->
    State;
parse({endElement, _, Element, _},
      State) when Element == "models";
		  Element == "dtdVersion";
		  Element == "interMim";
		  Element == "mib" ->
    State;

parse({startElement, _, "mim", _, Attrs}, State) ->
    As = attrs(Attrs),
    Name = proplists:get_value(name, As),
    State#gmfMX{mim = {Name, As}};
parse({endElement, _, "mim", _}, %%_) ->
      #gmfMX{file  = File,
	     table = T,
	     cxpProdId = CP,
	     cxpRev    = CR,
	     mim   = {Mim, As}}) ->
    ets:insert(T, {mim,
		   {Mim, CP, CR},
		   As}),
    %%io:format("Mim ~p~n", [Mim]),
    #gmfMX{file      = File,
	   cxpProdId = CP,
	   cxpRev    = CR,
	   table     = T};

parse({startElement, _, Element, _, Attrs},
      #gmfMX{data = Data} = State) ->
    As = attrs(Attrs),
    State#gmfMX{data = [{Element, As, []}|Data]};
parse({characters, Chars},
      State = #gmfMX{data = [{E,A,C}|Data]}) ->
    State#gmfMX{data = [{E,A,[{chars,Chars}|C]}|Data]};
parse({endElement, _, Element, _},
      #gmfMX{data = [{Element, _, _}|_]} = State) ->
    data(State); %%,
%%State#gmfMX{data = NewData};

parse(_, State) ->
    State.
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
attrs(Attrs) ->
    [{?L2A(A),V} || {_,_,A,V} <- Attrs].


%% data([{E,_, _}|_])
%%   when E == "models";
%%        E == "dtdVersion";
%%        E == "momMetaData" ->
%%     [];

%% data([{"interMim", _, C}|_]) ->
%%     io:format(" Data ~p~n", [lists:reverse(C)]),
%%     [];

%% data([{"mim", Attr, C}|_]) ->
%%     case lists:keysearch(domainExtension, 1, C) of
%% 	{value, {_, DA, DC}} ->
%% 	    data([{"domainExtension", DA, DC}]);
%% 	false ->
%% 	    ok
%%     end,
%%     io:format(" Attrs ~p~n", [Attr]),
%%     io:format(" Data  ~p~n", [lists:reverse(C)]),
%%    [];

data(#gmfMX{data = [{"momMetaData", _, _}]} = State) ->
    State#gmfMX{data = []};

%% Special handling for topmost domainExtension
data(#gmfMX{data = [{"domainExtension", Am, C}],
	    mim  = {Mim, _},
	    cxpProdId = CP,
	    cxpRev    = CR,
	    table     = T} = State) ->
    C1 = lists:filter(fun(X) -> is_tuple(X) andalso element(1,X) == extension end, C),
    C2 = lists:map(fun({_,A,_}) ->
			   {?L2A(proplists:get_value(name, A)),
			    proplists:get_value(value, A)}
		   end, C1),
    ets:insert(T, {domainExtension,
		   {domainExtension, Mim, CP, CR},
		   proplists:get_value(domain,Am),
		   C2}),
    %%io:format("Data ~p~n", [{T, domainExtension, proplists:get_value(domain,Am), C2}]),
    State#gmfMX{data = []};
data(#gmfMX{data = [Data],
	    mim  = MimData,
	    cxpProdId = CP,
	    cxpRev    = CR,
	    table     = T} = State) ->
    %%io:format("Data ~p~n", [datae(Data, MimData)]),
    insert(datae(Data, MimData), MimData, CP, CR, T),
    State#gmfMX{data = []};
data(#gmfMX{data = [Data, {PE,PA,PC}|Rest],
	    mim  = MimData} = State) ->
    State#gmfMX{data = [{PE,PA,[datae(Data, MimData)|PC]}|Rest]}.

insert({relationship = SC, N, {_, MimC} = C, P}, MimD, CP, CR, T) ->
    Mim = case MimD of
	      undefined ->
		  %% interMim relation
		  MimC;
	      {MimC, _} ->
		  MimC
	  end,
    ets:insert(T, {SC,
		   {N, Mim, CP, CR},
		   C,
		   P});
insert({biDirectionalAssociation = SC, N, {_, _, MimC, _} = C, S},
       MimD, CP, CR, T) ->
    Mim = case MimD of
	      undefined ->
		  %% Will this ever happen? etxpeno
		  %% interMim relation
		  MimC;
	      {Mim1, _} ->
		  Mim1
	  end,
    ets:insert(T, {SC,
		   {N, Mim, CP, CR},
		   C,
		   S});
insert({SC, N, C}, {Mim, _}, CP, CR, T) ->
    ets:insert(T, {SC,
		   {N, Mim, CP, CR},
		   C}).

%% Flags
datae({E, _, _},_)
  when E == "local";
       E == "mandatory";
       E == "noNotification";
       E == "nonPersistent";
       E == "readOnly";
       E == "restricted";
       E == "static";
       E == "undefined";
       E == "lockBeforeModify";
       E == "validationRules";
       E == "restartType";
       E == "indexed";
       E == "key";
       E == "isNillable";
       E == "isOrderKey";
       E == "root";
       E == "isReserving" ->
    ?L2A(E);

%% General mim name
datae({"mimName",_, C},_) ->
    {mimName, proplists:get_value(chars, C)};

%% Relationship
datae({"hasClass", A, C},_) ->
    Class = proplists:get_value(name, A),
    Mim   = proplists:get_value(mimName, C),
    {hasClass, {Class, Mim}};
datae({E, _, C},_) when E == "parent";
			E == "child" ->
    {Class,Mim} = proplists:get_value(hasClass, C),
    {?L2A(E), {Class, Mim}};
datae({E, _, PC},_) when E == "containment";
			 E == "biDirectionalAssociation";
			 E == "uniDirectionalAssociation" ->
    PC;
datae({"associationEnd", A, C}, _) ->
    Name = proplists:get_value(name, A),
    {Class, Mim} = proplists:get_value(hasClass, C),
    case proplists:is_defined(isReserving, C) of
	true ->
	    {reservingAssociationEnd, {Name, Class, Mim, C}};
	false ->
	    {reservedAssociationEnd, {Name, Class, Mim, C}}
    end;
datae({"relationship", A, PC1},_) ->
    PC = lists:flatten(PC1),
    Child = proplists:get_value(child, PC),
    Parent = proplists:get_value(parent, PC),
    ReservingAssociationEnd = proplists:get_value(reservingAssociationEnd, PC),
    ReservedAssociationEnd = proplists:get_value(reservedAssociationEnd, PC),

    if
	Child =/= undefined andalso Parent =/= undefined ->
	    {relationship,
	     proplists:get_value(name, A),
	     Child,
	     Parent};
	ReservingAssociationEnd =/= undefined andalso
	ReservedAssociationEnd =/= undefined ->
	    {biDirectionalAssociation,
	     proplists:get_value(name, A),
	     ReservingAssociationEnd,
	     ReservedAssociationEnd};
	ReservedAssociationEnd =/= undefined ->
	    {uniDirectionalAssociation,
	     proplists:get_value(name, A),
	     ReservedAssociationEnd}
    end;

%% Common to struct and attrib
datae({"defaultValue",_, C},_) ->
    {defaultValue, proplists:get_value(chars, C)};
datae({"dataType", _, C} = _X,_) ->
    case proplists:get_value(gmf_t, C) of
	undefined ->
	    %% Can only be a sequence if it is moRef/structRef
	    %% crash otherwise
	    {sequence, _, Elem} = lists:keyfind(sequence, 1, C),
	    {dataType, {sequence, proplists:get_value(gmf_t, Elem)}};
	Type ->
	    {dataType, Type}
    end;
%%    {dataType, proplists:get_value(gmf_t, C)};
datae({E, A, C},_)
  when E == "moRef" ->
    {gmf_t, {?L2A(E),
	     proplists:get_value(name, A),
	     proplists:get_value(mimName, C)}};
datae({E, A, C}, MimData)
  when E == "structRef";
       E == "enumRef";
       E == "derivedDataTypeRef" ->
    MimName =
	case MimData of
	    undefined -> undefined;
	    {M,_} -> M
	end,
    {gmf_t, {?L2A(E),
	     proplists:get_value(name, A),
	     proplists:get_value(mimName, C, MimName)}};
datae({E, _, C},_)
  when E == "boolean";
       E == "octet";
       E == "char";
       E == "double";
       E == "float";
       E == "long";
       E == "longlong";
       E == "short";
       E == "string";
       E == "wstring";
       E == "int8";
       E == "int16";
       E == "int32";
       E == "int64";
       E == "uint8";
       E == "uint16";
       E == "uint32";
       E == "uint64" ->
    {gmf_t, {?L2A(E), undefined, proplists:get_value(defaultValue, C)}};

datae({"attribute", A, C},_) ->
    {attribute,
     proplists:get_value(name, A),
     lists:map(fun attr_flag/1, C)};

datae({"action", A, C},_) ->
    %%io:format("##############~p~n", [[A,C]]),
    {action,
     proplists:get_value(name, A),
     lists:foldl(fun action_data/2, [], C)};


%% See if someone shouts
datae({"structMember", A, C},_) ->
    Type =
	case proplists:get_value(gmf_t, C) of
	    undefined ->
		{sequence, _, Elem} = lists:keyfind(sequence, 1, C),
		{sequence, proplists:get_value(gmf_t, Elem)};
	    T ->
		T
	end,

    {structMember,
     proplists:get_value(name, A),
     [{dataType, Type}]};

%% %% baseType
%% datae({"baseType", A, C}) ->
%%     Type = proplists:get_value(gmf_t, C),
%%     {baseType,
%%      "GMF_baseType",
%%      [{dataType, Type}]};

datae({E, A, C},_) when E == "class";
			E == "struct" ->
    {?L2A(E),
     proplists:get_value(name, A),
     lists:reverse(C)};

datae({E, _, C}, _) when E == "min";
			 E == "max" ->
    {?L2A(E), list_to_integer(proplists:get_value(chars, C))};

datae({"cardinality", _, C}, _) ->
    {cardinality, C};

%% In GMF, we are only interested about the "baseType"
datae({"derivedDataType", A, C},_) ->
    {value, {baseType, _, BC}, _} = lists:keytake(baseType, 1, C),
    Type = proplists:get_value(gmf_t, BC),
    {derivedDataType, proplists:get_value(name, A), [{dataType, Type}]};

datae({E,A,C},_) ->
    content(E, C),
    {?L2A(E),A,lists:reverse(C)}.


content(_, C) ->
    C.

%%% #---------------------------------------------------------
%%% GET
%%% #---------------------------------------------------------
%%% mom is stored as {mim, Key, Attributes}
get_all_mom_info(Tid) ->
    ets:select(Tid, [{{mim, '$1', '$2'}, [],[{{'$1','$2'}}]}]).

get_domain_extension(Tid, {Mom, CxpProdId, CxpRev}) ->
    ets:select(Tid, [{{domainExtension,
		       {domainExtension, Mom, CxpProdId, CxpRev},
		       '$1',
		       '$2'},
		      [],
		      [{{'$1','$2'}}]}]).

get_all_classes(Tid, {Mom, CxpProdId, CxpRev}) ->
    ets:select(Tid, [{{class,
		       {'$1', Mom, CxpProdId, CxpRev},
		       '$2'},
		      [],
		      [{{'$1','$2'}}]}]).
get_all_relations(Tid) ->
    ets:select(Tid, [{{relationship,
		       '_',
		       '$1',
		       '$2'},
		      [],
		      [{{'$1','$2'}}]}]).
get_all_structs(Tid, {Mom, CxpProdId, CxpRev}) ->
    ets:select(Tid, [{{struct,
		       {'$1', Mom, CxpProdId, CxpRev},
		       '$2'},
		      [],
		      [{{'$1','$2'}}]}]).

get_all_derivedtypes(Tid, {Mom, CxpProdId, CxpRev}) ->
    ets:select(Tid, [{{derivedDataType,
		       {'$1', Mom, CxpProdId, CxpRev},
		       '$2'},
		      [],
		      [{{'$1','$2'}}]}]).

get_all_bidirectionalassociations(Tid, {Mom, CxpProdId, CxpRev}) ->
    ets:select(Tid, [{{biDirectionalAssociation,
		       {'_', Mom, CxpProdId, CxpRev},
		       '$1',
		       '$2'},
		      [],
		      [{{'$1','$2'}}]}]).

attr_flag({domainExtension, _, Es}) ->
    NVs = [{proplists:get_value(name, NV),
	    proplists:get_value(value, NV)} || {_, NV, _} <- Es],
    case proplists:get_value("isNillable", NVs) of
	"true" -> isNillable;
	_      -> isNotNillable %% Give it some name
    end;
attr_flag(C) -> C.



action_data({domainExtension, _, Es}, Acc) ->
    %% OK with ++
    [{proplists:get_value(name, NV),
      proplists:get_value(value, NV)} || {_, NV, _} <- Es] ++ Acc;
action_data({Type, A, C}, Acc) ->
    Name = proplists:get_value(name, A),
    case {proplists:get_value(gmf_t, C),
	  proplists:get_value(dataType, C)} of
	{undefined, undefined} ->
	    case lists:keyfind(sequence, 1, C) of
	        {sequence, _, Elem} ->
                    DType = proplists:get_value(gmf_t, Elem),
	            [{Type, Name, [{dataType, DType}]}] ++ Acc;
                _ ->
	            Acc
            end;
	{DType, undefined} ->
	    [{Type, Name, [{dataType, DType}]}] ++ Acc;
	{undefined, DType} ->
	    [{Type, Name, [{dataType, DType}]}] ++ Acc
    end.



%%{attribute,"class3",[{dataType,...},noNotification|...]}
%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
