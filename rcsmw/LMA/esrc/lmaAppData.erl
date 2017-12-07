%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaAppData.erl %
%%% @author echhedb
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/1
%%%
%%% @doc ==Application Manager appdata registration==
%%% This module handles the registration appdata with target 'license'.
%%% Two types of appdata is handled: loadmodule and lmlist.
%%% The loadmodule describes files related to a load module.
%%% The lmlist describes what load modules belong to a board type.
%%%
%%% ----------------------------------------------------------
-module(lmaAppData).
-vsn('/main/R11A/R12A/1').
-date('2017-11-02').
-author('echhedb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
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
%%% %CCaseCopyrightEnd%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R12A/1     20170818   echhedb     SP086: Added AppData functionality.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-compile(export_all).
-export([appdata/3]).
-export([get_keyids/0]).
-export([convert_stringlist_to_structarray_binary/1]).

%% General
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(ELSE, true).


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------




%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
-include_lib("xmerl/include/xmerl.hrl").
-include("lma.hrl").


%%% ----------------------------------------------------------
%%% #		appdata(ProdNo, ProdRev, RegData)
%%% @doc
%%% Description:	Function that receives appdata with target 'license' from swm.
%%% 			The registration of lmaAppData as an appdata receiver is made
%%%			in lmaDataInit.
%%%			Received appdata is then parsed and stored in an mnesia table.
%%% Input:		ProdNo:string(), ProdRev:string(), RegData:tuple()
%%% Output:
%%% Exceptions:
%%% ----------------------------------------------------------

-spec appdata(ProdNo :: string(), ProdRev :: string(), Data :: tuple()) -> ok.

appdata(ProdNo, ProdRev, #xmlElement{content = Content}) ->

    R = parse_elements(Content,[]),

    % Printout for testing
    %sysInitI:info_msg(
    %  "~p:store_appdata - license~n"
    %  "ProdNo: ~p ~n"
    %  "ProdRev: ~p ~n"
    %  "R: ~p ~n"
    %  "XML Content: ~p ~n",
    %  [?MODULE, ProdNo, ProdRev , R, Content]), % Content#xmlElement.name,

    % fun() that calls function populate_and_store_records(..) which populates records of type 
    % lmaLicenseKeyData, and stores them in an mnesia table
    PopulateAndStoreRecord =
	fun() ->
		populate_and_store_records(ProdNo, ProdRev, R)
	end,

    % Using above fun()
    case mnesia:transaction(PopulateAndStoreRecord) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [Content])
    end
.


%%% ---------------------------------------------------------
%%% #           	populate_and_store_records(ProdNo, ProdRev, List)
%%% @doc 
%%% Description:	Takes the content of appdata elements with attribute target=license, 
%%%			where every child element matching featurelicense	is parsed.
%%%			The value of each keyId attribute found in each featurelicense element 
%%%			(along with corresponding prodNo and prodRev) is then saved as an 
%%%			lmaLicenseKeyData record. Each created record is then written to the 
%%%			equivalent mnesia table (also called lmaLicenseKeyData).
%%%			The function overwrites any duplicate keyIds that are found.
%%% Input:		ProdNo:string(), ProdRev:string(), List:list()
%%% Output:		ok
%%% Exceptions:
%%% @end
%%% ----------------------------------------------------------

% Recursive case
populate_and_store_records(ProdNo, ProdRev, [{featurelicense, Attributes, _Content}|Rest]) ->
	KeyId = attr(keyId, Attributes),
	%io:format("keyid: ~s~n", [KeyId]),
	sysInitI:info_msg("keyid: ~s~n", [KeyId]),
			
	Record = #lmaLicenseKeyData{
		keyId = KeyId,
		prodNo = ProdNo,
		prodRev = ProdRev				
	},

	ok = mnesia:write(Record),
	%sysInitI:info_msg("Wrote to mnesiatable with Record: ~s~n", [Record]),
	populate_and_store_records(ProdNo, ProdRev, Rest);

% Base case
populate_and_store_records(_ProdNo, _ProdRev,[]) ->
	ok.


%%% ---------------------------------------------------------
%%% #           	get_keyids()
%%% @doc 
%%% Description:	Returns a list of keyId strings from the lmaLicenseKeyData mnesia 
%%%			table using list comprehension.
%%% Input:		
%%% Output:		list()
%%% Exceptions:
%%% @end
%%% ----------------------------------------------------------

get_keyids() ->
	[X#lmaLicenseKeyData.keyId || X <- ets:tab2list(lmaLicenseKeyData)]
.


%%% ---------------------------------------------------------
%%% #		convert_stringlist_to_structarray_binary(ListOfStrings)
%%% @doc
%%% Description:	Converts a lists of strings to a binary string format corresponding to a 
%%%			certain array of structs (struct: GlmsFeatureConfigurationData defined 
%%%			in csrc/glmsadpi/GlmsDataTypes.h) that will be sent to and read by 
%%%			GLMS CC.
%%% Input:		ListOfStrings:list()
%%% Output:		binary()
%%% Exceptions:
%%% @end
%%%
%%% Example:
%%% > ListOfStrings = ["CXC4011018", "CXC4011929", "CXC4040010"].
%%% > rp(lmaAppData:convert_stringlist_to_structarray_binary(ListOfStrings)).
%%% ----------------------------------------------------------

convert_stringlist_to_structarray_binary(ListOfStrings) ->

	% fun() that creates a 0-padded list Pad with the size 24 - length(LElem) 
	% (where 24 is the defined length of GLMS_KEY_ID_LEN)
	% LElem and Pad are then added to form a new list represented by numbers
	ConvertedListElem = fun(LElem) ->
		Pad = [0 || _X <- lists:seq(1,24-length(LElem)) ],
		%PaddedLElem = LElem ++ Pad
		LElem ++ Pad
	end,

	% Above fun() is then mapped over ListOfStrings, where the fun() is applied 
	% on each element of ListOfStrings
	NewListofConvertedStrings = lists:map(ConvertedListElem, ListOfStrings),
	
	% All elements in the resulting list are then appended into a new list which is 
	% then converted to binary
	NewList = lists:append(NewListofConvertedStrings),
	list_to_binary(NewList)
.




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

parse_elements([],Acc) ->
    lists:reverse(Acc);
parse_elements([#xmlElement{name = Name,
			    attributes = Attributes,
			    content = Content} | T] , Acc) ->
    parse_elements(T,[ {Name,
			parse_attributes(Attributes,[]) ,
			parse_elements(Content,[])
		       } | Acc]);
parse_elements([_H|T],Acc) ->
    parse_elements(T,Acc).


%%% ----------------------------------------------------------
parse_attributes([],Acc) ->
    lists:reverse(Acc);
parse_attributes([#xmlAttribute{ name = Name, value = Value}|T],Acc) ->
    parse_attributes(T,[{Name,Value}|Acc]).


%%% ----------------------------------------------------------
attr(Key,List) ->
    attr(Key,List,undefined).

attr(Key,List,DefVal) ->
    case lists:keyfind(Key,1,List) of
	{Key,Val} ->
	    Val;
	_ ->
	    DefVal
    end.

%%% ----------------------------------------------------------



%%% 4     CODE FOR TEMPORARY CORRECTIONS
%%% ---------------------------------------------------------
%%% for test



    
