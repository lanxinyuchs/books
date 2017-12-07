%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_xml.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_xml).
-vsn('/main/R1A/3').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% /R1A/3     2012-04-11 etxpeno     Update of #lici_lkf{}
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([convert/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("xmerl/include/xmerl.hrl").
-include("lih_lici.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec convert(Filename) -> Result when
      Filename :: string(),
      Result :: {ok, #lici_lkf{}}.
convert(Filename) ->
    DTD = filename:join(code:priv_dir(base), "licFile20.dtd"),
    {Doc, _} = xmerl_scan:file(Filename, [{validation, dtd},
					  {doctype_DTD, DTD}]),
    {ok, parse(Doc, #lici_lkf{status = ok})}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

parse(#xmlElement{name       = body,
		  content    = Content,
		  attributes = Attributes}, Acc) ->
    NewAcc = Acc#lici_lkf{format_version = get_attr_value(Attributes,
							  formatVersion),
			  signature_type = get_signature_type(Attributes)},
    lists:foldl(fun parse/2, NewAcc, Content);
parse(#xmlElement{name       = 'SWLT',
		  content    = Content,
		  attributes = Attributes}, Acc) ->
    NewAcc = Acc#lici_lkf{customer_id = get_attr_value(Attributes, customerId),
			  product_type = get_attr_value(Attributes,
							productType),
			  swlt_id = get_attr_value(Attributes, swltId)},
    lists:foldl(fun parse/2, NewAcc, Content);
parse(#xmlElement{name       = fingerprint,
		  content    = Content,
		  attributes = Attributes}, Acc) ->
    NewAcc = Acc#lici_lkf{method = get_method(Attributes),
			  print  = get_attr_value(Attributes, print)},
    lists:foldl(fun parse/2, NewAcc, Content);
parse(#xmlElement{name    = sequenceNumber,
		  content = Content}, Acc) ->
    Acc#lici_lkf{seq_no = get_seq_no(Content)};
parse(#xmlElement{name       = Name,
		  content    = Content,
		  attributes = Attributes},
      #lici_lkf{license_keys = List} = Acc) when Name =:= featureKey;
						 Name =:= capacityKey ->
    LicenseKey = get_license_key(Attributes, Name, Content),
    Acc#lici_lkf{license_keys = [LicenseKey|List]};
parse(#xmlElement{name    = emergencyResetKey,
		  content = Content},
      #lici_lkf{license_keys = List} = Acc) ->
    LicenseKey = get_emergency_reset_license_key(Content),
    Acc#lici_lkf{license_keys = [LicenseKey|List]};
parse(#xmlElement{name    = licFile,
		  content = Content}, Acc) ->
    lists:foldl(fun parse/2, Acc, Content);
parse(#xmlElement{}, Acc) ->
    Acc;
parse(#xmlText{}, Acc) ->
    Acc.

get_attr_value(Attributes, Name) ->
    Tuple = lists:keyfind(Name, #xmlAttribute.name, Attributes),
    Tuple#xmlAttribute.value.

get_seq_no([#xmlText{value = Value}]) ->
    list_to_integer(Value).

get_method(Attributes) ->
    list_to_integer(get_attr_value(Attributes, method)).

get_signature_type(Attributes) ->
    list_to_integer(get_attr_value(Attributes, signatureType)).

get_license_key(Attributes, Type, Content) ->
    Acc0 = #lici_license_key{id = {Type, get_id(Attributes)}},
    lists:foldl(fun get_license_key/2, Acc0, Content).

get_emergency_reset_license_key(Content) ->
    Acc0 = #lici_license_key{id = emergencyReset, emergency_reset = true},
    lists:foldl(fun get_license_key/2, Acc0, Content).

get_license_key(#xmlElement{name = start, content = Content}, Acc) ->
    Acc#lici_license_key{start = get_start(Content)};
get_license_key(#xmlElement{name = stop, content = Content}, Acc) ->
    Acc#lici_license_key{stop = get_stop(Content)};
get_license_key(#xmlElement{name = noStop}, Acc) ->
    Acc#lici_license_key{stop = infinity};
get_license_key(#xmlElement{name = capacity, content = Content}, Acc) ->
    Acc#lici_license_key{capacity = get_capacity(Content)};
get_license_key(#xmlElement{name = noCapacityLimit}, Acc) ->
    Acc#lici_license_key{capacity = ?LICI_NO_LIMIT};
get_license_key(#xmlElement{name = hardLimit, content = Content}, Acc) ->
    Acc#lici_license_key{hard_limit = get_hard_limit(Content)};
get_license_key(#xmlElement{name = noHardLimit}, Acc) ->
    Acc#lici_license_key{hard_limit = ?LICI_NO_LIMIT};
get_license_key(#xmlElement{}, Acc) ->
    Acc;
get_license_key(#xmlText{}, Acc) ->
    Acc.

get_id(Attributes) ->
    Value = get_attr_value(Attributes, id),
    lih_lib:convert_id(Value).

get_start([#xmlText{value = Value}]) ->
    [Y, M, D] = string:tokens(Value, "-"),
    {list_to_integer(Y), list_to_integer(M), list_to_integer(D)}.

get_stop(Arg) -> get_start(Arg).

get_capacity([#xmlText{value = Value}]) ->
    ?LICI_LIMIT(list_to_integer(Value)).

get_hard_limit(Arg) -> get_capacity(Arg).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
