%%% #0.    BASIC INFORMATION
-module(comsaSshConfig).
-vsn('/main/R6A/R11A/2').
-date('2017-11-28').
-author('etxtory').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R6A/1      2016-06-13 uabhgma     Created
%%% R6A/2      2016-07-19 uabhgma     Removed test function
%%% R11A/1     2017-11-27 etxtory     Added diffie-hellman
%%% R11A/2     2017-11-28 etxtory     Changed order for d-h above
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERNAL FUNCTIONS
%%% ---------------------------------------------------------

%% ====================================================================
%% EXPORTED FUNCTIONS
%% ====================================================================
-export([get_supported_algorithms_by_type/1]).
-export([extract_selected_algorithms/2]).
-export([convert_selected_algos_to_ssh_opt_format/2]).
-export([get_ssh_preferred_algorithms/3]).

%% ====================================================================
%% CODE FOR EXPORTED FUNCTIONS
%% ====================================================================

%%% ----------------------------------------------------------
%%% #get_supported_algorithms_by_type(Type)       	
%%%  Input: Type - kex | cipher | mac
%%%  Output: [supported algorithms as strings] | [] 
%%%  Exceptions:
%%%  Description: The function retrieves the supported algorithms from the 
%%%               OTP SSH module and extracts the algos for the specified type
%%% ----------------------------------------------------------
get_supported_algorithms_by_type(Type) ->
    AllSupportedAlgos = ssh:default_algorithms(),
    case lists:keyfind(Type, 1, AllSupportedAlgos) of
	{kex, Algos} ->
	    AddedKexs =  ['diffie-hellman-group1-sha1'],
	    AllAlgos = add_algorithms_by_type(kex, Algos, AddedKexs, _Acc = []),
	    lists:map(fun(X)->atom_to_list(X) end, AllAlgos);

        {Type, Algos} ->
            %% some types supports different algos for client and server
            case lists:keyfind(client2server, 1, Algos) of
                {client2server, Algorithms} -> 
		    lists:map(fun(X)->atom_to_list(X) end, Algorithms);
                {server2client, Algorithms} -> 
		    lists:map(fun(X)->atom_to_list(X) end, Algorithms);
                _ ->
		    lists:map(fun(X)->atom_to_list(X) end, Algos)
            end;

        _ -> 
            []
    end.

add_algorithms_by_type(kex, Algos, [Kex | T], Acc) ->
    case lists:member(Kex, Algos) of
	true ->
	    add_algorithms_by_type(kex, Algos, T, Acc);
	false ->
	    add_algorithms_by_type(kex, Algos, T, Acc ++ [Kex])
    end;
add_algorithms_by_type(kex, Algos, [], Acc) ->
    lists:append(Algos, Acc).

%%% ----------------------------------------------------------
%%% #get_selected_algorithms(Supported, Wanted)       	
%%%  Input: [Supported] - a list of supported algorithms for this type, eg kex 
%%%	        [Wanted] - an ordered list of preferred algorithms
%%%  Output: [selected algos] | [] 
%%%  Exceptions:
%%%  Description: The function returns the list of wanted algorithms that are
%%%               supported in the wanted order
%%% ----------------------------------------------------------
extract_selected_algorithms(Supported, Wanted) -> 
    SupportedAsAtoms = lists:map(fun(X)->list_to_atom(X) end, Supported),
    WantedAsAtoms = lists:map(fun(X)->list_to_atom(X) end, Wanted),
    SelectedAsAtoms = select_algorithms(SupportedAsAtoms, WantedAsAtoms,[]),
    case SelectedAsAtoms of
        [] -> [];
        _ -> SelectedList = lists:map(fun(X)->atom_to_list(X) end, SelectedAsAtoms),
             lists:reverse(SelectedList)
    end.

%%% ----------------------------------------------------------
%%% #convert_selected_algos_to_ssh_opt_format(Selected, Type)       	
%%%  Input: [Selected] - a list of supported algorithms for this type, eg kex 
%%%	        Type - the type of algorithms, e.g kex or mac
%%%  Output: {Type,[{client2server,[Selected]},server2client[Selected]]} |
%%%          {Type,[Selected]} |
%%%          invalid_type
%%%  Exceptions:
%%%  Description: The function returns a type keyed tuple with the selected 
%%%               algorithms. Note: that for the kex type there is no
%%%               distinction betwwen server2client and client2server
%%% ----------------------------------------------------------
convert_selected_algos_to_ssh_opt_format(Selected, Type) -> 
    SelectedAsAtoms = lists:map(fun(X)->list_to_atom(X) end, Selected),
    case Type of 
        kex -> {kex, SelectedAsAtoms};
        cipher -> {cipher,[{client2server,SelectedAsAtoms},{server2client,SelectedAsAtoms}]};
        mac -> {mac,[{client2server,SelectedAsAtoms},{server2client,SelectedAsAtoms}]};
        _ -> invalid_type
    end.

%%% ----------------------------------------------------------
%%% #get_ssh_preferred_algorithms(Selected_ciphers, Selected_kex, Selected_macs)       	
%%%  Input: [Selected_ciphers] - a list of supprted algorithms for encryption 
%%%	        [Selected_kex] - a list of supported algorithms for key exchange
%%%	        [Selected_macs] - a list of supported algorithms for message authentication
%%%  Output: {preferred_algorithms,[{cipher,{client2server,[Selected_ciphers]},{server2client[Selected_ciphers]}},
%%%                                {kex,[Selected_kex]},
%%%                                {mac,{client2server,[Selected_macs]},{server2client[Selected_macs]}}]}	
%%%          |
%%%          invalid_type
%%%  Exceptions:
%%%  Description: The function returns a keyed tuple with the selected 
%%%               algorithms for each. Note: that for the kex type there is no
%%%               distinction between server2client and client2server
%%% ----------------------------------------------------------
get_ssh_preferred_algorithms(SelectedCiphers, SelectedKex, SelectedMacs) -> 
    KexOpt = convert_selected_algos_to_ssh_opt_format(SelectedKex, kex),
    CipherOpt = convert_selected_algos_to_ssh_opt_format(SelectedCiphers, cipher),
    MacsOpt = convert_selected_algos_to_ssh_opt_format(SelectedMacs, mac),
    
    {preferred_algorithms, [CipherOpt, KexOpt, MacsOpt]}.

%% ====================================================================
%% CODE FOR INTERNAL FUNCTIONS
%% ====================================================================
%% loop through the list of wanted algos and save each wanted algorithm that 
%% also exist in the list of supported algos
select_algorithms(_, [], Final) -> Final;
select_algorithms(Supported, [H|T], Final) ->
   case lists:member(H, Supported) of
      true  -> select_algorithms(Supported, T, [H|Final]);
      false -> select_algorithms(Supported, T, Final)
   end.
