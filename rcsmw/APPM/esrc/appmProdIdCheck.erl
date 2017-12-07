%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmProdIdCheck.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R5A/1
%%%
%%% @doc ==Application Manager appdata registration==
%%% This module handles the .
%%%
%%% ----------------------------------------------------------
-module(appmProdIdCheck).
-vsn('/main/R3A/R5A/1').
-date('2016-03-11').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R3A/1     20141118   etxarnu      Created (from hwmProdIdCheck in IS :) )
%%% R5A/1     20160311   etxberb      Module deprecated. Equivalent and extended
%%%                                   functionality implemented in SYS.
%%% -----------------------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%% ###########################################################################
%%% !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!!
%%% Please use 'is_rev_XXX/2' functions in 'sysUtil.erl' instead.
%%% !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!!
%%% ###########################################################################
-export([check_prod_rev/2]).



%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%%   check_prod_rev(FoundRev, ExpRev)
%%% Input:
%%%   FoundRev = A string whith the found revision 
%%%   ExpRev = A string whith the expectied revision    
%%% Output: true | false               
%%% Exceptions: none    
%%% Description:
%%%    This function returns true if FoundRev >= ExpRev
%%% ----------------------------------------------------------

%%% ###########################################################################
%%% !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!!
%%% Please use 'is_rev_XXX/2' functions in 'sysUtil.erl' instead.
%%% !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!! DEPRECATED !!!
%%% ###########################################################################
check_prod_rev(FoundRev,ExpRev) ->  

    case check_in_par(FoundRev,ExpRev) of
        true ->
            check_rev(FoundRev,ExpRev);
        false ->
            false
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%%----------------------------------------------------------------------      
%%%----------------------------------------------------------------------      

%%% ----------------------------------------------------------
%%%   check_in_par(FoundRev, ExpRev)
%%% Input:
%%%   FoRev = The found revision 
%%%   ExpRev = The expectied revision   
%%% Output: true or false               
%%% Exceptions: none    
%%% Description:
%%%    This function returns true if the parameter FoundRev
%%%    and the parameter ExpRev is of type string and is of
%%%    valid length
%%% ----------------------------------------------------------

check_in_par(FoRev,ExpRev) when is_list(FoRev), is_list(ExpRev) ->

    FL = string:len(FoRev),
    EL = string:len(ExpRev),
    check_in_par(FoRev,ExpRev,FL,EL);

check_in_par(_FoRev,_ExpRev) ->    
    false.

check_in_par(_FoRev,_ExpRev,FL,EL) when FL>1,FL<8,EL>1,EL<8 ->
    true;

check_in_par(_FoRev,_ExpRev,_FL,_EL) ->
    false.


%%% ----------------------------------------------------------
%%%   check_rev(FoundRev, ExpRev)
%%% Input:
%%%   FoundRev = The found revision 
%%%   ExpRev = The expectied revision   
%%% Output: true or false               
%%% Exceptions: none    
%%% Description:
%%%    This function returns true if the parameter FoundRev
%%%    and the parameter ExpRev is of type string and is equal
%%%    else run function diff_rev(FoundRev,ExpRev)
%%% ----------------------------------------------------------

check_rev(FoundRev,ExpRev) ->        
    case string:equal(FoundRev,ExpRev) of 
        true -> 
            true;
        false ->
            diff_rev(FoundRev,ExpRev) 
    end.


%%% ----------------------------------------------------------
%%%   diff_rev(FoundRev, ExpRev)
%%% Input:
%%%   FoundRev = The found revision 
%%%   ExpRev = The expectied revision   
%%% Output: true or false               
%%% Exceptions: none    
%%% Description:
%%%    Separate the different cases of P and R, picks out
%%%    the revision number and runs the right function
%%%    FoundRev = R.., ExpRev = P.. start compare_rev3
%%%    FoundRev = P.., ExpRev = R.. start compare_rev3
%%%    FoundRev = P.., ExpRev = P.. start compare_rev1
%%%    FoundRev = R.., ExpRev = R.. start compare_rev2
%%% ----------------------------------------------------------

diff_rev(FoundRev,ExpRev) ->

    case hd(FoundRev) == hd(ExpRev) of
        true ->
            case string:substr(FoundRev,1,1) == "P" of
                true ->
                    FoNr = pick_rev_nr(FoundRev),
                    ExpNr = pick_rev_nr(ExpRev),
                    compare_rev1(FoNr,ExpNr,FoundRev,ExpRev);
                false ->
                    FoNr = pick_rev_nr(FoundRev),
                    ExpNr = pick_rev_nr(ExpRev),
                    compare_rev2(FoNr,ExpNr)
            end;
        false ->
            FoNr = pick_rev_nr(FoundRev),
            ExpNr = pick_rev_nr(ExpRev),
            compare_rev3(FoNr,ExpNr,FoundRev,ExpRev)
    end.


%%% ----------------------------------------------------------
%%%   pick_rev_nr(RevString)
%%% Input:
%%%   RevString = String with the revision      
%%% Output: a integer           
%%% Exceptions: none    
%%% Description:
%%%    Picks out the number from the input string at
%%%    position 2 and 3 or only 2, (max revision 99) 
%%% ----------------------------------------------------------

pick_rev_nr(RevString) ->
    OkList = ["0","1","2","3","4","5","6","7","8","9"],
    case lists:member(string:substr(RevString,2,1),OkList) of
        true ->
            case lists:member(string:substr(RevString,3,1),OkList) of
                true ->
		    case lists:member(string:substr(RevString,4,1),OkList) of
			true ->
			    case lists:member(string:substr(RevString,5,1),OkList) of
				true ->
				    list_to_integer(string:substr(RevString,2,4));
				false ->
				    list_to_integer(string:substr(RevString,2,3))
			    end;
			false ->
			    list_to_integer(string:substr(RevString,2,2))
		    end;
                false ->
                       list_to_integer(string:substr(RevString,2,1))
            end;
        false ->
            false                                     
    end.


%%% ----------------------------------------------------------
%%%   compare_rev1(FoNr,ExpNr,FoundRev,ExpRev)
%%% Input:
%%%   FoNr = Found rev number
%%%   ExpNr = Expectied rev number
%%%   FoundRev = The found revision 
%%%   ExpRev = The expectied revision   
%%% Output: true or false               
%%% Exceptions: none    
%%% Description:
%%%   Returns true if FoNr > ExpNr and false if opposit
%%%   if FoNr = ExpNr then checks the letter after the
%%%   number and compares 
%%% ----------------------------------------------------------

compare_rev1(FoNr,_ExpNr,_FoundRev,_ExpRev) when FoNr== false -> 
    false;

compare_rev1(_FoNr,ExpNr,_FoundRev,_ExpRev) when ExpNr== false ->
    false;

compare_rev1(FoNr,ExpNr,_FoundRev,_ExpRev) when FoNr > ExpNr ->
    true;

compare_rev1(FoNr,ExpNr,_FoundRev,_ExpRev) when FoNr < ExpNr ->
    false;

compare_rev1(FoNr,ExpNr,FoundRev,ExpRev) when FoNr == ExpNr ->

    case FoNr < 10 of
        true ->
            FoRevLetter = string:substr(FoundRev, 3, 1),
            ExpRevLetter = string:substr(ExpRev, 3, 1),
            compare_letter(FoRevLetter,ExpRevLetter);
        false ->
            FoRevLetter = string:substr(FoundRev, 4, 1),
            ExpRevLetter = string:substr(ExpRev, 4, 1),
            compare_letter(FoRevLetter,ExpRevLetter)
    end.
    

%%% ----------------------------------------------------------
%%%   compare_letter(FoLetter,ExpLetter)
%%% Input:
%%%   FoLetter = Found rev letter
%%%   ExpLetter = Expectied rev letter  
%%% Output: true or false               
%%% Exceptions: none    
%%% Description:
%%%   Returns true if the FoundRev letter is higher or equal
%%%   to the ExpRev letter and false if opposit
%%% ----------------------------------------------------------

compare_letter(FoLetter,ExpLetter) when FoLetter>=ExpLetter ->
    true;

compare_letter(_FoLetter,_ExpLetter) ->
    false.


%%% ----------------------------------------------------------
%%%   compare_rev2(FoNr,ExpNr)
%%% Input:
%%%   FoNr = Found rev number
%%%   ExpNr = Expectied rev number      
%%% Output: true or false               
%%% Exceptions: none    
%%% Description:
%%%   Returns true if FoNr >= ExpNr and false if FoNr < ExpNr
%%% ----------------------------------------------------------

compare_rev2(FoNr,_ExpNr) when FoNr== false -> 
    false;

compare_rev2(_FoNr,ExpNr) when ExpNr== false ->
    false;

compare_rev2(FoNr,ExpNr) when FoNr >= ExpNr ->
    true;

compare_rev2(_FoNr,_ExpNr) ->
    false.


%%% ----------------------------------------------------------
%%%   compare_rev3(FoNr,ExpNr,FoundRev,ExpRev)
%%% Input:
%%%   FoNr = Found rev number
%%%   ExpNr = Expectied rev number
%%%   FoundRev = The found revision 
%%%   ExpRev = The expectied revision   
%%% Output: true or false               
%%% Exceptions: none    
%%% Description:
%%%   Returns true if FoNr > ExpNr and false if opposit
%%%   if FoNr = ExpNr then checks the first letter of the
%%%   FoundRev and if R returns true else false
%%% ----------------------------------------------------------

compare_rev3(FoNr,_ExpNr,_FoundRev,_ExpRev) when FoNr== false -> 
    false;

compare_rev3(_FoNr,ExpNr,_FoundRev,_ExpRev) when ExpNr== false ->
    false;

compare_rev3(FoNr,ExpNr,_FoundRev,_ExpRev) when  FoNr > ExpNr ->
   true;

compare_rev3(FoNr,ExpNr,_FoundRev,_ExpRev) when FoNr < ExpNr ->
   false;
    
compare_rev3(_FoNr,_ExpNr,FoundRev,_ExpRev) ->
    string:substr(FoundRev,1,1) == "R".

