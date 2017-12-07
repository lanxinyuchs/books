%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coiSecurity.erl %
%%% @author ehsmbj
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/8

%%% @doc == COI - Control system Oam Interface ==
%%% This module contains functions for reading the Management Information
%%% Base (MIB) which supports the coi.erl interface functionality.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coiSecurity).
-vsn('/main/R11A/8').
-date('2017-10-13').
-author(eweiwan).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1   2017-09-22 eweiwan  Created.
%% R11A/3-4 2017-10-02 eralils Added getHttpsPorts, updated lookup_user
%%                             and added createFakeUser.
%% R11A/5  2017-10-02 eralils  Added logging to security log for lookup
%% R11A/6   2017-10-05 ehsmbj  Changed to use omc_server api for ldap_lookup.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
-export([lookup_user/1,
		 lookup_user/2,
		 getHttpsPorts/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% For test purposes (for testing lookup function):
-export([createFakeUser/0,
         deleteFakeUser/0,
         createFakeMaintenanceUser/0,
         deleteFakeMaintenanceUser/0]).

%%% ###---------------------------------------------------------------------###
%%% # Extended Interface
%%% ###---------------------------------------------------------------------###

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("coi.hrl").
%%###-include_lib("snmp/include/snmp_types.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(SUPER_ROLE, "EricssonSupport").
-define(PERMISSION_DENIED, "Permission denied").

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% Create tables.
%%%
%%% 
%%% ###=====================================================================###


%%% ###########################################################################
%%% @doc function to authenticate user with password and fetch role 
%%%
%%%   Example maintenace user (without ldap lookup):
%%%     coiSecurity:lookup_user("labuser","Letmein01").
%%%     {true,["EricssonSupport"]}
%%%
%%%   Example user (with ldap lookup):
%%%     coiSecurity:lookup_user("super","super01").    
%%%     {true,["Rcs_Application_User",
%%%       "Rcs_Application_SecurityAdministrator",
%%%       "Rcs_Application_Administrator","ENodeB_Application_User",
%%%       "ENodeB_Application_SecurityAdministrator",
%%%       "ENodeB_Application_Administrator",
%%%       "SystemSecurityAdministrator","SystemAdministrator",
%%%       "expert"]}
%%%
%%% @end
%%% ###=====================================================================###
-spec lookup_user(Username::string(), Password::string()) -> 
   {true, list(string())} | {false, string()}.
%%% ###=====================================================================###
%% Note 1: Logging to security log:
%% It was decided that the lookup message that shall be logged 
%% to security log shall has the same content as the lookup message 
%% logged by module "omc_server" (that handle authentication and 
%% authorization for interfaces cli, coli, etc).
%%
%% Note 2: Return error Msg from COI to user:
%% It was decided that the error message returned from COI to the COI interface 
%% user shall has the same content as a user receives when login on
%% interfaces cli, coli, etc. Those messages are handled by module omc_server.
%%
lookup_user(User, Pw) ->
	
    case comsaI:is_super_oam_user({User, Pw}) of
        true ->
            Roles = [?SUPER_ROLE],
            Facility = 4, 		%% - security/authorization messages
            Msg = "Maintenance user login: " ++ User ++ ", Roles: " ++
            roles_to_string(Roles),
            coi:log_security(Facility, info, Msg),
            {true, Roles};   
        false ->
            case omc_api:ldap_lookup(User, Pw) of
                {true, Roles} ->
                    Facility = 4, 		%% - security/authorization messages
                    Msg = "LDAP: lookup for user: " ++ User ++
                    ", Authenticated: true, Roles: " ++
                    roles_to_string(Roles),
                    coi:log_security(Facility, info, Msg),
                    {true, Roles};
                {false, Reason} ->
                    Facility = 4, 		%% - security/authorization messages
                    Msg = "LDAP: lookup, Authenticated: " ++
				    "false, Reason: " ++ Reason,
                    coi:log_security(Facility, info, Msg),
                    {false, ?PERMISSION_DENIED}
            end
    end.
	
 
%%% ###########################################################################
%%% @doc function to authenticate certificate user and fetch role 
%%%
%%%   Example maintenace user (without ldap lookup):
%%%     coiSecurity:lookup_user(Certificate)
%%%     {true,["EricssonSupport"]}
%%%
%%%   Example user (with ldap lookup):
%%%     coiSecurity:lookup_user("super").
%%%     {true,["Rcs_Application_User",
%%%       "Rcs_Application_SecurityAdministrator",
%%%       "Rcs_Application_Administrator","ENodeB_Application_User",
%%%       "ENodeB_Application_SecurityAdministrator",
%%%       "ENodeB_Application_Administrator",
%%%       "SystemSecurityAdministrator","SystemAdministrator",
%%%       "expert"]}
%%%
%%% @end
%%% ###=====================================================================###
-spec lookup_user(Certificate::term()) -> 
   {true, list(string())} | {false, string()}.
%%% ###=====================================================================###
%% Note 1: Logging to security log:
%% It was decided that the lookup message that shall be logged 
%% to security log shall has the same content as the lookup message 
%% logged by module "omc_server" (that handle authentication and 
%% authorization for interfaces cli, coli, etc).
%%
%% Note 2: Return error Msg from COI to user:
%% It was decided that the error message returned from COI to the COI interface 
%% user shall has the same content as a user receives when login on
%% interfaces cli, coli, etc. Those messages are handled by module omc_server.
%%
lookup_user(User) ->
	
    case comsaI:is_super_oam_user(User) of
        true ->
            Roles = [?SUPER_ROLE],
            Facility = 4, 		%% - security/authorization messages
            Msg = "Maintenance user login: " ++ User ++ ", Roles: " ++
            roles_to_string(Roles),
            coi:log_security(Facility, info, Msg),
            {true, Roles};   
        false ->
            case omc_api:ldap_lookup(User) of
                {true, Roles} ->
                    Facility = 4, 		%% - security/authorization messages
                    Msg = "LDAP: lookup for user: " ++ User ++
                    ", Authenticated: true, Roles: " ++
                    roles_to_string(Roles),
                    coi:log_security(Facility, info, Msg),
                    {true, Roles};
                {false, Reason} ->
                    Facility = 4, 		%% - security/authorization messages
                    Msg = "LDAP: lookup, Authenticated: " ++
				    "false, Reason: " ++ Reason,
                    coi:log_security(Facility, info, Msg),
                    {false, ?PERMISSION_DENIED}
            end
    end.
	
	
%%% ###########################################################################
%%% @doc Get the https ports from the node.
%%%
%%%   Example:
%%%   > coi:getHttpsPorts().
%%%   [{https,443},{https_login,8443}]
%%% @end
%%% ###########################################################################
-spec getHttpsPorts() -> [tuple()]. 
%%% ###=====================================================================###
getHttpsPorts() ->
    HttpsPorts = [{https, sysEnv:get_port_conf(https)},
        {https_login, sysEnv:get_port_conf(https_login)}],
    HttpsPorts.


%%% ###########################################################################
%%% Read MIB files and populate the MIB table.
%%%
%%% 
%%% ###=====================================================================###


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% Get a list of file names of all MIB files in the system.
%%%
%%%
%%% ###=====================================================================###

%%% ###########################################################################
%%% ----------------------------------------------------------
%% Translate from raw integer value in the MIM to the corresponding string
%%   defined in the MIB.
%%
%% 
%%% ###=====================================================================###



%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%% This function should only be used for test when rcssim or VRCS is running.
%% The function creates a fake user ("faketestuser") with some roles.
%%   Example:
%%   {ok,[{{roles,"faketestuser"},
%%          ["SystemAdministrator","SystemReadOnly","expert"]}]}
createFakeUser() ->
    %% Use fake user for VRCS (and for simulated node) until AI is in place
    %% profile is read in omc_ldap_server by calling
    %% omc_ldap_server localy instead of via gen_server call
    case os:getenv("USE_LDAP") =:= false of
	    true ->
	        Mode = sysEnv:rcs_mode_2(),
		    case Mode =:= vrcs orelse Mode =:= simulated of			 
		        true ->
                    put({roles,"faketestuser"},["SystemAdministrator","SystemReadOnly","expert"]),
                    {ok, [{{roles,"faketestuser"}, ["SystemAdministrator","SystemReadOnly", "expert"]}]};

		        _ ->
                    {false, "Not a simulated node"}
		    end;
		_ ->
            {false, "Not created due to that a LDAP server exists"}
	end.

%%% ###=====================================================================###
%%% This function should only be used for test when 
%%% rcssim or VRCS is running.
%%% The function deletes the fake user and its roles
%%% created with function "createFakeUser".
%%% (user= "faketestuser").
deleteFakeUser() ->
    case os:getenv("USE_LDAP") =:= false of
	    true ->
	        Mode = sysEnv:rcs_mode_2(),
		    case Mode =:= vrcs orelse Mode =:= simulated of			 
		        true ->
                    erase({roles,"faketestuser"}),
                    ok;
		        _ ->
                    {false, "Not a simulated node"}
		    end;
		_ ->
            {false, "Not created due to that a LDAP server exists"}
	end.

%%% ###=====================================================================###
%%% This function should only be used for test when 
%%% rcssim or VRCS is running.
%%% The function creates a fake maintenance user 
%%% (user= "faketestmaintuser", pwd = "Letmein01").
%%% maintenance user will be checked by module comsaI.
createFakeMaintenanceUser() ->
    Mode = sysEnv:rcs_mode_2(),
	case Mode =:= vrcs orelse Mode =:= simulated of			 
	    true ->
            %% Create a maintenance user 
            mnesia:dirty_write(
                {maintenanceUser,{"1","1","1","1","1","51"},undefined,"faketestmaintuser",
                {'EcimPassword',true,"Letmein01"}});
        _ ->
            {false, "Not a simulated node"}
    end.

%%% ###=====================================================================###
%%% This function should only be used for test when 
%%% rcssim or VRCS is running.
%%% The function deletes the fake maintenance user 
%%% created with function createFakeMaintenanceUser.
%%% (user= "faketestmaintuser", pwd = "Letmein01").
deleteFakeMaintenanceUser() ->
    Mode = sysEnv:rcs_mode_2(),
	case Mode =:= vrcs orelse Mode =:= simulated of			 
	    true ->
            %% Delete the maintenance user created with function "createFakeMaintenanceUser"
            mnesia:dirty_delete_object(
                {maintenanceUser,{"1","1","1","1","1","51"},undefined,"faketestmaintuser",
                {'EcimPassword',true,"Letmein01"}});
        _ ->
            {false, "Not a simulated node"}
    end.

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% 

%%% ###########################################################################
%%% roles_to_string
%%%
%%% ###=====================================================================###
roles_to_string(Roles) ->
    roles_to_string(Roles, []).

roles_to_string([], []) -> [];
roles_to_string([Role], Acc) -> %last
    lists:reverse(role(Role, Acc));
roles_to_string([R | T], Acc) -> %last
    roles_to_string(T, [$\s, $, | role(R, Acc)]).

%%% ###########################################################################
%%% role
%%%
%%% ###=====================================================================###
role([], Acc) -> Acc;
role([H | T], Acc) ->
    role(T, [H | Acc]).


%%% ###=====================================================================###


	


%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
