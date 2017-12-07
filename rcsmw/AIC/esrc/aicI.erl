%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aicI.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R6A/R7A/R10A/R11A/R12A/2
%%%
%%% @doc ==Initialization of Auto-Integration Control==
%%% This module contains the necessary initialization of auto
%%% integration control.
%%% ----------------------------------------------------------
-module(aicI).
-vsn('/main/R2A/R3A/R4A/R6A/R7A/R10A/R11A/R12A/2').
-date('2017-11-07').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      14-02-17   etxtory     Created
%%% R2A/2      14-06-26   etxtory     Interface for sec-files
%%% R2A/3      14-08-19   etxtory     Added securityFile
%%% R2A/4      14-08-20   etxtory     Interface for update OAM IP addr
%%% R2A/5      14-09-26   etxtory     Added interface to check for AI
%%% ----------------------------------------------------------
%%% R3A/1      15-02-02   etxtory     updated_oam_ip_data via aicServer
%%%				      to avoid interference with AI (update_snmp)
%%% R3A/2      15-03-02   etxtory     Added interface for clean up at
%%%                                   COLI factory reset (called from sysNetloader)
%%% R3A/3      15-05-07   etxtory     Added clean_up/1 for soft/hard
%%% ----------------------------------------------------------
%%% R4A/1      15-06-25   etxtory     HT85636 - Better is_ai_ongoing
%%% R4A/2      15-08-28   etxtory     HU12176
%%% R4A/3      15-11-10   etxtory     get_backup_name (called by SWM)
%%% ----------------------------------------------------------
%%% R11A/1-2   17-08-06   uabesvi     added heartbeat/0
%%% R11A/3     17-10-10   etxderb     Modified get_lkf*, get_sec* for vrcs
%%% ----------------------------------------------------------
%%% R12A/1     17-10-26   qselert     Added ready_for_service callback functionality for SP086
%%% R12A/2     17-11-07   etxpeno     Add send_change_ip_address_trap/0
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([get_backup_name/0]).
-export([get_lkf_fn/0]).
-export([get_sec_fn/0]).
-export([is_ai_ongoing/0]).
-export([is_ready_for_service/0]).
-export([register_ready_for_service_cb/1]).
-export([updated_oam_ip_data/1]).
-export([clean_up/0, clean_up/1]).
-export([heartbeat/0]).
-export([send_change_ip_address_trap/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RmeAI.hrl").
-include("aic.hrl").

%% Configuration file produced by NL
-define(LKF_FILE, "licensingKeyFile.xml").

%% Certificate file produced by NL
-define(CERT_FILE, "certificateFile.tar").

%% Security file produced by NL
-define(SEC_FILE, "securityFile").



%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Returns the name of AI-backup.
%%% This interface is used by SWM so the SWM housekeeping
%%% mechanism does not remove the AI-backup. AIC removes the
%%% AI-backup when node reaches RbsConfigLevel=READY_FOR_SERVICE.
%%% See aicServer.erl for more information.
%%% @end
%%% ----------------------------------------------------------
get_backup_name() ->
    aicServer:get_backup_name().

%%% ----------------------------------------------------------
%%% @doc Gets the full path and filename to the LKF-file.
%%% This function is called by LMA at startup to check if
%%% an LMF-file was included in initial installation (NL).
%%% LMA needs to remove the file when ready.
%%% @end
%%% ----------------------------------------------------------
get_lkf_fn() ->
    LKF = aicServer:get_cfgfiles_path(?LKF_FILE),
    case file:read_file_info(LKF) of
        {ok, _} ->
            {ok, LKF};
        {error, Reason} ->
            {error, Reason}
    end.

%%% ----------------------------------------------------------
%%% @doc Gets the full path and filename to the CERT-file.
%%% It also return the content of the SEC-FILE.
%%% This function is called by CERT at startup to check if
%%% an CERT-file/SEC-FILE was included in initial installation (NL).
%%% CERT needs to remove the CERT-file when ready.
%%% SEC-FILE is handled by AIC
%%% @end
%%% ----------------------------------------------------------
get_sec_fn() ->
    CertFile = aicServer:get_cfgfiles_path(?CERT_FILE),
    case file:read_file_info(CertFile) of
	{ok, _} ->
	    Password = get_sec_file(),
	    {ok, {CertFile, Password}};
	{error, Reason} ->
	    {error, Reason}
    end.

get_sec_file() ->
    SecFile = aicServer:get_cfgfiles_path(?SEC_FILE),
    case file:read_file(SecFile) of
	{ok, Bin} ->
	    List = binary_to_list(Bin),
	    [Password | _] = string:tokens(List, "\n"),
	    Password;
	_ ->
	    ""
    end.

%%% ----------------------------------------------------------
%%% @doc Check if any autointegration is ongoing.
%%% true - Autointegration ongoing
%%% false - No autointegration ongoing
%%% @end
%%% ----------------------------------------------------------
is_ai_ongoing() ->
    aicServer:is_ai_ongoing().

%%% ----------------------------------------------------------
%%% @doc Check if ready for service is set to true.
%%% true  - RFS
%%% false - not RFS
%%% @end
%%% ----------------------------------------------------------
is_ready_for_service() ->
    aicServer:is_ready_for_service().


register_ready_for_service_cb(CbModule) ->
    aicServer:register_ready_for_service_cb(CbModule).

%%% ----------------------------------------------------------
%%% @doc Called from OOT when any data is updated.
%%% @end
%%% ----------------------------------------------------------
updated_oam_ip_data(OamIpData) ->
    aicServer:update_oam_ip_data(OamIpData).

%%% ----------------------------------------------------------
%%% @doc Called from SYS to clean up before factory reset.
%%% @end
%%% ----------------------------------------------------------
clean_up() ->
    clean_up(soft).

clean_up(soft) ->
    %% Remove all files except:
    %% nl_esi_dirs - ESI directory information
    %% config_lab.sh - Lab configuration
    Keep = ["nl_esi_dirs", "config_lab.sh"],
    Dir = filename:join([sysEnv:rcs_dir(), "networkloader"]),
    case file:list_dir(Dir) of
	{ok, Fs} ->
	    RemoveFs = Fs -- Keep,
	    [file:delete(filename:join([Dir, File])) || File <- RemoveFs];
	_Other ->
	    ok
    end;
clean_up(hard) ->
    %% Remove all files except.
    Dir = filename:join([sysEnv:rcs_dir(), "networkloader"]),
    case file:list_dir(Dir) of
	{ok, Fs} ->
	    [file:delete(filename:join([Dir, File])) || File <- Fs];
	_Other ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc  returns {Status, Detail, ErrorResponse}
%%%
%%% Status           = ?HB_STARTING | ?HB_OPERATIONAL | ?HB_FAILED
%%% Detail           = string() | undefined
%%%                    Valid only for Status = ?HB_STARTING
%%%                    Max length is 255 chars
%%% ErrorResponse    = undefined | {HttpResCode, ErrorDetail, AdditionalDetail}
%%%                    Only valid for Status = ?HB_FAILED
%%% HttpResCode      = HTTP result codes
%%% ErrorDetail      = string()
%%%                    Max length is 255 chars
%%% AdditionalDetail = string()
%%%                    Max length is 255 chars
%%%
%%% This interface is used by VNFC when heartbeat
%%% is received from VNMF.
%%%
%%% This function is only called as long as AI is executing,
%%% i.e. until AI has returned ?HTTP_OK (200) or an error code
%%% other than ?HTTP_ACCEPTED (202) or ?HTTP_SERVICE_UNAVAILABLE (503)
%%%
%%% @end
%%% ----------------------------------------------------------
heartbeat() ->
    aicHbServer:heartbeat().


%%% ----------------------------------------------------------
%%% @doc Request sending of the eriChangeIPAddressEvent trap
%%% @end
%%% ----------------------------------------------------------
send_change_ip_address_trap() ->
    aicServer:send_change_ip_address_trap().
