%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaLinxHandler.erl %
%%% @author echhedb
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R9A/R11A/R12A/2
%%% @doc ==Interface module towards GLMS==
%%% This module implements the interface and handle the parsing of the massages 
%%% from and towards the GLMS component.
%%% @end
%%% ----------------------------------------------------------
-module(lmaLinxHandler).
-vsn('/main/R2A/R3A/R4A/R5A/R9A/R11A/R12A/2').
-date('2017-11-08').
-author('echhedb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: lmaLinxHandler.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R3A/1      2014-10-06   etxpejn    Added LIHI signals 
%%% R3A/3      2014-11-26   etxpejn    Added capacity signals 
%%% R3A/4      2014-12-05   etxpejn    Corrected create and modify GP MO
%%% R3A/5      2014-12-18   etxpejn    Added update_grace_period_attributes
%%% R3A/6      2015-01-22   etxpejn    Changed GrantedCapacityLevel to signed
%%% R3A/7      2015-01-27   etxpejn    Changed Value to signed :)
%%% R3A/9      2015-03-26   etxpejn    Corrected HT59552, added ActiveFeatureKeyId
%%% R3A/10     2015-03-30   etxpejn    Added ActiveFeatureKeyId for capacity state
%%% R3A/11     2015-04-09   etxpejn    Corrected hunt bug
%%% R4A/1      2015-05-08   etxpejn    Corr bug in send_persistant_param_index_list_rsp
%%% R4A/3      2015-06-24   etxpejn    Added functionality for IP sec - embargo
%%% R4A/4      2015-07-26   etxpejn    HT92440: Added read_mo_grace_period
%%% R4A/5      2015-09-17   etxpejn    Changed error_logger to sysInitI
%%% R4A/6      2015-09-30   etxpejn    HU22309: {badarg,[{erlang,list_to_binary in 
%%%                                    send_soft_param_index_list_rsp
%%% R5A/1      2016-01-25   etxpejn    Added *_alarm_rsp signals for alarm correlation
%%% R9A/1      2017-04-11   etxpejn    HV32829, added license_key_exiration_alarm
%%% R11A/1     2017-07-13   qselert    SP086 updates
%%% R12A/1     2017-10-31   echhedb    SP086: Added more SP086 functionality.
%%% R12A/2     2017-11-08   echhedb    SP086: Corrected an error by changing an
%%%                                    attribute's length in a signal.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([parse_itc/1]).

-export([
	 send_activate/5,
	 send_activate_eu/2,
	 send_activate_iu/2,
	 send_activate_pu/2,
	 send_conn_to_server/2,
	 send_create_mo_rsp/5,
	 send_deactivate/2,
	 send_deactivate_pu/2,
	 send_delete_mo_rsp/5,
	 send_download_key_file_rsp/4,
	 send_dump_capacity_key_data_req/2,
	 send_dump_feature_key_data_req/2,
	 send_dump_glms_state_data_req/2,
	 send_dump_lcci_client_data_req/2,
	 send_dump_lfci_client_data_req/2,
	 send_feature_subscribe_req/6,
	 send_get_capacity_key_mo_list_req/2,
	 send_get_capacity_state_mo_list_req/2,
	 send_get_feature_key_mo_list_req/2,
	 send_get_feature_state_mo_list_req/2,
	 send_get_key_file_loc_rsp/4,
	 send_heartbeat_req/2,
	 send_install_keyfile_req/4,
	 send_keyfile_fault_alarm_rsp/3,
	 send_license_key_not_available_alarm_rsp/5,
	 send_ns_hunt_req/2,
	 send_persistent_parameter_get_rsp/7,
	 send_persistant_param_delete_index_rsp/4,
	 send_persistant_param_index_list_rsp/4,
	 send_persistant_param_set_rsp/4,
	 send_pki_verification_rsp/4,
	 send_read_am_mo_req/2,
	 send_read_licensesupport_mo_req/2,
	 send_read_eu_mo_req/2,
	 send_read_iu_mo_req/2,
	 send_read_mo_install_keyfile_report_progress_req/2,
	 send_read_mo_key_file_information_req/2,
	 send_read_mo_lm_req/2,
	 send_read_mo_req/4,
	 send_refresh_license_inventory/2,
	 send_set_feature_state_req/4,
	 send_set_fingerprint_req/3,
	 send_software_parameter_delete_index_rsp/4,
	 send_software_parameter_get_rsp/7,
	 send_soft_param_index_list_rsp/4,
	 send_soft_param_set_rsp/4,
	 send_store_key_file_rsp/4,
	 send_subscribe_mo_updates_req/2,
	 send_update_grace_period_attributes_req/6,
	 send_feature_configuration_list_rsp/5,
     send_install_area_license_keys_req/4,
	 send_audit_req/2,
	 send_update_area_id_req/3
	]).

-define(GLMS_MO_KEY_LEN, 34).
-define(GLMS_MO_KEY_LEN_36, 36).
-define(GLMS_KEY_ID_LEN, 24).
-define(GLMS_KEY_NAME_LEN, 128).
-define(GLMS_DESCRIPTION_LEN, 128).
-define(GLMS_FINGERPRINT_LEN, 256).
-define(GLMS_PRODUCT_TYPE_LEN, 128). 
-define(GLMS_PRODUCT_TYPE_LEN_130, 130).
-define(GLMS_TABLE_NAME_LEN, 80).
-define(GLMS_RESULT_INFO_LEN, 200).
-define(GLMS_CAPACITY_UNIT_LEN, 31).
-define(GLMS_AREA_ID_LEN, 256).

-define(CLIENT_REF, 12).
-define(PROTOCOL_VERS, 1).

-define(OPERABLE, 1).
-define(INOPERABLE, 0).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lmaGlmsSig.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Parse calls from GLMS
%%% @end
%%% ----------------------------------------------------------
-spec parse_itc(tuple()) -> tuple().

parse_itc({?GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_REQ,
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary>>}) ->
    {software_parameter_index_list_req, RequestId, to_string(Table)};

parse_itc({?GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_REQ,
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary,
	      Index:4/native-unsigned-integer-unit:8>>}) ->
    {software_parameter_delete_index_req, RequestId, to_string(Table), Index};

parse_itc({?GLMS_ADPI_SOFTWARE_PARAMETER_SET_REQ,
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary,
	      Index:4/native-unsigned-integer-unit:8,
	      Value/binary>>}) ->
    {software_parameter_set_req, RequestId, to_string(Table), Index, Value};

parse_itc({?GLMS_ADPI_SOFTWARE_PARAMETER_GET_REQ,
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary>>}) ->
    {software_parameter_get_req, RequestId, to_string(Table), Table};

parse_itc({?GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_REQ,
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary>>}) ->
    {persistent_parameter_index_list_req, RequestId, to_string(Table)};

parse_itc({?GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_REQ, 
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary,
	      Index:4/native-unsigned-integer-unit:8>>}) ->
    {persistent_parameter_delete_index_req, RequestId, to_string(Table), Index};

parse_itc({?GLMS_ADPI_PERSISTENT_PARAMETER_SET_REQ,
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary,
	      Index:4/native-unsigned-integer-unit:8,
	      Value/binary>>}) ->
    {persistent_parameter_set_req, RequestId, to_string(Table), Index, Value};

parse_itc({?GLMS_ADPI_PERSISTENT_PARAMETER_GET_REQ,
	    <<RequestId:4/native-unsigned-integer-unit:8,
	      Table:?GLMS_TABLE_NAME_LEN/binary>>}) ->
    {persistent_parameter_get_req, RequestId, to_string(Table), Table};

parse_itc({?GLMS_ADPI_ACTIVATE_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      HighestSupportedPv:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {activate_rsp, GlmsResult, HighestSupportedPv, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_ACTIVATE_EU_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {activate_eu_rsp, GlmsResult, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_ACTIVATE_IU_RSP, 
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {activate_iu_rsp, GlmsResult, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_ACTIVATE_PU_RSP,
	    <<Expiration:4/native-signed-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8,
	      ActivationsLeft:4/native-unsigned-integer-unit:8,
	      PuDeactivated:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {activate_pu_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, PuDeactivated, 
     to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_DEACTIVATE_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {deactivate_rsp, GlmsResult, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_DEACTIVATE_PU_RSP,
	    <<Expiration:4/native-signed-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8,
	      ActivationsLeft:4/native-unsigned-integer-unit:8,
	      PuDeactivated:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {deactivate_pu_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, PuDeactivated, 
     to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_SUBSCRIBE_MO_UPDATES_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {subscribe_mo_updates_rsp, GlmsResult, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_CREATE_CAPACITY_KEY_MO_REQ,
	   <<NoLimit:4/native-unsigned-integer-unit:8,
	     Value:4/native-signed-integer-unit:8,
	     ValidFrom:4/native-unsigned-integer-unit:8,
	     Expiration:4/native-signed-integer-unit:8,
	     LicensedCapacityLimitReached:4/native-unsigned-integer-unit:8,
	     GrantedCapacityLevel:4/native-signed-integer-unit:8,
	     CapacityUnit:?GLMS_CAPACITY_UNIT_LEN/binary,
	     CapacityKeyId:?GLMS_MO_KEY_LEN/binary,
	     KeyId:?GLMS_KEY_ID_LEN/binary,
	     Name:?GLMS_KEY_NAME_LEN/binary,
	     ProductType/binary>>}) ->
    {create_capacity_key_mo_req, NoLimit, Value, ValidFrom, Expiration, 
     LicensedCapacityLimitReached, GrantedCapacityLevel, to_string(CapacityUnit), 
     to_string(CapacityKeyId), to_string(KeyId), to_string(Name), to_string(ProductType)};

parse_itc({?GLMS_ADPI_CREATE_CAPACITY_STATE_MO_REQ,
	   <<NoLimit:4/native-unsigned-integer-unit:8,
	     Value:4/native-signed-integer-unit:8,
	     LicensedCapacityLimitReached:4/native-signed-integer-unit:8,  
	     GrantedCapacityLevel:4/native-signed-integer-unit:8,
	     LicenseState :4/native-unsigned-integer-unit:8, 
	     CapacityUnit:?GLMS_CAPACITY_UNIT_LEN/binary,
	     CapacityStateId:?GLMS_MO_KEY_LEN/binary,
	     Description:?GLMS_DESCRIPTION_LEN/binary,
	     KeyId:?GLMS_KEY_ID_LEN/binary,
	     ActiveCapacityKeyId/binary>>}) ->
    {create_capacity_state_mo_req, NoLimit, Value, LicensedCapacityLimitReached, 
     GrantedCapacityLevel, LicenseState, to_string(CapacityUnit), to_string(CapacityStateId),
     to_string(Description), to_string(KeyId), to_string(ActiveCapacityKeyId)};

parse_itc({?GLMS_ADPI_CREATE_FEATURE_KEY_MO_REQ,
    	    <<ValidFrom:4/native-signed-integer-unit:8,
	      Expiration:4/native-signed-integer-unit:8,
	      Shared:4/native-unsigned-integer-unit:8,
	      FeatureKeyId:?GLMS_MO_KEY_LEN/binary,
	      KeyId:?GLMS_KEY_ID_LEN/binary,
	      Name:?GLMS_KEY_NAME_LEN/binary,
	      ProductType/binary>>}) ->
    {create_feature_key_mo_req, ValidFrom, Expiration, Shared, to_string(FeatureKeyId), to_string(KeyId), 
     to_string(Name), to_string(ProductType)};

parse_itc({?GLMS_ADPI_DELETE_CAPACITY_KEY_MO_REQ,
	   <<CapacityKeyId/binary>>}) ->
    {delete_capacity_key_mo_req, to_string(CapacityKeyId)};

parse_itc({?GLMS_ADPI_DELETE_CAPACITY_STATE_MO_REQ,
	   <<CapacityStateId/binary>>}) ->
    {delete_capacity_state_mo_req, to_string(CapacityStateId)};

parse_itc({?GLMS_ADPI_DELETE_FEATURE_KEY_MO_REQ,
	    <<FeatureKeyId/binary>>}) ->
    {delete_feature_key_mo_req, to_string(FeatureKeyId)};

parse_itc({?GLMS_ADPI_DELETE_GRACE_PERIOD_MO_REQ,
	   <<GracePeriodId/binary>>}) ->
    {delete_grace_period_mo_req, to_string(GracePeriodId)};

parse_itc({?GLMS_ADPI_CREATE_FEATURE_STATE_MO_REQ,
	    <<FeatureState:4/native-unsigned-integer-unit:8,
	      LicenseState:4/native-unsigned-integer-unit:8,
	      ServiceState:4/native-unsigned-integer-unit:8,
	      FeatureStateId:?GLMS_MO_KEY_LEN/binary,
	      Description:?GLMS_DESCRIPTION_LEN/binary,
	      KeyId:?GLMS_KEY_ID_LEN/binary,
	      ActiveFeatureKeyId:?GLMS_MO_KEY_LEN/binary>>}) ->
    {create_feature_state_mo_req, FeatureState, LicenseState, ServiceState, 
     to_string(FeatureStateId), to_string(Description), to_string(KeyId), 
     to_string(ActiveFeatureKeyId)};

parse_itc({?GLMS_ADPI_CREATE_GRACE_PERIOD_MO_REQ,
	   <<Expiration:4/native-signed-integer-unit:8,
	     ConfiguredLength:4/native-unsigned-integer-unit:8,
	     ConfiguredResetThreshold:4/native-unsigned-integer-unit:8,
	     ConfiguredActivationThreshold:4/native-unsigned-integer-unit:8,
	     GracePeriodState:4/native-unsigned-integer-unit:8,
	     GracePeriodId/binary>>}) ->
    {create_grace_period_mo_req, Expiration, ConfiguredLength, ConfiguredResetThreshold, 
     ConfiguredActivationThreshold, GracePeriodState, to_string(GracePeriodId)};

parse_itc({?GLMS_ADPI_DELETE_FEATURE_STATE_MO_REQ,
	    <<FeatureStateId/binary>>}) ->
    {delete_feature_state_mo_req, to_string(FeatureStateId)};

parse_itc({?GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_IND,
	   <<NoLimit:4/native-signed-integer-unit:8, 
	     Value:4/native-signed-integer-unit:8, 
	     LicensedCapacityLimitReached:4/native-signed-integer-unit:8, 
	     LicenseState:4/native-signed-integer-unit:8, 
	     GrantedCapacityLevel:4/native-signed-integer-unit:8,
	     CapacityStateId:?GLMS_KEY_ID_LEN/binary,
	     Description:?GLMS_DESCRIPTION_LEN/binary,
	     KeyId:?GLMS_KEY_ID_LEN/binary,
	     ActiveCapacityKeyId/binary>>}) -> 
    {mo_update_capacity_state_ind, NoLimit, Value, LicensedCapacityLimitReached, LicenseState, 
     GrantedCapacityLevel, to_string(CapacityStateId), to_string(Description), to_string(KeyId),
     to_string(ActiveCapacityKeyId)};

parse_itc({?GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_CAPACITY_UNIT_IND,
	   <<CapacityUnit:?GLMS_CAPACITY_UNIT_LEN/binary,
	     CapacityKeyId/binary>>}) ->
    {mo_update_capacity_key_capacity_unit_ind, to_string(CapacityUnit), to_string(CapacityKeyId)};

parse_itc({?GLMS_ADPI_MO_UPDATE_CAPACITY_STATE_CAPACITY_UNIT_IND,
	   <<CapacityUnit:?GLMS_CAPACITY_UNIT_LEN/binary,
	     CapacityStateId/binary>>}) ->
    {mo_update_capacity_state_capacity_unit_ind, to_string(CapacityUnit), 
     to_string(CapacityStateId)};

parse_itc({?GLMS_ADPI_MO_UPDATE_LM_IND,
	    <<LastInventoryChange:4/native-signed-integer-unit:8,
	      LastLicenseInventoryRefresh:4/native-signed-integer-unit:8,
	      FingerprintUpdateable:4/native-unsigned-integer-unit:8,
	      LmState:4/native-unsigned-integer-unit:8,
	      Fingerprint:?GLMS_FINGERPRINT_LEN/binary,
	      _ReferenceToLicenseServer/binary>>}) ->
    {mo_update_lm_ind, to_string(Fingerprint), FingerprintUpdateable, LmState, LastInventoryChange, 
     LastLicenseInventoryRefresh};

parse_itc({?GLMS_ADPI_READ_MO_LM_RSP,
	    <<LastInventoryChange:4/native-signed-integer-unit:8,
	      LastLicenseInventoryRefresh:4/native-signed-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      FingerprintUpdateable:4/native-unsigned-integer-unit:8, 
	      LmState:4/native-unsigned-integer-unit:8,
	      Fingerprint:?GLMS_FINGERPRINT_LEN/binary,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      _ReferenceToLicenseServer/binary>>}) ->
    {read_mo_lm_rsp, GlmsResult, to_string(Fingerprint), FingerprintUpdateable, LmState, 
     LastInventoryChange, LastLicenseInventoryRefresh, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_MO_UPDATE_EU_IND,
	    <<Expiration:4/native-signed-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8,
	      ActivationsLeft:4/native-unsigned-integer-unit:8>>}) ->
    {mo_update_eu_ind, ActivationState, Expiration, ActivationsLeft};

parse_itc({?GLMS_ADPI_READ_EU_MO_RSP,
	    <<Expiration:4/native-signed-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8,
	      ActivationsLeft:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {read_eu_mo_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, 
     to_string(ResultInfo)};



% SP086
parse_itc({?GLMS_ADPI_MO_UPDATE_LICENSE_SUPPORT_IND,
	    <<AreaId/binary>>}) ->
    {mo_update_licensesupport_ind, AreaId};

% SP086
parse_itc({?GLMS_ADPI_READ_MO_LICENSE_SUPPORT_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      AreaId:?GLMS_AREA_ID_LEN/binary>>}) ->
    {read_licensesupport_mo_rsp, GlmsResult, to_string(ResultInfo), to_string(AreaId)};




parse_itc({?GLMS_ADPI_MO_UPDATE_IU_IND,
	    <<Expiration:4/native-signed-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8,
	      ActivationsLeft:4/native-unsigned-integer-unit:8>>}) ->
    {mo_update_iu_ind, ActivationState, Expiration, ActivationsLeft};

parse_itc({?GLMS_ADPI_READ_AM_MO_RSP, 
	    <<Expiration:4/native-signed-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary>>}) ->
     {read_am_mo_rsp, GlmsResult, Expiration, ActivationState, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_READ_IU_MO_RSP,
	    <<Expiration:4/native-signed-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8,
	      ActivationsLeft:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {read_iu_mo_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, 
     to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_MO_UPDATE_AM_IND, 
	    <<Expiration:4/native-signed-integer-unit:8,
	      ActivationState:4/native-unsigned-integer-unit:8>>}) ->
    {mo_update_am_ind, ActivationState, Expiration};

parse_itc({?GLMS_ADPI_MO_UPDATE_FEATURE_STATE_IND,
	    <<FeatureState:4/native-unsigned-integer-unit:8,
	      LicenseState:4/native-unsigned-integer-unit:8,
	      ServiceState:4/native-unsigned-integer-unit:8,
	      FeatureStateId:?GLMS_MO_KEY_LEN/binary,
	      Description:?GLMS_DESCRIPTION_LEN/binary,
	      KeyId:?GLMS_KEY_ID_LEN/binary,
	      ActiveFeatureKeyId:?GLMS_MO_KEY_LEN/binary>>}) ->
    {mo_update_feature_state_ind, FeatureState, LicenseState, ServiceState, 
     to_string(FeatureStateId), to_string(Description), to_string(KeyId), to_string(ActiveFeatureKeyId)};

parse_itc({?GLMS_ADPI_MO_UPDATE_GRACE_PERIOD_IND,
	   <<Expiration:4/native-signed-integer-unit:8,
	     ConfiguredLength:4/native-signed-integer-unit:8,
	     ConfiguredResetThreshold:4/native-signed-integer-unit:8,
	     ConfiguredActivationThreshold:4/native-signed-integer-unit:8,
	     GracePeriodState:4/native-unsigned-integer-unit:8,
	     GracePeriodId/binary>>}) ->
    {mo_update_grace_period_ind, Expiration, ConfiguredLength, ConfiguredResetThreshold, 
     ConfiguredActivationThreshold, GracePeriodState, to_string(GracePeriodId)};

parse_itc({?GLMS_ADPI_READ_MO_CAPACITY_KEY_RSP,
	   <<NoLimit:4/native-unsigned-integer-unit:8,
	     Value:4/native-signed-integer-unit:8,
	     ValidFrom:4/native-signed-integer-unit:8,
	     Expiration:4/native-signed-integer-unit:8,
	     LicensedCapacityLimitReached:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8,
	     GrantedCapacityLevel:4/native-signed-integer-unit:8,
	     CapacityUnit:?GLMS_CAPACITY_UNIT_LEN/binary,
	     ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	     CapacityKeyId:?GLMS_MO_KEY_LEN/binary,
	     KeyId:?GLMS_KEY_ID_LEN/binary,
	     Name:?GLMS_KEY_NAME_LEN/binary,
	     ProductType/binary>>}) ->
    {read_mo_capacity_key_rsp, NoLimit, Value, ValidFrom, Expiration, LicensedCapacityLimitReached,
     Result, GrantedCapacityLevel, to_string(CapacityUnit), to_string(ResultInfo), 
     to_string(CapacityKeyId), to_string(KeyId), to_string(Name), to_string(ProductType)};

parse_itc({?GLMS_ADPI_READ_MO_CAPACITY_STATE_RSP,
	   <<NoLimit:4/native-unsigned-integer-unit:8,
	     Value:4/native-signed-integer-unit:8,
	     LicensedCapacityLimitReached:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8,
	     LicenseState:4/native-unsigned-integer-unit:8,
	     GrantedCapacityLevel:4/native-signed-integer-unit:8,
	     CapacityUnit:?GLMS_CAPACITY_UNIT_LEN/binary,
	     CapacityStateId:?GLMS_MO_KEY_LEN/binary,
	     Description:?GLMS_DESCRIPTION_LEN/binary,
	     KeyId:?GLMS_KEY_ID_LEN/binary,
	     ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	     ActiveCapacityKeyId/binary>>}) ->
    {read_mo_capacity_state_rsp, NoLimit, Value, LicensedCapacityLimitReached, Result, LicenseState,
     GrantedCapacityLevel, to_string(CapacityUnit), to_string(CapacityStateId), 
     to_string(Description), to_string(KeyId), to_string(ResultInfo), to_string(ActiveCapacityKeyId)};

parse_itc({?GLMS_ADPI_READ_MO_FEATURE_KEY_RSP,
	    <<ValidFrom:4/native-signed-integer-unit:8,
	      Expiration:4/native-signed-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      FeatureKeyId:?GLMS_MO_KEY_LEN/binary,
	      KeyId:?GLMS_KEY_ID_LEN/binary,
	      Name:?GLMS_KEY_NAME_LEN/binary,
	      ProductType:?GLMS_PRODUCT_TYPE_LEN_130/binary,
	      Shared:4/native-unsigned-integer-unit:8>>}) ->
    {read_mo_feature_key_rsp, GlmsResult, ValidFrom, Expiration, to_string(FeatureKeyId), 
     to_string(KeyId), to_string(Name), to_string(ProductType), Shared, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_READ_MO_FEATURE_STATE_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      FeatureState:4/native-unsigned-integer-unit:8,
	      LicenseState:4/native-unsigned-integer-unit:8,
	      ServiceState:4/native-unsigned-integer-unit:8,
	      FeatureStateId:?GLMS_MO_KEY_LEN/binary,
	      Description:?GLMS_DESCRIPTION_LEN/binary,
	      KeyId:?GLMS_KEY_ID_LEN/binary,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      ActiveFeatureKeyId:?GLMS_MO_KEY_LEN/binary>>}) ->
    {read_mo_feature_state_rsp, GlmsResult, FeatureState, LicenseState, ServiceState, 
     to_string(FeatureStateId), to_string(Description), to_string(KeyId), to_string(ResultInfo),
     to_string(ActiveFeatureKeyId)};

parse_itc({?GLMS_ADPI_READ_MO_GRACE_PERIOD_RSP,
	   <<Expiration:4/native-signed-integer-unit:8,
	     ConfiguredLength:4/native-signed-integer-unit:8,
	     ConfiguredResetThreshold:4/native-signed-integer-unit:8,
	     ConfiguredActivationThreshold:4/native-signed-integer-unit:8,
	     GracePeriodState:4/native-unsigned-integer-unit:8,
	     GracePeriodId:?GLMS_MO_KEY_LEN/binary,
	     GlmsResult:4/native-unsigned-integer-unit:8,
	     _CapacityStateId:?GLMS_MO_KEY_LEN/binary,
	     ResultInfo/binary>>}) ->
    {read_mo_grace_period_rsp, Expiration, ConfiguredLength, ConfiguredResetThreshold, 
     ConfiguredActivationThreshold, GracePeriodState, to_string(GracePeriodId), GlmsResult, 
     to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      NrOfCapacityKeyIds:4/native-unsigned-integer-unit:8,
	      CapacityKeyId/binary>>}) ->
    {get_capacity_key_mo_list_rsp, GlmsResult, NrOfCapacityKeyIds, CapacityKeyId};

parse_itc({?GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      NrOfCapacityStateIds:4/native-unsigned-integer-unit:8,
	      CapacityStateId/binary>>}) ->
    {get_capacity_state_mo_list_rsp, GlmsResult, NrOfCapacityStateIds, CapacityStateId};

parse_itc({?GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      NrOfFeatureKeyIds:4/native-unsigned-integer-unit:8,
	      FeatureKeyId/binary>>}) ->
    {get_feature_key_mo_list_rsp, GlmsResult, NrOfFeatureKeyIds, FeatureKeyId};

parse_itc({?GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      NrOfFeatureStateIds:4/native-unsigned-integer-unit:8,
	      FeatureStateId/binary>>}) ->
    {get_feature_state_mo_list_rsp, GlmsResult, NrOfFeatureStateIds, FeatureStateId};

parse_itc({?GLMS_ADPI_MO_UPDATE_KEY_FILE_IND, 
	    <<GlmsAsyncAction:348/binary,
	      InstallationTime:4/native-signed-integer-unit:8,
	      SequenceNumber:4/native-unsigned-integer-unit:8,
	      Locatable:4/native-unsigned-integer-unit:8,
	      ProductType/binary>>}) ->

    <<TimeActionCompleted:4/native-signed-integer-unit:8,
      TimeActionStarted:4/native-signed-integer-unit:8,
      TimeOfLastStatusUpdate:4/native-signed-integer-unit:8,
      Result:4/native-unsigned-integer-unit:8,
      Glms_State:4/native-unsigned-integer-unit:8,
      ActionId:4/native-unsigned-integer-unit:8,
      ProgressPercentage:4/native-unsigned-integer-unit:8,
      ActionName:20/binary,
      AdditionalInfo:100/binary,
      ProgressInfo:100/binary,
      ResultInfo:100/binary>> = GlmsAsyncAction, 

    {mo_update_keyfile_ind, SequenceNumber, InstallationTime, Locatable, ActionId, 
     to_string(ActionName), to_string(AdditionalInfo), to_string(ProgressInfo), ProgressPercentage, 
     Result, to_string(ResultInfo), Glms_State, TimeActionCompleted, TimeActionStarted, 
     TimeOfLastStatusUpdate, to_string(ProductType)};

parse_itc({?GLMS_ADPI_MO_UPDATE_CAPACITY_KEY_NAME_IND,
	    <<CapacityKeyId:?GLMS_MO_KEY_LEN/binary,
	      Name/binary>>}) ->
    {mo_update_capacity_key_name_ind, to_string(CapacityKeyId), to_string(Name)};

parse_itc({?GLMS_ADPI_MO_UPDATE_FEATURE_KEY_NAME_IND,
	    <<FeatureKeyId:?GLMS_MO_KEY_LEN/binary,
	      Name/binary>>}) ->
    {mo_update_feature_key_name_ind, to_string(FeatureKeyId), to_string(Name)};

parse_itc({?GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_RSP,
	    <<GlmsAsyncAction:348/binary,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->

    <<TimeActionCompleted:4/native-signed-integer-unit:8,
      TimeActionStarted:4/native-signed-integer-unit:8,
      TimeOfLastStatusUpdate:4/native-signed-integer-unit:8,
      Result:4/native-unsigned-integer-unit:8,
      Glms_State:4/native-unsigned-integer-unit:8,
      ActionId:4/native-unsigned-integer-unit:8,
      ProgressPercentage:4/native-unsigned-integer-unit:8,
      ActionName:20/binary,
      AdditionalInfo:100/binary,
      ProgressInfo:100/binary,
      ResultInfo2:100/binary>> = GlmsAsyncAction, 

    {read_mo_install_keyfile_report_progress_rsp, GlmsResult, ActionId, to_string(ActionName), 
     to_string(AdditionalInfo), to_string(ProgressInfo), ProgressPercentage, Result, 
     to_string(ResultInfo), Glms_State, TimeActionCompleted, TimeActionStarted, 
     TimeOfLastStatusUpdate, to_string(ResultInfo2)};

parse_itc({?GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_RSP,
	    <<InstallationTime:4/native-signed-integer-unit:8,
	      GlmsResult:4/native-unsigned-integer-unit:8,
	      SequenceNumber:4/native-unsigned-integer-unit:8,
	      Locatable:4/native-unsigned-integer-unit:8,
	      ProductType:?GLMS_PRODUCT_TYPE_LEN/binary,
	      ResultInfo/binary>>}) ->
    {read_mo_keyfile_information_rsp, GlmsResult, SequenceNumber, InstallationTime, 
     Locatable, to_string(ProductType), to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_GET_KEY_FILE_LOCATION_REQ,
	    _Data}) ->
    {get_keyfile_location_req};

parse_itc({?GLMS_ADPI_INSTALL_KEY_FILE_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ActionId:4/native-unsigned-integer-unit:8>>}) ->
    {install_keyfile_rsp, GlmsResult, ActionId};

%SP086
parse_itc({?GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_RSP,
	    <<ResultCode:4/native-unsigned-integer-unit:8>>}) ->
    {install_area_license_keys_rsp,  ResultCode};	

%SP086
parse_itc({?GLMS_ADPI_UPDATE_AREA_ID_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
          ResultInfo:?GLMS_RESULT_INFO_LEN/binary>>}) ->
    {update_area_id_rsp, GlmsResult, to_string(ResultInfo)};		
%SP086
parse_itc({?GLMS_ADPI_STATE_MO_AUDIT_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
          ResultInfo:?GLMS_RESULT_INFO_LEN/binary>>}) ->
    {state_mo_audit_rsp, GlmsResult, to_string(ResultInfo)};	

%SP086
parse_itc({?GLMS_ADPI_FEATURE_CONFIGURATION_LIST_REQ,<<>>}) ->
    {feature_configuration_list_req};		

parse_itc({?GLMS_ADPI_INSTALL_KEY_FILE_IND,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      AdditionalInfo:256/binary>>}) ->
    {install_keyfile_ind, GlmsResult, to_string(AdditionalInfo)};

parse_itc({?GLMS_ADPI_STORE_KEY_FILE_REQ,
	    <<KFLocationBin/binary>>}) ->
    {store_keyfile_req, to_string(KFLocationBin)};

parse_itc({?GLMS_ADPI_DOWNLOAD_KEY_FILE_REQ,
	    <<KfUri:512/binary,
	      Password/binary>>}) ->
    {download_keyfile_req, KfUri, Password};

parse_itc({?GLMS_ADPI_REFRESH_LICENSE_INVENTORY_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {refresh_license_inventory_rsp, GlmsResult, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_PKI_VERIFICATION_REQ,
	   <<KfLocation/binary>>}) ->
    {pki_verification_req, KfLocation};

parse_itc({?GLMS_ADPI_SET_FINGERPRINT_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo/binary>>}) ->
    {set_fingerprint_rsp, GlmsResult, to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_SET_FEATURE_STATE_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      FeatureStateId/binary>>}) ->
    {set_feature_state_rsp, GlmsResult, to_string(ResultInfo), to_string(FeatureStateId)};

parse_itc({?GLMS_ADPI_KEY_FILE_FAULT_ALARM_REQ,
	    <<GlmsAlarmState:4/native-unsigned-integer-unit:8>>}) ->
    {keyfile_fault_alarm_req, GlmsAlarmState};

parse_itc({?GLMS_ADPI_EU_ALARM_IND,
	    <<GlmsAlarmState:4/native-unsigned-integer-unit:8,
	      GlmsAlarmSeverity:4/native-unsigned-integer-unit:8>>}) ->
    {eu_alarm_ind, GlmsAlarmState, GlmsAlarmSeverity};

parse_itc({?GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_REQ,
	    <<KeyId:?GLMS_MO_KEY_LEN_36/binary,
	      GlmsLicenseType:4/native-unsigned-integer-unit:8,
	      GlmsAlarmState:4/native-unsigned-integer-unit:8,
	      GlmsAlarmReason:4/native-unsigned-integer-unit:8>>}) ->
    {license_key_not_available_alarm_req, to_string(KeyId), GlmsLicenseType, GlmsAlarmState, 
     GlmsAlarmReason};

parse_itc({?GLMS_ADPI_AM_ALARM_IND,
	   <<GlmsAlarmState:4/native-unsigned-integer-unit:8>>}) ->
    {am_alarm_ind, GlmsAlarmState};

parse_itc({?GLMS_ADPI_GP_ALARM_IND,
	   <<GlmsAlarmState:4/native-unsigned-integer-unit:8,
	     GlmsAlarmSeverity:4/native-unsigned-integer-unit:8,
	     GracePeriodId/binary>>}) ->
    {gp_alarm_ind, to_string(GracePeriodId), GlmsAlarmState, GlmsAlarmSeverity};

parse_itc({?GLMS_ADPI_LICENSE_KEY_EXPIRATION_ALARM_IND,
	   <<GlmsAlarmState:4/native-unsigned-integer-unit:8,
	     AdditionalInfo/binary>>}) ->
    {license_key_exiration_alarm_ind, GlmsAlarmState, to_string(AdditionalInfo)};

parse_itc({?GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      LastResponse:4/native-unsigned-integer-unit:8,
	      _SizeOfDump:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      Dump/binary>>}) ->
    {dump_capacity_key_data_rsp, GlmsResult, LastResponse, to_string(ResultInfo), Dump};

parse_itc({?GLMS_ADPI_DUMP_FEATURE_STATE_DATA_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      LastResponse:4/native-unsigned-integer-unit:8,
	      _SizeOfDump:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      Dump/binary>>}) ->
    {dump_feature_key_data_rsp, GlmsResult, LastResponse, to_string(ResultInfo), Dump};

parse_itc({?GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      LastResponse:4/native-unsigned-integer-unit:8,
	      _SizeOfDump:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      Dump/binary>>}) ->
    {dump_lcci_client_data_rsp, GlmsResult, LastResponse, to_string(ResultInfo), Dump};

parse_itc({?GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      LastResponse:4/native-unsigned-integer-unit:8,
	      _SizeOfDump:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      Dump/binary>>}) ->
    {dump_lfci_client_data_rsp, GlmsResult, LastResponse, to_string(ResultInfo), Dump};

parse_itc({?GLMS_ADPI_DUMP_GLMS_STATE_DATA_RSP,
	    <<GlmsResult:4/native-unsigned-integer-unit:8,
	      LastResponse:4/native-unsigned-integer-unit:8,
	      _SizeOfDump:4/native-unsigned-integer-unit:8,
	      ResultInfo:?GLMS_RESULT_INFO_LEN/binary,
	      Dump/binary>>}) ->
    {dump_glms_state_data_rsp, GlmsResult, LastResponse, to_string(ResultInfo), Dump};

parse_itc({?GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_RSP,
	   <<GlmsResult:4/native-unsigned-integer-unit:8,
	     CapacityStateId:?GLMS_MO_KEY_LEN/binary,
	     ResultInfo/binary>>}) ->
    {update_grace_period_attributes_rsp, GlmsResult, to_string(CapacityStateId), 
     to_string(ResultInfo)};

parse_itc({?GLMS_ADPI_HEARTBEAT_RSP, _Data}) ->
    {heartbeat_rsp};

parse_itc({?GLMS_ADPI_LOG_IND,
	    LogData}) ->
    {Data, _Gargage} = lists:splitwith(fun(B) -> B =/= 0 end, binary_to_list(LogData)),
    {log_ind, Data};


parse_itc({?LFCI_HUNT, _Data}) ->
    {lfci_hunt_ok};
parse_itc({?LFCI_CONN_TO_SERVER_CFM, 
	  <<?CLIENT_REF:4/native-unsigned-integer-unit:8,
	    ServerRef:4/native-unsigned-integer-unit:8,
	    ?PROTOCOL_VERS:4/native-unsigned-integer-unit:8>>}) ->
    {lfci_conn_to_server_cfm, ServerRef};
parse_itc({?LFCI_CONN_TO_SERVER_REJ,
	  <<?CLIENT_REF:4/native-unsigned-integer-unit:8,
	    _ServerRef:4/native-unsigned-integer-unit:8,
	    ErrorMsg/binary>>}) ->
    sysInitI:error_msg("lmaLinxHandler, LFCI_CONN_TO_SERVER_REJ", 
		       [to_string(ErrorMsg)]);
parse_itc({?LFCI_FEATURE_LICENSE_SUBSCRIBE_CFM,
	  <<?CLIENT_REF:4/native-unsigned-integer-unit:8,
	    _ServerRef:4/native-unsigned-integer-unit:8,
	    1:4/native-unsigned-integer-unit:8,
	    LicenseInfo/binary>>}) ->

    <<LicenseKeyId:24/binary,
      ServiceState:2/native-unsigned-integer-unit:8,
      _LicenseState:2/native-unsigned-integer-unit:8,
      _FeatureState:2/native-unsigned-integer-unit:8,
      _Padding/binary>> = LicenseInfo,
    {lfci_feature_license_sub_cfm, to_string(LicenseKeyId), transform_state(ServiceState)};
parse_itc({?LFCI_FEATURE_LICENSE_CHANGE_IND, 
	 <<?CLIENT_REF:4/native-unsigned-integer-unit:8,
	   _ServerRef:4/native-unsigned-integer-unit:8,
	   LicenseInfo/binary>>}) ->

    <<LicenseKeyId:24/binary,
      ServiceState:2/native-unsigned-integer-unit:8,
      _LicenseState:2/native-unsigned-integer-unit:8,
      _FeatureState:2/native-unsigned-integer-unit:8,
      _Padding/binary>> = LicenseInfo,
    {lfci_feature_license_change_ind, to_string(LicenseKeyId), transform_state(ServiceState)};
parse_itc({?LFCI_FEATURE_LICENSE_DISCONNECT_IND,
	   <<_ClientRef:4/native-unsigned-integer-unit:8,
	     _ServerRef:4/native-unsigned-integer-unit:8,
	     ErrorInfo/binary>>}) ->
    {lfci_feature_license_disconnect_ind, to_string(ErrorInfo)};

parse_itc({SigNo, _Data}) ->
    %% io:format("SigNo: ~p, Data: ~p~n", [SigNo, Data]),
    {unknown_signal, SigNo}.

transform_state(?INOPERABLE) ->
    inoperable;
transform_state(?OPERABLE) ->
    operable.
    

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Used by the adaptation layer to activate the GLMS component.
%%% @end
%%% ----------------------------------------------------------
-spec send_activate(port(),integer(),integer(),integer(), list()) -> ok.

send_activate(ItcPort, Spid, SelectedPV, LogLevel, RestrictedLicenses) ->
    
    RestrictedLicensesBin = embargo_bin(RestrictedLicenses, <<>>),

    Data = <<SelectedPV:4/native-unsigned-integer-unit:8,
	     LogLevel:4/native-unsigned-integer-unit:8,
	     RestrictedLicensesBin/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_ACTIVATE_REQ, Data).

embargo_bin([], Bin) ->
    Bin;
embargo_bin([License], Bin) ->
    B = list_to_binary(License),
    <<Bin/binary, B/binary>>;
embargo_bin([License | Rest], Bin) ->
    B = list_to_binary([License, ";"]),
    NewBin = <<Bin/binary, B/binary>>,
    embargo_bin(Rest, NewBin).


%%% ----------------------------------------------------------
%%% @doc A request from the adapter to GLMS to activate emergency unlock.
%%% @end
%%% ----------------------------------------------------------
-spec send_activate_eu(port(),integer()) -> ok.

send_activate_eu(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_ACTIVATE_EU_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc A request from the adapter to GLMS to activate integration unlock.
%%% @end
%%% ----------------------------------------------------------
-spec send_activate_iu(port(),integer()) -> ok.

send_activate_iu(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_ACTIVATE_IU_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc A request from the adapter to GLMS to activate production unlock.
%%% @end
%%% ----------------------------------------------------------
-spec send_activate_pu(port(),integer()) -> ok.

send_activate_pu(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_ACTIVATE_PU_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc The License User requests to establish a connection with GLMS over LIHI.
%%% @end
%%% ----------------------------------------------------------
-spec send_conn_to_server(port(),integer()) -> ok.

send_conn_to_server(ItcPort, Spid) ->
    SizeOfProtocolRevisions = 1,
    ServerRef = 1,

    Data = <<?CLIENT_REF:4/native-unsigned-integer-unit:8,
	     ServerRef:4/native-unsigned-integer-unit:8, 
	     SizeOfProtocolRevisions:4/native-unsigned-integer-unit:8,
	     ?PROTOCOL_VERS:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?LFCI_CONN_TO_SERVER_REQ, Data).


%%% ----------------------------------------------------------
%%% @doc The response to a GlmsAdpiCreateCapacityKeyMoReq, GlmsAdpiCreateCapacityStateMoReq
%%% GlmsAdpiCreateFeatureKeyMoReq or GlmsAdpiCreateFeatureStateMoReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_create_mo_rsp(port(),integer(),integer(),binary(), atom()) -> ok.

send_create_mo_rsp(ItcPort, Spid, Result, KeyId, Mo) ->
    NewKeyId =  change_bin_to_right_size(byte_size(KeyId), ?GLMS_MO_KEY_LEN, KeyId),
    Data = <<Result:4/native-unsigned-integer-unit:8,
	     NewKeyId/binary>>,
    case Mo of
	capacityKey ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_CREATE_CAPACITY_KEY_MO_RSP, Data);
	capacityState ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_CREATE_CAPACITY_STATE_MO_RSP, Data);
	featureKey ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_CREATE_FEATURE_KEY_MO_RSP, Data);
	featureState ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_CREATE_FEATURE_STATE_MO_RSP, Data);
	gracePeriod ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_CREATE_GRACE_PERIOD_MO_RSP, Data)
    end.

%%% ----------------------------------------------------------
%%% @doc Used by the adaptation layer to deactivate the GLMS component. All
%%% LIHI clients will be disconnected and all runtime data are cleared
%%% by GLMS. After deactivation the GLMS component will unpublish the
%%% LIHI interfaces from the nameserver.
%%% @end
%%% ----------------------------------------------------------
-spec send_deactivate(port(),integer()) -> ok.

send_deactivate(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DEACTIVATE_REQ,  <<>>).

%%% ----------------------------------------------------------
%%% @doc A request from the adapter to GLMS to deactivate production unlock.
%%% @end
%%% ----------------------------------------------------------
-spec send_deactivate_pu(port(),integer()) -> ok.

send_deactivate_pu(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DEACTIVATE_PU_REQ,  <<>>).


%%% ----------------------------------------------------------
%%% @doc The response to a GlmsAdpiDeleteCapacityKeyMoReq, GlmsAdpiDeleteCapacityStateMoReq,
%%% GlmsAdpiDeleteFeatureKeyMoReq, GlmsAdpiDeleteFeatureStateMoReq or 
%%% GlmsAdpiDeleteGracePeriodMoReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_delete_mo_rsp(port(),integer(),integer(),binary(), atom()) -> ok.

send_delete_mo_rsp(ItcPort, Spid, Result,Id, Mo) ->
    NewId =  change_bin_to_right_size(byte_size(Id), ?GLMS_MO_KEY_LEN, Id),
    Data = <<Result:4/native-unsigned-integer-unit:8,
	     NewId/binary >>,
    case Mo of
	capacityKey ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DELETE_CAPACITY_KEY_MO_RSP, Data);
	capacityState ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DELETE_CAPACITY_STATE_MO_RSP, Data);
	featureKey ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DELETE_FEATURE_KEY_MO_RSP, Data);
	featureState ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DELETE_FEATURE_STATE_MO_RSP, Data);
	gracePeriod ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DELETE_GRACE_PERIOD_MO_RSP, Data)
    end.

%%% ----------------------------------------------------------
%%% @doc A response to GlmsAdpiDownloadKeyFileReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_download_key_file_rsp(port(),integer(),integer(),binary()) -> ok.

send_download_key_file_rsp(ItcPort, Spid, Result, LKFPath) ->
    NewLKFPath = change_bin_to_right_size(byte_size(LKFPath), 512, LKFPath),
    Data = <<Result:4/native-unsigned-integer-unit:8,
	     NewLKFPath/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DOWNLOAD_KEY_FILE_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current CapacityState and CapacityKey data from GLMS.
%%% @end
%%% ----------------------------------------------------------
-spec send_dump_capacity_key_data_req(port(),integer()) -> ok.

send_dump_capacity_key_data_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DUMP_CAPACITY_STATE_DATA_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current FeatureState and FeatureKey data from GLMS.
%%% @end
%%% ----------------------------------------------------------
-spec send_dump_feature_key_data_req(port(),integer()) -> ok.

send_dump_feature_key_data_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DUMP_FEATURE_STATE_DATA_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current GLMS status data.
%%% @end
%%% ----------------------------------------------------------
-spec send_dump_glms_state_data_req(port(),integer()) -> ok.

send_dump_glms_state_data_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DUMP_GLMS_STATE_DATA_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current LCCI client statuses from GLMS.
%%% @end
%%% ----------------------------------------------------------
-spec send_dump_lcci_client_data_req(port(),integer()) -> ok.

send_dump_lcci_client_data_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current LFCI client statuses from GLMS.
%%% @end
%%% ----------------------------------------------------------
-spec send_dump_lfci_client_data_req(port(),integer()) -> ok.

send_dump_lfci_client_data_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc The License User subscribes for license controlled features.
%%% @end
%%% ----------------------------------------------------------
-spec send_feature_subscribe_req(port(),integer(),integer(),binary(),binary(),binary()) -> ok.

send_feature_subscribe_req(ItcPort, Spid, ServerRef, LicenseKeyId, LicenseMoId, LicenseName) ->
    SizeOfLicenseData = 1,
    LicenseKeyIdBin = change_bin_to_right_size(byte_size(LicenseKeyId), 24, LicenseKeyId),
    LicenseMoIdBin = change_bin_to_right_size(byte_size(LicenseMoId), 30, LicenseMoId),
    LicenseNameBin = change_bin_to_right_size(byte_size(LicenseName), 128, LicenseName),
    
    LicenseData = <<LicenseKeyIdBin/binary,
    		    LicenseMoIdBin/binary,
		    0:2/native-unsigned-integer-unit:8, % Padding
    		    LicenseNameBin/binary>>,
			
    Data = <<?CLIENT_REF:4/native-unsigned-integer-unit:8,
	     ServerRef:4/native-unsigned-integer-unit:8,
	     SizeOfLicenseData:4/native-unsigned-integer-unit:8,
	     LicenseData/binary>>,
    ok = itc:send(ItcPort, Spid, ?LFCI_FEATURE_LICENSE_SUBSCRIBE_REQ, Data). 

%%% ----------------------------------------------------------
%%% @doc A request to get the list of available Capacity Keys.
%%% @end
%%% ----------------------------------------------------------
-spec send_get_capacity_key_mo_list_req(port(),integer()) -> ok.

send_get_capacity_key_mo_list_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_GET_CAPACITY_KEY_MO_LIST_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc A request to get the list of available Capacity State MO's.
%%% @end
%%% ----------------------------------------------------------
-spec send_get_capacity_state_mo_list_req(port(),integer()) -> ok.

send_get_capacity_state_mo_list_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_GET_CAPACITY_STATE_MO_LIST_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc A request to get the list of available Feature Keys.
%%% @end
%%% ----------------------------------------------------------
-spec send_get_feature_key_mo_list_req(port(),integer()) -> ok.

send_get_feature_key_mo_list_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_GET_FEATURE_KEY_MO_LIST_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc A request to get the list of available Feature State MO's.
%%% @end
%%% ----------------------------------------------------------
-spec send_get_feature_state_mo_list_req(port(),integer()) -> ok.

send_get_feature_state_mo_list_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_GET_FEATURE_STATE_MO_LIST_REQ, Data).
   
%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiGetKeyFileLocationReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_get_key_file_loc_rsp(port(),integer(),integer(),binary()) -> ok.

send_get_key_file_loc_rsp(ItcPort, Spid, Result, LKFPath) ->
    NewLKFPath = change_bin_to_right_size(byte_size(LKFPath), 512, LKFPath),
    Data = <<Result:4/native-unsigned-integer-unit:8,
	     NewLKFPath/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_GET_KEY_FILE_LOCATION_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc A request to GLMS to know if it is responding or hanging.
%%% @end
%%% ----------------------------------------------------------
-spec send_heartbeat_req(port(),integer()) -> ok.

send_heartbeat_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_HEARTBEAT_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc Request GLMS to install a set of new Area Keys.
%%% @end
%%% ----------------------------------------------------------
-spec send_install_area_license_keys_req(port(),integer(),integer(),binary()) -> ok.

send_install_area_license_keys_req(ItcPort, Spid, CreateStateMos, Keys) ->
    %LengthOfKeys = length(Keys),
    %KeysBin = embargo_bin(Keys, <<>>),
    %KeysBin = turn_list_into_binary(Keys, <<>>),
    %io:format("lmaLinxHandler:send_install_area_license_keys_req   Keys ~p~n", [Keys]),
    
    KeysNull=[Keys,0],
    BinKeys = list_to_binary(KeysNull),
    %io:format("Binary  Keys ~p~n", [BinKeys]),

    Data = <<CreateStateMos:4/native-unsigned-integer-unit:8,BinKeys/binary>>,
  
    %io:format("lmaLinxHandler:send_install_area_license_keys_req   DATA ~p~n", [Data]),
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_INSTALL_AREA_LICENSE_KEYS_REQ, Data).

	   
%%% ----------------------------------------------------------
%%% @doc Request GLMS to trigger an audit of the State MOs.
%%% @end
%%% ----------------------------------------------------------
-spec send_audit_req(port(),integer()) -> ok.

send_audit_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_STATE_MO_AUDIT_REQ, <<>>).
 

%%% ----------------------------------------------------------
%%% @doc Request GLMS to install a new LKF.
%%% @end
%%% ----------------------------------------------------------
-spec send_install_keyfile_req(port(),integer(),binary(),binary()) -> ok.

send_install_keyfile_req(ItcPort, Spid, Uri, Password) ->
    NewUri = change_bin_to_right_size(byte_size(Uri), 512, Uri),
    NewPassword = change_bin_to_right_size(byte_size(Password), 128, Password),
    Data = <<NewUri/binary,
	     NewPassword/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_INSTALL_KEY_FILE_REQ, Data).


%%% ----------------------------------------------------------
%%% @doc Send alarm event id to GLMS.
%%% @end
%%% ----------------------------------------------------------
-spec send_keyfile_fault_alarm_rsp(port(),integer(),integer()) -> ok.

send_keyfile_fault_alarm_rsp(ItcPort, Spid, AlarmEventId) ->
    Data = <<AlarmEventId:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_KEY_FILE_FAULT_ALARM_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc Send alarm event id to GLMS.
%%% @end
%%% ----------------------------------------------------------
-spec send_license_key_not_available_alarm_rsp(port(),integer(),binary(),integer(),integer()) -> ok.

send_license_key_not_available_alarm_rsp(ItcPort, Spid, MoId, GlmsLicenseType, 
					 AlarmEventId) ->
    NewMoId =  change_bin_to_right_size(byte_size(MoId), ?GLMS_MO_KEY_LEN, MoId),
    Data = <<NewMoId/binary,
    	     0:2/native-unsigned-integer-unit:8, % Padding
    	     GlmsLicenseType:4/native-unsigned-integer-unit:8,
    	     AlarmEventId:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_LICENSE_KEY_NOT_AVAILABLE_ALARM_RSP, Data).
    
%%% ----------------------------------------------------------
%%% @doc Request GLMS to install a new Key File.
%%% @end
%%% ----------------------------------------------------------
-spec send_ns_hunt_req(port(),integer()) -> ok.

send_ns_hunt_req(ItcPort, Spid) ->
    TagOffset = 0,
    HuntSignalOffset = 40,
    NSReserved = 0,
    Offset = 0,
    
    Data0 = <<"LICENSE_FEATURE_CONTROL_I_SERVICE_NAME", 0, 0, 
	      ?LFCI_HUNT:4/native-unsigned-integer-unit:8>>,
    Data = <<TagOffset:4/native-unsigned-integer-unit:8,
	     HuntSignalOffset:4/native-unsigned-integer-unit:8,
	     NSReserved:4/native-unsigned-integer-unit:8,
	     Data0/binary,
	     Offset:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?NS_HUNT_REQUEST, Data).

%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiPersistentParameterDeleteIndexReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_persistant_param_delete_index_rsp(port(),integer(),integer(),integer()) -> ok.

send_persistant_param_delete_index_rsp(ItcPort, Spid, RequestId, Result) ->
    Data = <<RequestId:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_PERSISTENT_PARAMETER_DELETE_INDEX_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiFeatureConfigurationListReq
%%% @end
%%% ----------------------------------------------------------
-spec send_feature_configuration_list_rsp(port(),integer(),integer(),integer(),list()) -> ok.


send_feature_configuration_list_rsp(ItcPort, Spid, Result,FeatureConfigListLen, FeatureConfigList) ->
    %NewList =  change_bin_to_right_size(byte_size(FeatureConfigList), FeatureConfigListLen, FeatureConfigList),
    NewList = lmaAppData:convert_stringlist_to_structarray_binary(FeatureConfigList),
    %io:format("feature_configuration_list_rsp... ~p~n",[FeatureConfigList]),  
    logI:write_log("LicensingLog", "lmaLinxHandler", info, "feature_configuration_list_rsp "),
    Data = 	<<Result:4/native-unsigned-integer-unit:8,
         		FeatureConfigListLen:4/native-unsigned-integer-unit:8,
         		NewList/binary>>,
   	
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_FEATURE_CONFIGURATION_LIST_RSP, Data).


%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiFeatureConfigurationListReq
%%% @end
%%% ----------------------------------------------------------
-spec send_update_area_id_req(port(),integer(),binary()) -> ok.

send_update_area_id_req(ItcPort, Spid, AreaId) ->
    NewId =  change_bin_to_right_size(byte_size(AreaId), ?GLMS_AREA_ID_LEN, AreaId),
    Data = <<NewId/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_UPDATE_AREA_ID_REQ, Data).




%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiPersistentParameterIndexListReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_persistant_param_index_list_rsp(port(),integer(),integer(),list()) -> ok.

send_persistant_param_index_list_rsp(ItcPort, Spid, RequestId, IndexList) ->
    Result = ?GLMS_OK,
    NrOfIndexes = length(IndexList),
    case NrOfIndexes of
	0 ->
	    Indexes = 0,
	    %% TODO, How can this be make in a better way?
	    Data = <<RequestId:4/native-unsigned-integer-unit:8,
		     Result:4/native-unsigned-integer-unit:8,
		     NrOfIndexes:4/native-unsigned-integer-unit:8,
		     Indexes:4/native-unsigned-integer-unit:8>>;
	_Else ->
	    Indexes = turn_list_into_binary(IndexList, <<>>),
	    Data = <<RequestId:4/native-unsigned-integer-unit:8,
		     Result:4/native-unsigned-integer-unit:8,
		     NrOfIndexes:4/native-unsigned-integer-unit:8,
		     Indexes/binary>>
    end,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_PERSISTENT_PARAMETER_INDEX_LIST_RSP, Data).

turn_list_into_binary([], BinList) ->
    BinList;
turn_list_into_binary([Index | RestList], BinList) ->
    B = <<Index>>,
    NewBinList = <<BinList/binary, B/binary>>,
    turn_list_into_binary(RestList, NewBinList).

%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiPersistentParameterSetReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_persistant_param_set_rsp(port(),integer(),integer(),integer()) -> ok.

send_persistant_param_set_rsp(ItcPort, Spid, RequestId, Result) ->
    Data = <<RequestId:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_PERSISTENT_PARAMETER_SET_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc A response to GlmsAdpiPkiVerificationReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_pki_verification_rsp(port(),integer(),integer(),binary()) -> ok.

send_pki_verification_rsp(ItcPort, Spid, Result, KFLocation) ->
    NewKFLocation = change_bin_to_right_size(byte_size(KFLocation), 512, KFLocation),
    Data = <<Result:4/native-unsigned-integer-unit:8,
	     NewKFLocation/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_PKI_VERIFICATION_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc A request to get the attributes of the AutonomousMode MO.
%%% @end
%%% ----------------------------------------------------------
-spec send_read_am_mo_req(port(),integer()) -> ok.

send_read_am_mo_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_AM_MO_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc A request to get the attributes of the EmergencyUnlock MO.
%%% @end
%%% ----------------------------------------------------------
-spec send_read_eu_mo_req(port(),integer()) -> ok.

send_read_eu_mo_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_EU_MO_REQ, <<>>).






%%% ----------------------------------------------------------
%%% @doc A request to get the attributes of the LicenseSupport MO. SP086
%%% @end
%%% ----------------------------------------------------------
-spec send_read_licensesupport_mo_req(port(),integer()) -> ok.

send_read_licensesupport_mo_req(ItcPort, Spid) ->
    logI:write_log("LicensingLog", "lmaLinxHandler", info, "Entered send_read_licensesupport_mo_req"),
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_LICENSE_SUPPORT_REQ, <<>>).





%%% ----------------------------------------------------------
%%% @doc A request to get the attributes of the IntegrationUnlock MO.
%%% @end
%%% ----------------------------------------------------------
-spec send_read_iu_mo_req(port(),integer()) -> ok.

send_read_iu_mo_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_IU_MO_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc Requests the MO with the given Id. If
%%% no MO with the given Id exist the response will have a
%%% result of GLMS_NOK.
%%% @end
%%% ----------------------------------------------------------
-spec send_read_mo_req(port(),integer(),binary(),atom()) -> ok.

send_read_mo_req(ItcPort, Spid, Id, Mo) ->
    NewId =  change_bin_to_right_size(byte_size(Id), ?GLMS_MO_KEY_LEN, Id),
    Data = <<NewId/binary>>,
    case Mo of
	capacityKey ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_CAPACITY_KEY_REQ, Data);
	capacityState ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_CAPACITY_STATE_REQ, Data),
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_GRACE_PERIOD_REQ, Data);
	featureKey ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_FEATURE_KEY_REQ, Data);
	featureState ->
	    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_FEATURE_STATE_REQ, Data)
    end.

    
%%% ----------------------------------------------------------
%%% @doc Requests the reportProgress on the KeyFileManagement MO.
%%% @end
%%% ----------------------------------------------------------
-spec send_read_mo_install_keyfile_report_progress_req(port(),integer()) -> ok.

send_read_mo_install_keyfile_report_progress_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_INSTALL_KEY_FILE_REPORT_PROGRESS_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc Requests the MO attributes of the MO KeyFileInformation.
%%% @end
%%% ----------------------------------------------------------
-spec send_read_mo_key_file_information_req(port(),integer()) -> ok.

send_read_mo_key_file_information_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_KEY_FILE_INFORMATION_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc Requests the MO attributes of the Lm MO.
%%% @end
%%% ----------------------------------------------------------
-spec send_read_mo_lm_req(port(),integer()) -> ok.

send_read_mo_lm_req(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_READ_MO_LM_REQ, <<>>).

%%% ----------------------------------------------------------
%%% @doc Request GLMS to refresh the license inventory. 
%%% This will result in GLMS to attempt to re-read the installed 
%%% KeyFile and revalidate all licenses.
%%% @end
%%% ----------------------------------------------------------
-spec send_refresh_license_inventory(port(),integer()) -> ok.

send_refresh_license_inventory(ItcPort, Spid) ->
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_REFRESH_LICENSE_INVENTORY_REQ, <<>>).
 
%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiPersistentParameterGetReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_persistent_parameter_get_rsp(port(),integer(),integer(),integer(),binary(),
					integer(),binary()) -> ok.

send_persistent_parameter_get_rsp(ItcPort, Spid, RequestId, Result, BinTable, Index, Value) ->
    NewBinTable = change_bin_to_right_size(byte_size(BinTable), 80, BinTable),
    Data = <<RequestId:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8,
	     NewBinTable:80/binary,
	     Index:4/native-unsigned-integer-unit:8,
	     Value/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_PERSISTENT_PARAMETER_GET_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc Request to GLMS to set the feature state of a FeatureKey MO.
%%% @end
%%% ----------------------------------------------------------
-spec send_set_feature_state_req(port(),integer(),integer(),binary()) -> ok.

send_set_feature_state_req(ItcPort, Spid, FeatureState, FeatureStateId) ->
    NewFeatureStateId =  change_bin_to_right_size(byte_size(FeatureStateId), ?GLMS_MO_KEY_LEN, 
						  FeatureStateId),
    Data = <<FeatureState:4/native-unsigned-integer-unit:8,
	     NewFeatureStateId/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_SET_FEATURE_STATE_REQ, Data).
    
%%% ----------------------------------------------------------
%%% @doc Request GLMS to update the fingerprint.
%%% @end
%%% ----------------------------------------------------------
-spec send_set_fingerprint_req(port(),integer(),binary()) -> ok.

send_set_fingerprint_req(ItcPort, Spid, Fingerprint) ->
    NewFingerprint = 
	change_bin_to_right_size(byte_size(Fingerprint), 256, Fingerprint),
    Data = <<NewFingerprint/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_SET_FINGERPRINT_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiPersistentParameterDeleteIndexReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_software_parameter_delete_index_rsp(port(),integer(),integer(),integer()) -> ok.

send_software_parameter_delete_index_rsp(ItcPort, Spid, RequestId, Result) ->
    Data = <<RequestId:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_SOFTWARE_PARAMETER_DELETE_INDEX_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiSoftwareParameterGetReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_software_parameter_get_rsp(port(),integer(),integer(),integer(),
				      binary(),integer(),binary()) -> ok.

send_software_parameter_get_rsp(ItcPort, Spid, RequestId, Result, BinTable, Index, Value) ->
    NewBinTable = change_bin_to_right_size(byte_size(BinTable), 80, BinTable),
    Data = <<RequestId:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8,
	     NewBinTable/binary,
	     Index:4/native-unsigned-integer-unit:8,
	     Value/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_SOFTWARE_PARAMETER_GET_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiSoftwareParameterIndexListReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_soft_param_index_list_rsp(port(),integer(),integer(),list()) -> ok.

send_soft_param_index_list_rsp(ItcPort, Spid, RequestId, IndexList) ->
    Result = ?GLMS_OK,
    NrOfIndexes = length(IndexList),
    case NrOfIndexes of
	0 ->
	    Indexes = 0,
	    %% TODO, How can this be make in a better way?
	    Data = <<RequestId:4/native-unsigned-integer-unit:8,
		     Result:4/native-unsigned-integer-unit:8,
		     NrOfIndexes:4/native-unsigned-integer-unit:8,
		     Indexes:4/native-unsigned-integer-unit:8>>;
	_Else ->
	    Indexes = turn_list_into_binary(IndexList, <<>>),
	    Data = <<RequestId:4/native-unsigned-integer-unit:8,
		     Result:4/native-unsigned-integer-unit:8,
		     NrOfIndexes:4/native-unsigned-integer-unit:8,
		     Indexes/binary>>
    end,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_SOFTWARE_PARAMETER_INDEX_LIST_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc Response to GlmsAdpiSoftwareParameterSetReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_soft_param_set_rsp(port(),integer(),integer(),integer()) -> ok.

send_soft_param_set_rsp(ItcPort, Spid, RequestId, Result) ->
    Data = <<RequestId:4/native-unsigned-integer-unit:8,
	     Result:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_SOFTWARE_PARAMETER_SET_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc A response to GlmsAdpiStoreKeyFileReq.
%%% @end
%%% ----------------------------------------------------------
-spec send_store_key_file_rsp(port(),integer(),integer(),binary()) -> ok.

send_store_key_file_rsp(ItcPort, Spid, Result, KFLocation) ->
    Data = <<Result:4/native-unsigned-integer-unit:8,
	     KFLocation/binary>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_STORE_KEY_FILE_RSP, Data).

%%% ----------------------------------------------------------
%%% @doc A request to subscribe to MO update Indications.
%%% @end
%%% ----------------------------------------------------------
-spec send_subscribe_mo_updates_req(port(),integer()) -> ok.

send_subscribe_mo_updates_req(ItcPort, Spid) ->
    Data = <<>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_SUBSCRIBE_MO_UPDATES_REQ, Data).

%%% ----------------------------------------------------------
%%% @doc A request to subscribe to MO update Indications.
%%% @end
%%% ----------------------------------------------------------
-spec send_update_grace_period_attributes_req (port(),integer(),binary(),integer(),
					       integer(),integer()) -> ok.

send_update_grace_period_attributes_req(ItcPort, Spid, GracePeriodId, GpActivationThreshold, 
					GpResetThreshold, GpLength) ->
    NewGracePeriodId = 
	change_bin_to_right_size(byte_size(GracePeriodId), ?GLMS_MO_KEY_LEN, GracePeriodId),
    Data = <<NewGracePeriodId/binary,
	     GpActivationThreshold:4/native-unsigned-integer-unit:8,
	     GpResetThreshold:4/native-unsigned-integer-unit:8,
	     GpLength:4/native-unsigned-integer-unit:8>>,
    ok = itc:send(ItcPort, Spid, ?GLMS_ADPI_UPDATE_GRACE_PERIOD_ATTRIBUTES_REQ, Data).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
change_bin_to_right_size(Size, MaxSize, Bin) when Size == MaxSize ->
    Bin;
change_bin_to_right_size(Size, MaxSize, Bin) when Size < MaxSize ->
    Add = <<0>>,
    NewBin = <<Bin/binary, Add/binary>>,
    change_bin_to_right_size(byte_size(NewBin), MaxSize, NewBin).
	
to_string(Text) when is_binary(Text) ->
    to_string(binary_to_list(Text));

to_string(Text) when is_list(Text) ->
    lists:takewhile(fun(B) -> B =/= 0 end, Text).
    


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------




