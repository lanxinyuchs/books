#!/usr/bin/env python

##
# (C) Ericsson AB 2016 All rights reserved.
# The information in this document is the property of Ericsson. Except
# as specifically authorized in writing by Ericsson, the receiver of
# this document shall keep the information contained herein confidential
# and shall protect the same in whole or in part from disclosure and
# dissemination to third parties. Disclosure and disseminations to the
# receivers employees shall only be made on a strict need to know basis.
##

"""
Lifecycle Management Script for upgrade verify operation.
"""


# Temporary added due to old pylint version in Jenkins
# pylint used in HUB (newer version) do not print out this faulty
# warnings/ errors
#pylint: disable=E1002
#pylint: disable=E1101
#pylint: disable=W0201
#pylint: disable=E0203
#pylint: disable=R0201
#pylint: disable=R0902
#pylint: disable=R0904
#pylint: disable=R0912
#pylint: disable=R0913
#pylint: disable=R0912
#pylint: disable=C0103

from lcm import (lcm_util,
                 vnfm_api_comm,
                 lcm_db_handler,
                 lcm_upgrade_job)

import imp
import hashlib

def import_common(module_name):
    """ Used to import dynamically a common module """
    file_handler, file_path, descr = imp.find_module(module_name)
    return imp.load_module(hashlib.md5(file_path).hexdigest(),
                           file_handler,
                           file_path,
                           descr)

lcm_wf_utility = import_common('lcm_wf_utility')
vnf_api_comm = import_common('vnf_api_comm')

import logging
import time
import threading
from common.http_utils import host_in_url

LOG = logging.getLogger('VERIFY')



class VerifyCheckResult:
    """ Holds information related to a specific verify check """
    SUCCESS = "SUCCESS"
    FAILURE = "FAILURE"

    def __init__(self, name_of_check, result, add_info):
        """ Constructor """
        LOG.debug("__init__(self, name_of_check, result, add_info) - name_of_check:"
                  " %s, result: %s, add_info: %s" % (name_of_check, result, add_info))
        self.name_of_check = name_of_check
        self.result = result
        self.add_info = add_info

    def get_result(self):
        """ Returns result """
        return self.result

    def get_additional_info(self):
        """ Returns additional information for result """
        return self.add_info

    def get_name_of_check(self):
        """ Returns name of the check """
        return self.name_of_check

    def __str__(self):
        return "Verify Check: %s, Result: %s, Add info: %s" % (
            self.name_of_check, self.result, self.add_info)

    def __repr__(self):
        return "Verify Check: %s, Result: %s, Add info: %s" % (
            self.name_of_check, self.result, self.add_info)

def main(job_id, operation_data, upgrade_job, lcm_config, callback):
    """ Main function for execution of a Verify Request """
    LOG.debug("main(job_id, operation_data, upgrade_job, "
              "callback) - job_id: %s, operation_data: %s, "
              "upgrade_job: %s, callback: %s"
              % (job_id, operation_data, upgrade_job, callback))
    if (upgrade_job.get_state() == lcm_upgrade_job.get_state_enum().PREPARE_COMPLETED):
        verify = Verify(job_id, upgrade_job, lcm_config, callback)
        verify.start()
        verify.main(operation_data)
        LOG.info("Verify initiated (job id: %s)" % (job_id))
        return verify
    else:
        raise lcm_util.LcmException(
            lcm_util.LcmException.NOT_ALLOWED,
            "Attempt to execute operation verify for an upgrade job (job_id: %s"
            ") when not expected. Current state: %s, Current workflow: %s"
            % (job_id, upgrade_job.state, upgrade_job.current_workflow))

def get_verify_wf(job_id, operation_data, upgrade_job, lcm_config, callback):
    """ Verify function for execution of verify """
    LOG.debug("get_verify_wf(job_id, operation_data, upgrade_job, "
              "lcm_config, callback) - job_id: %s, operation_data: %s, "
              "upgrade_job: %s, lcm_config: %s, callback: %s"
              % (job_id, operation_data, upgrade_job, lcm_config, callback))
    return Verify(job_id, upgrade_job, lcm_config, callback)


class Verify(lcm_wf_utility.GenAsyncQueue):
    """ Verify workflow script """

    CURRENT_VERSION = lcm_util.EnumVersion.VER_1_0

    def __init__(self, job_id, upgrade_job, lcm_config, callback):
        """ Constructor """
        super(Verify, self).__init__(job_id,
                                     upgrade_job,
                                     lcm_config,
                                     callback,
                                     'Verify_' + str(job_id),
                                     LOG,
                                     Verify.CURRENT_VERSION)
        self.timeout_expired = False
        self.cancel_requested = False
        self.previous_state = None
        self.requested_time = lcm_util.get_lcm_log_handler().get_lcm_timestamp()
        self.fallback_timer = None
        self.operation_name = None
        self.operation_data = None
        self.vnfm_url = "http://%s:%s/" % (host_in_url(self.lcm_config.get_vnfm_http_host()),
                                           self.lcm_config.get_vnfm_http_port())
        self.vnfm_comm_handle = vnfm_api_comm.get_vnfm_comm(self.vnfm_url)
        self.progress_wf = lcm_wf_utility.EnumProgressWf(LOG)
        self.result_wf = lcm_wf_utility.EnumOperationResultWf(LOG)
        self.add_info_trans = lcm_wf_utility.AddInfoTransformer()
        self.vnf_port = self.lcm_config.get_vnfi_http_port()
        self.upgrade_job = upgrade_job

    def main(self, *operation_data):
        """ Main method for script Verify """
        LOG.debug('main(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Verify Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def cancel(self, *operation_data):
        """ Cancel the verify process """
        LOG.debug('cancel(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("Cancel received (job id: %s)" % (self.job_id))
        LOG.info("LCM Workflow Verify Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.cancel_requested = True

    #
    # Verify thread loop - Aynchronous operations
    #
    def handle_main(self, operation_data):
        """
        Script entry point for the asynchronous lcm operation verify
        """
        LOG.debug("handle_main(self, operation_data) - "
                  "operation_data: %s" % (operation_data))
        try:
            self.operation_name = lcm_upgrade_job.get_operation_enum().VERIFY
            self.previous_state = self.upgrade_job.get_state()
            self.operation_data = operation_data
            self.__start_timer(self.get_fallback_timeout(operation_data))
            self.__set_state(lcm_upgrade_job.get_state_enum().VERIFY_IN_PROGRESS)
            self.__set_progress(
                10,
                self.progress_wf.get_enum(self.progress_wf.PRE_CHECK))

            # For CT purposes, delay workflow execution as specified in ct_lcm.cfg file
            if self.__is_wf_to_be_delayed():
                return True

            self.__abort_if_cancel_requested()
            self.__abort_if_timeout()
            verify_check_results = self.execute_verify_checks(True)

            self.__add_operation_result(
                self.operation_name,
                self.requested_time,
                lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                self.get_fallback_timeout(operation_data),
                self.result_wf.get_enum(self.define_result(verify_check_results)),
                self.add_info_trans.get_add_info(self.build_add_info(verify_check_results)))
            self.__set_progress(
                100,
                self.progress_wf.get_enum(self.progress_wf.PRE_CHECK))
            self.handle_cleanup(self.previous_state)
            return False

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(operation_data)
            return False

        except lcm_wf_utility.WorkflowTmoException as exception:
            self.handle_timeout(operation_data)
            return False

        except lcm_util.LcmException as exception:
            LOG.debug("Activate of the UP with JOB ID (%s) failed due to %s"
                      % (self.job_id, exception.get_error_message()))
            self.handle_error(exception.get_error_message(), operation_data)
            return False

        except Exception as exception:
            LOG.error("Activate of the UP with JOB ID (%s) failed due to %s"
                      % (self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), operation_data)
            return False

    def handle_cancel(self, operation_data):
        """
        Script entry point for work flow cancellation.
        """
        LOG.debug('handle_cancel(self, operation_data) - ')
        self.__set_progress(
            50,
            self.progress_wf.get_enum(self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED))
        self.__add_operation_result(
            self.operation_name,
            self.requested_time,
            lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
            self.get_fallback_timeout(operation_data),
            self.result_wf.get_enum(self.result_wf.CANCELLED),
            self.add_info_trans.get_add_info(["Operation cancelled"]))
        self.__set_progress(
            100,
            self.progress_wf.get_enum(self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED))
        self.handle_cleanup(self.previous_state)

    def handle_timeout(self, operation_data):
        """
        Script entry point for the case where the fallback timer expired
        """
        LOG.debug('handle_timeout(self, operation_data) - operation_data: %s'
                  % (operation_data))
        self.__set_progress(
            50,
            self.progress_wf.get_enum(self.progress_wf.FALLBACK_TIMER_EXPIRED))
        self.__add_operation_result(
            self.operation_name,
            self.requested_time,
            lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
            self.get_fallback_timeout(operation_data),
            self.result_wf.get_enum(self.result_wf.FALLBACK_TIMEOUT),
            self.add_info_trans.get_add_info(["Fallback timer expired"]))
        self.__set_progress(
            100,
            self.progress_wf.get_enum(self.progress_wf.FALLBACK_TIMER_EXPIRED))
        self.handle_cleanup(self.previous_state)

    def handle_error(self, reason, operation_data):
        """
        Handle Script entry point for the case where a crash occured caught
        by base class
        """
        LOG.debug('handle_error(self, reason, operation_data)')
        self.__set_progress(
            92,
            self.progress_wf.get_enum(self.progress_wf.CLEANUP_DUE_TO_FAILURE))
        self.__add_operation_result(
            self.operation_name,
            self.requested_time,
            lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
            self.get_fallback_timeout(operation_data),
            self.result_wf.get_enum(self.result_wf.FAILURE),
            self.add_info_trans.get_add_info([reason]))
        self.__set_progress(
            100,
            self.progress_wf.get_enum(self.progress_wf.CLEANUP_DUE_TO_FAILURE))
        self.handle_cleanup(self.previous_state)

    def execute_verify_checks(self, is_verify_action = False):
        """ Performs all existing verify checks """
        LOG.debug("execute_verify_checks(self, is_verify_action = False)"
                  " - is_verify_action: %s" % (is_verify_action))
        vnf_info = self.__get_vnf_info(self.upgrade_job.get_vnf_id())
        verify_check_results = list()
        verify_check_result = self.__check_from_vnf(vnf_info)
        verify_check_results.append(verify_check_result)
        self.__abort_if_cancel_requested()
        self.__abort_if_timeout()

        if vnf_info != None:
            # Get detailed information of VNF
            vnf_comm_handle = vnf_api_comm.get_vnf_comm(
                self.upgrade_job.get_vnf_id(),
                self.vnf_port,
                self.vnfm_url)

            vnf_info = self.vnfm_comm_handle.get_vnf_info(self.upgrade_job.get_vnf_id(),
                                                          [vnfm_api_comm.VnfmComm.STORAGE])
            self.__abort_if_cancel_requested()
            self.__abort_if_timeout()

            verify_check_result = self.__check_vnf_is_operational(vnf_info)
            verify_check_results.append(verify_check_result)

            verify_check_result = self.__check_vnf_storage(vnf_comm_handle,
                                                           self.upgrade_job.get_vnf_id(),
                                                           vnf_info)
            verify_check_results.append(verify_check_result)

            verify_check_result = self.__is_upgrade_of_vsd_supported(vnf_info)
            verify_check_results.append(verify_check_result)

            verify_check_result = self.__vnf_verify_phase(vnf_comm_handle,
                                                          vnf_info,
                                                          is_verify_action)
            verify_check_results.append(verify_check_result)

        verify_check_result = self.__check_vnfd_id(
            self.upgrade_job.get_vnf_descriptor_id(),
            self.upgrade_job.get_vnfd_id_in_package())
        verify_check_results.append(verify_check_result)

        verify_check_result = self.__check_all_workflows_exists(
            self.upgrade_job.get_workflows())
        verify_check_results.append(verify_check_result)

        return verify_check_results

    def define_result(self, verify_check_results):
        """ Defines the result from all checks performed """
        for verify_check_result in verify_check_results:
            if verify_check_result.get_result() == VerifyCheckResult.FAILURE:
                return VerifyCheckResult.FAILURE
        return VerifyCheckResult.SUCCESS

    def build_add_info(self, verify_check_results):
        """ Returns add infos as a list with infos from all checks """
        add_infos = list()
        for verify_check_result in verify_check_results:
            if (type(verify_check_result.get_additional_info()) is str or
                type(verify_check_result.get_additional_info()) is unicode):
                if verify_check_result.get_additional_info() != '':
                    add_infos.append(verify_check_result.get_additional_info())

            else:
                # Assume it is a list
                if len(verify_check_result.get_additional_info()) > 0:
                    add_infos.extend(verify_check_result.get_additional_info())
        return add_infos

    def __check_from_vnf(self, vnf_info):
        """ Ensure that from state VNF do exist """
        LOG.debug('__check_from_vnf(self, vnf_info) - vnf_info: %s' % (vnf_info))
        LOG.info("Checks if From-VNF exists (job id: %s)"
                 % (self.job_id))
        result = VerifyCheckResult.SUCCESS
        add_info = ''

        if vnf_info == None:
            result = VerifyCheckResult.FAILURE
            add_info = ("VNF to be upgraded (%s) do not exist"
                        % (self.upgrade_job.get_vnf_id()))
        elif vnf_info['instantiationState'] == 'NOT_INSTANTIATED':
            result = VerifyCheckResult.FAILURE
            add_info = ("VNF to be upgraded (%s) exists but is NOT instantiated"
                        % (vnf_info['instanceId']))
        verify_check_result = VerifyCheckResult('Ensure from VNF exists', result, add_info)
        LOG.debug("__check_from_vnf(...) - RETURNS - %s" % (verify_check_result))
        return verify_check_result

    def __check_vnf_is_operational(self, vnf_info):
        """ Checks that the VNF is operational """
        LOG.debug("__check_vnf_is_operational(vnf_info) - vnf_info: %s" % (vnf_info))
        LOG.info("Checks if From-VNF is operational (job id: %s)"
                 % (self.job_id))

        result = VerifyCheckResult.SUCCESS
        add_info = ''
        if vnf_info.operational_state == 'DISABLED':
            result = VerifyCheckResult.FAILURE
            add_info = ("The operational state is 'disabled' i.e. currently there is no "
                        "connection with the VNF (%s). This might be a temporary problem." % (
                        vnf_info.instance_id))
        verify_check_result = VerifyCheckResult('Ensure from VNF is operational', result,
                                                add_info)
        LOG.debug("__check_vnf_is_operational(...) - RETURNS - %s" % (verify_check_result))
        return verify_check_result

    def __check_vnf_storage(self, vnf_comm_handle, vnf_id, vnf_info):
        """ Ensures that storage is valid for From-VNF """
        LOG.debug("__check_vnf_storage(self, vnf_comm_handle, vnf_id, vnf_info) - vnf_id: %s, vnf_info: %s"
                  % (vnf_id, vnf_info))
        LOG.info("Checks storages of From-VNF (job id: %s)"
                 % (self.job_id))
        result = VerifyCheckResult.SUCCESS
        add_info = ''
        if len(vnf_info.get_storages()) == 0:
            # NFS is used
            pass
        elif len(vnf_info.get_storages()) == 2:
            # Cinder storage is used
            # Check that names of storages are valid i.e. supported
            for storage in vnf_info.get_storages():
                if storage.name != 'log_storage' and storage.name != 'db_storage':
                    result = VerifyCheckResult.FAILURE
                    add_info = ("The From-VNF are using two storages but at least one of them"
                                " is using a not supported name. Used names: %s. Supported names"
                                ": 'log_storage' and 'db_storage'" % (vnf_info.get_storage_names()))
                    break

            if result == VerifyCheckResult.SUCCESS:
                if not self.__is_vnf_supporting(vnf_comm_handle, vnf_id, 2):
                    result = VerifyCheckResult.FAILURE
                    add_info = ("The From-VNF are using two valid storages. However the "
                                "From-VNF does not support VNF-LCM Rest IF version 2 which"
                                " is required to be able to upgrade a VNF using storages"
                                " (Cinder).")
                if not self.vnfm_comm_handle.is_supported(lcm_util.EnumVersion.VER_2_0):
                    result = VerifyCheckResult.FAILURE
                    rej_reason = ("The VNFM (LCM Framework) does not support required fault"
                                  " handling mechanism when From-VNF are using storages "
                                  "(Cinder volumes). VNFM (comm handle) Required version: %s, VNFM "
                                  "(comm handle) Current version: %s"
                                  % (lcm_util.EnumVersion.VER_2_0,
                                     self.vnfm_comm_handle.get_current_version()))
                    if add_info != '':
                        add_info = "%s Also %s" % (add_info, rej_reason)
                    else:
                        add_info = "%s" % (rej_reason)

        else:
            result = VerifyCheckResult.FAILURE
            add_info = ("The From-VNF are using not supported storages. The name of the storages"
                        " are: %s. Supported names are: 'log_storage' and 'db_storage'"
                        % (vnf_info.get_storage_names()))

        verify_check_result = VerifyCheckResult('Ensure that storage is valid for From-VNF',
                                                result,
                                                add_info)
        LOG.debug("__check_vnf_storage(...) - RETURNS - %s" % (verify_check_result))
        return verify_check_result

    def __is_upgrade_of_vsd_supported(self, vnf_info):
        """ Checks if it is possible to upgrade vSD nodes """
        LOG.debug("__is_upgrade_of_vsd_supported(self, vnf_info) - vnf_info: %s" % (vnf_info))
        result = VerifyCheckResult.SUCCESS
        add_info = list()
        try:
            if self.__is_upgrade_of_vsd(vnf_info):
                if not self.vnfm_comm_handle.is_supported(lcm_util.EnumVersion.VER_4_0):
                    result = VerifyCheckResult.FAILURE
                    msg = "Upgrade of vSD nodes are not supported due to not supported by R-VNFM."
                    add_info.append(msg)
                    LOG.info("%s - Required version of VNFM: %s, Current version of VNFM: %s"
                             % (msg, lcm_util.EnumVersion.VER_4_0,
                                self.vnfm_comm_handle.get_current_version()))

        except vnfm_api_comm.VnfmCommException as exception:
            msg = "Problems reported when trying to understand if upgrade of vSD is possible or not"
            LOG.warning("%s. Problem: %s" % (msg, exception))
            add_info.extend(msg)

        verify_check_result = VerifyCheckResult("Is upgrade of vSD nodes supported",
                                                result,
                                                add_info)
        LOG.debug("__is_upgrade_of_vsd_supported(...) - RETURNS - %s" % (verify_check_result))
        return verify_check_result

    def __is_upgrade_of_vsd(self, vnf_info):
        """ Checks if it is upgrade of a vSD node """
        LOG.debug("__is_upgrade_of_vsd(self, vnf_info) - vnf_info: %s" % (vnf_info))
        is_vsd_upgrade = False

        if len(vnf_info.get_storages()) == 2:
            for wf in self.upgrade_job.get_workflows():
                if wf.get_operation() == lcm_upgrade_job.get_workflow_enum().ACTIVATE_CINDER_OPERATION:
                    if 'vsd' in wf.get_sw_package_path():
                        is_vsd_upgrade = True
                        break
        LOG.debug("__is_upgrade_of_vsd(...) - RETURNS %s" % (is_vsd_upgrade))
        return is_vsd_upgrade

    def __vnf_verify_phase(self, vnf_comm_handle, vnf_info, is_verify_action):
        """
        Gives the From-VNF opportunity to do pre-checks prior the upgrade.
        """
        LOG.debug("__vnf_verify_phase(self, vnf_comm_handle, vnf_info, is_verify_action)"
                  " - vnf_info: %s, is_verify_action: %s" % (vnf_info, is_verify_action))
        result = VerifyCheckResult.SUCCESS
        add_info = list()
        if self.__is_vnf_supporting(vnf_comm_handle, vnf_info.instance_id, 3):
            try:
                if is_verify_action:
                    result_data = vnf_comm_handle.verify_preconditions()
                else:
                    result_data = vnf_comm_handle.activate_start()

                if result_data['result'] == 'SUCCESS':
                    if 'info' in result_data:
                        add_info.extend(result_data['info'])

                elif result_data['result'] == 'FAILURE':
                    result = VerifyCheckResult.FAILURE
                    if 'info' in result_data:
                        add_info.extend(result_data['info'])
                    else:
                        msg = "The From-VNF reported FAILURE but did NOT specify why!"
                        LOG.warning(msg)
                        add_info.append(msg)

                else:
                    result = VerifyCheckResult.FAILURE
                    if 'info' in result_data:
                        msg = ("The From-VNF reported unknown result (%s) (treated as failure)"
                               % (result_data['result']))
                        add_info.extend(result_data['info'])
                    else:
                        msg = ("The From-VNF reported unknown result (%s) (treated as failure)"
                               " and did NOT specify any reason!"
                               % (result_data['result']))
                        LOG.warning(msg)
                        add_info.append(msg)

            except vnf_api_comm.VnfCommException as exception:
                failure_reson = (
                    "The From-VNF could not complete the verify VNF check control"
                    " due to the reported failure reason: %s" % (exception))
                msg = ("%s. The problem to perform this specific check will not stop any upgrade attempt"
                       % (failure_reson))
                LOG.warning("%s" % (msg))
                add_info.append("WARNING: %s" % (msg))

        else:
            add_info = ("INFO: Not possible to request From-VNF to check if upgrade"
                        " is possible or not due to it does NOT support version"
                        " 3 of the Rest IF 'Ep-lcm-vnf'")
            LOG.info(add_info)
        verify_check_result = VerifyCheckResult("Ensure that preconditions are fulfilled"
                                                " prior upgrade in From-VNF",
                                                result,
                                                add_info)
        LOG.debug("__vnf_verify_phase(...) - RETURNS - %s" % (verify_check_result))
        return verify_check_result

    def __is_vnf_supporting(self, vnf_comm_handle, vnf_id, version):
        """
        Checks if gven VNF is supporting given version
        Returns True if supporting, otherwise False
        """
        LOG.debug("__is_vnf_supporting(self, vnf_comm_handle, vnf_id, version) - "
                  "vnf_id: %s, version: %s" % (vnf_id, version))

        is_supported = vnf_comm_handle.is_supported(version)
        LOG.debug("__is_vnf_supporting(...) - RETURNS: is_supported: %s"
                  % (is_supported))
        return is_supported

    def __is_vnfm_supporting(self, vnfm_comm_handle, vnf_id, version):
        """
        Checks if VNFM is supporting given version
        Returns True if supporting, otherwise False
        """
        LOG.debug("__is_vnfm_supporting(self, vnfm_comm_handle, vnf_id, version) - "
                  "vnf_id: %s, version: %s" % (vnf_id, version))

        is_supported = vnfm_comm_handle.is_supported(version)
        LOG.debug("__is_vnf_supporting(...) - RETURNS: is_supported: %s"
                  % (is_supported))
        return is_supported

    def __check_vnfd_id(self, vnfd_id_in, vnfd_id_in_package):
        """
        Ensure that VNFD ID given at create job matches the one
        in the VNF package
        """
        LOG.debug("__check_vnfd_id(self, vnfd_id_in, vnfd_id_in_package) - "
                  "vnfd_id_in: %s, vnfd_id_in_package: %s" % (vnfd_id_in, vnfd_id_in_package))
        LOG.info("Checks that VNFD ID matches the one in VNF package (job id: %s)"
                 % (self.job_id))
        result = VerifyCheckResult.SUCCESS
        add_info = ''
        if (vnfd_id_in != vnfd_id_in_package):
            result = VerifyCheckResult.FAILURE
            add_info = ("Given VNFD ID as argument in create operation differs from "
                        "VNFD ID in VNF package i.e. it must match. VNFD ID in package: %s"
                        % (vnfd_id_in_package))
        verify_check_result = VerifyCheckResult(
            'Ensure VNFD ID in create matches the one in VNF package',
            result,
            add_info)
        LOG.debug("__check_vnfd_id(...) - RETURNS - %s" % (verify_check_result))
        return verify_check_result

    def __check_all_workflows_exists(self, upgrade_forkflows):
        """
        Ensure that all required upgrade workflows have been specified
        in the VNFD
        """
        LOG.debug("__check_all_workflows_exists(self, upgrade_forkflows)"
                  " - upgrade_forkflows: %s" % (upgrade_forkflows))
        LOG.info("Checks that all workflows exists in VNF package (job id: %s)"
                 % (self.job_id))

        result = VerifyCheckResult.SUCCESS
        missing_wf = list()
        for operation in lcm_upgrade_job.get_workflow_enum().get_all_operations():
            found = False
            for workflow in upgrade_forkflows:
                if workflow.get_operation() == operation:
                    found = True
                    break
            if not found:
                result = VerifyCheckResult.FAILURE
                missing_wf.append(operation)

        add_info = ""
        if len(missing_wf) > 0:
            add_info = "Following upgrade workflows are missing from the vnf descriptor: %s" % (missing_wf)
        verify_check_result = VerifyCheckResult(
            'Ensure that required upgrade workflows have been specified'
            ' in the VNFD',
            result,
            add_info)
        LOG.debug("__check_all_workflows_exists(...) - RETURNS - %s" % (verify_check_result))
        return verify_check_result

    def __get_vnf_info(self, vnf_id):
        """ Returns info for requested VNF, if possible (else None is returned)"""
        LOG.debug("__get_vnf_info(self, vnf_id) - vnf_id: %s" % (vnf_id))

        response_json_data = self.vnfm_comm_handle.get_all_vnf_instance_infos()
        found_vnf = None
        for vnf_info in response_json_data['vnfs']:
            if vnf_id == vnf_info['instanceId']:
                found_vnf = vnf_info
                break
        LOG.debug("__get_vnf_info(...) - Returns: %s" % (found_vnf))
        return found_vnf

    def handle_cleanup(self, state):
        """
        Clean-up required after the Verify operation is finished or if the operation is aborted.
        """
        LOG.debug('handle_cleanup(self, state) - state: %s' % (state))
        if self.fallback_timer != None:
            LOG.info("Stops fallback timer")
            self.fallback_timer.cancel()
        self.__finish(state)
        self.__set_progress(0, self.progress_wf.get_enum(self.progress_wf.IDLE))
        self.upgrade_job = None
        self.previous_state = None

    def __abort_if_cancel_requested(self):
        """ Check if cancel is requested and break current execution """
        if self.cancel_requested:
            LOG.debug("Cancel of the ongoing VERIFY requested. Cleanup initiated")
            self.__set_progress(
                50,
                self.progress_wf.get_enum(self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED))
            raise lcm_wf_utility.CancelWorkflowException("Operation cancelled")

    def timeout(self):
        """ Callback to be executed when fallback timer expires """
        LOG.info("Fallback timer on %s s. expired (job id: %s)"
                 % (self.get_fallback_timeout(self.operation_data),
                    self.job_id))
        if self.upgrade_job == None:
            LOG.warning("A fallback timeout occured %s, when the workflow is already finished." %
                        (self.fallback_timer))
            return
        self.timeout_expired = True
        self.put((self.function(), (self.operation_data, )))

    def __abort_if_timeout(self):
        """ Check if timeout expired and break current execution """
        if self.timeout_expired:
            LOG.debug("Operation %s is aborted since fallback timer expired "
                      % (self.operation_name))
            self.__set_progress(
                10,
                self.progress_wf.get_enum(self.progress_wf.FALLBACK_TIMER_EXPIRED))
            raise lcm_wf_utility.WorkflowTmoException("Fallback timer expired")

    def __finish(self, state, callback = True):
        """ Ensure the workflow marks its execution as finished """
        self.upgrade_job.shadow_state = state
        self.upgrade_job.set_shadow(True)
        self.__set_state(state)
        try:
            lcm_db_handler.get_lcm_db_handler().set_job(self.upgrade_job)
        except lcm_util.LcmException as exception:
            LOG.warning("__finish(self, state, callback = True) - state: %s, "
                        "callback: %s. Failed to save data in LCM DB which "
                        "might cause future problem for this upgrade job. "
                        "Upgrade job data: %s"
                        % (state, callback, self.upgrade_job))
            LOG.exception(exception)
        if callback:
            self.callback_when_finished(self.job_id, self)
        self.upgrade_job.set_shadow(False)

    def __is_wf_to_be_delayed(self):
        """
        Checks if prepare shall be delayed for CT purposes
        Delays workflow execution as specified in ct_lcm.cfg file
        """
        LOG.debug("__is_wf_to_be_delayed(self)")

        if self.lcm_config.is_cloud_env():
            return False

        if self.lcm_config.is_supported(lcm_util.EnumVersion.VER_2_0):
            if 'delay_in_verify' in self.lcm_config.get_lcm_test_data():
                req_delay = self.lcm_config.get_lcm_test_data()['delay_in_verify']
                if req_delay == -1:
                    return False
                elif req_delay == 0:
                    LOG.debug("__is_wf_to_be_delayed(self) - req_delay: %s" % (req_delay))
                    return True # Return to rcv queue, and wait for fallback timer to expire
                else:
                    self.__do_delay(req_delay)
        else:
            log_info_text = 'A call to the CT test function, __is_wf_to_be_delayed()'
            LOG.info("%s - %s - LCM FW version: %s, Required version: %s" % (
                log_info_text,
                "Could not be performed as it is not supported by current LCM Framework version.",
                self.lcm_config.get_current_version(),
                lcm_util.EnumVersion.VER_2_0))
        return False

    def __do_delay(self, req_delay):
        """ Perform the delay """
        LOG.debug("__do_delay(self, req_delay) - req_delay: %s" % (req_delay))
        delayed = 0
        while delayed <= req_delay and not self.timeout_expired:
            time.sleep(1)
            LOG.debug("Delay: %s" % (delayed))
            delayed += 1

    def __start_timer(self, timer_value):
        """ Starts the fallback timer """
        self.fallback_timer = threading.Timer(timer_value, self.timeout)
        self.fallback_timer.start()
        LOG.info("Fallback timer (%s s) started (job id: %s)"
                 % (timer_value, self.job_id))

    def __set_state(self, value):
        """ Used to set state value """
        LOG.info("Setting state to '%s' (job id: %s)" % (value, self.job_id))
        self.upgrade_job.set_state(value)

    def __set_progress(self, level, value):
        """ Used to set progress value """
        LOG.info("Setting progress to %s (%s) (job id: %s)" % (value, level, self.job_id))
        self.upgrade_job.set_progress(level, value)

    def __add_operation_result(self, operation_name, requested_time, timestamp,
                               fallback_timeout, result, add_infos):
        """ Adds operation result to upgrade job """
        LOG.info("Setting operation result to - result: %s, add_infos: %s (job id: %s)"
                 % (result, add_infos, self.job_id))
        self.upgrade_job.add_operation_result(operation_name,
                                              requested_time,
                                              timestamp,
                                              fallback_timeout,
                                              result,
                                              add_infos)
