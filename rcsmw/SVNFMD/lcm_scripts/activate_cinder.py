#!/usr/bin/env python

##
# (C) Ericsson AB 2017 All rights reserved.
# The information in this document is the property of Ericsson. Except
# as specifically authorized in writing by Ericsson, the receiver of
# this document shall keep the information contained herein confidential
# and shall protect the same in whole or in part from disclosure and
# dissemination to third parties. Disclosure and disseminations to the
# receivers employees shall only be made on a strict need to know basis.
##

"""
Lifecycle Management Script for upgrade activate operation.
"""

#pylint: disable=W0703
#pylint: disable=R0201
#pylint: disable=R0913
#pylint: disable=R0904
#pylint: disable=R0902
#pylint: disable=R0911
#pylint: disable=R0915
#pylint: disable=W0232
#pylint: disable=W0150
#pylint: disable=R0903
#pylint: disable=C0302

# Temporary added due to old pylint version in Jenkins
# pylint used in HUB (newer version) do not print out this faulty
# warnings/ errors
#pylint: disable=E1002
#pylint: disable=E1101
#pylint: disable=W0201
#pylint: disable=E0203
#pylint: disable=C0103

from lcm import (lcm_util,
                 workflow_handler,
                 vnfm_api_comm,
                 lcm_db_handler,
                 lcm_python_manager,
                 lcm_upgrade_job)

import imp
import hashlib
import calendar

def import_common(module_name):
    """ Used to import dynamically a common module """
    file_handler, file_path, descr = imp.find_module(module_name)
    return imp.load_module(hashlib.md5(file_path).hexdigest(),
                           file_handler,
                           file_path,
                           descr)

lcm_wf_utility = import_common('lcm_wf_utility')
vnf_api_comm = import_common('vnf_api_comm')

import time
import logging
import threading
import os
import base64
from common.http_utils import host_in_url

LOG = logging.getLogger('ACTIVATE_CINDER')

def main(job_id, operation_data, upgrade_job, lcm_config, callback_when_finished):
    """ Main function for activate to initiate the activate request """
    LOG.debug("main(job_id, operation_data, upgrade_job, lcm_config, "
              "callback_when_finished) - job_id: %s, operation_data: %s, "
              "upgrade_job: %s, lcm_config: %s, callback_when_finished: %s"
              % (job_id, operation_data, upgrade_job, lcm_config,
                 callback_when_finished))
    if (upgrade_job.get_state() == lcm_upgrade_job.get_state_enum().PREPARE_COMPLETED):
        activate = startup(job_id, operation_data, upgrade_job, lcm_config,
                           callback_when_finished)
        activate.main(operation_data)
        LOG.info("Activate cinder initiated for JOB_ID: %s" % (job_id))
        return activate
    else:
        raise lcm_util.LcmException(
            lcm_util.LcmException.NOT_ALLOWED,
            "Attempt to execute operation activate for an upgrade job (job_id: %s"
            ") when not expected. Current state: %s, Current workflow: %s"
            % (job_id, upgrade_job.state, upgrade_job.current_workflow))

def startup(job_id, operation_data, upgrade_job, lcm_config, callback_when_finished):
    """ Startup function for activate to startup WF activate """
    LOG.debug("startup(job_id, operation_data, upgrade_job, lcm_config, "
              "callback_when_finished) - job_id: %s, operation_data: %s, "
              "upgrade_job: %s, lcm_config: %s, callback_when_finished: %s"
              % (job_id, operation_data, upgrade_job, lcm_config,
                 callback_when_finished))
    activate = Activate(job_id, upgrade_job, lcm_config, callback_when_finished)
    activate.start()
    LOG.info("WF Activate started up and waits for further instructions for "
              "JOB_ID: %s" % (job_id))
    return activate


def restore_job(job_id, operation_data, upgrade_job, lcm_config, callback):
    """
    Upgrade jobs that were in progress at restart of R-VNMF are restored.
    It means that resources may be released or that if a confirm was in
    progress it may be auto-initiated if From-VNF has been terminated.
    """
    LOG.debug("restore_job(job_id, operation_data, upgrade_job, "
              "lcm_config, callback) - job_id: %s, operation_data: %s, "
              "upgrade_job: %s, lcm_config: %s, callback: %s"
              % (job_id, operation_data, upgrade_job, lcm_config, callback))
    return Activate(job_id, upgrade_job, lcm_config, callback).restore_job()


class EnumWorkingState:
    """ Working states within Activate workflow """
    IDLE = "IDLE"
    WAIT_FOR_CREATE_BACKUP_RESPONSE = "WAIT_FOR_CREATE_BACKUP_RESPONSE"
    WAIT_FOR_TO_VNF_UP_AND_RUNNING = "WAIT_FOR_TO_VNF_UP_AND_RUNNING"
    WAIT_FOR_FETCH_UPGRADE_LOG_RESULT = "WAIT_FOR_FETCH_OF_UPGRADE_LOG_RESULT"
    WAIT_FOR_FETCH_ESI_RESULT = "WAIT_FOR_FETCH_ESI_RESULT"

class Activate(lcm_wf_utility.GenAsyncQueue):
    """ Activate workflow script """
    CURRENT_VERSION = lcm_util.EnumVersion.VER_1_0

    BACKUP_FILE_NAME_AT_STARTUP = 'backup.zip'
    UPGRADE_LOGS_FILE_NAME = 'upgrade_logs.tar.gz.zip'
    ESI_FILE_NAME_ROLLBACK = 'esi_rollback.tar.gz.zip'

    # Actions that are asynchronous towards VNFM
    INSTANTIATE_VNF = 'instantiate'
    TERMINATE_VNF = 'terminate'
    HEAL_VNF = 'heal'
    OPERATE_VNF = 'operate'

    def __init__(self, job_id, upgrade_job, lcm_config, callback_when_finished):
        """ Constructor """
        super(Activate, self).__init__(job_id,
                                       upgrade_job,
                                       lcm_config,
                                       callback_when_finished,
                                       'Activate_' + str(job_id),
                                       LOG,
                                       Activate.CURRENT_VERSION)
        self.is_cancel_ongoing = False
        self.cancel_requested = False
        self.fallback_timeout_flag = None
        self.fallback_timer = None
        self.lcop_rsp_timer = None
        self.lcop_rsp_timeout_value = None
        self.lcop_rsp_timeout_flag = False
        self.action_id = None
        self.working_state = EnumWorkingState.IDLE
        self.vnf_comm_handle_from = None
        self.backup_name = None
        self.ext_operation_data = None
        self.vnfm_url = "http://%s:%s/" % (host_in_url(self.lcm_config.get_vnfm_http_host()),
                                           self.lcm_config.get_vnfm_http_port())
        self.vnfm_comm_handle = vnfm_api_comm.get_vnfm_comm(self.vnfm_url)
        self.vnf_backup_dir_path = None
        self.vnf_port = self.lcm_config.get_vnfi_http_port()
        self.to_vnf_instantiated = False
        self.from_vnf_shutdown = False
        self.vnf_comm_handle_to = None
        self.upgrade_logs_file_path = None
        self.progress_wf = lcm_wf_utility.EnumProgressWf(LOG)
        self.result_wf = lcm_wf_utility.EnumOperationResultWf(LOG)
        self.add_info_trans = lcm_wf_utility.AddInfoTransformer()
        self.esi_file_path = None
        self.add_infos = list()
        self.is_lccn_ongoing_for_to_vnf = False
        self.lccn_ongoing_operation_name = ""
        self.requested_time_for_vnf = None

    def main(self, operation_data):
        """ Main method for script activate """
        LOG.debug('main(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Activate Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def cancel(self, *operation_data):
        """ Cancel requested """
        LOG.debug('cancel(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Activate Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.cancel_requested = True
        self.put((self.function(), operation_data))

    def confirm(self, *operation_data):
        """ Confirm requested """
        LOG.debug('confirm(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Activate Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def backup_status(self, *operation_data):
        """ Backup status response """
        LOG.debug('backup_status(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Activate Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def upgrade_status(self, *operation_data):
        """ Upgrade status response """
        LOG.debug('upgrade_status(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Activate Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def fetch_upgrade_log_result(self, *operation_data):
        """ Fetch of upgrade logs result i.e. the VNF has completed it """
        LOG.debug("fetch_upgrade_log_result(self, operation_data) - operation_data: %s" % (operation_data))
        self.put((self.function(), operation_data))

    def vnf_lccn(self, *operation_data):
        """ VNF lifecycle operation notification """
        LOG.debug('vnf_lccn(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Activate Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def get_action_id(self):
        """ Returns the action id """
        return self.action_id

    def restore_job(self):
        """
        This workflow method is called at start-up to clean-up resources
        and restore the upgrade job, after a RAN-VNFM restart/crash. Any
        resources that might have been allocated but not free marked, due
        to crash/restart, are removed.
        In case the job state before the RAN VNFM restart/crash was
        WAITING_FOR_CONFIRM,  there are two main cases considered:
        1) Confirm was received, but the from-state-vnf was not terminated,
        before the RAN-VNFM restarted/crashed.
        -> If the to-state-vnf is operational, the upgrade state is kept in
        WATING_FOR_CONFIRM, and it is the operator's decision  to re-confirm
        the upgrade or not.
        Otherwise (to-state-vnf is non operational), the upgrade is rolled back.
        2) Confirm was received, and the from-state-vnf was terminated, before
        the RAN-VNFM restarted/crashed
        -> The upgrade is automatically confirmed when RAN-VNFM is up after
        restart/crash, regardless of operational state of the to-state-vnf.
        """
        LOG.debug("restore_job(self) - upgrade_job: %s" % (self.upgrade_job))
        if self.upgrade_job.get_state() == lcm_upgrade_job.get_state_enum().WAITING_FOR_CONFIRM:
            self.upgrade_job.to_vnf_id = self.upgrade_job.resource.to_vnf_id
            # Confirm was received before R-VNFM restart
            if (self.upgrade_job.resource.operation != None and
                self.upgrade_job.resource.wf_name != None and
                self.upgrade_job.resource.requested_time != None):
                try:
                    from_vnf_info = self.__get_vnf_info(self.upgrade_job.vnf_id)
                    # Check if rollback is possible (from-state-vnf exists).
                    # Otherwise, automatically confirm the upgrade.
                    if (from_vnf_info != None and
                        from_vnf_info['instantiationState'] == 'INSTANTIATED'):
                        # Check that To-VNF is OK
                        self.__check_to_vnf_at_restore()
                    else:
                        LOG.info("restore_job(self) - Restart of R-VNFM occured while confirmation"
                                 " was in progress. At startup it is detected that From-VNF either "
                                 "do not exist or is not instantiated i.e. the ongoing upgrade is "
                                 "auto-confirmed. From-VNF info: %s" % (
                                     from_vnf_info))
                        add_info = ['Restart of R-VNFM occured while confirmation was in progress.'
                                    ' At startup it is detected that From-VNF has been terminated '
                                    'i.e. the ongoing upgrade is auto-confirmed']
                        return self.handle_confirm(dict(), True, add_info)

                except Exception as exception:
                    LOG.warning("restore_job(self) - When trying to find out what to do "
                                "after restart of R-VNFM an exception was caught and due"
                                " to this manual decisions is required to decide if the "
                                "activation shall be confirmed or not. Upgrade job id: %s,"
                                " Reason in exception: %s" % (
                                    self.upgrade_job.get_job_id(), exception))
                    add_info = ["A restart of R-VNFM occured while this operation was "
                                "in progress i.e. it is aborted. When trying to find out what to do "
                                "after restart an exception was caught and due to this manual "
                                "decisions is required to decide if the upgrade shall be "
                                "confirmed or not."]
                    add_info.extend(self.add_infos)
                    self.__add_operation_result(
                        self.upgrade_job.resource.operation,
                        self.upgrade_job.resource.requested_time,
                        lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                        self.upgrade_job.resource.fallback_timeout,
                        self.result_wf.get_enum(self.result_wf.FAILURE),
                        self.add_info_trans.get_add_info(add_info))
                    self.upgrade_job.resource.cleanup_wfc()
                    self.__set_job()
                     #Restart fallback timer
                    if self.upgrade_job.resource.fallback_timeout != None:
                        self.fallback_timer = threading.Timer(self.upgrade_job.resource.fallback_timeout,
                                                              self.timeout)
                        self.fallback_timer.start()
                        LOG.debug("Activate Fallback timer started (%s) at restore. Timeout:  %s" %
                                  (self.fallback_timer, self.upgrade_job.resource.fallback_timeout))

            # Confirm was not yet received at R-VNFM restart
            else:
                if self.upgrade_job.resource.fallback_timeout != None:
                    self.fallback_timer = threading.Timer(self.upgrade_job.resource.fallback_timeout,
                                                          self.timeout)
                    self.fallback_timer.start()
                    LOG.debug("Activate Fallback timer started (%s) at restore. Timeout:  %s" %
                              (self.fallback_timer, self.upgrade_job.resource.fallback_timeout))

        else:
            self.__rollback_at_restore()

    def import_upgrade_logs(self, vnf_id):
        """ Requests for import of upgrade logs """
        LOG.debug("import_upgrade_logs(self, vnf_id) - vnf_id: %s" % (vnf_id))
        message_data = ("Working_state: %s, To-VNF id: %s, Attempt from VNF id: %s"
                        "From-VNF id: %s"
                        % (self.working_state, self.upgrade_job.to_vnf_id, vnf_id,
                           self.upgrade_job.vnf_id))
        self.__check(self.working_state == EnumWorkingState.WAIT_FOR_FETCH_UPGRADE_LOG_RESULT,
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs unexpected working state. %s"
                     % (message_data))
        self.__check(self.upgrade_job.to_vnf_id == vnf_id,
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs unexpected VNF ID. %s"
                     % (message_data))
        return self.upgrade_logs_file_path

    def import_esi(self, vnf_id):
        """ Requests for import of ESI """
        LOG.debug("import_esi(self, vnf_id) - vnf_id: %s" % (vnf_id))
        message_data = ("Working_state: %s, To-VNF id: %s, Attempt from VNF id: %s"
                        "From-VNF id: %s"
                        % (self.working_state, self.upgrade_job.to_vnf_id, vnf_id,
                           self.upgrade_job.vnf_id))
        self.__check(self.working_state == EnumWorkingState.WAIT_FOR_FETCH_ESI_RESULT,
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs in unexpected working state. %s"
                     % (message_data))
        self.__check((self.upgrade_job.vnf_id == vnf_id or self.upgrade_job.to_vnf_id == vnf_id),
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs from unexpected VNF ID. %s"
                     % (message_data))
        return self.esi_file_path

    #
    # Activate thread loop - Aynchronous operations
    #
    def handle_main(self, operation_data):
        """
        Script entry point for the asynchronous lcm operation activate
        """
        LOG.debug("handle_main(self, operation_data) - "
                  "operation_data: %s" % (operation_data))
        try:
            self.upgrade_job.resource.previous_state = self.upgrade_job.get_state()
            self.ext_operation_data = operation_data
            self.fallback_timer = threading.Timer(self.get_fallback_timeout(self.ext_operation_data),
                                                  self.timeout)
            self.fallback_timer.start()
            LOG.debug("Activate Fallback timer started (%s). Timeout:  %s" %
                      (self.fallback_timer, self.get_fallback_timeout(self.ext_operation_data)))
            self.__set_state(lcm_upgrade_job.get_state_enum().ACTIVATION_IN_PROGRESS)
            self.__set_progress(
                2,
                self.progress_wf.get_enum(self.progress_wf.PRE_CHECK))
            self.upgrade_job.resource.operation = lcm_upgrade_job.get_operation_enum().ACTIVATE
            self.requested_time_for_vnf = str(calendar.timegm(time.localtime()))
            self.upgrade_job.resource.wf_name = lcm_upgrade_job.get_workflow_enum().ACTIVATE_CINDER_OPERATION
            self.upgrade_job.resource.requested_time = lcm_util.get_lcm_log_handler().get_lcm_timestamp()
            self.upgrade_job.resource.fallback_timeout = self.get_fallback_timeout(operation_data)
            self.lcop_rsp_timeout_value = self.__get_lcop_tmo()
            self.__set_resource()
            del self.add_infos[:]

            verify_check_add_infos = self.__execute_auto_verify(
                self.__get_verify_workflow_path(),
                self.upgrade_job.get_job_id(),
                dict(),
                self.upgrade_job,
                self.lcm_config,
                None)
            LOG.debug("handle_main(self, operation_data) - verify_check_add_infos: %s"
                      % (verify_check_add_infos))
            self.add_infos.extend(verify_check_add_infos)
            self.__is_cancel_requested()
            self.__is_fallback_timeout()
            self.vnf_comm_handle_from = vnf_api_comm.get_vnf_comm(
                self.upgrade_job.get_vnf_id(),
                self.vnf_port,
                self.vnfm_url)
            self.__set_progress(
                10,
                self.progress_wf.get_enum(self.progress_wf.STOP_CONFIGURATION_FOR_FROM_STATE_VNF))

            LOG.info("Requesting From-VNF to prevent configuration changes")
            self.vnf_comm_handle_from.stop_configuration()
            self.upgrade_job.resource.is_configuration_stoped = True
            self.__set_resource()

            self.backup_name = "%s--%s.%s" % (self.upgrade_job.get_job_id(),
                                              self.upgrade_job.get_vnf_id(),
                                              "backup")

            LOG.info("Requesting From-VNF to create a backup with name '%s'"
                     % (self.backup_name))
            self.__set_progress(
                15,
                self.progress_wf.get_enum(self.progress_wf.CREATING_BACKUP_FOR_FROM_STATE_VNF))
            self.working_state = EnumWorkingState.WAIT_FOR_CREATE_BACKUP_RESPONSE
            self.__delete_vnf_backup(self.backup_name)
            self.action_id = self.vnf_comm_handle_from.create_upgrade_backup_v2(
                self.backup_name)

            # Wait for backup_status
            return True

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(self.ext_operation_data)
            return False

        except lcm_wf_utility.WorkflowTmoException as exception:
            self.handle_timeout(self.ext_operation_data)
            return False

        except lcm_wf_utility.VerifyWorkflowException as exception:
            LOG.debug("Activate of the UP with JOB ID (%s) failed due to verify %s"
                      % (self.job_id, str(exception)))
            self.handle_error(exception.add_infos, operation_data)
            return False

        except (lcm_util.LcmException,
                vnf_api_comm.VnfCommException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("Activate of the UP with JOB ID (%s) failed due to %s"
                      % (self.job_id, str(exception)))
            self.handle_error(str(exception), operation_data)
            return False

        except Exception as exception:
            LOG.error("Activate of the UP with JOB ID (%s) failed due to %s"
                      % (self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), operation_data)
            return False

    def handle_confirm(self, operation_data, continues = False, add_info = list()):
        """
        Script entry point for the case where and ongoing upgrade is confirmed
        """
        LOG.debug('handle_confirm(self, operation_data, continues = False, add_info = '
                  ') - operation_data: %s, continues: %s, add_info: %s'
                  % (operation_data, continues, add_info))
        try:
            if self.is_lccn_ongoing_for_to_vnf and not continues:
                self.__confirm_rejected_lccn_ongoing(add_info)
                LOG.info("Confirm not allowed since an lccn operation is ongoing"
                         " on To-VNF")
                return True

            LOG.info("Confirm of Activate is ongoing")
            self.__set_state(lcm_upgrade_job.get_state_enum().CONFIRM_IN_PROGRESS)
            self.__set_progress(
                75,
                self.progress_wf.get_enum(self.progress_wf.CONFIRMING_TO_STATE_VNF))
            del self.add_infos[:]
            if self.fallback_timer != None:
                self.fallback_timer.cancel()
            self.upgrade_job.resource.fallback_timeout = self.get_fallback_timeout(operation_data)
            self.fallback_timer = threading.Timer(
                self.upgrade_job.resource.fallback_timeout,
                self.timeout)
            self.fallback_timer.start()
            LOG.debug("Confirm Fallback timer started (%s). Timeout:  %s" %
                      (self.fallback_timer, self.upgrade_job.resource.fallback_timeout))
            if not continues:
                self.upgrade_job.resource.requested_time = lcm_util.get_lcm_log_handler().get_lcm_timestamp()
                self.upgrade_job.resource.operation = lcm_upgrade_job.get_operation_enum().CONFIRM
                self.__set_resource()
            self.__unsubscribe_for_lcop(self.upgrade_job.resource.subscription_url_id)
            LOG.info("Sending confirm to To-VNF")
            self.__confirm_to_vnf()

            LOG.info("Requesting VNFM to delete From-VNF")
            self.__set_progress(
                85,
                self.progress_wf.get_enum(self.progress_wf.TERMINATING_FROM_STATE_VNF))
            self.__terminate_vnf(self.upgrade_job.get_vnf_id(), "From-")
            self.__remove_vnf_id(self.upgrade_job.get_vnf_id(), "From-")

            add_info.append("To-VNF ID: %s" % (self.upgrade_job.get_to_vnf_id()))
            add_info.extend(self.add_infos)
            self.__add_operation_result(
                self.upgrade_job.resource.operation,
                self.upgrade_job.resource.requested_time,
                lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                self.upgrade_job.resource.fallback_timeout,
                self.result_wf.get_enum(self.result_wf.SUCCESS),
                self.add_info_trans.get_add_info(add_info))
            self.handle_cleanup(
                lcm_upgrade_job.get_state_enum().CONFIRM_COMPLETED,
                self.progress_wf.get_enum(self.progress_wf.TERMINATING_FROM_STATE_VNF))
            return False

        except Exception as exception:
            LOG.warning("Failed to remove (clean up) the From-VNF at confirmation. Job id: %s,"
                        " Reason: %s (ignored since can't do anything else))"
                        % (self.job_id, exception))
            LOG.exception(exception)
            try:
                add_info = ["%s" % (exception)]
                add_info.extend(self.add_infos)
                self.__add_operation_result(
                    self.upgrade_job.resource.operation,
                    self.upgrade_job.resource.requested_time,
                    lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                    self.get_fallback_timeout(operation_data),
                    self.result_wf.get_enum(self.result_wf.FAILURE),
                    self.add_info_trans.get_add_info(add_info))
                self.handle_cleanup(
                    self.upgrade_job.resource.previous_state,
                    self.progress_wf.get_enum(self.progress_wf.TERMINATING_FROM_STATE_VNF))

            finally:
                return False

    def handle_backup_status(self, stat_operation_data):
        """ Handling of backup status response """
        LOG.debug('handle_backup_status(self, stat_operation_data) - stat_operation_data: %s'
                  % (stat_operation_data))

        try:
            if self.working_state == EnumWorkingState.WAIT_FOR_CREATE_BACKUP_RESPONSE:
                self.__handle_create_backup_response(stat_operation_data)
            else:
                LOG.warning("handle_backup_status(self, stat_operation_data) - "
                            "stat_operation_data: %s - Received this operation "
                            "when it was NOT expected (ignored). working_state:"
                            " %s, upgrade_job: %s"
                            % (stat_operation_data, self.working_state, self.upgrade_job))
            return True

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(self.ext_operation_data)
            return False

        except lcm_wf_utility.WorkflowTmoException as exception:
            self.handle_timeout(self.ext_operation_data)
            return False

        except (lcm_util.LcmException,
                vnf_api_comm.VnfCommException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("handle_backup_status(self, stat_operation_data) - "
                      "stat_operation_data: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (stat_operation_data, self.job_id, exception))
            self.handle_error(str(exception), stat_operation_data)
            return False

        except Exception as exception:
            LOG.error("handle_backup_status(self, stat_operation_data) - "
                      "stat_operation_data: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (stat_operation_data, self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), self.ext_operation_data)
            return False



    def handle_fetch_upgrade_log_result(self, result, stop_traffic_data):
        """
        Handling of the fetch of upgrade logs when completed
        Argument result contains the result of the fetch
        """
        LOG.debug("handle_fetch_upgrade_log_result(self, result, stop_traffic_data)"
                  " - result: %s, stop_traffic_data: %s" % (result, stop_traffic_data))

        try:
            if result['result'] == 'SUCCESS':
                LOG.info("Requesting From-VNF to be stopped")
                self.__set_progress(
                    62,
                    self.progress_wf.get_enum(self.progress_wf.STOP_EXECUTION_OF_FROM_STATE_VNF))
                self.__operate_vnf(self.upgrade_job.vnf_id, 'STOPPED', 'From', False)

                self.from_vnf_shutdown = True
                self.__is_cancel_requested()

                LOG.info("Requesting To-VNF to start traffic")
                self.__set_progress(
                    65,
                    self.progress_wf.get_enum(self.progress_wf.START_TRAFFIC_FOR_TO_STATE_VNF))

                self.__get_vnf_comm_to().start_traffic_v2(stop_traffic_data)
                self.__is_cancel_requested()

                add_info = ["To-VNF ID: %s" % (self.upgrade_job.get_to_vnf_id())]
                add_info.extend(self.add_infos)
                self.__add_operation_result(
                    self.upgrade_job.resource.operation,
                    self.upgrade_job.resource.requested_time,
                    lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                    self.get_fallback_timeout(self.ext_operation_data),
                    self.result_wf.get_enum(self.result_wf.SUCCESS),
                    self.add_info_trans.get_add_info(add_info))
                self.upgrade_job.resource.cleanup_wfc()
                self.__set_progress(
                    70,
                    self.progress_wf.get_enum(self.progress_wf.WAITING_FOR_CONFIRMATION))
                self.__finish(lcm_upgrade_job.get_state_enum().WAITING_FOR_CONFIRM, False)
                return True

            else:
                reason = ("Fetch of upgrade logs failed (for To-VNF) due to failure"
                          " reported from To-VNF (%s). Reported failure: %s"
                          % (self.upgrade_job.get_to_vnf_id(), result))
                LOG.error("handle_fetch_upgrade_log_result(self, result) - "
                          "%s" % (reason))
                self.handle_error(reason, self.ext_operation_data)
                return False

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(self.ext_operation_data)
            return False

        except (lcm_util.LcmException,
                vnf_api_comm.VnfCommException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("handle_fetch_upgrade_log_result(self, result) - "
                      "result: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (result, self.job_id, exception))
            self.handle_error(str(exception), self.ext_operation_data)
            return False

        except Exception as exception:
            LOG.error("handle_fetch_upgrade_log_result(self, result) - "
                      "result: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (result, self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), self.ext_operation_data)
            return False



    def handle_upgrade_status(self, upg_operation_data):
        """ Handling of upgrade status response """
        LOG.debug('handle_upgrade_status(self, upg_operation_data) - upg_operation_data: %s'
                  % (upg_operation_data))

        try:
            if self.__is_vnf_known(upg_operation_data['vnf_id']):
                if upg_operation_data['status']['status_code'] == "WAITING_FOR_COMMIT":
                    if self.working_state == EnumWorkingState.WAIT_FOR_TO_VNF_UP_AND_RUNNING:
                        return self.__continue_activation(upg_operation_data)
                    else:
                        LOG.warning("handle_upgrade_status(self, upg_operation_data) - "
                                    "upg_operation_data: %s. Upgrade status information "
                                    "received with status code WAITING_FOR_COMMIT when not "
                                    "expecting it i.e. internal working state is not "
                                    "WAIT_FOR_TO_VNF_UP_AND_RUNNING. Upgrade job: %s (ignored)"
                                    % (upg_operation_data, self.upgrade_job))

                elif upg_operation_data['status']['status_code'] == "INFO":
                    LOG.info("Upgrade status information received from VNF (%s) with "
                             "status code: INFO and message: '%s'"
                             % (upg_operation_data['vnf_id'],
                                upg_operation_data['status']['info']))
                    self.add_infos.append("Upgrade status information received from VNF (%s) with "
                                          "status code: INFO and message: '%s'"
                                          % (upg_operation_data['vnf_id'],
                                             upg_operation_data['status']['info']))

                elif upg_operation_data['status']['status_code'] == "FAILURE":
                    LOG.error("handle_upgrade_status(self, upg_operation_data) - "
                              "Upgrade status information received from VNF (%s) with"
                              " status code: FAILURE and message: %s"
                              % (upg_operation_data['vnf_id'],
                                 upg_operation_data['status']['info']))
                    self.handle_error("Upgrade status information received from VNF (%s)"
                                      " with status code: FAILURE and message: %s"
                                      % (upg_operation_data['vnf_id'],
                                         upg_operation_data['status']['info']),
                                      upg_operation_data)
                    return False

                else:
                    LOG.info("Upgrade status information received from VNF (%s) with "
                             "unexpected status code: %s and message: '%s'"
                             % (upg_operation_data['vnf_id'],
                                upg_operation_data['status']['status_code'],
                                upg_operation_data['status']['info']))

            else:
                LOG.warning("handle_upgrade_status(self, upg_operation_data) - "
                            "upg_operation_data: %s. Upgrade status information "
                            "received from an unknown VNF ID (ignored), upgrade job: %s"
                            % (upg_operation_data, self.upgrade_job))

            return True

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(self.ext_operation_data)
            return False

        except lcm_wf_utility.WorkflowTmoException as exception:
            self.handle_timeout(self.ext_operation_data)
            return False

        except (lcm_util.LcmException,
                vnf_api_comm.VnfCommException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("handle_upgrade_status(self, upg_operation_data) - "
                      "upg_operation_data: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (upg_operation_data, self.job_id, exception))
            self.handle_error(str(exception), upg_operation_data)
            return False

        except Exception as exception:
            LOG.error("handle_upgrade_status(self, upg_operation_data) - "
                      "upg_operation_data: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (upg_operation_data, self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), self.ext_operation_data)
            return False

    def handle_vnf_lccn(self, vnf_lccn_operation_data):
        """ Handling of VNF life cycle change notification """
        LOG.debug('handle_vnf_lccn(self, vnf_lccn_operation_data) - vnf_lccn_operation_data: %s'
                  % (vnf_lccn_operation_data))

        try:
            if self.__is_vnf_known(vnf_lccn_operation_data['vnfInstanceId']):
                return self.__act_on_lccn_operation_indication(vnf_lccn_operation_data)
            else:
                LOG.warning("handle_vnf_lccn(self, vnf_lccn_operation_data) - "
                            "vnf_lccn_operation_data: %s. vnf lccn operation notification "
                            "received from an unknown VNF ID (ignored), upgrade job: %s"
                            % (vnf_lccn_operation_data, self.upgrade_job))
                return True

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(self.ext_operation_data)
            return False

        except (lcm_util.LcmException,
                vnf_api_comm.VnfCommException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("handle_vnf_lccn(self, vnf_lccn_operation_data) - "
                      "vnf_lccn_operation_data: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (vnf_lccn_operation_data, self.job_id, exception))
            self.handle_error(str(exception), vnf_lccn_operation_data)
            return False

        except Exception as exception:
            LOG.error("handle_vnf_lccn(self, vnf_lccn_operation_data) - "
                      "vnf_lccn_operation_data: %s. Activate of the UP with "
                      "JOB ID (%s) failed due to %s"
                      % (vnf_lccn_operation_data, self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), self.ext_operation_data)
            return False

    def __act_on_lccn_operation_indication(self, vnf_lccn_operation_data):
        """ Acts on the indicated operation performed on To-VNF """
        LOG.debug("__act_on_lccn_operation_indication(self, vnf_lccn_operation_data) - "
                  "vnf_lccn_operation_data: %s" % (vnf_lccn_operation_data))

        if (vnf_lccn_operation_data['operation'] == 'heal'):
            return self.__act_on_heal_operation_indication(vnf_lccn_operation_data)

        elif vnf_lccn_operation_data['operation'] == 'terminate':
            return self.__act_on_terminate_operation_indication(vnf_lccn_operation_data)

        elif vnf_lccn_operation_data['operation'] == 'operate':
            return self.__act_on_operate_operation_indication(vnf_lccn_operation_data)

        else:
            LOG.info("handle_vnf_lccn(self, vnf_lccn_operation_data) - "
                     "Received lccn information not subscribed on (ignored)."
                     "vnf_lccn_operation_data: %s " % (vnf_lccn_operation_data))
        return True

    def __act_on_heal_operation_indication(self, vnf_lccn_operation_data):
        """ Act on heal indication of To-VNF """
        LOG.debug("__act_on_heal_operation_indication(self, vnf_lccn_operation_data)"
                  " - vnf_lccn_operation_data: %s" % (vnf_lccn_operation_data))

        if (vnf_lccn_operation_data['status'] == 'success' or
            vnf_lccn_operation_data['status'] == 'failure'):
            reason = ("A heal of To-VNF (%s) has been requested when not expected. "
                      "The result of the heal request is %s. "
                      "The activation will be rolled back due to this."
                      % (vnf_lccn_operation_data['vnfInstanceId'],
                         vnf_lccn_operation_data['status']))
            LOG.info("handle_vnf_lccn(self, vnf_lccn_operation_data) - "
                     "vnf_lccn_operation_data: %s. %s"
                     % (vnf_lccn_operation_data, reason))
            self.handle_error(reason, self.ext_operation_data)
            self.is_lccn_ongoing_for_to_vnf = False
            return False

        elif vnf_lccn_operation_data['status'] == 'start':
            self.is_lccn_ongoing_for_to_vnf = True
            self.lccn_ongoing_operation_name = 'heal'
            return True

    def __act_on_terminate_operation_indication(self, vnf_lccn_operation_data):
        """ Act on terminate indication of To-VNF """
        LOG.debug("__act_on_terminate_operation_indication(self, vnf_lccn_operation_data)"
                  " - vnf_lccn_operation_data: %s" % (vnf_lccn_operation_data))

        if vnf_lccn_operation_data['status'] == 'success':
            reason = ("To-VNF (%s) has been terminated when not expected. "
                      "The activation will be rolled back due to this."
                      % (vnf_lccn_operation_data['vnfInstanceId']))
            LOG.info("handle_vnf_lccn(self, vnf_lccn_operation_data) - "
                     "vnf_lccn_operation_data: %s. %s"
                     % (vnf_lccn_operation_data, reason))
            self.handle_error(reason, self.ext_operation_data)
            self.is_lccn_ongoing_for_to_vnf = False
            return False

        elif vnf_lccn_operation_data['status'] == 'start':
            self.is_lccn_ongoing_for_to_vnf = True
            self.lccn_ongoing_operation_name = 'terminate'
            return True

        elif vnf_lccn_operation_data['status'] == 'failure':
            self.is_lccn_ongoing_for_to_vnf = False
            return True

    def __act_on_operate_operation_indication(self, vnf_lccn_operation_data):
        """ Act on operate indication of To-VNF """

        LOG.debug("__act_on_operate_operation_indication(self, vnf_lccn_operation_data)"
                  " - vnf_lccn_operation_data: %s" % (vnf_lccn_operation_data))
        if (vnf_lccn_operation_data['status'] == 'success' and
            not self.__check_vnf_is_operational(vnf_lccn_operation_data['vnfInstanceId'])):
            reason = ("To-VNF (%s) has been stopped when not expected. "
                      "The activation will be rolled back due to this."
                      % (vnf_lccn_operation_data['vnfInstanceId']))
            LOG.info("handle_vnf_lccn(self, vnf_lccn_operation_data) - "
                     "vnf_lccn_operation_data: %s. %s"
                    % (vnf_lccn_operation_data, reason))
            self.handle_error(reason, self.ext_operation_data)
            self.is_lccn_ongoing_for_to_vnf = False
            return False

        elif vnf_lccn_operation_data['status'] == 'start':
            self.is_lccn_ongoing_for_to_vnf = True
            self.lccn_ongoing_operation_name = 'operate'
            return True

        elif vnf_lccn_operation_data['status'] == 'failure':
            self.is_lccn_ongoing_for_to_vnf = False
            return True
        else:
            self.is_lccn_ongoing_for_to_vnf = False
            return True

    def __get_vnf_comm_to(self):
        """ Returns a handle to vnf_comm_to """
        if self.vnf_comm_handle_to == None:
            if self.upgrade_job.get_to_vnf_id() != None:
                self.vnf_comm_handle_to = vnf_api_comm.get_vnf_comm(
                    self.upgrade_job.get_to_vnf_id(),
                    self.vnf_port,
                    self.vnfm_url)
            else:
                LOG.info("An attempt was made to get a handle to To-VNF when it does not exist")

        return self.vnf_comm_handle_to

    def __continue_activation(self, upg_operation_data):
        """ Continues the activation after that the TO VNF is up and running """
        LOG.debug("__continue_activation(self, upg_operation_data) - "
                  "upg_operation_data: %s" % (upg_operation_data))

        if (upg_operation_data['vnf_id'] == self.upgrade_job.get_to_vnf_id()):
            self.__is_cancel_requested()
            self.__is_fallback_timeout()
            self.__set_progress(
                60,
                self.progress_wf.get_enum(self.progress_wf.STOP_TRAFFIC_FOR_FROM_STATE_VNF))
            stop_traffic_data = self.vnf_comm_handle_from.stop_traffic_v2(self.requested_time_for_vnf,
                                                                          "UpgradeNormal")
            self.upgrade_job.resource.is_traffic_stoped = True
            self.__set_resource()

            self.__is_cancel_requested()
            self.__is_fallback_timeout()
            LOG.info("Requesting From-VNF to export certain logs to tmp directory (entries recorded"
                     " since To-VNF instantiation)")
            self.upgrade_logs_file_path ='%s%s/%s' % (self.lcm_config.get_lcm_workflow_file_path(),
                                                      self.upgrade_job.get_job_id(),
                                                      self.UPGRADE_LOGS_FILE_NAME)
            self.vnf_comm_handle_from.export_upgrade_logs(self.upgrade_logs_file_path)

            LOG.info("Requesting To-VNF to import upgrade logs from From-VNF")
            self.working_state = EnumWorkingState.WAIT_FOR_FETCH_UPGRADE_LOG_RESULT
            result = self.__get_vnf_comm_to().fetch_upgrade_logs()
            return self.handle_fetch_upgrade_log_result(result, stop_traffic_data)

        else:
            LOG.warning("__continue_activation(self, upg_operation_data) - "
                        "upg_operation_data: %s. Upgrade status indication with status "
                        "code WAITING_FOR_COMMIT has been received from the From-VNF which "
                        "is not expected (ignored), Upgrade job: %s"
                        % (upg_operation_data, self.upgrade_job))
            return True

    def __handle_create_backup_response(self, operation_data):
        """
        Handling of activation after response has been received from
        create backup
        """
        LOG.debug("__handle_create_backup_response(self, operation_data)"
                  " - operation_data: %s" % (operation_data))

        self.working_state = EnumWorkingState.IDLE
        self.__check_backup_response(operation_data, 'create')

        self.__set_progress(
            40,
            self.progress_wf.get_enum(self.progress_wf.CREATING_TO_STATE_VNF))
        vnf_info = self.vnfm_comm_handle.get_vnf_info(
            self.upgrade_job.get_vnf_id(),
            [vnfm_api_comm.VnfmComm.EXT_VIRTUAL_LINKS,
             vnfm_api_comm.VnfmComm.STORAGE])
        ext_virtual_links = vnf_info.get_ext_virtual_links()
        LOG.debug("External virtual links for From-VNF: %s" % (ext_virtual_links))
        storages = vnf_info.get_storages()
        LOG.debug("Storages for From-VNF: %s" % (storages))
        LOG.info("Requesting VNFM to create a new To-VNF")
        self.working_state = EnumWorkingState.WAIT_FOR_TO_VNF_UP_AND_RUNNING
        self.__instantiate_vnf(vnf_info.instance_name,
                               vnf_info.description,
                               ext_virtual_links,
                               storages)
        self.__is_cancel_requested()
        self.__is_fallback_timeout()
        self.__subscribe_for_lcop(self.upgrade_job.to_vnf_id, ['heal', 'operate', 'terminate'])

        self.__upgrade_clone_complete()
        self.__set_progress(
            50,
            self.progress_wf.get_enum(self.progress_wf.WAITING_FOR_VNF_APPLICATION_READY))
        return True

    def __check_backup_response(self, operation_data, backup_function):
        """ Checks that response is OK otherwise exception is raised """
        LOG.debug("__check_backup_response(operation_data, backup_function)"
                  " - operation_data: %s, backup_function: %s"
                  % (operation_data, backup_function))

        reason = "Failure reported"
        if operation_data['status']['status_code'] == "SUCCESS":
            if operation_data['action_id'] == self.action_id:
                return
            else:
                reason = "Action id didn't match"

        LOG.error("__check_backup_response(self, operation_data) - "
                  "Got a %s backup response from VNF indication failure."
                  " Reason: %s. Aborting the activation procedure. Received "
                  "data: %s, Waiting for action_id: %s, upgrade "
                  "job: %s"
                  % (backup_function, reason, operation_data, self.action_id,
                     self.upgrade_job))
        raise lcm_wf_utility.WorkflowException(
            "Got a %s backup response from VNF indication failure."
            " Reason: %s. Aborting the activation procedure. Received data from VNF: "
            "%s, Waiting for action_id: %s"
            % (backup_function, reason, operation_data, self.action_id))

    def __is_vnf_known(self, from_vnf_id):
        """ Checks that vnf id is known for this workflow """
        LOG.debug("__is_vnf_known(from_vnf_id) - from_vnf_id: %s" % (from_vnf_id))

        if ((from_vnf_id == self.upgrade_job.get_vnf_id()) or
            (from_vnf_id == self.upgrade_job.get_to_vnf_id())):
            return True
        else:
            return False

    def handle_cleanup(self, state, final_progress_value):
        """
        Script entry point for the case where an ongoing job is finished and
        needs to be cleaned up
        """
        LOG.debug('handle_cleanup(self, state, final_progress_value) - state: %s,'
                  ' final_progress_value: %s' % (state, final_progress_value))

        if self.fallback_timer != None:
            self.fallback_timer.cancel()
        self.__remove_upgrade_logs_file()
        self.upgrade_job.resource.cleanup()
        self.__finish(state)
        self.__set_progress(100, final_progress_value)
        self.__set_progress(
            0,
            self.progress_wf.get_enum(self.progress_wf.IDLE))
        self.upgrade_job = None
        self.is_cancel_ongoing = False

    def handle_cancel(self, operation_data):
        """
        Script entry point for the case where an ongoing job is canceled
        """
        LOG.debug('handle_cancel(self, operation_data) - operation_data: %s'
                  % (operation_data))

        self.__handle_interrupt("cancel", operation_data)
        return False

    def handle_timeout(self, operation_data):
        """
        Script entry point for the case where the fallback timer expired
        """
        LOG.debug('handle_timeout(self, operation_data) - operation_data: %s'
                  % (operation_data))

        self.__handle_interrupt("timeout", operation_data)
        return False

    def __handle_interrupt(self, interrupt_type, operation_data):
        """
        Handles the case where an ongoing job is interrupted
        """
        LOG.debug('handle_interrupt(self, interrupt_type, operation_data) - interrupt_type: %s, '
                  'operation_data: %s' % (interrupt_type, operation_data))

        #Workflow Progress and Result data assigments
        init_progress_level = 90
        cleanup_progress_level = 92
        if interrupt_type == "cancel":
            init_progress_value = self.progress_wf.get_enum(
                self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED)
            cleanup_progress_value = self.progress_wf.get_enum(
                self.progress_wf.CLEANUP_DUE_TO_CANCEL_REQUEST)
            result = self.result_wf.get_enum(self.result_wf.CANCELLED)
            add_info = ["Operation cancelled"]
        elif interrupt_type == "timeout":
            init_progress_value = self.progress_wf.get_enum(
                self.progress_wf.FALLBACK_TIMER_EXPIRED)
            cleanup_progress_value = self.progress_wf.get_enum(
                self.progress_wf.CLEANUP_DUE_TO_TIMEOUT)
            result = self.result_wf.get_enum(self.result_wf.FALLBACK_TIMEOUT)
            add_info = ["Fallback timer expired"]

        self.__set_progress(init_progress_level, init_progress_value)
        if interrupt_type == "cancel":
            self.is_cancel_ongoing = True
        self.__cancel_from_vnf()
        self.__unsubscribe_for_lcop(self.upgrade_job.resource.subscription_url_id)
        self.__esi_handling()
        self.__heal_from_vnf_if_needed("The upgrade job (%s) has been cancelled"
                                       % (self.upgrade_job.get_job_id()),
                                       self.upgrade_job.resource.is_traffic_stoped,
                                       self.upgrade_job.get_to_vnf_id())
        self.__set_progress(cleanup_progress_level, cleanup_progress_value)
        self.__terminate_to_vnf()
        self.__start_configuration(self.upgrade_job.resource.is_configuration_stoped,
                                   self.upgrade_job.resource.is_traffic_stoped,
                                   self.upgrade_job.get_to_vnf_id())
        add_info.extend(self.add_infos)
        requested_time = self.upgrade_job.resource.requested_time
        if self.upgrade_job.get_state() == lcm_upgrade_job.get_state_enum().WAITING_FOR_CONFIRM:
            requested_time = self.upgrade_job.get_operation_results()[-1].get_requested_time()
        self.__add_operation_result(self.upgrade_job.resource.operation,
                                    requested_time,
                                    lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                                    self.get_fallback_timeout(operation_data),
                                    result,
                                    self.add_info_trans.get_add_info(add_info))
        self.handle_cleanup(
            self.upgrade_job.resource.previous_state,
            cleanup_progress_value)

    def handle_error(self, reason, operation_data):
        """
        Workflow entry point for the case where an unexpected problem occured
        Arg reason may be a string or list
        """
        LOG.debug("handle_error(self, reason, operation_data) - reason: %s, "
                  "operation_data: %s" % (reason, operation_data))

        self.__set_progress(
            92,
            self.progress_wf.get_enum(self.progress_wf.CLEANUP_DUE_TO_FAILURE))
        self.__unsubscribe_for_lcop(self.upgrade_job.resource.subscription_url_id)
        self.__esi_handling()
        self.__heal_from_vnf_if_needed("A problem has been detected during the upgrade and"
                                       " hence it is aborted (job id: %s)"
                                       % (self.upgrade_job.get_job_id()),
                                       self.upgrade_job.resource.is_traffic_stoped,
                                       self.upgrade_job.get_to_vnf_id())
        self.__terminate_to_vnf()
        self.__start_configuration(self.upgrade_job.resource.is_configuration_stoped,
                                   self.upgrade_job.resource.is_traffic_stoped,
                                   self.upgrade_job.get_to_vnf_id())
        if not type(reason) is list:
            reason = [reason]
        reason.extend(self.add_infos)
        requested_time = self.upgrade_job.resource.requested_time
        if (self.upgrade_job.get_state() ==
            lcm_upgrade_job.get_state_enum().WAITING_FOR_CONFIRM):
            requested_time = self.upgrade_job.get_operation_results()[-1].get_requested_time()
        self.__add_operation_result(
            self.upgrade_job.resource.operation,
            requested_time,
            lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
            self.get_fallback_timeout(operation_data),
            self.result_wf.get_enum(self.result_wf.FAILURE),
            self.add_info_trans.get_add_info(reason))
        self.handle_cleanup(
            self.upgrade_job.resource.previous_state,
            self.progress_wf.get_enum(self.progress_wf.CLEANUP_DUE_TO_FAILURE))

    def __terminate_to_vnf(self):
        """ Terminates any created TO VNF, if exists """
        LOG.debug("__terminate_to_vnf(self)")
        try:
            if self.upgrade_job.get_to_vnf_id() != None:
                LOG.info("Requesting VNFM terminate To-VNF and delete "
                         "its VNF ID (%s) due to rollback of the activation"
                         % (self.upgrade_job.get_to_vnf_id()))
                self.__set_progress(
                    90,
                    self.progress_wf.get_enum(self.progress_wf.TERMINATING_TO_STATE_VNF))
                self.__terminate_vnf(self.upgrade_job.get_to_vnf_id(), "To-")
                self.__remove_vnf_id(self.upgrade_job.get_to_vnf_id(), "To-")
                self.upgrade_job.set_to_vnf_id(None)

        except Exception as exception:
            LOG.info("__terminate_to_vnf(self) - Exception caught when trying to"
                     " terminate to VNF and remove its VNF id i.e. resources might"
                     " still be allocated due to this")
            LOG.exception(exception)

    def __start_configuration(self, is_configuration_stoped, is_traffic_stoped, to_vnf_id):
        """ Starts the configuration if needed i.e. removes the configuration lock """
        LOG.debug("__start_configuration(is_configuration_stoped, is_traffic_stoped, to_vnf_id)"
                  " - is_configuration_stoped: %s, is_traffic_stoped: %s, to_vnf_id: %s"
                  % (is_configuration_stoped, is_traffic_stoped, to_vnf_id))
        try:
            if (is_configuration_stoped == True and is_traffic_stoped == False):
                LOG.info("Requesting From-VNF % to allow configuration changes")
                self.vnf_comm_handle_from.start_configuration()

        except Exception as exception:
            LOG.error("__start_configuration_vnf(self, is_configuration_stoped, "
                      "is_traffic_stoped, to_vnf_id) - Exception caught when sending operation"
                      " start configuration to From-VNF. The From-VNF will be "
                      "healed instead - is_configuration_stoped: %s, is_traffic_stoped:"
                      " %s, to_vnf_id: %s"
                      % (is_configuration_stoped, is_traffic_stoped, to_vnf_id))
            LOG.exception(exception)
            self.__heal_from_vnf("Failure reported when sending operation start"
                                 " configuration to From-VNF to allow configuration"
                                 " changes. To solve that heal of the From-VNF is"
                                 " required.", to_vnf_id)

    def __react_to_any_cancel_request(self, is_interrupt_allowed):
        """ Indicate to end user that cancel has been accepted """
        if not self.is_cancel_ongoing:
            if (self.cancel_requested and
                is_interrupt_allowed and
                (self.upgrade_job.get_progress_detail() !=
                 lcm_upgrade_job.get_progress_enum().CANCEL_ONGOING_OPERATION_REQUESTED)):
                LOG.debug("Operation %s will be aborted since operation "
                          "Cancel has been received"
                          %(self.upgrade_job.resource.operation))
                self.__set_progress(
                    92,
                    self.progress_wf.get_enum(self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED))

    def __is_cancel_requested(self):
        """ Check if cancel is requested and act on it """
        if not self.is_cancel_ongoing:
            if self.cancel_requested:
                LOG.debug("Operation %s is aborted since operation "
                          "Cancel has been received"
                          % (self.upgrade_job.resource.operation))
                self.__set_progress(
                    92,
                    self.progress_wf.get_enum(self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED))
                self.is_cancel_ongoing = True
                raise lcm_wf_utility.CancelWorkflowException("Operation cancelled")

    def timeout(self):
        """ Callback to be executed when fallback timer expires """
        LOG.debug('Activate workflow execution timer exceeded the fallback timeout: %s seconds.'
                  % (self.upgrade_job.resource.fallback_timeout))
        if self.upgrade_job == None:
            LOG.warning("A fallback timeout occured %s, but the workflow is already finished." %
                        (self.fallback_timer))
            return
        self.fallback_timeout_flag = True
        self.put((self.function(),
                  ({'fallback_timeout': self.upgrade_job.resource.fallback_timeout}, )))

    def __is_fallback_timeout(self):
        """ Check if fallback timer expired, and act on it """
        if self.fallback_timeout_flag:
            LOG.debug("Operation %s is aborted since fallback timer expired "
                      % (self.upgrade_job.resource.operation))
            self.upgrade_job.set_progress(
                10,
                self.progress_wf.get_enum(self.progress_wf.FALLBACK_TIMER_EXPIRED))
            raise lcm_wf_utility.WorkflowTmoException("Fallback timer expired")

    def lcop_rsp_timeout(self):
        """ Callback to be executed when lcop_response timer expires """
        LOG.warning('Maximum VNFM lcop response wait time, %s seconds, exceeded.'
                  % (self.lcop_rsp_timeout_value))
        if self.upgrade_job == None:
            LOG.warning("A lcop response timeout occured %s, but the workflow is already finished." %
                        (self.lcop_rsp_timer))
            return
        self.lcop_rsp_timeout_flag = True

    def __instantiate_vnf(self, instance_name,  description, ext_virtual_links, storages):
        """ Instantiates a VNF """
        LOG.debug("__instantiate_vnf(self, instance_name,  description, ext_virtual_links, storages)"
                  " - instance_name: %s,  description: %s, ext_virtual_links: %s, storages: %s"
                  % (instance_name,  description, ext_virtual_links, storages))

        # Create vnf id
        self.upgrade_job.to_vnf_id = self.vnfm_comm_handle.create_vnf_id(
            self.upgrade_job.get_vnfd_id_in_package(), instance_name, description)
        self.upgrade_job.resource.to_vnf_id = self.upgrade_job.to_vnf_id
        self.__set_resource()

        # Create vnf instance
        http_payload = self.__add_storages(storages)
        http_payload = self.__add_ext_virtual_links(ext_virtual_links, http_payload)
        instantiate_rsp_data = self.vnfm_comm_handle.instantiate_vnf(
            self.upgrade_job.to_vnf_id, http_payload)
        self.to_vnf_instantiated = True
        self.__set_progress(
            45,
            self.progress_wf.get_enum(self.progress_wf.WAITING_FOR_CS_READY))
        self.__wait_for_lc_op_ok(instantiate_rsp_data['vnfLcOpId'],
                                 'After requested instantiation of VNF ID: %s'
                                 % (self.upgrade_job.to_vnf_id),
                                 True,
                                 self.INSTANTIATE_VNF)
        self.__is_cancel_requested()
        self.__is_fallback_timeout()
        self.__check_to_vnf_instantiated(self.upgrade_job.to_vnf_id)
        return self.upgrade_job.to_vnf_id

    def __add_log_storage_from_to_vnf(self, payload):
        """ Adds the log storage from the To-VNF to the From-VNF """
        LOG.debug("__add_log_storage_from_to_vnf(self, payload) - payload: %s"
                  % (payload))
        to_vnf_info = self.vnfm_comm_handle.get_vnf_info(
            self.upgrade_job.get_to_vnf_id(),
            [vnfm_api_comm.VnfmComm.STORAGE])
        to_vnf_log_storage = to_vnf_info.get_log_storage()
        return self.__add_storage_heal(to_vnf_log_storage, payload)

    def __add_storages(self, storages):
        """ Adds storages to http payload at instantiate of VNF """
        LOG.debug("__add_storages(self, storages) - storages: %s" % (storages))
        payload_dict = dict()
        payload_dict["additionalParams"] = []
        for storage in storages:
            payload_dict["additionalParams"].append(
                {"type": "string",
                 "name": storage.get_volume_name(),
                 "value": storage.resource_id})

        LOG.debug("__add_storages(...) - RETURNS - payload_dict: %s" % (payload_dict))
        return payload_dict

    def __add_storage_heal(self, storage, payload_dict):
        """ Adds storage to http payload at instantiate of VNF """
        LOG.debug("__add_storage_heal(self, storage, payload_dict) - storage: %s, payload_dict: %s"
                  % (storage, payload_dict))
        payload_dict["additionalParams"] = []
        payload_dict["additionalParams"].append({"volumes": storage.name})
        payload_dict["additionalParams"].append({storage.name: storage.resource_id})

        LOG.debug("__add_storage_heal(...) - RETURNS - payload_dict: %s" % (payload_dict))
        return payload_dict

    def __read_backup_file_to_http_data(self, vnf_backup_dir_path, backup_file_name):
        """ Read backup file and save it in returned dict """
        LOG.debug("__read_backup_file_to_http_data(self, vnf_backup_dir_path, backup_file_name)"
                  " - vnf_backup_dir_path: %s, backup_file_name: %s"
                  % (vnf_backup_dir_path, backup_file_name))
        payload_dict = dict()
        payload_dict["additionalParams"] = []
        file_to_open = vnf_backup_dir_path + backup_file_name
        LOG.debug("__read_backup_file_to_http_data(...) - "
                  "file_to_open: %s" % (file_to_open))
        try:
            with open(file_to_open, 'r') as file_handle:
                payload_dict["additionalParams"].append(
                    {"type": "file",
                     "name": "%s" % (Activate.BACKUP_FILE_NAME_AT_STARTUP),
                     "value": base64.encodestring(file_handle.read())})
        except Exception as exception:
            LOG.error("__read_backup_file_to_http_data(self, backup_file_name)"
                      " - backup_file_name: %s. Exception caught when trying "
                      "to encode backup file to ascii representation for sending"
                      " to VNFM. File to open: %s. Exception: %s"
                      % (backup_file_name, file_to_open, exception))
            LOG.exception(exception)
            raise lcm_wf_utility.WorkflowException(
                "Exception caught when trying to encode backup file to ascii"
                " representation for sending to VNFM. File to open: %s. "
                "Exception: %s" % (file_to_open, exception))
        return payload_dict

    def __add_ext_virtual_links(self, ext_virtual_links, http_payload):
        """ Adds the external virtual links to the payload """
        LOG.debug("__add_ext_virtual_links(self, ext_virtual_links, http_payload)"
                  " - ext_virtual_links: %s, http_payload: ***" % (ext_virtual_links))
        ext_virtual_links_payload = list()
        for ext_virtual_link in ext_virtual_links:
            ext_virtual_link_http = {'extVirtualLinkId': ext_virtual_link.ext_virtual_link_id,
                                     'extCps': self.__extract_cpd_id(ext_virtual_link.ext_cps)}
            ext_virtual_links_payload.append(ext_virtual_link_http)
        if len(ext_virtual_links_payload) > 0:
            http_payload['extVirtualLinks'] = ext_virtual_links_payload
        return http_payload

    def __extract_cpd_id(self, ext_virtual_link_ext_cps):
        """ Extract the key value for cpdId from input argument """
        LOG.debug("__extract_cpd_id(self, ext_virtual_link_ext_cps) - ext_virtual_link_ext_cps: %s"
                  % (ext_virtual_link_ext_cps))
        ext_virtual_links = list()
        for ext_virt_link_data in ext_virtual_link_ext_cps:
            ext_virtual_links.append({'cpdId': ext_virt_link_data['cpdId']})

        LOG.debug("__extract_cpd_id(self, ext_virtual_link_ext_cps) - RETURNS: %s"
                  % (ext_virtual_links))
        return ext_virtual_links

    def __operate_vnf(self, vnf_id, state, which_vnf, ignore):
        """ Operation state is changed i.e. started or stopped """
        LOG.debug("__operate_vnf(self, vnf_id, state, which_vnf, ignore) - vnf_id: %s, state: %s, "
                  "which_vnf: %s, ignore: %s" % (vnf_id, state, which_vnf, ignore))
        try:
            if vnf_id != None:
                LOG.info("Requests for operate on state for %s-VNF" % (which_vnf))
                if self.vnfm_comm_handle.is_supported(lcm_util.EnumVersion.VER_3_0):
                    operate_state_rsp_data = self.vnfm_comm_handle.operate_state(
                        vnf_id, state)
                    self.__wait_for_lc_op_ok(operate_state_rsp_data['vnfLcOpId'],
                                             'After requested change of operate state of VNF with ID: %s'
                                             % (vnf_id),
                                             True,
                                             self.OPERATE_VNF)
                else:
                    self.vnfm_comm_handle.change_state(vnf_id, state)
                    LOG.info("Waits for 30 seconds to be sure that the %s-VNF has been operated."
                             " This is due to that the LCM Framework do not support required"
                             " version. Required version: %s, Current LCM Framework version: %s"
                             % (which_vnf, lcm_util.EnumVersion.VER_3_0,
                                self.vnfm_comm_handle.get_current_version()))
                    time.sleep(30)

        except (lcm_util.LcmException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.info("__operate_vnf(self, vnf_id, state, which_vnf) - vnf_id: %s, state: %s, "
                     "which_vnf: %s. LCM Exception caught when trying operate the state of given "
                     "VNF (ignored)" % (vnf_id, state, which_vnf))
            if not ignore:
                raise exception

        except Exception as exception:
            LOG.info("__terminate_vnf(self, vnf_id, which_vnf) - vnf_id: %s. Exception "
                     "caught when trying to terminate given VNF (ignored)"
                     % (vnf_id))
            LOG.exception(exception)
            if not ignore:
                raise lcm_util.LcmException(exception)

    def __terminate_vnf(self, vnf_id, which_vnf):
        """ Terminates given VNF and removes the vnf id """
        LOG.debug("__terminate_vnf(self, vnf_id, which_vnf) - vnf_id: %s, which_vnf: %s"
                  % (vnf_id, which_vnf))
        try:
            if vnf_id != None:
                terminate_rsp_data = self.vnfm_comm_handle.terminate_vnf(vnf_id)
                self.__wait_for_lc_op_ok(terminate_rsp_data['vnfLcOpId'],
                                         'After requested termination of VNF with ID: %s'
                                         % (vnf_id),
                                         False,
                                         self.TERMINATE_VNF)

        except (lcm_util.LcmException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.info("__terminate_vnf(self, vnf_id, which_vnf) - vnf_id: %s. LCM Exception "
                     "caught when trying to terminate given VNF (ignored)"
                     % (vnf_id))
            self.add_infos.append("WARNING: Termination of %sVNF with id '%s' failed. Please, "
                                  " ensure that this VNF is manually terminated"
                                  " to avoid resource leakage. Reason reported from VNFM: %s" %
                                  (which_vnf, vnf_id, exception))

        except Exception as exception:
            LOG.info("__terminate_vnf(self, vnf_id, which_vnf) - vnf_id: %s. Exception "
                     "caught when trying to terminate given VNF (ignored)"
                     % (vnf_id))
            LOG.exception(exception)
            self.add_infos.append("WARNING: Termination of %sVNF with id '%s' failed. Please, "
                                  " ensure that this VNF is manually terminated"
                                  " to avoid resource leakage. Reason reported from VNFM: %s"
                                  % (which_vnf, vnf_id, exception))

    def __remove_vnf_id(self, vnf_id, which_vnf):
        """ Removes VNF id """
        LOG.debug("__remove_vnf_id(self, vnf_id, which_vnf) - vnf_id: %s, which_vnf: %s"
                  % (vnf_id, which_vnf))
        try:
            if vnf_id != None:
                self.vnfm_comm_handle.remove_vnf_id(vnf_id)
        except (lcm_util.LcmException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.info("__remove_vnf_id(self, vnf_id) - vnf_id: %s. LCM Exception "
                     "caught when trying to remove given VNF id (ignored)" % (vnf_id))
            self.add_infos.append("WARNING: Removal of %sVNF id '%s' failed. Please, ensure that"
                                  " this VNF id is manually removed to avoid resource leakage"
                                  ". Reason reported from VNFM: %s"
                                  % (which_vnf, vnf_id, exception))

        except Exception as exception:
            LOG.warning("__remove_vnf_id(self, vnf_id) - vnf_id: %s. Exception "
                        "caught when trying to remove given VNF id (ignored)" % (vnf_id))
            LOG.exception(exception)
            self.add_infos.append("WARNING: Removal of %sVNF id '%s' failed. Please, ensure that"
                                  " this VNF id is manually removed to avoid resource leakage"
                                  ". Reason reported from VNFM: %s"
                                  % (which_vnf, vnf_id, exception))

    def __delete_vnf_backup(self, backup_name):
        """ Removes any existing backup with given name """
        LOG.debug("__delete_vnf_backup(self, backup_name) - backup_name: %s"
                  % (backup_name))
        try:
            self.vnf_comm_handle_from.delete_backup(self.backup_name)
        except (lcm_util.LcmException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.info("__delete_vnf_backup(self, backup_name) - backup_name: %s."
                     " LCM Exception caught when requesting for deletion of given "
                     "VNF backup (ignored). Exception reason: %s" % (backup_name,
                                                                     exception))

        except Exception as exception:
            LOG.warning("__delete_vnf_backup(self, backup_name) - backup_name: %s."
                        " Exception caught when requesting for deletion of given "
                        "VNF backup (ignored)" % (backup_name))

    def __check_to_vnf_instantiated(self, vnf_id):
        """ Ensures that TO VNF has been instantiated """
        LOG.debug("__check_to_vnf_instantiated(self, vnf_id) - "
                  "vnf_id: %s" % (vnf_id))
        self.__wait_for_vnf_to_be_active(vnf_id)

    def __wait_for_lc_op_ok(self, vnf_lc_op_id, lcop_request_info_str, is_interrupt_allowed,
                            action_type):
        """ Waits until lifecycle operation reports success """
        LOG.debug("__wait_for_lc_op_ok(self, "
                  "vnf_lc_op_id, lcop_request_info_str, is_interrupt_allowed, action_type) "
                  "- vnf_lc_op_id: %s, lcop_request_info_str: %s, "
                  "is_interrupt_allowed:%s, action_type: %s" % (
                      vnf_lc_op_id, lcop_request_info_str, is_interrupt_allowed, action_type))
        while (True):
            self.__start_lcop_rsp_timer_if_needed(vnf_lc_op_id)
            self.__check_lcop_rsp_timer(vnf_lc_op_id, lcop_request_info_str)
            self.__react_to_any_cancel_request(is_interrupt_allowed)
            LOG.debug("__wait_for_lc_op_ok(...) - "
                      "Waits for 2 second before continuing (%s) - vnf_lc_op_id: %s"
                      % (lcop_request_info_str, vnf_lc_op_id))
            time.sleep(2)
            vnf_lc_op_rsp = self.vnfm_comm_handle.get_vnf_lc_op(vnf_lc_op_id)
            if  vnf_lc_op_rsp['status'] == 'success':
                self.__cancel_lcop_rsp_timer(vnf_lc_op_id)
                break
            elif vnf_lc_op_rsp['status'] == 'failure':
                LOG.error("__wait_for_lc_op_ok(self, vnf_lc_op_id, "
                          "lcop_request_info_str, is_interrupt_allowed, action_type)"
                          " - While waiting for the lifecycle "
                          "operation to finish it reported "
                          "failure - vnf_lc_op_id: %s, Reason: %s"
                          "Response received from VNFM: %s"
                          % (vnf_lc_op_id, lcop_request_info_str, vnf_lc_op_rsp))
                self.__cancel_lcop_rsp_timer(vnf_lc_op_id)
                raise lcm_wf_utility.WorkflowException('While waiting for the lifecycle '
                                                       'operation to finish it reported '
                                                       'failure - vnf_lc_op_id: %s, Reason: %s'
                                                       'Response received from VNFM: %s'
                                                       % (vnf_lc_op_id, lcop_request_info_str,
                                                          vnf_lc_op_rsp))
            else:
                LOG.debug("__wait_for_lc_op_ok(self, "
                          "vnf_lc_op_id) - vnf_lc_op_rsp: %s" % (vnf_lc_op_rsp))

        return

    def __cancel_lcop_rsp_timer(self, vnf_lc_op_id):
        """ Cancel LCOP response timer if needed """
        if self.lcop_rsp_timer != None:
            LOG.debug("LCOP timer cancelled, as VNFM LCOP responded. vnf_lc_op_id: %s"
                      % (vnf_lc_op_id))
            self.lcop_rsp_timer.cancel()
            self.lcop_rsp_timer = None
            self.lcop_rsp_timeout_flag = False

    def __start_lcop_rsp_timer_if_needed(self, vnf_lc_op_id):
        """
        Start LCOP response timer if fallback timer has expired
        while waiting for LCOP response
        """
        if self.fallback_timeout_flag and self.lcop_rsp_timer == None:
            if (self.upgrade_job.get_progress_detail() !=
                self.progress_wf.get_enum(self.progress_wf.FALLBACK_TIMER_EXPIRED)):
                self.upgrade_job.set_progress(
                    90,
                    self.progress_wf.get_enum(self.progress_wf.FALLBACK_TIMER_EXPIRED))
                self.lcop_rsp_timer = threading.Timer(self.lcop_rsp_timeout_value,
                                                      self.lcop_rsp_timeout)
                self.lcop_rsp_timer.start()
                LOG.debug("LCOP timer started (%s). Operation Fallback timer has expired "
                          "and we are waiting/polling for VNFM LCOP result. "
                          "LCOP timeout value:  %s, vnf_lc_op_id: %s" %
                          (self.lcop_rsp_timer, self.lcop_rsp_timeout_value, vnf_lc_op_id))

    def __check_lcop_rsp_timer(self, vnf_lc_op_id, lcop_request_info_str):
        """ Check and act on an expired LCOP response timer """
        LOG.debug("Checking if LCOP timer expired: %s -- %s" % (
            self.lcop_rsp_timeout_flag, self.lcop_rsp_timer))
        if self.lcop_rsp_timeout_flag:
            LOG.debug("LCOP response timer - timeout occured")
            raise lcm_wf_utility.WorkflowException('While waiting for the lifecycle '
                                                   'operation to finish a timeout occured'
                                                   '-  vnf_lc_op_id: %s, Reason: %s. '
                                                   'Expected lcop result not received within '
                                                   '%s seconds'
                                                   % (vnf_lc_op_id, lcop_request_info_str,
                                                      self.lcop_rsp_timeout_value))

    def __wait_for_vnf_to_be_active(self, vnf_id):
        """ Waits for VNF to become ACTIVE """
        LOG.debug("__wait_for_vnf_to_be_active(self, vnf_id) - "
                  "vnf_id: %s" % (vnf_id))

        while (True):
            self.__is_cancel_requested()
            self.__is_fallback_timeout()
            instance_info = self.vnfm_comm_handle.get_vnf_instance_info(vnf_id)
            LOG.debug("__wait_for_vnf_to_be_active(...) - VNF instance info: %s"
                      % (instance_info))
            if instance_info['instantiationState'] == 'INSTANTIATED':
                if len(instance_info['vnfcResources']) > 0:
                    vnfc_resource = instance_info['vnfcResources'][0]
                    if vnfc_resource['state'] == 'ACTIVE':
                        LOG.debug("__wait_for_vnf_to_be_active(...) - To-VNF has been "
                                  "instantiated successfully and is ACTIVE")
                        break
                    else:
                        LOG.debug("__wait_for_vnf_to_be_active(...) - VNF "
                                  "still not active - state: %s"
                                  % (vnfc_resource['state']))
                        time.sleep(1)
                else:
                    LOG.debug("__wait_for_vnf_to_be_active(...) - vnfc resource "
                              "list is empty - vnfcResources: %s"
                              % (instance_info['vnfcResources']))
            else:
                LOG.error("__wait_for_vnf_to_be_active(...) - The TO VNF has been "
                          "requested to be instantiated but "
                          "it has NOT been that for some reason. to_vnf_id: %s, "
                          "vnfm_instance_info: %s, job id: %s"
                          % (vnf_id, instance_info, self.upgrade_job.get_job_id()))
                raise lcm_wf_utility.WorkflowException(
                    "The TO VNF has  been requested to be instantiated but "
                    "it has NOT been that for some reason. to_vnf_id: %s, "
                    "vnfm_instance_info: %s, job id: %s" %
                    (vnf_id, instance_info, self.upgrade_job.get_job_id()))

    def __get_verify_workflow_path(self):
        """ Returns the verify workflow """
        LOG.debug("__get_verify_workflow_path(self)")
        for workflow in self.upgrade_job.get_workflows():
            if ((workflow.get_function_area() ==
                 lcm_upgrade_job.get_workflow_enum().FUNCTION_AREA) and
                (workflow.get_operation() ==
                 lcm_upgrade_job.get_workflow_enum().VERIFY_OPERATION)):
                return workflow.get_local_path()

        LOG.error("__get_verify_workflow_path(self) - Failed to lookup the"
                  " verify workflow. This is required to be able to execute"
                  " the verify checks that is needed to secure a successfull"
                  " upgrade. Existing workflows: %s"
                  % (self.upgrade_job.get_workflows()))
        raise lcm_wf_utility.WorkflowException(
            "__get_verify_workflow_path(self) - Failed to lookup the"
            " verify workflow. This is required to be able to execute"
            " the verify checks that is needed to secure a successfull"
            " upgrade. Existing workflows: %s"
            % (self.upgrade_job.get_workflows()))

    def __execute_auto_verify(self, verify_wf_path, *args, **kwargs):
        """
        Executes the verification checks that exists to ensure that
        the upgrade will be successfully executed
        """
        LOG.debug("__execute_auto_verify(self, verify_wf_path, *args, **kwargs)"
                  " - verify_wf_path: %s, args: %s, kwargs: %s"
                  % (verify_wf_path, args, kwargs))

        wf_path = workflow_handler.get_workflow_handler().import_wf(
            self.upgrade_job,
            lcm_upgrade_job.get_workflow_enum().FUNCTION_AREA,
            lcm_upgrade_job.get_workflow_enum().VERIFY_OPERATION)
        wf_module = lcm_python_manager.get_workflow_python_manager().import_source(
            wf_path,
            self.upgrade_job,
            workflow_handler,
            lcm_upgrade_job.get_workflow_enum().FUNCTION_AREA)
        function = getattr(wf_module, 'get_verify_wf')
        verifier = function(*args, **kwargs)

        verify_check_results = verifier.execute_verify_checks()
        if (verifier.define_result(verify_check_results) == wf_module.VerifyCheckResult.FAILURE):
            LOG.warning("__execute_auto_verify(self, verify_wf_path, *args, **kwargs)"
                        " - Auto verification prior execution of activate detected at"
                        " least one stopping problem. The verify check results are: %s"
                        % (verify_check_results))
            raise lcm_wf_utility.VerifyWorkflowException(
                "Auto verification prior execution of activate detected at"
                " least one stopping problem. The verify check results are: ",
                verifier.build_add_info(verify_check_results))
        else:
            LOG.info("__execute_auto_verify(self, verify_wf_path, *args, **kwargs)"
                     " - Auto verification prior execution of activate did not detect"
                     " anything that prevents the activation. The verify check results"
                     " are: %s" % (verify_check_results))
            return verifier.build_add_info(verify_check_results)

    def __esi_handling(self):
        """ If needed, the ESI shall be copied from To-VNF to From-VNF """
        LOG.debug("__esi_handling(self) - to_vnf_instantiated: %s, "
                  "from_vnf_shutdown: %s"
                  % (self.to_vnf_instantiated, self.from_vnf_shutdown))
        if self.to_vnf_instantiated and not self.from_vnf_shutdown:
            try:
                LOG.info("Requesting To-VNF to export ESI to tmp directory")
                export_result = self.__export_esi(
                    self.__get_vnf_comm_to(),
                    self.upgrade_job.get_to_vnf_id(),
                    self.ESI_FILE_NAME_ROLLBACK,
                    "Requesting To-VNF to export ESI to LCM tmp directory")
                LOG.info("Requesting From-VNF to import ESI (origin from To-VNF)")
                self.__import_esi(self.vnf_comm_handle_from,
                                  self.upgrade_job.get_vnf_id(),
                                  export_result,
                                  "Requesting From-VNF to import ESI (origin from To-VNF)")
            except Exception as exception:
                LOG.info("Failed to copy the ESI logs from To-VNF (%s) to From-VNF"
                         " (%s) (ignored) due to : %s"
                         % (self.upgrade_job.get_to_vnf_id(),
                            self.upgrade_job.get_vnf_id(),
                            exception))

    def __export_esi(self, vnf_comm_handle, vnf_id, esi_file_name, log_info_text):
        """ Used to export ESI from given VNF """
        LOG.debug("__export_esi(self, vnf_comm_handle, vnf_id, esi_file_name, log_info_text) "
                  "- vnf_comm_handle: %s, vnf_id: %s, esi_file_name: %s"
                  % (vnf_comm_handle, vnf_id, esi_file_name))

        if vnf_id == None:
            LOG.info("%s - %s" % (
                log_info_text, "Could not be performed due to vnf id is set to None."))
            return False

        if not lcm_wf_utility.get_lcm_wfm_proxy().is_supported(lcm_util.EnumVersion.VER_2_0):
            LOG.info("%s - %s - LCM FW version: %s, Required version: %s" % (
                log_info_text,
                "Could not be performed due to LCM Framework do not support this.",
                lcm_wf_utility.get_lcm_wfm_proxy().get_current_version(),
                lcm_util.EnumVersion.VER_2_0))
            return False

        try:
            LOG.info(log_info_text)
            self.esi_file_path ='%s%s/%s' % (self.lcm_config.get_lcm_workflow_file_path(),
                                             self.upgrade_job.get_job_id(),
                                             esi_file_name)
            vnf_comm_handle.export_esi(self.esi_file_path)
            return True

        except Exception as exception:
            LOG.info("__export_esi(self, vnf_comm_handle). Failed to export ESI logs "
                     "from VNF %s due to %s (ignored)." % (vnf_id, exception))
            return False

    def __import_esi(self, vnf_comm_handle, vnf_id, export_esi_result, log_info_text):
        """ Used to import ESI to given VNF """
        LOG.debug("__import_esi(self, vnf_comm_handle, vnf_id, export_esi_result,"
                  " log_info_text) - vnf_comm_handle:"
                  " %s, vnf_id: %s, export_esi_result: %s"
                  % (vnf_comm_handle, vnf_id, export_esi_result))
        is_esi_supported = lcm_wf_utility.get_lcm_wfm_proxy().is_supported(
            lcm_util.EnumVersion.VER_2_0)
        if export_esi_result and is_esi_supported:
            try:
                LOG.info(log_info_text)
                self.working_state = EnumWorkingState.WAIT_FOR_FETCH_ESI_RESULT
                result = vnf_comm_handle.fetch_esi()
                if result['result'] != 'SUCCESS':
                    LOG.warning("Failure reported from VNF when requesting it to fetch ESI (ignored)."
                                " Result: %s, VNF: %s" % (result, vnf_id))
                    self.working_state = EnumWorkingState.IDLE
                return True

            except Exception as exception:
                LOG.info("__import_esi(self, vnf_comm_handle). Failed to import ESI logs "
                         "from VNF %s due to %s (ignored)." % (vnf_id, exception))
                return False
        else:
            info_text = ("%s - %s - LCM FW supports ESI: %s, LCM FW version: %s, "
                         "Required version: %s, Export ESI result: %s"
                         % (log_info_text, "Could not be performed due to LCM Framework"
                            " do not support this or the export of ESI failed.",
                            is_esi_supported,
                            lcm_wf_utility.get_lcm_wfm_proxy().get_current_version(),
                            lcm_util.EnumVersion.VER_2_0,
                            export_esi_result))
            LOG.info(info_text)
            return False

    def __heal_from_vnf_if_needed(self, reason, is_stop_traffic_requested, to_vnf_id):
        """ Used to request for heal of From-VNF, if needed """
        LOG.debug("__heal_from_vnf_if_needed(self, reason, is_stop_traffic_requested,"
                  " to_vnf_id) - reason: %s, is_stop_traffic_requested: %s, to_vnf_id: %s"
                  % (reason, is_stop_traffic_requested, to_vnf_id))
        if is_stop_traffic_requested:
            LOG.info("The From-VNF (%s) is requested to be healed due to"
                     " the ongoing upgrade job is aborted after stop of "
                     "traffic has been issued (job id: %s)" %
                     (self.upgrade_job.get_vnf_id(), self.upgrade_job.get_job_id()))
            self.__heal_from_vnf(reason, to_vnf_id)
        else:
            LOG.debug("__heal_from_vnf_if_needed(...) - No need to heal the From-VNF, since we did not yet "
                      "request the From-VNF to stop traffic")

    def __heal_from_vnf(self, reason, to_vnf_id):
        """ Used to request for heal of From-VNF """
        LOG.debug("__heal_from_vnf(self, reason, to_vnf_id) - reason: %s, to_vnf_id: %s"
                  % (reason, to_vnf_id))

        # Get the log disk volume id from To-VNF and add it to heal operation of From-VNF
        http_payload = dict()
        if self.from_vnf_shutdown:
            try:
                LOG.info("The From-VNF has been shutdowned. The log volume from To-VNF needs to"
                         " be copied back to From-VNF. This will be done using mechanism in "
                         "VNFM (heal request)")
                if to_vnf_id != None:
                    LOG.debug("Execution mode of To-VNF (%s) is to be stoped " % (to_vnf_id))
                    self.upgrade_job.set_progress(
                        70,
                        self.progress_wf.get_enum(self.progress_wf.STOP_EXECUTION_OF_TO_STATE_VNF))
                    self.__operate_vnf(to_vnf_id, 'STOPPED', 'To', True)
                    http_payload = self.__add_log_storage_from_to_vnf(http_payload)
                    http_payload['cause'] = 'rollback'
            except Exception as exception:
                LOG.warning("Failed to either stop the execution of To-VNF or collect storage information. "
                            "The From-VNF will be healed anyhow but without copying log storage data from "
                            "To-VNF to From-VNF. Reason for exception: %s" % (exception))
                LOG.exception(exception)
                http_payload['cause'] = reason
        else:
            http_payload['cause'] = reason
        try:
            heal_rsp_data = self.vnfm_comm_handle.heal_vnf_2(self.upgrade_job.get_vnf_id(),
                                                             http_payload)
            LOG.debug("__heal_from_vnf(...) - Heal requested and lcop id is %s. "
                      "Needs to wait for result of the heal" % (heal_rsp_data['vnfLcOpId']))
            self.__wait_for_lc_op_ok(heal_rsp_data['vnfLcOpId'],
                                     'After requested heal of From-VNF with ID: %s'
                                     % (self.upgrade_job.get_vnf_id()),
                                     False,
                                     self.HEAL_VNF)

        except (lcm_wf_utility.WorkflowException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.warning("__heal_from_vnf(self, reason, to_vnf_id). The requested heal "
                        "reported failure for From-VNF (%s). I.e. it might end up in "
                        "that this VNF not will be fully functional. This problem is "
                        "ignored since nothing more can be done. Upgrade job: %s,reason:"
                        " %s, to_vnf_id: %s"
                        % (self.upgrade_job.get_vnf_id(), self.upgrade_job.get_job_id(),
                           reason, to_vnf_id))
            LOG.exception(exception)
            self.add_infos.append("WARNING: Heal of From-VNF with id '%s' failed. I.e. "
                                  "it might end up in that this VNF not will be fully "
                                  "functional. This may require manual interaction from"
                                  " end user e.g. manual heal of From-VNF. The failure "
                                  "reason reported from VNFM: %s" %
                                  (self.upgrade_job.get_vnf_id(), exception))

    def __cancel_from_vnf(self):
        """
        Used to request for cancel of ongoing operation e.g. create backup,
        export backup
        """
        LOG.debug("__cancel_from_vnf(self)")
        if (self.working_state == EnumWorkingState.WAIT_FOR_CREATE_BACKUP_RESPONSE):
            try:
                self.vnf_comm_handle_from.cancel(self.action_id)

            except vnfm_api_comm.VnfmCommException as exception:
                LOG.error("__cancel_from_vnf(self). Failed to cancel ongoing operation "
                          "in From-VNF (%s). This problem is ignored since nothing more"
                          " can be done. Upgrade job: %s"
                          % (self.upgrade_job.get_vnf_id(), self.upgrade_job.get_job_id()))
                LOG.exception(exception)
        else:
            LOG.debug("__cancel_from_vnf(...) - No need to cancel anything in From-VNF")

    def __subscribe_for_lcop(self, vnf_id, operations):
        """ Subscribe for lcop from given vnf """
        LOG.debug("__subscribe_for_lcop(self, vnf_id, operations) - "
                  "VNF id: %s, operations: %s" % (vnf_id, operations))
        endpoint = "http://%s:%s%s/v1/vnf/notifications" % (
            host_in_url('localhost'),
            self.lcm_config.get_lcm_mani_http_port(),
            lcm_util.get_lcm_base_uri().LCM_LCOP_API_BASE)
        self.upgrade_job.resource.subscription_url_id = self.vnfm_comm_handle.subscribe_lcop(
            endpoint, vnf_id, operations)
        self.__set_resource()

    def __unsubscribe_for_lcop(self, subscription_url_id):
        """ Subscribe for lcop from given vnf """
        LOG.debug("__unsubscribe_for_lcop(self, subscription_url_id) - "
                  "subscription_url_id: %s" % (subscription_url_id))
        try:
            if subscription_url_id != None:
                self.vnfm_comm_handle.unsubscribe_lcop(subscription_url_id)
                self.subscription_url_id = None
            else:
                LOG.debug("__unsubscribe_for_lcop(...) - No need to unsubscribe")

        except vnfm_api_comm.VnfmCommException as exception:
            LOG.info("__unsubscribe_for_lcop(self, subscription_url_id). Failed to unsubscribe "
                     "for lccn for VNF (%s). This problem is ignored since nothing more"
                     " can be done. Subscription url id: %s"
                     % (self.upgrade_job.get_to_vnf_id(), subscription_url_id))

        except Exception as exception:
            LOG.warning("__unsubscribe_for_lcop(self, subscription_url_id). Failed to unsubscribe "
                        "for lccn for VNF (%s). This problem is ignored since nothing more"
                        " can be done. Subscription url id: %s"
                        % (self.upgrade_job.get_to_vnf_id(), subscription_url_id))
            LOG.exception(exception)

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





    def __set_job(self, dump = True):
        """ Saves job data to DB """
        LOG.debug("__set_job(self, dump = True) - dump: %s" % (dump))
        try:
            lcm_db_handler.get_lcm_db_handler().set_job(self.upgrade_job, True)
        except lcm_util.LcmException as exception:
            LOG.warning("__set_job(self, dump = True) - dump: %s, "
                        "Failed to save data in to LCM DB which "
                        "might cause future problem for this upgrade job. "
                        "Upgrade job data: %s"
                        % (dump, self.upgrade_job))
            LOG.exception(exception)

    def __set_resource(self, dump = True):
        """ Saves allocated resource in DB """
        LOG.debug("__set_resource(self, dump = True) - dump: %s" % (dump))
        try:
            lcm_db_handler.get_lcm_db_handler().set_resource(
                self.upgrade_job.get_job_id(), self.upgrade_job.resource)
        except lcm_util.LcmException as exception:
            LOG.warning("__set_resource(self). Failed to save allocated resources"
                        " in LCM DB which might cause future problem for this "
                        "upgrade job. Upgrade job data: %s" % (self.upgrade_job))
            LOG.exception(exception)

    def __finish(self, state, callback = True):
        """ Ensure the workflow marks its execution as finished """
        LOG.debug("__finish(self, state, callback = True) - state: %s, callback: %s"
                  % (state, callback))
        self.upgrade_job.shadow_state = state
        self.upgrade_job.set_shadow(True)
        self.__set_state(state)
        self.__set_job()
        if callback:
            self.callback_when_finished(self.job_id, self)
        self.upgrade_job.set_shadow(False)

    def __check_vnf_is_operational(self, vnf_id):
        """ Checks that the VNF is operational """
        LOG.debug("__check_vnf_is_operational(self, vnf_id) - vnf_id: %s" % (vnf_id))

        vnf_info = self.__get_vnf_info(vnf_id)
        if vnf_info == None:
            LOG.debug("__check_vnf_is_operational(...) - No operational state "
                      "available since VNF (%s) do not exist" % (vnf_info['instanceId']))
            return False
        elif (vnf_info['operationalState'] == 'DISABLED'):
            LOG.debug("__check_vnf_is_operational(...) - The operational state is %s for"
                      "VNF %s" % (vnf_info['operationalState'], vnf_info['instanceId']))
            return False
        else:
            return True

    def __rollback_at_restore(self, add_info = list()):
        """ Roll back ongoing upgrade at restore of job (R-VNFM has been restarted) """
        LOG.debug("__rollback_at_restore()")
        if self.vnf_comm_handle_from == None:
            self.vnf_comm_handle_from = vnf_api_comm.get_vnf_comm(
                self.upgrade_job.get_vnf_id(),
                self.vnf_port,
                self.vnfm_url)

        self.__unsubscribe_for_lcop(self.upgrade_job.resource.subscription_url_id)
        self.__heal_from_vnf_if_needed("A restart of R-VNFM was done while an activation was "
                                       "in progress. The activation is rolled back and hence "
                                       "From-VNF needs to be healed since traffic have been "
                                       "stopped. Job id: %s, From-VNF: %s"
                                       % (self.upgrade_job.get_job_id(),
                                          self.upgrade_job.get_vnf_id()),
                                       self.upgrade_job.resource.is_traffic_stoped,
                                       self.upgrade_job.resource.to_vnf_id)
        self.__terminate_vnf(self.upgrade_job.resource.to_vnf_id, "To-")
        self.__remove_vnf_id(self.upgrade_job.resource.to_vnf_id, "To-")
        self.__start_configuration(self.upgrade_job.resource.is_configuration_stoped,
                                   self.upgrade_job.resource.is_traffic_stoped,
                                   self.upgrade_job.resource.to_vnf_id)
        add_info.append("A restart of R-VNFM has occured while this operation was "
                        "in progress i.e. upgrade is rolled back. ")
        add_info.extend(self.add_infos)
        self.__add_operation_result(
            self.upgrade_job.resource.operation,
            self.upgrade_job.resource.requested_time,
            lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
            self.upgrade_job.resource.fallback_timeout,
            self.result_wf.get_enum(self.result_wf.FAILURE),
            self.add_info_trans.get_add_info(add_info))
        self.__set_progress(
            0,
            self.progress_wf.get_enum(self.progress_wf.IDLE))
        self.__finish(self.upgrade_job.resource.previous_state, False)
        self.upgrade_job = None
        self.is_cancel_ongoing = False

    def __check_to_vnf_at_restore(self):
        """
        Checks state of To-VNF at restore and if:
        - It it is operational the confirm operation that was in progress at restart will be
        set to failed and the upgrade job state WAITING_FOR_CONFIM is kept.
        - It is Not operational a rollback is initiated.
        """
        LOG.debug("__check_to_vnf_at_restore(self)")

        to_vnf_info = self.__get_vnf_info(self.upgrade_job.to_vnf_id)
        if (to_vnf_info != None and
            to_vnf_info['operationalState'] == 'ENABLED'):
            LOG.debug("__check_to_vnf_at_restore(self) - A restart of R-VNFM occured while "
                      "this operation was in progress i.e. it is aborted")
            add_info = ["A restart of R-VNFM occured while this operation was "
                        "in progress i.e. it is aborted"]
            add_info.extend(self.add_infos)
            self.__add_operation_result(
                self.upgrade_job.resource.operation,
                self.upgrade_job.resource.requested_time,
                lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                self.upgrade_job.resource.fallback_timeout,
                self.result_wf.get_enum(self.result_wf.FAILURE),
                self.add_info_trans.get_add_info(add_info))
            self.upgrade_job.resource.cleanup_wfc()
            self.__set_job()
            #Restart fallback timer
            if self.upgrade_job.resource.fallback_timeout != None:
                self.fallback_timer = threading.Timer(self.upgrade_job.resource.fallback_timeout,
                                                      self.timeout)
                self.fallback_timer.start()
                LOG.debug("Activate Fallback timer started (%s) at restore. Timeout:  %s" %
                          (self.fallback_timer, self.upgrade_job.resource.fallback_timeout))


        else:
            LOG.debug("__check_to_vnf_at_restore(self) - Either To-VNF do not exist "
                      "or operational state is not as it should. Rollback is requested "
                      "due to this problem. To-VNF info: %s"
                      % (to_vnf_info))
            add_info = [' Reason for the rollback is that the To-VNF is not operational '
                        'or that To-VNF has been terminated']
            self.__rollback_at_restore(add_info)

    def __confirm_to_vnf(self):
        """ Send confirm to To-VNF """
        LOG.debug("__confirm_to_vnf(self)")
        try:
            self.__get_vnf_comm_to().confirm()
        except (lcm_util.LcmException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.info("__confirm_to_vnf(self). VnfmCommException "
                     "caught when trying to confirm To-VNF (ignored).")

        except Exception as exception:
            LOG.warning("__confirm_to_vnf(self). Exception "
                        "caught when trying to confirm To-VNF (ignored)")
            LOG.exception(exception)

    def __remove_upgrade_logs_file(self):
        """ Used to remove downloaded upgrade log file, if any """
        LOG.debug("__remove_upgrade_logs_file(self)")
        try:
            if (self.upgrade_logs_file_path != None and
                os.path.exists(self.upgrade_logs_file_path)):
                LOG.debug("__remove_upgrade_logs_file(self) - Removing file: %s"
                          % (self.upgrade_logs_file_path))
                os.remove(self.upgrade_logs_file_path)

        except Exception as exception:
            LOG.warning("__remove_upgrade_logs_file(self). Exception "
                        "caught when trying to remove the tmp upgrade logs file (ignored)")
            LOG.exception(exception)

    def __check(self, condition, error_code, message):
        """
        If condition is False a LcmException is thrown and a warning is logged
        else nothing is done
        """
        if not condition:
            LOG.warning(message)
            raise lcm_util.LcmException(error_code, message)

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

    def __confirm_rejected_lccn_ongoing(self, add_info):
        """ Confirm operation shall be rejected to to an ongoing lccn operation """
        LOG.debug("__confirm_rejected_lccn_ongoing(self, add_info) - add_info: %s"
                  % (add_info))

        add_info.append("To-VNF ID: %s" % (self.upgrade_job.get_to_vnf_id()))
        add_info.append("Not possible to confirm right now due to operation '%s' has "
                        "been requested on To-VNF %s. In most cases this leads to rollback)." %
                        (self.lccn_ongoing_operation_name, self.upgrade_job.get_to_vnf_id()))
        add_info.extend(self.add_infos)
        self.__add_operation_result(
            lcm_upgrade_job.get_operation_enum().CONFIRM,
            self.upgrade_job.resource.requested_time,
            lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
            self.upgrade_job.resource.fallback_timeout,
            self.result_wf.get_enum(self.result_wf.FAILURE),
            self.add_info_trans.get_add_info(add_info))

    def __get_lcop_tmo(self):
        """
        Get lcop response timeout value fron configuration file if lcm frame work supports it.
        Otherwise hardcoded default value.
        """
        if self.lcm_config.is_supported(lcm_util.EnumVersion.VER_4_0):
            return self.lcm_config.get_vnfm_lcop_wait_time()
        else:
            return 3601

    def __upgrade_clone_complete(self):
        """ Send upgrade clone complete to From-VNF """
        LOG.debug("__upgrade_clone_complete(self)")
        try:
            self.vnf_comm_handle_from.upgrade_clone_complete()

        except (vnf_api_comm.VnfCommException) as exception:
            LOG.debug("__upgrade_clone_complete(self). VnfCommException "
                      "caught when trying to inform From-VNF that clone of disks"
                      " are ready (ignored).")

        except Exception as exception:
            LOG.info("__upgrade_clone_complete(self). Exception "
                     "caught when trying to inform From-VNF that clone of disks"
                     " are ready (ignored).")
            LOG.exception(exception)
