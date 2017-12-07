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
Lifecycle Management Script for restore operation.
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
                 vnfm_api_comm,
                 lcm_db_handler,
                 lcm_upgrade_job,
                 lcm_wfm)

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
from common.http_utils import host_in_url

LOG = logging.getLogger('RESTORE_CINDER')

def main(job_id, operation_data, a_restore_job, lcm_config, callback_when_finished):
    """ Main function for restore to initiate the restore request """
    LOG.debug("main(job_id, operation_data, a_restore_job, lcm_config, "
              "callback_when_finished) - job_id: %s, operation_data: %s, "
              "a_restore_job: %s, lcm_config: %s, callback_when_finished: %s"
              % (job_id, operation_data, a_restore_job, lcm_config,
                 callback_when_finished))
    the_restore = startup(job_id, operation_data, a_restore_job, lcm_config,
                          callback_when_finished)
    the_restore.main(operation_data)
    LOG.info("Restore cinder initiated for JOB_ID: %s" % (job_id))
    return the_restore

def startup(job_id, operation_data, a_restore_job, lcm_config, callback_when_finished):
    """ Startup function for restore to startup WF restore """
    LOG.debug("startup(job_id, operation_data, , lcm_config, "
              "callback_when_finished) - job_id: %s, operation_data: %s, "
              "a_restore_job: %s, lcm_config: %s, callback_when_finished: %s"
              % (job_id, operation_data, a_restore_job, lcm_config,
                 callback_when_finished))
    the_restore = Restore(job_id, a_restore_job, lcm_config, callback_when_finished)
    the_restore.start()
    LOG.info("WF Restore started up and waits for further instructions for "
             "JOB_ID: %s" % (job_id))
    return the_restore


def restore_job(job_id, operation_data, a_restore_job, lcm_config, callback):
    """
    Restore jobs that were in progress at restart of R-VNMF are restored.
    It means that resources may be released or that if a confirm was in
    progress it may be auto-initiated if From-VNF has been terminated.
    """
    LOG.debug("restore_job(job_id, operation_data, a_restore_job, "
              "lcm_config, callback) - job_id: %s, operation_data: %s, "
              "a_restore_job: %s, lcm_config: %s, callback: %s"
              % (job_id, operation_data, a_restore_job, lcm_config, callback))
    return Restore(job_id, a_restore_job, lcm_config, callback).restore_job()


class EnumWorkingState:
    """ Working states within Activate workflow """
    IDLE = "IDLE"
    WAIT_FOR_TO_VNF_UP_AND_RUNNING = "WAIT_FOR_TO_VNF_UP_AND_RUNNING"
    WAIT_FOR_POST_CHECK_RESULT = "WAIT_FOR_POST_CHECK_RESULT"
    WAIT_FOR_FETCH_ESI_RESULT = "WAIT_FOR_FETCH_ESI_RESULT"
    WAIT_FOR_FETCH_UPGRADE_LOG_RESULT = "WAIT_FOR_FETCH_UPGRADE_LOG_RESULT"

class Restore(lcm_wf_utility.GenAsyncQueue):
    """ Restore workflow script """
    CURRENT_VERSION = lcm_util.EnumVersion.VER_1_0

    UPGRADE_LOGS_FILE_NAME = 'upgrade_logs.tar.gz.zip'
    ESI_FILE_NAME_ROLLBACK = 'esi_restore_rollback.tar.gz.zip'

    # Actions that are asynchronous towards VNFM
    INSTANTIATE_VNF = 'instantiate'
    TERMINATE_VNF = 'terminate'
    HEAL_VNF = 'heal'
    OPERATE_VNF = 'operate'

    CONFIRM_TIMEOUT_VALUE = 2400 # seconds
    #POST_CHECK_TIMER_VALUE = 2
    POST_CHECK_TIMER_VALUE = 20*60 # 20 minutes
    def __init__(self, job_id, a_restore_job, lcm_config, callback_when_finished):
        """ Constructor """
        super(Restore, self).__init__(job_id,
                                      a_restore_job,
                                      lcm_config,
                                      callback_when_finished,
                                      'Restore' + str(job_id),
                                      LOG,
                                      Restore.CURRENT_VERSION)
        LOG.debug("Class Restore - __init__(self, job_id, a_restore_job, lcm_config"
                  ", callback_when_finished) - job_id: %s, a_restore_job: %s, "
                  "lcm_config: %s, callback_when_finished: %s" %
                  (job_id, a_restore_job, lcm_config, callback_when_finished))

        self.is_cancel_ongoing = False
        self.cancel_requested = False
        self.fallback_timeout_flag = None
        self.fallback_timer = None
        self.lcop_rsp_timer = None
        self.lcop_rsp_timeout_value = None
        self.lcop_rsp_timeout_flag = False
        self.working_state = EnumWorkingState.IDLE
        self.vnf_comm_handle_from = None
        self.ext_operation_data = None
        self.vnfm_url = "http://%s:%s/" % (host_in_url(self.lcm_config.get_vnfm_http_host()),
                                           self.lcm_config.get_vnfm_http_port())
        self.vnfm_comm_handle = vnfm_api_comm.get_vnfm_comm(self.vnfm_url)
        self.vnf_port = self.lcm_config.get_vnfi_http_port()
        self.to_vnf_instantiated = False
        self.vnf_comm_handle_to = None
        self.esi_file_path = None
        self.add_infos = list()
        self.is_lccn_ongoing_for_to_vnf = False
        self.lccn_ongoing_operation_name = ""
        self.requested_time_for_vnf = None
        self.post_check_timer = None

    def main(self, *operation_data):
        """ Main method for script restore """
        LOG.debug('main(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Restore Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        # Ensure that restore is supported by LCM FW
        current_lcm_fw_version = lcm_util.get_lcm_fw_version().get_current_version()
        if current_lcm_fw_version < lcm_util.EnumVersion.VER_3_0:
            abort_reason = ("Function Restore is not allowed to be executed due to"
                            " the LCM Framework do NOT support it."
                            " Current version: %s, Required version: %s"
                            % (lcm_util.get_lcm_fw_version().get_current_version(),
                               lcm_util.EnumVersion.VER_3_0))
            LOG.info(abort_reason)
            raise lcm_util.LcmException(
                lcm_util.LcmException.NOT_ALLOWED,abort_reason)

        self.put((self.function(), operation_data))

    def cancel(self, *operation_data):
        """ Cancel requested """
        LOG.debug('cancel(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Restore Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        if (self.__get_restore_job().get_state() == (lcm_upgrade_job.get_restore_state_enum().
                                                     RESTORE_IN_PROGRESS) or
            self.__get_restore_job().get_state() == (lcm_upgrade_job.get_restore_state_enum().
                                                     WAITING_FOR_CONFIRM) or
            self.__get_restore_job().get_state() == (lcm_upgrade_job.get_restore_state_enum().
                                                     CONFIRM_IN_PROGRESS)):
            self.cancel_requested = True
            self.put((self.function(), operation_data))

        else:
            raise lcm_util.LcmException(
                lcm_util.LcmException.NOT_ALLOWED,
                "Attempt to execute operation cancel for a restore job (job_id: %s"
                ") when not expected. Current state: %s, Current workflow: %s"
                % (self.job_id, self.__get_restore_job().state,
                   self.__get_restore_job().current_workflow))

    def confirm(self, *operation_data):
        """ Confirm requested """
        LOG.debug('confirm(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Restore Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        if (self.__get_restore_job().get_state() == (lcm_upgrade_job.get_restore_state_enum().
                                                     WAITING_FOR_CONFIRM)):
            self.put((self.function(), operation_data))

        else:
            raise lcm_util.LcmException(
                lcm_util.LcmException.NOT_ALLOWED,
                "Attempt to execute operation confirm for a restore job (job_id: %s"
                ") when not expected. Current state: %s, Current workflow: %s"
                % (self.job_id, self.__get_restore_job().state,
                   self.__get_restore_job().current_workflow))

    def restore_status(self, *operation_data):
        """ Restore status response """
        LOG.debug('restore_status(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Restore Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
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

    def post_check_status(self, *operation_data):
        """ Post check status indication from To-VNF """
        LOG.debug("post_check_status(self, *operation_data) - operation_data: %s" % (operation_data))
        LOG.info("LCM Workflow Restore Cinder Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def restore_job(self):
        """
        This workflow method is called at start-up to clean-up resources
        and restore the restore job, after a RAN-VNFM restart/crash. Any
        resources that might have been allocated but not free marked, due
        to crash/restart, are removed.
        In case the job state before the RAN VNFM restart/crash was
        WAITING_FOR_CONFIRM,  there are two main cases considered:
        1) Confirm was received, but the from-state-vnf was not terminated,
        before the RAN-VNFM restarted/crashed.
        -> If the to-state-vnf is operational, the restore state is kept in
        WATING_FOR_CONFIRM, and it is the operator's decision  to re-confirm
        the restore or not.
        Otherwise (to-state-vnf is non operational), the restore is rolled back.
        2) Confirm was received, and the from-state-vnf was terminated, before
        the RAN-VNFM restarted/crashed
        -> The restore is automatically confirmed when RAN-VNFM is up after
        restart/crash, regardless of operational state of the to-state-vnf.
        """
        LOG.debug("restore_job(self) - restore_job: %s" % (self.__get_restore_job()))
        if (self.__get_restore_job().get_state() ==
            lcm_upgrade_job.get_restore_state_enum().WAITING_FOR_CONFIRM):
            self.__get_restore_job().to_vnf_id = self.__get_restore_job().resource.to_vnf_id
            # Check if Confirm was received before R-VNFM restart
            if (self.__get_restore_job().resource.operation == (lcm_upgrade_job.
                                                                get_restore_operation_enum().
                                                                CONFIRM) and
                self.__get_restore_job().resource.wf_name != None):
                LOG.debug("restore_job(self) - Confirm of restore has been recieved")
                try:
                    from_vnf_info = self.__get_vnf_info(self.__get_restore_job().vnf_id)
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
                                "restore shall be confirmed or not. Restore job id: %s,"
                                " Reason in exception: %s" % (
                                    self.__get_restore_job().get_job_id(), exception))
                    add_info = ["A restart of R-VNFM occured while this operation was "
                                "in progress i.e. it is aborted. When trying to find out what to do "
                                "after restart an exception was caught and due to this manual "
                                "decisions is required to decide if the restore shall be "
                                "confirmed or not."]
                    self.add_infos.extend(add_info)
                    self.add_infos.append("To-VNF ID: %s" % (self.__get_restore_job().get_to_vnf_id()))
                    self.__set_operation_result(
                        self.__get_restore_job().resource.operation,
                        self.__get_restore_job().resource.fallback_timeout,
                        lcm_upgrade_job.get_restore_op_result_enum().FAILURE,
                        self.add_infos)
                    self.__get_restore_job().resource.cleanup_wfc()
                    self.__set_job()
                    #Restart fallback timer
                    if self.__get_restore_job().resource.fallback_timeout != None:
                        self.fallback_timer = threading.Timer(
                            self.__get_restore_job().resource.fallback_timeout,
                            self.timeout)
                        self.fallback_timer.start()
                        LOG.debug("Activate Fallback timer started (%s) at restore."
                                  " Timeout:  %s" %
                                  (self.fallback_timer,
                                   self.__get_restore_job().resource.fallback_timeout))

            # Confirm was not yet received at R-VNFM restart
            else:
                if self.__get_restore_job().resource.fallback_timeout != None:
                    self.fallback_timer = threading.Timer(self.__get_restore_job().resource.fallback_timeout,
                                                          self.timeout)
                    self.fallback_timer.start()
                    LOG.debug("Restore Fallback timer started (%s) at restore. Timeout:  %s" %
                              (self.fallback_timer, self.__get_restore_job().resource.fallback_timeout))

        else:
            self.__rollback_at_restore()

    def __check_to_vnf_at_restore(self):
        """
        Checks state of To-VNF at restore and if:
        - It it is operational the confirm operation that was in progress at restart will be
        set to failed and the upgrade job state WAITING_FOR_CONFIRM is kept.
        - It is Not operational a rollback is initiated.
        """
        LOG.debug("__check_to_vnf_at_restore(self)")

        to_vnf_info = self.__get_vnf_info(self.__get_restore_job().to_vnf_id)
        if (to_vnf_info != None and
            to_vnf_info['operationalState'] == 'ENABLED'):
            LOG.debug("__check_to_vnf_at_restore(self) - A restart of R-VNFM occured while "
                      "this operation was in progress i.e. it is aborted")
            self.add_infos.append("A restart of R-VNFM occured while this operation was "
                                  "in progress i.e. it is aborted")
            self.__set_operation_result(
                self.__get_restore_job().resource.operation,
                self.__get_restore_job().resource.fallback_timeout,
                lcm_upgrade_job.get_restore_op_result_enum().FAILURE,
                self.add_infos)
            self.__get_restore_job().resource.cleanup_wfc()
            self.__set_job()
            #Restart fallback timer
            if self.__get_restore_job().resource.fallback_timeout != None:
                self.fallback_timer = threading.Timer(self.__get_restore_job().resource.fallback_timeout,
                                                      self.timeout)
                self.fallback_timer.start()
                LOG.debug("Activate Fallback timer started (%s) at restore. Timeout:  %s" %
                          (self.fallback_timer, self.__get_restore_job().resource.fallback_timeout))


        else:
            LOG.debug("__check_to_vnf_at_restore(self) - Either To-VNF do not exist "
                      "or operational state is not as it should. Rollback is requested "
                      "due to this problem. To-VNF info: %s"
                      % (to_vnf_info))
            add_info = [' Reason for the rollback is that the To-VNF is not operational '
                        'or that To-VNF has been terminated']
            self.__rollback_at_restore(add_info)

    def import_esi(self, vnf_id):
        """ Requests for import of ESI """
        LOG.debug("import_esi(self, vnf_id) - vnf_id: %s" % (vnf_id))
        message_data = ("Working_state: %s, To-VNF id: %s, Attempt from VNF id: %s"
                        "From-VNF id: %s"
                        % (self.working_state, self.__get_restore_job().to_vnf_id, vnf_id,
                           self.__get_restore_job().vnf_id))
        self.__check(self.working_state == EnumWorkingState.WAIT_FOR_FETCH_ESI_RESULT,
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs in unexpected working state. %s"
                     % (message_data))
        self.__check((self.__get_restore_job().vnf_id == vnf_id or
                      self.__get_restore_job().to_vnf_id == vnf_id),
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs from unexpected VNF ID. %s"
                     % (message_data))
        return self.esi_file_path

    def import_upgrade_logs(self, vnf_id):
        """ Requests for import of upgrade logs """
        LOG.debug("import_upgrade_logs(self, vnf_id) - vnf_id: %s" % (vnf_id))
        message_data = ("Working_state: %s, To-VNF id: %s, Attempt from VNF id: %s"
                        "From-VNF id: %s"
                        % (self.working_state, self.__get_restore_job().to_vnf_id, vnf_id,
                           self.__get_restore_job().vnf_id))
        self.__check(self.working_state == EnumWorkingState.WAIT_FOR_FETCH_UPGRADE_LOG_RESULT,
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs unexpected working state. %s"
                     % (message_data))
        self.__check(self.__get_restore_job().to_vnf_id == vnf_id,
                     lcm_util.LcmException.INTERNAL_ERROR,
                     "Attempt to import upgrade logs unexpected VNF ID. %s"
                     % (message_data))
        return self.upgrade_logs_file_path

    #
    # Activate thread loop - Aynchronous operations
    #
    def handle_main(self, operation_data):
        """
        Script entry point for the asynchronous lcm operation restore
        """
        LOG.debug("handle_main(self, operation_data) - "
                  "operation_data: %s" % (operation_data))
        try:
            self.__get_restore_job().resource.previous_state = self.__get_restore_job().get_state()
            self.ext_operation_data = operation_data
            self.fallback_timer = threading.Timer(self.get_fallback_timeout(self.ext_operation_data),
                                                  self.timeout)
            self.fallback_timer.start()
            LOG.debug("Restore Fallback timer started (%s). Timeout:  %s" %
                      (self.fallback_timer, self.get_fallback_timeout(self.ext_operation_data)))
            self.__set_state(lcm_upgrade_job.get_restore_state_enum().RESTORE_IN_PROGRESS)
            self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().RESTORING)
            self.__get_restore_job().resource.operation = (lcm_upgrade_job.
                                                           get_restore_operation_enum().
                                                           RESTORE)
            self.__get_restore_job().resource.wf_name = (lcm_upgrade_job.
                                                         get_restore_workflow_enum().
                                                         RESTORE_CINDER_OPERATION)
            self.requested_time_for_vnf = str(calendar.timegm(time.localtime()))
            self.__get_restore_job().resource.fallback_timeout = self.get_fallback_timeout(
                operation_data)
            self.lcop_rsp_timeout_value = self.__get_lcop_tmo()
            self.__set_job()
            del self.add_infos[:]

            self.__is_cancel_requested()
            self.__is_fallback_timeout()

            LOG.info("Request to start restore recived. Ensure that any upgrade job"
                     " for given VNF is aborted. VNF: %s" % (operation_data['vnf_id']))
            self._ensure_no_upgrade_is_ongoing(operation_data['vnf_id'])
            LOG.info("No upgrade job is in working state for given VNF: %s" %
                     (operation_data['vnf_id']))

            self.working_state = EnumWorkingState.WAIT_FOR_TO_VNF_UP_AND_RUNNING
            self.__create_restored_vnf()
            self.__upgrade_clone_complete()
            self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().WAITING_FOR_VNF_APPLICATION_READY)
            return True

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(self.ext_operation_data)
            return False

        except lcm_wf_utility.WorkflowTmoException as exception:
            self.handle_timeout(self.ext_operation_data)
            return False

        except lcm_wf_utility.RestoreFailedException as exception:
            self.handle_error(str(exception), self.ext_operation_data,
                              exception.reason_info)
            return False

        except (lcm_util.LcmException,
                vnf_api_comm.VnfCommException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("Restore of the UP with JOB ID (%s) failed due to %s"
                      % (self.job_id, str(exception)))
            self.handle_error(str(exception), operation_data)
            return False

        except Exception as exception:
            LOG.error("Restore of the UP with JOB ID (%s) failed due to %s"
                      % (self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), operation_data)
            return False

    def handle_timeout_post_check(self, _operation_data):
        """ Timeout occured while waiting for post check result """
        LOG.debug("handle_timeout_post_check(self, _operation_data)")
        if self.working_state == EnumWorkingState.WAIT_FOR_POST_CHECK_RESULT:
            self.working_state = EnumWorkingState.IDLE
            message = ("The post check performed in To-VNF didn't respond within"
                       " pre-defined time (%s s)." % (self.POST_CHECK_TIMER_VALUE))
            LOG.info(message)
            self.handle_error(message, self.ext_operation_data,
                              lcm_wf_utility.RestoreFailedException.POST_CHECK_TIMEOUT)
            return False

        else:
            LOG.info("Post check timer expired when not waiting for the post"
                     " check result from To-VNF (ignored)")
            return True

    def handle_post_check_status(self, operation_data):
        """ Post check status indication received from To-VNF """
        LOG.debug("handle_post_check_status(self, operation_data) - "
                  "operation_data: %s" % (operation_data))
        try:
            if self.working_state == EnumWorkingState.WAIT_FOR_POST_CHECK_RESULT:
                self.working_state = EnumWorkingState.IDLE
                if self.post_check_timer != None:
                    LOG.debug("Post check timer is stopped")
                    self.post_check_timer.cancel()
                if operation_data['result'] == 'SUCCESS':
                    self.add_infos.append("To-VNF ID: %s" % (self.__get_restore_job().get_to_vnf_id()))
                    if 'info' in operation_data:
                        self.add_infos.extend(operation_data['info'])
                    self.__set_operation_result(
                        self.__get_restore_job().resource.operation,
                        self.get_fallback_timeout(self.ext_operation_data),
                        lcm_upgrade_job.get_restore_op_result_enum().SUCCESS,
                        self.add_infos)
                    self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().WAITING_FOR_CONFIRMATION)
                    self.__finish(lcm_upgrade_job.get_restore_state_enum().WAITING_FOR_CONFIRM, False)
                else:
                    self.add_infos.append("To-VNF ID: %s" % (self.__get_restore_job().get_to_vnf_id()))
                    self.add_infos.extend(operation_data['info'])
                    self.__set_operation_result(
                        self.__get_restore_job().resource.operation,
                        self.get_fallback_timeout(self.ext_operation_data),
                        lcm_upgrade_job.get_restore_op_result_enum().FAILURE,
                        self.add_infos)
                    self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().WAITING_FOR_CONFIRMATION)
                    self.__finish(lcm_upgrade_job.get_restore_state_enum().WAITING_FOR_CONFIRM, False)

            else:
                message = ("Received VNF Post check status indication when not expected (ignored)."
                           " Received operation data: %s" % (operation_data))
                LOG.info(message)

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
            LOG.debug("handle_post_check_status(self, operation_data) - "
                      "operation_data: %s. Restore of the UP with "
                      "job id (%s) failed due to %s"
                      % (operation_data, self.job_id, exception))
            self.handle_error(str(exception), operation_data)
            return False

        except Exception as exception:
            LOG.error("handle_post_check_status(self, operation_data) - "
                      "operation_data: %s. Restore of the UP with "
                      "job id (%s) failed due to %s"
                      % (operation_data, self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), self.ext_operation_data)
            return False


    def handle_restore_status(self, operation_data):
        """ Handling of restore status response """
        LOG.debug('handle_restore_status(self, operation_data) - operation_data: %s'
                  % (operation_data))

        try:
            if self.__is_vnf_known(operation_data['vnf_id']):
                if operation_data['status']['status_code'] == "RUNNING":
                    if self.working_state == EnumWorkingState.WAIT_FOR_TO_VNF_UP_AND_RUNNING:
                        return self.__continue_restore(operation_data)
                    else:
                        LOG.warning("handle_restore_status(self, operation_data) - "
                                    "operation_data: %s. Restore status information "
                                    "received with status code RUNNING when not "
                                    "expecting it i.e. internal working state is not "
                                    "WAIT_FOR_TO_VNF_UP_AND_RUNNING. Restore job: %s (ignored)"
                                    % (operation_data, self.__get_restore_job()))

                elif operation_data['status']['status_code'] == "FAILURE":
                    LOG.error("handle_restore_status(self, operation_data) - "
                              "Restore status information received from VNF (%s) with"
                              " status code: FAILURE and message: %s"
                              % (operation_data['vnf_id'],
                                 operation_data['status']['info']))
                    self.handle_error("Restore status information received from VNF (%s)"
                                      " with status code: FAILURE and message: %s"
                                      % (operation_data['vnf_id'],
                                         operation_data['status']['info']),
                                      operation_data)
                    return False

                else:
                    LOG.info("Restore status information received from VNF (%s) with "
                             "unexpected status code: %s and message: '%s'"
                             % (operation_data['vnf_id'],
                                operation_data['status']['status_code'],
                                operation_data['status']['info']))

            else:
                LOG.warning("handle_restore_status(self, operation_data) - "
                            "operation_data: %s. Restore status information "
                            "received from an unknown VNF ID (ignored), restore job: %s"
                            % (operation_data, self.__get_restore_job()))

            return True

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(self.ext_operation_data)
            return False

        except lcm_wf_utility.WorkflowTmoException as exception:
            self.handle_timeout(self.ext_operation_data)
            return False

        except lcm_wf_utility.RestoreFailedException as exception:
            self.handle_error(str(exception), self.ext_operation_data,
                              exception.reason_info)
            return False

        except (lcm_util.LcmException,
                vnf_api_comm.VnfCommException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("handle_restore_status(self, operation_data) - "
                      "operation_data: %s. Restore of the UP with "
                      "job id (%s) failed due to %s"
                      % (operation_data, self.job_id, exception))
            self.handle_error(str(exception), operation_data)
            return False

        except Exception as exception:
            LOG.error("handle_restore_status(self, operation_data) - "
                      "operation_data: %s. Restore of the UP with "
                      "job id (%s) failed due to %s"
                      % (operation_data, self.job_id, exception))
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
                            % (vnf_lccn_operation_data, self.__get_restore_job()))
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

    def handle_confirm(self, operation_data, continues = False, add_info = list()):
        """
        Script entry point for the case where an ongoing upgrade is confirmed
        """
        LOG.debug('handle_confirm(self, operation_data, continues = False, add_info = '
                  ') - operation_data: %s, continues: %s, add_info: %s'
                  % (operation_data, continues, add_info))
        to_vnf_id = self.__get_restore_job().get_to_vnf_id()
        try:
            del self.add_infos[:]
            if self.is_lccn_ongoing_for_to_vnf and not continues:
                self.__confirm_rejected_lccn_ongoing(add_info)
                LOG.info("Confirm not allowed since an lccn operation is ongoing"
                         " on To-VNF")
                return True

            LOG.info("Confirm of Restore is ongoing")
            if not 'confirm_timeout' in operation_data:
                operation_data['confirm_timeout'] = self.CONFIRM_TIMEOUT_VALUE
            self.__set_state(lcm_upgrade_job.get_restore_state_enum().CONFIRM_IN_PROGRESS)
            self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().TERMINATING_FROM_STATE_VNF)
            self.__get_restore_job().operation_result.result = ''
            self.__get_restore_job().operation_result.additional_infos = self.add_infos
            self.__get_restore_job().operation_result.fallback_timeout = operation_data[
                'confirm_timeout']
            if self.fallback_timer != None:
                self.fallback_timer.cancel()
            self.__get_restore_job().resource.fallback_timeout = operation_data[
                'confirm_timeout']
            self.fallback_timer = threading.Timer(
                self.__get_restore_job().resource.fallback_timeout,
                self.timeout)
            self.fallback_timer.start()
            LOG.debug("Confirm Fallback timer started (%s). Timeout:  %s" %
                      (self.fallback_timer, self.__get_restore_job().resource.fallback_timeout))
            if not continues:
                self.__get_restore_job().resource.operation = (lcm_upgrade_job.
                                                               get_restore_operation_enum().
                                                               CONFIRM)
                self.__set_resource()
            self.__unsubscribe_for_lcop(self.__get_restore_job().resource.subscription_url_id)

            LOG.info("Requesting VNFM to delete From-VNF")
            self.__terminate_vnf(self.__get_restore_job().get_vnf_id(), "From-")
            self.__remove_vnf_id(self.__get_restore_job().get_vnf_id(), "From-")
            self.add_infos.extend(add_info)
            self.__set_operation_result(
                self.__get_restore_job().resource.operation,
                self.__get_restore_job().resource.fallback_timeout,
                lcm_upgrade_job.get_restore_op_result_enum().SUCCESS,
                self.add_infos)
            self.handle_cleanup(
                lcm_upgrade_job.get_restore_state_enum().CONFIRM_COMPLETED,
                lcm_upgrade_job.get_restore_progress_enum().TERMINATING_FROM_STATE_VNF)
            self.__restore_confirmed(to_vnf_id, self.add_infos)
            return False

        except Exception as exception:
            LOG.warning("Failed to remove (clean up) the From-VNF at confirmation. Job id: %s,"
                        " Reason: %s (ignored since can't do anything else))"
                        % (self.job_id, exception))
            LOG.exception(exception)
            try:
                self.add_infos.append("%s" % (exception))
                self.__set_operation_result(
                    self.__get_restore_job().resource.operation,
                    self.get_fallback_timeout(operation_data),
                    lcm_upgrade_job.get_restore_op_result_enum().FAILURE,
                    self.add_infos)
                self.handle_cleanup(
                    self.__get_restore_job().resource.previous_state,
                    lcm_upgrade_job.get_restore_progress_enum().TERMINATING_FROM_STATE_VNF)
                self.__restore_confirmed(to_vnf_id, self.add_infos)

            finally:
                return False

    def __continue_restore(self, operation_data):
        """ Continues the restore after that the TO VNF is up and running """
        LOG.debug("__continue_restore(self, operation_data) - "
                  "operation_data: %s" % (operation_data))

        if (operation_data['vnf_id'] == self.__get_restore_job().get_to_vnf_id()):
            self.__is_cancel_requested()
            self.__is_fallback_timeout()

            LOG.info("Requesting From-VNF to stop traffic")
            self.__get_restore_job().set_progress(
                lcm_upgrade_job.get_restore_progress_enum().STOP_TRAFFIC_FOR_FROM_STATE_VNF)
            stop_traffic_data = self.__stop_traffic(self.requested_time_for_vnf,
                                                    "SoftwareRestore")

            self.__copy_upgrade_logs()

            LOG.info("Requesting From-VNF to be stopped")
            self.__get_restore_job().set_progress(
                lcm_upgrade_job.get_restore_progress_enum().STOP_EXECUTION_OF_FROM_STATE_VNF)
            from_vnf_shutdown_time = str(calendar.timegm(time.localtime()))
            self.__operate_vnf(self.__get_restore_job().vnf_id, 'STOPPED', 'From', False)
            self.__get_restore_job().resource.is_from_vnf_stoped = True
            self.__set_resource()
            LOG.info("Shutdown info related to From-VNF is sent to To-VNF")
            if stop_traffic_data == None:
                stop_traffic_data = {'stop_time': from_vnf_shutdown_time}
                self.add_infos.append("Using shutdown request time of From-VNF as "
                                      "stop traffic data (%s)" %
                                      (from_vnf_shutdown_time))
            self.__start_traffic(stop_traffic_data)

            LOG.info("Requesting post check for To-VNF")
            self.working_state = EnumWorkingState.WAIT_FOR_POST_CHECK_RESULT
            self.post_check_timer = threading.Timer(self.POST_CHECK_TIMER_VALUE,
                                                    self.timeout_post_check)
            self.post_check_timer.start()
            LOG.debug("Post check request timer started (%s s.)" %
                      (self.POST_CHECK_TIMER_VALUE))
            self.__post_check_req()
            return True

        else:
            LOG.warning("__continue_restore(self, operation_data) - "
                        "operation_data: %s. Restore status indication with status "
                        "code RUNNING has been received from the From-VNF which "
                        "is not expected (ignored), Restore job: %s"
                        % (operation_data, self.__get_restore_job557()))
            return True

    def __create_restored_vnf(self):
        """ Creates the To-VNF for the VNF to be restored """
        LOG.debug("__create_restored_vnf(self)")
        self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().CREATING_TO_STATE_VNF)
        vnf_info = self.vnfm_comm_handle.get_vnf_info(
            self.__get_restore_job().get_vnf_id(),
            [vnfm_api_comm.VnfmComm.EXT_VIRTUAL_LINKS,
             vnfm_api_comm.VnfmComm.STORAGE])
        ext_virtual_links = vnf_info.get_ext_virtual_links()
        LOG.debug("External virtual links for From-VNF: %s" % (ext_virtual_links))
        storages = vnf_info.get_storages()
        LOG.debug("Storages for From-VNF: %s" % (storages))
        LOG.info("Requesting VNFM to create a new To-VNF")
        self.__instantiate_vnf(vnf_info.instance_name,
                               vnf_info.description,
                               ext_virtual_links,
                               storages)
        self.__is_cancel_requested()
        self.__is_fallback_timeout()
        self.__subscribe_for_lcop(self.__get_restore_job().to_vnf_id, ['heal', 'operate', 'terminate'])


    def __instantiate_vnf(self, instance_name,  description, ext_virtual_links, storages):
        """ Instantiates a VNF """
        LOG.debug("__instantiate_vnf(self, instance_name,  description, ext_virtual_links, storages)"
                  " - instance_name: %s,  description: %s, ext_virtual_links: %s, storages: %s"
                  % (instance_name,  description, ext_virtual_links, storages))

        # Create vnf id
        self.__get_restore_job().to_vnf_id = self.vnfm_comm_handle.create_vnf_id(
            self.__get_restore_job().get_vnf_descriptor_id(), instance_name, description)
        self.__get_restore_job().resource.to_vnf_id = self.__get_restore_job().to_vnf_id
        self.__set_job()

        # Create vnf instance
        http_payload = self.__add_storages(storages)
        http_payload = self.__add_ext_virtual_links(ext_virtual_links, http_payload)
        instantiate_rsp_data = self.vnfm_comm_handle.instantiate_vnf(
            self.__get_restore_job().to_vnf_id, http_payload)
        self.to_vnf_instantiated = True
        self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().WAITING_FOR_CS_READY)
        self.__wait_for_lc_op_ok(instantiate_rsp_data['vnfLcOpId'],
                                 'After requested instantiation of VNF ID: %s'
                                 % (self.__get_restore_job().to_vnf_id),
                                 True,
                                 self.INSTANTIATE_VNF)
        self.__is_cancel_requested()
        self.__is_fallback_timeout()
        self.__wait_for_vnf_to_be_active(self.__get_restore_job().to_vnf_id)
        return self.__get_restore_job().to_vnf_id

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

    def __subscribe_for_lcop(self, vnf_id, operations):
        """ Subscribe for lcop from given vnf """
        LOG.debug("__subscribe_for_lcop(self, vnf_id, operations) - "
                  "VNF id: %s, operations: %s" % (vnf_id, operations))
        endpoint = "http://%s:%s%s/v1/vnf/notifications" % (
            host_in_url('localhost'),
            self.lcm_config.get_lcm_mani_http_port(),
            lcm_util.get_lcm_base_uri().LCM_LCOP_API_BASE)
        self.__get_restore_job().resource.subscription_url_id = self.vnfm_comm_handle.subscribe_lcop(
            endpoint, vnf_id, operations)
        self.__set_resource()

    def __get_vnf_comm_to(self, to_vnf_id = None):
        """ Returns a handle to vnf_comm_to """
        LOG.debug("__get_vnf_comm_to(self, to_vnf_id = None) - to_vnf_id: %s" %
                  (to_vnf_id))
        if self.vnf_comm_handle_to == None:
            if to_vnf_id == None:
                to_vnf_id_use = self.__get_restore_job().get_to_vnf_id()
            else:
                to_vnf_id_use = to_vnf_id

            if to_vnf_id_use != None:
                self.vnf_comm_handle_to = vnf_api_comm.get_vnf_comm(
                    to_vnf_id_use,
                    self.vnf_port,
                    self.vnfm_url)
            else:
                LOG.info("An attempt was made to get a handle to To-VNF when it does not exist")

        return self.vnf_comm_handle_to

    def __is_vnf_known(self, from_vnf_id):
        """ Checks that vnf id is known for this workflow """
        LOG.debug("__is_vnf_known(from_vnf_id) - from_vnf_id: %s" % (from_vnf_id))

        if ((from_vnf_id == self.__get_restore_job().get_to_vnf_id())):
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
        self.__get_restore_job().resource.cleanup()
        self.__finish(state)
        self.__set_progress(final_progress_value)
        self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().IDLE)
        self.__set_restore_job(None)
        self.is_cancel_ongoing = False


    def handle_cancel(self, operation_data):
        """
        Script entry point for the case where an ongoing job is canceled
        """
        LOG.debug('handle_cancel(self, operation_data) - operation_data: %s'
                  % (operation_data))
        del self.add_infos[:]
        self.__handle_interrupt("cancel", operation_data,
                                lcm_wf_utility.RestoreFailedException.CANCELLED)
        return False

    def handle_timeout(self, operation_data):
        """
        Script entry point for the case where the fallback timer expired
        """
        LOG.debug('handle_timeout(self, operation_data) - operation_data: %s'
                  % (operation_data))

        self.__handle_interrupt("timeout", operation_data,
                                lcm_wf_utility.RestoreFailedException.FALLBACK_TIMEOUT)
        return False

    def __handle_interrupt(self, interrupt_type, operation_data, functional_reason = None):
        """
        Handles the case where an ongoing job is interrupted
        """
        LOG.debug('handle_interrupt(self, interrupt_type, operation_data, functional_reason'
                  ' = None) - interrupt_type: %s, operation_data: %s, functional_reason: '
                  '%s' % (interrupt_type, operation_data, functional_reason))

        #Workflow Progress and Result data assigments
        if interrupt_type == "cancel":
            init_progress_value = (lcm_upgrade_job.get_restore_progress_enum().
                                   CANCEL_ONGOING_OPERATION_REQUESTED)
            cleanup_progress_value = (lcm_upgrade_job.get_restore_progress_enum().
                                      CLEANUP_DUE_TO_CANCEL_REQUEST)
            result = lcm_upgrade_job.get_restore_op_result_enum().CANCELLED
        elif interrupt_type == "timeout":
            init_progress_value = (lcm_upgrade_job.get_restore_progress_enum().
                                   FALLBACK_TIMER_EXPIRED)
            cleanup_progress_value = (lcm_upgrade_job.get_restore_progress_enum().
                                      CLEANUP_DUE_TO_TIMEOUT)
            result = lcm_upgrade_job.get_restore_op_result_enum().FALLBACK_TIMEOUT

        self.__set_progress(init_progress_value)
        if interrupt_type == "cancel":
            self.is_cancel_ongoing = True
        self.__unsubscribe_for_lcop(self.__get_restore_job().resource.subscription_url_id)
        export_result = self.__export_esi(self.ESI_FILE_NAME_ROLLBACK,
                                          "Requesting To-VNF to export ESI to LCM tmp directory")
        self.__heal_from_vnf_if_needed("The restore job (%s) has been cancelled"
                                       % (self.__get_restore_job().get_job_id()),
                                       self.__get_restore_job().get_to_vnf_id())
        self.__import_esi(self.__vnf_api_comm_from(),
                          self.__get_restore_job().get_vnf_id(),
                          export_result,
                          "Requesting From-VNF to export ESI (origin from To-VNF)")
        self.__set_progress(cleanup_progress_value)
        self.__terminate_to_vnf()
        if (interrupt_type == "cancel" and
            'info' in operation_data):
            self.add_infos.extend(operation_data['info'])
        self.__restore_aborted(self.add_infos, functional_reason)
        self.__set_operation_result(self.__get_restore_job().resource.operation,
                                    self.get_fallback_timeout(operation_data),
                                    result,
                                    self.add_infos)

        self.handle_cleanup(
            self.__get_restore_job().resource.previous_state,
            cleanup_progress_value)

    def handle_error(self, reason, operation_data, functional_reason = None):
        """
        Workflow entry point for the case where an unexpected problem occured
        Arg reason may be a string or list
        """
        LOG.debug("handle_error(self, reason, operation_data, functional_reason = None)"
                  " - reason: %s, operation_data: %s, functional_reason: %s"
                  % (reason, operation_data, functional_reason))

        self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().CLEANUP_DUE_TO_FAILURE)
        self.__unsubscribe_for_lcop(self.__get_restore_job().resource.subscription_url_id)
        export_result = self.__export_esi(self.ESI_FILE_NAME_ROLLBACK,
                                          "Requesting To-VNF to export ESI to LCM tmp directory")
        self.__heal_from_vnf_if_needed("A problem has been detected during the restore and"
                                       " hence it is aborted (job id: %s)"
                                       % (self.__get_restore_job().get_job_id()),
                                       self.__get_restore_job().get_to_vnf_id())
        self.__import_esi(self.__vnf_api_comm_from(),
                          self.__get_restore_job().get_vnf_id(),
                          export_result,
                          "Requesting From-VNF to export ESI (origin from To-VNF)")
        self.__terminate_to_vnf()
        if not type(reason) is list:
            reason = [reason]
        self.add_infos.extend(reason)
        self.__restore_aborted(self.add_infos, functional_reason)
        self.__set_operation_result(
            self.__get_restore_job().resource.operation,
            self.get_fallback_timeout(operation_data),
            lcm_upgrade_job.get_restore_op_result_enum().FAILURE,
            self.add_infos)
        self.handle_cleanup(
            self.__get_restore_job().resource.previous_state,
            lcm_upgrade_job.get_restore_progress_enum().CLEANUP_DUE_TO_FAILURE)

    def __terminate_to_vnf(self):
        """ Terminates any created TO VNF, if exists """
        LOG.debug("__terminate_to_vnf(self)")
        try:
            if self.__get_restore_job().get_to_vnf_id() != None:
                LOG.info("Requesting VNFM terminate To-VNF and delete "
                         "its VNF ID (%s) due to rollback of the restore"
                         % (self.__get_restore_job().get_to_vnf_id()))
                self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().TERMINATING_TO_STATE_VNF)
                self.__terminate_vnf(self.__get_restore_job().get_to_vnf_id(), "To-")
                self.__remove_vnf_id(self.__get_restore_job().get_to_vnf_id(), "To-")
                self.__get_restore_job().set_to_vnf_id(None)

        except Exception as exception:
            LOG.info("__terminate_to_vnf(self) - Exception caught when trying to"
                     " terminate to VNF and remove its VNF id i.e. resources might"
                     " still be allocated due to this")
            LOG.exception(exception)

    def __is_cancel_requested(self):
        """ Check if cancel is requested and act on it """
        if not self.is_cancel_ongoing:
            if self.cancel_requested:
                LOG.debug("Operation %s is aborted since operation "
                          "Cancel has been received"
                          % (self.__get_restore_job().resource.operation))
                self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().
                                    CANCEL_ONGOING_OPERATION_REQUESTED)
                self.is_cancel_ongoing = True
                raise lcm_wf_utility.CancelWorkflowException("Operation cancelled")

    def timeout(self):
        """ Callback to be executed when fallback timer expires """
        LOG.debug('Restore workflow execution timer exceeded the fallback timeout: %s seconds.'
                  % (self.__get_restore_job().resource.fallback_timeout))
        if self.__get_restore_job() == None:
            LOG.warning("A fallback timeout occured %s, but the workflow is already finished." %
                        (self.fallback_timer))
            return
        self.fallback_timeout_flag = True
        self.put((self.function(),
                  ({'fallback_timeout': self.__get_restore_job().resource.fallback_timeout}, )))

    def timeout_post_check(self):
        """ Callback to be executed when post check timer expires """
        LOG.debug('timeout_post_check(self)')
        if self.__get_restore_job() == None:
            LOG.warning("A post check  timeout occured %s, but the workflow is already finished." %
                        (self.post_check_timer))
            return
        self.put((self.function(), ({}, )))

    def __is_fallback_timeout(self):
        """ Check if fallback timer expired, and act on it """
        if self.fallback_timeout_flag:
            LOG.debug("Operation %s is aborted since fallback timer expired "
                      % (self.__get_restore_job().resource.operation))
            self.__get_restore_job().set_progress(lcm_upgrade_job.get_restore_progress_enum().
                                          FALLBACK_TIMER_EXPIRED)
            raise lcm_wf_utility.WorkflowTmoException("Fallback timer expired")

    def lcop_rsp_timeout(self):
        """ Callback to be executed when lcop_response timer expires """
        LOG.warning('Maximum VNFM lcop response wait time, %s seconds, exceeded.'
                  % (self.lcop_rsp_timeout_value))
        if self.__get_restore_job() == None:
            LOG.warning("A lcop response timeout occured %s, but the workflow is already finished." %
                        (self.lcop_rsp_timer))
            return
        self.lcop_rsp_timeout_flag = True

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

    def __restore_confirmed(self, to_vnf_id, infos):
        """ Inform VNF that restore is confirmed """
        LOG.debug("__restore_confirmed(self, to_vnf_id, infos) - to_vnf_id: %s, infos: %s" %
                  (to_vnf_id, infos))
        try:
            self.__get_vnf_comm_to(to_vnf_id).restore_confirmed({'info': infos})
        except Exception as exception:
            LOG.warning("__restore_confirmed(self, infos) - infos: %s. Exception "
                        "caught when trying to inform VNF that restore is confirmed (ignored)"
                        % (infos))
            LOG.exception(exception)


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
                          % (vnf_id, instance_info, self.__get_restore_job().get_job_id()))
                raise lcm_wf_utility.WorkflowException(
                    "The TO VNF has  been requested to be instantiated but "
                    "it has NOT been that for some reason. to_vnf_id: %s, "
                    "vnfm_instance_info: %s, job id: %s" %
                    (vnf_id, instance_info, self.__get_restore_job().get_job_id()))

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
                result_info = None
                if action_type == self.INSTANTIATE_VNF:
                    result_info = lcm_wf_utility.RestoreFailedException.INSTANTIATION_FAILURE_TO_VNF
                elif action_type == self.OPERATE_VNF:
                    result_info = lcm_wf_utility.RestoreFailedException.SHUTDOWN_FROM_VNF_FAILURE
                raise lcm_wf_utility.RestoreFailedException(
                    lcm_util.LcmException.INTERNAL_ERROR,
                    'While waiting for the lifecycle operation to finish '
                    'it reported failure - vnf_lc_op_id: %s, Reason: %s'
                    'Response received from VNFM: %s' %
                    (vnf_lc_op_id, lcop_request_info_str,
                     vnf_lc_op_rsp),
                    result_info)
            else:
                LOG.debug("__wait_for_lc_op_ok(self, "
                          "vnf_lc_op_id) - vnf_lc_op_rsp: %s" % (vnf_lc_op_rsp))

    def __react_to_any_cancel_request(self, is_interrupt_allowed):
        """ Indicate to end user that cancel has been accepted """
        if not self.is_cancel_ongoing:
            if (self.cancel_requested and
                is_interrupt_allowed and
                (self.__get_restore_job().get_progress() !=
                 lcm_upgrade_job.get_restore_progress_enum().CANCEL_ONGOING_OPERATION_REQUESTED)):
                LOG.debug("Operation %s will be aborted since operation "
                          "Cancel has been received"
                          %(self.__get_restore_job().resource.operation))
                self.__set_progress(
                    lcm_upgrade_job.get_restore_progress_enum().CANCEL_ONGOING_OPERATION_REQUESTED)

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
            if (self.__get_restore_job().get_progress() !=
                lcm_upgrade_job.get_restore_progress_enum().FALLBACK_TIMER_EXPIRED):
                self.__set_progress(
                    lcm_upgrade_job.get_restore_progress_enum().FALLBACK_TIMER_EXPIRED)
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

    def __export_esi(self, esi_file_name, log_info_text):
        """ Used to export ESI from To-VNF """
        LOG.debug("__export_esi(self, esi_file_name, log_info_text) "
                  "- esi_file_name: %s" % (esi_file_name))

        if not self.to_vnf_instantiated or self.__get_restore_job().get_to_vnf_id() == None:
            LOG.info("%s - %s" % (
                log_info_text, "Could not be performed due to To-VNF id is not instantiated."))
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
                                             self.__get_restore_job().get_job_id(),
                                             esi_file_name)
            self.__get_vnf_comm_to().export_esi(self.esi_file_path)
            return True

        except Exception as exception:
            LOG.info("__export_esi(self, esi_file_name, log_info_text). Failed to export ESI logs "
                     "from To-VNF %s due to %s (ignored)." %
                     (self.__get_restore_job().get_to_vnf_id(), exception))
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

    def __heal_from_vnf_if_needed(self, reason, to_vnf_id):
        """ Used to request for heal of From-VNF, if needed """
        LOG.debug("__heal_from_vnf_if_needed(self, reason, to_vnf_id) - "
                  "reason: %s, to_vnf_id: %s" % (reason, to_vnf_id))
        if (self.__get_restore_job().resource.is_from_vnf_stoped or
            self.__get_restore_job().resource.is_traffic_stoped):
            LOG.info("The From-VNF (%s) is requested to be healed due to"
                     " the ongoing restore job is aborted after From-VNF either has "
                     "been stoped or traffic stopped (job id: %s)" %
                     (self.__get_restore_job().get_vnf_id(), self.__get_restore_job().get_job_id()))
            self.__heal_from_vnf(reason, to_vnf_id)
        else:
            LOG.debug("__heal_from_vnf_if_needed(...) - No need to heal the From-VNF, since we did not yet "
                      "request the From-VNF to be stopped")

    def __heal_from_vnf(self, reason, to_vnf_id):
        """ Used to request for heal of From-VNF """
        LOG.debug("__heal_from_vnf(self, reason, to_vnf_id) - reason: %s, to_vnf_id: %s"
                  % (reason, to_vnf_id))

        try:
            if to_vnf_id != None:
                LOG.debug("Execution mode of To-VNF (%s) is to be stoped " % (to_vnf_id))
                self.__get_restore_job().set_progress(
                    lcm_upgrade_job.get_restore_progress_enum().STOP_EXECUTION_OF_TO_STATE_VNF)
                self.__operate_vnf(to_vnf_id, 'STOPPED', 'To', True)
        except Exception as exception:
            LOG.warning("Failed to stop the execution of To-VNF. The From-VNF will be "
                        "healed anyhow. Reason for exception: %s" % (exception))
            LOG.exception(exception)

        try:
            http_payload = {'cause': reason}
            heal_rsp_data = self.vnfm_comm_handle.heal_vnf_2(self.__get_restore_job().get_vnf_id(),
                                                             http_payload)
            LOG.debug("__heal_from_vnf(...) - Heal requested and lcop id is %s. "
                      "Needs to wait for result of the heal" % (heal_rsp_data['vnfLcOpId']))
            self.__wait_for_lc_op_ok(heal_rsp_data['vnfLcOpId'],
                                     'After requested heal of From-VNF with ID: %s'
                                     % (self.__get_restore_job().get_vnf_id()),
                                     False,
                                     self.HEAL_VNF)
            self.__get_restore_job().resource.is_from_vnf_stoped = False
            self.__get_restore_job().resource.is_traffic_stoped = False

        except (lcm_wf_utility.WorkflowException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.warning("__heal_from_vnf(self, reason, to_vnf_id). The requested heal "
                        "reported failure for From-VNF (%s). I.e. it might end up in "
                        "that this VNF not will be fully functional. This problem is "
                        "ignored since nothing more can be done. Restore job: %s,reason:"
                        " %s, to_vnf_id: %s"
                        % (self.__get_restore_job().get_vnf_id(), self.__get_restore_job().get_job_id(),
                           reason, to_vnf_id))
            LOG.exception(exception)
            self.add_infos.append("WARNING: Heal of From-VNF with id '%s' failed. I.e. "
                                  "it might end up in that this VNF not will be fully "
                                  "functional. This may require manual interaction from"
                                  " end user e.g. manual heal of From-VNF. The failure "
                                  "reason reported from VNFM: %s" %
                                  (self.__get_restore_job().get_vnf_id(), exception))

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
                     % (self.__get_restore_job().get_to_vnf_id(), subscription_url_id))

        except Exception as exception:
            LOG.warning("__unsubscribe_for_lcop(self, subscription_url_id). Failed to unsubscribe "
                        "for lccn for VNF (%s). This problem is ignored since nothing more"
                        " can be done. Subscription url id: %s"
                        % (self.__get_restore_job().get_to_vnf_id(), subscription_url_id))
            LOG.exception(exception)

    def __set_resource(self, dump = True):
        """ Saves allocated resource in DB """
        LOG.debug("__set_resource(self, dump = True) - dump: %s" % (dump))
        try:
            lcm_db_handler.get_lcm_db_handler().set_restore_resource(
                self.__get_restore_job().get_job_id(),
                self.__get_restore_job().resource)
        except lcm_util.LcmException as exception:
            LOG.warning("__set_resource(self). Failed to save allocated resources"
                        " in LCM DB which might cause future problem for this "
                        "restore job. Restore job data: %s" % (self.__get_restore_job()))
            LOG.exception(exception)

    def __set_job(self, dump = True):
        """ Saves job data to DB """
        LOG.debug("__set_job(self, dump = True) - dump: %s" % (dump))
        try:
            lcm_db_handler.get_lcm_db_handler().set_restore_job(self.__get_restore_job(), True)
        except lcm_util.LcmException as exception:
            LOG.warning("__set_job(self, dump = True) - dump: %s, "
                        "Failed to save data in to LCM DB which "
                        "might cause future problem for this restore job. "
                        "Restore job data: %s"
                        % (dump, self.__get_restore_job()))
            LOG.exception(exception)

    def __finish(self, state, callback = True):
        """ Ensure the workflow marks its execution as finished """
        LOG.debug("__finish(self, state, callback = True) - state: %s, callback: %s"
                  % (state, callback))
        self.__get_restore_job().shadow_state = state
        self.__get_restore_job().set_shadow(True)
        self.__set_state(state)
        self.__set_job()
        if callback:
            self.callback_when_finished(self.job_id, self)
        self.__get_restore_job().set_shadow(False)

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
        self.__get_restore_job().set_state(value)

    def __set_progress(self, value):
        """ Used to set progress value """
        LOG.info("Setting progress to %s (job id: %s)" % (value, self.job_id))
        self.__get_restore_job().set_progress(value)

    def __set_operation_result(self, operation_name, fallback_timeout, result,
                               add_infos):
        """ Adds operation result to restore job """
        LOG.info("Setting operation result to - operation_name: %s, result: %s,"
                 " add_infos: %s (job id: %s)"
                 % (operation_name, result, add_infos, self.job_id))
        self.__get_restore_job().set_operation_result(operation_name,
                                                      fallback_timeout,
                                                      result,
                                                      add_infos)

    def __confirm_rejected_lccn_ongoing(self, add_info):
        """ Confirm operation shall be rejected due to an ongoing lccn operation """
        LOG.debug("__confirm_rejected_lccn_ongoing(self, add_info) - add_info: %s"
                  % (add_info))

        self.add_infos.append("To-VNF ID: %s" % (self.__get_restore_job().get_to_vnf_id()))
        self.add_infos.append("Not possible to confirm right now due to operation '%s' has "
                              "been requested on To-VNF %s. In most cases this leads to rollback)." %
                              (self.lccn_ongoing_operation_name, self.__get_restore_job().get_to_vnf_id()))
        self.__set_operation_result(
            lcm_upgrade_job.get_restore_operation_enum().CONFIRM,
            self.__get_restore_job().resource.fallback_timeout,
            lcm_upgrade_job.get_restore_op_result_enum().FAILURE,
            self.add_infos)

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
            self.__vnf_api_comm_from().upgrade_clone_complete()

        except (vnf_api_comm.VnfCommException) as exception:
            LOG.debug("__upgrade_clone_complete(self). VnfCommException "
                      "caught when trying to inform From-VNF that clone of disks"
                      " are ready (ignored).")

        except Exception as exception:
            LOG.info("__upgrade_clone_complete(self). Exception "
                     "caught when trying to inform From-VNF that clone of disks"
                     " are ready (ignored).")
            LOG.exception(exception)

    def __copy_upgrade_logs(self):
        """ Copies upgrade logs from From-VNF to To-VNF """
        LOG.debug("__copy_upgrade_logs(self)")
        LOG.info("Requesting From-VNF to export certain logs to tmp directory (entries recorded"
                 " since To-VNF instantiation)")
        self.upgrade_logs_file_path ='%s%s/%s' % (self.lcm_config.get_lcm_workflow_file_path(),
                                                  self.__get_restore_job().get_job_id(),
                                                  self.UPGRADE_LOGS_FILE_NAME)
        if self.__export_upgrade_logs():
            LOG.info("Requesting To-VNF to import upgrade logs from From-VNF")
            self.working_state = EnumWorkingState.WAIT_FOR_FETCH_UPGRADE_LOG_RESULT
            result = self.__get_vnf_comm_to().fetch_upgrade_logs()
            if result['result'] != 'SUCCESS':
                reason = ("Fetch of upgrade logs failed (for To-VNF) due to failure"
                          " reported from To-VNF (%s). Reported failure: %s"
                          % (self.__get_restore_job().get_to_vnf_id(), result))
                LOG.error("handle_fetch_upgrade_log_result(self, result) - "
                          "%s" % (reason))
                raise lcm_util.LcmException(
                    lcm_util.LcmException.INTERNAL_ERROR, reason)

    def __get_restore_job(self):
        """
        Returns the restore job object.
        Due to history the restore job is called upgrade_job below.
        """
        return self.upgrade_job

    def __set_restore_job(self, value):
        """
        Sets the restore job object.
        Due to history the restore job is called upgrade_job below.
        """
        self.upgrade_job = value

    def __restore_aborted(self, add_infos, func_reason = None):
        """ Inform VNF that restore has been aborted """
        LOG.debug("__restore_aborted(self, add_infos) - add_infos: %s" % (add_infos))
        try:
            self.__vnf_api_comm_from().restore_aborted(add_infos,
                                                       result_info = func_reason)

        except (vnf_api_comm.VnfCommException) as exception:
            LOG.debug("__restore_aborted(self). VnfCommException "
                      "caught when trying to inform From-VNF that restore "
                      "has been aborted (ignored).")

        except Exception as exception:
            LOG.info("__restore_aborted(self). Exception "
                     "caught when trying to inform From-VNF that restore "
                     "has been aborted (ignored).")
            LOG.exception(exception)

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

        except lcm_wf_utility.RestoreFailedException as exception:
            raise exception

        except Exception as exception:
            LOG.info("__operate_vnf(self, vnf_id, state, which_vnf) - vnf_id: %s, state: %s, "
                     "which_vnf: %s. LCM Exception caught when trying operate the state of given "
                     "VNF" % (vnf_id, state, which_vnf))
            if not ignore:
                raise lcm_wf_utility.RestoreFailedException(
                    lcm_util.LcmException.INTERNAL_ERROR,
                    "Problem detected when trying to operate the state to (%s) for %s-VNF (%s)."
                    % (state, which_vnf, vnf_id),
                    lcm_wf_utility.RestoreFailedException.ONGOING_UPGRADE)

    def __post_check_req(self):
        """ Gives To-VNF opportunity to ensure that it is OK """
        LOG.debug("__post_check_req()")
        try:
            self.__get_vnf_comm_to().post_restore_check({})
        except vnf_api_comm.VnfCommException as exception:
            failure_reson = (
                "Failed to get the post check executed in To-VNF. Reported reason: %s"
                % (exception))
            LOG.info("%s" % (failure_reson))
            self.add_infos.append(failure_reson)
            raise lcm_wf_utility.RestoreFailedException(
                lcm_util.LcmException.INTERNAL_ERROR,
                failure_reson,
                lcm_wf_utility.RestoreFailedException.POST_CHECK_FATAL_ERROR)

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
            LOG.info("__act_on_lccn_operation_indication(self, vnf_lccn_operation_data) - "
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
                      "The result of the heal request is %s."
                      % (vnf_lccn_operation_data['vnfInstanceId'],
                         vnf_lccn_operation_data['status']))
            LOG.info("__act_on_heal_operation_indication(self, vnf_lccn_operation_data) - "
                     "vnf_lccn_operation_data: %s. %s"
                     % (vnf_lccn_operation_data, reason))
            self.handle_error(reason, self.ext_operation_data,
                              lcm_wf_utility.RestoreFailedException.TO_VNF_RESTARTED)
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
            reason = ("To-VNF (%s) has been terminated when not expected."
                      % (vnf_lccn_operation_data['vnfInstanceId']))
            LOG.info("__act_on_terminate_operation_indication(self, vnf_lccn_operation_data) - "
                     "vnf_lccn_operation_data: %s. %s"
                     % (vnf_lccn_operation_data, reason))
            self.handle_error(reason, self.ext_operation_data,
                              lcm_wf_utility.RestoreFailedException.TO_VNF_TERMINATED)
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
            reason = ("To-VNF (%s) has been stopped when not expected."
                      % (vnf_lccn_operation_data['vnfInstanceId']))
            LOG.info("__act_on_operate_operation_indication(self, vnf_lccn_operation_data) - "
                     "vnf_lccn_operation_data: %s. %s"
                    % (vnf_lccn_operation_data, reason))
            self.handle_error(reason, self.ext_operation_data,
                              lcm_wf_utility.RestoreFailedException.TO_VNF_SHUTDOWNED)
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

    def __export_upgrade_logs(self):
        """
        Exports upgrade logs from From-VNF.
        Returns True if success otherwise False.
        """
        try:
            self.__vnf_api_comm_from().export_upgrade_logs(self.upgrade_logs_file_path)
            return True
        except vnf_api_comm.VnfCommException as exception:
            failure_reson = (
                "Failed to export upgrade logs from From-VFN (ignored). "
                "Reported reason: %s" % (exception))
            LOG.info("%s" % (failure_reson))
            self.add_infos.append(failure_reson)
            return False

    def __stop_traffic(self, time_of_action, cause):
        """ Used to stop traffic in From-VNF """
        LOG.debug("__stop_traffic(self, time_of_action, cause) - time_of_action: %s, cause: %s"
                  % (time_of_action, cause))
        try:
            stop_traffic_data = self. __vnf_api_comm_from().stop_traffic_v2(
                time_of_action, cause)
            self.__get_restore_job().resource.is_traffic_stoped = True
            return stop_traffic_data

        except vnf_api_comm.VnfCommException as exception:
            failure_reson = (
                "Failed to stop traffic in From-VFN (ignored). "
                "Reported reason: %s" % (exception))
            LOG.info("%s" % (failure_reson))
            self.add_infos.append(failure_reson)
            return None

    def __rollback_at_restore(self, add_info = list()):
        """ Roll back ongoing restore at restore of job (R-VNFM has been restarted) """
        LOG.debug("__rollback_at_restore()")
        self.__unsubscribe_for_lcop(self.__get_restore_job().resource.subscription_url_id)
        self.__heal_from_vnf_if_needed("A restart of R-VNFM was done while a restore was "
                                       "in progress. The restore is rolled back and hence "
                                       "From-VNF needs to be healed since it has been "
                                       "stopped (traffic have been stopped). Job id: %s,"
                                       " From-VNF: %s"
                                       % (self.__get_restore_job().get_job_id(),
                                          self.__get_restore_job().get_vnf_id()),
                                       self.__get_restore_job().resource.to_vnf_id)
        self.__terminate_vnf(self.__get_restore_job().resource.to_vnf_id, "To-")
        self.__remove_vnf_id(self.__get_restore_job().resource.to_vnf_id, "To-")
        self.add_infos.append("A restart of R-VNFM has occured while operation '%s' was "
                              "in progress i.e. restore is rolled back." %
                              (self.__get_restore_job().resource.operation))
        self.add_infos.extend(add_info)
        self.__restore_aborted(self.add_infos)
        self.__set_operation_result(
            self.__get_restore_job().resource.operation,
            self.__get_restore_job().resource.fallback_timeout,
            lcm_upgrade_job.get_restore_op_result_enum().FAILURE,
            self.add_infos)
        self.__set_progress(lcm_upgrade_job.get_restore_progress_enum().IDLE)
        self.__finish(self.__get_restore_job().resource.previous_state, False)
        self.__set_restore_job(None)
        self.is_cancel_ongoing = False

    def __vnf_api_comm_from(self):
        """ returns a handle to vnf api comm towards From-VNF """
        if self.vnf_comm_handle_from == None:
            self.vnf_comm_handle_from = self.vnf_comm_handle_from = vnf_api_comm.get_vnf_comm(
                self.__get_restore_job().get_vnf_id(),
                self.vnf_port,
                self.vnfm_url)
        return self.vnf_comm_handle_from

    def _ensure_no_upgrade_is_ongoing(self, vnf_id):
        """
        Ensure that any ongoing upgrade for this VNF is aborted to enable
        restore to be executed
        """
        LOG.debug("_ensure_no_upgrade_is_ongoing(self, vnf_id) - vnf_id: %s"
                  % (vnf_id))
        upgrade_job = lcm_wfm.get_lcm_wfm().get_upgrade_job_for_vnf(vnf_id)
        if upgrade_job != None:
            if upgrade_job.state == lcm_upgrade_job.get_state_enum().PREPARE_IN_PROGRESS:
                self._abort_operation(upgrade_job,
                                     lcm_upgrade_job.get_state_enum().PREPARE_IN_PROGRESS)

            elif upgrade_job.state == lcm_upgrade_job.get_state_enum().VERIFY_IN_PROGRESS:
                self._abort_operation(upgrade_job,
                                     lcm_upgrade_job.get_state_enum().VERIFY_IN_PROGRESS)

            elif upgrade_job.state == lcm_upgrade_job.get_state_enum().ACTIVATION_IN_PROGRESS:
                if upgrade_job.to_vnf_id == vnf_id:
                    raise lcm_wf_utility.RestoreFailedException(
                        lcm_util.LcmException.NOT_ALLOWED,
                        "The VNF from where Restore is requested is involved in upgrade where state "
                        "is ACTIVATION_IN_PROGRESS. The restore is requested from To-VNF and "
                        "rejected due to that. Can't cancel the ongoing upgrade since the To-VNF "
                        "will be terminated from where also the restore "
                        "is requested in that case.",
                        lcm_wf_utility.RestoreFailedException.ONGOING_UPGRADE)

                else:
                    self._abort_operation(upgrade_job,
                                         lcm_upgrade_job.get_state_enum().ACTIVATION_IN_PROGRESS)

            elif upgrade_job.state == lcm_upgrade_job.get_state_enum().WAITING_FOR_CONFIRM:
                raise lcm_wf_utility.RestoreFailedException(
                    lcm_util.LcmException.NOT_ALLOWED,
                    "The VNF from where Restore is requested is involved in upgrade where state "
                    "is WAITING_FOR_CONFIRM.",
                    lcm_wf_utility.RestoreFailedException.ONGOING_UPGRADE)

            elif upgrade_job.state == lcm_upgrade_job.get_state_enum().CONFIRM_IN_PROGRESS:
                LOG.debug("_ensure_no_upgrade_is_ongoing(self, vnf_id) - State is confirm in progress)")
                self._wait_for_confirm_to_finish(upgrade_job)

        LOG.debug("No upgrade is ongoing for VNF: %s, upgrade_job: %s" % (vnf_id, upgrade_job))

    def _abort_operation(self, upgrade_job, current_state):
        """ Abort a specific ongoing operation """
        LOG.debug("_abort_operation(self, upgrade_job, current_state) - upgrade_job: %s,"
                  " current_state: %s" % (upgrade_job, current_state))
        self._invoke_cancel(upgrade_job.job_id)
        while upgrade_job.state == current_state:
            LOG.debug("_abort_operation(...) - Waiting (2 s) for upgrade job enters a non"
                      " working state. Job id: %s, state: %s" %
                      (upgrade_job.job_id, upgrade_job.state))
            time.sleep(2)
        LOG.debug("Upgrade job after wait. Job id: %s, state: %s" %
                  (upgrade_job.job_id, upgrade_job.state))

    def _wait_for_confirm_to_finish(self, upgrade_job):
        """ Wait for confirm to comlpete or rolled back """
        LOG.debug("_wait_for_confirm_to_finish(self, upgrade_job) -"
                  " upgrade_job: %s" % (upgrade_job))

        while (upgrade_job.state == lcm_upgrade_job.get_state_enum().CONFIRM_IN_PROGRESS):
            LOG.debug("_wait_for_confirm_to_finish(...) - Waiting (4 s) for upgrade job "
                      "enters a non working state. Job id: %s, state: %s" %
                      (upgrade_job.job_id, upgrade_job.state))
            time.sleep(4)
            LOG.debug("Upgrade job after wait. Job id: %s, state: %s" %
                      (upgrade_job.job_id, upgrade_job.state))

    def _invoke_cancel(self, job_id):
        """ Invokes cancel for an upgrade job that are in progress """
        LOG.debug("_invoke_cancel(self, job_id)")
        try:
            response = lcm_wfm.get_lcm_wfm().cancel(job_id, {})
            LOG.debug("Response from cancel of ongoing upgrade job: %s"
                      % (response))
        except Exception as exception:
            LOG.info("_invoke_cancel(self, job_id) - Failed to invoke cancel"
                     " for upgrade job id %s (ignored). Reason: %s"
                     % (job_id, exception))

    def __start_traffic(self, stop_traffic_data):
        """ Used to start traffic in To-VNF """
        LOG.debug("__start_traffic(self, stop_traffic_data) - stop_traffic_data: %s"
                  % (stop_traffic_data))
        try:
            self.__get_vnf_comm_to().start_traffic_v2(stop_traffic_data)

        except Exception as exception:
            message = "Failure reason: %s" % (exception)
            LOG.info("Failed to start traffic in To-VNF. %s" % (message))
            raise lcm_wf_utility.RestoreFailedException(
                lcm_util.LcmException.INTERNAL_ERROR,
                message,
                lcm_wf_utility.RestoreFailedException.START_TRAFFIC_TO_VNF_FAILURE)
