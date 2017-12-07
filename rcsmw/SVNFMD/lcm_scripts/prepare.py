#!/usr/bin/env python

##
# (C) Ericsson AB 2016-2017 All rights reserved.
# The information in this document is the property of Ericsson. Except
# as specifically authorized in writing by Ericsson, the receiver of
# this document shall keep the information contained herein confidential
# and shall protect the same in whole or in part from disclosure and
# dissemination to third parties. Disclosure and disseminations to the
# receivers employees shall only be made on a strict need to know basis.
##

"""
Lifecycle Management Script for upgrade prepare operation.
"""

#pylint: disable=W0703
#pylint: disable=R0201
#pylint: disable=R0913

# Temporary added due to old pylint version in Jenkins
# pylint used in HUB (newer version) do not print out this faulty
# warnings/ errors
#pylint: disable=E1002
#pylint: disable=E1101
#pylint: disable=W0201
#pylint: disable=E0203
#pylint: disable=C0103
#pylint: disable=R0904
#pylint: disable=R0902

from lcm import (lcm_util,
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

import logging
import time
import threading

LOG = logging.getLogger('PREPARE')

def main(job_id, operation_data, upgrade_job, lcm_config, callback_when_finished):
    """ Main function for prepare to initiate the prepare request """
    LOG.debug("main(job_id, operation_data, upgrade_job, lcm_config, "
              "callback_when_finished) - job_id: %s, operation_data: %s, "
              "upgrade_job: %s, lcm_config: %s, callback_when_finished: %s"
              % (job_id, operation_data, upgrade_job, lcm_config, callback_when_finished))
    if (upgrade_job.get_state() == lcm_upgrade_job.get_state_enum().INITIALIZED):
        prepare = Prepare(job_id, upgrade_job, lcm_config, callback_when_finished)
        prepare.start()
        prepare.main(operation_data)
        LOG.debug("Prepare initiated for JOB_ID: %s" % (job_id))
        return prepare
    else:
        raise lcm_util.LcmException(
            lcm_util.LcmException.NOT_ALLOWED,
            "Attempt to execute operation prepare for an upgrade job (job_id: %s "
            "when not expected. Current state: %s, Current workflow: %s"
            % (job_id, upgrade_job.state, upgrade_job.current_workflow))

class Prepare(lcm_wf_utility.GenAsyncQueue):
    """ Prepare workflow script """

    CURRENT_VERSION = lcm_util.EnumVersion.VER_1_0

    def __init__(self, job_id, upgrade_job, lcm_config, callback_when_finished):
        """ Constructor """
        super(Prepare, self).__init__(job_id,
                                      upgrade_job,
                                      lcm_config,
                                      callback_when_finished,
                                      'Prepare_' + str(job_id),
                                      LOG,
                                      Prepare.CURRENT_VERSION)
        self.timeout_expired = False
        self.cancel_requested = False
        self.previous_state = None
        self.requested_time = lcm_util.get_lcm_log_handler().get_lcm_timestamp()
        self.progress_wf = lcm_wf_utility.EnumProgressWf(LOG)
        self.result_wf = lcm_wf_utility.EnumOperationResultWf(LOG)
        self.add_info_trans = lcm_wf_utility.AddInfoTransformer()
        self.fallback_timer = None
        self.operation_name = None
        self.operation_data = None

    def main(self, *operation_data):
        """ Main method for script prepare """
        LOG.debug('main(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Prepare Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.put((self.function(), operation_data))

    def cancel(self, *operation_data):
        """ Cancel requested """
        LOG.debug('cancel(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Prepare Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))
        self.cancel_requested = True
        self.put((self.function(), operation_data))

    #
    # Prepare thread loop - Aynchronous operations
    #
    def handle_main(self, operation_data):
        """
        Script entry point for the asynchronous lcm operation prepare.
        """
        LOG.debug("handle_main(self, operation_data) - "
                  "operation_data: %s" % (operation_data))
        try:
            self.operation_name = lcm_upgrade_job.get_operation_enum().PREPARE
            LOG.info("LCM Workflow Prepare Version: %s, LCM Framework Version: %s (job id: %s)"
                     % (lcm_wf_utility.get_lcm_workflow_version(),
                        lcm_util.get_lcm_fw_version(),
                        self.job_id))
            self.previous_state = self.upgrade_job.get_state()
            self.__set_state(lcm_upgrade_job.get_state_enum().PREPARE_IN_PROGRESS)
            self.__set_progress(
                10,
                self.progress_wf.get_enum(self.progress_wf.PREPARING))
            self.operation_data = operation_data
            self.fallback_timer = threading.Timer(self.get_fallback_timeout(operation_data),
                                                  self.timeout)
            self.fallback_timer.start()
            LOG.debug("Prepare Fallback timer started: %s" % (self.fallback_timer))

            #For CT purposes, delay workflow execution as specified in ct_lcm.cfg file
            if self.__is_wf_to_be_delayed():
                return True

            # Check if operation was cancelled or timed out ...
            self.__is_cancel_requested()
            self.__is_timeout()
            # ...otherwise report success and finish
            self.__add_operation_result(
                self.operation_name,
                self.requested_time,
                lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                self.get_fallback_timeout(operation_data),
                self.result_wf.get_enum(self.result_wf.SUCCESS),
                self.add_info_trans.get_add_info(list()))
            self.__set_progress(
                100,
                self.progress_wf.get_enum(self.progress_wf.PREPARING))
            self.handle_cleanup(lcm_upgrade_job.get_state_enum().PREPARE_COMPLETED)
            # Make WF thread exit execution
            return False

        except lcm_wf_utility.CancelWorkflowException as exception:
            self.handle_cancel(operation_data)
            return False

        except lcm_wf_utility.WorkflowTmoException as exception:
            self.handle_timeout(operation_data)
            return False

        except lcm_util.LcmException as exception:
            LOG.debug("Prepare of the UP with JOB ID (%s) aborted due to %s"
                      % (self.job_id, exception.get_error_message()))
            self.handle_error(exception.get_error_message(), operation_data)
            return False

        except Exception as exception:
            LOG.error("Prepare of the UP with JOB ID (%s) aborted due to '%s'"
                      % (self.job_id, exception))
            LOG.exception(exception)
            self.handle_error(str(exception), operation_data)
            return False

    def handle_cleanup(self, state):
        """
        Script entry point for the case where an ongoing job is finished and
        needs to be cleaned up
        """
        LOG.debug('handle_cleanup(self, state) - state: %s' % (state))
        self.__set_progress(
            0,
            self.progress_wf.get_enum(self.progress_wf.IDLE))
        if self.fallback_timer != None:
            self.fallback_timer.cancel()
        self.__finish(state)
        self.upgrade_job = None
        self.previous_state = None
        self.operation_name = None
        self.fallback_timer = None

    def handle_cancel(self, operation_data):
        """
        Script entry point for work flow cancellation.
        """
        LOG.debug('handle_cancel(self, _operation_data).')
        try:
            self.__set_progress(
                92,
                self.progress_wf.get_enum(self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED))
            self.__add_operation_result(
                self.operation_name,
                self.requested_time,
                lcm_util.get_lcm_log_handler().get_lcm_timestamp(),
                self.get_fallback_timeout(operation_data),
                self.result_wf.get_enum(self.result_wf.CANCELLED),
                self.add_info_trans.get_add_info(['Operation cancelled']))
            self.__set_progress(
                100,
                self.progress_wf.get_enum(self.progress_wf.CLEANUP_DUE_TO_CANCEL_REQUEST))
            self.handle_cleanup(self.previous_state)

        except Exception as exception:
            LOG.error("handle_cancel(self, reason) - reason: %s. Failed to do "
                      "the complete handle cancel logic i.e. will anyhow ensure"
                      " that state is handled as it should")
            LOG.exception(exception)
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
        self.handle_cleanup(self.previous_state)
        return False

    def handle_error(self, reason, operation_data):
        """
        Workflow entry point for the case where an unexpected problem occured
        """
        LOG.debug('handle_error(self, reason, operation_data)'
                  ' - reason: %s, operation_data: %s'
                  % (reason, operation_data))

        try:
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

        except Exception as exception:
            LOG.error("handle_error(self, reason) - reason: %s. Failed to do "
                      "the complete handle error logic i.e. will anyhow ensure"
                      " that state is handled as it should")
            LOG.exception(exception)
            self.handle_cleanup(self.previous_state)

    def __is_cancel_requested(self):
        """ Check if cancel is requested and act on it """
        if self.cancel_requested:
            LOG.debug("Operation %s is aborted since operation "
                      "Cancel has been received"
                      % (self.operation_name))
            self.__set_progress(
                10,
                self.progress_wf.get_enum(self.progress_wf.CANCEL_ONGOING_OPERATION_REQUESTED))
            raise lcm_wf_utility.CancelWorkflowException("Operation cancelled")

    def timeout(self):
        """ Callback to be executed when fallback timer expires """
        if self.upgrade_job == None:
            LOG.warning("A fallback timeout occured %s, but the workflow is already finished." %
                        (self.fallback_timer))
            return
        LOG.debug('Prepare workflow execution time exceeded the fallback timeout: %s seconds.'
                  % (self.get_fallback_timeout(self.operation_data)))
        self.timeout_expired = True

        self.put((self.function(), (self.operation_data, )))

    def __is_timeout(self):
        """ Check if timeout expired and act on it """
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
            if 'delay_in_prepare' in self.lcm_config.get_lcm_test_data():
                req_delay = self.lcm_config.get_lcm_test_data()['delay_in_prepare']
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
