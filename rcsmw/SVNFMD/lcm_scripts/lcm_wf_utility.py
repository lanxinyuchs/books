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
LCM utility functions and classes
"""
#pylint: disable=R0201
#pylint: disable=R0904
#pylint: disable=R0911
#pylint: disable=R0912
#pylint: disable=R0913
#pylint: disable=R0903
#pylint: disable=W0703

from lcm import (lcm_util,
                 lcm_upgrade_job,
                 lcm_wfm)
import abc
import threading
import Queue
import traceback

SINGLETONS = {'lcm_workflow_version': None, 'lcm_wfm_proxy': None}

def get_lcm_workflow_version():
    """ Returns LCM Workflow version  """
    if SINGLETONS['lcm_workflow_version'] == None:
        SINGLETONS['lcm_workflow_version'] = LcmWorkflowVersion()

    return SINGLETONS['lcm_workflow_version']

def get_lcm_wfm_proxy():
    """ Returns a handle to the LcmWfmProxy singleton  """
    if SINGLETONS['lcm_wfm_proxy'] == None:
        SINGLETONS['lcm_wfm_proxy'] = LcmWfmProxy()

    return SINGLETONS['lcm_wfm_proxy']


# ==========================================
# ==========================================
# LCM Framework overall version
# ==========================================
# ==========================================

# NOTE: Do NOT instantiate this class outside this module
#       Please, use public get_lcm_fw_version method instead above
class LcmWorkflowVersion(lcm_util.LcmVersion):
    """
    Holds info of overall LCM framework version
    This version is just for human to easily see version of the LCM Framework
    NOTE: This version NOT used by and source code to decide how to act
    """

    DATE = '171124'
    BRANCH_NAME = 'dev'
    SEQUENCE_NO = 'X'
    LCM_VERSION = '1'
    VER_DESCR = 'fix-race-condition'

    # This may be used for version handling of this class. For now
    # it is not needed but added in case...
    CURRENT_VERSION = lcm_util.EnumVersion.VER_1_0

    def __init__(self, current_version = CURRENT_VERSION):
        """ Constructor """
        super(LcmWorkflowVersion, self).__init__(current_version)

    def __str__(self):
        """ To string method """
        return "%s:%s:%s - %s: %s" % (LcmWorkflowVersion.DATE,
                                      LcmWorkflowVersion.BRANCH_NAME,
                                      LcmWorkflowVersion.LCM_VERSION,
                                      LcmWorkflowVersion.SEQUENCE_NO,
                                      LcmWorkflowVersion.VER_DESCR)


# ==========================================
# ==========================================
# LCM WFM Proxy
# ==========================================
# ==========================================

# NOTE: Do NOT instantiate this class outside this module
#       Please, use public get_lcm_fw_version method instead above
class LcmWfmProxy(object):
    """
    Converts calls from a workflow to lcm_wfm, to appropriate calls
    depending on the lcm_wfm version currently running
    """

    def __init__(self):
        """ Constructor """
        pass

    def get_current_version(self):
        """ Adapt to the implementation of the lcm_wfm, which is currently being used """
        try:
            return lcm_wfm.get_lcm_wfm().get_current_version()
        except AttributeError:
            return lcm_wfm.get_lcm_wfm_version().get_current_version()

    def is_supported(self, version):
        """ Adapt to the implementation of the lcm_wfm, which is currently being used """
        try:
            return lcm_wfm.get_lcm_wfm().is_supported(version)
        except AttributeError:
            return lcm_wfm.get_lcm_wfm_version().is_supported(version)





#=================================
#=================================
#=================================
# LCM Exceptions of different kind
#=================================
#=================================
#=================================

#
# NOTE: Not allowed to change this in such way that it is not backward
#       compatible
class WorkflowException(lcm_util.LcmException):
    """ Exception raised by workflows """

    def __init__(self, description):
        """ Constructor """
        super(WorkflowException, self).__init__(lcm_util.LcmException.INTERNAL_ERROR,
                                                description)

#
# NOTE: Not allowed to change this in such way that it is not backward
#       compatible
class VerifyWorkflowException(WorkflowException):
    """ Exception raised by workflows """

    def __init__(self, description, add_infos):
        """ Constructor """
        super(VerifyWorkflowException, self).__init__("%s - %s" % (description, add_infos))
        self.add_infos = add_infos

#
# NOTE: Not allowed to change this in such way that it is not backward
#       compatible
class CancelWorkflowException(WorkflowException):
    """ Exception raised by workflows when cancelled """

    def __init__(self, description):
        """ Constructor """
        super(CancelWorkflowException, self).__init__(description)

#
# NOTE: Not allowed to change this in such way that it is not backward
#       compatible
class WorkflowTmoException(WorkflowException):
    """ Exception raised when fallback timer expires """

    def __init__(self, description):
        """ Constructor """
        super(WorkflowTmoException, self).__init__(description)

#
# NOTE: Not allowed to change this in such way that it is not backward
#       compatible
class RestoreFailedException(lcm_util.LcmException):
    """ Exception raised by workflows """
    FALLBACK_TIMEOUT = "The action was aborted due to fallback timeout"
    CANCELLED = "The action was cancelled by the user"
    INSTANTIATION_FAILURE_TO_VNF = "The action was aborted due to failure at instantiation of To-VNF"
    ONGOING_UPGRADE = "The action was aborted due to an ongoing upgrade prevents restore"
    SHUTDOWN_FROM_VNF_FAILURE = "The action was aborted due to failure at shutdown of From-VNF"
    START_TRAFFIC_TO_VNF_FAILURE = "The action was aborted due to failure at start of traffic for To-VNF"
    POST_CHECK_TIMEOUT = "The action was aborted due to post check timeout"
    POST_CHECK_FATAL_ERROR = "The action was aborted due to post check fatal error"
    TO_VNF_RESTARTED = "The action was aborted due to To-VNF restarted"
    TO_VNF_SHUTDOWNED = "The action was aborted due to To-VNF was shutdowned"
    TO_VNF_TERMINATED = "The action was aborted due to To-VNF terminated"

    def __init__(self, error_code, description, reason_info):
        """ Constructor """
        super(RestoreFailedException, self).__init__(lcm_util.LcmException.INTERNAL_ERROR,
                                                     description)
        self.reason_info = reason_info


# =============================================
# =============================================
# =============================================
# Asynchronous support class for e.g. workflows
# =============================================
# =============================================
# =============================================
class GenAsync(threading.Thread):
    """ Generic base class for handling of asynchronous operations """
    __metaclass__ = abc.ABCMeta

    def __init__(self, job_id, operation_data, upgrade_job,
                 lcm_config, callback_when_finished, thread_name, log_handle, version):
        """ Constructor """
        super(GenAsync, self).__init__()
        self.log = log_handle
        self.version = lcm_util.LcmVersion(version)
        self.name = thread_name
        self.job_id = job_id
        self.operation_data = operation_data
        self.upgrade_job = upgrade_job
        self.lcm_config = lcm_config
        self.callback_when_finished = callback_when_finished

    def run(self):
        """ Thread run method """
        try:
            self.log.debug('Invokes the main() method in workflow (thread) %s' % (self.name))
            self.handle_main()
            self.log.debug('Exits the workflow (thread) %s' % (self.name))

        except Exception as exception:
            self.log.error("A workflow script ended abnormaly i.e. failed due to " +
                           str(exception))
            self.log.exception(exception)
            try:
                self.handle_error(str(exception))
            except Exception as exception:
                self.log.warning("When trying to perform a graceful termination of "
                                 "actual workflow an unexpected problem occured."
                                 "This is ignored since nothing more can be done i.e."
                                 " will end the execution of this workflow (%s)" % (self))
                self.log.exception(exception)

        self.log.debug('Exiting from thread %s' % (self.name))
        return

    def get_name(self):
        """ Returns the name of the sub (child) class """
        return self.__class__.__name__

    def get_fallback_timeout(self, operation_data):
        """ Returns the fallback timeout value """
        if 'fallback_timeout' in operation_data:
            return operation_data['fallback_timeout']
        else:
            return self.upgrade_job.get_fallback_timeout()

    def is_supported(self, version):
        ''' Returns whether or not given version is supported '''
        return self.version.is_supported(version)

    def get_current_version(self):
        ''' Returns current version '''
        return self.version.version

    @abc.abstractmethod
    def handle_main(self):
        """ Handle main method - Needs to be overridden """
        return

    @abc.abstractmethod
    def handle_error(self, __reason):
        """ Handle error method - Needs to be overridden """
        return

# ========================================================
# ========================================================
# ========================================================
# Asynchronous support class with queue for e.g. workflows
# ========================================================
# ========================================================
# ========================================================
class GenAsyncQueue(threading.Thread):
    """ Generic base class for handling of asynchronous operations using queue """
    __metaclass__ = abc.ABCMeta


    def __init__(self, job_id, upgrade_job, lcm_config, callback_when_finished,
                 thread_name, log_handle, version):
        """ Constructor """
        super(GenAsyncQueue, self).__init__()
        self.log = log_handle
        self.version = lcm_util.LcmVersion(version)
        self.name = thread_name
        self.job_id = job_id
        self.upgrade_job = upgrade_job
        self.lcm_config = lcm_config
        self.callback_when_finished = callback_when_finished
        self.cont = True
        # FIFO queue
        self.queue = Queue.Queue(100)

    def run(self):
        """ Thread run method """
        operation_data = None
        try:
            while self.cont:
                self.log.debug('Retreives (waiting) for event to be put in queue')
                (function, operation_data) = self.queue.get()
                self.log.debug('Got event ' + str(operation_data) + '\t' + str(self.queue.qsize()) +
                               ' no of events in queue')
                self.log.debug("Function: %s" % (str(function)))
                func = getattr(self, 'handle_' + function)
                self.cont = func(*operation_data)
                self.log.debug("self.cont: %s" % (self.cont))

        except Exception as exception:
            self.log.error("A workflow script ended abnormaly. Exception caught with reason %s. "
                           "Upgrade job data: %s" % (exception, self.upgrade_job))
            self.log.exception(exception)
            try:
                self.handle_error(str(exception), operation_data)
            except Exception as exception:
                self.log.warning("When trying to perform a graceful termination of "
                                 "actual workflow an unexpected problem occured."
                                 "This is ignored since nothing more can be done i.e."
                                 " will end the execution of this workflow (%s)" % (self))
                self.log.exception(exception)
        self.log.debug('Exiting from this thread')
        return

    def put(self, operation_data):
        """ Used to put incoming operation into thread queue """
        self.queue.put(operation_data)
        self.log.debug('Added job to queue ' + str(operation_data) + '\t' + str(self.queue.qsize()) +
                       ' no of events in queue')

    def function(self):
        """ Used to get the name of calling function """
        return traceback.extract_stack(None, 2)[0][2]

    def get_name(self):
        """ Returns the name of the sub (child) class """
        return self.__class__.__name__

    def get_fallback_timeout(self, operation_data):
        """ Returns the fallback timeout value """
        if 'fallback_timeout' in operation_data:
            return operation_data['fallback_timeout']
        else:
            return self.upgrade_job.get_fallback_timeout()

    def is_supported(self, version):
        ''' Returns whether or not given version is supported '''
        return self.version.is_supported(version)

    def get_current_version(self):
        ''' Returns current version '''
        return self.version.version

    @abc.abstractmethod
    def main(self, __operation_data):
        """ Main method for scripts """
        return

    @abc.abstractmethod
    def cancel(self, __operation_data):
        """ Cancel requested """
        return

    @abc.abstractmethod
    def handle_main(self, __operation_data):
        """ Main method in thread scope - Needs to be overridden """
        return

    @abc.abstractmethod
    def handle_cancel(self, __operation_data):
        """ Cancel method - Needs to be overridden """
        return

    @abc.abstractmethod
    def handle_timeout(self, __operation_data):
        """ Fallback timeout method - Needs to be overridden """
        return

    @abc.abstractmethod
    def handle_error(self, __reason, __operation_data):
        """ Error occured method in thread scope - Needs to be overridden """
        return


class EnumProgressWf(object):
    """ Progress values - Version 2 to Version 1 handling"""
    # Progress values version 2
    IDLE = "IDLE"
    PREPARING = "PREPARING"
    PRE_CHECK = "PRE_CHECK"
    STOP_CONFIGURATION_FOR_FROM_STATE_VNF = "STOP_CONFIGURATION_FOR_FROM_STATE_VNF"
    CREATING_BACKUP_FOR_FROM_STATE_VNF = "CREATING_BACKUP_FOR_FROM_STATE_VNF"
    EXPORTING_BACKUP_FOR_FROM_STATE_VNF = "EXPORTING_BACKUP_FOR_FROM_STATE_VNF"
    CREATING_TO_STATE_VNF = "CREATING_TO_STATE_VNF"
    WAITING_FOR_CS_READY = "WAITING_FOR_CS_READY"
    WAITING_FOR_VNF_APPLICATION_READY = "WAITING_FOR_VNF_APPLICATION_READY"
    STOP_TRAFFIC_FOR_FROM_STATE_VNF = "STOP_TRAFFIC_FOR_FROM_STATE_VNF"
    START_TRAFFIC_FOR_TO_STATE_VNF = "START_TRAFFIC_FOR_TO_STATE_VNF"
    STOP_EXECUTION_OF_FROM_STATE_VNF = "STOP_EXECUTION_OF_FROM_STATE_VNF"
    POST_CHECK = "POST_CHECK"
    WAITING_FOR_CONFIRMATION = "WAITING_FOR_CONFIRMATION"
    CONFIRMING_TO_STATE_VNF = "CONFIRMING_TO_STATE_VNF"
    TERMINATING_FROM_STATE_VNF = "TERMINATING_FROM_STATE_VNF"
    TERMINATING_TO_STATE_VNF = "TERMINATING_TO_STATE_VNF"
    STOP_EXECUTION_OF_TO_STATE_VNF = "STOP_EXECUTION_OF_TO_STATE_VNF"
    START_CONFIGURATION_FOR_FROM_STATE_VNF = "START_CONFIGURATION_FOR_FROM_STATE_VNF"
    START_EXECUTION_OF_FROM_STATE_VNF = "START_EXECUTION_OF_FROM_STATE_VNF"
    CANCEL_ONGOING_OPERATION_REQUESTED = ""
    FALLBACK_TIMER_EXPIRED = "FALLBACK_TIMER_EXPIRED"
    CLEANUP_DUE_TO_CANCEL_REQUEST = "CLEANUP_DUE_TO_CANCEL_REQUEST"
    CLEANUP_DUE_TO_TIMEOUT = "CLEANUP_DUE_TO_TIMEOUT"
    CLEANUP_DUE_TO_FAILURE = "CLEANUP_DUE_TO_FAILURE"


    def __init__(self, log):
        """ Constructor """
        self.log = log

    def get_enum(self, progress_value_wf):
        """ Used to set proper progress value depenending on version in LCM FW """
        if lcm_upgrade_job.get_progress_enum().is_supported(lcm_util.EnumVersion.VER_2_0):
            return self.get_enum_v2(progress_value_wf)

        else:
            return self.get_enum_v1(progress_value_wf)

    def get_enum_v2(self, progress_value_wf):
        """ Returns the progress value acc. to version 2 """
        if progress_value_wf == self.IDLE:
            return lcm_upgrade_job.get_progress_enum().IDLE
        elif progress_value_wf == self.PREPARING:
            return lcm_upgrade_job.get_progress_enum().PREPARING
        elif progress_value_wf == self.PRE_CHECK:
            return lcm_upgrade_job.get_progress_enum().PRE_CHECK
        elif progress_value_wf == self.STOP_CONFIGURATION_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().STOP_CONFIGURATION_FOR_FROM_STATE_VNF
        elif progress_value_wf == self.CREATING_BACKUP_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().CREATING_BACKUP_FOR_FROM_STATE_VNF
        elif progress_value_wf == self.EXPORTING_BACKUP_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().EXPORTING_BACKUP_FOR_FROM_STATE_VNF
        elif progress_value_wf == self.CREATING_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().CREATING_TO_STATE_VNF
        elif progress_value_wf == self.WAITING_FOR_CS_READY:
            return lcm_upgrade_job.get_progress_enum().WAITING_FOR_CS_READY
        elif progress_value_wf == self.WAITING_FOR_VNF_APPLICATION_READY:
            return lcm_upgrade_job.get_progress_enum().WAITING_FOR_VNF_APPLICATION_READY
        elif progress_value_wf == self.STOP_TRAFFIC_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().STOP_TRAFFIC_FOR_FROM_STATE_VNF
        elif progress_value_wf == self.START_TRAFFIC_FOR_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().START_TRAFFIC_FOR_TO_STATE_VNF
        elif progress_value_wf == self.STOP_EXECUTION_OF_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().STOP_EXECUTION_OF_FROM_STATE_VNF
        elif progress_value_wf == self.POST_CHECK:
            return lcm_upgrade_job.get_progress_enum().POST_CHECK
        elif progress_value_wf == self.WAITING_FOR_CONFIRMATION:
            return lcm_upgrade_job.get_progress_enum().WAITING_FOR_CONFIRMATION
        elif progress_value_wf == self.CONFIRMING_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().CONFIRMING_TO_STATE_VNF
        elif progress_value_wf == self.TERMINATING_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().TERMINATING_FROM_STATE_VNF
        elif progress_value_wf == self.TERMINATING_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().TERMINATING_TO_STATE_VNF
        elif progress_value_wf == self.STOP_EXECUTION_OF_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().STOP_EXECUTION_OF_TO_STATE_VNF
        elif progress_value_wf == self.START_CONFIGURATION_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().START_CONFIGURATION_FOR_FROM_STATE_VNF
        elif progress_value_wf == self.START_EXECUTION_OF_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().START_EXECUTION_OF_FROM_STATE_VNF
        elif progress_value_wf == self.CANCEL_ONGOING_OPERATION_REQUESTED:
            return lcm_upgrade_job.get_progress_enum().CANCEL_ONGOING_OPERATION_REQUESTED
        elif progress_value_wf == self.FALLBACK_TIMER_EXPIRED:
            return lcm_upgrade_job.get_progress_enum().FALLBACK_TIMER_EXPIRED
        elif progress_value_wf == self.CLEANUP_DUE_TO_CANCEL_REQUEST:
            return lcm_upgrade_job.get_progress_enum().CLEANUP_DUE_TO_CANCEL_REQUEST
        elif progress_value_wf == self.CLEANUP_DUE_TO_TIMEOUT:
            return lcm_upgrade_job.get_progress_enum().CLEANUP_DUE_TO_TIMEOUT
        elif progress_value_wf == self.CLEANUP_DUE_TO_FAILURE:
            return lcm_upgrade_job.get_progress_enum().CLEANUP_DUE_TO_FAILURE
        else:
            self.log.error("get_enum_v2(self, progress_value_wf) - progress_value_wf: %s"
                           " Not supported value recieved i.e. returns POST_CHECK"
                           % (progress_value_wf))
            return lcm_upgrade_job.get_progress_enum().POST_CHECK

    def get_enum_v1(self, progress_value_wf):
        """ Returns the progress value acc. to version 1 """
        if progress_value_wf == self.IDLE:
            return lcm_upgrade_job.get_progress_enum().IDLE
        elif progress_value_wf == self.PREPARING:
            return lcm_upgrade_job.get_progress_enum().IDLE
        elif progress_value_wf == self.PRE_CHECK:
            return lcm_upgrade_job.get_progress_enum().VERIFYING
        elif progress_value_wf == self.STOP_CONFIGURATION_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        elif progress_value_wf == self.CREATING_BACKUP_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        elif progress_value_wf == self.EXPORTING_BACKUP_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        elif progress_value_wf == self.CREATING_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().CREATING_TO_STATE_VNF
        elif progress_value_wf == self.WAITING_FOR_CS_READY:
            return lcm_upgrade_job.get_progress_enum().CREATING_TO_STATE_VNF
        elif progress_value_wf == self.WAITING_FOR_VNF_APPLICATION_READY:
            return lcm_upgrade_job.get_progress_enum().IMPORTING_CONFIGURATION_TO_STATE_VNF
        elif progress_value_wf == self.STOP_TRAFFIC_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().IMPORTING_CONFIGURATION_TO_STATE_VNF
        elif progress_value_wf == self.START_TRAFFIC_FOR_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().IMPORTING_CONFIGURATION_TO_STATE_VNF
        elif progress_value_wf == self.STOP_EXECUTION_OF_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().IMPORTING_CONFIGURATION_TO_STATE_VNF
        elif progress_value_wf == self.POST_CHECK:
            return lcm_upgrade_job.get_progress_enum().IMPORTING_CONFIGURATION_TO_STATE_VNF
        elif progress_value_wf == self.WAITING_FOR_CONFIRMATION:
            return lcm_upgrade_job.get_progress_enum().WAITS_FOR_CONFIRMATION
        elif progress_value_wf == self.CONFIRMING_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().DELETE_OF_FROM_STATE_VNF
        elif progress_value_wf == self.TERMINATING_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().DELETE_OF_FROM_STATE_VNF
        elif progress_value_wf == self.TERMINATING_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().IMPORTING_CONFIGURATION_TO_STATE_VNF
        elif progress_value_wf == self.STOP_EXECUTION_OF_TO_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().IMPORTING_CONFIGURATION_TO_STATE_VNF
        elif progress_value_wf == self.START_CONFIGURATION_FOR_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        elif progress_value_wf == self.START_EXECUTION_OF_FROM_STATE_VNF:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        elif progress_value_wf == self.CANCEL_ONGOING_OPERATION_REQUESTED:
            return lcm_upgrade_job.get_progress_enum().CANCEL_ONGOING_OPERATION_REQUESTED
        elif progress_value_wf == self.FALLBACK_TIMER_EXPIRED:
            return lcm_upgrade_job.get_progress_enum().CANCEL_ONGOING_OPERATION_REQUESTED
        elif progress_value_wf == self.CLEANUP_DUE_TO_CANCEL_REQUEST:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        elif progress_value_wf == self.CLEANUP_DUE_TO_TIMEOUT:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        elif progress_value_wf == self.CLEANUP_DUE_TO_FAILURE:
            return lcm_upgrade_job.get_progress_enum().SAVING_CONFIGURATION_FROM_STATE_VNF
        else:
            self.log.error("get_enum_v2(self, progress_value_wf) - progress_value_wf: %s"
                           " Not supported value recieved i.e. returns POST_CHECK"
                           % (progress_value_wf))
            return lcm_upgrade_job.get_progress_enum().POST_CHECK


class AddInfoTransformer(object):
    """ Additional information transformer """

    def __init__(self):
        """ Constructor """
        pass

    def get_add_info(self, add_info):
        """ Returns correct format for additional information """
        if lcm_upgrade_job.get_operation_result_enum().is_supported(lcm_util.EnumVersion.VER_2_0):
            return add_info
        else:
            return " *** ".join(add_info)


class EnumOperationResultWf(object):
    """ Operation result values - Version 2 to Version 1 handling"""
    # Result values version 2
    SUCCESS = "SUCCESS"
    WARNING = "WARNING"
    FAILURE = "FAILURE"
    CANCELLED = "CANCELLED"
    FALLBACK_TIMEOUT = "FALLBACK_TIMEOUT"

    def __init__(self, log):
        """ Constructor """
        self.log = log

    def get_enum(self, result_value_wf):
        """ Used to set proper result value depenending on version in LCM FW """
        if lcm_upgrade_job.get_operation_result_enum().is_supported(lcm_util.EnumVersion.VER_2_0):
            return self.get_enum_v2(result_value_wf)

        else:
            return self.get_enum_v1(result_value_wf)

    def get_enum_v2(self, result_value_wf):
        """ Returns the result value acc. to version 2 """
        if result_value_wf == self.SUCCESS:
            return lcm_upgrade_job.get_operation_result_enum().SUCCESS
        elif result_value_wf == self.WARNING:
            return lcm_upgrade_job.get_operation_result_enum().WARNING
        elif result_value_wf == self.FAILURE:
            return lcm_upgrade_job.get_operation_result_enum().FAILURE
        elif result_value_wf == self.CANCELLED:
            return lcm_upgrade_job.get_operation_result_enum().CANCELLED
        elif result_value_wf == self.FALLBACK_TIMEOUT:
            return lcm_upgrade_job.get_operation_result_enum().FALLBACK_TIMEOUT
        else:
            self.log.error("get_enum_v2(self, result_value_wf) - result_value_wf: %s"
                           " Not supported value recieved i.e. returns WARNING"
                           % (result_value_wf))
            return lcm_upgrade_job.get_operation_result_enum().WARNING

    def get_enum_v1(self, result_value_wf):
        """ Returns the result value acc. to version 1 """
        if result_value_wf == self.SUCCESS:
            return lcm_upgrade_job.get_operation_result_enum().SUCCESS
        elif result_value_wf == self.WARNING:
            return lcm_upgrade_job.get_operation_result_enum().WARNING
        elif result_value_wf == self.FAILURE:
            return lcm_upgrade_job.get_operation_result_enum().FAILURE
        elif result_value_wf == self.CANCELLED:
            return lcm_upgrade_job.get_operation_result_enum().FAILURE
        elif result_value_wf == self.FALLBACK_TIMEOUT:
            return lcm_upgrade_job.get_operation_result_enum().FAILURE
        else:
            self.log.error("get_enum_v2(self, result_value_wf) - result_value_wf: %s"
                           " Not supported value recieved i.e. returns WARNING"
                           % (result_value_wf))
            return lcm_upgrade_job.get_operation_result_enum().WARNING
