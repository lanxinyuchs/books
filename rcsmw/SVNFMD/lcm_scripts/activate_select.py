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

#pylint: disable=R0903
#pylint: disable=C0103

from lcm import (vnfm_api_comm,
                 workflow_handler,
                 lcm_util,
                 lcm_python_manager,
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
from common.http_utils import host_in_url


LOG = logging.getLogger('ACTIVATE_SELECT')

def main(job_id, operation_data, upgrade_job, lcm_config, callback_when_finished):
    """ Main function for activate to initiate the activate request """
    LOG.debug("main(job_id, operation_data, upgrade_job, lcm_config, "
              "callback_when_finished) - job_id: %s, operation_data: %s, "
              "upgrade_job: %s, lcm_config: %s, callback_when_finished: %s"
              % (job_id, operation_data, upgrade_job, lcm_config,
                 callback_when_finished))
    if (upgrade_job.get_state() == lcm_upgrade_job.get_state_enum().PREPARE_COMPLETED):
        activate = Activate(job_id, upgrade_job, lcm_config, callback_when_finished)
        activate_wf = activate.main(operation_data)
        LOG.debug("Activate initiated for JOB_ID: %s" % (job_id))
        return activate_wf
    else:
        raise lcm_util.LcmException(
            lcm_util.LcmException.NOT_ALLOWED,
            "Attempt to execute operation activate for an upgrade job (job_id: %s"
            ") when not expected. Current state: %s, Current workflow: %s"
            % (job_id, upgrade_job.state, upgrade_job.current_workflow))


class Activate(object):
    """ Activate workflow script """

    def __init__(self, job_id, upgrade_job, lcm_config, callback_when_finished):
        """ Constructor """
        self.job_id = job_id
        self.upgrade_job = upgrade_job
        self.lcm_config = lcm_config
        self.callback_when_finished = callback_when_finished
        self.vnfm_url = "http://%s:%s/" % (host_in_url(self.lcm_config.get_vnfm_http_host()),
                                           self.lcm_config.get_vnfm_http_port())
        self.vnfm_comm_handle = vnfm_api_comm.get_vnfm_comm(self.vnfm_url)

    def main(self, *operation_data):
        """ Main method for script activate """
        LOG.debug('main(self, *operation_data) - operation_data: %s'
                  % (operation_data))
        LOG.info("LCM Workflow Activate Select Version: %s, LCM Framework Version: %s (job id: %s)"
                 % (lcm_wf_utility.get_lcm_workflow_version(),
                    lcm_util.get_lcm_fw_version(),
                    self.job_id))

        try:
            # Check whether Cinder or NFS is used for VNF
            wf_path = None
            if self.vnfm_comm_handle.is_storage_required(self.upgrade_job.vnf_id):
                LOG.debug("main(...) - Storage Cinder is required")
                wf_path = workflow_handler.get_workflow_handler().import_wf(
                    self.upgrade_job,
                    lcm_upgrade_job.get_workflow_enum().FUNCTION_AREA,
                    lcm_upgrade_job.get_workflow_enum().ACTIVATE_CINDER_OPERATION)
            else:
                LOG.debug("main(...) - NFS is required")
                wf_path = workflow_handler.get_workflow_handler().import_wf(
                    self.upgrade_job,
                    lcm_upgrade_job.get_workflow_enum().FUNCTION_AREA,
                    lcm_upgrade_job.get_workflow_enum().ACTIVATE_NFS_OPERATION)

            wf_module = lcm_python_manager.get_workflow_python_manager().import_source(
                wf_path, self.upgrade_job,
                workflow_handler,
                lcm_upgrade_job.get_workflow_enum().FUNCTION_AREA)
            workflow = self.__execute_workflow(wf_module,
                                               self.job_id,
                                               operation_data,
                                               self.upgrade_job,
                                               self.lcm_config,
                                               self.callback_when_finished)

            return workflow

        except (lcm_util.LcmException,
                vnfm_api_comm.VnfmCommException) as exception:
            LOG.debug("Activate of the UP with JOB ID (%s) failed when trying to "
                      "determine wether Cinder disks or NFS is used. Resason is: %s"
                      % (self.job_id, exception))
            raise lcm_wf_utility.WorkflowException(
                "Activate of the UP with JOB ID (%s) failed when trying to "
                "determine whether Cinder disks or NFS is used. Resason is: %s"
                % (self.job_id, exception))

        except Exception as exception:
            LOG.warning("Activate of the UP with JOB ID (%s) failed when trying to "
                        "determine wether Cinder disks or NFS is used. Resason is: %s"
                        % (self.job_id, exception))
            LOG.exception(exception)
            raise lcm_wf_utility.WorkflowException(
                "Activate of the UP with JOB ID (%s) failed when trying to "
                "determine wether Cinder disks or NFS is used. Resason is: %s"
                % (self.job_id, exception))

    def __execute_workflow(self, wf_module, *args, **kwargs):
        """
        Starts execution of a workflow
        """
        LOG.debug("__execute_workflow(self, wf_module, *args, **kwargs) - script: %s, "
                  "*args: %s, **kwargs: %s, self: %s"
                  % (wf_module, args, kwargs, self))

        function = getattr(wf_module, 'main')
        return function(*args, **kwargs)
