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
VNF API Communication Handler
"""

#pylint: disable=R0903
#pylint: disable=R0904
#pylint: disable=R0201
#pylint: disable=W0232
#pylint: disable=W0703

from lcm import (vnfm_api_comm,
                 lcm_config)
import logging
import requests
import json
from common.http_utils import (host_in_url,
                               get_request_session_by_net)
from common.netns import NetworkNS



LOG = logging.getLogger('VNF_API_COMM')
VNF_BASE_URL = "eriswm/swmv"
VNF_BASE_URL_V1 = "%s%s/" % (VNF_BASE_URL, 1)
VNF_BASE_URL_V2 = "%s%s/" % (VNF_BASE_URL, 2)
VNF_BASE_URL_V3 = "%s%s/" % (VNF_BASE_URL, 3)
VNF_BASE_URL_V4 = "%s%s/" % (VNF_BASE_URL, 4)
VNF_LOG_BASE_URL = "erilog/logWebIv1/"
HTTP_HEADERS_REQ = {'Accept': 'application/json',
                    'content-type': 'application/json'}
HTTP_HEADERS_BIN_REQ = {'content-type': 'application/x-gzip'}

class VnfCommException(Exception):
    """ Exception raised due to problems detected when interacting with VNF """

    def __init__(self, description):
        """ Constructor """
        Exception.__init__(self, description)


class VnfComm:
    """ A VNF communication handler to communicate with VNF """


    VNF_MGMT = "vnf_mgmt"

    def __init__(self, vnf_id, vnf_port, vnfm_url):
        """ Constructor """
        LOG.debug("__init__(self, vnf_id, vnf_port, vnfm_url) - vnf_id: %s,"
                  " vnf_port: %s, vnfm_url: %s" % (vnf_id, vnf_port, vnfm_url))
        self.vnf_id = vnf_id
        self.vnfm_url = vnfm_url
        self.vnf_ip_address = None
        self.vnfm_comm_handle = vnfm_api_comm.get_vnfm_comm(self.vnfm_url)
        self.vnf_port = self.__get_vnf_port(vnf_id, vnf_port)
        self.http_protocol = lcm_config.get_lcm_config().get_lcm_vnf_http_protocol()
        try:
            self.vnf_ip_address = self.__get_vnf_ip_address(self.vnf_id)
            LOG.info("VNF IP address: %s, VNF id: %s" % (self.vnf_ip_address, self.vnf_id))

        except VnfCommException as exception:
            LOG.info("__init__(...) - Failed to look up the IP address "
                     "from the VNF id %s (ignored). New attempt will be"
                     " done when really needed. Reason stated in "
                     "VnfCommException: %s" % (self.vnf_id, exception))

    def is_supported(self, version):
        """ Used to check if specific version is supported or not """
        LOG.debug("is_supported(self, version) - version: %s (to VNF: %s)"
                  % (version, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_base_url_x = "%s%s/" % (VNF_BASE_URL, version)
            vnf_url_request = "%s%s:%s/%sis_supported" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                vnf_base_url_x)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Checks if version is supported (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))
                LOG.info("Response from is version supported (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                return True

        except requests.exceptions.RequestException as exception:
            LOG.info("is_supported(self) - (to VNF: %s). Reason "
                     "for requests problem: %s" % (self.vnf_id,
                                                   exception))
            return False

        except VnfCommException as exception:
            LOG.debug("is_supported(self, version) - version: %s (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (version, self.vnf_id, exception))
            return False

        except Exception as exception:
            LOG.info("is_supported(self, version) - version: %s (to VNF: %s). Reason for "
                     "Exception: %s" % (version, self.vnf_id, exception))
            LOG.exception(exception)
            return False

    def create_backup(self, backup_name):
        """
        Creates the name of the backup file

        This is an asynchronous operation
        """
        LOG.debug("create_backup(self, backup_name) - backup_name:"
                  " %s (to VNF: %s))"
                  % (backup_name, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%screate_backup" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({'backup_name': backup_name})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for creation of backup (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from creation of backup (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                action_id = vnf_response_json_data["action_id"]
                LOG.debug("create_backup(...) - RETURNS - action_id%s"
                          % (action_id))
                return action_id

        except requests.exceptions.RequestException as exception:
            LOG.info("create_backup(self, backup_name) - backup_name: %s"
                     " (to VNF: %s). Reason for requests error: %s"
                     % (backup_name, self.vnf_id, exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request of creation of backup (%s) from VNF ID %s."
                             "  Reason: %s" % (
                exception,
                backup_name,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("create_backup(self, backup_name) - backup_name: %s."
                      " Reason for VnfCommException: %s" % (backup_name, exception))
            raise exception

        except Exception as exception:
            LOG.error("create_backup(self, backup_name) - backup_name: %s "
                      "(to VNF: %s). Reason for Exception: %s"
                      % (backup_name, self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request of creation of backup (%s) from VNF ID %s" % (
                exception,
                backup_name,
                self.vnf_id)
            raise VnfCommException(error_message)

    def create_upgrade_backup_v2(self, backup_name):
        """
        Creates the name of the backup file

        This is an asynchronous operation
        """
        LOG.debug("create_upgrade_backup_v2(self, backup_name) - backup_name:"
                  " %s (to VNF: %s))"
                  % (backup_name, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%screate_upgrade_backup" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V2)
            payload_req = json.dumps({'backup_name': backup_name})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for creation of upgrade backup v2 (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from creation of upgrade backup v2 (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                action_id = vnf_response_json_data["action_id"]
                LOG.debug("create_upgrade_backup_v2(...) - RETURNS - action_id: %s"
                          % (action_id))
                return action_id

        except requests.exceptions.RequestException as exception:
            LOG.info("create_upgrade_backup_v2(self, backup_name) - backup_name: %s"
                     " (to VNF: %s). Reason for requests error: %s"
                     % (backup_name, self.vnf_id, exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request of creation of upgrade backup (%s)"
                             " from VNF ID %s. Reason: %s" % (
                                 exception,
                                 backup_name,
                                 self.vnf_id,
                                 self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("create_upgrade_backup_v2(self, backup_name) - backup_name: %s."
                      " Reason for VnfCommException: %s" % (backup_name, exception))
            raise exception

        except Exception as exception:
            LOG.error("create_upgrade_backup_v2(self, backup_name) - backup_name: %s "
                      "(to VNF: %s). Reason for Exception: %s"
                      % (backup_name, self.vnf_id, exception))
            LOG.exception(exception)
            error_message = ("%s -- At request of creation of upgrade backup (%s) from"
                             " VNF ID %s" % (
                                 exception,
                                 backup_name,
                                 self.vnf_id))
            raise VnfCommException(error_message)

    def delete_backup(self, backup_name):
        """
        Deletes the name of the backup file

        This is an asynchronous operation
        """
        LOG.debug("delete_backup(self, backup_name) - backup_name:"
                  " %s (to VNF: %s))"
                  % (backup_name, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sdelete_backup" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({'backup_name': backup_name})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for deletion of backup (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))
                LOG.info("Response from deletion of backup (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response.text, vnf_response.status_code))
                vnf_response.raise_for_status()

        except requests.exceptions.RequestException as exception:
            LOG.info("delete_backup(self, backup_name) - backup_name: %s"
                     " (to VNF: %s). Reason for requests error: %s"
                     % (backup_name, self.vnf_id, exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request of deletion of backup (%s) from VNF ID %s."
                             " Reason: %s" % (
                exception,
                backup_name,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("delete_backup(self, backup_name) - backup_name: %s."
                      " Reason for VnfCommException: %s" % (backup_name, exception))
            raise exception

        except Exception as exception:
            LOG.warning("delete_backup(self, backup_name) - backup_name: %s "
                        "(to VNF: %s). Reason for Exception: %s"
                        % (backup_name, self.vnf_id, exception))
            error_message = "%s -- At request of deletion of backup (%s) from VNF ID %s" % (
                exception,
                backup_name,
                self.vnf_id)
            raise VnfCommException(error_message)

    def export_backup(self, url, backup_name, password = None):
        """
        Used to export a backup from a VNF to a specific path.
        URL gives where to put the backup file.
        Password is optional

        This is an asynchronous operation
        """
        LOG.debug("export_backup(self, url, backup_name, password = None) - url: %s,"
                  " backup_name: %s, password: %s (to VNF: %s)"
                  % (url, backup_name, password, self.vnf_id))
        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sexport_backup" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            data = {'url': url,
                    'backup_name': backup_name}
            if password != None:
                data['password'] = password
            payload_req = json.dumps(data)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for export of backup (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from export of backup (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                action_id = vnf_response_json_data["action_id"]
                return action_id

        except requests.exceptions.RequestException as exception:
            LOG.info("export_backup(self, url, backup_name, password = None) - url: %s,"
                     " backup_name: %s, password: %s (to VNF: %s). Reason for requests error: %s"
                     % (url, backup_name, password, self.vnf_id, exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request of export of backup to URL (%s) from VNF ID %s."
                             " Reason: %s" % (
                exception,
                url,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("export_backup(self, url, backup_name, password = None) - url: %s,"
                      " backup_name: %s, password: %s. Reason for VnfCommException: %s"
                      % (url, backup_name, password, exception))
            raise exception

        except Exception as exception:
            LOG.error("export_backup(self, url, backup_name, password = None) - url: %s,"
                      " backup_name: %s, password: %s (to VNF: %s). Reason for Exception: %s"
                      % (url, backup_name, password, self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request of export of backup to URL (%s) from VNF ID %s" % (
                exception,
                url,
                self.vnf_id)
            raise VnfCommException(error_message)


    def start_traffic(self):
        """ Used to request VNF to start the traffic """
        LOG.debug("start_traffic(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sstart_traffic" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for start of traffic (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from start traffic (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("start_traffic(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for start of traffic for VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("start_traffic(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("start_traffic(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for start of traffic for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def start_traffic_v2(self, stop_traffic_data):
        """ Used to request VNF to stop the traffic """
        LOG.debug("start_traffic_v2(self, stop_traffic_data) - stop_traffic_data: %s (to VNF: %s)"
                  % (stop_traffic_data, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sstart_traffic" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V2)
            payload_req = json.dumps(stop_traffic_data)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for start of traffic v2 (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from start traffic v2 (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("start_traffic_v2(self, stop_traffic_data) - stop_traffic_data: %s "
                     "(to VNF: %s). Reason for requests error: %s"
                     % (stop_traffic_data, self.vnf_id, exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for start of traffic V2 for VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("start_traffic_v2(self, stop_traffic_data) stop_traffic_data: %s - "
                      "(to VNF: %s). Reason for VnfCommException: %s" %
                      (stop_traffic_data, self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("start_traffic_v2(self, stop_traffic_data) - stop_traffic_data: %s"
                      " (to VNF: %s). Reason for Exception: %s" %
                      (stop_traffic_data, self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for start of traffic V2 for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def start_configuration(self):
        """  Used to request VNF to allow configuration changes """
        LOG.debug("start_configuration(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sstart_configuration" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for start of configuration (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from start configuration (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                LOG.debug("start_configuration(...) - RETURNS - %s"
                          % (vnf_response_json_data))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("start_configuration(self) - (to VNF: %s). Reason "
                     "for requests error: %s -- At request for stop of"
                     " configuration for VNF %s" % (self.vnf_id, exception, self.vnf_id))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for stop of configuration for VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("start_configuration(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("start_configuration(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for stop of configuration for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def stop_configuration(self):
        """ Used to request VNF to stop any attempt to change configuration """
        LOG.debug("stop_configuration(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sstop_configuration" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for stop of configuration (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from stop configuration (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                LOG.debug("stop_configuration(...) - RETURNS - %s"
                          % (vnf_response_json_data))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("stop_configuration(self) - (to VNF: %s). Reason "
                     "for requests error: %s -- At request for stop of"
                     " configuration for VNF %s" % (self.vnf_id, exception, self.vnf_id))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for stop of configuration for VNF %s. Reason: %s" %
                             (exception,
                              self.vnf_id,
                              self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("stop_configuration(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("stop_configuration(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for stop of configuration for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)


    def stop_traffic(self):
        """ Used to request VNF to stop current taffic """
        LOG.debug("stop_traffic(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sstop_traffic" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for stop of traffic (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from stop traffic (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("stop_traffic(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for stop of traffic for VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("stop_traffic(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("stop_traffic(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for stop of traffic for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def stop_traffic_v2(self, time_of_action, cause):
        """ Used to request VNF to stop current taffic """
        LOG.debug("stop_traffic_v2(self, time_of_action, cause) - , time_of_action: %s,"
                  " cause: %s (to VNF: %s)" % (time_of_action, cause, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sstop_traffic" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V2)
            payload_req = json.dumps({'time_of_action': time_of_action, 'cause': cause})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for stop of traffic v2 (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))
                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from stop traffic v2 (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("stop_traffic_v2(self, time_of_action, cause) - , time_of_action: %s,"
                     " cause: %s (to VNF: %s). Reason "
                     "for requests error: %s" % (time_of_action, cause, self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for stop of traffic V2 for VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("stop_traffic_v2(self, time_of_action, cause) - , time_of_action: %s,"
                      " cause: %s (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (time_of_action, cause, self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("stop_traffic_v2(self, time_of_action, cause) - , time_of_action: %s,"
                      " cause: %s (to VNF: %s). Reason for "
                      "Exception: %s" % (time_of_action, cause, self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for stop of traffic V2 for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def confirm(self):
        """ Used to confirm the upgrade for TO VNF """
        LOG.debug("confirm(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sconfirm" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for confirmation (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from confirmation (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                LOG.debug("confirm(...) - RETURNS - %s"
                          % (vnf_response_json_data))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("confirm(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for confirm for VNF %s. Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("confirm(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("confirm(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for confirm for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def cancel(self, action_id):
        """ Used to cancel ongoing operation during upgrade in From VNF """
        LOG.debug("cancel(self, action_id) - action_id: %s (to VNF: %s)"
                  % (action_id, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%scancel" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V1)
            payload_req = json.dumps({"action_id": action_id})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for cancel (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from cancel (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("cancel(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for cancel of VNF %s. Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("cancel(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("cancel(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for cancel of VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def export_upgrade_logs(self, upgrade_logs_file_path):
        """ Collects logs from the VNF """
        LOG.debug("export_upgrade_logs(self_id, upgrade_logs_file_path)"
                  " - upgrade_logs_file_path: %s (to VNF: %s)" %
                  (upgrade_logs_file_path, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sexport_upgrade_logs" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_LOG_BASE_URL)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for export of upgrade logs (towards VNF %s) - url:"
                         " %s" % (self.vnf_id, vnf_url_request))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.get(vnf_url_request, headers = HTTP_HEADERS_BIN_REQ,
                                           data='', verify = False, stream = True)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("response status code: %s" %  (vnf_response.status_code))
                LOG.info("Response from export upgrade logs (VNF %s)" % (self.vnf_id))
                vnf_response.raise_for_status()

                # Write to file in chunks
                LOG.info("Starts to write upgrade logs to the file (%s), length: %s (VNF %s)"
                          % (upgrade_logs_file_path, len(vnf_response.content), self.vnf_id))
                file_handle = open(upgrade_logs_file_path, "wb")
                for data_chunk in vnf_response.iter_content(chunk_size = 1024):
                    if data_chunk:
                        file_handle.write(data_chunk)
                LOG.info("Write finished (VNF %s)" % (self.vnf_id))

        except requests.exceptions.RequestException as exception:
            LOG.info("export_upgrade_logs(self, upgrade_logs_file_path) - "
                     "upgrade_logs_file_path: %s (to VNF: %s). Reason "
                     "for requests error: %s" % (upgrade_logs_file_path,
                                                 self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for export of upgrade logs from VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("export_upgrade_logs(self, upgrade_logs_file_path) -  "
                      "upgrade_logs_file_path: %s -- At request for export of upgrade logs "
                      "from VNF %s. Reason for VnfCommException: %s" %
                      (upgrade_logs_file_path, self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("export_upgrade_logs(self, upgrade_logs_file_path) - "
                      "upgrade_logs_file_path: %s (to VNF: %s). Reason for "
                      "Exception: %s" % (upgrade_logs_file_path, self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for export of upgrade logs from VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def fetch_upgrade_logs(self):
        """ Requesting VNF to start fetching upgrade logs """
        LOG.debug("fetch_upgrade_logs(self) - (to VNF: %s)" % (self.vnf_id))
        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sfetch_upgrade_logs" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_LOG_BASE_URL)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            payload_req = json.dumps({})
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for fetch of upgrade logs (towards VNF %s) - url: %s"
                         ", payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.get(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                           data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from fetch upgrade logs (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("fetch_upgrade_logs(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for import of upgrade logs from VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("fetch_upgrade_logs(self) -  -- At request for import of upgrade logs "
                      "from VNF %s. Reason for VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("fetch_upgrade_logs(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for import of upgrade logs from VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def fetch_esi(self):
        """ Requesting VNF to start fetching ESI """
        LOG.debug("fetch_esi(self) - (to VNF: %s)" % (self.vnf_id))
        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sfetch_esi" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_LOG_BASE_URL)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            payload_req = json.dumps({})
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for fetch of ESI (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.get(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                           data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from fetch ESI (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("fetch_esi(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for fetch  of ESI from VNF %s. Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("fetch_esi(self) -  -- At request for fetch of ESI "
                      "from VNF %s. Reason for VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("fetch_esi(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for fetch of ESI from VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def export_esi(self, esi_file_path):
        """ Collects logs from the VNF """
        LOG.debug("export_esi(self_id, esi_file_path)"
                  " - esi_file_path: %s (to VNF: %s)" %
                  (esi_file_path, self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sexport_esi" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_LOG_BASE_URL)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests for export of ESI (towards VNF %s) - url:"
                         " %s" % (self.vnf_id, vnf_url_request))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.get(vnf_url_request, headers = HTTP_HEADERS_BIN_REQ,
                                           data='', verify = False, stream = True)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("response status code: %s" %  (vnf_response.status_code))
                LOG.info("Response from export ESI (VNF %s) " % (self.vnf_id))

                vnf_response.raise_for_status()

                # Write to file in chunks
                LOG.info("Starts to write ESI to the file (%s), length: %s (VNF %s)"
                         % (esi_file_path, len(vnf_response.content), self.vnf_id))
                file_handle = open(esi_file_path, "wb")
                for data_chunk in vnf_response.iter_content(chunk_size = 1024):
                    if data_chunk:
                        file_handle.write(data_chunk)
                LOG.info("Write finished (VNF %s)" % (self.vnf_id))

        except requests.exceptions.RequestException as exception:
            LOG.info("export_esi(self, esi_file_path) - "
                     "esi_file_path: %s (to VNF: %s). Reason "
                     "for requests error: %s" % (esi_file_path,
                                                 self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for export of upgrade logs from VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("export_esi(self, esi_file_path) -  "
                      "esi_file_path: %s -- At request for export of upgrade logs "
                      "from VNF %s. Reason for VnfCommException: %s" %
                      (esi_file_path, self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("export_esi(self, esi_file_path) - "
                      "esi_file_path: %s (to VNF: %s). Reason for "
                      "Exception: %s" % (esi_file_path, self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for export of upgrade logs from VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def verify_preconditions(self):
        """
        Used to give VNF possibility to execute pre checks (verify that certain
        pre conditions are fulfilled) at execution of the verify action
        """
        LOG.debug("verify_preconditions(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sverify_preconditions" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V3)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests VNF to execute verify_preconditions (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from verify_preconditions (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("verify_preconditions(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for verify_preconditions in VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("verify_preconditions(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("verify_preconditions(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for verify_preconditions in VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def activate_start(self):
        """
        Used to give VNF possibility to execute activate_start (verify that certain
        pre conditions are fulfilled) prior execution of the activate action
        """
        LOG.debug("activate_start(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%sactivate_start" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V3)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS(self.VNF_MGMT):
                LOG.info("Requests VNF to execute activate_start (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s" %  (vnf_response.text))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from activate_start (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("activate_start(self) - (to VNF: %s). Reason "
                     "for requests error: %s" % (self.vnf_id,
                                                 exception))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for activate_start in VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.debug("activate_start(self) - (to VNF: %s). Reason for "
                      "VnfCommException: %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.error("activate_start(self) - (to VNF: %s). Reason for "
                      "Exception: %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for activate_start in VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def upgrade_clone_complete(self):
        """
        Used to inform From-VNF that clone of the disks have been performed (Cinder solution)
        """
        LOG.debug("upgrade_clone_complete(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%supgrade_clone_complete" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V2)
            payload_req = json.dumps({})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS("vnf_mgmt"):
                LOG.info("Requests for upgrade clone complete (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from upgrade clone complete (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                LOG.debug("upgrade_clone_complete(...) - RETURNS - %s"
                          % (vnf_response_json_data))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("upgrade_clone_complete(self) - (to VNF: %s). Reason "
                     "for requests exception (reject): %s -- At request for upgrade clone complete"
                     " for VNF %s" % (self.vnf_id, exception, self.vnf_id))
            LOG.info("vnf_response.text: %s" % (vnf_response.text))
            error_message = ("%s -- At request for upgrade clone complete for VNF %s."
                             " Reason: %s" % (
                exception,
                self.vnf_id,
                self.__get_response_error_info(vnf_response)))
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.info("upgrade_clone_complete(self) - (to VNF: %s). Reason for "
                     "VnfCommException (reject): %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.info("upgrade_clone_complete(self) - (to VNF: %s). Reason for "
                     "Exception (reject): %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for upgrade clone complete for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def restore_aborted(self, add_infos, result_info = None):
        """
        Used to inform From-VNF that restore has been aborted
        """
        LOG.debug("restore_aborted(self, add_infos, result_info = None) - "
                  "(to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%srestore_aborted" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V4)
            if result_info == None:
                payload_req = json.dumps({'info': add_infos})
            else:
                payload_req = json.dumps({'info': add_infos,
                                          'result_info': result_info})
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS("vnf_mgmt"):
                LOG.info("Requests for restore aborted (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from restore aborted (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                LOG.debug("restore_aborted(...) - RETURNS - %s"
                          % (vnf_response_json_data))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("restore_aborted(self) - (to VNF: %s). Reason "
                     "for requests exception (reject): %s -- At request for restore aborted"
                     " for VNF %s" % (self.vnf_id, exception, self.vnf_id))
            error_message = "%s -- At request for restore aborted for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.info("restore_aborted(self) - (to VNF: %s). Reason for "
                     "VnfCommException (reject): %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.info("restore_aborted(self) - (to VNF: %s). Reason for "
                     "Exception (reject): %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for restore aborted for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def post_restore_check(self, payload):
        """
        Used to give VNF possibility to check that conditions are fulfilled
        """
        LOG.debug("post_restore_check(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%spost_restore_check" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V4)
            payload_req = json.dumps(payload)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS("vnf_mgmt"):
                LOG.info("Requests for a post restore check (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from post restore check (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                LOG.debug("post_restore_check(...) - RETURNS - %s"
                          % (vnf_response_json_data))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("post_restore_check(self) - (to VNF: %s). Reason "
                     "for requests exception (reject): %s -- At request for post restore check"
                     " for VNF %s" % (self.vnf_id, exception, self.vnf_id))
            error_message = "%s -- At request for post restore check for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.info("post_restore_check(self) - (to VNF: %s). Reason for "
                     "VnfCommException (reject): %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.info("post_restore_check(self) - (to VNF: %s). Reason for "
                     "Exception (reject): %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for post restore check for VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def restore_confirmed(self, payload):
        """
        Used to inform VNF that restore is confirmed
        """
        LOG.debug("restore_confirmed(self) - (to VNF: %s)" % (self.vnf_id))

        try:
            self.__lookup_vnf_ip_address(self.vnf_id)
            vnf_url_request = "%s%s:%s/%srestore_confirmed" % (
                self.http_protocol, host_in_url(self.vnf_ip_address), self.vnf_port,
                VNF_BASE_URL_V4)
            payload_req = json.dumps(payload)
            LOG.debug("vnf_api_comm request url: %s" % (vnf_url_request))
            LOG.debug("vnf_api_comm request data: %s" % (payload_req))
            with NetworkNS("vnf_mgmt"):
                LOG.info("Requests for restore_confirmed (towards VNF %s) - url:"
                         " %s, payload: %s"
                         % (self.vnf_id, vnf_url_request, payload_req))
                request = get_request_session_by_net(self.VNF_MGMT)
                vnf_response = request.post(vnf_url_request, headers = HTTP_HEADERS_REQ,
                                            data=payload_req, verify = False)
                LOG.debug("vnf_api_comm response headers: %s" %  (vnf_response.headers))
                LOG.debug("vnf_api_comm response data: %s, status_code: %s" %
                          (vnf_response.text, vnf_response.status_code))

                vnf_response.raise_for_status()
                vnf_response_json_data = json.loads(vnf_response.text)
                LOG.info("Response from restore confirmed (VNF %s) - payload: %s, status_code: %s"
                         % (self.vnf_id, vnf_response_json_data, vnf_response.status_code))
                LOG.debug("restore_confirmed(...) - RETURNS - %s"
                          % (vnf_response_json_data))
                return vnf_response_json_data

        except requests.exceptions.RequestException as exception:
            LOG.info("restore_confirmed(self) - (to VNF: %s). Reason "
                     "for requests exception (reject): %s -- At request for post restore check"
                     " for VNF %s" % (self.vnf_id, exception, self.vnf_id))
            error_message = "%s -- At request for restore confirmed to VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

        except VnfCommException as exception:
            LOG.info("restore_confirmed(self) - (to VNF: %s). Reason for "
                     "VnfCommException (reject): %s" % (self.vnf_id, exception))
            raise exception

        except Exception as exception:
            LOG.info("restore_confirmed(self) - (to VNF: %s). Reason for "
                     "Exception (reject): %s" % (self.vnf_id, exception))
            LOG.exception(exception)
            error_message = "%s -- At request for restore confirmed to VNF %s" % (
                exception,
                self.vnf_id)
            raise VnfCommException(error_message)

    def __lookup_vnf_ip_address(self, vnf_id):
        """ Defines VNF IP address if needed """
        LOG.debug("__lookup_vnf_ip_address(self, vnf_id) - vnf_id: %s"
                  " (VNF IP address: %s)" % (vnf_id, self.vnf_ip_address))
        if self.vnf_ip_address == None:
            self.vnf_ip_address = self.__get_vnf_ip_address(vnf_id)

    def __get_vnf_ip_address(self, vnf_id):
        """ Returns IP address of VNF """
        LOG.debug("__get_vnf_ip_address(self, vnf_id) - vnf_id: %s" % (vnf_id))
        try:
            return self.vnfm_comm_handle.get_ip_address(vnf_id)

        except vnfm_api_comm.IpAddressLookupFailureException as exception:
            LOG.info("__get_vnf_ip_address(self, vnf_id) - Failed "
                     "to look up the IP address for the VNF id %s."
                     "Reason stated in IpAddressLookupFailureException: %s"
                     % (self.vnf_id, exception))
            raise VnfCommException(str(exception))

    def __get_vnf_port(self, vnf_id, def_vnf_port):
        """ Return port no to be used for vnf id """
        LOG.debug("get_vnf_port(self, vnf_id, def_vnf_port) - vnf_id: %s, "
                  "def_vnf_port: %s" % (vnf_id, def_vnf_port))
        if lcm_config.get_lcm_config().is_cloud_env():
            return def_vnf_port
        else:
            vnf_port_no = self.vnfm_comm_handle.get_vnf_port_no(vnf_id)
            LOG.debug("get_vnf_port(...) - Not cloud env - RETURNS port no: %s"
                      % (vnf_port_no))
            return vnf_port_no

    def __get_response_error_info(self, response):
        """ Returns error information """
        LOG.debug("__get_response_error_info(self, response) - response: %s" % (response))
        try:
            description = response.text
            response_data = json.loads(description)
            if 'error' in response_data:
                if 'description' in response_data['error']:
                    description =  response_data['error']['description']
            return description

        except Exception as exception:
            LOG.info("__get_response_error_info(self, response) - response: %s - "
                     "Failed to extract description for the problem in nice way."
                     " Will instead include complete response body: %s. Exception: %s" %
                     (response, response.text, exception))
            return response.text


def get_vnf_comm(vnf_id, vnf_port, vnfm_url):
    """ Returns a handle to a VNF communication handler object"""
    LOG.debug("get_vnf_comm(vnf_id, vnf_port, vnfm_url) - vnf_id: %s, "
              "vnf_port: %s, vnfm_url: %s" % (vnf_id, vnf_port, vnfm_url))
    return VnfComm(vnf_id, vnf_port, vnfm_url)
