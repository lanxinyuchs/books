'''Lifecycle Management Script, add configuration files to VNFC'''

LCM_EE_VERSION = "2"

import os
import subprocess
import shutil
import time

def add_certificates(handler, logger):
    """
    Provide Certificate files for all VNFC in the VNFM
    """
    vnf_info = handler.get_vnf_info('vnfcResources')
    logger.info("vnf name = %s" % (vnf_info['instanceName']))
    for vnfc in vnf_info.get('vnfcResources', []):
        logger.info("Found VNFC: %s" % (str(vnfc)))
        vnfc_dir = os.path.join(handler.get_vnf_files_dir(),
                                vnfc.get('vnfcInstanceId'))
        os.mkdir(vnfc_dir)
        try:
            handler.create_vnfc_cert(vnfc.get('vnfcInstanceId'), vnfc_dir)

            for i in range(10):
                if os.listdir(vnfc_dir):
                    break
                if i == 9:
                    raise Exception("Certificates not generated")
                time.sleep(1)

            for filename in os.listdir(vnfc_dir):
                logger.info("Addding %s" % (filename))
                content = open(os.path.join(vnfc_dir, filename)).read()
                handler.append_user_data(
                    vnfc.get('vnfcInstanceId'), ["write_files"],
                    {
                        'content': content,
                        'path': '/rcs/%s/%s' % (vnfc['vnfcInstanceId'], filename),
                        'permissions': '0644'
                    })

        except subprocess.CalledProcessError as err:
            logger.error("Failed to deploy certificate files: %s" % str(err))
        except IOError as err:
            logger.error("Failed to deploy certificate files: %s" % str(err))



def add_ai_files(handler, logger):
    """
    Add ai files from VNF files directory to directory ai_config on NFS.
    """
    vnf_files_dir = handler.get_vnf_files_dir()
    vnf_info = handler.get_vnf_info('vnfcResources')
    # Add new directory 'aiconf' in the mounted NFS disk of the VNF
    if os.path.exists(os.path.join(vnf_files_dir, "sitebasic.xml")):
        content = open(os.path.join(vnf_files_dir, "sitebasic.xml")).read()
        for vnfc in vnf_info.get('vnfcResources', []):
            handler.append_user_data(vnfc.get('vnfcInstanceId'), ["write_files"],
                        {
                            'content': content,
                            'path': '/rcs/aiconf/siteBasicFile.netconf',
                            'permissions': '0644'
                        })

    if os.path.exists(os.path.join(vnf_files_dir, "ossnodeprotocol.xml")):
        content = open(os.path.join(vnf_files_dir, "ossnodeprotocol.xml")).read()
        for vnfc in vnf_info.get('vnfcResources', []):
            handler.append_user_data(vnfc.get('vnfcInstanceId'), ["write_files"],
                        {
                            'content': content,
                            'path': '/rcs/aiconf/siteSecurityFile.netconf',
                            'permissions': '0644'
                        })

    if os.path.exists(os.path.join(vnf_files_dir, "licensekeyfile.xml")):
        content = open(os.path.join(vnf_files_dir, "licensekeyfile.xml")).read()
        for vnfc in vnf_info.get('vnfcResources', []):
            handler.append_user_data(vnfc.get('vnfcInstanceId'), ["write_files"],
                        {
                            'content': content,
                            'path': '/rcs/aiconf/licensingKeyFile.xml',
                            'permissions': '0644'
                        })

def execute(handler):
    """
    Execute Lifecycle Management Script

    Args:
        handler: Lifecycle Change Management script handler
    """
    logger = handler.get_logger(__file__)
    add_certificates(handler, logger)
    add_ai_files(handler, logger)
