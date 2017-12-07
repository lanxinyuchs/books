'''Lifecycle Management Script for config NFS server'''

LCM_EE_VERSION = "1"

import os
import subprocess
import shutil

def add_certificates(handler, logger, vnf_nfs_dir):
    """
    Provide Certificate files for all VNFC in the VNFM
    """
    vnf_info = handler.get_vnf_info('vnfcResources')
    logger.info("vnf name = %s" % (vnf_info['instanceName']))
    for vnfc in vnf_info.get('vnfcResources', []):
        logger.info("Found VNFC: %s" % (str(vnfc)))
        vnfc_nfs_dir = os.path.join(vnf_nfs_dir, vnfc.get('vnfcInstanceId'))
        os.mkdir(vnfc_nfs_dir)
        try:
            handler.create_vnfc_cert(
                    vnfc.get('vnfcInstanceId'),
                    vnfc_nfs_dir)
        except subprocess.CalledProcessError as err:
            logger.warning("Failed to deploy certificate files: %s" % str(err))


def copy_config_files(handler, logger, vnf_nfs_dir):
    """
    Copy configuration files to the NFS share
    """
    vnf_files_dir = handler.get_vnf_files_dir()
    if os.path.exists(vnf_files_dir):
        copy_upgrade_files(vnf_files_dir, vnf_nfs_dir, logger)

    else:
        logger.info('VNF configuration files directory %s does not exist'
                    % vnf_files_dir)

def copy_upgrade_files(vnf_files_dir, vnf_nfs_dir, logger):
    """
    Copy upgrade files from VNF files directory to lcm/backup directory on NFS.
    """
    # Add new folder 'lcm/init_config' in the mounted NFS disk of the VNF
    dst_dir = os.path.join(vnf_nfs_dir, 'lcm', 'init_config')
    logger.info("Copy upgrade files from %s to %s" % (vnf_files_dir, dst_dir))
    if not os.path.exists(dst_dir):
        logger.debug('Creating destination directory %s' % dst_dir)
        os.makedirs(dst_dir)

    for file_name in os.listdir(vnf_files_dir):
        # Rename the upgrade files (ignore other files)
        if file_name == 'backup.zip':
            copy_file(os.path.join(vnf_files_dir, file_name),
                      "%s/%s" % (dst_dir, file_name),
                      logger)


def copy_file(src_file_path, dst_file_path, logger):
    """
    Copying a file from source to destination
    """
    if os.path.isfile(src_file_path):
        logger.info("Copying from: %s to: %s" % (src_file_path, dst_file_path))
        shutil.copy(src_file_path, dst_file_path)


def execute(handler):
    """
    Execute Lifecycle Management Script

    Args:
        handler: Lifecycle Change Management script handler
        vnf_id: VNF operated upon
        operation: LCM operation that triggers the script execution
        status: status of the operation
        additional_arguments: additional arguments for an LCM operation
    Returns:
        (HTTP code, message)
    """
    logger = handler.get_logger(__file__)
    nfsdir = handler.get_vnf_nfs_dir()
    add_certificates(handler, logger, nfsdir)
    copy_config_files(handler, logger, nfsdir)
    handler.add_nfs_dir(nfsdir)
