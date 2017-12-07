'''Lifecycle Management Script for start VNF heartbeat'''
from time import sleep, time

LCM_EE_VERSION = "1"

# Is used as timeout value when sending the http request towards lcm_ee
INITIAL_HEARTBEAT_TIMEOUT = 300

def execute(handler):
    """
    Execute Lifecycle Management Script

    Args:
        handler: Lifecycle Change Management script handler
    """
    logger = handler.get_logger(__file__)

    # Get port used for VNFM-VNF communication (currently 'tenant_port')
    vnf_addresses  = handler.get_ip_addresses("tenant_port")
    if not vnf_addresses:
        handler.error("could not find VNF IP address")

    # Wait for successful heartbeat before completing Lifecycle Operation
    heartbeat = handler.enable_heartbeat(
            'https', vnf_addresses[0], 4443, "erisup/vnfcs/heartbeat")
    
    start = time()
    while (not heartbeat.check_heartbeat(handler.get_operation())):
        sleep(5)
        if time() > start+INITIAL_HEARTBEAT_TIMEOUT:
            handler.error("No reply on Heartbeat in %ss" % (INITIAL_HEARTBEAT_TIMEOUT))

    logger.info("Heartbeat response received from VNF (%s)." % handler.get_vnf_id())

    heartbeat.start_timer()





