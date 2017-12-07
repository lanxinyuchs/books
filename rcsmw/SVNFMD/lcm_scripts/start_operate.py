'''
Lifecycle Management Script for Start OperateVNF
Request LCM_EE to disable/enable the heartbeat function
'''

LCM_EE_VERSION = "1"


def execute(handler):
    """
    Execute Lifecycle Management Script

    Args:
        handler: Lifecycle Change Management script handler
    """
    if handler.get_additional_param('changeStateTo') == "STARTED":
        # Get port used for VNFM-VNF communication (currently 'tenant_port')
        vnf_addresses  = handler.get_ip_addresses("tenant_port")
        vnf_addresses += handler.get_ip_addresses("oam_port")
        if not vnf_addresses:
            handler.error("could not find VNF IP address")
        heartbeat = handler.enable_heartbeat(
                'https', vnf_addresses[0], 4443, "erisup/vnfcs/heartbeat")
        heartbeat.start_timer()

    if handler.get_additional_param('changeStateTo') == "STOPPED":
        handler.disable_heartbeat()
