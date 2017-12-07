'''Lifecycle Management Script for configuring volume cloning'''

LCM_EE_VERSION = "2"

def execute(handler):
    """
    Execute Lifecycle Management Script

    Args:
        handler: Lifecycle Change Management script handler
    """
    # Get port used for VNFM-VNF communication
    handler.prepare_volume_cloning("filecopy",
                                   {"port_name": "tenant_port",
                                    "scheme": "https",
                                    "port": 4443})
