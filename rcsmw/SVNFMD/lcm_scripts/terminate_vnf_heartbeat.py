'''
Lifecycle Management Script for terminate VNF heartbeat
Request LCM_EE to terminate heartbeat function
'''

LCM_EE_VERSION = "1"

def execute(handler):
    """
    Execute Lifecycle Management Script

    Args:
        handler: Lifecycle Change Management script handler
    """
    handler.disable_heartbeat()
