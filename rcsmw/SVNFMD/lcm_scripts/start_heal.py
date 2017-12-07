'''
Lifecycle Management Script for start heal
'''

LCM_EE_VERSION = "2"


def execute(handler):
    """
    Execute Lifecycle Management Script

    Args:
        handler: Lifecycle Change Management script handler
    """
    logger = handler.get_logger(__file__)
    logger.info('start_heal(cause=%s)' % handler.get_additional_param('cause'))
    if handler.get_additional_param('cause') == "rollback":
        volumes = handler.get_additional_param('volumes')
        if not volumes:
            return

        for volume_name in volumes.split(','):
            volume_ids = handler.get_additional_param(volume_name)
            if volume_ids:
                logger.info('volume_ids=%s, volume=%s' % (volume_ids, volume_name))
                handler.reassign_volume(volume_ids, volume_name)
