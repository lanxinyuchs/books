/**
 * @author Lukas Larsson
 * @created 2011-04-13
 */

#include <stdlib.h>
#include <stdio.h>
#include <MafMwSpiServiceIdentities_1.h>

#include "ComMwSpiComtEComponent_1.h"
#include "ComMwSpiComtEAccessManagement_1.h"
#include "ComtEUtils_1.h"
#include "ComtEComm_1.h"
#include "ComMwSpiComtEConverter_1.h"

comte_mw_component_t* am_component = NULL;

static MafReturnT getRoles(const char* user,
		MafMwSpiAccessManagementRoleT** roles) {
	ENTER();
	MafReturnT res = MafOk;
	comte_con_handle_t connection;

	connection.quiet = 0;
	res = comte_connect(am_component->config->comte_ip, am_component->config->comte_port, &connection);
	if (res != MafOk) {
		ERROR("connect failed");
		return res;
	}

	comte_buff_t buff;
	res = encode_getRoles(user, &buff);
	if (res != MafOk) {
		INFO("encode failed");
		comte_disconnect(&connection);
		return res;
	}

	res = comte_send_recv(&buff, &connection);
	if (res != MafOk) {
		return res;
	}

	res = decode_getRoles(&buff, roles);
	if (res != MafOk) {
		INFO("decode failed");
		comte_free(buff.buff);
		comte_disconnect(&connection);
		return res;
	}

	comte_free(buff.buff);
	comte_disconnect(&connection);
	LEAVE();
	return res;
}

static MafReturnT freeRoles(MafMwSpiAccessManagementRoleT* roles) {
	ENTER();
	MafMwSpiAccessManagementRoleT* role = roles;
	while (*role != NULL) {
		comte_free((char*)*role);
		role++;
	}
	comte_free(roles);
	LEAVE();
	return MafOk;
}

MafReturnT comte_access_management_create(comte_mw_component_t* comp) {

	MafReturnT res = MafOk;

	am_component = comp;
	am_component->am = comte_malloc(sizeof(comte_am_t));
	comte_am_t* am = am_component->am;

	am->base.base.componentName = MW_COMPONENT_NAME;
	am->base.base.interfaceName = MafMwSpiAccessManagement_1Id.interfaceName;
	am->base.base.interfaceVersion =
			MafMwSpiAccessManagement_1Id.interfaceVersion;

	am->base.freeRoles = freeRoles;
	am->base.getRoles = getRoles;

	return res;
}

MafReturnT comte_access_management_destroy(comte_mw_component_t* comp) {

	MafReturnT res = MafOk;
	comte_free(comp->am);
	return res;
}
