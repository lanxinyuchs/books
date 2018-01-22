#ifndef __EMCA_LINK_API_H_
#define __EMCA_LINK_API_H_

#include <stdint.h>

#include <itc.h>

struct emca_link_if;


struct emca_link_config {
	itc_mbox_id_t            owner_mbox;
	uint32_t                 snid;
};

#define EMCA_LINK_UP             0xddd0a /* TBD */
#define EMCA_LINK_DOWN           0xddd0b /* TBD */

struct emca_link_event {
	uint32_t                msgno;
	uint32_t                instance;
	char                    link_name[];
};

int  emca_link_init(struct emca_link_if **handle);
void emca_link_shutdown(struct emca_link_if **handle);
int emca_link_create(struct emca_link_if *handle, struct emca_link_config *cfg, uint32_t *link_instance);
int emca_link_destroy(struct emca_link_if *handle, uint32_t link_instance);

#endif

