#ifndef ComMwSpiComtEReplicatedList_1_h
#define ComMwSpiComtEReplicatedList_1_h

#include <MafMwSpiReplicatedList_1.h>

typedef struct comte_replicated_list_interface comte_replicated_list_interface_t;

#include "ComMwSpiComtEComponent_1.h"
#include "bert.h"

struct comte_replicated_list_interface {
	MafMwSpiReplicatedList_1T base;
};

typedef struct comte_replicated_list_item comte_replicated_list_item_t;

struct comte_replicated_list_item {
	void *value;
	comte_replicated_list_item_t* next;
};

typedef struct comte_replicated_list {
	bert_list_t* curr;
	bert_list_t* first;
} comte_replicated_list_t;

MafReturnT comte_replicated_list_create(comte_mw_component_t* component);
MafReturnT comte_replicated_list_destroy(comte_mw_component_t* component);

#endif
