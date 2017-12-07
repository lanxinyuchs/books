#ifndef ComOamSpiComtEObjectImplementer_1_h
#define ComOamSpiComtEObjectImplementer_1_h

#include <MafOamSpiRegisterObjectImplementer_2.h>

/* #if COM_TRANS_H < 2 */
/* #include <MafOamSpiTransactionalResource_1.h> */
/* #else */
/* #include <MafOamSpiTransactionalResource_2.h> */
/* #endif */

#if COM_TRANS_H < 2
#include <MafOamSpiTransactionMaster_1.h>
#define ComtETransMasterId MafOamSpiTransactionMaster_1Id
typedef MafOamSpiTransactionMaster_1T ComtETransMasterT;
#else
#include <MafOamSpiTransactionMaster_2.h>
#define ComtETransMasterId MafOamSpiTransactionMaster_2Id
typedef MafOamSpiTransactionMaster_2T ComtETransMasterT;
#endif


typedef struct comte_object_implementer comte_object_implementer_t;

#include "ComtEUtils_1.h"
#include "bert.h"


struct comte_object_implementer {
    MafOamSpiRegisterObjectImplementer_2T base;

};

typedef enum {
    OI_TYPE_CLASS = 1,
    OI_TYPE_DN = 2,
    OI_TYPE_NONE = 3
} ComteOiRegType;


typedef struct comte_oi_key {
    char *key;
    ComteOiRegType type;
    void *attr_root;
    MafMgmtSpiInterface_1T moId;
    MafMgmtSpiInterface_1T trId;
} comte_oi_key_t;


MafReturnT comte_object_implementer_create(comte_oam_component_t* component);
MafReturnT comte_object_implementer_destroy(comte_oam_component_t* component);

MafReturnT oi_join(comte_oi_key_t* oi, MafOamSpiTransactionHandleT txHandle);



char* moc_path_to_class(const char* moc_path);
bool is_readonly_attr(const char* attributeName, comte_oi_key_t* oi);
MafReturnT get_oi(const char* dn, comte_oi_key_t** oi_out);
MafReturnT get_oi_by_class(const char* className, comte_oi_key_t** oi_out);
MafOamSpiManagedObject_3T* get_mo_if(MafMgmtSpiInterface_1T mgmtId);
MafOamSpiManagedObject_3_1T* get_mo_ext_if(MafMgmtSpiInterface_1T mgmtId);
bool is_mo_version(MafMgmtSpiInterface_1T moIf, const char* cmp_version);

#endif

