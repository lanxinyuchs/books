#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <search.h>
#include <libgen.h>

#include <MafOamSpiServiceIdentities_1.h>
#include <MafOamSpiModelRepository_3.h>
#include <MafMgmtSpiInterface_1.h>

#include "ComOamSpiComtEObjectImplementer_1.h"
#include "ComtEUtils_1.h"
#include "ComtEComm_1.h"
#include "ComOamSpiComtETrans_1.h"
#include "ComOamSpiComtEConverter_1.h"


comte_oam_component_t* oam_comp;
void* root = NULL;


/* Forward declarations */
MafReturnT store_readonly_attrs(comte_oi_key_t* oi, const char* key);

int oi_comp (const void *a, const void *b){
    const comte_oi_key_t* oi_a = (const comte_oi_key_t*) a;
    const comte_oi_key_t* oi_b = (const comte_oi_key_t*) b;
    return strcmp(oi_a->key, oi_b->key);
}

int attr_comp (const void *a, const void *b){
    const char* str_a = (const char*) a;
    const char* str_b = (const char*) b;
    return strcmp(str_a, str_b);
}

void set_oi_data(comte_oi_key_t* oi,
                 MafMgmtSpiInterface_1T managedObjectInterfaceId,
                 MafMgmtSpiInterface_1T transactionalResourceId,
                 int type){

    oi->moId = managedObjectInterfaceId;
    oi->trId = transactionalResourceId;

    oi->type = type;
    oi->attr_root = NULL;

    return;
}

void destroy_oi_reg(void *node){
    comte_oi_key_t* oi = (comte_oi_key_t*)node;

    INFO("Destroy OI with key: %s",oi->key);
    char* key = (char *) oi->key;
    comte_free(key);

    comte_free(oi);
}


void destroy_attr(void* node){
    char* attrName = (char *)node;
    comte_free(attrName);
}

static void destroy_oi_data(comte_oi_key_t* oi){
    /* Traverse attribute tree */
    if(oi->attr_root){
        comte_tree_destroy(oi->attr_root, destroy_attr);
        oi->type = OI_TYPE_NONE;
    }
    oi->attr_root = NULL;

}

/**
 * Register as object implementer for a specific MO class.
 *
 * @param managedObjectInterface the ManagedObject interface
 * identity that the SA shall use when forwarding.
 * @param transactionalResource the TransactionalResource interface
 * identity that the SA shall use when forwarding.
 * @param mocPath the complete path from the root and down to
 * the MOC the OI will represent. Example: /Me/ApplX/SomeMoc
 * The path is needed for the OI to resolve ambiguous MOC names.
 * @return MafOk if the  the operation succeeded and the OamSa is
 * responsible for this mocPath, MafNotExist if the OamSa doesn't
 * handle this mocPath, MafAlreadyExist if the object is already
 * registered, otherwise one of the other
 * MafReturnT error codes
 */

MafReturnT registerClass(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                         MafMgmtSpiInterface_1T transactionalResourceId,
                         const char * mocPath){

    ENTER();
    INFO("MOI: %s-%s-%s",
         managedObjectInterfaceId.componentName,
         managedObjectInterfaceId.interfaceName,
         managedObjectInterfaceId.interfaceVersion);
    INFO("TRI: %s-%s-%s",
         transactionalResourceId.componentName,
         transactionalResourceId.interfaceName,
         transactionalResourceId.interfaceVersion);
    INFO("MOC Path: %s", mocPath);

    char* class = moc_path_to_class(mocPath);

    comte_oi_key_t *found_oi;
    comte_oi_key_t oi;
    oi.key = class;

    comte_oi_key_t** coi = (comte_oi_key_t**) comte_tree_find(&oi, &root, oi_comp);
    if(!coi){
        found_oi = comte_malloc(sizeof(comte_oi_key_t));
        found_oi->key = strdup(class);
    } else
        found_oi = *coi;

    if(!found_oi){
        ERROR("Could not register class: %s", class);
        return MafFailure;
    }

    /* Set OI data */
    set_oi_data(found_oi,
                managedObjectInterfaceId,
                transactionalResourceId,
                OI_TYPE_CLASS);


    /* Add potential readonly attrs */
    store_readonly_attrs(found_oi, found_oi->key);

    /* Insert into tree */
    comte_tree_search(found_oi, &root, oi_comp);

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_registerClass(managedObjectInterfaceId,
                                          transactionalResourceId,
                                          mocPath,
                                          buff),
                     decode_registerClass(buff))

}


/**
 * Register for a particular Mo instance
 * that the Sa will forward calls to from the middleware.
 *
 * @param managedObjectInterface the ManagedObject interface id
 * @param transactionalResource the TransactionalResource interface id.
 * @param dn the dn in 3GPP format
 * @return MafOk if the operation succeeded and the OamSa is
 * responsible for this dn, MafNotExist if the OamSa doesn't
 * handle this dn, otherwise one of the other
 * MafReturnT error codes
 */


MafReturnT registerDn(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                      MafMgmtSpiInterface_1T transactionalResourceId,
                      const char * dn) {

    ENTER();
    INFO("MOI: %s-%s-%s",
         managedObjectInterfaceId.componentName,
         managedObjectInterfaceId.interfaceName,
         managedObjectInterfaceId.interfaceVersion);
    INFO("TRI: %s-%s-%s",
         transactionalResourceId.componentName,
         transactionalResourceId.interfaceName,
         transactionalResourceId.interfaceVersion);
    INFO("DN: %s", dn);

    comte_oi_key_t *found_oi;
    comte_oi_key_t oi;
    oi.key = (char*)dn;

    comte_oi_key_t** coi = (comte_oi_key_t**) comte_tree_find(&oi, &root, oi_comp);
    if(!coi){
        found_oi = comte_malloc(sizeof(comte_oi_key_t));
        found_oi->key = strdup(dn);
    } else
        found_oi = *coi;

    if(!found_oi){
        ERROR("Could not register DN: %s", dn);
        return MafFailure;
    }

    /* Set OI */
    set_oi_data(found_oi,
                managedObjectInterfaceId,
                transactionalResourceId,
                OI_TYPE_DN);

    /* Add potential readonly attrs */
    store_readonly_attrs(found_oi, found_oi->key);

    /* Store dn in the tree */
    comte_tree_search(found_oi, &root, oi_comp);

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_registerDn(managedObjectInterfaceId,
                                       transactionalResourceId,
                                       dn,
                                       buff),
                     decode_registerDn(buff))

}




/**
 * @param managedObjectInterface the ManagedObject interface id
 * @param transactionalResource the TransactionalResource interface id.
 * @param mocPath the complete path from the root and down to
 * the MOC the OI will represent. Example: /Me/ApplX/SomeMoc
 * will be handled by this OI
 * @return MafOk if the operation succeeded, MafNotExist if the class is not
 * registered, otherwise one of the other
 * MafReturnT error codes
 */
MafReturnT unregisterClass(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                           MafMgmtSpiInterface_1T transactionalResourceId,
                           const char * mocPath){

    INFO("Unregistering: %s", mocPath);
    char* class = moc_path_to_class(mocPath);

    comte_oi_key_t oi;
    oi.key = (char*)class;

    comte_oi_key_t** del = (comte_oi_key_t**) comte_tree_find(&oi, &root, oi_comp);
    if(!del){
        ERROR("Registration for class=%s was not found", mocPath);
        return MafFailure;
    }
    else {
        destroy_oi_data(*del);
        /* tree delete frees the oi/node, so we destroy the
         complete tree at shutdown */
    }

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_unregisterClass(managedObjectInterfaceId,
                                            transactionalResourceId,
                                            mocPath,
                                            buff),
                     decode_unregisterClass(buff))
}

/**
 * @param managedObjectInterface the ManagedObject interface id
 * @param transactionalResource the TransactionalResource interface id.
 * @param dn the dn
 * @return MafOk if the operation succeeded, otherwise one of the other
 * MafReturnT error codes
 */
MafReturnT unregisterDn(MafMgmtSpiInterface_1T managedObjectInterfaceId,
                        MafMgmtSpiInterface_1T transactionalResourceId,
                        const char * dn){

    INFO("Unregistering: %s", dn);
    comte_oi_key_t oi;
    oi.key = (char*)dn;
    comte_oi_key_t** del = (comte_oi_key_t**) comte_tree_find(&oi, &root, oi_comp);

    if(!del){
        ERROR("Registration for dn=%s was not found", dn);
        return MafFailure;
    }
    else {
        /* free data within the oi, the oi
         itself is destroyed during shutdown */
        destroy_oi_data(*del);
    }

    GENERIC_BERT_RPC(oam_comp->config,
                     encode_unregisterDn(managedObjectInterfaceId,
                                       transactionalResourceId,
                                       dn,
                                       buff),
                     decode_unregisterDn(buff))
}



MafReturnT comte_object_implementer_create(comte_oam_component_t* comp){

    oam_comp = comp;

    oam_comp->object_impl =
        comte_malloc(sizeof(comte_object_implementer_t));
    comte_object_implementer_t* coi = comp->object_impl;

    coi->base.base.componentName = OAM_COMPONENT_NAME;
    coi->base.base.interfaceName =
        MafOamSpiRegisterObjectImplementer_1Id.interfaceName;
    coi->base.base.interfaceVersion =
        MafOamSpiRegisterObjectImplementer_1Id.interfaceVersion;


    coi->base.registerClass = registerClass;
    coi->base.registerDn = registerDn;
    coi->base.unregisterClass = unregisterClass;
    coi->base.unregisterDn = unregisterDn;

    return MafOk;
}

MafReturnT comte_object_implementer_destroy(comte_oam_component_t* comp){
    comte_tree_destroy(root, destroy_oi_reg);
    comte_free(oam_comp->object_impl);

    return MafOk;
}



static ComtETransResourceT* get_tr_if(MafMgmtSpiInterface_1T transId){
    MafReturnT res = MafFailure;
    /* Get the interface */
    MafMgmtSpiInterface_1T* interface = NULL;
    res = oam_comp->portal->getInterface(transId,
                                         (MafMgmtSpiInterface_1T**) &interface);
    if(res != MafOk){
        ERROR("Get interface failed: %d",res);
        return NULL;
    }
    ComtETransResourceT* trIf = (ComtETransResourceT*) interface;

    return trIf;
}

MafOamSpiManagedObject_3T* get_mo_if(MafMgmtSpiInterface_1T mgmtId){
    MafReturnT res = MafFailure;

    /* Get the interface */
    MafMgmtSpiInterface_1T* interface = NULL;
    res = oam_comp->portal->getInterface(mgmtId,
                                         (MafMgmtSpiInterface_1T**) &interface);
    if (res != MafOk){
        ERROR("Get interface failed: %d",res);
        return NULL;
    }
    MafOamSpiManagedObject_3T* moIf =
        (MafOamSpiManagedObject_3T*) interface;

    return moIf;

}



bool is_mo_version(MafMgmtSpiInterface_1T moIf, const char* cmp_version){
    if(strcmp(moIf.interfaceVersion, cmp_version) == 0){
        return true;
    } else {
        return false;
    }
}

MafOamSpiManagedObject_3_1T* get_mo_ext_if(MafMgmtSpiInterface_1T mgmtId){
    MafReturnT res = MafFailure;

    /* Get the interface */
    MafMgmtSpiInterface_1T* interface = NULL;
    res = oam_comp->portal->getInterface(mgmtId,
                                         (MafMgmtSpiInterface_1T**) &interface);
    if (res != MafOk){
        ERROR("Get interface failed: %d",res);
        return NULL;
    }
    MafOamSpiManagedObject_3_1T* moExtIf =
        (MafOamSpiManagedObject_3_1T*) interface;

    return moExtIf;

}

static MafOamSpiModelRepository_3T* get_model_if_3(){
    MafReturnT res = MafFailure;
    /* Get the interface */
    MafMgmtSpiInterface_1T* interface = NULL;
    res = oam_comp->portal->getInterface(MafOamSpiModelRepository_3Id,
                                         (MafMgmtSpiInterface_1T**) &interface);
    if (res != MafOk){
        ERROR("Get model repo failed");
        return NULL;
    }
    MafOamSpiModelRepository_3T* mrIf =
        (MafOamSpiModelRepository_3T*) interface;

    return mrIf;
}


static bool is_registered(ComtETransResourceT* trIf,
                          MafOamSpiTransactionHandleT txHandle){

    MafReturnT res = MafFailure;
    bool result = false;


    /* Get the interface */
    MafMgmtSpiInterface_1T* interface = NULL;
    res = oam_comp->portal->getInterface(ComtETransMasterId,
                                         (MafMgmtSpiInterface_1T**) &interface);
    if (res != MafOk){
        ERROR("Get transaction master failed: %i", res);
        return result;
    }
    ComtETransMasterT* trMIf =
        (ComtETransMasterT*) interface;

    res = trMIf->isRegistered(txHandle, trIf, &result);
    if(res != MafOk){
        ERROR("Could not check whether %s was registered in transaction %lu, res: %i",
              trIf->base.componentName, txHandle, res);
    }

    return result;
}

MafReturnT oi_join(comte_oi_key_t* oi, MafOamSpiTransactionHandleT txHandle){

    MafReturnT res = MafOk;

    /* Get the interface */
    ComtETransResourceT* trIf = get_tr_if(oi->trId);

    if(is_registered(trIf, txHandle)){
        INFO("%s is registered in tx %d",
             trIf->base.componentName,txHandle);
        return MafOk;
    }

    INFO("Calling join for %s", trIf->base.componentName);
    res = trIf->join(txHandle);
    if(res != MafOk){
        ERROR("join OI failed: %i", res);
    }

    return res;
}




char* moc_path_to_class(const char* moc_path){
    return basename((char *)moc_path);
}

char* get_class_of_dn(const char* dn){
    MafOamSpiMrMocHandle_3T mocHandle;
    MafOamSpiMrGeneralPropertiesHandle_3T propHandle;
    char* className;

    MafOamSpiModelRepository_3T* mr = get_model_if_3();
    mr->entry->getMocFromDn(dn, false, &mocHandle);
    mr->moc->getGeneralProperties(mocHandle, &propHandle);
    mr->generalProperties->getStringProperty(propHandle,
                                             MafOamSpiMrGeneralProperty_name_3,
                                             (const char**)&className);

    return className;

}

MafReturnT get_oi(const char * dn, comte_oi_key_t** oi_out){
    comte_oi_key_t** found;
    comte_oi_key_t oi;
    oi.key = (char*)dn;
    oi.type = OI_TYPE_DN;

    found = comte_tree_find(&oi, &root, oi_comp);
    if(found && (*found)->type != OI_TYPE_NONE){
        *oi_out = *found;
        return MafOk;
    } else {
        /* Find by class */
        char* class = get_class_of_dn(dn);
        oi.key = class;
        oi.type = OI_TYPE_CLASS;

        found = comte_tree_find(&oi, &root, oi_comp);
        if(found && (*found)->type != OI_TYPE_NONE){
            *oi_out = *found;
	    return MafOk;
        }
        return MafFailure;
    }

}

MafReturnT get_oi_by_class(const char * className, comte_oi_key_t** oi_out){
    comte_oi_key_t** found;
    comte_oi_key_t oi;

    oi.key = (char*)className;
    oi.type = OI_TYPE_CLASS;

    found = comte_tree_find(&oi, &root, oi_comp);
    if(found){
        *oi_out = *found;
        return MafOk;
    }

    /* INFO("Could not find registered OI for DN: %s", className); */
    return MafFailure;
}



bool is_readonly_attr(const char* attributeName, comte_oi_key_t* oi){
    if(oi->attr_root){
        if(comte_tree_find(attributeName, &(oi->attr_root), attr_comp))
            return true;
    }
    return false;
}

MafReturnT store_readonly_attrs(comte_oi_key_t* oi, const char* key){

     /* Get the interface */
    MafReturnT mom_res = MafOk;
    MafReturnT retVal = MafFailure;
    MafReturnT attr_res = MafFailure;

    MafOamSpiModelRepository_3T* mr = get_model_if_3();
    MafOamSpiMrMomHandle_3T momHandle;
    MafOamSpiMrMocHandle_3T mocHandle;
    MafOamSpiMrAttributeHandle_3T attrHandle;
    MafOamSpiMrGeneralPropertiesHandle_3T propHandle;
    char* momVersion = NULL;
    char* momName = NULL;

    switch(oi->type){
    case OI_TYPE_NONE:
        ERROR("OI type unset!");
        return MafFailure;
    case OI_TYPE_DN:

        retVal = mr->entry->getMocFromDn(key, false, &mocHandle);
        retVal = mr->moc->getAttribute(mocHandle, &attrHandle);
        break;

    case OI_TYPE_CLASS:


        mom_res = mr->entry->getMom(&momHandle);
        while(mom_res == MafOk){
            retVal = mr->mom->getStringProperty(momHandle,
                                                MafOamSpiMrMomProperty_version_3,
                                                (const char **)&momVersion);
            mr->mom->getGeneralProperties(momHandle, &propHandle);
            retVal = mr->generalProperties->getStringProperty(propHandle,
                                                              MafOamSpiMrGeneralProperty_name_3,
                                                              (const char**)&momName);
            retVal = mr->entry->getMoc((const char*)momName,
                                       (const char*)momVersion,
                                       (const char*)key,
                                       &mocHandle);
            if(retVal != MafOk){
                /* Try next mom */
                mom_res = mr->mom->getNext(momHandle, &momHandle);
            }
            else {
                INFO("Found class %s in %s", key, momName);
                /* Continue with attributes */
                break;
            }
        }

        /* Found the class, now get the attributes */
        if(retVal == MafOk)
            attr_res = mr->moc->getAttribute(mocHandle, &attrHandle);

        break;
    }

    bool isReadOnly = false;

    // Iterate over the root moc's attributes
    while (attr_res == MafOk) {
        attr_res = mr->attribute->getBoolProperty(attrHandle,
                                                  MafOamSpiMrAttributeBoolProperty_isReadOnly_3,
                                                  &isReadOnly);
        if (attr_res != MafOk) {
            // Handle error code
            ERROR("Could not determine if attribute is readonly: %d", attr_res);
        } else {

            if (isReadOnly) {
                // This is a read only attribute
                MafOamSpiMrGeneralPropertiesHandle_3T propHandle;
                mr->attribute->getGeneralProperties(attrHandle, &propHandle);

                char* attrName;
                retVal = mr->generalProperties->getStringProperty(propHandle,
                                                                  MafOamSpiMrGeneralProperty_name_3,
                                                                  (const char**)&attrName);
                if(retVal == MafOk){
                    /* Store attribute in structure to be able
                       to match against succeeding calls to set/getMoAttribute. */
                    char *storedAttrName = strdup(attrName);
                    INFO("RO AttrName: %s", storedAttrName);
                    comte_tree_search(storedAttrName, &(oi->attr_root), attr_comp);
                } else {
                    attr_res = MafFailure;
                    ERROR("Could not get name of attribute: %d", retVal);
                }
            }
        }

        /* Get next attribute */
        if(attr_res == MafOk)
            attr_res = mr->attribute->getNext(attrHandle, &attrHandle);
    }

    return attr_res;

}


