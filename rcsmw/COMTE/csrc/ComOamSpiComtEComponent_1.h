#ifndef ComOamSpiComtEComponent_1_h
#define ComOamSpiComtEComponent_1_h

#include <MafMgmtSpiComponent_1.h>
#include <MafMgmtSpiInterfacePortal_1.h>

// Forward declare for usage in MO and Trans
typedef struct comte_oam_component comte_oam_component_t;

#include "ComtEUtils_1.h"
#include "ComOamSpiComtEManagedObject_1.h"
#include "ComOamSpiComtETrans_1.h"
#include "ComOamSpiComtEAlarmProducer_1.h"
#include "ComOamSpiComtECmEventProducer_1.h"
#include "ComOamSpiComtEPmEventProducer_1.h"
#include "ComOamSpiComtEObjectImplementer_1.h"

#ifdef COM_PM_MEAS_H
#include "ComOamSpiComtEPmMeasurement_1.h"
#endif
#include "ComOamSpiComtEPm_1.h"

#define OAM_COMPONENT_NAME "OamComtEComponent"
#define OAM_COMPONENT_VSN "1"


struct comte_oam_component {
    MafMgmtSpiComponent_1T base;
    MafMgmtSpiInterfacePortal_1T* portal;
    comte_mo_t* mo;
    comte_trans_t* trans;
    comte_alarm_producer_t* alarm_producer;
    comte_cm_event_producer_t* cm_event_producer;
    comte_pm_event_producer_t* pm_event_producer;
    comte_object_implementer_t* object_impl;

#ifdef COM_PM_MEAS_H
    comte_pm_meas_t* pm_meas;
#endif
    comte_pm_t* pm;

    comte_config_t* config;
};


MafReturnT comte_oam_component_create(comte_config_t* config,
                                      MafMgmtSpiInterfacePortal_1T* portal, 
                                      comte_oam_component_t* component);
MafReturnT comte_oam_component_destroy(comte_oam_component_t* component);

#endif
