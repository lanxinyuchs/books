#ifndef ComOamSpiComtEPmCounter_1_h
#define ComOamSpiComtEPmCounter_1_h

#if ((COM_PM_MEAS_H+0) >= 2)

#if ((COM_PM_MEAS_H+0) >= 3)
#include <ComOamSpiPmMeasurements_3.h>
#else
#include <ComOamSpiPmMeasurements_2.h>
#endif

#define CAT_HELPER(a, b) a ## b
#define CAT(a, b) CAT_HELPER(a, b)

#define ComOamSpiPmMeasurementValueType_n                \
  CAT(ComOamSpiPmMeasurementValueType_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasurementValueType_nT       \
  CAT(ComOamSpiPmMeasurementValueType_n, T)
#define ComOamSpiPmMeasurementValueType_n_INT    \
  CAT(ComOamSpiPmMeasurementValueType_n, _INT)
#define ComOamSpiPmMeasurementValueType_n_FLOAT  \
  CAT(ComOamSpiPmMeasurementValueType_n, _FLOAT)
#define ComOamSpiPmMeasurementDefinition_n               \
  CAT(ComOamSpiPmMeasurementDefinition_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasurementDefinition_nT      \
    CAT(ComOamSpiPmMeasurementDefinition_n, T)
#define ComOamSpiPmMeasuredObject_n                  \
  CAT(ComOamSpiPmMeasuredObject_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasuredObject_nT             \
  CAT(ComOamSpiPmMeasuredObject_n, T)
#define ComOamSpiPmMeasuredObject__nrOfMeasurements(P)  \
    ((P)->nrOfMeasurements)
#define ComOamSpiPmMeasuredObjectMeasurementNames_n                  \
    CAT(ComOamSpiPmMeasuredObjectMeasurementNames_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasuredObjectMeasurementNames_nT         \
      CAT(ComOamSpiPmMeasuredObjectMeasurementNames_n, T)
#define ComOamSpiPmMeasurementFilter_n                   \
      CAT(ComOamSpiPmMeasurementFilter_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasurementFilter_nT          \
  CAT(ComOamSpiPmMeasurementFilter_n, T)
#define ComOamSpiPmMeasuredObjectJobIds_n                \
  CAT(ComOamSpiPmMeasuredObjectJobIds_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasuredObjectJobIds_nT       \
  CAT(ComOamSpiPmMeasuredObjectJobIds_n, T)
#define ComOamSpiPmMeasurement_n                 \
  CAT(ComOamSpiPmMeasurement_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasurement_nT                \
  CAT(ComOamSpiPmMeasurement_n, T)
#define ComOamSpiPmMeasurements_n                \
  CAT(ComOamSpiPmMeasurements_, COM_PM_MEAS_H)
#define ComOamSpiPmMeasurements_nT               \
  CAT(ComOamSpiPmMeasurements_n, T)
#define ComOamSpiPmMeasurementsInterface_nId                         \
  CAT(CAT(ComOamSpiPmMeasurementsInterface_, COM_PM_MEAS_H), Id)

#else

#include <ComOamSpiPmMeasurements_1.h>
#define ComOamSpiPmMeasurementValueType_n ComOamSpiPmMeasurementValueType_1
#define ComOamSpiPmMeasurementValueType_nT ComOamSpiPmMeasurementValueType_1T
#define ComOamSpiPmMeasurementValueType_n_INT \
    ComOamSpiPmMeasurementValueType_1_INT
#define ComOamSpiPmMeasurementValueType_n_FLOAT \
    ComOamSpiPmMeasurementValueType_1_FLOAT
#define ComOamSpiPmMeasurementDefinition_n ComOamSpiPmMeasurementDefinition_1
#define ComOamSpiPmMeasurementDefinition_nT ComOamSpiPmMeasurementDefinition_1T
#define ComOamSpiPmMeasuredObject_n ComOamSpiPmMeasuredObject_1
#define ComOamSpiPmMeasuredObject_nT ComOamSpiPmMeasuredObject_1T
#define ComOamSpiPmMeasuredObject__nrOfMeasurements(P) \
	((P)->nrOfValues)
#define ComOamSpiPmMeasuredObjectMeasurementNames_n \
    ComOamSpiPmMeasuredObjectMeasurementNames_1
#define ComOamSpiPmMeasuredObjectMeasurementNames_nT \
    ComOamSpiPmMeasuredObjectMeasurementNames_1T
#define ComOamSpiPmMeasurement_n ComOamSpiPmMeasurement_1
#define ComOamSpiPmMeasurement_nT ComOamSpiPmMeasurement_1T
#define ComOamSpiPmMeasurements_n ComOamSpiPmMeasurements_1
#define ComOamSpiPmMeasurements_nT ComOamSpiPmMeasurements_1T
#define ComOamSpiPmMeasurementsInterface_nId ComOamSpiPmMeasurementsInterface_1Id

#endif


typedef struct comte_pm_meas comte_pm_meas_t;

#include "ComOamSpiComtEComponent_1.h"

struct comte_pm_meas {
	ComOamSpiPmMeasurements_nT base;
};

MafReturnT comte_pm_meas_create(comte_oam_component_t* component);
MafReturnT comte_pm_meas_destroy(comte_oam_component_t* component);


#endif
