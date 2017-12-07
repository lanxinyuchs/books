#include "master.h"
#include "cello_lici.h"
#include "cello_lici.sig"

#define CelloLici_initiateMemory_no 1
#define CelloLici_initiateService_no 2
#define CelloLici_terminateService_no 3
#define CelloLici_internal_no 4
#define CelloLici_featureSubscription_no 5
#define CelloLici_capacitySubscription_no 6
#define CelloLici_statusSubscription_no 7
#define CelloLici_isLKFInstalled_no 8


union SIGNAL
{
  SIGSELECT sigNo;
  CelloLiciInitiateServiceCfm   celloLiciInitiateServiceCfm;
  CelloLiciInitiateServiceSus   celloLiciInitiateServiceSus;
  CelloLiciInitiateServiceRej   celloLiciInitiateServiceRej;
  //CelloLiciServerDownInd        celloLiciServerDownInd;
  CelloLiciFeatureSubscribeCfm  celloLiciFeatureSubscribeCfm;
  CelloLiciFeatureSubscribeRej  celloLiciFeatureSubscribeRej;
  CelloLiciCapacitySubscribeCfm celloLiciCapacitySubscribeCfm;
  CelloLiciCapacitySubscribeRej celloLiciCapacitySubscribeRej;
  CelloLiciFeatureChangeInd     celloLiciFeatureChangeInd;
  CelloLiciCapacityChangeInd    celloLiciCapacityChangeInd;
  CelloLiciStatusSubscribeCfm   celloLiciStatusSubscribeCfm;
  CelloLiciStatusChangeInd      celloLiciStatusChangeInd;
  CelloLiciStatusSubscribeRej   celloLiciStatusSubscribeRej;
  CelloLiciIsLKFInstalledRsp    celloLiciIsLKFInstalledRsp;
};

ETERM *send_sig_lici(void **liciMemory_p, union SIGNAL *sig_p, int func, ETERM *args) {
  CelloLiciResult celloLiciResult;
  ETERM *termp;

  CelloLiciProtocolVersion firstWantedPV;
  CelloLiciProtocolVersion secondWantedPV;
  CelloLiciProtocolVersion thirdWantedPV;
  CelloLiciFeatureId featureId;
  CelloLiciCapacityId capacityId;

  printf("test_lici %i\n", func);
  switch (func) {
    /* Interface Management Functions */
  case CelloLici_initiateMemory_no:    
    printf("CelloLici_initiateMemory_define\n");
    *liciMemory_p = CelloLici_initiateMemory();
    return(erl_format("{ok, memory_initiated}"));
  case CelloLici_initiateService_no:
    printf("CelloLici_initiateService_define %p\n", args);
    firstWantedPV = ERL_INT_VALUE(termp = erl_element(1, args));
    erl_free_term(termp);
    secondWantedPV = ERL_INT_VALUE(termp = erl_element(2, args));
    erl_free_term(termp);
    thirdWantedPV = ERL_INT_VALUE(termp = erl_element(3, args));
    erl_free_term(termp);
    celloLiciResult = CelloLici_initiateService(*liciMemory_p, 
						firstWantedPV,
						secondWantedPV, 
						thirdWantedPV);
    return(erl_format("{ok, ~i}", (unsigned int) celloLiciResult));
  case CelloLici_internal_no:
    printf("CelloLici_internal_define %p\n",sig_p);
    celloLiciResult = CelloLici_internal(*liciMemory_p, sig_p);
    free_buf(&sig_p);
    return(erl_format("{ok, ~i}", (unsigned int) celloLiciResult));
  case CelloLici_terminateService_no:
    printf("CelloLici_terminateService_define %p\n",sig_p);
    celloLiciResult = CelloLici_terminateService(*liciMemory_p);
    return(erl_format("{ok, ~i}", (unsigned int) celloLiciResult));
   /* Interface Management Functions */

  case CelloLici_featureSubscription_no:
    printf("CelloLici_featureSubscription_define %p\n",args);
    termp = erl_element(1, args);
    strcpy((char *)featureId, (char *)ERL_ATOM_PTR(termp));
    erl_free_term(termp);
    celloLiciResult = CelloLici_featureSubscription(*liciMemory_p, featureId);
    return(erl_format("{ok, ~i}", (unsigned int) celloLiciResult));
  case CelloLici_capacitySubscription_no:
    printf("CelloLici_capacitySubscription_define %p\n",args);
    termp = erl_element(1, args);
    strcpy((char *)capacityId, (char *)ERL_ATOM_PTR(termp));
    erl_free_term(termp);
    celloLiciResult = CelloLici_capacitySubscription(*liciMemory_p, capacityId);
    return(erl_format("{ok, ~i}", (unsigned int) celloLiciResult));
  case CelloLici_statusSubscription_no:
    printf("CelloLici_statusSubscription_define %p\n",args);
    celloLiciResult = CelloLici_statusSubscription(*liciMemory_p);
    return(erl_format("{ok, ~i}", (unsigned int) celloLiciResult));
   case CelloLici_isLKFInstalled_no:
    printf("CelloLici_isLKFInstalled_define %p\n",args);
    celloLiciResult = CelloLici_isLKFInstalled(*liciMemory_p);
    return(erl_format("{ok, ~i}", (unsigned int) celloLiciResult));
 }
  return(erl_format("{nok, function_does_not_exist}"));
} 

ETERM *recv_sig_lici(void **liciMemory_p,union SIGNAL *sig_p) {
  ETERM *termp;

  switch(sig_p->sigNo) {
    /* Interface Management Signals */
  case CELLO_LICI_SERVER_UP_IND:
    printf("CELLO_LICI_SERVER_UP_IND %p\n",sig_p);
    return(erl_format("{signal, {~i}}", (unsigned int) CELLO_LICI_SERVER_UP_IND));
  case CELLO_LICI_INITIATE_SERVICE_CFM:
    printf("CELLO_LICI_INITIATE_SERVICE_CFM %p\n",sig_p);
    return(erl_format("{signal, {~i,~i,~i}}", 
		      (unsigned int) CELLO_LICI_INITIATE_SERVICE_CFM, 
		      (unsigned int) sig_p->celloLiciInitiateServiceCfm.signalRevision, 
		      (unsigned int) sig_p->celloLiciInitiateServiceCfm.protocolVersion));
  case CELLO_LICI_INITIATE_SERVICE_SUS:
    printf("CELLO_LICI_INITIATE_SERVICE_SUS %p\n",sig_p);
    return(erl_format("{signal, {~i,~i,~i}}", 
		      (unsigned int) CELLO_LICI_INITIATE_SERVICE_SUS, 
		      (unsigned int) sig_p->celloLiciInitiateServiceSus.signalRevision, 
		      (unsigned int) sig_p->celloLiciInitiateServiceSus.protocolVersion));
  case CELLO_LICI_INITIATE_SERVICE_REJ:
    printf("CELLO_LICI_INITIATE_SERVICE_REJ %p\n",sig_p);
    return(erl_format("{signal, {~i,~i,~i,~i}}", 
		      (unsigned int) CELLO_LICI_INITIATE_SERVICE_REJ,
		      (unsigned int) sig_p->celloLiciInitiateServiceRej.signalRevision, 
		      (unsigned int) sig_p->celloLiciInitiateServiceRej.protocolVersion, 
		      (unsigned int) sig_p->celloLiciInitiateServiceRej.rejectReason));
  case CELLO_LICI_SERVER_DOWN_IND:
    printf("CELLO_LICI_SERVER_DOWN_IND %p\n",sig_p);
    return(erl_format("{signal, {~i}}", (unsigned int) CELLO_LICI_SERVER_DOWN_IND));
  case CELLO_LICI_SERVER_UNPUBLISH_IND:
    printf("CELLO_LICI_SERVER_UNPUBLISH_IND %p\n",sig_p);
    return(erl_format("{signal, {~i}}", (unsigned int) CELLO_LICI_SERVER_UNPUBLISH_IND));
    /* Interface Management Signals */

  case CELLO_LICI_TERMINATE_SERVICE_CFM:
    printf("CELLO_LICI_TERMINATE_SERVICE_CFM %p\n",sig_p);
    return(erl_format("{signal, {~i}}", (unsigned int) CELLO_LICI_TERMINATE_SERVICE_CFM));
  case CELLO_LICI_FEATURE_SUBSCRIBE_CFM:
    printf("CELLO_LICI_FEATURE_SUBSCRIBE_CFM %p\n",sig_p);
    termp = erl_format("{signal, {~i,~s,~i,~i}}", 
		       (unsigned int) CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
		       sig_p->celloLiciFeatureSubscribeCfm.featureId,
		       (unsigned int) sig_p->celloLiciFeatureSubscribeCfm.featureStatus,
		       (unsigned int) sig_p->celloLiciFeatureSubscribeCfm.changeReason);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_FEATURE_SUBSCRIBE_REJ:
    printf("CELLO_LICI_FEATURE_SUBSCRIBE_REJ %p\n",sig_p);
    termp = erl_format("{signal, {~i,~s,~i}}", 
		       (unsigned int) CELLO_LICI_FEATURE_SUBSCRIBE_REJ,
		       sig_p->celloLiciFeatureSubscribeRej.featureId,
		       (unsigned int) sig_p->celloLiciFeatureSubscribeRej.rejectReason);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_FEATURE_CHANGE_IND:
    printf("CELLO_LICI_FEATURE_CHANGE_IND %p\n",sig_p);
    termp = erl_format("{signal, {~i,~s,~i,~i}}", 
		       (unsigned int) CELLO_LICI_FEATURE_CHANGE_IND,
		       sig_p->celloLiciFeatureChangeInd.featureId,
		       (unsigned int) sig_p->celloLiciFeatureChangeInd.featureStatus,
		       (unsigned int) sig_p->celloLiciFeatureChangeInd.changeReason);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_CAPACITY_SUBSCRIBE_CFM:
    printf("CELLO_LICI_CAPACITY_SUBSCRIBE_CFM %p\n",sig_p);
    termp = erl_format("{signal, {~i,~s,~i,~i,~i,~i,~i}}", 
		       (unsigned int) CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
		       sig_p->celloLiciCapacitySubscribeCfm.capacityId,
		       (unsigned int)sig_p->celloLiciCapacitySubscribeCfm.licensedLevel.noLimit,
      		       (unsigned int)sig_p->celloLiciCapacitySubscribeCfm.licensedLevel.value,
		       (unsigned int)sig_p->celloLiciCapacitySubscribeCfm.hardLimit.noLimit,
		       (unsigned int)sig_p->celloLiciCapacitySubscribeCfm.hardLimit.value,
		       (unsigned int)sig_p->celloLiciCapacitySubscribeCfm.changeReason);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_CAPACITY_SUBSCRIBE_REJ:
    printf("CELLO_LICI_CAPACITY_SUBSCRIBE_REJ %p\n",sig_p);
    termp = erl_format("{signal, {~i,~s,~i}}", 
		       (unsigned int) CELLO_LICI_CAPACITY_SUBSCRIBE_REJ,
		       sig_p->celloLiciCapacitySubscribeRej.capacityId,
    		       sig_p->celloLiciCapacitySubscribeRej.rejectReason);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_CAPACITY_CHANGE_IND:
    printf("CELLO_LICI_CAPACITY_CHANGE_IND %p\n",sig_p);
    termp = erl_format("{signal, {~i,~s,~i,~i,~i,~i,~i}}", 
		       (unsigned int) CELLO_LICI_CAPACITY_CHANGE_IND,
		       sig_p->celloLiciCapacityChangeInd.capacityId,
		       (unsigned int)sig_p->celloLiciCapacityChangeInd.licensedLevel.noLimit,
      		       (unsigned int)sig_p->celloLiciCapacityChangeInd.licensedLevel.value,
		       (unsigned int)sig_p->celloLiciCapacityChangeInd.hardLimit.noLimit,
		       (unsigned int)sig_p->celloLiciCapacityChangeInd.hardLimit.value,
		       (unsigned int)sig_p->celloLiciCapacityChangeInd.changeReason);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_STATUS_SUBSCRIBE_CFM:
    printf("CELLO_LICI_STATUS_SUBSCRIBE_CFM %p\n",sig_p);
    termp = erl_format("{signal, {~i,~i,~i}}", 
		       (unsigned int)CELLO_LICI_STATUS_SUBSCRIBE_CFM,
		       (unsigned int)sig_p->celloLiciStatusSubscribeCfm.licenseManagerStatus,
		       (unsigned int)sig_p->celloLiciStatusSubscribeCfm.emergencyCounter);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_STATUS_SUBSCRIBE_REJ:
    printf("CELLO_LICI_STATUS_SUBSCRIBE_REJ %p\n",sig_p);
    termp = erl_format("{signal, {~i,~i}}", 
		       (unsigned int)CELLO_LICI_STATUS_SUBSCRIBE_REJ,
		       (unsigned int)sig_p->celloLiciStatusSubscribeRej.rejectReason);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_STATUS_CHANGE_IND:
    printf("CELLO_LICI_STATUS_CHANGE_IND %p\n",sig_p);
    termp = erl_format("{signal, {~i,~i,~i}}", 
		       (unsigned int)CELLO_LICI_STATUS_CHANGE_IND,
		       (unsigned int)sig_p->celloLiciStatusChangeInd.licenseManagerStatus,
		       (unsigned int)sig_p->celloLiciStatusChangeInd.emergencyCounter);
    free_buf(&sig_p);
    return(termp);
  case CELLO_LICI_IS_LKF_INSTALLED_RSP:
    printf("CELLO_LICI_IS_LKF_INSTALLED_RSP %p\n",sig_p);
    termp = erl_format("{signal, {~i,~i,~i}}", 
		       (unsigned int)CELLO_LICI_IS_LKF_INSTALLED_RSP,
		       (unsigned int)sig_p->celloLiciIsLKFInstalledRsp.signalRevision,
		       (unsigned int)sig_p->celloLiciIsLKFInstalledRsp.result);
    free_buf(&sig_p);
    return(termp);
  default:
    return(erl_format("{signal, ~i}", (unsigned int) sig_p->sigNo));
  }
}
