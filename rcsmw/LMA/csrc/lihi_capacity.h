#ifndef GLMS_LIHI_CAPACITY_H
#define GLMS_LIHI_CAPACITY_H

void    handleLcciConnToServerReq(union itc_msg *sig);
void    handleLcciCapacityLicenseSubscribeReq(union itc_msg *sig);
void    handleLcciCapacityLicenseUnsubscribeReq(union itc_msg *sig);
void    handleLcciCapacityLicenseGpActivatedFwd(union itc_msg *sig);
void    disconnectAndDeleteAllLcciClients(char *errorInfo);

#endif /* GLMS_LIHI_CAPACITY_H */
