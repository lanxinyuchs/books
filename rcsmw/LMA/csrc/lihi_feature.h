#ifndef GLMS_LIHI_FEATURE_H
#define GLMS_LIHI_FEATURE_H



void    handleLfciConnToServerReq(union itc_msg *sig);
void    handleLfciFeatureLicenseSubscribeReq(union itc_msg *sig);
void    handleLfciFeatureLicenseUnsubscribeReq(union itc_msg *sig);
void    disconnectAndDeleteAllLfciClients(char *errorInfo);

#endif /* GLMS_LIHI_FEATURE_H */
