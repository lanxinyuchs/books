#ifndef GLMS_CLIENTS_H
#define GLMS_CLIENTS_H



struct LfciClient_s
{
   int32_t      serverRef;
   int32_t      clientServerRef;
   uint32_t     lfciPv;

   struct LfciClient_s   *nextLfciClient;
};
typedef struct LfciClient_s LfciClient;

struct LcciClient_s
{
   int32_t      serverRef;
   int32_t      clientServerRef;
   uint32_t     lcciPv;
   uint32_t     isGpControlled;
   
   struct LcciClient_s   *nextLcciClient;
};
typedef struct LcciClient_s LcciClient;

struct LihiClient_s

{
   itc_mbox_id_t      clientMid;
   itc_monitor_id_t   monitorId;
   LfciClient         *lfciClientList;
   LcciClient         *lcciClientList;

   struct LihiClient_s   *prevLihiClient;
   struct LihiClient_s   *nextLihiClient;
};
typedef struct LihiClient_s LihiClient;


typedef struct
{
   LihiClient *firstLihiClient;
   LihiClient *lastLihiClient;
} LihiClientRoot;




/*
 * LIHI Client handling function prototypes
 *
 */
void lihiClientInit();
void lihiClientClear();

GlmsBool    client_isLastClient(LihiClient *lihiClient);
LihiClient *client_getFirst();
LihiClient *client_getLast();
LihiClient *client_getNext(LihiClient *lihiClient);
LihiClient *client_getPrevious(LihiClient *lihiClient);
LihiClient *client_findByMid(itc_mbox_id_t clientMid);
void        client_delete(LihiClient *lihiClient, GlmsBool unmonitorClient);
LihiClient *client_create(itc_mbox_id_t clientMid);

itc_mbox_id_t client_getMid(LihiClient *lihiClient);

/*
 * LFCI Client handling function prototypes
 *
 */
LfciClient *lfci_create(itc_mbox_id_t clientMid, uint32_t lfciPv, int32_t clientServerRef);
LfciClient *lfci_findByMidAndServerRef(itc_mbox_id_t mid, int32_t serverRef);
LfciClient *lfci_getFirst(LihiClient *lihiClient);
LfciClient *lfci_getNext(LfciClient *lfciClient);
uint32_t    lfci_getPv(LfciClient *lfciClient);
int32_t     lfci_getServerRef(LfciClient *lfciClient);
int32_t     lfci_getClientServerRef(LfciClient *lfciClient);
void        lfci_deleteByMidAndServerRef(itc_mbox_id_t mid,
                                         int32_t serverRef,
                                         GlmsBool unmonitorMid);
/*
 * LCCI Client handling function prototypes
 *
 */
LcciClient *lcci_create(itc_mbox_id_t clientMid, uint32_t lcciPv, int32_t clientServerRef);
LcciClient *lcci_findByMidAndServerRef(itc_mbox_id_t mid, int32_t serverRef);
LcciClient *lcci_getFirst(LihiClient *lihiClient);
LcciClient *lcci_getNext(LcciClient *lcciClient);
uint32_t    lcci_getPv(LcciClient *lcciClient);
int32_t     lcci_getServerRef(LcciClient *lcciClient);
int32_t     lcci_getClientServerRef(LcciClient *lcciClient);
void        lcci_deleteByMidAndServerRef(itc_mbox_id_t mid,
                                         int32_t serverRef,
                                         GlmsBool unmonitorMid);
void        lcci_setIsGpControlled(LcciClient *lcciClient,
                                   uint32_t isGracePeriodControlled);
uint32_t    lcci_getIsGpControlled(LcciClient *lcciClient);

/*
 * Functions for COLI dumps
 *
 */
void        handleGlmsAdpiDumpLfciClientDataReq(union itc_msg *sigRec);
void        handleGlmsAdpiDumpLcciClientDataReq(union itc_msg *sigRec);

#endif /* GLMS_CLIENTS_H */
