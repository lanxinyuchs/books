/*
 *
 * Copyright (c) Ericsson AB 2012-2015 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 */

#ifndef CLH_INTERNAL_H
#define CLH_INTERNAL_H

#define CSI_GET_STATE     0   /* Deprecated */
#define CSI_SUBSCRIBE     1   /* Deprecated */
#define CSI_UNSUBSCRIBE   2   /* Deprecated */
#define CSI_GET_ROLE      3   /* Deprecated */
#define CSI_SUBSCRIBE2    4   /* Deprecated */
#define CSI_UNSUBSCRIBE2  5   /* Deprecated */
#define CHI_GET_STATE     6   /* Deprecated */
#define CHI_SUBSCRIBE     7   /* Deprecated */
#define CHI_UNSUBSCRIBE   8   /* Deprecated */
#define CSI_GET_OWN_MPID                  9
#define CSI_SUBSCRIBE_CORE_STATE         10
#define CSI_UNSUBSCRIBE_CORE_STATE       11
#define CHI_SUBSCRIBE_OP_STATE           12
#define CHI_UNSUBSCRIBE_OP_STATE         13
#define CHI_ASSOCIATE_MP                 14
#define CHI_DISASSOCIATE_MP              15
#define CHI_RESTART_CLUSTER              16
#define CSI_GET_HUNT_PATH_PREFIX         17
#define CSI_SUBSCRIBE_CLUSTER_RESTART    18
#define CSI_UNSUBSCRIBE_CLUSTER_RESTART  19
#define CSI_CLUSTER_RESTART_REPLY        20

#endif   /* CLH_INTERNAL_H */
