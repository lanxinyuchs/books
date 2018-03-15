/**
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef PARAMDB_DTB_NVPI3_TESTS__
#define PARAMDB_DTB_NVPI3_TESTS__

/* data types
#define str_typ          "#str#";
#define u8_typ           "#u8";
#define bin_typ          "#bin";
#define u32_typ          "#u32";
*/

/* Data bases */
/* #define db_dont_exist "dont_exist"; */
/* #define empty_node    "empty_node"; */

#define root_node         "/"

/* top_node */
#define root_top_node     "/"
#define top0_str_k        "top0_str#str"
#define top0_str_sk       "top0_str"
#define top0_str_v        "EARTH"

#define top0_u8_k         "top0_u8#u8"
#define top0_u8_sk        "top0_u8"
#define top0_u8_v         "08 32 88 97"

#define top0_bin_k  "top0_bin"
#define top0_bin_sk "top0_bin_"
#define top0_bin_v  "11 22 33 44 55 66"

#define top_u320_k  "top0_u32#u32"
#define top_u320_sk "top0_u32"
#define top_u320_v  "89 37 123 241"


/* env_node  */
#define env_node_root "/env_node"

/* a_node */
#define root_a_node "/a_node"

#define a0_str_k "a0_str#str"
#define a0_str_sk "a0_str"
#define a0_str_v "TIGER"

#define a1_str_k "a1_str#str"
#define a1_str_sk "a1_str"
#define a1_str_v "RU"

/* ab_node */
#define root_ab_node "/a_node/ab_node"

#define ab0_str_k  "ab0_str#str"
#define ab0_str_sk "ab0_str"
#define ab0_str_v  "SUSHI"

/* ac_node */
#define root_ac_node "/a_node/ac_node"

#define ac0_str_k  "ac0_str#str"
#define ac0_str_sk "ac0_str"
#define ac0_str_v  "VOLVO"

#define ac2_str_k "ac2_str#str"
#define ac2_str_sk "ac2_str"
#define ac2_str_v "NISSAN"

/* b_node */
#define root_b_node "/b_node"

#define b0_str_k "b0_str#str"
#define b0_str_sk "b0_str"
#define b0_str_v "SHEEP"

/* bbb_node */
#define root_bbb_node "/b_node/bb_node/bbb_node"

#define bbb0_str_k "bbb0_str#str"
#define bbb0_str_sk "bbb0_str"
#define bbb0_str_v "HORSE"


#endif /* PARAMDB_DTB_NVPI3_TESTS__ */
