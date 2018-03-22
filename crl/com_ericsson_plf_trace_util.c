/**
 *   Tracepoint c file for tracelogger
 *
 *   @file com_ericsson_plf_trace_util.c
 *
 *   Copyright (C) 2015 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */
/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2015-01-27 Nils Carlson <nils.carlson@ericsson.com>
 *   Change  : First revision. Defines tracepoints.
 *   -----------------------
 */

#define TRACEPOINT_CREATE_PROBES
/*
 * The header containing our TRACEPOINT_EVENTs.
 */
#define TRACEPOINT_DEFINE
#include "com_ericsson_plf_trace_util.h"
