/**
 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

#ifndef TIMET_FIX_H
#define TIMET_FIX_H

/* time_t variables are swapped throughout the code with time_t32
   so that it will be easy to later find a more suitable solution
   for the time_t problem seen on x64 architecture. 
   The problem is that on x64 time_t is 64 bit in length and all the
   erlang interfaces expect time_t to be 32 bits in length */
typedef int32_t time_t32;

#endif /* TIMET_FIX_H */
