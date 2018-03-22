/**
 *   Test client header file.

 *   Copyright (C) 2016 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */


#ifndef _LTTNG_TEST_H
#define _LTTNG_TEST_H

#ifdef __cplusplus
extern "C" {
#endif


#define MULTI_SEG_TRACE_1 "1/5: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse mattis felis sed diam " \
     "commodo lacinia. Fusce elementum mi elementum libero aliquam faucibus. Cras ultrices augue " \
     "ac consequat scelerisque. Curabitur hendrerit sit amet ligula sed varius. Vivamus sollicitudin " \
     "augue eget dui molestie placerat. Sed vestibulum lacinia magna, id mollis dui blandit in. Nam sit " \
     "amet neque nec lorem pellentesque venenatis. Curabitur mollis ligula vel erat sollicitudin ultrices. " \
     "Proin aliquet facilisis ligula a bibendum. Donec ullamcorper tempor sodales. Nam non eros nec " \
     "lacus consequat sollicitudin. Nam eleifend eleifend gravida. Proin quis posuere massa. Proin " \
     "ac dui dictum, placerat nisi ut, dictum nulla. Donec eget tempus massa, sed sodales nisl. " \
     "Phasellus vulputate risus vel neque volutpat tincidunt."

#define MULTI_SEG_TRACE_2 "2/5: Pellentesque in libero vehicula, porta leo sed, rhoncus lorem. Suspendisse elementum nibh in " \
     "quam gravida, bibendum tempor odio dapibus. Nulla vel sem malesuada, sagittis nunc rutrum, fringilla " \
     "libero. Duis scelerisque pretium dui. Nunc neque orci, elementum sit amet convallis vel, lobortis in " \
     "diam. Nullam sed purus lobortis est blandit convallis in nec lacus. Cras vitae ipsum auctor, pharetra " \
     "neque sed, posuere lorem. Sed imperdiet nec erat adipiscing molestie. In in cursus turpis. Vestibulum " \
     "dolor neque, laoreet a tempor vel, consectetur vel ante. Sed volutpat, dolor vitae gravida sodales, " \
     "nunc nisi consequat nibh, ut sollicitudin erat purus ac turpis. Etiam luctus fringilla neque, sit amet " \
     "feugiat metus facilisis ac. Sed rutrum vulputate magna, ac sollicitudin nulla interdum eu."

#define MULTI_SEG_TRACE_3 "3/5: Donec a orci eget eros ullamcorper congue at et massa. Proin accumsan quis sapien at pretium. " \
     "Proin est est, venenatis non viverra in, tincidunt a dui. Vestibulum aliquam felis diam, quis viverra " \
     "ipsum elementum at. In porttitor tellus et fringilla dictum. Nullam vulputate fermentum turpis, quis " \
     "pulvinar justo varius dictum. Ut eu tempor lectus. Donec tempor aliquam diam. Aenean pretium accumsan " \
     "luctus. Nunc euismod ligula eget feugiat sodales. Suspendisse erat elit, sagittis ut massa quis, " \
     "scelerisque vestibulum orci."

#define MULTI_SEG_TRACE_4 "4/5: Praesent tempor mattis velit quis suscipit. Sed pulvinar felis velit, molestie porttitor ipsum " \
     "pellentesque non. Pellentesque sit amet nisl dolor. Mauris adipiscing justo eu porta tristique. Ut " \
     "laoreet, dui at tincidunt tempus, lectus ante semper libero, eget mattis elit sapien eget felis. " \
     "Donec aliquam scelerisque sapien faucibus fermentum. Mauris pellentesque magna sapien, eget pretium " \
     "est rutrum sit amet. Integer convallis posuere ornare. Phasellus congue sit amet massa id suscipit. " \
     "Aenean sed lectus a nisi varius egestas. Praesent nec mattis risus, ut tempus purus. In justo erat, " \
     "pretium sed aliquam sed, convallis nec nunc."

#define MULTI_SEG_TRACE_5 "5/5: Aenean iaculis urna eget justo vulputate aliquam. Proin mollis sollicitudin odio. Fusce auctor " \
     "nec risus at scelerisque. Aenean eget est ut lacus ultricies egestas. Ut eget turpis non erat posuere " \
     "tincidunt. Nullam id metus eu tellus egestas ornare condimentum et erat. Vestibulum euismod eu nisl quis " \
     "pretium. Aliquam fringilla feugiat bibendum. Sed augue mauris, vulputate ornare lectus tincidunt, " \
     "consequat pellentesque massa. Cras hendrerit lacus nec imperdiet adipiscing. Pellentesque non consectetur nulla."


extern void *lttng2_tri_test_2(void *arg);
extern void *lttng2_tri_kill_test(void *arg);
extern void *high_traceA(void *arg);
extern void *high_traceB(void *arg);
extern void *high_trace_defprocA(void *arg);
extern void *high_trace_defprocB(void *arg);

#ifdef __cplusplus
}
#endif

#endif /* _LTTNG_TEST_H */
