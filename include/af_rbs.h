#ifndef AF_RBS_H__
#define AF_RBS_H__

#include <net/sock.h>

#include <uapi/linux/rbs/af_rbs.h>

struct rbs_sock {
	struct sock sk;
	int dead;
	void *proto_data; /* to be used by protocol */
};

struct rbs_proto_ops {
	int (*init)(struct rbs_sock *, void *);
	void (*done)(struct rbs_sock *);
	int (*ioctl)(struct rbs_sock *, unsigned int, unsigned long);
	int (*compat_ioctl)(struct rbs_sock *, unsigned int, unsigned long);
	int (*oob_avail)(struct rbs_sock *);
	void *(*oob_recv)(struct rbs_sock *, int *);
	int (*bind)(struct rbs_sock *, struct sockaddr *, int);
	int (*sendmsg)(struct rbs_sock *, void *, int, struct sk_buff *);
	int (*sockaddr)(struct rbs_sock *, void *, int, int *);
	int (*mmap)(struct rbs_sock *, struct vm_area_struct *);
};

struct rbs_proto_mcgrp;

extern int rbs_proto_register(int, struct rbs_proto_ops *, void *);
extern void rbs_proto_unregister(int, struct rbs_proto_ops *);
extern int rbs_proto_send(struct rbs_sock *, struct sk_buff *);
extern void rbs_proto_oobavail(int);

extern struct rbs_proto_mcgrp *rbs_proto_mcgrp_alloc(int, int);
extern int rbs_proto_mcgrp_add(struct rbs_proto_mcgrp *, struct rbs_sock *);
extern void rbs_proto_mcgrp_remove(struct rbs_proto_mcgrp *,
	struct rbs_sock *);
extern int rbs_proto_mcgrp_send(struct rbs_proto_mcgrp *, struct sk_buff *);
extern int rbs_proto_mcgrp_post(struct rbs_proto_mcgrp *, void *, int);
extern void rbs_proto_mcgrp_free(struct rbs_proto_mcgrp *);
extern int rbs_proto_mcgrp_cb_add(struct rbs_proto_mcgrp *mcgrp,
				  int (*sk_cb)(struct rbs_sock *, void *));
extern int rbs_proto_mcgrp_send_wf(struct rbs_proto_mcgrp *mcgrp,
		struct sk_buff *skb,
		int (*filter)(struct rbs_sock *, void *), void *fparam);


#endif /* AF_RBS_H__ */
