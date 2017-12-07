#include <stdlib.h>
#include <string.h>

#include "list.h"

#define EMPTYLIST NULL

void init_list(struct list *l,int keysize) {
	l->first=l->last=NULL;
	l->keysize=keysize;
	l->count=0;
}

struct list_node *add_elem(struct list *l,void *elem) {
	struct list_node *newnode=(struct list_node*)malloc(sizeof(struct list_node));
	newnode->data=elem;
	newnode->previous=l->last;
	newnode->next=NULL;
	if (l->count==0) {
		l->first=l->last=newnode;
	}
	else {
		l->last->next=newnode;
		l->last=newnode;
	}
	l->count++;
	return newnode;
}

void delete_node(struct list *l,struct list_node *node) {
	if (l->count==1) {
		l->first=l->last=NULL;
	}
	else if (node==l->first) {
		node->next->previous=NULL;
		l->first=node->next;
	}
	else if (node==l->last) {
		node->previous->next=NULL;
		l->last=node->previous;
	}
	else {
		node->previous->next=node->next;
		node->next->previous=node->previous;
	}
	l->count--;
	free(node);
	node = NULL;
}

void destroy_node(struct list *l,struct list_node *node) {
	free(node->data);
	node->data=NULL;
	delete_node(l,node);
}

int is_EMPTYLIST_list(struct list *l) {
	return (l->count==0?TRUE:FALSE);
}

int get_list_count(struct list *l) {
	return l->count;
}

void *first_elem(struct list *l) {
	return l->first->data;
}

struct list_node *first_node(struct list *l) {
	return l->first;
}

void *last_elem(struct list *l) {
	return l->last->data;
}

struct list_node *last_node(struct list *l) {
	return l->last;
}

struct list_node *xlocate_node(struct list *l,void *elem,int offset,int length) {
	struct list_node *tmp;
	tmp=l->first;
	while(tmp!=NULL) {
		if(!memcmp((char*)tmp->data+offset,elem,length==0?l->keysize:length)) return (tmp);
		tmp=tmp->next;
	}
	return EMPTYLIST;
}

struct list_node *locate_node(struct list *l,void *elem) {
	return(xlocate_node(l,elem,0,0));
}

void *xlocate_elem(struct list *l,void *elem,int offset,int length) {
	struct list_node *node=xlocate_node(l,elem,offset,length);
	return(node==NULL?NULL:node->data);
}

void *locate_elem(struct list *l,void *elem) {
	return(xlocate_elem(l,elem,0,0));
}

void clear_list(struct list *l) {
	struct list_node *tmp;
	while(l->first!=EMPTYLIST) {
		tmp=l->first;
		l->first=l->first->next;
		free(tmp);
		tmp=NULL;
	}
	l->last=EMPTYLIST;
	l->count=0;
}

void destroy_list(struct list *l) {
	struct list_node *tmp;
	while(l->first!=EMPTYLIST) {
		tmp=l->first;
		l->first=l->first->next;
		free(tmp->data);
		tmp->data=NULL;
		free(tmp);
		tmp=NULL;
	}
	l->last=EMPTYLIST;
	l->count=0;
}
