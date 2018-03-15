#ifndef DL_LIST_H__
#define DL_LIST_H__

#include <stddef.h>

struct dl_list {
	struct dl_list *next;
	struct dl_list *prev;
};

/* gcc -pedantic friendly container_of */
#undef container_of
#define container_of(ptr, type, member) \
	(type *)(((char *) ((typeof( ((type *)0)->member) *)(ptr))) - \
			offsetof(type, member))

#define dl_list_entry(_elm, _type, _field) \
	container_of(_elm, _type, _field)


static inline void dl_list_init(struct dl_list *head)
{
	head->next = head;
	head->prev = head;
}

static inline struct dl_list *dl_list_next(struct dl_list *elm)
{
	return elm->next;
}

static inline int dl_list_empty(struct dl_list *head)
{
	return (head->next == head) ? 1 : 0;
}

static inline void dl_list_insert_tail(struct dl_list *head,
		struct dl_list *elm)
{
	struct dl_list *tail = head->prev;
	
	tail->next->prev = elm;
	elm->next = tail->next;
	tail->next = elm;
	elm->prev = tail;
}

static inline void dl_list_insert_before(struct dl_list *fore,
		struct dl_list *elm)
{
	elm->prev = fore->prev;
	elm->next = fore;
	fore->prev = elm;
	elm->prev->next = elm;
}

static inline void dl_list_insert_head(struct dl_list *head,
		struct dl_list *elm)
{
	dl_list_insert_before(head->next, elm);
}

static inline void dl_list_remove(struct dl_list *elm)
{
	elm->next->prev = elm->prev;
	elm->prev->next = elm->next;
	elm->next = elm;
	elm->prev = elm;
}

static inline struct dl_list *dl_list_first(struct dl_list *head)
{
	if (head->next == head)
		return NULL;
	return head->next;
}

#define dl_list_first_entry(_head, _type, _field) \
	dl_list_entry((_head)->next, _type, _field)

#define dl_list_foreach(_it, _head, _field) \
	for (_it = dl_list_entry((_head)->next, typeof(*_it), _field); \
	     &_it->_field != (_head); \
	     _it = dl_list_entry(_it->_field.next, typeof(*_it), _field))

#define dl_list_foreach_safe(_it, _tmp, _head, _field) \
	for (_it = dl_list_entry((_head)->next, typeof(*_it), _field), \
	     _tmp = dl_list_entry(_it->_field.next, typeof(*_tmp), _field); \
	     &_it->_field != (_head); \
	     _it = _tmp, \
	     _tmp= dl_list_entry(_tmp->_field.next, typeof(*_tmp), _field))

#endif /* !RHAI_LIST_H__ */
