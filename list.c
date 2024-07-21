#include <stdio.h>
#include <stdlib.h>

typedef struct item {
	int val;
	struct item *prev;
	struct item *next;
} item;

item *new_item(int val) {
	item *it = (item *)malloc(sizeof(item));
	it->val = val;
	it->prev = NULL;
	it->next = NULL;
	return it;
}

typedef struct list {
	size_t length;
	item *first;
	item *last;
} list;

list *new_list() {
	list *ls = (list *)malloc(sizeof(list));
	ls->length = 0;
	return ls;
}

item *get(list *ls, int index) {
	if (ls->length == 0) {
		return NULL;
	}
	if (index >= ls->length) {
		// TODO: Out of bounds error
		return NULL;
	}

	item *it = ls->first;
	for (int i = 0; i < index; i++) {
		it = it->next;
	}
	return it;
}

void append(list *ls, int val) {
	item *it = new_item(val);

	if (ls->length == 0) {
		ls->first = it;
		ls->last = it;
		ls->length = 1;
	} else {
		item *last = ls->last;
		last->next = it;
		it->prev = last;
		ls->last = it;
		ls->length++;
	}
}

void insert(list *ls, int index, int val) {
	if (index > ls->length) {
		// TODO: Out of bounds error
		return;
	}

	if (index == ls->length) {
		append(ls, val);
		return;
	}

	if (index == 0) {
		item *it = new_item(val);
		ls->first = it;
		ls->last = it;
		ls->length++;
		return;
	}

	item *it = ls->first;
	for (int i = 0; i < index-1; i++) {
		it = it->next;
	}

	item *new_it = new_item(val);
	new_it->prev = it;
	new_it->next = it->next;
	it->next = new_it;

	if (index == ls->length - 1) {
		ls->last = new_it;
	}

	ls->length++;
}

void print(list *ls) {
	if (ls->length == 0) {
		return;
	}

	item *it = ls->first;
	for (int i = 0; i < ls->length; i++) {
		printf("%d ", it->val);
		it = it->next;
	}
	puts("");
}

int main() {
	list *ls = new_list();
	append(ls, 5);
	append(ls, 3);
	append(ls, 9);
	insert(ls, 0, 7);
	print(ls);

	printf("%d\n", get(ls, 1)->val);

	return 0;
}
