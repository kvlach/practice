#include <stdio.h>
#include <stdlib.h>

typedef struct item {
	int value;
	struct item *prev, *next;
} item;

typedef struct list {
	size_t length;
	item *head, *tail;
} list;

item *new_item(int value) {
	item *it = (item *)malloc(sizeof(item));
	it->value = value;
	it->prev = NULL;
	it->next = NULL;
	return it;
}

list *new_list(size_t length) {
	if (length == 0) {
		return NULL;
	}

	list *ls = (list *)malloc(sizeof(list));
	ls->length = length;

	item *it = new_item(0);
	ls->head = it;
	for (int i = 0; i < length-1; i++) {
		it->next = new_item(0);
		it->next->prev = it;
		it = it->next;
	}
	ls->tail = it;

	return ls;
}

void expand(list *ls, size_t length) {
	ls->length += length;

	item *it = ls->tail;
	for (int i = 0; i < length; i++) {
		it->next = new_item(0);
		it->next->prev = it;
		it = it->next;
	}
	ls->tail = it;
}

item *set(list *ls, int index, int value) {
	if (index > ls->length-1) {
		return NULL;
	}

	item *it = ls->head;
	for (int i = 0; i < index; i++) {
		it = it->next;
	}
	it->value = value;
}

int get(list *ls, int index) {
	if (index > ls->length-1) {
		return NULL;
	}

	item *it = ls->head;
	for (int i = 0; i < index; i++) {
		it = it->next;
	}
	return it->value;
}

void delete(list *ls, int index) {
	if (index > ls->length-1) {
		// TODO: Error
		return;
	}

	item *it = ls->head;
	for (int i = 0; i < index; i++) {
		it = it->next;
	}


	if (it == ls->tail) {
		ls->tail = it->prev;
	} else {
		it->next->prev = it->prev;
		it->prev->next = it->next;
	}

	ls->length--;

	free(it);
}

void print(list *ls) {
	item *it = ls->head;

	for (int i = 0; i < ls->length; i++) {
		printf("%d ", it->value);
		it = it->next;
	}
	puts("");
}

int main() {
	list *ls = new_list(5);
	set(ls, 0, 10);
	set(ls, 4, 55);
	expand(ls, 2);
	set(ls, 2, 19);
	set(ls, 6, 66);
	delete(ls, 5);
	print(ls);
	printf("%d\n", get(ls, 2));

	return 0;
}
