#include <stdlib.h>
#include <stdio.h>

typedef struct item {
	int val;
	struct item *prev;
	struct item *next;
} item;

typedef struct list {
	size_t length;
	size_t size;
	item *first;
	item *last;
} list;

list *new_list(size_t sz) {
	list *ls = (list *)malloc(sizeof(list));
	ls->size = sz;
	ls->length = 0;
	ls->first = NULL;
	ls->last = NULL;
	return ls;
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

void append(list *ls, int val) {
	if (ls->length >= ls->size) {
		// TODO: Error, can't append item
		return;
	}

	item *it = (item *)malloc(sizeof(item));
	it->val = val;
	it->prev = NULL;
	it->next = NULL;

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

int main() {
	list *nums = new_list(5);
	append(nums, 4);
	append(nums, 6);
	append(nums, 7);
	print(nums);

	return 0;
}
