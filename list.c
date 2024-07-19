#include <stdio.h>
#include <stdlib.h>

typedef struct item {
	int value;
	struct item *prev;
	struct item *next;
} item;

item *new_item

typedef struct list {
	int length;
	int size;
	struct item *first;
} list;

list *new_list(size_t len) {
	list *ls = (list *) malloc(sizeof(list));
	ls->length = 0;
	ls->size = len;
	return ls;
}

void append(list *ls, int val) {
	if (ls->length == 0) {
		ls->length++;
		ls->first = item { value: val };
		return;
	}

	if (ls->length > ls->size) {
		// TODO: Error
	}

	item *it = ls->first;
	for (int i = 0; i < ls->length; i++) {
		it = it->next;
	}
	ls->length++;
	it->next = item { value: val, prev: it };
}

void print(list *ls) {
	if (ls->length == 0) {
		return;
	}

	item *it = ls->first;
	for (int i = 0; i < ls->length; i++) {
		printf("%d ", it->value);
		it = it->next;
	}
	printf("\n");
}

int main() {
	list *nums = new_list(5);
	append(nums, 2);
	append(nums, 6);
	print(nums);
}
