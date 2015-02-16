#include "utlist.h"

typedef struct item {
  int id;
  struct item *prev, *next;
} item;

struct item *list = NULL;

int main() {
  struct item *item;
  /* ... allocate and populate item ... */
  DL_APPEND(list, item);
}
