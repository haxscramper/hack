#ifndef INCLUDE_GENNY_MAIN_H
#define INCLUDE_GENNY_MAIN_H
void *memcpy(void *dest, const void * src, size_t n);

typedef struct Obj {
  long long field;
} Obj;
Obj obj(long long field) {
  Obj result;
  result.field = field;
  return result;
}

#endif
