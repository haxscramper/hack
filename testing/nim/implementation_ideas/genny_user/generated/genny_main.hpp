#ifndef INCLUDE_GENNY_MAIN_H
#define INCLUDE_GENNY_MAIN_H
#include <cstring>
#include "nimcxx.hpp"

void *memcpy(void *dest, const void * src, std::size_t n);



// Nim Types
struct Obj { // cpp:213
  int64 field;
  inline Obj() { nimZeroMem(this, sizeof(*this)); }; // cpp:218
  Obj( int64 _field) { // cpp:228
    this->field = _field; // cpp:238
  }
  inline int64 get_data();
  struct items_iter {
    NimClosureIter impl;
    int64 operator*();
    bool operator!=(NimClosureEnd other);
  };
  inline items_iter items_begin();
  inline NimClosureEnd items_end();
};

// Nim DLL procs
extern "C" int64 genny_main_obj_get_data(Obj o); // cpp:62

// Implementation of 'items' iterator for obj
extern "C"  NimClosureIter genny_main_obj_items_begin(Obj obj);
extern "C"  int64 genny_main_obj_items_get_next(NimClosureIter* iter);
extern "C"  bool genny_main_obj_items_finished(NimClosureIter* iter);

// C++ methods
inline int64 Obj::get_data() { return genny_main_obj_get_data(*this); }
inline Obj::items_iter Obj::items_begin() {
  return Obj::items_iter{genny_main_obj_items_begin(*this)};
}

inline bool Obj::items_iter::operator!=(NimClosureEnd other) {
  return genny_main_obj_items_finished(&(this->impl));
}

inline int64 Obj::items_iter::operator*() {
  return genny_main_obj_items_get_next(&(this->impl));
}


#endif

