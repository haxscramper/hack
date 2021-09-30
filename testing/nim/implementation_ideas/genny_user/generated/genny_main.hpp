#ifndef INCLUDE_GENNY_MAIN_H
#define INCLUDE_GENNY_MAIN_H
#include <cstring>

void *memcpy(void *dest, const void * src, std::size_t n);



// Nim Types
struct Obj { // cpp:168
  long long field;
  Obj() = default; // cpp:173
  Obj( long long _field) { // cpp:183
    this->field = _field; // cpp:193
  }
  inline long long get_data();
};

// Nim DLL procs
extern "C" long long genny_main_obj_get_data(Obj o); // cpp:68


// C++ methods
inline long long Obj::get_data() { return genny_main_obj_get_data(*this); }


#endif

