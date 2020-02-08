// main.cpp
#include <iostream>
#include "mylib.h"

using namespace std;

int main() {
  NimMain();
  std::cout << "calling nim code that calls c++ code\n";
  std::cout << nimLogic("one,two,three") << "\n";
  return 0;
}
