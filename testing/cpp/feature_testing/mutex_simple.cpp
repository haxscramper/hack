#include <iostream>
// include init list in order to be able to use literal sequences
#include "util.hpp"
#include <chrono>
#include <initializer_list>
#include <mutex>

#include <string>
#include <thread>

#include <gtest/gtest.h>

// mutex to protect data in the critical section
std::mutex mtx{};
// atomic data must be trivially copyable, so had to use pointer here.
std::atomic<std::string *> output{};

void print_block(int n, char c, bool use_mutex) {
  if (use_mutex) {
    mtx.lock();
  }
  for (int i = 0; i < n; ++i) {
    // pause for millisecond to allow threads to steal the execution
    // from each other in case of no-mutex implementation
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
    (*output.load()) += c;
  }
  if (use_mutex) {
    mtx.unlock();
  }
}

TEST(Mutex, Simple) {
  std::string data{};
  output.store(&data);
  for (bool mutex : {true, false}) {
    std::thread th1(print_block, 50, '*', mutex);
    std::thread th2(print_block, 50, '$', mutex);
    th1.join();
    th2.join();
    data += "\n";
  }

  LOG_INFO(ol_log(), "{}", data);
}
