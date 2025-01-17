#include "util.hpp"
#include <future>
#include <gtest/gtest.h>
#include <mutex>
#include <string>

std::mutex m;

void foo(int i, const std::string &str) {
  std::lock_guard<std::mutex> _guard{m};
  LOG_INFO(ol_log(), "foo {} {}", str, i);
}

void bar(const std::string &str) {
  std::lock_guard<std::mutex> _guard{m};
  LOG_INFO(ol_log(), "bar {}", str);
}

int baz(int i) {
  std::lock_guard<std::mutex> _guard{m};
  LOG_INFO(ol_log(), "{}", i);
  return i + 10;
}

TEST(Async, Simple) {
  // Calls foo(42, "Hello") with default policy:
  // may print "Hello 42" concurrently or defer execution
  std::future<void> a1 = std::async(foo, 42, "Hello");
  // Calls bar("world!") with deferred policy
  // prints "world!" when a2.get() or a2.wait() is called
  std::future<void> a2 = std::async(std::launch::deferred, bar, "world!");
  // Calls baz(43); with async policy
  // prints "43" concurrently
  std::future<int> a3 = std::async(std::launch::async, baz, 43);
  a2.wait();                          // prints "world!"
  LOG_INFO(ol_log(), "{}", a3.get()); // prints "53"
} // if a1 is not done at this point, destructor of a1 prints "Hello 42"
  // here
