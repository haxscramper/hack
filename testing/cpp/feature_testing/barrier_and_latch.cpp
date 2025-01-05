#include "util.hpp"
#include <gtest/gtest.h>

#include <barrier>
#include <format>
#include <string>
#include <thread>
#include <vector>

TEST(BarrierTest, BasicUsage) {
  constexpr int num_threads = 4;
  std::barrier barrier(num_threads, []() {
    static int phase = 0;
    LOG_INFO(ol_log(), "Phase {} completed", ++phase);
  });

  std::vector<std::thread> threads;
  std::vector<std::string> results(num_threads);

  for (int i = 0; i < num_threads; ++i) {
    threads.emplace_back([&, i]() {
      results[i] = std::format("Thread {} reached barrier", i);
      barrier.arrive_and_wait();
      results[i] += " and proceeded";
    });
  }

  for (auto &t : threads) {
    t.join();
  }

  for (const auto &result : results) {
    LOG_INFO(ol_log(), "{}", result);
  }
}
