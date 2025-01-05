#include <atomic>
#include <iostream>
#include <thread>

std::atomic<int> x{0};
std::atomic<int> y{0};
std::atomic<int> z{0};

void memory_order_relaxed() {
  x.store(1, std::memory_order_relaxed);
  int a = x.load(std::memory_order_relaxed);
}

void memory_order_consume() {
  y.store(2, std::memory_order_relaxed);
  // Consume relies on the dependency between y's value and the subsequent
  // operation
  int b = y.load(std::memory_order_consume) * 3;
}

void memory_order_acquire_release() {
  z.store(3, std::memory_order_release); // Ensure all prior stores are visible
                                         // before this
  int c = z.load(std::memory_order_acquire); // Ensure no memory reordering
                                             // happens after this
}

void memory_order_seq_cst() {
  // Sequentially consistent ensures a single total order across threads
  y.store(5, std::memory_order_seq_cst);
  int e = y.load(std::memory_order_seq_cst);
}

int main() {
  std::thread t1(memory_order_relaxed);
  std::thread t2(memory_order_consume);
  std::thread t3(memory_order_acquire_release);
  std::thread t5(memory_order_seq_cst);

  t1.join();
  t2.join();
  t3.join();
  t5.join();

  return 0;
}
