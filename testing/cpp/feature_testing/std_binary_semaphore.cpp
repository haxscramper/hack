#include "util.hpp"
#include <gtest/gtest.h>
#include <iostream>
#include <semaphore>
#include <thread>

// global binary semaphore instances
// object counts are set to zero
// objects are in non-signaled state
std::binary_semaphore smphSignalMainToThread{0}, smphSignalThreadToMain{0};

void ThreadProc() {
  // wait for a signal from the main proc
  // by attempting to decrement the semaphore
  smphSignalMainToThread.acquire();

  // this call blocks until the semaphore's count
  // is increased from the main proc

  LOG_INFO(ol_log(), "[thread] Got the signal"); // response message

  // wait for 3 seconds to imitate some work
  // being done by the thread
  using namespace std::literals;
  std::this_thread::sleep_for(3s);

  LOG_INFO(ol_log(), "[thread] Send the signal"); // message

  // signal the main proc back
  smphSignalThreadToMain.release();
}

TEST(Semaphore, BinarySemaphore) {
  // create some worker thread
  std::thread thrWorker(ThreadProc);

  LOG_INFO(ol_log(), "[main] Send the signal"); // message

  // signal the worker thread to start working
  // by increasing the semaphore's count
  smphSignalMainToThread.release();

  // wait until the worker thread is done doing the work
  // by attempting to decrement the semaphore's count
  smphSignalThreadToMain.acquire();

  LOG_INFO(ol_log(), "[main] Got the signal"); // response message
  thrWorker.join();
}

std::vector<int> myVec{};

std::counting_semaphore<1> prepareSignal(0); // (1)

void prepareWork() {
  myVec.insert(myVec.end(), {0, 1, 0, 3});
  LOG_INFO(ol_log(), "Sender: Data prepared.");
  prepareSignal.release(); // (2)
}

void completeWork() {
  LOG_INFO(ol_log(), "Waiter: Waiting for data.");
  prepareSignal.acquire(); // (3)
  myVec[2] = 2;
  LOG_INFO(ol_log(), "Waiter: Complete the work.");
  for (auto i : myVec) {
    LOG_INFO(ol_log(), "{}", i);
  }
}

TEST(Semaphore, CountingSemaphore) {
  std::thread t1(prepareWork);
  std::thread t2(completeWork);
  t1.join();
  t2.join();
}
