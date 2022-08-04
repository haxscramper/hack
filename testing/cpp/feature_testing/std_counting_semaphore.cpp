#include <iostream>
#include <semaphore>
#include <thread>
#include <vector>

std::vector<int> myVec{};

std::counting_semaphore<1> prepareSignal(0); // (1)

void prepareWork() {
    myVec.insert(myVec.end(), {0, 1, 0, 3});
    std::cout << "Sender: Data prepared." << '\n';
    prepareSignal.release(); // (2)
}

void completeWork() {
    std::cout << "Waiter: Waiting for data." << '\n';
    prepareSignal.acquire(); // (3)
    myVec[2] = 2;
    std::cout << "Waiter: Complete the work." << '\n';
    for (auto i : myVec) {
        std::cout << i << " ";
    }
    std::cout << '\n';
}

int main() {
    std::thread t1(prepareWork);
    std::thread t2(completeWork);
    t1.join();
    t2.join();
    std::cout << '\n';
}
