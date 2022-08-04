#include <iostream>
#include <atomic>
#include <thread>

std::atomic<int> x, y;

void task_1() {
    x.store(123);
    y.store(345);
}

void task_2() {
    std::cout << "x = " << x.load() << std::endl;
    std::cout << "y = " << y.load() << std::endl;
}

int main() {
    std::thread t1{task_1};
    std::thread t2{task_2};

    t1.join();
    t2.join();

    std::cout << "finished\n";
}
