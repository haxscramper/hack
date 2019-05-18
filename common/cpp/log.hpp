#include <iostream>

class Logger
{
  public:
    ~Logger() {
        std::cout << "\b\n";
    }

    static Logger log() {
        std::cout << "  > ";
        return Logger();
    }

    template <class T>
    Logger& operator<<(T t) {
        std::cout << t << " ";
        return *this;
    }
};

#define LOG Logger::log()
