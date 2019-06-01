#include <iostream>

class Logger
{

  public:
    enum class Type
    {
        Info,
        Log,
    };

    ~Logger() {
        std::cout << "\b\n";
    }

    static Logger log(Logger::Type type = Logger::Type::Log) {
        switch (type) {
            case Type::Log: {
                std::cout << "  > ";
                break;
            }
            case Type::Info: {
                std::cout << "\033[32m--> \033[0m";
            }
            default: break;
        }
        return Logger();
    }

    template <class T>
    Logger& operator<<(T t) {
        std::cout << t << " ";
        return *this;
    }
};

class VoidLogger
{
  public:
    template <class T>
    VoidLogger& operator<<(T t) {
        return *this;
    }

    static VoidLogger log() {
        return VoidLogger();
    }
};

#ifdef DEBUG

#    define LOG Logger::log()
#    define INFO Logger::log(Logger::Type::Info)

#else

#    define LOG VoidLogger::log()
#    define INFO VoidLogger::log()

#endif
