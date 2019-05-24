#include <cstring>
#include <iostream>
#include <map>
#include <utility>

/*
  - [ ] TODO write hash function for unordered_multimap and use ti
*/

class C;

// ОПисываем новый тип переменных. Вместо того чтобы каждый раз каждый
// раз писать ~void (C::*)(std::string&)~ мы можем писать ~slot_func~
// или ~signal_func~. Это *типы* переменных, также как и ~int~ или
// ~char~. То есть если бы мы написали ~slot_func var~ это было бы
// тоже самое что ~void (C::var*)(std::string&)~ --- в обоих случаях
// это означает "указатель на метод класса C, принимающий строку и
// ничего не возвращающий (тип возвращаемого значения ~void~)"
using slot_func   = void (C::*)(std::string&);
using signal_func = void (C::*)(std::string&);


struct signal_compare {
    bool operator()(const signal_func& lhs, const signal_func& rhs) {
        return std::memcmp(&lhs, &rhs, sizeof(lhs)) < 0;
    }
};


using signal_map = std::multimap<
    signal_func,              //
    std::pair<C*, slot_func>, //
    signal_compare>;

using signal_iter = signal_map::iterator;


// Класс для примера, у него мы определим две функции-сигнала и две
// функции-слот. Они ничем не отличаются от обычных функций за
// исключением некоторого дополнительного смысла который мы на них
// повесим (то что они являются сигналами и слотами)
class C
{
    // API
  public:
    // Функция для создания нового подключения между двумя объектами:
    // текущий объект (тот на который указывает ~this~) является
    // объектом который будем "испускать" сигнал, второй объект (на
    // который указывает указатель ~target~) будет принимать сигнал.
    void set_connection(
        signal_func signal, // К какому сигналу класса ~C~ подключать
                            // целевой объект и его слот
        C* target, // Целевой обект (у него будет вызыват слот который мы
                   // подключаем)
        slot_func slot // Слот целевого объекта (это функцию мы будет
                       // вызывать)
    ) {
        // Так как у нас есть ~std::multimap<signal_func, std::pair<C*,
        // slot_func>~ то мы пользуемся методом ~std::multimap::insert~
        // (это метод который позволяет добавить новый элемент в
        // контейнер).
        connects.insert( // Мы ассоциируем
            {
                signal,        // сигнал
                {target, slot} // с парой объект-слот
            });
    }

    void emit_signal(signal_func signal, std::string& data) {
        std::pair<signal_iter, signal_iter>
            equal_range = connects.equal_range(signal);

        for (signal_iter slot = equal_range.first;
             slot != equal_range.second;
             ++slot) {

            C*          target = slot->second.first;
            signal_func signal = slot->second.second;

            // У нас есть объект ~target~ и метод класса ~C~ (этот
            // метод является одним их слотов но мы не знаем точно
            // каким: это может быть любой и двух слотов. Единственное
            // что мы знаем это то что этот метод принимает
            // ~std::string~ и ничего не возвращает)
            (target->*signal)(data);
        }
    }


    // Signals
  public:
    // Определяем сигналы. Как уже говорилось это простоые функции,
    // которые ничем не отличаются от обычных. Когда функция-сигнал
    // выполняется то она просто вызывает ~C::emit_signal~, передавая
    // переданный ей аргумент дальше.
    void signal_1(std::string& arg) {
        std::cout << "Executed signal 1\n";
        emit_signal(&C::signal_1, arg);
    }

    void signal_2(std::string& arg) {
        std::cout << "Executed signal 2\n";
        emit_signal(&C::signal_2, arg);
    }

    // Slots
  public:
    // Фукнции-слоты еще более обычни: они не вызывают никаких другий
    // функций а просто печатают свои аргумент
    void slot_1(std::string& arg) {
        std::cout << "Called slot 1" << arg;
    }

    void slot_2(std::string& arg) {
        std::cout << "Called slot 2" << arg;
    }

  private:
    signal_map connects;
};


int main() {
    C c;

    c.set_connection(&C::signal_1, &c, &C::slot_1);
    std::string arg = "hello\n";
    c.signal_1(arg);

    std::cout << "Hello\n";
}
