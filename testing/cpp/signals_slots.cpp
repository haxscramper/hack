#include <cstring>
#include <iostream>
#include <map>
#include <utility>

/*
  - [ ] TODO write hash function for unordered_multimap and use ti
*/

class C;


// На функцию можно взять указатель также как это делается для обычных
// объектов: используя оператор & и саму функцию мы можем получиьт ее
// адресс: ~&some_funcion~ даст нам указатель на функцию. Для того
// чтобы хранить значенои подобного выражения нам нужна переменная
// типа "указатель на функцию". Подобный тип описывается следующим
// образом: ~<тип возвращаемого значения> (*<называние
// переменной-указателя> )(<тип принимаемых значений>)~. Например,
// если у нас есть функция, принимающая целое число и возвращающая
// целое число (например мы определим ее как ~int get_number(int val)
// { return var +1 ;} (она будт просто возвращать значение переменной,
// увеличенное на 1)) то указатель на функцию подобного типа будет
// определяться как ~int (*pointer_var)(int)~. В типе принимаемого
// значения не пишется название аргументов, просто их типы.

// Для того чтобы вызвать фукнцию по указателью нужно использовать
// слудующий синтаксис: ~(<переменная-указатель>)(<аргументы которые
// юудут переданы в функцию>)~

// Пример

int get_number(int val) {
    return val + 1;
}

void func_pointer_test() {
    // Берем указатель на функцию
    int         // Тип возвращаемого значания
        (*      // Указатель на простую функцию
         func   // Название переменной
         )(int) // Тип принимаемого значания
        = &     // Берем адрес
          get_number; // У функции
    // Вызываем функцию, должно напечатать 1
    std::cout << " [ " << func(0) << " ] \n";
}

// Также можно взять указатель на метод класса, но из-за неявного
// параметра ~this~ и некоторых других особенностей указатели на
// функции и указатели на методы отличаются.

// Если есть класс и у него есть метод и переменная:

class K
{
  public:
    int get_number(int val) {
        return this->increment + val;
    }
    int increment = 0;
};

void method_pointer_test() {
    K  val;               // Создаем объект
    K* ptr        = &val; // Берем его адресс
    val.increment = 10; // Изменяем значание переменной класса

    // Берем указатель на метод
    int // Тип возвращаемого значания int
        (K::* // Является указателем на метод класса K
             method_pointer // Название переменной
         )(int)          // Как аргумент принимает int
        = &              // Берем адрес
          K::get_number; // У метода K::get_number


    // Должно напечатать 10 в обоих случаях так как ~val~ и ~ptr~
    // работают с одним и тем же объектом. Важно обратить внимание на
    // то что оператор ->* это *одни* оператор, не нескольо! То же
    // самое и с .* --- это один оператор.
    std::cout << " [ " << (ptr->*method_pointer)(0) << " ] \n";
    //                    [          1         ][2]
    // В [1] мы получаем саму фукнцию и в [2] мы вызывам ее с какими-то
    // аргументами.
    std::cout << " [ " << (val.*method_pointer)(0) << " ] \n";
}


// ОПисываем новый тип переменных. Вместо того чтобы каждый раз каждый
// раз писать ~void (C::*)(std::string&)~ мы можем писать ~slot_func~
// или ~signal_func~. Это *типы* переменных, также как и ~int~ или
// ~char~. То есть если бы мы написали ~slot_func var~ это было бы
// тоже самое что ~void (C::*var)(std::string&)~ --- в обоих случаях
// это означает "указатель на метод класса C, принимающий строку и
// ничего не возвращающий (тип возвращаемого значения ~void~)"
using slot_func   = void (C::*)(std::string&);
using signal_func = void (C::*)(std::string&);


// Из-за внутренних особенностех std::multimap ключи, используемые для
// получения объектов должны поддерживать операцию сравнения (больше
// или меньше). Не имеет приципиального значения, каким образом
// элементы должны сравниваться, главное чтобы их можно было
// упорядочить по возрастанию и по убыванию. Не все типы переменных
// поддерживают подобно сравнение изначально, для некоторых нужно
// писать специальный класс-сраниватель.
struct signal_compare {
    // Структура для сравнения должна реализовывать только один оператор
    // (т.е. не нужно писать конструктор, деструктор и т.д.) ---
    // оператор ~(T& lhs, T& rhs)~.
    bool operator()(const signal_func& lhs, const signal_func& rhs) {
        // Как уже говорилось не имеет значания, как конкретно
        // сравниваются объекты, нужно просто чтобы они были
        // упорядочены. Здесь используется ~std::memcmp~ который
        // сравнивает дво объекта побайтово. Эта функция принимает на
        // вход два указателя на объекты и их размер в байтах.
        return std::memcmp(&lhs, &rhs, sizeof(lhs)) < 0;
    }
};

// Каждому сигналу соответсвует несколько пар объект-слот (точнее
// *указатель* на объект и *указатель на функцию-слот). Одному сигналу
// соответсвует несколько таких пар.
using signal_map = std::multimap<
    signal_func,              //
    std::pair<C*, slot_func>, //
    signal_compare>;

// Еще одно объявление типа, для того чтобы не писать каждый раз
// signal_map::iterator
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
        connects.insert( // Мы ассоциируем (добавляем новый элемент в
                         // контейнер)
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
        std::cout << "Called slot 1: " << arg;
    }

    void slot_2(std::string& arg) {
        std::cout << "Called slot 2: " << arg;
    }

  private:
    signal_map connects;
};

void signal_slots_test() {
    {
        std::cout << "test 1\n\n\n";

        C c;

        c.set_connection(&C::signal_1, &c, &C::slot_1);
        std::string arg = "hello\n";
        c.signal_1(arg);
    }

    {
        std::cout << "test 2\n\n\n";

        C emitter;
        C reciever;

        std::string arg = "argument string\n";

        emitter.set_connection(&C::signal_2, &reciever, &C::slot_2);

        std::cout << "testing signal 2\n";
        emitter.signal_2(arg); // Должно вызвать второй слот

        std::cout << "testing signal 1\n";
        emitter.signal_1(arg); // Никакх слотов вызвано не будет так
                               // как подключений нет
    }
}


int main() {
    func_pointer_test();
    method_pointer_test();
    signal_slots_test();
}