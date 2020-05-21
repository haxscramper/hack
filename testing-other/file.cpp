#include <iostream>

class Calculator
{
    int result;

  public:
    void compute(int lhs, char op, int rhs) {
        switch (op) {
            case '+': result = lhs + rhs; break;
            case '-': result = lhs - rhs; break;
            case '%': result = lhs % rhs; break;
            case '*': result = lhs * rhs; break;
        };
    }

    void compute(char op, int rhs) {
        compute(result, op, rhs);
    }

    int getResult() const {
        return result;
    }
};

int main() {
    Calculator calc;

    int  lhs = 0;
    int  rhs = 0;
    char op;

    std::cin >> lhs >> op >> rhs;
    calc.compute(lhs, op, rhs);

    std::cin >> op >> rhs;
    calc.compute(op, rhs);

    std::cin >> op >> rhs;
    calc.compute(op, rhs);

    std::cout << calc.getResult();
}
