#include "../../common/cpp/macro.hpp"
#include "../../common/cpp/using.hpp"

#include "../../common/cpp/string_utils.hpp"

#define DEBUG
#include "signals_slots.hpp"

using Num           = int;
using ResultMessage = std::tuple<Num, char, Num, Num>;


class Printer : public signal_base
{
  public:
    void slot_accept_results(signal_msg _res) {
        ResultMessage res = scastp<ResultMessage>(_res);
        LOG << std::get<0>(res) << std::get<1>(res) << std::get<2>(res)
            << "=" << std::get<3>(res);
    }
};

class Parser : public signal_base
{
  public:
    void start_reading() {
        Str res = "1";
        for_i(op, 20) {
            switch (rand() % 4) {
                case 0: res += " +"; break;
                case 1: res += " -"; break;
                case 2: res += " *"; break;
                case 3: res += " %"; break;
            }

            res += " " + std::to_string(rand() % 100 + 1);
        }

        LOG << res;

        Vec<Str> tokens = split(res, " ");

        for_i(i, tokens.size()) {
            switch (i % 2) {
                case 0: signal_export_operand(&tokens[i]); break;
                case 1:
                    signal_export_operator(&tokens[i][0]);
                    signal_require_operations(nullptr);
                    break;
            }
        }
    }

    void signal_export_operand(signal_msg operand) {
        emit_signal(signal_cast(&Parser::signal_export_operand), operand);
    }

    void signal_export_operator(signal_msg op) {
        emit_signal(signal_cast(&Parser::signal_export_operator), op);
    }

    void signal_require_operations(signal_msg tmp) {
        emit_signal(signal_cast(&Parser::signal_require_operations), tmp);
    }
};

class OpRunner : public signal_base
{
  public:
    void slot_accept_operand(signal_msg operand) {
        rhsOp = std::stoi(scastp<Str>(operand));
    }

    void slot_accept_operator(signal_msg _op) {
        op = scastp<char>(_op);
    }

    void slot_run_operation(signal_msg tmp) {
        ResultMessage msg = {res, op, rhsOp, 0};

        switch (op) {
            case '-': res = res - rhsOp; break;
            case '+': res = res + rhsOp; break;
            case '*': res = res * rhsOp; break;
            case '%': res = res % rhsOp; break;
        }

        std::get<3>(msg) = res;

        signal_compute_finished(&msg);
    }

    void signal_compute_finished(signal_msg res) {
        emit_signal(signal_cast(&OpRunner::signal_compute_finished), res);
    }

  private:
    Num  rhsOp;
    char op;
    Num  res = 0;
};

int main() {
    Printer  printer;
    Parser   parser;
    OpRunner opRunner;

    connect(
        &opRunner,
        &OpRunner::signal_compute_finished,
        &printer,
        &Printer::slot_accept_results);

    connect(
        &parser,
        &Parser::signal_export_operand,
        &opRunner,
        &OpRunner::slot_accept_operand);

    connect(
        &parser,
        &Parser::signal_export_operator,
        &opRunner,
        &OpRunner::slot_accept_operator);

    connect(
        &parser,
        &Parser::signal_require_operations,
        &opRunner,
        &OpRunner::slot_run_operation);


    printer.print_connections();
    parser.print_connections();
    opRunner.print_connections();

    parser.start_reading();

    LOG << "Done main";
}
