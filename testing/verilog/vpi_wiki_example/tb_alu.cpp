#include <stdlib.h>
#include <iostream>
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Valu.h"
#include "Valu___024unit.h"

#define MAX_SIM_TIME 20
vluint64_t sim_time = 0;

int main(int argc, char** argv, char** env) {
    Valu* dut = new Valu;

    Verilated::traceEverOn(true);
    VerilatedVcdC* m_trace = new VerilatedVcdC;
    dut->trace(m_trace, 5);
    m_trace->open("waveform.vcd");

    while (sim_time < MAX_SIM_TIME) {
        dut->rst = 1;
        if (sim_time > 1 && sim_time < 5) {
            dut->a_in     = 0;
            dut->b_in     = 0;
            dut->op_in    = 0;
            dut->in_valid = 0;
        }

        dut->clk ^= 1;
        dut->eval();
        m_trace->dump(sim_time);
        sim_time++;

        std::cout << "Sim time " << sim_time //
                  << " out [" << dut->out << "]" << std::endl;
    }

    m_trace->close();
    delete dut;
    exit(EXIT_SUCCESS);
}
