// [1,2,3,,4,4,4]


// [R1] -- register
// [R2] -- register
// [PTR] --
// [R3] -- store array size
// [FLAG] -- store last condition check

// # write initial values to the registers
// movR1 0 // max value
// movR2 256 // min value
// movR3 7 # store array size in the register

// [1,2,3,4,5,6]
// R2 = 256

// loop_herere:
//   movMaxR1 @PTR
//   movMinR2 @PTR
//   incReg PTR # PTR = PTR + 1
//   isLessThanR3 PTR # if [R3] < PTR: FLAG = true
//   jumpIf loop_herere

//   movMaxR1 1 -> R1 := 1
//   movMinR2 1 -> R2 := 1
//   movMaxR1 2 -> R1 := 2
//   movMinR2 2 -> R2 := 1


// movMaxR1 @PTR

// if [R1] < @PTR:
//   [R1] = @PTR

#include <string>
#include <iostream>
#include <vector>
#include <fstream>
#include <charconv>
#include <sstream>
#include <iomanip>
#include <cstring>

enum CommandKind
{
    Nop,
    MovR1,
    MovR2,
    MovR3,
    MovPTR,
    MovMaxR1,
    MovMinR2,
    IncReg,
    JumpIf,
    JumpIfNot,
    IsLessThanR3
};

// mov R3, 3 == movR3 3

enum ArgumentKind
{
    ArgConstant,    // read constant like 0, 2, etc
    ArgFromPointer, // read
    ArgRegValue     // read register value
};

using Byte = unsigned char;
struct Argument
{
    ArgumentKind kind = ArgConstant;
    Byte         value;
};

struct Command
{
    CommandKind kind;
    Argument    arg;
};


static_assert(sizeof(Byte) == 1);

struct CPU
{
    Byte R1   = 0;
    Byte R2   = 0;
    Byte R3   = 0;
    bool FLAG = false;
    Byte PTR  = 0;
    int  pc   = 0;
    // Harward architecture, memory and commands are separate
    std::vector<Byte>    memory;
    std::vector<Command> commands;

    Byte getArgumentValue(Argument arg) {
        switch (arg.kind) {
            case ArgConstant: return arg.value;
            case ArgFromPointer: return memory.at(PTR);
            case ArgRegValue: return getRegRef(arg.value);
            default: std::cerr << "unknown command kind " << arg.kind << std::endl;
        }
    }

    Byte& getRegRef(int regIdx) {
        // PTR [0]
        // R1 [1]
        // R2 [2]
        // R3 [3]
        switch (regIdx) {
            case 0: return PTR;
            case 1: return R1;
            case 2: return R2;
            case 3: return R3;
        }
    }
};

std::ostream& operator<<(std::ostream& os, CPU const& cpu) {
    os << "CPU:[R1: " << (unsigned int)cpu.R1
       << ", R2: " << (unsigned int)cpu.R2
       << ", R3: " << (unsigned int)cpu.R3 << "]";
    return os;
}

std::ostream& operator<<(std::ostream& os, Argument arg) {
    switch (arg.kind) {
        case ArgConstant: os << (int)arg.value; break;
        case ArgFromPointer: os << "@PTR"; break;
        case ArgRegValue: {
            switch (arg.value) {
                case 0: os << "PTR"; break;
                case 1: os << "R1"; break;
                case 2: os << "R2"; break;
                case 3: os << "R3"; break;
            }
            break;
        }
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, Command cmd) {
    switch (cmd.kind) {
        case MovR1: os << "MovR1"; break;
        case MovR2: os << "MovR2"; break;
        case MovR3: os << "MovR3"; break;
        case MovPTR: os << "MovPTR"; break;
        case MovMaxR1: os << "MovMaxR1"; break;
        case MovMinR2: os << "MovMinR2"; break;
        case IncReg: os << "IncReg"; break;
        case JumpIf: os << "JumpIf"; break;
        case JumpIfNot: os << "JumpIfNot"; break;
        case IsLessThanR3: os << "IsLessThanR3"; break;
    }

    os << " [";
    os << cmd.arg;
    os << "]";
    

    return os;
}

Command parseCommand(std::vector<std::string> const& split) {
    Command result;

    if (split[0] == "movR1") {
        result.kind = MovR1;
    } else if (split[0] == "movR2") {
        result.kind = MovR2;
    } else if (split[0] == "movR3") {
        result.kind = MovR3;
    } else if (split[0] == "movPTR") {
        result.kind = MovPTR;
    } else if (split[0] == "nop") {
        result.kind = Nop;
    } else if (split[0] == "movMaxR1") {
        result.kind = MovMaxR1;
    } else if (split[0] == "movMinR2") {
        result.kind = MovMinR2;
    } else if (split[0] == "incReg") {
        result.kind = IncReg;
    } else if (split[0] == "jumpIf") {
        result.kind = JumpIf;
    } else if (split[0] == "jumpIfNot") {
        result.kind = JumpIfNot;
    } else if (split[0] == "isLessThanR3") {
        result.kind = IsLessThanR3;
    } else {
        std::cerr << split[0] << " unknown command\n";
        abort();
    }

    if (1 < split.size()) {
        std::string_view arg{split[1]};
        //  @PTR
        // [^^^^]
        if (arg[0] == '@') {
            result.arg.kind = ArgFromPointer;
            arg.remove_prefix(0);
        }
        // @PTR
        // [^^^]

        if (arg == "PTR") {
            result.arg.value = 0; 
        } else if (arg == "R1") {
            result.arg.value = 1;
        } else if (arg == "R2") {
            result.arg.value = 2;
        } else if (arg == "R3") {
            result.arg.value = 3;
        } else {
            std::from_chars(arg.data(), arg.data() + arg.size(), result.arg.value);
        }
    }

    return result;
}

void tokenize(
    std::string const &str, const char* delim,
    std::vector<std::string> &out) {
    char *token = strtok(const_cast<char*>(str.c_str()), delim);
    while (token != nullptr) {
        out.push_back(std::string(token));
        token = strtok(nullptr, delim);
    }
}


int main(int argc, char** argv) {
    std::ifstream file{"program.txt"};
    
    std::string temp;

    CPU cpu;
    cpu.memory = {
        1, 2, 3, 4, 5, 6, 7, 8,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
    };

    // Get the input from the input file until EOF
    while (std::getline(file, temp)) {
        // Add to the list of output strings
        //outputs.push_back(temp);
        std::vector<std::string> out;
        std::cout << "CMD: " << temp << "\n";
        tokenize(temp, " ", out);
        cpu.commands.push_back(parseCommand(out));
        std::cout << "test" << std::endl;
    }



    //  cpu.commands = {
    //     {MovR1, {ArgConstant, 0}},
    //     {MovR2, {ArgConstant, 255}},
    //     // loop_herere:
    //     {Nop},
    //     //   movMaxR1 @PTR
    //     {MovMaxR1, {ArgFromPointer, 0}},
    //     //   movMinR2 @PTR
    //     {MovMinR2, {ArgFromPointer, 0}},
    //     //   incReg PTR # PTR = PTR + 1 // incReg [0]
    //     {IncReg, {ArgRegValue, 0}},
    //     //   isLessThanR3 PTR # if [R3] < PTR: FLAG = true
    //     // isLessThanR3 [0]
    //     {IsLessThanR3, {ArgRegValue, 0}},
    //     //   jumpIf loop_herere
    //     {Nop, {ArgConstant, 2}},
    // };


    while (cpu.pc < cpu.commands.size()) {
        auto cmd = cpu.commands[cpu.pc];
        std::cout << cpu << "\t" << cmd;
        switch (cmd.kind) {
            case Nop: {
                break;
            }
            case MovR1: {
                // movR1 (command) 2 (argument)
                cpu.R1 = cpu.getArgumentValue(cmd.arg);
                break;
            }
            case MovR2: {
                cpu.R2 = cpu.getArgumentValue(cmd.arg);
                break;
            }
            case MovR3: {
                cpu.R3 = cpu.getArgumentValue(cmd.arg);
                break;
            }
            case MovMaxR1: {
                if (cpu.getArgumentValue(cmd.arg) > cpu.R1) {
                    cpu.R1 = cpu.getArgumentValue(cmd.arg);
                }
                break;
            }

            case MovMinR2: {
                if (cpu.getArgumentValue(cmd.arg) < cpu.R2) {
                    cpu.R2 = cpu.getArgumentValue(cmd.arg);
                }
                break;
            }

            case JumpIf: {
                if (cpu.FLAG) {
                    // -1 because of ++cpu.pc at the end of the 
                    // `switch` statement.
                    cpu.pc = cmd.arg.value;
                    continue;
                }
                break;
            }
            case JumpIfNot: {
                if (!cpu.FLAG) {
                    cpu.pc = cmd.arg.value;
                    continue;
                }
                break;
            }

            case IsLessThanR3: {
                cpu.FLAG = cpu.getRegRef(cmd.arg.value) < cpu.R3;
                break;
            }
            case IncReg: {
                // INC PTR
                // INC [0]
                ++cpu.getRegRef(cmd.arg.value);
                break;
            }
        }
        ++cpu.pc;
        std::cout << " --> " << cpu << std::endl;
    }

    std::cout << "FINAL: " << cpu << std::endl;   
    std::cout << "done\n";
}
