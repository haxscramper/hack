#include "../../common/cpp/macro.hpp"

// Include and declarations were made in the middle of include to
// provide operator overloading for logging macro

// TODO Add numeration for grid rows and columns (is it possible to
// display rotated numbers in ternminal)
#include <iostream>
#include <string>
#include <vector>

#include "../../common/cpp/using.hpp"

using Grid = Vec<Str>;

struct Pos {
    size_t row = 0;
    size_t col = 0;
};

std::ostream& operator<<(std::ostream& os, const Grid& grid) {
    os << "[ " << grid.size() << ", " << grid.at(0).size() << " ]";
    return os;
}

std::ostream& operator<<(std::ostream& os, const Pos& pos) {
    os << "( " << pos.row << ", " << pos.col << " )";
    return os;
}

#include "signals_slots.hpp"

const char fill_char   = '0';
const char path_char   = '1';
const char change_char = 'F';


int min(int a, int b) {
    return (a < b ? a : b);
}

size_t row_count(const Grid& grid) {
    return grid.size();
}

size_t col_count(const Grid& grid) {
    return grid.at(0).size();
}

void bound_decrement(size_t& num) {
    if (num > 0) {
        --num;
    }
}

void bound_increment(size_t& num, size_t max) {
    if (num < max) {
        ++num;
    }
}


Grid generate_grid(uS grid_cols, uS grid_rows = 0) {
    if (grid_rows == 0) {
        grid_rows = grid_cols;
    }

    Grid grid(grid_rows);

    for_i(i, grid_rows) {
        grid[i] = Str(grid_cols, fill_char);
    }


    Pos pos = {rand() % grid_rows, rand() % grid_cols / 5};

    size_t dir = 1;
    // down: 0, forward: 1,3, up: 2

    LOG << "Grid:" << grid;

    size_t ctx = 0;
    while (pos.col < grid_cols) {
        // INFO << "----";
        u32 step = 2 + (rand() % min(20, min(grid_rows, grid_cols))) / 2;

        // LOG << "Step: " << step;
        for_i(i, step) {
            // LOG << "Pos:" << pos.row << pos.col << "Dir:" << dir;
            grid.at(pos.row).at(pos.col) = path_char;
            switch (dir) {
                case 0: bound_decrement(pos.row); break;
                case 1: bound_increment(pos.col, grid_cols - 1); break;
                case 2: bound_increment(pos.row, grid_rows - 1); break;
                case 3: bound_increment(pos.col, grid_cols - 1); break;
                default: INFO << "Wrong dir " << dir; break;
            }
        }


        dir = (dir + 1) % 4;

        ctx++;
        if (ctx > grid_cols) {
            break;
        }

        // INFO << "++++";
    }

    return grid;
}

class Reader : public signal_base
{
  public:
    void signal_emit_grid(Grid* grid) {
        METHOD_DBG
        emit_signal(&Reader::signal_emit_grid, grid);
    }

    void set_grid(Grid* grid) {
        METHOD_DBG
        this->signal_emit_grid(grid);
    }
};

class Writer : public signal_base
{

  public:
    void slot_write_grid(Grid& grid) {
        Vec<Str> top;

        // TODO print index in header row
        for_i(i, grid.size()) {
        }

        for_i(i, grid.size()) {
            Str& row = grid[i];
            printf(" %2d ", i);
            for (char c : row) {
                switch (c) {
                    case path_char: printf("\033[32m%c\033[0m", c); break;
                    case change_char:
                        printf("\033[31m%c\033[0m", c);
                        break;
                    default: printf("%c", c); break;
                }
            }
            printf("\n");
        }
    }
};

struct CheckRequest {
    Pos   pos;
    Grid& grid;
};


class Stepper : public signal_base
{
  public:
    void slot_accept_grid(Grid* _grid) {
        METHOD_DBG
        Grid& grid = *_grid;

        for_i(col, col_count(grid)) {
            for_i(row, row_count(grid)) {
                if (grid.at(row).at(col) == path_char) {
                    currentPos.row = row;
                    currentPos.col = col;
                    goto found;
                }
            }
        }

    found:
        for (size_t ctx = 0;
             ctx < col_count(grid) * 5 && currentPos.col < col_count(grid);
             ++ctx) {

            CheckRequest request = {currentPos, grid};
            signal_check_cell(request);
        }


        signal_request_grid_print(&grid);
    }

    void signal_request_grid_print(Grid* grid) {
        emit_signal(&Stepper::signal_request_grid_print, grid);
    }

    void signal_check_cell(CheckRequest request) {
        // Pos   pos  = scastp<CheckRequest>(request).pos;
        // Grid& grid = scastp<CheckRequest>(request).grid;

        // LOG << "signal_check_cell"
        //    << "Pos: " << pos << "Grid:" << grid << grid.at(0).size();

        emit_signal(&Stepper::signal_check_cell, request);
    }

    void slot_found_match(CheckRequest request) {
        //        LOG << "slot_found_match";
        // CheckRequest request = scastp<CheckRequest>(msg);
        Pos& pos = request.pos;

        grid.at(pos.row).at(pos.col) = change_char;

        LOG << pos;

        currentPos = pos;
    }

    void slot_missed_match(CheckRequest msg) {
    }

  private:
    Grid grid;
    Pos  currentPos;
};

class Checker : public signal_base
{
  public:
    enum Direction
    {
        Forward,
        Down,
        Up
    };


    Checker(Direction _direction) : direction(_direction) {
    }


    void slot_check_cell(CheckRequest request) {
        Grid& grid = request.grid;
        Pos   pos  = request.pos;

        switch (direction) {
            case Forward:
                bound_increment(pos.col, col_count(grid) - 1);
                break;
            case Down: bound_decrement(pos.row); break;
            case Up: bound_increment(pos.row, row_count(grid) - 1); break;
        }


        request.pos = pos;

        if (grid.at(pos.row).at(pos.col) == path_char) {
            signal_found_match(request);
        } else {
            signal_missed_match(request);
        }
    }

    void signal_found_match(CheckRequest& request) {
        emit_signal(&Checker::signal_found_match, request);
    }

    void signal_missed_match(CheckRequest& request) {
        emit_signal(&Checker::signal_missed_match, request);
    }

  private:
    Direction direction;
};


int main() {
    srand(time(nullptr));

    Reader reader;
    Writer writer;

    Checker upChecker(Checker::Up);
    Checker downChecker(Checker::Down);
    Checker forwardChecker(Checker::Forward);

    Stepper stepper;

    connect(
        &reader,
        &Reader::signal_emit_grid,
        &stepper,
        &Stepper::slot_accept_grid);

    connect(
        &stepper,
        &Stepper::signal_request_grid_print,
        &writer,
        &Writer::slot_write_grid);


#define CONNECT_CHECKER_TO_STEPPER(checkerDirection)                      \
    connect(                                                              \
        &stepper,                                                         \
        &Stepper::signal_check_cell,                                      \
        &checkerDirection##Checker,                                       \
        &Checker::slot_check_cell);

    CONNECT_CHECKER_TO_STEPPER(down);
    CONNECT_CHECKER_TO_STEPPER(up);
    CONNECT_CHECKER_TO_STEPPER(forward);


#define CONNECT_STEPPER_TO_CHECKER(checkerDirection, connectionName)      \
    connect(                                                              \
        &checkerDirection##Checker,                                       \
        &Checker::signal_##connectionName,                                \
        &stepper,                                                         \
        &Stepper::slot_##connectionName);

    CONNECT_STEPPER_TO_CHECKER(up, found_match)
    CONNECT_STEPPER_TO_CHECKER(down, found_match)
    CONNECT_STEPPER_TO_CHECKER(forward, found_match)

    CONNECT_STEPPER_TO_CHECKER(up, missed_match)
    CONNECT_STEPPER_TO_CHECKER(down, missed_match)
    CONNECT_STEPPER_TO_CHECKER(forward, missed_match)

#ifdef DEBUG

    reader.print_connections();
    stepper.print_connections();
    writer.print_connections();
    upChecker.print_connections();
    downChecker.print_connections();
    forwardChecker.print_connections();

#endif


    Grid grid = generate_grid(60, 20);

    reader.set_grid(&grid);

    LOG << "Done main";
}
