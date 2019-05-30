#include "../../common/cpp/macro.hpp"
#include "../../common/cpp/using.hpp"
#include "signals_slots.hpp"
#include <vector>

const char fill_char   = '0';
const char path_char   = '1';
const char change_char = 'F';

template <class T>
using Vec  = std::vector<T>;
using Str  = std::string;
using Grid = Vec<Str>;

struct Pos {
    size_t row;
    size_t col;
};

int min(int a, int b) {
    return (a < b ? a : b);
}


Grid generate_grid(uS grid_cols, uS grid_rows = 0) {
    if (grid_rows == 0) {
        grid_rows = grid_cols;
    }

    Grid grid(grid_rows);

    for_i(i, grid_rows) {
        grid[i] = Str(grid_cols, fill_char);
    }


    Pos pos = {rand() % grid_rows, 0};


    size_t dir = 1;
    // down: 0, forward: 1,3, up: 2

    LOG << "Grid:" << grid.size() << grid[0].size();

    size_t ctx = 0;
    while (pos.col < grid_cols) {
        // INFO << "----";
        u32 step = 2 + (rand() % min(20, min(grid_rows, grid_cols))) / 2;

        // LOG << "Step: " << step;
        for_i(i, step) {
            // LOG << "Pos:" << pos.row << pos.col << "Dir:" << dir;
            grid.at(pos.row).at(pos.col) = path_char;
            switch (dir) {
                case 0: {
                    if (pos.row > 0) {
                        pos.row -= 1;
                    }
                    break;
                }

                case 1: { // forward
                    if (pos.col < grid_cols - 1) {
                        pos.col += 1;
                    }

                    break;
                }

                case 2: {
                    if (pos.row < grid_rows - 1) {
                        pos.row += 1;
                    }
                    break;
                }

                case 3: { // forward
                    if (pos.col < grid_cols - 1) {
                        pos.col += 1;
                    }

                    break;
                }

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
    void signal_emit_grid(signal_msg grid) {
        emit_signal(signal_cast(&Reader::signal_emit_grid), grid);
    }

    void set_grid(Grid& grid) {
        this->signal_emit_grid(&grid);
    }
};

class Writer : public signal_base
{

  public:
    void slot_write_grid(signal_msg data) {
        Grid& grid = scastp<Grid>(data);

        for (Str& row : grid) {
            printf("    ");
            for (char c : row) {
                switch (c) {
                    case path_char: printf("\033[32m%c\033[0m", c); break;
                    case change_char: printf("%c", c); break;
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
    void slot_accept_grid(signal_msg data) {
        grid = scastp<Grid>(data);

        // Some algorihm goes here

        Pos          pos     = {0, 0};
        CheckRequest request = {pos, grid};

        signal_check_cell(&request);

        signal_done_search(&grid);
    }

    void signal_done_search(signal_msg data) {
        emit_signal(signal_cast(&Stepper::signal_done_search), data);
    }

    void signal_check_cell(signal_msg request) {
        Pos pos = scastp<CheckRequest>(request).pos;

        LOG << "signal_check_cell"
            << "Pos: " << pos.row << pos.col;
        emit_signal(signal_cast(&Stepper::signal_check_cell), &request);
    }

    void slot_found_match(signal_msg msg) {
        LOG << "slot_found_match";
        CheckRequest request = scastp<CheckRequest>(msg);
    }

    void slot_missed_match(signal_msg msg) {
    }

  private:
    Grid grid;
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


    void slot_check_cell(signal_msg msg) {
        LOG << "slot_check_cell";

        CheckRequest request = scastp<CheckRequest>(msg);


        Grid& grid = request.grid;
        Pos   pos  = request.pos;


        switch (direction) {
            case Forward: {
                pos.col += 1;
                break;
            }

            case Down: {
                pos.row -= 1;
                break;
            }

            case Up: {
                pos.row += 1;
                break;
            }
        }


        request.pos = pos;

        LOG << grid.size();


        if (grid.at(pos.row).at(pos.col) == path_char) {
            LOG << "Found match";
            signal_found_match(&request);
        } else {
            LOG << "Missed match";
            signal_missed_match(&request);
        }

        LOG << "slot done";
    }

    void signal_found_match(signal_msg request) {
        LOG << "Signal found match";
        emit_signal(signal_cast(&Checker::signal_found_match), request);
    }

    void signal_missed_match(signal_msg request) {
        LOG << "Signal missed match";
        emit_signal(signal_cast(&Checker::signal_missed_match), request);
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
        &Stepper::signal_done_search,
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

    Grid grid = generate_grid(100, 30);

    reader.set_grid(grid);

    LOG << "Done main";
}
