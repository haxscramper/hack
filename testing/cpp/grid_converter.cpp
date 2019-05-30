#include "../../common/cpp/macro.hpp"
#include "../../common/cpp/using.hpp"
#include "signals_slots.hpp"
#include <vector>

template <class T>
using Vec  = std::vector<T>;
using Str  = std::string;
using Grid = Vec<Str>;

Grid generate_grid(uS grid_cols, uS grid_rows = 0) {
    if (grid_rows == 0) {
        grid_rows = grid_cols;
    }

    Grid grid(grid_rows);

    for_i(i, grid_rows) {
        grid[i] = Str(grid_cols, '0');
    }


    struct Pos {
        size_t row;
        size_t col;
    };

    Pos pos = {rand() % grid_rows, 0};


    size_t dir = 1;
    // down: 0, forward: 1,3, up: 2

    LOG << "Grid:" << grid.size() << grid[0].size();

    size_t ctx = 0;
    while (pos.col < grid_cols) {
        // INFO << "----";
        u32 step = 2 + (rand() % std::min(grid_cols, grid_rows)) / 2;

        // LOG << "Step: " << step;
        for_i(i, step) {
            // LOG << "Pos:" << pos.row << pos.col << "Dir:" << dir;
            grid.at(pos.row).at(pos.col) = '1';
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

        if (ctx > 40) {
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
                    case '1': printf("\033[32m%c\033[0m", c); break;
                    case 'F': printf("%c", c); break;
                    default: printf("%c", c); break;
                }
            }
            printf("\n");
        }
    }
};

class Stepper : public signal_base
{
  public:
    void slot_accept_grid(signal_msg data) {
        grid = scastp<Grid>(data);

        // Some algorihm goes here

        signal_done_search(&grid);
    }

    void signal_done_search(signal_msg data) {
        emit_signal(signal_cast(&Stepper::signal_done_search), data);
    }

  private:
    Grid grid;
};

class Checker : public signal_base
{
};


int main() {
    srand(time(nullptr));

    Reader reader;
    Writer writer;

    Checker upChecker;
    Checker downChecker;
    Checker forwardChecker;

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


    Grid grid = generate_grid(50, 20);

    reader.set_grid(grid);


    LOG << "Done main";
}
