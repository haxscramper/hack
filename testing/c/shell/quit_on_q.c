#include <stdio.h>
// Library for reading/writin terminal settings.
#include <termios.h>
#include <unistd.h>

int main() {
    // Terminal attributes data structure
    struct termios raw;

    // Get current terminal attributes
    tcgetattr(STDIN_FILENO, &raw);

    // Disable echo and canonical mode
    raw.c_lflag &= ~(ECHO | ICANON);

    // Set updater terminal attributes
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);

    char c;

    // Wait for `q` to be entered
    while (read(STDIN_FILENO, &c, 1) == 1 && c != 'q') {
        printf("Entered character %c (ascii code is %d)\n", c, c);
    }

    return 0;
}
