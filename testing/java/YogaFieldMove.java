class YogaFieldMove {
  private static char[][] field;

  public static void setFromStringlist(String[] fieldInit) {

    for (int row = 0; row < 7; ++row) {
      field[row] = fieldInit[row].toCharArray();
    }
  }

  public static void printField() {
    System.out.printf("    1 2 3 4 5 6 7\n");

    for (int row = 0; row < 7; ++row) {
      System.out.print((row + 1) + " [ ");
      for (int col = 0; col < 7; ++col) {
        char c = field[row][col];
        String out = "";
        switch (c) {
        case ' ':
          out = " ";
          break;
        case '#':
          out = "\033[0;31m#\033[0m";
          break;
        case 'x':
          out = "\033[0;33mx\033[0m";
          break;
        }
        System.out.print(out + " ");
      }

      System.out.println("]");
    }
  }

  public static void main(String[] args) {
    field = new char[7][7];
    setFromStringlist(new String[] {

        "xx   xx", //
        "xx   xx", //
        "       ", //
        "   #   ", //
        "       ", //
        "xx   xx", //
        "xx   xx"  //

    });

    if (canMakeMoves()) {
      System.out.printf("Can make move\n");
    } else {
      System.out.printf("No moves possible\n");
    }
    printField();
  }

  static Boolean canMakeMoves() {
    final int fieldSize = 7;
    final char emptyChar = ' ';
    final char noFieldChar = 'x';
    final char occupiedChar = '#';
    final int[][] directions = new int[][] {

        // row-col pairs
        {1, 0},
        {-1, 0},
        {0, 1},
        {0, -1}
        // --
    };

    for (int row = 0; row < fieldSize; ++row) {
      for (int col = 0; col < fieldSize; ++col) {
        for (int dir = 0; dir < directions.length; ++dir) {
          char onCell = field[row][col];
          if (onCell == occupiedChar) {
            int targetRow = 2 * directions[dir][0] + row;
            int targetCol = 2 * directions[dir][1] + col;

            if ((targetCol < 0 || targetCol >= fieldSize) ||
                (targetRow < 0 || targetRow >= fieldSize)) {
              continue;
            } else {
              char targetChar = field[row][col];
              if (targetChar == emptyChar || targetChar == noFieldChar) {
                continue;
              } else {
                int middleRow = directions[dir][0] + row;
                int middleCol = directions[dir][1] + col;
                char middleChar = field[middleRow][middleCol];
                if (middleChar == occupiedChar) {
                  System.out.printf(
                      "Can jump from cell {%d, %d} to {%d, %d}\n", row,
                      col, targetRow, targetCol);
                  System.out.printf(
                      "target char: {%c}\nmiddle char: {%c}\n", targetChar,
                      middleChar);

                  return true;
                } else {
                  continue;
                }
              }
            }
          }
        }
      }
    }

    return false;
  }
}
