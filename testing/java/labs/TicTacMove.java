import java.util.Random;

public class TicTacMove {

  static final int noValue = -1;
  static final char empty = ' ';
  static final char computer = 'o';
  static final char human = 'x';

  // return arrays [row, col, who won]

  public static int[] calculateMove(String[] fieldIn) {
    char[][] field = new char[3][3];
    for (int row = 0; row < 3; ++row) {
      field[row] = fieldIn[row].toCharArray();
    }

    return calculateMove(field);
  }

  public static int[] calculateMove(char[][] fieldIn) {
    int[] result = new int[] {noValue, noValue, noValue};

    int[][][] diagonals = new int[][][] {

        // {{row1, col1}, {row2, col2}, {row3, col3}}
        {{0, 0}, {0, 1}, {0, 2}},
        {{1, 0}, {1, 1}, {1, 2}},
        {{2, 0}, {2, 1}, {2, 2}}
        // ---

    };

    Boolean doneMove = false;

    for (int d = 0; d < diagonals.length && !doneMove; ++d) {
      int missingRow = -1;
      int missingCol = -1;
      int missingCount = 0;

      for (int cell = 0; cell < 3; ++cell) {
        int row = diagonals[d][cell][0];
        int col = diagonals[d][cell][1];

        char inCell = fieldIn[row][col];
        if (inCell == empty) {
          ++missingCount;
          missingRow = row;
          missingCol = col;
        } else if (inCell == human) {
          ++missingRow;
        } else if (inCell == computer) {
        }
      }

      if (
          // only one cell is missing
          missingCount == 1 &&
          // and it was empty
          missingRow != -1 && missingCol != -1) {
        result[0] = missingRow;
        result[1] = missingCol;
        doneMove = true;
      }
    }

    if (doneMove) {
      return result;
    } else {
      int attempts = 1;
      Random rand = new Random();
      int row = rand.nextInt(3);
      int col = rand.nextInt(3);
      while ((fieldIn[row][col] != empty) && attempts < 9) {
        ++attempts;
        row = rand.nextInt(3);
        col = rand.nextInt(3);
      }

      if (attempts > 9) {
        System.out.println("Made more than 9 attempts to find empty cell");
      }

      result[0] = row;
      result[1] = col;

      return result;
    }
  }

  public static void main(String[] args) {
    System.out.printf("Hello world\n");

    var result = calculateMove(new String[] {"o o", "xxx", "xxx"});

    System.out.printf("%d %d\n", result[0], result[1]);
  }
}
