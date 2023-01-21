import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import java.util.Random;
import java.util.regex.*;
import javax.imageio.*;
import javax.swing.*;
import javax.swing.table.*;

// clang-format off
include(utils.m4);
include(misc.m4.java);
include(ui-misc.m4.java);
// clang-format on

class Pos {
  public int r;
  public int c;

  static Pos dist(Pos a, Pos b) {
    var res = new Pos();
    res.r = Math.abs(a.r - b.r);
    res.c = Math.abs(a.c - b.c);
    return res;
  }

  public String toChessPos() { return r + "-" + (char)('a' + c); }
}

class ChessField {
  ImageIcon loadImage(String relativePath) throws java.io.IOException {
    BufferedImage picture = ImageIO.read(new File(relativePath));
    Image scaled = picture.getScaledInstance(60, 60, Image.SCALE_SMOOTH);
    return new ImageIcon(scaled);
  }

  String[][] field;
  HashMap<String, ImageIcon> chessImages =
      new HashMap<String, ImageIcon>();
  HashMap<String, String> chessLetters = new HashMap<>() {
    {
      put("b-k", "♚"); // black-king
      put("b-q", "♛"); // black-queen
      put("b-t", "♜"); // black-tower
      put("b-b", "♝"); // black-bishop
      put("b-h",
          "♞"); // black-knight (aka 'horse' (for key // consistency))
      put("b-p", "♟"); // black-pawn
      put("w-k", "♔"); // white-king
      put("w-q", "♕"); // white-queen
      put("w-t", "♖"); // white-tower
      put("w-b", "♗"); // white-bishop
      put("w-h",
          "♘"); // white-knight (aka 'horse' (for key // consistency))
      put("w-p", "♙"); // white-pawn
    }
  };

  ChessField(String[][] defaultField) {
    var figureNames = new String[] {
        "b-k", // black-king
        "b-q", // black-queen
        "b-t", // black-tower
        "b-b", // black-bishop
        "b-h", // black-knight (aka 'horse' (for key // consistency))
        "b-p", // black-pawn
        "w-k", // white-king
        "w-q", // white-queen
        "w-t", // white-tower
        "w-b", // white-bishop
        "w-h", // white-knight (aka 'horse' (for key // consistency))
        "w-p"  // white-pawn
    };

    for (String figure : figureNames) {
      String imageFile = "final_1_" + figure + ".png";
      try {
        chessImages.put(figure, loadImage(imageFile));
      } catch (java.io.IOException ioException) {
        System.out.println("Failed to read image " + imageFile);
      }
    }

    field = defaultField;
  }

  /** Draw current state of the board on table */
  void drawOnTable(JTable table) {
    for (int row = 0; row < field.length; ++row) {
      for (int col = 0; col < field[row].length; ++col) {
        table.setValueAt(chessImages.get(field[row][col]), row, col + 1);
      }
    }
  }

  Boolean canGoThere(String pieceCode, Pos target, Pos prev) {
    Pos d = Pos.dist(prev, target);
    if (pieceCode.length() != 3) {
      return false;
    } else {
      var last = pieceCode.charAt(2);
      switch (last) {
      case 'h':
        return (d.r == 2 && d.c == 1) || (d.r == 1 && d.c == 2);
      case 'b':
        return (d.r == d.c) && d.r > 0;
      case 'q':
        return ((d.r == d.c && d.r > 0) || //
                (d.r == 0 && d.c != 0) ||  //
                (d.c == 0 && d.c != 0));
      case 'k':
        return d.r <= 1 && d.c <= 1;
      case 't':
        return (d.r == 0 && d.c != 0) || (d.c == 0 && d.c != 0);
      case 'p':
        if (d.r == 1) {
          char color = pieceCode.charAt(0);
          Boolean moveDirectionAllowed =
              // white pieces start at the 0 row and move downwards
              (color == 'w' && target.r > prev.r) ||
              // black pieces start at the 2 row and move upwards
              (color == 'b' && target.r < prev.r);

          if (!moveDirectionAllowed) {
            Pprint("direction is not allowed");
            return false;
          }

          if (d.c == 0) {
            // If we are on the same column, pawn can go
            // 'forward' only if there are no pieces
            return field[target.r][target.c].equals("");
          } else if (d.c == 1) {
            // If we try to move to side column we need to check
            // whether or not there is something we can attack.
            // (Cell is not empty) and color of the chess piece is opposite
            // of ours
            var adjCode = field[target.r][target.c];
            return (!adjCode.equals("") && adjCode.charAt(0) != color);
          } else {
            Pprint("Cannot move more than one column");
            return false;
          }
        } else {
          Pprint("Have to move exactly one row");
          return false;
        }
      default:
        System.out.println("Unknown piece type:" + last);
        return false;
      }
    }
  }

  void setVal(String value, int row, int col) { field[row][col] = value; }
  String getVal(int row, int col) { return field[row][col]; }
  String getChessletter(String chessCode) {
    String result = "";
    try {
      result = chessLetters.get(chessCode);
    } catch (Exception e) {
    }
    return result;
  }

  Boolean isFinished() {
    Boolean result = true;
    for (int col = 0; col < field[0].length; ++col) {
      result = result &&                           //
               (field[0][col].length() > 0 &&      //
                field[0][col].charAt(0) == 'b') && //
               (field[2][col].length() > 0 &&      //
                field[2][col].charAt(0) == 'w');
    }
    return result;
  }
}

class MicroChess extends JFrame {
  JTable gameField;
  JTable moveTable;
  Boolean gameOver = false;

  DefaultTableModel fieldModel;

  DefaultTableModel movesModel = new DefaultTableModel(
      new Object[] {"Фигура", "Клетка n-1", "Клетка n"}, 0) {
    @Override
    public boolean isCellEditable(int row, int column) {
      return false;
    }
  };

  JButton gameInit = new JButton("Начать игру");

  ChessField field;
  Pos prevPos;

  void resetField() {
    gameOver = false;
    field = new ChessField(
        // --
        new String[][] {
            {"w-p", "w-p", "w-p"}, //
            {"", "", ""},          //
            {"b-h", "b-h", "b-h"}  //
        });

    var dm = (DefaultTableModel)moveTable.getModel();
    while (dm.getRowCount() > 0) {
      dm.removeRow(0);
    }

    field.drawOnTable(gameField);
  }

  void cellPressed(int rowIdx, int colIdx) {
    var nowPos = new Pos();
    colIdx = colIdx - 1; // Account for first column with row names
    nowPos.r = rowIdx;
    nowPos.c = colIdx;

    if (prevPos != null) {
      // Previous button has already been pressed
      var code = field.getVal(prevPos.r, prevPos.c);
      if (field.canGoThere(code, nowPos, prevPos)) {
        field.setVal(                         //
            field.getVal(nowPos.r, nowPos.c), //
            prevPos.r,                        //
            prevPos.c);

        field.setVal(code, rowIdx, colIdx);

        field.drawOnTable(gameField);
        System.out.println("Updated table");

        movesModel.addRow(new Object[] {
            field.getChessletter(code), //
            prevPos.toChessPos(),       //
            nowPos.toChessPos()         //
        });

        gameOver = field.isFinished();

      } else {
        System.out.println("Failed to move cehss piece");
      }
      prevPos = null; // Series of press has ended
    } else {
      // This is first button press in series;
      System.out.println("Started movement");
      prevPos = nowPos;
    }
  }

  void initUI() {
    fieldModel =
        new DefaultTableModel(new Object[] {"", "A", "B", "C"}, 0) {
          @Override
          public Class<?> getColumnClass(int column) {
            if (column > 0) {
              return ImageIcon.class;
            }

            return Object.class;
          }

          @Override
          public boolean isCellEditable(int row, int column) {
            return false;
          }
        };

    gameInit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { resetField(); }
    });

    field = new ChessField(
        // --
        new String[][] {
            {"w-p", "w-p", "w-p"}, //
            {"", "", ""},          //
            {"b-h", "b-h", "b-h"}  //
        });

    fieldModel.setRowCount(3);
    fieldModel.setColumnCount(4);
    fieldModel.setValueAt("1", 0, 0);
    fieldModel.setValueAt("2", 1, 0);
    fieldModel.setValueAt("3", 2, 0);

    gameField = new JTable(fieldModel);
    moveTable = new JTable(movesModel);

    gameField.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(LYTBuilder.makeAnnotatedInput(
            "Игровое поле", LYTBuilder.makeScrollable(gameField),
            BoxLayout.Y_AXIS));

        add(LYTBuilder.makeAnnotatedInput(
            "Таблица ходов", LYTBuilder.makeScrollable(moveTable),
            BoxLayout.Y_AXIS));
      }
    }));

    add(Misc.setMax_WH(gameInit, 96, 24));

    gameField.getColumnModel().getColumn(0).setMaxWidth(8);

    for (int i = 1; i < 4; ++i) {
      gameField.getColumnModel().getColumn(i).setMaxWidth(100);
    }

    gameField.setRowHeight(100);

    gameField.addMouseListener(new java.awt.event.MouseAdapter() {
      @Override
      public void mouseClicked(java.awt.event.MouseEvent evt) {
        int row = gameField.rowAtPoint(evt.getPoint());
        int col = gameField.columnAtPoint(evt.getPoint());
        if (row >= 0 && col >= 0 && !gameOver) {
          System.out.println("Pressed at: " + row + " " + col);
          cellPressed(row, col);
          if (gameOver) {

            JOptionPane.showMessageDialog(
                null, "Игра окончена !", "",
                JOptionPane.INFORMATION_MESSAGE |
                    JOptionPane.OK_CANCEL_OPTION);
          }
        }
      }
    });

    field.drawOnTable(gameField);
  }

  MicroChess() {
    initUI();
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class TicTacField {
  String[][] field = {{"", "", ""}, {"", "", ""}, {"", "", ""}};

  final int noValue = -1;
  final char empty = ' ';
  final char computer = 'o';
  final char human = 'x';

  public int[] calculateMove(char[][] fieldIn) {
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
      System.out.printf("Finishing move to [%d %d]\n", result[0],
                        result[1]);

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

      System.out.printf("Random move to[%d %d]\n", result[0], result[1]);

      return result;
    }
  }

  void doMove() {
    char[][] fieldIn = new char[3][3];
    for (int row = 0; row < 3; ++row) {
      for (int col = 0; col < 3; ++col) {
        var inCell = field[row][col];
        if (inCell.length() > 0) {
          fieldIn[row][col] = inCell.charAt(0);
        } else {
          fieldIn[row][col] = empty;
        }
      }
    }

    var result = calculateMove(fieldIn);

    int row = result[0];
    int col = result[1];
    field[row][col] = "" + computer;
  }

  void setCell(int row, int col) {
    Pprint("Setting cell "
           + " " + row + " " + col);
    field[row][col] = "x";
  }

  Boolean isEmpty(int row, int col) { return field[row][col].equals(""); }

  /** Draw current state of the board on table */
  void drawOnTable(JTable table) {
    for (int row = 0; row < field.length; ++row) {
      for (int col = 0; col < field[row].length; ++col) {
        table.setValueAt(field[row][col], row, col);
      }
    }
  }

  String isFinished() {
    int dim = 3;
    // check horizontal
    System.out.println("Checking horizontal");
    for (int row = 0; row < dim; ++row) {
      String rstring = String.join("", field[row]);
      if (rstring.equals("xxx")) {
        Pprint("ok: " + rstring);
        return "human";
      } else if (rstring.equals("ooo")) {
        Pprint("ok: " + rstring);
        return "computer";
      } else {
        Pprint("Does not match:" + rstring);
      }
    }

    // check vertical
    System.out.println("Checking vertical");
    for (int col = 0; col < dim; ++col) {
      String cstring = field[0][col] + field[1][col] + field[2][col];
      if (cstring.equals("xxx")) {
        Pprint("ok: " + cstring);
        return "human";
      } else if (cstring.equals("ooo")) {
        Pprint("ok: " + cstring);
        return "computer";
      } else {
        Pprint("Does not match:" + cstring);
      }
    }

    String ldiag = field[0][0] + field[1][1] + field[2][2];
    String rdiag = field[0][2] + field[1][1] + field[2][0];

    if (ldiag.equals("ooo") || rdiag.equals("ooo")) {
      return "computer";
    } else if (ldiag.equals("xxx") || rdiag.equals("xxx")) {
      return "human";
    } else {
      Pprint("Does not match:" + ldiag);
      Pprint("Does not match:" + rdiag);
    }

    return "none";
  }
}

class TicTacToe extends JFrame {
  JTable fieldTable = new JTable();
  JButton computerMove = new JButton("Начинает компьютер");
  Boolean gameOver = false;
  String gameResult = "none";
  TicTacField field = new TicTacField();

  void checkIsFinished() {
    gameResult = field.isFinished();
    if (gameResult.equals("none")) {
      gameOver = false;
      System.out.println("No winner");
    } else if (gameResult.equals("human") ||
               gameResult.equals("computer")) {
      gameOver = true;
    } else {
      System.out.println("Unknow game result " + gameResult);
    }
  }

  void cellPressed(int rowIdx, int colIdx) {
    if (field.isEmpty(rowIdx, colIdx)) {
      field.setCell(rowIdx, colIdx);

      checkIsFinished();

      if (!gameOver) {
        field.doMove();
      } else {
        Pprint("Game over");
      }

      checkIsFinished();

      field.drawOnTable(fieldTable);
    } else {
      System.out.printf("Cell %d %d is occupied\n", rowIdx, colIdx);
    }
  }

  void resetField() {
    field = new TicTacField();
    gameOver = false;
    field.doMove();
    field.drawOnTable(fieldTable);
  }

  void initUI() {
    add(fieldTable);
    var model = (DefaultTableModel)fieldTable.getModel();

    model.setRowCount(3);
    model.setColumnCount(3);

    add(computerMove);

    field.doMove();
    field.drawOnTable(fieldTable);

    for (int i = 0; i < 3; ++i) {
      fieldTable.getColumnModel().getColumn(i).setMaxWidth(100);
    }

    fieldTable.setRowHeight(100);

    fieldTable.setFont(new Font("Serif", Font.BOLD, 72));
    computerMove.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { resetField(); }
    });

    LYTBuilder.setCellsAlignment(fieldTable, SwingConstants.CENTER);

    fieldTable.addMouseListener(new java.awt.event.MouseAdapter() {
      @Override
      public void mouseClicked(java.awt.event.MouseEvent evt) {
        int row = fieldTable.rowAtPoint(evt.getPoint());
        int col = fieldTable.columnAtPoint(evt.getPoint());
        if (row >= 0 && col >= 0 && !gameOver) {
          System.out.println("Pressed at: " + row + " " + col);
          cellPressed(row, col);
          if (gameOver) {
            String winnerMessage = "";
            if (gameResult.equals("human")) {
              winnerMessage = "Игра окончена выиграл человек";
            } else if (gameResult.equals("computer")) {
              winnerMessage = "Игра окончена выиграл компьютер";
            }
            JOptionPane.showMessageDialog(
                null, winnerMessage, "",
                JOptionPane.INFORMATION_MESSAGE |
                    JOptionPane.OK_CANCEL_OPTION);
          }
        }
      }
    });
  }

  TicTacToe() {
    initUI();
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class ChessFrame extends JFrame {
  JTabbedPane tabs = new JTabbedPane();
  JTextArea help = new JTextArea();

  void initUI() {
    var chess = new MicroChess();
    help.setText(
        "Для хода нажмите на исходную ячейку и затем на целевую. Для сброса ходов нажмите на кнопку");
    tabs.addTab("Переставь фигуры", chess.getContentPane());
    tabs.addTab("Помощь", help);
    add(tabs);
  }

  ChessFrame() {
    initUI();
    this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class TicTacFrame extends JFrame {
  JTabbedPane tabs = new JTabbedPane();
  JTextArea help = new JTextArea();

  void initUI() {
    var ticTacToe = new TicTacToe();
    help.setText(
        "Для хода нажмите на исходную ячейку. Если игра не законцена то компьютер автоматически сделает ход. Для сброса ходов нажмите на кнопку");
    tabs.addTab("Крестики-нолики", ticTacToe.getContentPane());
    tabs.addTab("Помощь", help);
    add(tabs);
  }

  TicTacFrame() {
    initUI();
    this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class MainFrame extends JFrame {
  JMenuBar menuBar = new JMenuBar();
  JMenu menu = new JMenu("...");
  TicTacFrame tictacFrame = new TicTacFrame();
  ChessFrame chessFrame = new ChessFrame();

  void initUI() {
    var chess = new JMenuItem("Переставь фигуры");
    var tictac = new JMenuItem("Крестики-нолики");
    var quit = new JMenuItem("Выход");

    menu.add(chess);
    menu.add(tictac);
    menu.add(quit);

    chess.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { chessFrame.show(); }
    });

    tictac.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { tictacFrame.show(); }
    });

    quit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { System.exit(0); }
    });

    menuBar.add(menu);
    this.setJMenuBar(menuBar);
  }

  MainFrame() {
    initUI();
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class Main {
  public static void main(String args[]) {
    var frame = new MainFrame();
    frame.show();
  }
}
