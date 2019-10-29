import java.awt.event.*;
import java.awt.image.*;
import java.io.*;
import java.util.*;
import java.util.Random;
import java.util.regex.*;
import javax.imageio.*;
import javax.swing.*;
import javax.swing.table.*;

include(utils.m4) include(misc.m4.java);
include(`ui-misc.m4.java');

class Pos {
  public int r;
  public int c;

  static Pos dist(Pos a, Pos b) {
    var res = new Pos();
    res.r = Math.abs(a.r - b.r);
    res.c = Math.abs(a.c - b.c);
    return res;
  }
}

class ChessField {
  ImageIcon loadImage(String relativePath) throws java.io.IOException {
    BufferedImage picture = ImageIO.read(new File(relativePath));
    Image scaled = picture.getScaledInstance(60, 60, Image.SCALE_SMOOTH);
    return new ImageIcon(scaled);
  }

  String[][] field;
  HashMap<String, ImageIcon> chessImages = new HashMap<String, ImageIcon>();
  HashMap<String, String> chessLetters = new HashMap<>() {
    {
      put("b-k", "♚"); // black-king
      put("b-q", "♛"); // black-queen
      put("b-t", "♜"); // black-tower
      put("b-b", "♝"); // black-bishop
      put("b-h", "♞"); // black-knight (aka 'horse' (for key // consistency))
      put("b-p", "♟"); // black-pawn
      put("w-k", "♔"); // white-king
      put("w-q", "♕"); // white-queen
      put("w-t", "♖"); // white-tower
      put("w-b", "♗"); // white-bishop
      put("w-h", "♘"); // white-knight (aka 'horse' (for key // consistency))
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
            // (Cell is not empty) and color of the chess piece is opposite of
            // ours
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
      new Object[] {"Фигура", "Клетка n-1", "Клетка n"}, 0);

  JButton gameInit = new JButton("Начать игру");

  ChessField field;
  Pos prevPos;

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
            field.getChessletter(code),  //
            prevPos.r + " " + prevPos.c, //
            nowPos.r + " " + nowPos.c    //
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
    fieldModel = new DefaultTableModel(new Object[] {"", "A", "B", "C"}, 0) {
      @Override
      public Class<?> getColumnClass(int column) {
        if (column > 0) {
          return ImageIcon.class;
        }

        return Object.class;
      }
    };

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

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(LYTBuilder.makeAnnotatedInput("Игровое поле",
                                          LYTBuilder.makeScrollable(gameField),
                                          BoxLayout.Y_AXIS));

        add(LYTBuilder.makeAnnotatedInput("Таблица ходов",
                                          LYTBuilder.makeScrollable(moveTable),
                                          BoxLayout.Y_AXIS));
      }
    }));

    add(Misc.setMax_WH(gameInit, 96, 24));

    gameField.getColumnModel().getColumn(0).setMaxWidth(8);
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

            JOptionPane.showMessageDialog(null, "Game over", "Err",
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

  void doMove() {
    int attempts = 1;
    Random rand = new Random();
    int row = rand.nextInt(3);
    int col = rand.nextInt(3);
    while (!isEmpty(row, col) && attempts < 9) {
      ++attempts;
      row = rand.nextInt(3);
      col = rand.nextInt(3);
    }

    if (attempts > 9) {
      Pprint("Made more than 9 attempts to find empty cell");
    }

    field[row][col] = "o";
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

  Boolean isFinished() {
    int dim = 3;
    // check horizontal
    for (int row = 0; row < dim; ++row) {
      String rstring = String.join("", field[row]);
      if (rstring.equals("xxx") || rstring.equals("ooo")) {
          Pprint("ok: " + rstring );
        return true;
      } else {
        Pprint("Does not match:" + rstring);
      }
    }

    // check vertical
    for (int col = 0; col < dim; ++col) {
      String cstring = field[col][0] + field[col][1] + field[col][2];
      if (cstring.equals("xxx") || cstring.equals("ooo")) {
          Pprint("ok: " + cstring );
        return true;
      } else {
        Pprint("Does not match:" + cstring);
      }
    }

    String ldiag = field[0][0] + field[1][1] + field[2][2];
    String rdiag = field[0][2] + field[1][1] + field[2][0];

    if (ldiag.equals("xxx")    //
        || ldiag.equals("ooo") //
        || rdiag.equals("xxx") //
        || rdiag.equals("ooo")) {
      return true;
    } else {
      Pprint("Does not match:" + ldiag);
      Pprint("Does not match:" + rdiag);
    }

    return false;
  }
}

class TicTacToe extends JFrame {
  JTable fieldTable = new JTable();
  JButton computerMove = new JButton("Начинает компьютер");
  Boolean gameOver = false;
  TicTacField field = new TicTacField();

  void cellPressed(int rowIdx, int colIdx) {
    if (field.isEmpty(rowIdx, colIdx)) {
      field.setCell(rowIdx, colIdx);
    }

    if (!field.isFinished()) {
      field.doMove();
    }

    gameOver = field.isFinished();

    if (gameOver) {
      Pprint("Game over");
    }

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

    fieldTable.setRowHeight(100);
    fieldTable.setFont(new Font("Serif", Font.BOLD, 72));

    fieldTable.addMouseListener(new java.awt.event.MouseAdapter() {
      @Override
      public void mouseClicked(java.awt.event.MouseEvent evt) {
        int row = fieldTable.rowAtPoint(evt.getPoint());
        int col = fieldTable.columnAtPoint(evt.getPoint());
        if (row >= 0 && col >= 0 && !gameOver) {
          System.out.println("Pressed at: " + row + " " + col);
          cellPressed(row, col);
          if (gameOver) {
            JOptionPane.showMessageDialog(null, "Game over", "Err",
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

class MainFrame extends JFrame {
  JTabbedPane tabs = new JTabbedPane();

  void initUI() {
    var chess = new MicroChess();
    var ticTacToe = new TicTacToe();
    tabs.addTab("Переставь фигуры", chess.getContentPane());
    tabs.addTab("Крестики-нолики", ticTacToe.getContentPane());

    add(tabs);
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
