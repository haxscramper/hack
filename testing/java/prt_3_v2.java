import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;
import javax.swing.table.*;

// clang-format off
include(pair.m4.java);
include(misc.m4.java);
include(bits.m4.java);
include(ui-misc.m4.java);
include(prt_3_conversions.java);
// clang-format on

class IntegerStructure extends JFrame {
  int bitsNum = 16;

  JTextField inNum = new JTextField();
  JButton shiftLeft = new JButton("<<");
  JButton shiftRight = new JButton(">>");
  JLabel signLbl = new JLabel("Знак");
  JLabel bitsLbl = new JLabel("Биты числа");
  JButton convertBack = new JButton("Обратный перевод");

  JTable reprTable;
  JTextField reverse = new JTextField();

  Optional<String> getBits() {
    Optional<Integer> inVal = Misc.getInteger(
        inNum, "Ввод не может бысть распознан как целое число");
    if (inVal.isPresent()) {
      return Optional.of(Convert.toBits(inVal.get()));
    } else {
      return Optional.empty();
    }
  }

  String getTableBits() {
    String res = "";
    for (int i = 0; i < bitsNum; ++i) {
      res += (char)reprTable.getValueAt(0, i + 1);
    }
    System.out.printf("table bits  : %s\n", res);
    return res;
  }

  void numberEntered() {
    var bits = getBits();
    if (bits.isPresent()) {
      System.out.printf("Parsed number to bits %s\n", bits.get());
      setNumber(bits.get());
    }
  }

  void doShiftLeft() {
    var bits = getTableBits();
    pprint("shifting left");
    setNumber(Convert.shiftLeft(bits));
  }

  void doShiftRight() {
    var bits = getTableBits();
    pprint("shifting right");
    setNumber(Convert.shiftRight(bits));
  }

  void setNumber(String bits) {
    System.out.printf("setting bits: %s\n", bits);
    for (int i = 0; i < bitsNum; ++i) {
      reprTable.setValueAt(bits.charAt(i), 0, 1 + i);
    }
  }

  void convertBack() {
    short res = Convert.fromBits(getTableBits());
    reverse.setText(String.valueOf(res));
  }

  void initUI() {
    var tModel = new DefaultTableModel(
        new Object[] {            //
                      "Вес бита", //

                      "32768", //
                      "16384", //
                      "8192",  //
                      "4096",  //

                      "2048", //
                      "1024", //
                      "512",  //
                      "256",  //

                      "128", //
                      "64",  //
                      "32",  //
                      "16",  //

                      "8", //
                      "4", //
                      "2", //
                      "1"

        },
        0);

    tModel.setRowCount(1);
    tModel.setColumnCount(bitsNum + 1);

    reprTable = new JTable(tModel);

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(LYTBuilder.makeAnnotatedInput("Число в десятичной СС", inNum,
                                          BoxLayout.Y_AXIS,
                                          Misc.whd(360, 48)));
        add(shiftLeft);
        add(shiftRight);
      }
    }));

    signLbl.setBackground(Color.blue);
    bitsLbl.setBackground(Color.yellow);
    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(signLbl);
        add(Misc.setMin_WH(bitsLbl, 480, 48));
      }
    }));

    add(LYTBuilder.makeScrollable(reprTable));
    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(Misc.setMax_WH(reverse, 360, 48));
        add(Misc.setMax_WH(convertBack, 360, 48));
      }
    }));

    inNum.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { numberEntered(); }
    });

    shiftLeft.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { doShiftLeft(); }
    });

    shiftRight.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { doShiftRight(); }
    });

    convertBack.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { convertBack(); }
    });
  }

  IntegerStructure() {
    initUI();
    this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class FloatStructure extends JFrame {
  JTextField inNum = new JTextField();
  JTable reprTable;
  JTextField reverse = new JTextField();

  void run() {}

  void initUI() {
    var tModel = new DefaultTableModel(new Object[] {""}, 0);
    tModel.setRowCount(4);
    tModel.setColumnCount(32 + 1);

    reprTable = new JTable(tModel);

    add(LYTBuilder.makeAnnotatedInput("annotation_string", inNum,
                                      BoxLayout.X_AXIS, 48));

    add(LYTBuilder.makeScrollable(reprTable));

    add(LYTBuilder.makeAnnotatedInput("annotation_string", reverse,
                                      BoxLayout.X_AXIS, 48));

    inNum.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { run(); }
    });
  }

  FloatStructure() {
    initUI();
    this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class DoubleStructure extends JFrame {
  JTextField numIn = new JTextField();
  JTextField outNum = new JTextField();
  JTable reprTable;

  void run() {
    var dbl = Misc.getDouble(
        numIn, "Введенное число не может быть распознано как double");
    if (dbl.isPresent()) {
      Double num = dbl.get();
      reprTable.setValueAt(Convert.getSign(num), 0, 0);
      reprTable.setValueAt(Convert.getExponent(num), 0, 1);
      reprTable.setValueAt(Convert.getMantissa(num), 0, 2);
    }
  }

  void initUI() {
    var tModel = new DefaultTableModel(new Object[] {"da"}, 0);
    tModel.setRowCount(1);
    tModel.setColumnCount(3);

    reprTable = new JTable(tModel);

    add(LYTBuilder.makeAnnotatedInput("Число", numIn,
                                      BoxLayout.Y_AXIS, Misc.whd(240, 48)));

    add(LYTBuilder.makeScrollable(reprTable));

    add(LYTBuilder.makeAnnotatedInput("Обратный перевод", outNum,
                                      BoxLayout.Y_AXIS, Misc.whd(240, 48)));

    numIn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { run(); }
    });
  }

  DoubleStructure() {
    initUI();
    this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class CharStructure extends JFrame {
  JTextField charIn = new JTextField();
  JLabel intValue = new JLabel();
  JTable reprTable;

  void charEntered() {
    char c = charIn.getText().charAt(0);
    System.out.printf("User entered char '%s'\n", c);
    var hex = Convert.tetrades(c);
    var bits = Convert.hexTetrades(c);
    for (int col = 1; col < 5; ++col) {
      int i = col - 1;
      reprTable.setValueAt(bits[i], 0, col);
      reprTable.setValueAt(hex[i], 1, col);
    }
  }

  void initUI() {
    var model = new DefaultTableModel(
        new Object[] {"Тетрады", "Тетрада 3 (15-12)", "Тетрада 2 (11-8)",
                      "Тетрада 1 (7-4)", "Тетрада 0 (3-0)"},
        0);

    model.setRowCount(2);
    model.setColumnCount(5);

    reprTable = new JTable(model);

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(LYTBuilder.makeAnnotatedInput(
            "Символ", charIn, BoxLayout.Y_AXIS, Misc.whd(240, 48)));

        add(LYTBuilder.makeAnnotatedInput("Десятичный код", intValue,
                                          BoxLayout.Y_AXIS,
                                          Misc.whd(240, 48)));
      }
    }));

    add(LYTBuilder.makeScrollable(reprTable));

    charIn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { charEntered(); }
    });
  }

  CharStructure() {
    initUI();
    this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class MainFrame extends JFrame {
  CharStructure cstruct = new CharStructure();
  DoubleStructure dstruct = new DoubleStructure();
  FloatStructure fstruct = new FloatStructure();
  IntegerStructure istruct = new IntegerStructure();

  JMenuBar menuBar = new JMenuBar();
  JMenu menu = new JMenu("...");

  void initUI() {
    var floatStructItem = new JMenuItem("flt");
    var doubleStructItem = new JMenuItem("dbl");
    var integerStructItem = new JMenuItem("int");
    var charStructItem = new JMenuItem("char");

    menu.add(floatStructItem);
    menu.add(doubleStructItem);
    menu.add(integerStructItem);
    menu.add(charStructItem);

    floatStructItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { fstruct.show(); }
    });

    doubleStructItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { dstruct.show(); }
    });

    integerStructItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { istruct.show(); }
    });

    charStructItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { cstruct.show(); }
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
  public static void main(String[] args) {
    if (args.length > 0 && args[0].equals("main")) {
      var mframe = new MainFrame();
      mframe.show();
    } else {
      System.out.println("headless test");
      Convert.runTests();
    }
  }
}
