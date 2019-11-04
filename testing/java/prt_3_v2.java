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
  JTextField inNum = new JTextField();
  JButton shiftLeft = new JButton("<<");
  JButton shiftRight = new JButton(">>");
  JLabel signLbl = new JLabel("");
  JLabel bitsLbl = new JLabel("");

  JTable reprTable;
  JTextField reverse = new JTextField();

  Optional<String> getBits() {
    Optional<Integer> inVal = Misc.getInteger(inNum, "????");
    if (inVal.isPresent()) {
      return Optional.of(Convert.toBits(inVal.get()));
    } else {
      return Optional.empty();
    }
  }

  void numberEntered() {
    var bits = getBits();
    if (bits.isPresent()) {
      setNumber(bits.get());
    }
  }

  void doShiftLeft() {
    var bits = getBits();
    if (bits.isPresent()) {
      setNumber(Convert.shiftLeft(bits.get()));
    }
  }

  void doShiftRight() {
    var bits = getBits();
    if (bits.isPresent()) {
      setNumber(Convert.shiftRight(bits.get()));
    }
  }

  void setNumber(String bits) {
    for (int i = 0; i < 8 + 1; ++i) {
      reprTable.setValueAt(bits.charAt(i), 8 - i, 0);
    }
  }

  void initUI() {
    var tModel = new DefaultTableModel(new Object[] {""}, 0);
    tModel.setRowCount(1);
    tModel.setColumnCount(8 + 1);

    reprTable = new JTable(tModel);

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(LYTBuilder.makeAnnotatedInput("annotation_string", inNum,
                                          BoxLayout.Y_AXIS));
        add(shiftLeft);
        add(shiftRight);
      }
    }));

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(signLbl);
        add(bitsLbl);
      }
    }));

    add(LYTBuilder.makeScrollable(reprTable));
    add(LYTBuilder.makeAnnotatedInput("annotation_string", reverse,
                                      BoxLayout.X_AXIS));

    inNum.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { numberEntered(); }
    });

    shiftLeft.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { doShiftLeft(); }
    });

    shiftRight.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { doShiftRight(); }
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

  void run() {}

  void initUI() {
    var tModel = new DefaultTableModel(new Object[] {"da"}, 0);
    tModel.setRowCount(1);
    tModel.setColumnCount(3);

    reprTable = new JTable(tModel);

    add(LYTBuilder.makeAnnotatedInput("annotation_string", numIn,
                                      BoxLayout.Y_AXIS));

    add(LYTBuilder.makeScrollable(reprTable));

    add(LYTBuilder.makeAnnotatedInput("annotation_string", outNum,
                                      BoxLayout.Y_AXIS));

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
        add(LYTBuilder.makeAnnotatedInput("Символ", charIn, BoxLayout.Y_AXIS));
        add(LYTBuilder.makeAnnotatedInput("Десятичный код", intValue,
                                          BoxLayout.Y_AXIS));
      }
    }));

    add(LYTBuilder.makeScrollable(reprTable));
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
