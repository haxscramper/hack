import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;

include(`misc.m4.java');
include(`bits.m4.java');
include(`ui-misc.m4.java');

class TextFieldExample {
  static JTextArea textArea;
  static JFrame frame;
  static BoxLayout layout;
  static JLabel outRes;
  static JButton runButton;

  static JMenuBar menuBar;

  static void initUI() {
    frame = new JFrame("TextField Example");

    BoxLayout boxLayout =
        new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS);
    frame.setLayout(boxLayout);

    textArea = new JTextArea("Тестовый текст"
                             + "\nВторая строкa");

    runButton = new JButton("Run");
    outRes = new JLabel("");

    frame.add(textArea);
    frame.add(runButton);
    frame.add(outRes);

    menuBar = new JMenuBar();
    var menu = new JMenu("...");

    var helpItem = new JMenuItem("Help");
    menu.add(helpItem);
    helpItem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("Displaying help");

        JOptionPane.showMessageDialog(
            null,
            "Введите текст в поле и нажмите кнопку 'Run' для подсчета"
                + "непробельных символов в тексте.\n"
                + "Для закрытия окна помощи нажмите 'Ok'",
            "Help",
            JOptionPane.QUESTION_MESSAGE | JOptionPane.OK_CANCEL_OPTION);
      }
    });

    menuBar.add(menu);
    frame.setJMenuBar(menuBar);

    frame.pack();
    frame.setVisible(true);
  }

  static void initSignals() {
    runButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        String newLabel = "Количество символов в тексте: " + getCharCount();
        outRes.setText(newLabel);
        System.out.println(newLabel);
      }
    });
  }

  static int getCharCount() {
    int res = 0;
    String text = textArea.getText();
    for (int i = 0; i < text.length(); ++i) {
      char c = text.charAt(i);
      if (c != ' ' && c != '\n') {
        ++res;
      }
    }
    return res;
  }

  public static void main(String args[]) {
    initUI();
    initSignals();
  }
}

class NumberStructure extends JFrame {
  JLabel baseLabel;
  JLabel base10_conversion;
  JButton doShiftLeft_btn;
  JButton doShiftRight_btn;

  JLabel numberBits_lbl;
  JLabel numberSign_lbl;
  JTable numberRepr_tbl;

  JTextField num_input_fld;
  JButton reverse_btn;

  Bits bits;

  void initUI() {
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    baseLabel = new JLabel("Base 10 ");
    base10_conversion = new JLabel();
    doShiftRight_btn = new JButton(">>");
    doShiftLeft_btn = new JButton("<<");

    numberBits_lbl = new JLabel("биты числа");
    numberSign_lbl = new JLabel("знак");

    num_input_fld = new JTextField();

    reverse_btn = new JButton("Reverse");

    String data[][] = {{"101", "Amit", "670000"},
                       {"102", "Jai", "780000"},
                       {"101", "Sachin", "700000"}};
    String column[] = {"ID", "NAME", "SALARY"};
    numberRepr_tbl = new JTable(data, column);

    getContentPane().setLayout(
        new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(base10_conversion);
        add(doShiftLeft_btn);
        add(doShiftRight_btn);
      }
    }));

    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(numberSign_lbl);
        add(numberBits_lbl);
      }
    }));

    add(numberRepr_tbl);
    add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(num_input_fld);
        add(reverse_btn);
      }
    }));
  }

  Optional<Integer> readInt() {
    try {
      var num = Integer.parseInt(num_input_fld.getText());
      return Optional.of(num);
    } catch (NumberFormatException e) {
      System.out.println("Error " + e.getMessage());
      Misc.showErrorMessage("Invalid input: enter integer");
      return Optional.empty();
    }
  }

  void setInt(Integer in) { num_input_fld.setText(Integer.toString(in)); }

  void update() {
    var num = readInt();
    if (num.isPresent()) {
      base10_conversion.setText(Integer.toString(num.get()));
    }
  }

  public NumberStructure() {
    initUI();
    initSignals();
  }

  void doShift(char dir) {
    var num = readInt();
    if (num.isPresent()) {
      bits = Bits.fromNum(num.get());

      // XXXX Constant for selecting target integer size
      bits.resize(32);
      bits.doShift(dir);
      setInt(bits.toNum());
    }
  }

  void initSignals() {
    doShiftLeft_btn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { doShift('l'); }
    });

    doShiftRight_btn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { doShift('r'); }
    });

    reverse_btn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { update(); }
    });
  }

  void run() { show(); }
}

class MainFrame extends JFrame {
  static void runA() {
    var frame = new NumberStructure();
    frame.run();
  }

  static void runB() {}
  static void runC() {}
  static void runD() {}

  public static void main(String[] args) {
    if (args.length > 0) {
      if (args[0].equals("a")) {
        runA();
      } else if (args[0].equals("b")) {
        runB();
      } else if (args[0].equals("c")) {
        runC();
      } else if (args[0].equals("d")) {
        runD();
      }
    }
  }
}
