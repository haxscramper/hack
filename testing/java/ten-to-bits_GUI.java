import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;

class Misc {
  public static void showErrorMessage(String msg) {
    JOptionPane.showMessageDialog(null, msg, "Err",
                                  JOptionPane.ERROR_MESSAGE |
                                      JOptionPane.OK_CANCEL_OPTION);
  }
  public static String toString(ArrayList<Boolean> in) {
    return (
        in.stream().map(n -> n ? "1" : "0").collect(Collectors.joining("")));
  }
}

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

    Container contentPanel = getContentPane();
    GroupLayout groupLayout = new GroupLayout(contentPanel);

    contentPanel.setLayout(groupLayout);

    groupLayout.setHorizontalGroup(  //
        groupLayout                  //
            .createSequentialGroup() //
            .addGroup(               //
                groupLayout
                    .createParallelGroup() //
                    .addComponent(baseLabel)
                    .addComponent(base10_conversion)
                    .addComponent(numberRepr_tbl)
                    .addComponent(num_input_fld)

                    )
            .addGroup( //
                groupLayout
                    .createParallelGroup() //
                    .addComponent(doShiftLeft_btn)
                    .addComponent(numberSign_lbl)
                    .addComponent(reverse_btn)

                    )
            .addGroup( //
                groupLayout
                    .createParallelGroup() //
                    .addComponent(doShiftRight_btn)
                    .addComponent(numberBits_lbl)

                    ));

    groupLayout.setVerticalGroup(    //
        groupLayout                  //
            .createSequentialGroup() //
            .addGroup(               //
                groupLayout
                    .createParallelGroup() //
                    .addComponent(baseLabel)
                    .addComponent(doShiftLeft_btn)
                    .addComponent(doShiftRight_btn))
            .addGroup( //
                groupLayout
                    .createSequentialGroup() //
                    .addComponent(base10_conversion))
            .addGroup( //
                groupLayout
                    .createParallelGroup() //
                    .addComponent(numberSign_lbl)
                    .addComponent(numberBits_lbl))
            .addGroup( //
                groupLayout
                    .createParallelGroup() //
                    .addComponent(numberRepr_tbl))
            .addGroup( //
                groupLayout
                    .createParallelGroup() //
                    .addComponent(num_input_fld)
                    .addComponent(reverse_btn)));

    this.pack();
  }

  ArrayList<Boolean> toBits(int num) {
    var res = new ArrayList<Boolean>();

    while (num > 0) {
      res.add(num % 2 == 1);
      num = num / 2;
    }

    return res;
  }

  int fromBits(ArrayList<Boolean> bits) {
    int res = 0;
    for (int idx = 0; idx < bits.size(); ++idx) {
      res += (int)Math.pow(2, idx) * (bits.get(idx) ? 1 : 0);
    }
    return res;
  }

  ArrayList<Boolean> doShift(ArrayList<Boolean> in, Boolean right) {
    if (right) {
      for (int i = 0; i < in.size() - 1; ++i) {
        in.set(i, in.get(i + 1));
      }
      in.set(in.size() - 1, false);
    } else {
      for (int i = in.size() - 1; i > 1; --i) {
        in.set(i, in.get(i - 1));
      }
    }

    return in;
  }

  ArrayList<Boolean> resize(ArrayList<Boolean> in, int targetSize) {
    if (in.size() > targetSize) {
      return new ArrayList<Boolean>(
          in.subList(targetSize - in.size() - 1, in.size() - 1));
    } else {
      for (int i = in.size(); i <= targetSize; ++i) {
        in.add(false);
      }
      var res = new ArrayList<Boolean>(
          Collections.nCopies(targetSize - in.size(), false));
      res.addAll(in);
      return res;
    }
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

  void initSignals() {
    doShiftLeft_btn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("Requested shift left");
        var num = readInt();
        if (num.isPresent()) {
          System.out.println("Input value is" + num.get());
          var bits = resize(toBits(num.get()), 16);
          System.out.println("Bit represenetation: " + Misc.toString(bits));
          bits = doShift(bits, false);
          System.out.println("After shift: " + Misc.toString(bits));

          setInt(fromBits(bits));
        }
      }
    });

    doShiftRight_btn.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.out.println("Requested right shift");
      }
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
