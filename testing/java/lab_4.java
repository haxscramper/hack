import java.awt.event.*;
import java.util.*;
import java.util.ArrayList;
import java.util.regex.*;
import javax.swing.*;
import javax.swing.table.*;

include(utils.m4);
include(misc.m4.java);
include(`ui-misc.m4.java');

class FieldArraySort extends JFrame {
  JTextField inField = new JTextField();
  JButton button = new JButton("Выполнить");
  JTextArea outArea = new JTextArea();

  void runComputation() {

    var outString = new String();

    outString += "Массив а\n";
    var inText = inField.getText();
    outString += inText + "\n";

    Integer positiveSum = 0;

    for (String num : inText.split(" ")) {
      var n = Integer.parseInt(num);
      if (n > 0) {
        positiveSum += n;
      }
    }

    outString += "Сумма положительных элементов массива\n";
    outString += positiveSum;

    outArea.setText(outString);
  }

  void initUI() {
    this.add(LYTBuilder.makeAnnotatedInput("Исходный массив", inField,
                                           BoxLayout.Y_AXIS));

    this.add(button);
    this.add(outArea);

    button.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { runComputation(); }
    });

    inField.addKeyListener(new KeyAdapter() {
      public void keyReleased(KeyEvent e) {
        String value = inField.getText();
        if (e.getKeyChar() == '-' || e.getKeyChar() == ' ' ||
            ('0' <= e.getKeyChar() && e.getKeyChar() <= '9')) {
          System.out.println("ok");
        } else {
          System.out.println("bad input");
          System.out.println(value);

          inField.setText(value.substring(0, value.length() - 2));
        }
      }
    });
  }

  FieldArraySort() {
    initUI();
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}

class ArraySorter extends JFrame {
  JTable inputTable;
  JTable outTable;

  JButton button = new JButton("Выполнить");
  JTextArea textArea = new JTextArea();

  void sortArray() {

    var input = new ArrayList<Float>();
    var model = (DefaultTableModel)inputTable.getModel();

    for (int i = 1; i < inputTable.getColumnCount(); ++i) {
      var num = 0.0f;
      try {
        num = Float.parseFloat(model.getValueAt(0, i).toString());
        input.add(num);
      } catch (Exception e) {
          System.out.println(e);
        Misc.showErrorMessage(
            "Ошибка при обработке значение в колонке " + (i - 1) +
            "\nВведенная строка не можут быть обработана как Float");
      }
    }


    var outString = new String();

    outString += "Массив а\n";

    Float positiveSum = 0f;

    for (Float num : input) {
       outString += num  + " ";
      if (num > 0) {
        positiveSum += num;
      }
    }

    outString += "\n";
    outString += "Сумма положительных элементов массива\n";
    outString += positiveSum;

   textArea.setText(outString);


    var N = input.size() - 1;
    for (int i = 0; i <= N - 1; i++) {
      for (int j = i + 1; j <= N; j++) {
        if (input.get(i) < input.get(j))
          Collections.swap(input, i, j);
      }
    }

    for (int i = 0; i < input.size(); ++i) {
      outTable.setValueAt(input.get(i), 0, i + 1);
    }
  }

  void initUI() {
    var inTableModel = new DefaultTableModel(
        new Object[] {"Индексы ячеек", 1, 2, 3, 4, 5, 6}, 0);
    inTableModel.setRowCount(1);

    inputTable = new JTable(inTableModel);
    inputTable.setValueAt("Значения ячеек", 0, 0);

    inputTable.setValueAt(-2, 0, 1);
    inputTable.setValueAt(1.3, 0, 2);
    inputTable.setValueAt(18, 0, 3);
    inputTable.setValueAt(6, 0, 4);
    inputTable.setValueAt(20, 0, 5);
    inputTable.setValueAt(1, 0, 6);

    var outTableModel = new DefaultTableModel(
        new Object[] {"Индексы ячеек", 1, 2, 3, 4, 5, 6}, 0);
    outTableModel.setRowCount(1);

    outTable = new JTable(outTableModel);
    outTable.setValueAt("Значения ячеек", 0, 0);

    add(LYTBuilder.makeAnnotatedInput("Исходый массив",
                                      LYTBuilder.makeScrollable(inputTable),
                                      BoxLayout.Y_AXIS));

    add(Misc.setMax_WH(button, -1, 48));

    button.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { sortArray(); }
    });

    add(textArea);
    add(LYTBuilder.makeAnnotatedInput("Отсортированный массив",
                                      LYTBuilder.makeScrollable(outTable),
                                      BoxLayout.Y_AXIS));
  }

  ArraySorter() {
    initUI();
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    this.setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
  }
}


class Main {
  static void runTextSorter() {
    var frame = new FieldArraySort();
    frame.show();
  }

  static void runArraySorter() {
    var frame = new ArraySorter();
    frame.show();
  }

  public static void main(String args[]) {
    if (args.length > 0 && args[0].equals("b")) {
      runTextSorter();
    } else if (args.length > 0 && args[0].equals("c")) {
      runArraySorter();
    } else {
      System.out.println(
          "Input 'b' or 'c' as argument to select operation mode");
    }
  }
}
