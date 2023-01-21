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
