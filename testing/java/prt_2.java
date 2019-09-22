import java.awt.event.*;
import java.util.ArrayList;
import java.util.regex.*;
import javax.swing.*;

include(misc.m4.java);

class CountChars {
    JTextArea textArea;
    JFrame    frame;
    BoxLayout layout;
    JLabel    outRes;
    JButton   runButton;

    void initUI() {
        frame = new JFrame("TextField Example");

        BoxLayout boxLayout = new BoxLayout(
            frame.getContentPane(), BoxLayout.Y_AXIS);
        frame.setLayout(boxLayout);

        textArea = new JTextArea(
            "Тестовый текст"
            + "\nВторая строкa");

        runButton = new JButton("Run");
        outRes    = new JLabel("");

        frame.add(textArea);
        frame.add(runButton);
        frame.add(outRes);

        Misc.addHelpMenuItem(
            frame,
            "Введите текст в поле и нажмите кнопку 'Run' для подсчета"
                + "непробельных символов в тексте.\n"
                + "Для закрытия окна помощи нажмите 'Ok'");

        frame.pack();
    }

    void initSignals() {
        runButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String newLabel = "Количество символов в тексте: "
                                  + getCharCount();
                outRes.setText(newLabel);
                System.out.println(newLabel);
            }
        });
    }

    int getCharCount() {
        int    res  = 0;
        String text = textArea.getText();
        for (int i = 0; i < text.length(); ++i) {
            char c = text.charAt(i);
            if (c != ' ' && c != '\n') {
                ++res;
            }
        }
        return res;
    }

    public CountChars() {
        initUI();
        initSignals();
    }

    public void run() {
        frame.setVisible(true);
    }
}

class FilterNumbers {
    JTextArea textArea;
    JFrame    frame;
    BoxLayout layout;
    JButton   runButton;

    JMenuBar menuBar;

    void initUI() {
        frame = new JFrame("TextField Example");

        BoxLayout boxLayout = new BoxLayout(
            frame.getContentPane(), BoxLayout.Y_AXIS);
        frame.setLayout(boxLayout);

        textArea = new JTextArea(
            "Тестовый текст"
            + "\nВторая строкa\n 12\n23 234  2333345"
            + "\nЭто число должно исчезнуть: 86000"
            + "\nЭто число должно исчезнуть: 899996"
            + "\nЭто число должно исчезнуть: 699999990"

        );

        runButton = new JButton("Run");

        frame.add(textArea);
        frame.add(runButton);

        menuBar  = new JMenuBar();
        var menu = new JMenu("...");

        Misc.addHelpMenuItem(
            frame,
            "Введите текст в поле и нажмите кнопку 'Run' для подсчета"
                + "непробельных символов в тексте.\n"
                + "Для закрытия окна помощи нажмите 'Ok'");

        frame.pack();
    }

    void initSignals() {
        runButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                filterText();
            }
        });
    }

    void filterText() {
        String in     = textArea.getText();
        var    okNums = new ArrayList<String>();

        Matcher m = Pattern.compile("[0-9]+").matcher(in);
        while (m.find()) {
            String  num  = m.group();
            int     prev = 0;
            boolean isOk = true;
            for (int i = 0; i < num.length(); ++i) {
                int parsed = Integer.parseInt(
                    String.valueOf(num.charAt(i)));
                if (parsed % 2 == 0) {
                    if (prev > parsed) {
                        isOk = false;
                        break;
                    }
                    prev = parsed;
                }
            }

            if (isOk) {
                okNums.add(num);
            }
        }

        String resText = "";
        for (int i = 0; i < okNums.size();) {
            resText += okNums.get(i);
            if (++i < okNums.size()) {
                resText += "\n\n";
            }
        }

        textArea.setText(resText);
    }

    public FilterNumbers() {
        initUI();
        initSignals();
    }

    public void run() {
        frame.setVisible(true);
    }
}

class Main {
    static void runCharCounter() {
        CountChars counter = new CountChars();
        counter.run();
    }

    static void runNumberFilter() {
        FilterNumbers filter = new FilterNumbers();
        filter.run();
    }

    public static void main(String args[]) {
        if (args.length > 0 && args[0] == "a") {
            runCharCounter();
        } else if (args.length > 0 && args[0].equals("b")) {
            runNumberFilter();
        } else {
            System.out.println(
                "Input 'a' or 'b' as argument to select operation mode");
        }
    }
}
