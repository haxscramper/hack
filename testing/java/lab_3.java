import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;

include(`misc.m4.java');
include(`bits.m4.java');
include(`ui-misc.m4.java');

class MainWindow extends JFrame {
    JTextField in_a;
    JTextField in_b;
    JTextField in_c;
    JTextField in_x;

    JButton    compute;
    JTextField out_y;

    JTextField in_x_start;
    JTextField in_x_final;
    JTextField in_x_step;

    JButton computeTable;

    JTextArea tableRes;

    Float evalY(Float a, Float b, Float c, Float x) {
        return (
            float)((a * x + 3.8 * Math.tan(x)) / (Math.sqrt(b * x * x + c)));
    }

    void initUI() {
        // Create widgets
        {
            in_a = new JTextField();
            in_b = new JTextField();
            in_c = new JTextField();
            in_x = new JTextField();

            in_a.setText("1");
            in_b.setText("12.2");
            in_c.setText("-2.1232");
            in_x.setText("-9");

            compute = new JButton("Вычислить");
            out_y   = new JTextField();

            in_x_start = new JTextField();
            in_x_final = new JTextField();
            in_x_step  = new JTextField();

            in_x_start.setText("1");
            in_x_final.setText("21");
            in_x_step.setText("0.5");

            computeTable = new JButton("Вычислить таблицу значений");
            tableRes = new JTextArea();
        }
    }

    Optional<Float> getVal(char valName) {
        String in;

        if (valName == 'a') {
            in = in_a.getText();
        } else if (valName == 'b') {
            in = in_b.getText();
        } else if (valName == 'c') {
            in = in_c.getText();
        } else if (valName == 'x') {
            in = in_x.getText();
        } else {
            throw new IllegalArgumentException(
                "Unkndon input field: " + valName);
        }

        try {
            var res = Float.parseFloat(in);
            return Optional.of(res);
        } catch (Exception e) {
            Misc.showErrorMessage(
          "Некорректно введено число в поле '" + valName +
          "': требется ввести целое число или число с плавающей точкой.");
            return Optional.empty();
        }
    }

    void computeY() {
        var a = getVal('a');
        var b = getVal('b');
        var c = getVal('c');
        var x = getVal('x');

        if (a.isPresent() && b.isPresent() && c.isPresent()
            && x.isPresent()) {
            Float y = evalY(a.get(), b.get(), c.get(), x.get());
            out_y.setText(Float.toString(y));
        } else {
            System.out.println("Invalid input");
            out_y.setText("");
        }
    }

    void computeTable() {
        var x_start = Misc.getFloat(
            in_x_start, "Некорректно введено начальное х");
        var x_final = Misc.getFloat(
            in_x_final, "Некорректно введено конечное х");
        var x_step = Misc.getFloat(in_x_step, "Некорректно введен шаг х");
        if (x_start.isPresent() && x_final.isPresent()
            && x_step.isPresent()) {
            var a = getVal('a');
            var b = getVal('b');
            var c = getVal('c');

            if (a.isPresent() && b.isPresent() && c.isPresent()) {
                Float  x_current = x_start.get();
                String res       = "a\tb\tc\tx\ty\n";
                while (x_current <= x_final.get()) {
                    res =                  //
                        res +              //
                        a.get() + "\t" +   //
                        b.get() + "\t" +   //
                        c.get() + "\t" +   //
                        x_current + "\t" + //
                        evalY(a.get(), b.get(), c.get(), x_current) + "\n";
                    x_current += x_step.get();
                }
                tableRes.setText(res);
            } else {
                System.out.println("Invalid input for a || b || c");
            }
        } else {
            System.out.println("Invalid input for table");
        }
    }

    MainWindow() {
        initUI();
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setLayout(
            new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));

        add(LYTBuilder.makeAnnotatedInput("a", in_a, BoxLayout.X_AXIS));
        add(LYTBuilder.makeAnnotatedInput("b", in_b, BoxLayout.X_AXIS));
        add(LYTBuilder.makeAnnotatedInput("c", in_c, BoxLayout.X_AXIS));
        add(LYTBuilder.makeAnnotatedInput("x", in_x, BoxLayout.X_AXIS));

        add(compute);

        add(LYTBuilder.makeAnnotatedInput(
            "y = ", out_y, BoxLayout.X_AXIS));

        add(computeTable);

        add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
            {
                add(LYTBuilder.makeAnnotatedInput(
                    "x начальное", in_x_start, BoxLayout.Y_AXIS));
                add(LYTBuilder.makeAnnotatedInput(
                    "x конечное", in_x_final, BoxLayout.Y_AXIS));
                add(LYTBuilder.makeAnnotatedInput(
                    "x шаг", in_x_step, BoxLayout.Y_AXIS));
            }
        }));

        add(LYTBuilder.makeScrollable(tableRes));

        compute.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                computeY();
            }
        });

        computeTable.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                computeTable();
            }
        });
    }
}


public class HelloWorld {
    public static void main(String[] args) {
        var window = new MainWindow();
        window.show();
    }
}
