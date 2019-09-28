import java.util.*;
import java.util.stream.*;
import javax.swing.*;

class CharStructure extends JFrame {
    JTextField charIn_fld   = new JTextField();
    JLabel     intValue_lbl = new JLabel();
    JTable     reprTable;
    Character  chr;

    void setupTable() {
        // TODO highlight cell colors
        TableModel dataModel = new AbstractTableModel() {
            public int getRowCount() {
                return 2;
            }
            public int getColumnCount() {
                return 5;
            }
            public Object getValueAt(int row, int col) {
                if (chr == null) {
                    return "";
                } else {
                    if (col == 0) {
                        if (row == 0) {
                            return "Биты в тетраде";
                        } else {
                            return "16-чная цифра";
                        }
                    } else {
                        col    = col - 1;
                        char c = chr.charValue();
                        if (row == 0) {
                            String bin   = Integer.toBinaryString((int)c);
                            var    start = 4 * col;
                            var    end   = 4 * col + 4;
                            bin = "0".repeat(16 - bin.length()) + bin;
                            System.out.println(
                                "Binary string is " + bin
                                + ", substr range = " + start + " - "
                                + end);

                            return bin.substring(start, end);
                        } else {
                            String hex = Integer.toHexString((int)c);
                            hex = "0".repeat(4 - hex.length()) + hex;
                            System.out.println("Hex string is " + hex);


                            return hex.charAt(col);
                        }
                    }
                }
            }
        };

        reprTable = new JTable(dataModel);
        updateHeader();
    }

    void updateHeader() {
        var headerVals = new ArrayList<String>() {
            {
                add("Тетрады");
                add("Тетрада 3 (15-12)");
                add("Тетрада 2 (11-8)");
                add("Тетрада 1 (7-4)");
                add("Тетрада 0 (3-0)");
            }
        };

        Misc.setHeader(reprTable, headerVals);
    }

    void updateUI() {
        String in = charIn_fld.getText();
        if (in.length() < 1) {
            Misc.showErrorMessage("Пустое поле вводаа!");
        } else {
            chr = in.charAt(0);
            reprTable.tableChanged(null);
            updateHeader();
        }
    }


    void initUI() {
        setupTable();
        add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
            {
                add(LYTBuilder.makeAnnotatedInput(
                    "Символ", charIn_fld, BoxLayout.Y_AXIS));
                add(LYTBuilder.makeAnnotatedInput(
                    "Десятичный код", intValue_lbl, BoxLayout.Y_AXIS));
            }
        }));

        add(LYTBuilder.makeScrollable(reprTable));

        charIn_fld.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                updateUI();
            }
        });

        Misc.addHelpMenuItem(
            this,
            "Ведите символ в поле ввода.\n"
                + "Рассчет будет выполнен автоматически при нажатии"
                + "Enter");
    }

    CharStructure() {
        initUI();
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setLayout(
            new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
    }
}
