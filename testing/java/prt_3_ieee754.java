import java.util.*;
import java.util.stream.*;
import javax.swing.*;

// FIXME handle input with 1 and 0 as float values. Right now this
// creates insuffuciently long string that causes index out of range.

class IEEEStructure extends JFrame {
    JTextField inNumber_fld  = new JTextField();
    JTextField outNumber_fld = new JTextField();
    JTable     reprTable;
    String     ieeeStr;

    void setupTable() {
        // TODO highlight cell colors
        TableModel dataModel = new AbstractTableModel() {
            public int getRowCount() {
                return 1;
            }
            public int getColumnCount() {
                return 3;
            }

            public Object getValueAt(int row, int col) {
                if (ieeeStr == null) {
                    return 0;
                } else {
                    System.out.println(
                        "Printing table. ieee string is: '" + ieeeStr
                        + "'");
                    System.out.println(
                        "String length is: " + ieeeStr.length());
                    if (col == 0) {
                        return ieeeStr.charAt(63);
                    } else if (col == 1) {
                        String strExp  = ieeeStr.substring(52, 62);
                        Long   longExp = Long.parseLong(strExp, 2);
                        System.out.println("Exponenta is: " + strExp);
                        return longExp.toString();
                    } else {
                        String strMant  = ieeeStr.substring(0, 51);
                        Long   longMant = Long.parseLong(strMant, 2);
                        System.out.println("Mantissa is: " + strMant);
                        return longMant.toString();
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
                add("Знак (63)");
                add("Характеристика (62-52)");
                add("Мантисса (51-0)");
            }
        };

        Misc.setHeader(reprTable, headerVals);
    }

    void updateUI() {
        reprTable.tableChanged(null);
        updateHeader();
        // FIXME remove scientific notation from string
        String doubleStr = Double.toString(
            Double.longBitsToDouble(Long.parseLong(ieeeStr, 2)));

        outNumber_fld.setText(doubleStr);
    }


    Optional<Double> getDouble() {
        try {
            Double res = Double.parseDouble(inNumber_fld.getText());
            return Optional.of(res);
        } catch (Exception ex) {
            Misc.showErrorMessage("Неправильно введено число");
            return Optional.empty();
        }
    }


    void initUI() {
        setupTable();

        add(LYTBuilder.makeAnnotatedInput(
            "Число", inNumber_fld, BoxLayout.X_AXIS));

        add(Misc.setMax_WH(LYTBuilder.makeScrollable(reprTable), -1, 120));
        add(LYTBuilder.makeAnnotatedInput(
            "Обратный перевод", outNumber_fld, BoxLayout.X_AXIS));

        inNumber_fld.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                var dbl = getDouble();
                if (dbl.isPresent()) {
                    var d = dbl.get();
                    System.out.println("You entered: " + d);
                    ieeeStr = (d >= 0 ? "0" : "")
                              + Long.toBinaryString(
                                  Double.doubleToLongBits(
                                      dbl.get().doubleValue()));

                    updateUI();
                }
            }
        });
    }

    IEEEStructure() {
        initUI();
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setLayout(
            new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
    }
}
