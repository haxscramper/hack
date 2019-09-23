import java.util.*;
import java.util.stream.*;
import javax.swing.*;

class FloatStructure extends JFrame {
    JTextField       inNumber_fld  = new JTextField();
    JTextField       outNumber_fld = new JTextField();
    JTable           fltTable_tbl;
    Pair<Bits, Bits> bits;
    Integer          fltBits = 32;

    void setupTable() {
        // TODO highlight cell colors
        TableModel dataModel = new AbstractTableModel() {
            public int getRowCount() {
                return 4;
            }
            public int getColumnCount() {
                return 33;
            }
            public Object getValueAt(int row, int col) {
                // TODO change width of first column
                if (row == 0) {
                    if (col == 0) {
                        return "Байт";
                    } else {
                        col          = col - 1;
                        String str[] = {
                            "Байт №3", "Байт №2", "Байт №1", "Байт №0"};
                        int byte_idx = col / 8;
                        int bit_idx  = col % 8;
                        if (bit_idx >= str[byte_idx].length()) {
                            return "";
                        } else {
                            return str[byte_idx].charAt(bit_idx);
                        }
                    }
                } else if (row == 1) {
                    if (col == 0) {
                        return "№ Бита";
                    } else {
                        col = col - 1;
                        return 0;
                    }
                } else if (row == 2) {
                    if (col == 0) {
                        return "Вес бита";
                    } else {
                        col = col - 1;
                        col -= fltBits;
                        if (col < 0) {
                            return Math.pow(2, fltBits - col);
                        } else {
                            return (int)Math.pow(2, fltBits - col);
                        }
                    }
                } else if (row == 3) {
                    if (col == 0) {
                        return "Бит";
                    } else {
                        col = col - 1;
                        if (bits != null) {
                            if (col < fltBits / 2) {
                                return bits.k.get(col);
                            } else {
                                return bits.v.get(fltBits - col);
                            }
                        } else {
                            return 0;
                        }
                    }
                }
                return 0;
            }
        };

        fltTable_tbl = new JTable(dataModel);
        updateHeader();
    }


    void updateHeader() {
        var headerVals = new ArrayList<String>() {
            { add("Hello"); }
        };

        Misc.setHeader(fltTable_tbl, headerVals);
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

    Pair<Bits, Bits> splitDouble(Double in) {
        var head = Bits.fromNum(in.longValue());
        var decm = Bits.fromDbl(in - in.longValue(), fltBits / 2);

        head.debugPrint();
        decm.debugPrint();

        return new Pair<Bits, Bits>(head, decm);
    }

    void updateUI() {
        fltTable_tbl.tableChanged(null);
        updateHeader();
    }

    void initUI() {
        setupTable();
        add(LYTBuilder.makeAnnotatedInput(
            "Исходное число", inNumber_fld, BoxLayout.X_AXIS));
        add(LYTBuilder.makeScrollable(fltTable_tbl));
        add(LYTBuilder.makeAnnotatedInput(
            "Обратный побитовый перевод",
            outNumber_fld,
            BoxLayout.X_AXIS));


        inNumber_fld.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                var inDouble = getDouble();
                if (inDouble.isPresent()) {
                    bits = splitDouble(inDouble.get());
                }
            }
        });
    }

    FloatStructure() {
        initUI();
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setLayout(
            new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS));
    }
}
