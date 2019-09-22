import javax.swing.table.*;

class NumberStructure extends JFrame {
    JLabel  baseLabel         = new JLabel("Base 10:");
    JLabel  base10_conversion = new JLabel();
    JButton doShiftLeft_btn   = new JButton("<<");
    JButton doShiftRight_btn  = new JButton(">>");

    JLabel numberBits_lbl = new JLabel("биты числа");
    JLabel numberSign_lbl = new JLabel("знак");
    JTable numberRepr_tbl;

    JTextField num_input_fld = new JTextField("1232");
    JButton    reverse_btn   = new JButton("Reverse");

    Bits    bits;
    Integer bitSize = 16;

    void setupTable() {
        // TODO highlight cell colors
        TableModel dataModel = new AbstractTableModel() {
            public int getRowCount() {
                return 1;
            }
            public int getColumnCount() {
                return bitSize;
            }
            public Object getValueAt(int row, int col) {
                if (row == 0) {
                    if (bits == null) {
                        return 0;
                    } else {
                        return (bits.get(bitSize - col - 1) ? "1" : "0");
                    }
                } else {
                    return 0;
                }
            }
        };

        numberRepr_tbl = new JTable(dataModel);
        updateHeader();
    }

    void updateHeader() {
        var headerVals = new ArrayList<String>();
        for (int i = 0; i < bitSize; ++i) {
            headerVals.add(Integer.toString(i));
        }
        Misc.setHeader(numberRepr_tbl, headerVals);
}

    void initUI() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

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
                add(Misc.setMin_WH(numberSign_lbl, 96, 12));
                add(Misc.setMin_WH(numberBits_lbl, 96, 12));
            }
        }));


        setupTable();
        add(                //
            Misc.setMax_WH( //
                LYTBuilder.makeScrollable(numberRepr_tbl),
                -1,
                120));

        add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
            {
                add(Misc.setMax_WH(num_input_fld, -1, 64));
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

    void setInt(Integer in) {
        num_input_fld.setText(Integer.toString(in));
    }

    void update() {
        var num = readInt();
        if (num.isPresent()) {
            if (bits == null) {
                bits = Bits.fromNum(num.get());
                bits.resize(bitSize);
            }

            System.out.println("Updating table and conversion");
            base10_conversion.setText(Integer.toString(num.get()));
            numberRepr_tbl.tableChanged(null);
            updateHeader();
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
            bits.resize(bitSize);
            bits.doShift(dir);
            setInt(bits.toNum());
            update();
        }
    }

    void initSignals() {
        doShiftLeft_btn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                doShift('l');
            }
        });

        doShiftRight_btn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                doShift('r');
            }
        });

        reverse_btn.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                update();
            }
        });
    }

    void run() {
        show();
    }
}
