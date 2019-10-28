import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;
import javax.swing.table.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;

class FloatStructure extends JFrame {
    JTextField       inNumber_fld  = new JTextField();
    JTextField       outNumber_fld = new JTextField();
    JTable           fltTable_tbl;
    Pair<Bits, Bits> bits;
    Integer          fltBits = 16;

    void setupTable() {
        // TODO highlight cell colors
        /* Some multiline comments
           */
        TableModel dataModel = /* ad annoying inline comments */ new FloatTableModel(fltBits, bits);

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

        head.debugPrint('i');
        decm.debugPrint('f');

        return new Pair<Bits, Bits>(head, decm);
    }

    void updateUI() {
        fltTable_tbl.tableChanged(null);
        updateHeader();
        outNumber_fld.setText(Double.toString(
            Long.valueOf(bits.k.toNum()).doubleValue() + bits.v.toDbl()));
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
                    updateUI();
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
