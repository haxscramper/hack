import java.util.*;
import java.util.stream.*;
import javax.swing.*;

class Misc {
    public static void showErrorMessage(String msg) {
        JOptionPane.showMessageDialog(
            null,
            msg,
            "Err",
            JOptionPane.ERROR_MESSAGE | JOptionPane.OK_CANCEL_OPTION);
    }

    public static void main(String[] args) {
        System.out.println("misc.m4.java test");
    }

    public static Optional<Float> getFloat(
        JTextField in,
        String     errorMessage) {
        try {
            return Optional.of(Float.parseFloat(in.getText()));
        } catch (Exception e) {
            if (errorMessage.length() > 0) {
                showErrorMessage(errorMessage);
            }
            return Optional.empty();
        }
    }


    public static void addHelpMenuItem(JFrame frame, String message) {

        JMenuBar menuBar = new JMenuBar();
        var      menu    = new JMenu("...");

        var helpItem = new JMenuItem("Help");
        menu.add(helpItem);
        helpItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(
                    null,
                    message,
                    "Help",
                    JOptionPane.QUESTION_MESSAGE
                        | JOptionPane.OK_CANCEL_OPTION);
            }
        });

        menuBar.add(menu);
        frame.setJMenuBar(menuBar);
    }

    public static JComponent setMin_WH(
        JComponent in,
        Integer    width,
        Integer    height) {

        in.setMinimumSize(new Dimension(width, height));
        return in;
    }

    public static JComponent setMax_WH(

        JComponent in,
        Integer    width,
        Integer    height) {
        in.setMaximumSize(new Dimension(
            (width < 0 ? Integer.MAX_VALUE : width),
            (height < 0 ? Integer.MAX_VALUE : height)));

        return in;
    }

    public static JTable setHeader(
        JTable            table,
        ArrayList<String> values) {
        var header   = table.getTableHeader();
        var colModel = header.getColumnModel();
        for (int i = 0; i < values.size(); ++i) {
            var column = colModel.getColumn(i);
            column.setHeaderValue(values.get(i));
        }
        return table;
    }
}
