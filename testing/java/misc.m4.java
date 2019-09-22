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

    public static void addHelpMenuItem(
        JFrame frame,
        String message) {

        JMenuBar menuBar = new JMenuBar();
        var menu = new JMenu("...");

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
}
