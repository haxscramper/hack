import java.util.*;
import java.util.stream.*;
import javax.swing.*;

class Misc {
    public static void showErrorMessage(String msg) {
        JOptionPane.showMessageDialog(null, msg, "Err",
                                      JOptionPane.ERROR_MESSAGE |
                                      JOptionPane.OK_CANCEL_OPTION);
    }

    public static void main(String[] args) {
        System.out.println("misc.m4.java test");
    }
}
