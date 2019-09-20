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

  public static Optional<Float> getFloat(JTextField in, String errorMessage) {
    try {
      return Optional.of(Float.parseFloat(in.getText()));
    } catch (Exception e) {
      if (errorMessage.length() > 0) {
        showErrorMessage(errorMessage);
      }
      return Optional.empty();
    }
  }
}
