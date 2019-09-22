import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.stream.*;
import javax.swing.*;

include(misc.m4.java);
include(bits.m4.java);
include(ui-misc.m4.java);

include(prt_3_text_field.java)
include(prt_3_number_structure.java)

class MainFrame extends JFrame {
  static void runA() {
    var frame = new NumberStructure();
    frame.run();
  }

  static void runB() {}
  static void runC() {}
  static void runD() {}

  public static void main(String[] args) {
    if (args.length > 0) {
      if (args[0].equals("a")) {
        runA();
      } else if (args[0].equals("b")) {
        runB();
      } else if (args[0].equals("c")) {
        runC();
      } else if (args[0].equals("d")) {
        runD();
      }
    }
  }
}
