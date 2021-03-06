import java.util.*;
import java.util.stream.*;
import javax.swing.*;

public class LYTBuilder {
  static JPanel makeDirectedPanel(ArrayList<JComponent> items,
                                  int direction) {
    var panel = new JPanel();
    var layout = new BoxLayout(panel, direction);
    panel.setLayout(layout);

    for (JComponent item : items) {
      panel.add(item);
    }

    return panel;
  }

  static JPanel makeVerticalPanel(ArrayList<JComponent> items) {
    return makeDirectedPanel(items, BoxLayout.Y_AXIS);
  }

  static JPanel makeHorizontalPanel(ArrayList<JComponent> items) {
    return makeDirectedPanel(items, BoxLayout.X_AXIS);
  }

  static JPanel makeAnnotatedInput(String label, JComponent textField,
                                   int direction, Dimension dim) {
    ppnulltest(textField);

    textField.setMaximumSize(dim);

    var labelWidget = new JLabel(label);
    // FIXME does not affect width at all.
    // labelWidget.setMinimumSize(new Dimension(0, label.length() * 1000));
    return LYTBuilder.makeDirectedPanel(new ArrayList<JComponent>() {
      {
        add(labelWidget);
        add(textField);
      }
    }, direction);
  }

  static JPanel makeAnnotatedInput(String label, JComponent textField,
                                   int direction, int maxSize) {
    Dimension dim;
    if (direction == BoxLayout.X_AXIS) {
      dim = new Dimension(Integer.MAX_VALUE, maxSize);
    } else {
      dim = new Dimension(maxSize, Integer.MAX_VALUE);
    }

    return makeAnnotatedInput(label, textField, direction, dim);
  }

  static JPanel makeAnnotatedInput(String label, JComponent textField,
                                   int direction) {
    return makeAnnotatedInput(label, textField, direction,
                              Integer.MAX_VALUE);
  }

  public static JScrollPane makeScrollable(JComponent component) {
    ppnulltest(component);
    var pane = new JScrollPane(component);
    return pane;
  }

  public static void setCellsAlignment(JTable table, int alignment) {
    DefaultTableCellRenderer rightRenderer =
        new DefaultTableCellRenderer();
    rightRenderer.setHorizontalAlignment(alignment);

    TableModel tableModel = table.getModel();

    for (int columnIndex = 0; columnIndex < tableModel.getColumnCount();
         columnIndex++) {
      table.getColumnModel()
          .getColumn(columnIndex)
          .setCellRenderer(rightRenderer);
    }
  }
}

class Test {
  public static void main(String[] args) {
    var frame = new JFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    frame.add(LYTBuilder.makeVerticalPanel(new ArrayList<JComponent>() {
      {
        add(new JButton("test 1"));
        add(new JButton("test 1"));
        add(new JButton("test 1"));
        add(new JButton("test 1"));
        add(new JButton("test 1"));
      }
    }));

    frame.add(LYTBuilder.makeHorizontalPanel(new ArrayList<JComponent>() {
      {
        add(new JButton("test 1"));
        add(new JButton("test 1"));
        add(new JButton("test 1"));
        add(new JButton("test 1"));
        add(new JButton("test 1"));
      }
    }));

    frame.setVisible(true);
  }
}
