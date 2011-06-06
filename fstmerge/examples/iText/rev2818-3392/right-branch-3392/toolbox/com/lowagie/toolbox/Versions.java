





package com.lowagie.toolbox;

import java.awt.BorderLayout;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Properties;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

import com.lowagie.text.Document;
import javax.swing.ImageIcon;
import javax.swing.RowSorter;
import javax.swing.table.TableRowSorter;


public class Versions
    extends JFrame {

  
  private static final long serialVersionUID = 2925242862240301106L;

  
  JLabel library_versions = new JLabel();

  
  JTable plugin_versions = new JTable();

  
  JScrollPane scroll_versions = new JScrollPane();

  
  public Versions() {
    super("Plugins and their version");
    try {
      initialize();
    }
    catch (Exception ex) {
      ex.printStackTrace();
    }
  }

    
    private void initialize() throws Exception {
    this.getContentPane().setLayout(new BorderLayout());
    scroll_versions.setViewportView(plugin_versions);
    library_versions.setIcon(new ImageIcon(Versions.class.getResource(
        "1t3xt.gif")));
    this.getContentPane().add(library_versions, BorderLayout.NORTH);
    this.getContentPane().add(scroll_versions, BorderLayout.CENTER);
    Properties properties = System.getProperties();
    Runtime runtime = Runtime.getRuntime();
    StringBuffer sb = new StringBuffer();
    sb.append("<html>");
    sb.append("<p>iTexttoolbox version: " + Versions.class.getPackage().getImplementationVersion() + "</p>");
    sb.append("<p>iText version: " + Document.getVersion() + "</p>");
    sb.append("<p>java.version: " + properties.getProperty("java.version")
              + "</p>");
    sb.append("<p>java.vendor: " + properties.getProperty("java.vendor")
              + "</p>");
    sb.append("<p>java.home: " + properties.getProperty("java.home")
              + "</p>");
    sb.append("<p>java.freeMemory: " + runtime.freeMemory() + " bytes"
              + "</p>");
    sb.append("<p>java.totalMemory: " + runtime.totalMemory() + " bytes"
              + "</p>");
    sb.append("<p>user.home: " + properties.getProperty("user.home")
              + "</p>");
    sb.append("<p>os.name: " + properties.getProperty("os.name") + "</p>");
    sb.append("<p>os.arch: " + properties.getProperty("os.arch") + "</p>");
    sb.append("<p>os.version: " + properties.getProperty("os.version")
              + "</p>");
    sb.append("</html>");
    library_versions.setText(sb.toString());

    TableModel model = getVersionTableModel(AbstractTool.versionsarray);
    RowSorter<TableModel> sorter =
        new TableRowSorter<TableModel> (model);
    plugin_versions.setRowSorter(sorter);
    plugin_versions.setModel(model);

    pack();
  }

    
    public TableModel getVersionTableModel(final ArrayList<String> versionsarray) {
    return new AbstractTableModel() {

      private static final long serialVersionUID = 5105003782164682777L;

      public int getColumnCount() {
        return 4;
      }

      public int getRowCount() {
        return versionsarray.size();
      }

      public Object getValueAt(int rowIndex, int columnIndex) {
        String dummy;
        switch (columnIndex) {
          case 0:
            dummy = versionsarray.get(rowIndex).toString();
            return dummy.split(".java")[0];
          case 1:
            dummy = versionsarray.get(rowIndex).toString();
            return dummy.split(" ")[1];
          case 2:
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            dummy = versionsarray.get(rowIndex).toString();
            try {
              return df.parse(dummy.split(" ")[2] + " "
                              + dummy.split(" ")[3]);
            }
            catch (ParseException ex) {
              return null;
            }
            case 3:
              dummy = versionsarray.get(rowIndex).toString();
              return dummy.split(" ")[4];

        }
        return versionsarray;
      }

      public String getColumnName(int column) {
        switch (column) {
          case 0:
            return "Name";
          case 1:
            return "Version";
          case 2:
            return "Changed";
          case 3:
            return "ChangeBy";
          default:
            return "";
        }
      }

      public Class<? extends Object> getColumnClass(int column) {
        switch (column) {
          case 0:
            return String.class;
          case 1:
            return String.class;
          case 2:
            return java.util.Date.class;
          case 3:
            return String.class;
          default:
            return null;
        }
      }
    };

  }

    
    public static void main(String[] args) {
    Versions version = new Versions();
    version.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    version.setVisible(true);
  }
}
