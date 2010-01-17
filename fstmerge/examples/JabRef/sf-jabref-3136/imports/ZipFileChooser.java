
package net.sf.jabref.imports;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumnModel;

import net.sf.jabref.FocusRequester;
import net.sf.jabref.Globals;
import net.sf.jabref.JabRefPreferences;
import net.sf.jabref.Util;



class ZipFileChooser extends JDialog {

  
  class ZipFileChooserTableModel extends AbstractTableModel {
    
    private String[] columnNames = new String[] {
      Globals.lang("Name"),
      Globals.lang("Last modified"),
      Globals.lang("Size")
    };
    private ZipEntry[] rows = null;
    private ZipFile zipFile = null;
    
    ZipFileChooserTableModel(ZipFile zipFile, ZipEntry[] rows) {
      super();
      this.rows = rows;
      this.zipFile = zipFile;        
    }
    
    
    public int getColumnCount() {
      return columnNames.length;
    }

    
    public int getRowCount() {
      return this.rows.length;
    }

    
    public String getColumnName(int col) {
      return columnNames[col];
    }

    
    public ZipEntry getZipEntry(int rowIndex) {
      return this.rows[rowIndex];
    }
    
    
    public ZipFile getZipFile() {
      return this.zipFile;
    }
    
    
    public Object getValueAt(int rowIndex, int columnIndex) {
      Object value = null;
      ZipEntry entry = getZipEntry(rowIndex);
      if (columnIndex == 0) {
        value = entry.getName();
      } else if (columnIndex == 1) {
        value = SimpleDateFormat.getDateTimeInstance().format(new Date(entry.getTime()));
      } else if (columnIndex == 2) {
        value = new Long(entry.getSize());
      }
      return value;
    }
  }

  private JButton okButton = new JButton(Globals.lang("Ok"));
  private JButton cancelButton = new JButton(Globals.lang("Cancel"));

  
  private JTable table;
  
  private JabRefPreferences prefs = Globals.prefs;
  
  private ZipFileChooser zipFileChooser;
  
  private ImportCustomizationDialog importCustomizationDialog;
  
  
  public Dimension getSize() {
    return new Dimension(400, 300);
  }
  
  
  private ZipEntry[] getSelectableZipEntries(ZipFile zipFile) {
    List<ZipEntry> entries = new ArrayList<ZipEntry>();
    Enumeration<? extends ZipEntry> e = zipFile.entries();
    while (e.hasMoreElements()) {
      ZipEntry entry = e.nextElement();
      if (!entry.isDirectory() && entry.getName().endsWith(".class")) {
        entries.add(entry); 
      }
    }
    return entries.toArray(new ZipEntry[]{});
  }
  
  
  public ZipFileChooser(ImportCustomizationDialog owner, ZipFile zipFile) throws HeadlessException {
    super(owner, Globals.lang("Select file from ZIP-archive"), false);
    
    this.importCustomizationDialog = owner;
    this.zipFileChooser = this;
    
    
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        dispose();
      }
    });

    
    okButton.addActionListener(new ActionListener() {
     public void actionPerformed(ActionEvent e) {
       int row = table.getSelectedRow();
       if (row != -1) {
         ZipFileChooserTableModel model = (ZipFileChooserTableModel)table.getModel();
         ZipEntry tempZipEntry = model.getZipEntry(row);
         CustomImportList.Importer importer = prefs.customImports.new Importer();
         importer.setBasePath(model.getZipFile().getName());
         String className = tempZipEntry.getName().substring(0, tempZipEntry.getName().lastIndexOf('.'));
         importer.setClassName(className);
         try {
           ImportFormat importFormat = importer.getInstance();
           importer.setName(importFormat.getFormatName());
           importer.setCliId(importFormat.getCLIId());
           importCustomizationDialog.addOrReplaceImporter(importer);
           dispose();
         } catch (Exception exc) {           
           exc.printStackTrace();
           JOptionPane.showMessageDialog(zipFileChooser, Globals.lang("Could not instantiate %0 %1", importer.getName() + ":\n", exc.getMessage()));
         }
       } else {
         JOptionPane.showMessageDialog(zipFileChooser, Globals.lang("Please select an importer."));
       }
     }
    });
    

    ZipFileChooserTableModel tableModel = new ZipFileChooserTableModel( zipFile, getSelectableZipEntries(zipFile) );
    table = new JTable(tableModel);
    TableColumnModel cm = table.getColumnModel();
    cm.getColumn(0).setPreferredWidth(200);
    cm.getColumn(1).setPreferredWidth(150);
    cm.getColumn(2).setPreferredWidth(100);
    JScrollPane sp = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    table.setPreferredScrollableViewportSize(new Dimension(500, 150));
    if (table.getRowCount() > 0) {
      table.setRowSelectionInterval(0, 0);
    }

    
    JPanel mainPanel = new JPanel();
    
    
    
    
    mainPanel.setLayout(new BorderLayout());
    mainPanel.add(sp, BorderLayout.CENTER);

    JPanel optionsPanel = new JPanel();
    optionsPanel.add(okButton);
    optionsPanel.add(cancelButton);
    optionsPanel.add(Box.createHorizontalStrut(5));

    getContentPane().add(mainPanel, BorderLayout.CENTER);
    getContentPane().add(optionsPanel, BorderLayout.SOUTH);
    this.setSize(getSize());
    pack();
    Util.placeDialog(this, owner);
    new FocusRequester(table);
  }
}
