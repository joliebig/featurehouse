
package com.lowagie.toolbox.swing;

import java.awt.BorderLayout;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.swing.*;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.*;

import com.lowagie.text.pdf.PdfReader;


public class FileList
    extends JInternalFrame implements DropTargetListener {

  private static final long serialVersionUID = -7238230038043975672L;

  Vector<RowContainer> filevector = new Vector<RowContainer>();

  public FileList() {
    super("FileList", true, true, true);
    try {
      jbInit();
    }
    catch (Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    this.getContentPane().setLayout(borderLayout1);
    jTable1.addKeyListener(new FileList_jTable1_keyAdapter(this));
    jLabel1.setText("pages");
    jLabel2.setText("-");
    model.addTableModelListener(new FileList_ftm_tableModelAdapter(this));
    this.getContentPane().add(jPanel1, java.awt.BorderLayout.CENTER);
    jPanel1.setLayout(borderLayout2);
    jPanel2.setLayout(borderLayout3);
    jPanel1.add(jPanel2, java.awt.BorderLayout.CENTER);
    jPanel2.add(jScrollPane1, java.awt.BorderLayout.CENTER);
    jPanel1.add(jPanel3, java.awt.BorderLayout.NORTH);
    jPanel3.add(jLabel2);
    jPanel3.add(jLabel1);
    jScrollPane1.setViewportView(jTable1);
    jTable1.setRowSorter(sorter);

    this.pack();
  }

  JPanel jPanel1 = new JPanel();
  BorderLayout borderLayout1 = new BorderLayout();
  JPanel jPanel2 = new JPanel();
  BorderLayout borderLayout2 = new BorderLayout();
  JScrollPane jScrollPane1 = new JScrollPane();
  FileTableModel model = new FileTableModel();
  JTable jTable1 = new JTable(model);
  RowSorter<TableModel> sorter=new TableRowSorter<TableModel> (model);
  BorderLayout borderLayout3 = new BorderLayout();
  DropTarget dt = new DropTarget(this, DnDConstants.ACTION_COPY_OR_MOVE, this, true, null);
  JPanel jPanel3 = new JPanel();
  JLabel jLabel1 = new JLabel();
  JLabel jLabel2 = new JLabel();

  public void dragEnter(DropTargetDragEvent dtde) {
  }

  public void dragOver(DropTargetDragEvent dtde) {
  }

  public void dropActionChanged(DropTargetDragEvent dtde) {
    System.out.println("actionchanged");
  }

  public void drop(DropTargetDropEvent dtde) {
    if ( (dtde.getDropAction() & DnDConstants.ACTION_COPY_OR_MOVE) == 0) {
      dtde.rejectDrop();
      return;
    }
    dtde.acceptDrop(DnDConstants.ACTION_COPY);
    Vector<RowContainer> oldvec=this.filevector;

    Transferable transferable = dtde.getTransferable();
    try {
      java.util.List<File> filelist = (java.util.List<File>) transferable.getTransferData(
          DataFlavor.javaFileListFlavor);
      for (File f: filelist) {
        filevector.add(new RowContainer(f));

        model.fireTableDataChanged();
        System.out.println(f.toString());
      }
    }
    catch (IOException ex) {
        ex.printStackTrace();
    }
    catch (UnsupportedFlavorException ex) {
        ex.printStackTrace();
    }
    dtde.dropComplete(true);
    File[] filar=new File[filevector.size()];
    for(int i=0;i<filevector.size();i++){
        filar[i]=filevector.get(i).getFile();
    }
    super.firePropertyChange("filevector",
                null, filar);
  }

  public void dragExit(DropTargetEvent dte) {
  }

  class FileTableModel
      extends AbstractTableModel {
    private static final long serialVersionUID = -8173736343997473512L;
    private String[] columnNames = {
        "Filename", "Pages", "Directory"};

    public int getColumnCount() {
      return columnNames.length;
    }

    public int getRowCount() {
      return filevector.size();
    }

    public String getColumnName(int col) {
      return columnNames[col];
    }

    public Object getValueAt(int row, int col) {
      switch (col) {
        case 0:
          return filevector.get(row).getFile().getName();
        case 1:
          return Integer.valueOf(filevector.get(row).getPages());
        case 2:
          return filevector.get(row).getFile().getParent();
      }
      return null;
    }

    public Class<?> getColumnClass(int col) {
      switch (col) {
        case 0:
          return String.class;
        case 1:
          return Integer.class;
        case 2:
          return String.class;
      }
      return null;
    }

    public void removeRow(int row) {
      filevector.remove(row);
    }
  }

  public void jTable1_keyPressed(KeyEvent e) {
    if (e.getKeyCode() == 127) {
      int[] selected = jTable1.getSelectedRows();
      for (int i = selected.length - 1; i >= 0; i--) {
        model.removeRow(selected[i]);
        model.fireTableDataChanged();
      }
    }
  }

  public void ftm_tableChanged(TableModelEvent e) {
    int sum = 0;
    for (RowContainer c: filevector) {
      sum += c.getPages();
    }
    this.jLabel2.setText(Integer.toString(sum));
  }

    public Vector<RowContainer> getFilevector() {
        return filevector;
    }
    public String getStringreprasentation(){
        StringBuffer sb=new StringBuffer();
       Vector<RowContainer> vec=getFilevector();
       for(RowContainer c: vec){
           sb.append(c.getFile().getAbsolutePath()).append('\n');
       }
       return sb.toString();
    }
}

class FileList_ftm_tableModelAdapter
    implements TableModelListener {
  private FileList adaptee;
  FileList_ftm_tableModelAdapter(FileList adaptee) {
    this.adaptee = adaptee;
  }

  public void tableChanged(TableModelEvent e) {
    adaptee.ftm_tableChanged(e);
  }
}

class FileList_jTable1_keyAdapter
    extends KeyAdapter {
  private FileList adaptee;
  FileList_jTable1_keyAdapter(FileList adaptee) {
    this.adaptee = adaptee;
  }

  public void keyPressed(KeyEvent e) {
    adaptee.jTable1_keyPressed(e);
  }
}

class RowContainer {
  private File file;
  private int pages;
  public File getFile() {
    return file;
  }

  public int getPages() {
    return pages;
  }

  public void setFile(File file) {
    this.file = file;
  }

  public void setPages(int pages) {
    this.pages = pages;
  }

  
  RowContainer(File file) {
    this.file = file;
    PdfReader reader = null;
    try {
      reader = new PdfReader(file.
                             getAbsolutePath());
    }
    catch (IOException ex) {
    }
    this.pages = reader.getNumberOfPages();
  }
}
