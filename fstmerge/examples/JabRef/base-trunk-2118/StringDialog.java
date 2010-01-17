

package net.sf.jabref;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.table.*;
import java.util.*;
import net.sf.jabref.undo.*;
import net.sf.jabref.export.LatexFieldFormatter;
import javax.swing.undo.CompoundEdit;

public class StringDialog extends JDialog {

    
    BibtexDatabase base;
    JabRefFrame frame;
    BasePanel panel;
    JabRefPreferences prefs;
    TreeSet stringsSet; 
    Object[] strings;

    
    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints con = new GridBagConstraints();
    JLabel lab;
    Container conPane = getContentPane();
    JToolBar tlb = new JToolBar();
    JPanel pan = new JPanel();
    StringTable table;
    HelpAction helpAction;

    public StringDialog(JabRefFrame frame, BasePanel panel,
			BibtexDatabase base, JabRefPreferences prefs) {
	super(frame);
	this.frame = frame;
	this.panel = panel;
	this.base = base;
	this.prefs = prefs;

	sortStrings();

	helpAction = new HelpAction
	    (frame.helpDiag, GUIGlobals.stringEditorHelp, "Help");


	addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent e) {
		    closeAction.actionPerformed(null);
		}
	    });

	
	
	setFocusTraversalPolicy(new LayoutFocusTraversalPolicy() {
		protected boolean accept(Component c) {
		    return (super.accept(c) && (c instanceof StringTable));
		}
	    });

	setLocation(prefs.getInt("stringsPosX"), prefs.getInt("stringsPosY"));
	setSize(prefs.getInt("stringsSizeX"), prefs.getInt("stringsSizeY"));

	pan.setLayout(gbl);
	con.fill = GridBagConstraints.BOTH;
	con.weighty = 1;
	con.weightx = 1;

	StringTableModel stm = new StringTableModel(this, base);
	table = new StringTable(stm);
	if (base.getStringCount() > 0)
	    table.setRowSelectionInterval(0,0);

	gbl.setConstraints(table.getPane(), con);
	pan.add(table.getPane());

	InputMap im = tlb.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
	ActionMap am = tlb.getActionMap();
	im.put(prefs.getKey("String dialog, add string"), "add");
	am.put("add", newStringAction);
	im.put(prefs.getKey("String dialog, remove string"), "remove");
	am.put("remove", removeStringAction);
	
	
	
	
	im.put(prefs.getKey("Close dialog"), "close");
	am.put("close", closeAction);
	im.put(prefs.getKey("Help"), "help");
	am.put("help", helpAction);
	im.put(prefs.getKey("Undo"), "undo");
	am.put("undo", undoAction);
	im.put(prefs.getKey("Redo"), "redo");
	am.put("redo", redoAction);

	
	
	tlb.add(newStringAction);
	tlb.add(removeStringAction);
	tlb.addSeparator();
	
	
	tlb.addSeparator();
	tlb.add(helpAction);
	conPane.add(tlb, BorderLayout.NORTH);
	conPane.add(pan, BorderLayout.CENTER);

	if (panel.getFile() != null)
	    setTitle(Globals.lang(GUIGlobals.stringsTitle)+": "+panel.getFile().getName());
	else
	    setTitle(Globals.lang(GUIGlobals.stringsTitle)+": "+Globals.lang(GUIGlobals.untitledTitle));

    }

    class StringTable extends JTable {
	JScrollPane sp = new JScrollPane((JTable)this);
	public StringTable(StringTableModel stm) {
	    super(stm);
	    setShowVerticalLines(true);
	    setShowHorizontalLines(true);
	    setColumnSelectionAllowed(true);
	    DefaultCellEditor dce = new DefaultCellEditor(new JTextField());
	    dce.setClickCountToStart(2);
	    setDefaultEditor(String.class, dce);
	    TableColumnModel cm = getColumnModel();
	    cm.getColumn(0).setPreferredWidth(800);
	    cm.getColumn(1).setPreferredWidth(2000);
	    sp.getViewport().setBackground(Globals.prefs.getColor("tableBackground"));
	    
	    getInputMap().put(frame.prefs.getKey("Close dialog"), "close");
	    getActionMap().put("close", closeAction);
	    getInputMap().put(frame.prefs.getKey("Help"), "help");
	    getActionMap().put("help", helpAction);

	}

	public JComponent getPane() {
	    return sp;
	}

    }

    private void sortStrings() {
	
	stringsSet = new TreeSet(new BibtexStringComparator(false));
	Iterator i = base.getStringKeySet().iterator();
	for (;i.hasNext();) {
	    stringsSet.add(base.getString(i.next()));
	}
	strings = stringsSet.toArray();
    }

    public void refreshTable() {
	sortStrings();
	table.revalidate();
	table.clearSelection();
	table.repaint();
    }

    class StringTableModel extends AbstractTableModel {

	BibtexDatabase base;
	StringDialog parent;

	public StringTableModel(StringDialog parent, BibtexDatabase base) {
	    this.parent = parent;
	    this.base = base;
	}

	public Object getValueAt(int row, int col) {
	    return ((col == 0) ?
		    ((BibtexString)strings[row]).getName() :
		    ((BibtexString)strings[row]).getContent());
	}

	public void setValueAt(Object value, int row, int col) {
	    
	    
	                
	                
	    if (col == 0) {
		
		if (!((String)value).equals(((BibtexString)strings[row]).getName())) {
		    if (base.hasStringLabel((String)value))
			JOptionPane.showMessageDialog(parent,
						      Globals.lang("A string with that label "
								   +"already exists"),
						      Globals.lang("Label"),
						      JOptionPane.ERROR_MESSAGE);
                      else if (((String)value).indexOf(" ") >= 0) {
                        JOptionPane.showMessageDialog
                            (parent,
                             Globals.lang("The label of the string can not contain spaces."),
                             Globals.lang("Label"),
                             JOptionPane.ERROR_MESSAGE);
                      }
                      else if (((String)value).indexOf("#") >= 0) {
                        JOptionPane.showMessageDialog
                            (parent,
                            Globals.lang("The label of the string can not contain the '#' character."),
                            Globals.lang("Label"),
                            JOptionPane.ERROR_MESSAGE);
                      }   
                      else if ((value != null) && isNumber((String)value)) {
                          JOptionPane.showMessageDialog
                              (parent,
                               Globals.lang("The label of the string can not be a number."),
                               Globals.lang("Label"),
                               JOptionPane.ERROR_MESSAGE);
		    }
		    else {
			
			BibtexString subject = (BibtexString)strings[row];
			panel.undoManager.addEdit
			    (new UndoableStringChange
			     (panel, subject, true,
			      subject.getName(), (String)value));
			subject.setName((String)value);
			panel.markBaseChanged();
			refreshTable();
		    }
		}
	    } else {
		
		BibtexString subject = (BibtexString)strings[row];

		if (!((String)value).equals(subject.getContent())) {
                    try {
                        (new LatexFieldFormatter()).format((String)value, "__dummy");
                    } catch (IllegalArgumentException ex) {
                        return;
                    }
		    
		    panel.undoManager.addEdit
			(new UndoableStringChange
			 (panel, subject, false,
			  subject.getContent(), (String)value));

		    subject.setContent((String)value);
		    panel.markBaseChanged();
		}
	    }
	}

	public int getColumnCount() {
	    return 2;
	}

	public int getRowCount() {
	    return strings.length; 
	}

	public String getColumnName(int col) {
	    return ((col == 0) ?
		    Globals.lang("Name") : Globals.lang("Content"));
	}

	public boolean isCellEditable(int row, int col) {
	    return true;
	}
    }

    protected boolean isNumber(String name) {
	
	
	try {
	    Integer.parseInt(name);
	    return true;
	} catch (NumberFormatException ex) {
	    return false;
	}

    }

    protected void assureNotEditing() {
	if (table.isEditing()) {
	    int col = table.getEditingColumn(),
		row = table.getEditingRow();
	    table.getCellEditor(row, col).stopCellEditing();
	}
    }

    
    CloseAction closeAction = new CloseAction(this);
    class CloseAction extends AbstractAction {
	StringDialog parent;
	public CloseAction(StringDialog parent) {
	    super("Close window");
	    
	    putValue(SHORT_DESCRIPTION, Globals.lang("Close dialog"));
	    this.parent = parent;
	}
	public void actionPerformed(ActionEvent e) {
	    panel.stringsClosing();
	    dispose();
	    Point p = getLocation();
	    Dimension d = getSize();
	    prefs.putInt("stringsPosX", p.x);
	    prefs.putInt("stringsPosY", p.y);
	    prefs.putInt("stringsSizeX", d.width);
	    prefs.putInt("stringsSizeY", d.height);
	}
    }

    NewStringAction newStringAction = new NewStringAction(this);
    class NewStringAction extends AbstractAction {
	StringDialog parent;
	public NewStringAction(StringDialog parent) {
	    super("New string",
		  GUIGlobals.getImage("add"));
	    putValue(SHORT_DESCRIPTION, Globals.lang("New string"));
	    this.parent = parent;
	}
	public void actionPerformed(ActionEvent e) {
	    String name =
		JOptionPane.showInputDialog(parent,
					    Globals.lang("Please enter the string's label"));
	    if (name == null)
		return;
	    if (isNumber(name)) {
		JOptionPane.showMessageDialog
		    (parent,
		     Globals.lang("The label of the string can not be a number."),
		     Globals.lang("Label"),
		     JOptionPane.ERROR_MESSAGE);
		return;
	    }
            if (name.indexOf("#") >= 0) {
             JOptionPane.showMessageDialog
                 (parent,
                  Globals.lang("The label of the string can not contain the '#' character."),
                  Globals.lang("Label"),
                  JOptionPane.ERROR_MESSAGE);
             return;
           }           
           if (name.indexOf(" ") >= 0) {
             JOptionPane.showMessageDialog
                 (parent,
                  Globals.lang("The label of the string can not contain spaces."),
                  Globals.lang("Label"),
                  JOptionPane.ERROR_MESSAGE);
             return;
           }
	    try {
		String newId = Util.createNeutralId();
		BibtexString bs = new BibtexString(newId, name, "");

		
		panel.undoManager.addEdit
		    (new UndoableInsertString
		     (panel, panel.database, bs));

		base.addString(bs);
		refreshTable();
		
		panel.markBaseChanged();
	    } catch (KeyCollisionException ex) {
		JOptionPane.showMessageDialog(parent,
					      Globals.lang("A string with that label "
							   +"already exists"),
					      Globals.lang("Label"),
					      JOptionPane.ERROR_MESSAGE);
	    }
	}
    }

    StoreContentAction storeContentAction = new StoreContentAction(this);
    class StoreContentAction extends AbstractAction {
	StringDialog parent;
	public StoreContentAction(StringDialog parent) {
	    super("Store string",
		  GUIGlobals.getImage("add"));
	    putValue(SHORT_DESCRIPTION, Globals.lang("Store string"));
	    this.parent = parent;
	}
	public void actionPerformed(ActionEvent e) {
	}
    }

    RemoveStringAction removeStringAction = new RemoveStringAction(this);
    class RemoveStringAction extends AbstractAction {
	StringDialog parent;
	public RemoveStringAction(StringDialog parent) {
	    super("Remove selected strings",
		  GUIGlobals.getImage("remove"));
	    putValue(SHORT_DESCRIPTION, Globals.lang("Remove selected strings"));
	    this.parent = parent;
	}
	public void actionPerformed(ActionEvent e) {
	    int[] sel = table.getSelectedRows();
	    if (sel.length > 0) {

		
		
		assureNotEditing();

		String msg = Globals.lang("Really delete the selected")+" "+
		    ((sel.length>1) ? sel.length+" "+Globals.lang("entries")
		     : Globals.lang("entry"))+"?";
		int answer = JOptionPane.showConfirmDialog(parent, msg, Globals.lang("Delete strings"),
							   JOptionPane.YES_NO_OPTION,
							   JOptionPane.QUESTION_MESSAGE);
		if (answer == JOptionPane.YES_OPTION) {
		    CompoundEdit ce = new CompoundEdit();
		    for (int i=sel.length-1; i>=0; i--) {
			

			BibtexString subject = (BibtexString)strings[sel[i]];

			
			ce.addEdit(new UndoableRemoveString
				   (panel, base,
				    subject));

			base.removeString(subject.getId());
		    }
		    ce.end();
		    panel.undoManager.addEdit(ce);

		    
		    refreshTable();
		    if (base.getStringCount() > 0)
			table.setRowSelectionInterval(0,0);
		    
		    
		}
	    }
	}
    }

    

    UndoAction undoAction = new UndoAction();
    class UndoAction extends AbstractAction {
	public UndoAction() {
	    super("Undo", GUIGlobals.getImage("undo"));
	    putValue(SHORT_DESCRIPTION, Globals.lang("Undo"));
	}
	public void actionPerformed(ActionEvent e) {
	    try {
		panel.runCommand("undo");
	    } catch (Throwable ex) {}
	}
    }

    RedoAction redoAction = new RedoAction();
    class RedoAction extends AbstractAction {
	public RedoAction() {
	    super("Undo", GUIGlobals.getImage("redo"));
	    putValue(SHORT_DESCRIPTION, Globals.lang("Redo"));
	}
	public void actionPerformed(ActionEvent e) {
	    try {
		panel.runCommand("redo");
	    } catch (Throwable ex) {}
	}
    }


}
