package net.sf.jabref;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.layout.FormLayout;


class XmpPrefsTab extends JPanel implements PrefsTab {

	boolean tableChanged = false;

	int rowCount;

	JTable table;

	JCheckBox privacyFilterCheckBox = new JCheckBox(Globals
		.lang("Do not write the following fields to XMP Metadata:"));

	Vector<Object> tableRows = new Vector<Object>(10);

	
	public XmpPrefsTab() {
		setLayout(new BorderLayout());

		TableModel tm = new AbstractTableModel() {
			public int getRowCount() {
				return rowCount;
			}

			public int getColumnCount() {
				return 1;
			}

			public Object getValueAt(int row, int column) {
				if (row >= tableRows.size())
					return "";
				Object rowContent = tableRows.elementAt(row);
				if (rowContent == null)
					return "";
				return rowContent;
			}

			public String getColumnName(int col) {
				return Globals.lang("Field to filter");
			}

			public Class<?> getColumnClass(int column) {
				return String.class;
			}

			public boolean isCellEditable(int row, int col) {
				return true;
			}

			public void setValueAt(Object value, int row, int col) {
				tableChanged = true;

				if (tableRows.size() <= row) {
					tableRows.setSize(row + 1);
				}

				tableRows.setElementAt(value, row);
			}

		};

		table = new JTable(tm);
		TableColumnModel cm = table.getColumnModel();
		cm.getColumn(0).setPreferredWidth(140);

		FormLayout layout = new FormLayout("1dlu, 8dlu, left:pref, 4dlu, fill:pref", "");
		DefaultFormBuilder builder = new DefaultFormBuilder(layout);
		JPanel pan = new JPanel();

		JPanel tablePanel = new JPanel();
		tablePanel.setLayout(new BorderLayout());
		JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		table.setPreferredScrollableViewportSize(new Dimension(250, 200));
		scrollPane.setMinimumSize(new Dimension(250, 300));
		tablePanel.add(scrollPane, BorderLayout.CENTER);

		JToolBar toolbar = new JToolBar(SwingConstants.VERTICAL);
		toolbar.setFloatable(false);
		toolbar.setBorder(null);
		toolbar.add(new AddRowAction());
		toolbar.add(new DeleteRowAction());

		tablePanel.add(toolbar, BorderLayout.EAST);

		
		builder.appendSeparator(Globals.lang("XMP Export Privacy Settings"));
		builder.nextLine();

		builder.append(pan);
		builder.append(privacyFilterCheckBox);
		builder.nextLine();

		builder.append(pan);
		builder.append(tablePanel);
		builder.nextLine();

		pan = builder.getPanel();
		pan.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		add(pan, BorderLayout.CENTER);
	}

	class DeleteRowAction extends AbstractAction {
		public DeleteRowAction() {
			super("Delete row", GUIGlobals.getImage("remove"));
			putValue(SHORT_DESCRIPTION, Globals.lang("Delete rows"));
		}

		public void actionPerformed(ActionEvent e) {
			int[] rows = table.getSelectedRows();
			if (rows.length == 0)
				return;

			for (int i = rows.length - 1; i >= 0; i--) {
				if (rows[i] < tableRows.size()) {
					tableRows.remove(rows[i]);
				}
			}
			rowCount -= rows.length;
			if (rows.length > 1)
				table.clearSelection();
			table.revalidate();
			table.repaint();
			tableChanged = true;
		}
	}

	class AddRowAction extends AbstractAction {
		public AddRowAction() {
			super("Add row", GUIGlobals.getImage("add"));
			putValue(SHORT_DESCRIPTION, Globals.lang("Insert rows"));
		}

		public void actionPerformed(ActionEvent e) {
			int[] rows = table.getSelectedRows();
			if (rows.length == 0) {
				
				rowCount++;
				table.revalidate();
				table.repaint();
				return;
			}
			for (int i = 0; i < rows.length; i++) {
				if (rows[i] + i < tableRows.size())
					tableRows.add(rows[i] + i, "");
			}
			rowCount += rows.length;
			if (rows.length > 1)
				table.clearSelection();
			table.revalidate();
			table.repaint();
			tableChanged = true;
		}
	}

	
	public void setValues() {
		tableRows.clear();
		String[] names = JabRefPreferences.getInstance().getStringArray("xmpPrivacyFilters");
		for (int i = 0; i < names.length; i++) {
			tableRows.add(names[i]);
		}
		rowCount = tableRows.size() + 5;

		privacyFilterCheckBox.setSelected(JabRefPreferences.getInstance().getBoolean(
			"useXmpPrivacyFilter"));
	}

	
	public void storeSettings() {

		if (table.isEditing()) {
			int col = table.getEditingColumn();
			int row = table.getEditingRow();
			table.getCellEditor(row, col).stopCellEditing();
		}

		
		
        
        if (tableChanged ||
                (privacyFilterCheckBox.isSelected() && !Globals.prefs.hasKey("xmpPrivacyFilters"))) {

			
			for (int i = tableRows.size() - 1; i >= 0; i--) {
				if (tableRows.elementAt(i).equals(""))
					tableRows.removeElementAt(i);
			}

			
			JabRefPreferences.getInstance().putStringArray("xmpPrivacyFilters",
				tableRows.toArray(new String[tableRows.size()]));
		}

		JabRefPreferences.getInstance().putBoolean("useXmpPrivacyFilter", privacyFilterCheckBox.isSelected());
	}

	public boolean readyToClose() {
		return true;
	}

	public String getTabName() {
		return Globals.lang("XMP metadata");
	}
}
