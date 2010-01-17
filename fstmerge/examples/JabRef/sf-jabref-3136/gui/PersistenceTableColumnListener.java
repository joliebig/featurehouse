package net.sf.jabref.gui;

import java.util.Vector;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;

import net.sf.jabref.Globals;


public class PersistenceTableColumnListener implements TableColumnModelListener {

	public static final String ACTIVATE_PREF_KEY = 
	    "ActivatePersistenceTableColumnListener";

	public static final boolean DEFAULT_ENABLED = true;

	private static final String simpleClassName = 
	    PersistenceTableColumnListener.class.getSimpleName();

	
	
	private final MainTable mainTable;

	
	public PersistenceTableColumnListener(final MainTable mainTable) {
		this.mainTable = mainTable;
	}

	
	private void updateColumnPrefs() {
        final int columnCount = mainTable.getColumnCount();
		Vector<String> storedColumns = new Vector<String>(columnCount - 1);
		Vector<String> columnsWidths = new Vector<String>(columnCount - 1);
		int ncWidth = -1;

		for (int i = 0; i < columnCount; i++) {
			final String name = mainTable.getColumnName(i);
            if (name == null || name.equals("")) {
				continue;
			} else if (name.equals("#")) { 
				ncWidth = mainTable.getColumnModel().getColumn(i).getWidth();

			} else {
				storedColumns.add(name.toLowerCase());
				columnsWidths.add(String.valueOf(mainTable.getColumnModel().getColumn(
						i).getWidth()));

			}
		}

		
		Globals.prefs.putStringArray("columnNames",
				storedColumns.toArray(new String[0]));
		Globals.prefs.putStringArray("columnWidths",
				columnsWidths.toArray(new String[0]));

		
		Globals.prefs.putInt("numberColWidth", ncWidth);
	}

	
	public void columnAdded(TableColumnModelEvent e) {
		assert e != null : simpleClassName + " received null event";

		updateColumnPrefs();
	}

	
	public void columnMarginChanged(ChangeEvent e) {
		assert e != null : simpleClassName + " received null event";
		
		updateColumnPrefs();
	}

	
	public void columnMoved(TableColumnModelEvent e) {
		assert e != null : simpleClassName + " received null event";

		
		if (e.getFromIndex() == e.getToIndex())
			return;

		updateColumnPrefs();

	}

	
	public void columnRemoved(TableColumnModelEvent e) {
		assert e != null : simpleClassName + " received null event";

		updateColumnPrefs();

	}

	
	public void columnSelectionChanged(ListSelectionEvent e) {
		
	}

}
