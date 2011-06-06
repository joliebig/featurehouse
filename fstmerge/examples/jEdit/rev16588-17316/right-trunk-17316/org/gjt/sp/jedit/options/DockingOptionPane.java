

package org.gjt.sp.jedit.options;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.gjt.sp.jedit.AbstractOptionPane;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.DockableWindowManager;
import org.gjt.sp.util.StandardUtilities;



@SuppressWarnings("serial")
public class DockingOptionPane extends AbstractOptionPane
{
	
	public DockingOptionPane()
	{
		super("docking");
	} 

	
	public void _init()
	{
		setLayout(new BorderLayout());
		add(BorderLayout.NORTH,createDockingOptionsPanel());
		add(BorderLayout.CENTER,createWindowTableScroller());
		dockableSetSelection.setModel(
			new DefaultComboBoxModel(windowModel.getDockableSets()));
	} 

	
	public void _save()
	{
		jEdit.setBooleanProperty(AUTO_LOAD_MODE_LAYOUT_PROP, autoLoadModeLayout.isSelected());
		jEdit.setBooleanProperty(AUTO_SAVE_MODE_LAYOUT_PROP, autoSaveModeLayout.isSelected());
		windowModel.save();
	} 

	

	
	private JTable windowTable;
	private WindowTableModel windowModel;
	private JCheckBox autoLoadModeLayout;
	private JCheckBox autoSaveModeLayout;
	private JComboBox dockableSetSelection;
	

	private static final String DOCKING_OPTIONS_PREFIX = "options.docking.";
	public static final String AUTO_LOAD_MODE_LAYOUT_PROP = DOCKING_OPTIONS_PREFIX + "autoLoadModeLayout";
	private static final String AUTO_LOAD_MODE_LAYOUT_LABEL = AUTO_LOAD_MODE_LAYOUT_PROP + ".label";
	public static final String AUTO_SAVE_MODE_LAYOUT_PROP = DOCKING_OPTIONS_PREFIX + "autoSaveModeLayout";
	private static final String AUTO_SAVE_MODE_LAYOUT_LABEL = AUTO_SAVE_MODE_LAYOUT_PROP + ".label";
	
	private JPanel createDockingOptionsPanel()
	{
		JPanel p = new JPanel();
		p.setLayout(new GridLayout(0, 1));
		boolean autoLoadModeLayoutProp = jEdit.getBooleanProperty(
			AUTO_LOAD_MODE_LAYOUT_PROP, false);
		autoLoadModeLayout = new JCheckBox(
			jEdit.getProperty(AUTO_LOAD_MODE_LAYOUT_LABEL),
			autoLoadModeLayoutProp);
		p.add(autoLoadModeLayout);
		autoSaveModeLayout = new JCheckBox(
			jEdit.getProperty(AUTO_SAVE_MODE_LAYOUT_LABEL),
			jEdit.getBooleanProperty(AUTO_SAVE_MODE_LAYOUT_PROP, false));
		p.add(autoSaveModeLayout);
		autoSaveModeLayout.setEnabled(autoLoadModeLayoutProp);
		autoLoadModeLayout.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				autoSaveModeLayout.setEnabled(autoLoadModeLayout.isSelected());
			}
		});
		Box vSetSelection = Box.createVerticalBox();
		p.add(vSetSelection);
		Box setSelection = Box.createHorizontalBox();
		vSetSelection.add(setSelection);
		setSelection.add(Box.createHorizontalStrut(6));
		setSelection.add(new JLabel(jEdit.getProperty(
			"options.docking.selectSet.label")));
		setSelection.add(Box.createHorizontalStrut(6));
		dockableSetSelection = new JComboBox();
		setSelection.add(dockableSetSelection);
		dockableSetSelection.addItemListener(new ItemListener()
		{
			public void itemStateChanged(ItemEvent e)
			{
				windowModel.showSet((String) dockableSetSelection.getSelectedItem());
			}
		});
		setSelection.add(Box.createHorizontalStrut(6));
		vSetSelection.add(Box.createVerticalStrut(6));
		return p;
	}
	
	private JScrollPane createWindowTableScroller()
	{
		windowModel = createWindowModel();
		windowTable = new JTable(windowModel);
		windowTable.getTableHeader().setReorderingAllowed(false);
		windowTable.setColumnSelectionAllowed(false);
		windowTable.setRowSelectionAllowed(false);
		windowTable.setCellSelectionEnabled(false);

		DockPositionCellRenderer comboBox = new DockPositionCellRenderer();
		windowTable.setRowHeight(comboBox.getPreferredSize().height);
		TableColumn column = windowTable.getColumnModel().getColumn(1);
		column.setCellRenderer(comboBox);
		column.setCellEditor(new DefaultCellEditor(new DockPositionCellRenderer()));

		Dimension d = windowTable.getPreferredSize();
		d.height = Math.min(d.height,50);
		JScrollPane scroller = new JScrollPane(windowTable);
		scroller.setPreferredSize(d);
		return scroller;
	} 

	
	private static WindowTableModel createWindowModel()
	{
		return new WindowTableModel();
	} 

	

	
	static class DockPositionCellRenderer extends JComboBox
		implements TableCellRenderer
	{
		DockPositionCellRenderer()
		{
			super(new String[] {
				DockableWindowManager.FLOATING,
				DockableWindowManager.TOP,
				DockableWindowManager.LEFT,
				DockableWindowManager.BOTTOM,
				DockableWindowManager.RIGHT
			});
			DockPositionCellRenderer.this.setRequestFocusEnabled(false);
		}

		public Component getTableCellRendererComponent(JTable table,
			Object value, boolean isSelected, boolean hasFocus,
			int row, int column)
		{
			setSelectedItem(value);
			return this;
		}
	} 
} 


@SuppressWarnings("serial")
class WindowTableModel extends AbstractTableModel
{
	private static final String PLUGIN_SET_PREFIX = "Plugin: ";
	private static final String CORE_DOCKABLE_SET = "Core";
	private static final String ALL_DOCKABLE_SET = "All";
	private HashMap<String, Vector<Entry>> dockableSets;
	private Vector<Entry> windows;

	
	WindowTableModel()
	{
		dockableSets = new HashMap<String, Vector<Entry>>();
		Vector<Entry> all = new Vector<Entry>();
		dockableSets.put(ALL_DOCKABLE_SET, all);
		windows = new Vector<Entry>();
		String[] dockables = DockableWindowManager.getRegisteredDockableWindows();
		for (String dockable: dockables)
		{
			String plugin = DockableWindowManager.
				getDockableWindowPluginName(dockable);
			String set;
			if (plugin != null)
				set = PLUGIN_SET_PREFIX + plugin;
			else
				set = CORE_DOCKABLE_SET; 
			Vector<Entry> currentSetDockables = dockableSets.get(set);
			if (currentSetDockables == null)
			{
				currentSetDockables = new Vector<Entry>();
				dockableSets.put(set, currentSetDockables);
			}
			Entry entry = new Entry(dockable);
			currentSetDockables.add(entry);
			all.add(entry);
		}
		showSet(ALL_DOCKABLE_SET);
	} 

	public Vector<String> getDockableSets()
	{
		Vector<String> sets = new Vector<String>();
		for (String set: dockableSets.keySet())
			sets.add(set);
		sets.remove(ALL_DOCKABLE_SET);
		sets.remove(CORE_DOCKABLE_SET);
		Collections.sort(sets);
		sets.insertElementAt(CORE_DOCKABLE_SET, 0);
		sets.insertElementAt(ALL_DOCKABLE_SET, 0);
		return sets;
	}

	
	public void showSet(String set)
	{
		windows = dockableSets.get(set);
		Collections.sort(windows,new WindowCompare());
		fireTableDataChanged();
	} 

	
	public int getColumnCount()
	{
		return 2;
	} 

	
	public int getRowCount()
	{
		return windows.size();
	} 

	
	public Class getColumnClass(int col)
	{
		switch(col)
		{
		case 0:
		case 1:
			return String.class;
		default:
			throw new InternalError();
		}
	} 

	
	public Object getValueAt(int row, int col)
	{
		Entry window = (Entry)windows.elementAt(row);
		switch(col)
		{
		case 0:
			return window.title;
		case 1:
			return window.dockPosition;
		default:
			throw new InternalError();
		}
	} 

	
	public boolean isCellEditable(int row, int col)
	{
		return col != 0;
	} 

	
	public void setValueAt(Object value, int row, int col)
	{
		if(col == 0)
			return;

		Entry window = (Entry)windows.elementAt(row);
		switch(col)
		{
		case 1:
			window.dockPosition = (String)value;
			break;
		default:
			throw new InternalError();
		}

		fireTableRowsUpdated(row,row);
	} 

	
	public String getColumnName(int index)
	{
		switch(index)
		{
		case 0:
			return jEdit.getProperty("options.docking.title");
		case 1:
			return jEdit.getProperty("options.docking.dockPosition");
		default:
			throw new InternalError();
		}
	} 

	
	public void save()
	{
		for(int i = 0; i < windows.size(); i++)
		{
			((Entry)windows.elementAt(i)).save();
		}
	} 

	
	static class Entry
	{
		String name;
		String title;
		String dockPosition;

		Entry(String name)
		{
			this.name = name;
			title = jEdit.getProperty(name + ".title");
			if(title == null)
				title = name;

			dockPosition = jEdit.getProperty(name + ".dock-position");
			if(dockPosition == null)
				dockPosition = DockableWindowManager.FLOATING;
		}

		void save()
		{
			jEdit.setProperty(name + ".dock-position",dockPosition);
		}
	} 

	
	static class WindowCompare implements Comparator<Object>
	{
		public int compare(Object obj1, Object obj2)
		{
			Entry e1 = (Entry)obj1;
			Entry e2 = (Entry)obj2;

			return StandardUtilities.compareStrings(
				e1.title,e2.title,true);
		}
	} 
} 
