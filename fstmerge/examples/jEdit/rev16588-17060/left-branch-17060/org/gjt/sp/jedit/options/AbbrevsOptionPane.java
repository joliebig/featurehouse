

package org.gjt.sp.jedit.options;


import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.util.List;

import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.StandardUtilities;




public class AbbrevsOptionPane extends AbstractOptionPane
{
	
	public AbbrevsOptionPane()
	{
		super("abbrevs");
	} 

	
	@Override
	protected void _init()
	{
		setLayout(new BorderLayout());

		JPanel panel = new JPanel(new BorderLayout(6,6));

		expandOnInput = new JCheckBox(jEdit.getProperty("options.abbrevs"
			+ ".expandOnInput"),Abbrevs.getExpandOnInput());

		panel.add(expandOnInput,BorderLayout.NORTH);

		JPanel panel2 = new JPanel();
		panel2.setLayout(new BoxLayout(panel2,BoxLayout.X_AXIS));
		panel2.setBorder(new EmptyBorder(0,0,6,0));
		panel2.add(Box.createGlue());
		JLabel label = new JLabel(jEdit.getProperty("options.abbrevs.set"));
		label.setBorder(new EmptyBorder(0,0,0,12));
		panel2.add(label);

		Map<String,Hashtable<String,String>> _modeAbbrevs = Abbrevs.getModeAbbrevs();
		modeAbbrevs = new HashMap<String,AbbrevsModel>();
		Mode[] modes = jEdit.getModes();
		Arrays.sort(modes,new StandardUtilities.StringCompare<Mode>(true));
		String[] sets = new String[modes.length + 1];
		sets[0] = "global";
		for(int i = 0; i < modes.length; i++)
		{
			String name = modes[i].getName();
			sets[i+1] = name;
			modeAbbrevs.put(name,new AbbrevsModel(_modeAbbrevs.get(name)));
		}

		setsComboBox = new JComboBox(sets);
		ActionHandler actionHandler = new ActionHandler();
		setsComboBox.addActionListener(actionHandler);
		panel2.add(setsComboBox);
		panel2.add(Box.createGlue());
		panel.add(panel2,BorderLayout.SOUTH);

		add(BorderLayout.NORTH,panel);

		globalAbbrevs = new AbbrevsModel(Abbrevs.getGlobalAbbrevs());
		abbrevsTable = new JTable(globalAbbrevs);
		abbrevsTable.getColumnModel().getColumn(1).setCellRenderer(
			new Renderer());
		abbrevsTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		abbrevsTable.getTableHeader().setReorderingAllowed(false);
		abbrevsTable.getTableHeader().addMouseListener(new HeaderMouseHandler());
		abbrevsTable.getSelectionModel().addListSelectionListener(
			new SelectionHandler());
		abbrevsTable.getSelectionModel().setSelectionMode(
			ListSelectionModel.SINGLE_SELECTION);
		abbrevsTable.addMouseListener(new TableMouseHandler());
		Dimension d = abbrevsTable.getPreferredSize();
		d.height = Math.min(d.height,200);
		JScrollPane scroller = new JScrollPane(abbrevsTable);
		scroller.setPreferredSize(d);
		add(BorderLayout.CENTER,scroller);

		JPanel buttons = new JPanel();
		buttons.setLayout(new BoxLayout(buttons,BoxLayout.X_AXIS));
		buttons.setBorder(new EmptyBorder(6,0,0,0));

		add = new RolloverButton(GUIUtilities.loadIcon(jEdit.getProperty("options.abbrevs.add.icon")));
		add.setToolTipText(jEdit.getProperty("options.abbrevs.add"));
		add.addActionListener(actionHandler);
		buttons.add(add);
		remove = new RolloverButton(GUIUtilities.loadIcon(jEdit.getProperty("options.abbrevs.remove.icon")));
		remove.setToolTipText(jEdit.getProperty("options.abbrevs.remove"));
		remove.addActionListener(actionHandler);
		buttons.add(remove);
		edit = new RolloverButton(GUIUtilities.loadIcon(jEdit.getProperty("options.abbrevs.edit.icon")));
		edit.setToolTipText(jEdit.getProperty("options.abbrevs.edit"));
		edit.addActionListener(actionHandler);
		buttons.add(edit);
		buttons.add(Box.createGlue());

		add(BorderLayout.SOUTH,buttons);
		setsComboBox.setSelectedIndex(jEdit.getIntegerProperty("options.abbrevs.combobox.index", 0));
		updateEnabled();
	} 

	
	@Override
	protected void _save()
	{
		if(abbrevsTable.getCellEditor() != null)
			abbrevsTable.getCellEditor().stopCellEditing();

		Abbrevs.setExpandOnInput(expandOnInput.isSelected());

		Abbrevs.setGlobalAbbrevs(globalAbbrevs.toHashtable());

		Hashtable<String,Hashtable<String,String>> modeHash = new Hashtable<String,Hashtable<String,String>>();
		Set<Map.Entry<String,AbbrevsModel>> entrySet = modeAbbrevs.entrySet();
		for (Map.Entry<String,AbbrevsModel> entry : entrySet)
		{
			modeHash.put(entry.getKey(),entry.getValue().toHashtable());
		}
		Abbrevs.setModeAbbrevs(modeHash);
	} 

	

	
	private JComboBox setsComboBox;
	private JCheckBox expandOnInput;
	private JTable abbrevsTable;
	private AbbrevsModel globalAbbrevs;
	private Map<String,AbbrevsModel> modeAbbrevs;
	private JButton add;
	private JButton edit;
	private JButton remove;
	

	
	private void updateEnabled()
	{
		int selectedRow = abbrevsTable.getSelectedRow();
		edit.setEnabled(selectedRow != -1);
		remove.setEnabled(selectedRow != -1);
	} 

	
	private void edit()
	{
		AbbrevsModel abbrevsModel = (AbbrevsModel)abbrevsTable.getModel();

		int row = abbrevsTable.getSelectedRow();

		String abbrev = (String)abbrevsModel.getValueAt(row,0);
		String expansion = (String)abbrevsModel.getValueAt(row,1);
		String oldAbbrev = abbrev;

		EditAbbrevDialog dialog = new EditAbbrevDialog(
			GUIUtilities.getParentDialog(AbbrevsOptionPane.this),
			abbrev,expansion,abbrevsModel.toHashtable());
		abbrev = dialog.getAbbrev();
		expansion = dialog.getExpansion();
		if(abbrev != null && expansion != null)
		{
			for(int i = 0; i < abbrevsModel.getRowCount(); i++)
			{
				if(abbrevsModel.getValueAt(i,0).equals(oldAbbrev))
				{
					abbrevsModel.remove(i);
					break;
				}
			}

			add(abbrevsModel,abbrev,expansion);
		}
	} 

	
	private void add(AbbrevsModel abbrevsModel, String abbrev,
		String expansion)
	{
		for(int i = 0; i < abbrevsModel.getRowCount(); i++)
		{
			if(abbrevsModel.getValueAt(i,0).equals(abbrev))
			{
				abbrevsModel.remove(i);
				break;
			}
		}

		abbrevsModel.add(abbrev,expansion);
		updateEnabled();
	} 

	

	
	private class HeaderMouseHandler extends MouseAdapter
	{
		@Override
		public void mouseClicked(MouseEvent evt)
		{
			switch(abbrevsTable.getTableHeader().columnAtPoint(evt.getPoint()))
			{
			case 0:
				((AbbrevsModel)abbrevsTable.getModel()).sort(0);
				break;
			case 1:
				((AbbrevsModel)abbrevsTable.getModel()).sort(1);
				break;
			}
		}
	} 

	
	private class TableMouseHandler extends MouseAdapter
	{
		@Override
		public void mouseClicked(MouseEvent evt)
		{
			if(evt.getClickCount() == 2)
				edit();
		}
	} 

	
	private class SelectionHandler implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent evt)
		{
			updateEnabled();
		}
	} 

	
	private class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			AbbrevsModel abbrevsModel = (AbbrevsModel)abbrevsTable.getModel();

			Object source = evt.getSource();
			if(source == setsComboBox)
			{
				jEdit.setIntegerProperty("options.abbrevs.combobox.index", setsComboBox.getSelectedIndex());
				String selected = (String)setsComboBox.getSelectedItem();
				if(selected.equals("global"))
				{
					abbrevsTable.setModel(globalAbbrevs);
				}
				else
				{
					abbrevsTable.setModel(modeAbbrevs.get(selected));
				}
				updateEnabled();
			}
			else if(source == add)
			{
				EditAbbrevDialog dialog = new EditAbbrevDialog(
					GUIUtilities.getParentDialog(AbbrevsOptionPane.this),
					null,null,abbrevsModel.toHashtable());
				String abbrev = dialog.getAbbrev();
				String expansion = dialog.getExpansion();
				if(abbrev != null && abbrev.length() != 0
					&& expansion != null
					&& expansion.length() != 0)
				{
					add(abbrevsModel,abbrev,expansion);
				}
			}
			else if(source == edit)
			{
				edit();
			}
			else if(source == remove)
			{
				int selectedRow = abbrevsTable.getSelectedRow();
				abbrevsModel.remove(selectedRow);
				updateEnabled();
			}
		}
	} 

	
	private static class Renderer extends DefaultTableCellRenderer
	{
		@Override
		public Component getTableCellRendererComponent(
			JTable table,
			Object value,
			boolean isSelected,
			boolean cellHasFocus,
			int row,
			int col)
		{
			String valueStr = value.toString();

			
			
			if(valueStr.toLowerCase().startsWith("<html>"))
				valueStr = ' ' + valueStr;
			return super.getTableCellRendererComponent(table,valueStr,
				isSelected,cellHasFocus,row,col);
		}
	} 

	
	private static class AbbrevsModel extends AbstractTableModel
	{
		List<Abbrev> abbrevs;
		int lastSort;

		
		AbbrevsModel(Map<String,String> abbrevHash)
		{
			abbrevs = new Vector<Abbrev>();

			if(abbrevHash != null)
			{
				Set<Map.Entry<String,String>> entrySet = abbrevHash.entrySet();
				for (Map.Entry<String,String> entry : entrySet)
				{
					abbrevs.add(new Abbrev(entry.getKey(),
					                       entry.getValue()));
				}
				sort(0);
			}
		} 

		
		void sort(int col)
		{
			lastSort = col;
			Collections.sort(abbrevs,new AbbrevCompare(col));
			fireTableDataChanged();
		} 

		
		void add(String abbrev, String expansion)
		{
			abbrevs.add(new Abbrev(abbrev,expansion));
			sort(lastSort);
		} 

		
		void remove(int index)
		{
			abbrevs.remove(index);
			fireTableStructureChanged();
		} 

		
		public Hashtable<String,String> toHashtable()
		{
			Hashtable<String,String> hash = new Hashtable<String,String>();
			for(int i = 0; i < abbrevs.size(); i++)
			{
				Abbrev abbrev = abbrevs.get(i);
				if(abbrev.abbrev.length() > 0
				   && abbrev.expand.length() > 0)
				{
					hash.put(abbrev.abbrev,abbrev.expand);
				}
			}
			return hash;
		} 

		
		public int getColumnCount()
		{
			return 2;
		} 

		
		public int getRowCount()
		{
			return abbrevs.size();
		} 

		
		public Object getValueAt(int row, int col)
		{
			Abbrev abbrev = abbrevs.get(row);
			switch(col)
			{
				case 0:
					return abbrev.abbrev;
				case 1:
					return abbrev.expand;
				default:
					return null;
			}
		} 

		
		@Override
		public void setValueAt(Object value, int row, int col)
		{
			if(value == null)
				value = "";

			Abbrev abbrev = abbrevs.get(row);

			if(col == 0)
				abbrev.abbrev = (String)value;
			else
				abbrev.expand = (String)value;

			fireTableRowsUpdated(row,row);
		} 

		
		@Override
		public String getColumnName(int index)
		{
			switch(index)
			{
				case 0:
					return jEdit.getProperty("options.abbrevs.abbrev");
				case 1:
					return jEdit.getProperty("options.abbrevs.expand");
				default:
					return null;
			}
		} 

		
		private static class AbbrevCompare implements Comparator<Abbrev>
		{
			private int col;

			AbbrevCompare(int col)
			{
				this.col = col;
			}

			public int compare(Abbrev a1, Abbrev a2)
			{
				if(col == 0)
				{
					String abbrev1 = a1.abbrev.toLowerCase();
					String abbrev2 = a2.abbrev.toLowerCase();

					return StandardUtilities.compareStrings(
						abbrev1,abbrev2,true);
				}
				else
				{
					String expand1 = a1.expand.toLowerCase();
					String expand2 = a2.expand.toLowerCase();

					return StandardUtilities.compareStrings(
						expand1,expand2,true);
				}
			}
		} 

		
		private static class Abbrev
		{
			Abbrev() {}

			Abbrev(String abbrev, String expand)
			{
				this.abbrev = abbrev;
				this.expand = expand;
			}

			String abbrev;
			String expand;
		} 

	} 

} 
