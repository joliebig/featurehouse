

package org.gjt.sp.jedit.options;


import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.gui.FilteredTableModel;
import org.gjt.sp.jedit.gui.GrabKeyDialog;
import org.gjt.sp.jedit.gui.GrabKeyDialog.KeyBinding;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Vector;



public class ShortcutsOptionPane extends AbstractOptionPane
{
	
	public ShortcutsOptionPane()
	{
		super("shortcuts");
	} 

	
	@Override
	protected void _init()
	{
		allBindings = new Vector<KeyBinding>();

		setLayout(new BorderLayout(12,12));

		initModels();

		selectModel = new JComboBox(models);
		selectModel.addActionListener(new ActionHandler());
		selectModel.setToolTipText(jEdit.getProperty("options.shortcuts.select.tooltip"));
		Box north = Box.createHorizontalBox();
		north.add(new JLabel(jEdit.getProperty(
			"options.shortcuts.select.label")));
		north.add(Box.createHorizontalStrut(6));
		north.add(selectModel);

		filterTF = new JTextField(40);
		filterTF.setToolTipText(jEdit.getProperty("options.shortcuts.filter.tooltip"));
		filterTF.getDocument().addDocumentListener(new DocumentListener()
		{
			public void changedUpdate(DocumentEvent e)
			{
				setFilter();
			}

			public void insertUpdate(DocumentEvent e)
			{
				setFilter();
			}

			public void removeUpdate(DocumentEvent e)
			{
				setFilter();
			}
		});
		JButton clearButton = new JButton(jEdit.getProperty(
				"options.shortcuts.clear.label"));
		clearButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent arg0)
			{
				filterTF.setText("");
				filterTF.requestFocus();
			}
		});

		JPanel filterPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		filterPanel.add(new JLabel(jEdit.getProperty("options.shortcuts.filter.label")));
		filterPanel.add(filterTF);
		filterPanel.add(clearButton);

		keyTable = new JTable(filteredModel);
		filteredModel.setTable(keyTable);
		keyTable.getTableHeader().setReorderingAllowed(false);
		keyTable.getTableHeader().addMouseListener(new HeaderMouseHandler());
		keyTable.addMouseListener(new TableMouseHandler());
		Dimension d = keyTable.getPreferredSize();
		d.height = Math.min(d.height,200);
		JScrollPane scroller = new JScrollPane(keyTable);
		scroller.setPreferredSize(d);
		JPanel tableFilterPanel = new JPanel(new BorderLayout());
		tableFilterPanel.add(BorderLayout.NORTH,filterPanel);
		tableFilterPanel.add(BorderLayout.CENTER,scroller);

		add(BorderLayout.NORTH,north);
		add(BorderLayout.CENTER,tableFilterPanel);
		try
		{
			selectModel.setSelectedIndex(jEdit.getIntegerProperty("options.shortcuts.select.index", 0));
		}
		catch (IllegalArgumentException eae) {}
	} 

	
	@Override
	protected void _save()
	{
		if(keyTable.getCellEditor() != null)
			keyTable.getCellEditor().stopCellEditing();

		for (ShortcutsModel model : models)
			model.save();

		Macros.loadMacros();
	} 

	
	private JTable keyTable;
	private Vector<ShortcutsModel> models;
	private FilteredTableModel<ShortcutsModel> filteredModel;
	private JComboBox selectModel;
	private List<KeyBinding> allBindings;
	private JTextField filterTF;

	
	private void setFilter()
	{
		filteredModel.setFilter(filterTF.getText());
	} 

	
	private void initModels()
	{
		List<KeyBinding[]> allBindings = new Vector<KeyBinding[]>();
		models = new Vector<ShortcutsModel>();
		ActionSet[] actionSets = jEdit.getActionSets();
		for(int i = 0; i < actionSets.length; i++)
		{
			ActionSet actionSet = actionSets[i];
			if(actionSet.getActionCount() != 0)
			{
				String modelLabel = actionSet.getLabel();
				if(modelLabel == null)
				{
					Log.log(Log.ERROR,this,"Empty action set: "
						+ actionSet.getPluginJAR());
				}
				ShortcutsModel model = createModel(modelLabel,
						actionSet.getActionNames());
				models.addElement(model);
				allBindings.addAll(model.getBindings());
			}
		}
		if (models.size() > 1)
			models.addElement(new ShortcutsModel("All", allBindings));
		Collections.sort(models,new StandardUtilities.StringCompare<ShortcutsModel>(true));
		ShortcutsModel currentModel = models.elementAt(0);
		filteredModel = new FilteredTableModel<ShortcutsModel>(currentModel)
		{
			@Override
			public String prepareFilter(String filter)
			{
				return filter.toLowerCase();
			}

			@Override
			public boolean passFilter(int row, String filter)
			{
				String name = delegated.getBindingAt(row, 0).label.toLowerCase();
				return name.contains(filter);
			}
		};
	} 

	
	private ShortcutsModel createModel(String modelLabel, String[] actions)
	{
		List<GrabKeyDialog.KeyBinding[]> bindings = new Vector<GrabKeyDialog.KeyBinding[]>(actions.length);

		for(int i = 0; i < actions.length; i++)
		{
			String name = actions[i];
			EditAction ea = jEdit.getAction(name);
			String label = ea.getLabel();
			
			if(label == null)
				continue;

			label = GUIUtilities.prettifyMenuLabel(label);
			addBindings(name,label,bindings);
		}

		return new ShortcutsModel(modelLabel,bindings);
	} 

	
	private void addBindings(String name, String label, List<GrabKeyDialog.KeyBinding[]> bindings)
	{
		GrabKeyDialog.KeyBinding[] b = new GrabKeyDialog.KeyBinding[2];

		b[0] = createBinding(name,label,
			jEdit.getProperty(name + ".shortcut"));
		b[1] = createBinding(name,label,
			jEdit.getProperty(name + ".shortcut2"));

		bindings.add(b);
	} 

	
	private GrabKeyDialog.KeyBinding createBinding(String name,
		String label, String shortcut)
	{
		if(shortcut != null && shortcut.length() == 0)
			shortcut = null;

		GrabKeyDialog.KeyBinding binding
			= new GrabKeyDialog.KeyBinding(name,label,shortcut,false);

		allBindings.add(binding);
		return binding;
	} 

	

	
	private class HeaderMouseHandler extends MouseAdapter
	{
		@Override
		public void mouseClicked(MouseEvent evt)
		{
			ShortcutsModel shortcutsModel = filteredModel.getDelegated();
			switch(keyTable.getTableHeader().columnAtPoint(evt.getPoint()))
			{
			case 0:
				shortcutsModel.sort(0);
				break;
			case 1:
				shortcutsModel.sort(1);
				break;
			case 2:
				shortcutsModel.sort(2);
				break;
			}
			setFilter();
		}
	} 

	
	private class TableMouseHandler extends MouseAdapter
	{
		@Override
		public void mouseClicked(MouseEvent evt)
		{
			int row = keyTable.getSelectedRow();
			int col = keyTable.getSelectedColumn();
			if(col != 0 && row != -1)
			{
				 GrabKeyDialog gkd = new GrabKeyDialog(
					GUIUtilities.getParentDialog(
					ShortcutsOptionPane.this),
					filteredModel.getDelegated().getBindingAt(filteredModel.getTrueRow(row), col - 1),
					allBindings,null);
				if(gkd.isOK())
					filteredModel.setValueAt(
						gkd.getShortcut(),row,col);
			}
		}
	} 

	
	private class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			ShortcutsModel newModel
				= (ShortcutsModel)selectModel.getSelectedItem();
			if(filteredModel.getDelegated() != newModel)
			{
				jEdit.setIntegerProperty("options.shortcuts.select.index", selectModel.getSelectedIndex());
				filteredModel.setDelegated(newModel);
				setFilter();
			}
		}
	} 

	
	private class ShortcutsModel extends AbstractTableModel
	{
		private List<GrabKeyDialog.KeyBinding[]> bindings;
		private String name;

		ShortcutsModel(String name, List<GrabKeyDialog.KeyBinding[]> bindings)
		{
			this.name = name;
			this.bindings = bindings;
			sort(0);
		}

		public List<GrabKeyDialog.KeyBinding[]> getBindings()
		{
			return bindings;
		}

		public void sort(int col)
		{
			Collections.sort(bindings,new KeyCompare(col));
		}

		public int getColumnCount()
		{
			return 3;
		}

		public int getRowCount()
		{
			return bindings.size();
		}

		public Object getValueAt(int row, int col)
		{
			switch(col)
			{
			case 0:
				return getBindingAt(row,0).label;
			case 1:
				return getBindingAt(row,0).shortcut;
			case 2:
				return getBindingAt(row,1).shortcut;
			default:
				return null;
			}
		}

		@Override
		public void setValueAt(Object value, int row, int col)
		{
			if(col == 0)
				return;

			getBindingAt(row,col-1).shortcut = (String)value;

			
			
			fireTableDataChanged();
		}

		@Override
		public String getColumnName(int index)
		{
			switch(index)
			{
			case 0:
				return jEdit.getProperty("options.shortcuts.name");
			case 1:
				return jEdit.getProperty("options.shortcuts.shortcut1");
			case 2:
				return jEdit.getProperty("options.shortcuts.shortcut2");
			default:
				return null;
			}
		}

		public void save()
		{
			for (GrabKeyDialog.KeyBinding[] binding : bindings)
			{
				jEdit.setProperty(
					binding[0].name + ".shortcut",
					binding[0].shortcut);
				jEdit.setProperty(
					binding[1].name + ".shortcut2",
					binding[1].shortcut);
			}
		}

		public GrabKeyDialog.KeyBinding getBindingAt(int row, int nr)
		{
			GrabKeyDialog.KeyBinding[] binding = bindings.get(row);
			return binding[nr];
		}

		@Override
		public String toString()
		{
			return name;
		}

		private class KeyCompare implements Comparator<GrabKeyDialog.KeyBinding[]>
		{
			private int col;

			KeyCompare(int col)
			{
				this.col = col;
			}

			public int compare(GrabKeyDialog.KeyBinding[] k1, GrabKeyDialog.KeyBinding[] k2)
			{
				String label1 = k1[0].label.toLowerCase();
				String label2 = k2[0].label.toLowerCase();

				if(col == 0)
					return StandardUtilities.compareStrings(
						label1,label2,true);
				else
				{
					String shortcut1, shortcut2;
					if(col == 1)
					{
						shortcut1 = k1[0].shortcut;
						shortcut2 = k2[0].shortcut;
					}
					else
					{
						shortcut1 = k1[1].shortcut;
						shortcut2 = k2[1].shortcut;
					}

					if(shortcut1 == null && shortcut2 != null)
						return 1;
					else if(shortcut2 == null && shortcut1 != null)
						return -1;
					else if(shortcut1 == null)
						return StandardUtilities.compareStrings(label1,label2,true);
					else
						return StandardUtilities.compareStrings(shortcut1,shortcut2,true);
				}
			}
		}
	} 
	
	
}
