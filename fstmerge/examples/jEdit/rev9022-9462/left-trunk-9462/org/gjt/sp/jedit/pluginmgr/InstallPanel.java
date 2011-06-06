

package org.gjt.sp.jedit.pluginmgr;


import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.text.NumberFormat;
import java.util.*;
import java.util.List;

import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.StandardUtilities;



class InstallPanel extends JPanel
{
	
	InstallPanel(PluginManager window, boolean updates)
	{
		super(new BorderLayout(12,12));

		this.window = window;
		this.updates = updates;

		setBorder(new EmptyBorder(12,12,12,12));

		final JSplitPane split = new JSplitPane(
			JSplitPane.VERTICAL_SPLIT, jEdit.getBooleanProperty("appearance.continuousLayout"));

		
		table = new JTable(pluginModel = new PluginTableModel());
		table.setShowGrid(false);
		table.setIntercellSpacing(new Dimension(0,0));
		table.setRowHeight(table.getRowHeight() + 2);
		table.setPreferredScrollableViewportSize(new Dimension(500,200));
		table.setDefaultRenderer(Object.class, new TextRenderer(
			(DefaultTableCellRenderer)table.getDefaultRenderer(Object.class)));
		table.addFocusListener(new TableFocusHandler());
		InputMap tableInputMap = table.getInputMap(JComponent.WHEN_FOCUSED);
		ActionMap tableActionMap = table.getActionMap();
		tableInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB,0),"tabOutForward");
		tableActionMap.put("tabOutForward",new KeyboardAction(KeyboardCommand.TAB_OUT_FORWARD));
		tableInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB,InputEvent.SHIFT_MASK),"tabOutBack");
		tableActionMap.put("tabOutBack",new KeyboardAction(KeyboardCommand.TAB_OUT_BACK));
		tableInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE,0),"editPlugin");
		tableActionMap.put("editPlugin",new KeyboardAction(KeyboardCommand.EDIT_PLUGIN));
		tableInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,0),"closePluginManager");
		tableActionMap.put("closePluginManager",new KeyboardAction(KeyboardCommand.CLOSE_PLUGIN_MANAGER));

		TableColumn col1 = table.getColumnModel().getColumn(0);
		TableColumn col2 = table.getColumnModel().getColumn(1);
		TableColumn col3 = table.getColumnModel().getColumn(2);
		TableColumn col4 = table.getColumnModel().getColumn(3);
		TableColumn col5 = table.getColumnModel().getColumn(4);

		col1.setPreferredWidth(30);
		col1.setMinWidth(30);
		col1.setMaxWidth(30);
		col1.setResizable(false);

		col2.setPreferredWidth(180);
		col3.setPreferredWidth(130);
		col4.setPreferredWidth(70);
		col5.setPreferredWidth(70);

		JTableHeader header = table.getTableHeader();
		header.setReorderingAllowed(false);
		header.addMouseListener(new HeaderMouseHandler());

		scrollpane = new JScrollPane(table);
		scrollpane.getViewport().setBackground(table.getBackground());
		split.setTopComponent(scrollpane);

		
		JScrollPane infoPane = new JScrollPane(
			infoBox = new PluginInfoBox());
		infoPane.setPreferredSize(new Dimension(500,100));
		split.setBottomComponent(infoPane);

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				split.setDividerLocation(0.75);
			}
		});

		add(BorderLayout.CENTER,split);

		
		Box buttons = new Box(BoxLayout.X_AXIS);

		buttons.add(new InstallButton());
		buttons.add(Box.createHorizontalStrut(12));
		buttons.add(new SelectallButton());
		buttons.add(Box.createGlue());
		buttons.add(new SizeLabel());

		add(BorderLayout.SOUTH,buttons);
	} 

	
	public void updateModel()
	{
		final Set<String> savedChecked = new HashSet<String>();
		final Set<String> savedSelection = new HashSet<String>();
		pluginModel.saveSelection(savedChecked,savedSelection);
		pluginModel.clear();
		infoBox.setText(jEdit.getProperty("plugin-manager.list-download"));

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				infoBox.setText(null);
				pluginModel.update();
				pluginModel.restoreSelection(savedChecked,savedSelection);
			}
		});
	} 

	

	
	private final JTable table;
	private JScrollPane scrollpane;
	private PluginTableModel pluginModel;
	private PluginManager window;
	private PluginInfoBox infoBox;

	private boolean updates;
	

	
	private static String formatSize(int size)
	{
		NumberFormat df = NumberFormat.getInstance();
		df.setMaximumFractionDigits(1);
		df.setMinimumFractionDigits(0);
		String sizeText;
		if (size < 1048576)
			sizeText = (size >> 10) + "KB";
		else
			sizeText = df.format(size/ 1048576.0d) + "MB";
		return sizeText;
	} 

	

	

	
	public enum KeyboardCommand
	{
		NONE,
		TAB_OUT_FORWARD,
		TAB_OUT_BACK,
		EDIT_PLUGIN,
		CLOSE_PLUGIN_MANAGER
	} 

	
	class PluginTableModel extends AbstractTableModel
	{
		
		private List entries = new ArrayList();
		private int sortType = EntryCompare.NAME;

		
		public Class getColumnClass(int columnIndex)
		{
			switch (columnIndex)
			{
				case 0: return Boolean.class;
				case 1:
				case 2:
				case 3:
				case 4: return Object.class;
				default: throw new Error("Column out of range");
			}
		} 

		
		public int getColumnCount()
		{
			return 5;
		} 

		
		public String getColumnName(int column)
		{
			switch (column)
			{
				case 0: return " ";
				case 1: return ' '+jEdit.getProperty("install-plugins.info.name");
				case 2: return ' '+jEdit.getProperty("install-plugins.info.category");
				case 3: return ' '+jEdit.getProperty("install-plugins.info.version");
				case 4: return ' '+jEdit.getProperty("install-plugins.info.size");
				default: throw new Error("Column out of range");
			}
		} 

		
		public int getRowCount()
		{
			return entries.size();
		} 

		
		public Object getValueAt(int rowIndex,int columnIndex)
		{
			Object obj = entries.get(rowIndex);
			if(obj instanceof String)
			{
				if(columnIndex == 1)
					return obj;
				else
					return null;
			}
			else
			{
				Entry entry = (Entry)obj;

				switch (columnIndex)
				{
					case 0:
						return entry.install;
					case 1:
						return entry.name;
					case 2:
						return entry.set;
					case 3:
						if (updates)
							return entry.installedVersion + "->" + entry.version;
						return entry.version;
					case 4:
						return formatSize(entry.size);
					default:
						throw new Error("Column out of range");
				}
			}
		} 

		
		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			return columnIndex == 0;
		} 

		
		public void setSelectAll(boolean b)
		{
			if(isDownloadingList())
				return;

			int length = getRowCount();
			for (int i = 0; i < length; i++)
			{
				if (b)
					setValueAt(Boolean.TRUE,i,0);
				else
				{
					Entry entry = (Entry)entries.get(i);
					entry.parents = new LinkedList<Entry>();
					entry.install = false;
				}
			}
			fireTableChanged(new TableModelEvent(this));
		} 

		
		public void setSortType(int type)
		{
			sortType = type;
			sort(type);
		} 

		
		private void deselectParents(Entry entry)
		{
			Entry[] parents = entry.getParents();

			if (parents.length == 0)
				return;

			String[] args = { entry.name };

			int result = GUIUtilities.listConfirm(
				window,"plugin-manager.dependency",
				args,parents);
			if (result != JOptionPane.OK_OPTION)
			{
				entry.install = true;
				return;
			}

			for(int i = 0; i < parents.length; i++)
				 parents[i].install = false;

			fireTableRowsUpdated(0,getRowCount() - 1);
		} 

		
		public void setValueAt(Object aValue, int row, int column)
		{
			if (column != 0) return;

			Object obj = entries.get(row);
			if(obj instanceof String)
				return;

			Entry entry = (Entry)obj;
			entry.install = Boolean.TRUE.equals(aValue);

			if (!entry.install)
				deselectParents(entry);

			List<PluginList.Dependency> deps = entry.plugin.getCompatibleBranch().deps;

			for (int i = 0; i < deps.size(); i++)
			{
				PluginList.Dependency dep = deps.get(i);
				if (dep.what.equals("plugin"))
				{
					for (int j = 0; j < entries.size(); j++)
					{
						Entry temp = (Entry)entries.get(j);
						if (temp.plugin == dep.plugin)
						{
							if (entry.install)
							{
								temp.parents.add(entry);
								setValueAt(Boolean.TRUE,j,0);
							}
							else
								temp.parents.remove(entry);
						}
					}
				}
			}

			fireTableCellUpdated(row,column);
		} 

		
		public void sort(int type)
		{
			Set<String> savedChecked = new HashSet<String>();
			Set<String> savedSelection = new HashSet<String>();
			saveSelection(savedChecked,savedSelection);

			sortType = type;

			if(isDownloadingList())
				return;

			Collections.sort(entries,new EntryCompare(type));
			fireTableChanged(new TableModelEvent(this));
			restoreSelection(savedChecked,savedSelection);
		}
		

		
		private boolean isDownloadingList()
		{
			return entries.size() == 1 && entries.get(0) instanceof String;
		} 

		
		public void clear()
		{
			entries = new ArrayList();
			fireTableChanged(new TableModelEvent(this));
		} 

		
		public void update()
		{
			Set<String> savedChecked = new HashSet<String>();
			Set<String> savedSelection = new HashSet<String>();
			saveSelection(savedChecked,savedSelection);

			PluginList pluginList = window.getPluginList();

			if (pluginList == null) return;

			entries = new ArrayList();

			for(int i = 0; i < pluginList.pluginSets.size(); i++)
			{
				PluginList.PluginSet set = pluginList.pluginSets.get(i);
				for(int j = 0; j < set.plugins.size(); j++)
				{
					PluginList.Plugin plugin = pluginList.pluginHash.get(set.plugins.get(j));
					PluginList.Branch branch = plugin.getCompatibleBranch();
					String installedVersion =
						plugin.getInstalledVersion();
					if (updates)
					{
						if(branch != null
							&& branch.canSatisfyDependencies()
							&& installedVersion != null
							&& StandardUtilities.compareStrings(branch.version,
							installedVersion,false) > 0)
						{
							entries.add(new Entry(plugin,set.name));
						}
					}
					else
					{
						if(installedVersion == null && plugin.canBeInstalled())
							entries.add(new Entry(plugin,set.name));
					}
				}
			}

			sort(sortType);

			fireTableChanged(new TableModelEvent(this));
			restoreSelection(savedChecked,savedSelection);
		} 

		
		public void saveSelection(Set<String> savedChecked, Set<String> savedSelection)
		{
			if (entries.isEmpty())
				return;
			for (int i=0, c=getRowCount() ; i<c ; i++)
			{
				if ((Boolean)getValueAt(i,0))
				{
					savedChecked.add(entries.get(i).toString());
				}
			}
			int[] rows = table.getSelectedRows();
			for (int i=0 ; i<rows.length ; i++)
			{
				savedSelection.add(entries.get(rows[i]).toString());
			}
		} 

		
		public void restoreSelection(Set<String> savedChecked, Set<String> savedSelection)
		{
			for (int i=0, c=getRowCount() ; i<c ; i++)
			{
				setValueAt(savedChecked.contains(entries.get(i).toString()),i,0);
			}

			if (null != table)
			{
				table.setColumnSelectionInterval(0,0);
				if (!savedSelection.isEmpty())
				{
					int i = 0;
					int rowCount = getRowCount();
					for ( ; i<rowCount ; i++)
					{
						if (savedSelection.contains(entries.get(i).toString()))
						{
							table.setRowSelectionInterval(i,i);
							break;
						}
					}
					ListSelectionModel lsm = table.getSelectionModel();
					for ( ; i<rowCount ; i++)
					{
						if (savedSelection.contains(entries.get(i).toString()))
						{
							lsm.addSelectionInterval(i,i);
						}
					}
				}
				else
				{
					if (table.getRowCount() != 0)
						table.setRowSelectionInterval(0,0);
					JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
					scrollbar.setValue(scrollbar.getMinimum());
				}
			}
		} 
	} 

	
	class Entry
	{
		String name, installedVersion, version, author, date, description, set;
		int size;
		boolean install;
		PluginList.Plugin plugin;
		List<Entry> parents = new LinkedList<Entry>();

		Entry(PluginList.Plugin plugin, String set)
		{
			PluginList.Branch branch = plugin.getCompatibleBranch();
			boolean downloadSource = jEdit.getBooleanProperty("plugin-manager.downloadSource");
			int size = downloadSource ? branch.downloadSourceSize : branch.downloadSize;

			this.name = plugin.name;
			this.author = plugin.author;
			this.installedVersion = plugin.getInstalledVersion();
			this.version = branch.version;
			this.size = size;
			this.date = branch.date;
			this.description = plugin.description;
			this.set = set;
			this.install = false;
			this.plugin = plugin;
		}

		private void getParents(List<Entry> list)
		{
			for (Entry entry : parents)
			{
				if (entry.install && !list.contains(entry))
				{
					list.add(entry);
					entry.getParents(list);
				}
			}
		}

		Entry[] getParents()
		{
			List<Entry> list = new ArrayList<Entry>();
			getParents(list);
			Entry[] array = list.toArray(new Entry[list.size()]);
			Arrays.sort(array,new MiscUtilities.StringICaseCompare());
			return array;
		}

		public String toString()
		{
			return name;
		}
	} 

	
	class PluginInfoBox extends JTextArea implements ListSelectionListener
	{
		PluginInfoBox()
		{
			setEditable(false);
			setLineWrap(true);
			setWrapStyleWord(true);
			table.getSelectionModel().addListSelectionListener(this);
		}


		public void valueChanged(ListSelectionEvent e)
		{
			String text = "";
			if (table.getSelectedRowCount() == 1)
			{
				Entry entry = (Entry)pluginModel.entries
					.get(table.getSelectedRow());
				text = jEdit.getProperty("install-plugins.info",
					new String[] {entry.author,entry.date,entry.description});
			}
			setText(text);
			setCaretPosition(0);
		}
	} 

	
	class SizeLabel extends JLabel implements TableModelListener
	{
		private int size;

		SizeLabel()
		{
			size = 0;
			setText(jEdit.getProperty("install-plugins.totalSize")+formatSize(size));
			pluginModel.addTableModelListener(this);
		}

		public void tableChanged(TableModelEvent e)
		{
			if (e.getType() == TableModelEvent.UPDATE)
			{
				if(pluginModel.isDownloadingList())
					return;

				size = 0;
				int length = pluginModel.getRowCount();
				for (int i = 0; i < length; i++)
				{
					Entry entry = (Entry)pluginModel
						.entries.get(i);
					if (entry.install)
						size += entry.size;
				}
				setText(jEdit.getProperty("install-plugins.totalSize")+formatSize(size));
			}
		}
	} 

	
	class SelectallButton extends JCheckBox implements ActionListener, TableModelListener
	{
		SelectallButton()
		{
			super(jEdit.getProperty("install-plugins.select-all"));
			addActionListener(this);
			pluginModel.addTableModelListener(this);
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent evt)
		{
			pluginModel.setSelectAll(isSelected());
		}

		public void tableChanged(TableModelEvent e)
		{
			if(pluginModel.isDownloadingList())
				return;

			setEnabled(pluginModel.getRowCount() != 0);
			if (e.getType() == TableModelEvent.UPDATE)
			{
				int length = pluginModel.getRowCount();
				for (int i = 0; i < length; i++)
					if (!((Boolean)pluginModel.getValueAt(i,0)).booleanValue())
					{
						setSelected(false);
						return;
					}
				if (length > 0)
					setSelected(true);
			}
		}
	} 

	
	class InstallButton extends JButton implements ActionListener, TableModelListener
	{
		InstallButton()
		{
			super(jEdit.getProperty("install-plugins.install"));
			pluginModel.addTableModelListener(this);
			addActionListener(this);
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent evt)
		{
			if(pluginModel.isDownloadingList())
				return;

			boolean downloadSource = jEdit.getBooleanProperty(
				"plugin-manager.downloadSource");
			boolean installUser = jEdit.getBooleanProperty(
				"plugin-manager.installUser");
			Roster roster = new Roster();
			String installDirectory;
			if(installUser)
			{
				installDirectory = MiscUtilities.constructPath(
					jEdit.getSettingsDirectory(),"jars");
			}
			else
			{
				installDirectory = MiscUtilities.constructPath(
					jEdit.getJEditHome(),"jars");
			}

			int length = pluginModel.getRowCount();
			int instcount = 0;
			for (int i = 0; i < length; i++)
			{
				Entry entry = (Entry)pluginModel.entries.get(i);
				if (entry.install)
				{
					entry.plugin.install(roster,installDirectory,downloadSource);
					if (updates)
						entry.plugin.getCompatibleBranch().satisfyDependencies(
						roster,installDirectory,downloadSource);
					instcount++;
				}
			}

			if(roster.isEmpty())
				return;

			boolean cancel = false;
			if (updates && roster.getOperationCount() > instcount)
				if (GUIUtilities.confirm(window,
					"install-plugins.depend",
					null,
					JOptionPane.OK_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE) == JOptionPane.CANCEL_OPTION)
					cancel = true;

			if (!cancel)
			{
				new PluginManagerProgress(window,roster);

				roster.performOperationsInAWTThread(window);
				pluginModel.update();
			}
		}

		public void tableChanged(TableModelEvent e)
		{
			if(pluginModel.isDownloadingList())
				return;

			if (e.getType() == TableModelEvent.UPDATE)
			{
				int length = pluginModel.getRowCount();
				for (int i = 0; i < length; i++)
					if (((Boolean)pluginModel.getValueAt(i,0)).booleanValue())
					{
						setEnabled(true);
						return;
					}
				setEnabled(false);
			}
		}
	} 

	
	static class EntryCompare implements Comparator
	{
		public static final int NAME = 0;
		public static final int CATEGORY = 1;

		private int type;

		EntryCompare(int type)
		{
			this.type = type;
		}

		public int compare(Object o1, Object o2)
		{
			InstallPanel.Entry e1 = (InstallPanel.Entry)o1;
			InstallPanel.Entry e2 = (InstallPanel.Entry)o2;

			if (type == NAME)
				return e1.name.compareToIgnoreCase(e2.name);
			else
			{
				int result;
				if ((result = e1.set.compareToIgnoreCase(e2.set)) == 0)
					return e1.name.compareToIgnoreCase(e2.name);
				return result;
			}
		}
	} 

	
	class HeaderMouseHandler extends MouseAdapter
	{
		public void mouseClicked(MouseEvent evt)
		{
			switch(table.getTableHeader().columnAtPoint(evt.getPoint()))
			{
				case 1:
					pluginModel.sort(EntryCompare.NAME);
					break;
				case 2:
					pluginModel.sort(EntryCompare.CATEGORY);
					break;
				default:
			}
		}
	} 

	
	static class TextRenderer extends DefaultTableCellRenderer
	{
		private DefaultTableCellRenderer tcr;

		TextRenderer(DefaultTableCellRenderer tcr)
		{
			this.tcr = tcr;
		}

		public Component getTableCellRendererComponent(JTable table, Object value,
			boolean isSelected, boolean hasFocus, int row, int column)
		{
			return tcr.getTableCellRendererComponent(table,value,isSelected,false,row,column);
		}
	} 

	
	class KeyboardAction extends AbstractAction
	{
		private KeyboardCommand command = KeyboardCommand.NONE;

		KeyboardAction(KeyboardCommand command)
		{
			this.command = command;
		}

		public void actionPerformed(ActionEvent evt)
		{
			switch (command)
			{
			case TAB_OUT_FORWARD:
				KeyboardFocusManager.getCurrentKeyboardFocusManager().focusNextComponent();
				break;
			case TAB_OUT_BACK:
				KeyboardFocusManager.getCurrentKeyboardFocusManager().focusPreviousComponent();
				break;
			case EDIT_PLUGIN:
				int[] rows = table.getSelectedRows();
				Object[] state = new Object[rows.length];
				for (int i=0 ; i<rows.length ; i++)
				{
					state[i] = pluginModel.getValueAt(rows[i],0);
				}
				for (int i=0 ; i<rows.length ; i++)
				{
					pluginModel.setValueAt(state[i].equals(Boolean.FALSE),rows[i],0);
				}
				break;
			case CLOSE_PLUGIN_MANAGER:
				window.ok();
				break;
			default:
				throw new InternalError();
			}
		}
	} 

	
	class TableFocusHandler extends FocusAdapter
	{
		public void focusGained(FocusEvent fe)
		{
			if (-1 == table.getSelectedRow() && table.getRowCount() > 0)
			{
				table.setRowSelectionInterval(0,0);
				JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
				scrollbar.setValue(scrollbar.getMinimum());
			}
			if (-1 == table.getSelectedColumn())
			{
				table.setColumnSelectionInterval(0,0);
			}
		}
	} 

	
}
