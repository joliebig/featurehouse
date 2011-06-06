

package org.gjt.sp.jedit.pluginmgr;


import java.awt.*;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import java.net.URL;

import java.util.*;
import java.util.List;

import javax.swing.*;

import javax.swing.border.EmptyBorder;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.io.FileVFS;

import java.util.concurrent.ConcurrentHashMap;
import java.io.File;

import org.gjt.sp.jedit.help.*;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.IOUtilities;



public class ManagePanel extends JPanel
{

	
	private final JCheckBox hideLibraries;
	private final JTable table;
	private final JScrollPane scrollpane;
	private final PluginTableModel pluginModel;
	private final PluginManager window;
	private JPopupMenu popup;
	
	
	
	public ManagePanel(PluginManager window)
	{
		super(new BorderLayout(12,12));

		this.window = window;

		setBorder(new EmptyBorder(12,12,12,12));

		Box topBox = new Box(BoxLayout.X_AXIS);
		topBox.add(hideLibraries = new HideLibrariesButton());
		add(BorderLayout.NORTH,topBox);

		
		table = new JTable(pluginModel = new PluginTableModel());
		table.setShowGrid(false);
		table.setIntercellSpacing(new Dimension(0,0));
		table.setRowHeight(table.getRowHeight() + 2);
		table.setPreferredScrollableViewportSize(new Dimension(500,300));
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

		col1.setPreferredWidth(30);
		col1.setMinWidth(30);
		col1.setMaxWidth(30);
		col1.setResizable(false);

		col2.setPreferredWidth(300);
		col3.setPreferredWidth(100);
		col4.setPreferredWidth(100);

		JTableHeader header = table.getTableHeader();
		header.setReorderingAllowed(false);
		HeaderMouseHandler mouseHandler = new HeaderMouseHandler();
		header.addMouseListener(mouseHandler);
		table.addMouseListener(mouseHandler);
		scrollpane = new JScrollPane(table);
		scrollpane.getViewport().setBackground(table.getBackground());
		add(BorderLayout.CENTER,scrollpane);

		
		Box buttons = new Box(BoxLayout.X_AXIS);

		buttons.add(new RemoveButton());
		buttons.add(Box.createGlue());
		buttons.add(new HelpButton());

		add(BorderLayout.SOUTH,buttons);

		pluginModel.update();
	} 

	
	public void update()
	{
		pluginModel.update();
	} 

	
	private static File getPluginHome(String clazz, String settingsDirectory)
	{
		return new File(MiscUtilities.constructPath(settingsDirectory, "plugins", clazz));
	} 

	

	
	public enum KeyboardCommand
	{
		NONE,
		TAB_OUT_FORWARD,
		TAB_OUT_BACK,
		EDIT_PLUGIN,
		CLOSE_PLUGIN_MANAGER
	} 

	
	static class Entry
	{
		static final String ERROR = "error";
		static final String LOADED = "loaded";
		static final String NOT_LOADED = "not-loaded";

		final String status;
		final String jar;

		String clazz, name, version, author, docs;
		final List<String> jars;

		
		String dataSize;

		Entry(String jar)
		{
			jars = new LinkedList<String>();
			this.jar = jar;
			jars.add(this.jar);
			status = NOT_LOADED;
		}

		Entry(PluginJAR jar)
		{
			jars = new LinkedList<String>();
			this.jar = jar.getPath();
			jars.add(this.jar);

			EditPlugin plugin = jar.getPlugin();
			if(plugin != null)
			{
				status = plugin instanceof EditPlugin.Broken
					? ERROR : LOADED;
				clazz = plugin.getClassName();
				name = jEdit.getProperty("plugin."+clazz+".name");
				version = jEdit.getProperty("plugin."+clazz+".version");
				author = jEdit.getProperty("plugin."+clazz+".author");
				docs = jEdit.getProperty("plugin."+clazz+".docs");

				String jarsProp = jEdit.getProperty("plugin."+clazz+".jars");

				if(jarsProp != null)
				{
					String directory = MiscUtilities.getParentOfPath(this.jar);

					StringTokenizer st = new StringTokenizer(jarsProp);
					while(st.hasMoreElements())
					{
						jars.add(MiscUtilities.constructPath(
							directory,st.nextToken()));
					}
				}

				String filesProp = jEdit.getProperty("plugin."+clazz+".files");

				if(filesProp != null)
				{
					String directory = MiscUtilities.getParentOfPath(this.jar);

					StringTokenizer st = new StringTokenizer(filesProp);
					while(st.hasMoreElements())
					{
						jars.add(MiscUtilities.constructPath(
							directory,st.nextToken()));
					}
				}
			}
			else
				status = LOADED;
		}
	} 

	
	class PluginTableModel extends AbstractTableModel
	{
		private final List<Entry> entries;
		private int sortType = EntryCompare.NAME;
		private ConcurrentHashMap<String, Object> unloaded;
		

		
		PluginTableModel()
		{
			entries = new ArrayList<Entry>();
		} 

		
		public int getColumnCount()
		{
			return 5;
		} 

		
		public Class getColumnClass(int columnIndex)
		{
			switch (columnIndex)
			{
				case 0: return Boolean.class;
				default: return Object.class;
			}
		} 

		
		public String getColumnName(int column)
		{
			switch (column)
			{
				case 0:
					return " ";
				case 1:
					return jEdit.getProperty("manage-plugins.info.name");
				case 2:
					return jEdit.getProperty("manage-plugins.info.version");
				case 3:
					return jEdit.getProperty("manage-plugins.info.status");
				case 4:
					return jEdit.getProperty("manage-plugins.info.data");
				default:
					throw new Error("Column out of range");
			}
		} 

		
		public Entry getEntry(int rowIndex)
		{
			return entries.get(rowIndex);
		} 

		
		public int getRowCount()
		{
			return entries.size();
		} 

		
		public Object getValueAt(int rowIndex,int columnIndex)
		{
			Entry entry = entries.get(rowIndex);
			switch (columnIndex)
			{
				case 0:
					return Boolean.valueOf(!entry.status.equals(Entry.NOT_LOADED));
				case 1:
					if(entry.name == null)
					{
						return MiscUtilities.getFileName(entry.jar);
					}
					else
						return entry.name;
				case 2:
					return entry.version;
				case 3:
					return jEdit.getProperty("plugin-manager.status." + entry.status);
				case 4:
					if (entry.dataSize == null && entry.clazz != null)
					{
						String settingsDirectory = jEdit.getSettingsDirectory();
						if (settingsDirectory == null)
							return null;
						String pluginDirectory = MiscUtilities.constructPath(settingsDirectory, "plugins", entry.clazz);
						File file = new File(pluginDirectory);
						if (file.exists())
							entry.dataSize = MiscUtilities.formatFileSize(IOUtilities.fileLength(file));
						else
							entry.dataSize = "";
					}
					return entry.dataSize;
				default:
					throw new Error("Column out of range");
			}
		} 

		
		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			return columnIndex == 0;
		} 

		
		public void setValueAt(Object value, int rowIndex,
			int columnIndex)
		{
			Entry entry = entries.get(rowIndex);
			if(columnIndex == 0)
			{
				PluginJAR jar = jEdit.getPluginJAR(entry.jar);
				if(jar == null)
				{
					if(value.equals(Boolean.FALSE))
						return;

					PluginJAR.load(entry.jar, true);
				}
				else
				{
					if(value.equals(Boolean.TRUE))
						return;

					unloadPluginJARWithDialog(jar);
				}
			}

			update();
		} 

		
		public void setSortType(int type)
		{
			sortType = type;
			sort(type);
		} 

		
		public void sort(int type)
		{
			ArrayList<String> savedSelection = new ArrayList<String>();
			saveSelection(savedSelection);
			Collections.sort(entries,new EntryCompare(type));
			fireTableChanged(new TableModelEvent(this));
			restoreSelection(savedSelection);
		}
		

		
		public void update()
		{
			ArrayList<String> savedSelection = new ArrayList<String>();
			saveSelection(savedSelection);
			entries.clear();

			String systemJarDir = MiscUtilities.constructPath(
				jEdit.getJEditHome(),"jars");
			String userJarDir;
			String settingsDirectory = jEdit.getSettingsDirectory();
			if(settingsDirectory == null)
				userJarDir = null;
			else
			{
				userJarDir = MiscUtilities.constructPath(
					settingsDirectory,"jars");
			}

			PluginJAR[] plugins = jEdit.getPluginJARs();
			for(int i = 0; i < plugins.length; i++)
			{
				String path = plugins[i].getPath();
				if(path.startsWith(systemJarDir)
					|| (userJarDir != null
					&& path.startsWith(userJarDir)))
				{
					Entry e = new Entry(plugins[i]);
					if(!hideLibraries.isSelected()
						|| e.clazz != null)
					{
						entries.add(e);
					}
				}
			}

			String[] newPlugins = jEdit.getNotLoadedPluginJARs();
			for(int i = 0; i < newPlugins.length; i++)
			{
				Entry e = new Entry(newPlugins[i]);
				entries.add(e);
			}

			sort(sortType);
			restoreSelection(savedSelection);
		} 

		
		
		private void unloadPluginJARWithDialog(PluginJAR jar)
		{
			
			unloaded = new ConcurrentHashMap<String, Object>();
			String[] dependents = jar.getDependentPlugins();
			if(dependents.length == 0)
				unloadPluginJAR(jar);
			else
			{
				List<String> closureSet = new LinkedList<String>();
				PluginJAR.transitiveClosure(dependents, closureSet);
				ArrayList<String> listModel = new ArrayList<String>();
				listModel.addAll(closureSet);
				Collections.sort(listModel, new MiscUtilities.StringICaseCompare());

				int button = GUIUtilities.listConfirm(window,"plugin-manager.dependency",
					new String[] { jar.getFile().getName() }, listModel.toArray());
				if(button == JOptionPane.YES_OPTION)
					unloadPluginJAR(jar);
			}
		} 

		
		private void unloadPluginJAR(PluginJAR jar)
		{
			String[] dependents = jar.getDependentPlugins();
			for (String dependent : dependents) 
			{
				if (!unloaded.containsKey(dependent)) 
				{
					unloaded.put(dependent, Boolean.TRUE);
					PluginJAR _jar = jEdit.getPluginJAR(dependent);
					if(_jar != null)
						unloadPluginJAR(_jar);
				}
			}
			jEdit.removePluginJAR(jar,false);
			jEdit.setBooleanProperty("plugin-blacklist."+MiscUtilities.getFileName(jar.getPath()),true);
		} 
		
		
		public void saveSelection(List<String> savedSelection)
		{
			if (null != table)
			{
				int[] rows = table.getSelectedRows();
				for (int i=0 ; i<rows.length ; i++)
				{
					savedSelection.add(entries.get(rows[i]).jar);
				}
			}
		} 
		
		
		public void restoreSelection(List<String> savedSelection)
		{
			if (null != table)
			{
				table.setColumnSelectionInterval(0,0);
				if (!savedSelection.isEmpty())
				{
					int i = 0;
					int rowCount = getRowCount();
					for ( ; i<rowCount ; i++)
					{
						if (savedSelection.contains(entries.get(i).jar))
						{
							table.setRowSelectionInterval(i,i);
							break;
						}
					}
					ListSelectionModel lsm = table.getSelectionModel();
					for ( ; i<rowCount ; i++)
					{
						if (savedSelection.contains(entries.get(i).jar))
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

	
	class TextRenderer extends DefaultTableCellRenderer
	{
		private final DefaultTableCellRenderer tcr;

		TextRenderer(DefaultTableCellRenderer tcr)
		{
			this.tcr = tcr;
		}

		public Component getTableCellRendererComponent(JTable table, Object value,
			boolean isSelected, boolean hasFocus, int row, int column)
		{
			Entry entry = pluginModel.getEntry(row);
			if (entry.status.equals(Entry.ERROR))
				tcr.setForeground(Color.red);
			else
				tcr.setForeground(UIManager.getColor("Table.foreground"));
			return tcr.getTableCellRendererComponent(table,value,isSelected,false,row,column);
		}
	} 

	
	class HideLibrariesButton extends JCheckBox implements ActionListener
	{
		HideLibrariesButton()
		{
			super(jEdit.getProperty("plugin-manager.hide-libraries"));
			setSelected(jEdit.getBooleanProperty(
				"plugin-manager.hide-libraries.toggle"));
			addActionListener(this);
		}

		public void actionPerformed(ActionEvent evt)
		{
			jEdit.setBooleanProperty(
				"plugin-manager.hide-libraries.toggle",
				isSelected());
			ManagePanel.this.update();
		}
	} 

	
	class RemoveButton extends JButton implements ListSelectionListener, ActionListener
	{
		RemoveButton()
		{
			super(jEdit.getProperty("manage-plugins.remove"));
			table.getSelectionModel().addListSelectionListener(this);
			addActionListener(this);
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent evt)
		{
			int[] selected = table.getSelectedRows();

			List<String> listModel = new LinkedList<String>();
			Roster roster = new Roster();
			for(int i = 0; i < selected.length; i++)
			{
				Entry entry = pluginModel.getEntry(selected[i]);
				for (String jar : entry.jars)
				{
					listModel.add(jar);
					roster.addRemove(jar);
					table.getSelectionModel().removeSelectionInterval(selected[i], selected[i]);
				}
			}
			int button = GUIUtilities.listConfirm(window,
				"plugin-manager.remove-confirm",
				null,listModel.toArray());
			if(button == JOptionPane.YES_OPTION)
			{
				roster.performOperationsInAWTThread(window);
				pluginModel.update();
				if (table.getRowCount() != 0)
					table.setRowSelectionInterval(0,0);
				table.setColumnSelectionInterval(0,0);
				JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
				scrollbar.setValue(scrollbar.getMinimum());
			}
		}

		public void valueChanged(ListSelectionEvent e)
		{
			if (table.getSelectedRowCount() == 0)
				setEnabled(false);
			else
				setEnabled(true);
		}
	} 

	
	class HelpButton extends JButton implements ListSelectionListener, ActionListener
	{
		private URL docURL;

		HelpButton()
		{
			super(jEdit.getProperty("manage-plugins.help"));
			table.getSelectionModel().addListSelectionListener(this);
			addActionListener(this);
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent evt)
		{
			new HelpViewer(docURL);
		}

		public void valueChanged(ListSelectionEvent e)
		{
			if (table.getSelectedRowCount() == 1)
			{
				try
				{
					Entry entry = pluginModel.getEntry(table.getSelectedRow());
					String label = entry.clazz;
					String docs = entry.docs;
					if (label != null) {
						EditPlugin plug = jEdit.getPlugin(label, false);
						PluginJAR jar = null;
						if (plug != null) jar = plug.getPluginJAR();
						if(jar != null && docs != null)
						{
							URL url = jar.getClassLoader().getResource(docs);
							if(url != null)
							{
								docURL = url;
								setEnabled(true);
								return;
							}
						}
					}
				}
				catch (Exception ex) {
					Log.log(Log.ERROR, this, "ManagePanel HelpButton Update", ex);
				}
			}
			setEnabled(false);
		}
	} 

	
	private static class EntryCompare implements Comparator<ManagePanel.Entry>
	{
		public static final int NAME = 1;
		public static final int STATUS = 2;

		private final int type;

		EntryCompare(int type)
		{
			this.type = type;
		}

		public int compare(ManagePanel.Entry e1, ManagePanel.Entry e2)
		{
			if (type == NAME)
				return compareNames(e1,e2);
			else
			{
				int result;
				if ((result = e1.status.compareToIgnoreCase(e2.status)) == 0)
					return compareNames(e1,e2);
				return result;
			}
		}

		private static int compareNames(ManagePanel.Entry e1, ManagePanel.Entry e2)
		{
			String s1;
			if(e1.name == null)
				s1 = MiscUtilities.getFileName(e1.jar);
			else
				s1 = e1.name;
			String s2;
			if(e2.name == null)
				s2 = MiscUtilities.getFileName(e2.jar);
			else
				s2 = e2.name;

			return s1.compareToIgnoreCase(s2);
		}
	} 

	
	class HeaderMouseHandler extends MouseAdapter
	{
		public void mouseClicked(MouseEvent evt)
		{
			if (evt.getSource() == table.getTableHeader())
			{
				switch(table.getTableHeader().columnAtPoint(evt.getPoint()))
				{
					case 1:
						pluginModel.setSortType(EntryCompare.NAME);
						break;
					case 3:
						pluginModel.setSortType(EntryCompare.STATUS);
						break;
					default:
						break;
				}
			}
			else
			{
				if (GUIUtilities.isPopupTrigger(evt))
				{
					if (popup == null)
					{
						popup = new JPopupMenu();
						JMenuItem item = GUIUtilities.loadMenuItem("plugin-manager.cleanup");
						item.addActionListener(new CleanupActionListener());
						popup.add(item);
					}
					GUIUtilities.showPopupMenu(popup, table, evt.getX(), evt.getY());
				}
			}
		}

		private class CleanupActionListener implements ActionListener
		{
			public void actionPerformed(ActionEvent e)
			{
				int[] ints = table.getSelectedRows();
				List<String> list = new ArrayList<String>(ints.length);
				List<Entry> entries = new ArrayList<Entry>(ints.length);
				for (int i = 0; i < ints.length; i++)
				{
					Entry entry = pluginModel.getEntry(ints[i]);
					if (entry.clazz != null)
					{
						list.add(entry.name);
						entries.add(entry);
					}
				}

				String[] strings = list.toArray(new String[list.size()]);
				int ret = GUIUtilities.listConfirm(ManagePanel.this,
								   "plugin-manager.cleanup",
								   null,
								   strings);
				if (ret != JOptionPane.OK_OPTION)
					return;

				String settingsDirectory = jEdit.getSettingsDirectory();
				for (int i = 0; i < entries.size(); i++)
				{
					Entry entry = entries.get(i);
					String clazz = entry.clazz;
					File path = getPluginHome(clazz, settingsDirectory);
					Log.log(Log.NOTICE, this, "Removing datas of plugin " + entry.name + " home="+path);
					FileVFS.recursiveDelete(path);
					entry.dataSize = null;
				}
				table.repaint();
			}
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
				List<String> savedSelection = new ArrayList<String>();
				pluginModel.saveSelection(savedSelection);
				int[] rows = table.getSelectedRows();
				Object[] state = new Object[rows.length];
				for (int i=0 ; i<rows.length ; i++)
				{
					state[i] = pluginModel.getValueAt(rows[i],0);
				}
				for (int i=0 ; i<rows.length ; i++)
				{
					for (int j=0, c=pluginModel.getRowCount() ; j<c ; j++)
					{
						if (pluginModel.entries.get(j).jar.equals(savedSelection.get(i)))
						{
							pluginModel.setValueAt(state[i].equals(Boolean.FALSE),j,0);
							break;
						}
					}
				}
				pluginModel.restoreSelection(savedSelection);
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
			if (-1 == table.getSelectedRow())
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
