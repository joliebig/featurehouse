

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
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.msg.PropertiesChanged;

import java.util.concurrent.ConcurrentHashMap;
import java.io.*;

import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.browser.VFSFileChooserDialog;
import org.gjt.sp.jedit.gui.RolloverButton;
import org.gjt.sp.jedit.help.*;

import org.gjt.sp.util.Log;
import org.gjt.sp.util.IOUtilities;
import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.util.StandardUtilities;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;



public class ManagePanel extends JPanel
{
	
	private final JCheckBox hideLibraries;
	private final JTable table;
	private final JScrollPane scrollpane;
	private final PluginDetailPanel pluginDetailPanel;
	private final PluginTableModel pluginModel;
	private final PluginManager window;
	private JPopupMenu popup;
	private Set<String> selectedPlugins;
	private Set<String> jarNames;
	

	
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
		table.getSelectionModel().addListSelectionListener(new TableSelectionListener());
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
		pluginDetailPanel = new PluginDetailPanel();
		scrollpane.setPreferredSize(new Dimension(400,400));
		JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
			true, scrollpane, pluginDetailPanel);
		add(BorderLayout.CENTER, split);
		split.setResizeWeight(0.75);
		
		Box buttons = new Box(BoxLayout.X_AXIS);

		buttons.add(new RemoveButton());
		buttons.add(new SaveButton());
		buttons.add(new RestoreButton());
		buttons.add(new FindOrphan());
		buttons.add(Box.createGlue());
		buttons.add(new HelpButton());

		add(BorderLayout.SOUTH,buttons);

		pluginModel.update();
	} 

	
	public void update()
	{
		pluginModel.update();
	} 

	
	
	private class ManagePanelRestoreHandler extends DefaultHandler
	{
		ManagePanelRestoreHandler()
		{
			selectedPlugins = new HashSet<String>();
			jarNames = new HashSet<String>();
		}



		@Override
		public void startElement(String uri, String localName,
							String qName, Attributes attrs) throws SAXException
		{
			if (localName.equals("plugin"))
			{
				String jarName = attrs.getValue("jar");
				String name = attrs.getValue("name");
				Entry e = new Entry(jarName);
				e.name=name;
				selectedPlugins.add(name);
				jarNames.add(jarName);
			}
		}
	}

	
	boolean loadPluginSet(String path)
	{
		VFS vfs = VFSManager.getVFSForPath(path);
		Object session = vfs.createVFSSession(path, this);
		try
		{
			InputStream is = vfs._createInputStream(session, path, false, this);
			XMLUtilities.parseXML(is, new ManagePanelRestoreHandler());
			is.close();
			int rowCount = pluginModel.getRowCount();
			for (int i=0 ; i<rowCount ; i++)
			{
				Entry ent = pluginModel.getEntry(i);
				String name = ent.name;
				if (name != null)
				{
					pluginModel.setValueAt(selectedPlugins.contains(name), i, 0);
				}
				else
				{
					String jarPath = ent.jar;
					String jarName = jarPath.substring(1 + jarPath.lastIndexOf(File.separatorChar));
					try
					{
						pluginModel.setValueAt(jarNames.contains(jarName), i, 0);
					}
					catch (Exception e)
					{
						Log.log(Log.WARNING, this, "Exception thrown loading: " + jarName);
					}
				}
			}
		}
		catch (Exception e)
		{
			Log.log(Log.ERROR, this, "Loading Pluginset Error", e);
			return false;
		}
		pluginModel.update();
		return true;
	}

	
	
	private static Collection<String> getDeclaredJars(String jarName) throws IOException
	{
		Collection<String> jarList = new ArrayList<String>();
		PluginJAR pluginJAR = new PluginJAR(new File(jarName));
		PluginJAR.PluginCacheEntry pluginCacheEntry = PluginJAR.getPluginCache(pluginJAR);
		if (pluginCacheEntry == null)
		{
			pluginCacheEntry = pluginJAR.generateCache();
		}
		Properties cachedProperties = pluginCacheEntry.cachedProperties;

		String jars = cachedProperties.getProperty("plugin." + pluginCacheEntry.pluginClass + ".jars");

		if (jars != null)
		{
			String dir = MiscUtilities.getParentOfPath(pluginJAR.getPath());
			StringTokenizer st = new StringTokenizer(jars);
			while (st.hasMoreTokens())
			{
				String _jarPath = MiscUtilities.constructPath(dir, st.nextToken());
				if (new File(_jarPath).exists())
					jarList.add(_jarPath);
			}
		}
		jarList.add(jarName);
		return jarList;
	}

	

	
	private enum KeyboardCommand
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
		
		String description;
		EditPlugin plugin;
		
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

			plugin = jar.getPlugin();
			if(plugin != null)
			{
				status = plugin instanceof EditPlugin.Broken
					? ERROR : LOADED;
				clazz = plugin.getClassName();
				name = jEdit.getProperty("plugin."+clazz+".name");
				version = jEdit.getProperty("plugin."+clazz+".version");
				author = jEdit.getProperty("plugin."+clazz+".author");
				docs = jEdit.getProperty("plugin."+clazz+".docs");
				description = jEdit.getProperty("plugin."+clazz+".description");
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
			{
				status = LOADED;
			}
		}
	} 

	
	private class PluginTableModel extends AbstractTableModel
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

		
		@Override
		public Class getColumnClass(int columnIndex)
		{
			switch (columnIndex)
			{
				case 0: return Boolean.class;
				default: return Object.class;
			}
		} 

		
		@Override
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
					{
						return entry.name;
					}
				case 2:
					return entry.version;
				case 3:
					return jEdit.getProperty("plugin-manager.status." + entry.status);
				case 4:
					if (entry.dataSize == null && entry.plugin != null)
					{
						File pluginDirectory = entry.plugin.getPluginHome();
						if (null == pluginDirectory)
						{
							return null;
						}
						if (pluginDirectory.exists())
						{
							entry.dataSize = MiscUtilities.formatFileSize(IOUtilities.fileLength(pluginDirectory));
						}
						else
						{
							if (jEdit.getBooleanProperty("plugin." + entry.clazz + ".usePluginHome"))
							{
								entry.dataSize = MiscUtilities.formatFileSize(0);
							}
							else
							{
								entry.dataSize = jEdit.getProperty("manage-plugins.data-size.unknown");
							}

						}
					}
					return entry.dataSize;
				default:
					throw new Error("Column out of range");
			}
		} 

		
		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex)
		{
			return columnIndex == 0;
		} 

		
		@Override
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
			List<String> savedSelection = new ArrayList<String>();
			saveSelection(savedSelection);
			Collections.sort(entries,new EntryCompare(type));
			fireTableChanged(new TableModelEvent(this));
			restoreSelection(savedSelection);
		}
		

		
		public void update()
		{
			List<String> savedSelection = new ArrayList<String>();
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
				List<String> listModel = new ArrayList<String>();
				listModel.addAll(closureSet);
				Collections.sort(listModel, new StandardUtilities.StringCompare<String>(true));

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
			if (table != null)
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

	
	private class TextRenderer extends DefaultTableCellRenderer
	{
		private final DefaultTableCellRenderer tcr;

		TextRenderer(DefaultTableCellRenderer tcr)
		{
			this.tcr = tcr;
		}

		@Override
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

	
	private class HideLibrariesButton extends JCheckBox implements ActionListener
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

	
	
	private class RestoreButton extends RolloverButton implements ActionListener
	{
		RestoreButton()
		{
			setIcon(GUIUtilities.loadIcon(jEdit.getProperty("manage-plugins.restore.icon")));
			addActionListener(this);
			setToolTipText("Choose a PluginSet, select/deselect plugins based on set.");
		}

		public void actionPerformed(ActionEvent e)
		{
			String path = jEdit.getProperty(PluginManager.PROPERTY_PLUGINSET,
				jEdit.getSettingsDirectory() + File.separator);
			String[] selectedFiles = GUIUtilities.showVFSFileDialog(ManagePanel.this.window,
				jEdit.getActiveView(), path, VFSBrowser.OPEN_DIALOG, false);
			if (selectedFiles == null || selectedFiles.length != 1) return;
			path = selectedFiles[0];
			boolean success = loadPluginSet(path);
			if (success)
			{
				jEdit.setProperty(PluginManager.PROPERTY_PLUGINSET, path);
				EditBus.send(new PropertiesChanged(PluginManager.getInstance()));
			}

		}
	}

	
	
	private class SaveButton extends RolloverButton implements ActionListener
	{
		SaveButton()
		{
			setIcon(GUIUtilities.loadIcon(jEdit.getProperty("manage-plugins.save.icon")));
			setToolTipText("Save Currently Checked Plugins Set");
			addActionListener(this);
			setEnabled(true);
		}

		void saveState(String vfsURL, List<Entry> pluginList)
		{
			StringBuilder sb = new StringBuilder("<pluginset>\n ");

			for (Entry entry: pluginList)
			{
				String jarName = entry.jar.substring(1+entry.jar.lastIndexOf(File.separatorChar));
				sb.append("   <plugin name=\"").append(entry.name).append("\" jar=\"");
				sb.append(jarName).append("\" />\n ");
			}
			sb.append("</pluginset>\n");

			VFS vfs = VFSManager.getVFSForPath(vfsURL);
			Object session = vfs.createVFSSession(vfsURL, ManagePanel.this);
			Writer writer = null;
			try
			{
				OutputStream os = vfs._createOutputStream(session, vfsURL, ManagePanel.this);
				writer = new BufferedWriter(new OutputStreamWriter(os));
				writer.write(sb.toString());
			}
			catch (Exception e)
			{
				Log.log(Log.ERROR, this, "Saving State Error", e);
			}
			finally
			{
				IOUtilities.closeQuietly(writer);
			}

		}

		public void actionPerformed(ActionEvent e)
		{
			String path = jEdit.getProperty("plugin-manager.pluginset.path", jEdit.getSettingsDirectory() + File.separator);
			VFSFileChooserDialog fileChooser = new VFSFileChooserDialog(
				ManagePanel.this.window, jEdit.getActiveView(),
				path, VFSBrowser.SAVE_DIALOG, false , true);
			String[] fileselections = fileChooser.getSelectedFiles();
			List<Entry> pluginSelections = new ArrayList<Entry>();
			if (fileselections == null || fileselections.length != 1) return;

			PluginJAR[] jars = jEdit.getPluginJARs();
			for (PluginJAR jar : jars)
			{
				if (jar.getPlugin() != null)
				{
					Entry entry = new Entry (jar);
					pluginSelections.add(entry);
				}
			}
			saveState(fileselections[0], pluginSelections);
			jEdit.setProperty("plugin-manager.pluginset.path", fileselections[0]);
			EditBus.send(new PropertiesChanged(PluginManager.getInstance()));
		}
	}

	
	
	private class RemoveButton extends JButton implements ListSelectionListener, ActionListener
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
			Set<String> jarsToRemove = new HashSet<String>();
			
			
			
			Set<String> loadedJarsToRemove = new HashSet<String>();
			for(int i = 0; i < selected.length; i++)
			{
				Entry entry = pluginModel.getEntry(selected[i]);
				if (entry.status.equals(Entry.NOT_LOADED))
				{
					if (entry.jar != null)
					{
						try
						{
							Collection<String> jarList = getDeclaredJars(entry.jar);
							jarsToRemove.addAll(jarList);
						}
						catch (IOException e)
						{
							Log.log(Log.ERROR, this, e);
						}
					}
				}
				else
				{
					jarsToRemove.addAll(entry.jars);
					loadedJarsToRemove.addAll(entry.jars);
				}
				table.getSelectionModel().removeSelectionInterval(selected[i], selected[i]);
			}

			for (String jar : jarsToRemove)
			{
				listModel.add(jar);
				roster.addRemove(jar);
			}

			int button = GUIUtilities.listConfirm(window,
				"plugin-manager.remove-confirm",
				null,listModel.toArray());
			if(button == JOptionPane.YES_OPTION)
			{

				List<String> closureSet = new ArrayList<String>();
				PluginJAR.transitiveClosure(loadedJarsToRemove.toArray(new String[loadedJarsToRemove.size()]), closureSet);
				closureSet.removeAll(listModel);
				if (closureSet.isEmpty())
				{
					button = JOptionPane.YES_OPTION;
				}
				else
				{
					button = GUIUtilities.listConfirm(window,"plugin-manager.remove-dependencies",
						null, closureSet.toArray());
					Collections.sort(closureSet, new StandardUtilities.StringCompare<String>(true));
				}
				if(button == JOptionPane.YES_OPTION)
				{
					for (String jarName:closureSet)
					{
						PluginJAR pluginJAR = jEdit.getPluginJAR(jarName);
						jEdit.removePluginJAR(pluginJAR, false);
					}
					roster.performOperationsInAWTThread(window);
					pluginModel.update();
					if (table.getRowCount() != 0)
					{
						table.setRowSelectionInterval(0,0);
					}
					table.setColumnSelectionInterval(0,0);
					JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
					scrollbar.setValue(scrollbar.getMinimum());
				}
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

	
	private class FindOrphan extends JButton implements ActionListener
	{
		private FindOrphan()
		{
			super(jEdit.getProperty("plugin-manager.findOrphan.label"));
			addActionListener(this);
		}

		public void actionPerformed(ActionEvent e)
		{
			PluginJAR[] pluginJARs = jEdit.getPluginJARs();
			Set<String> neededJars = new HashSet<String>();

			Map<String, String> jarlibs = new HashMap<String, String>();
			for (PluginJAR pluginJAR : pluginJARs)
			{
				EditPlugin plugin = pluginJAR.getPlugin();
				if (plugin == null)
				{
					jarlibs.put(new File(pluginJAR.getPath()).getName(), pluginJAR.getPath());
				}
				else
				{
					Set<String> strings = plugin.getPluginJAR().getRequiredJars();
					for (String string : strings)
					{
						neededJars.add(new File(string).getName());
					}
				}
			}

			String[] notLoadedJars = jEdit.getNotLoadedPluginJARs();
			for (int i = 0; i < notLoadedJars.length; i++)
			{
				PluginJAR pluginJAR = new PluginJAR(new File(notLoadedJars[i]));
				PluginJAR.PluginCacheEntry pluginCacheEntry = PluginJAR.getPluginCache(pluginJAR);
				try
				{
					if (pluginCacheEntry == null)
					{
						pluginCacheEntry = pluginJAR.generateCache();
					}
					if (pluginCacheEntry.pluginClass == null)
					{
						
						jarlibs.put(new File(notLoadedJars[i]).getName(), notLoadedJars[i]);
						continue;
					}


					Properties cachedProperties = pluginCacheEntry.cachedProperties;

					String jars = cachedProperties.getProperty("plugin." + pluginCacheEntry.pluginClass + ".jars");

					if (jars != null)
					{
						StringTokenizer st = new StringTokenizer(jars);
						while (st.hasMoreTokens())
						{
							neededJars.add(st.nextToken());
						}
					}
				}
				catch (IOException e1)
				{
					Log.log(Log.ERROR, this, e);
				}
			}

			List<String> removingJars = new ArrayList<String>();
			Set<String> jarlibsKeys = jarlibs.keySet();
			for (String jar : jarlibsKeys)
			{
				if (!neededJars.contains(jar))
				{
					removingJars.add(jar);
					Log.log(Log.MESSAGE, this, "It seems that this jar do not belong to any plugin " +jar);
				}
			}
			if(removingJars.isEmpty())
			{
				GUIUtilities.message(ManagePanel.this, "plugin-manager.noOrphan", null);
				return;
			}

			String[] strings = removingJars.toArray(new String[removingJars.size()]);
			List<String> mustRemove = new ArrayList<String>();
			int ret = GUIUtilities.listConfirm(ManagePanel.this,
							   "plugin-manager.findOrphan",
							   null,
							   strings,
							   mustRemove);
			if (ret != JOptionPane.OK_OPTION || mustRemove.isEmpty())
				return;

			Roster roster = new Roster();
			for (int i = 0; i < mustRemove.size(); i++)
			{
				String entry = mustRemove.get(i);
				roster.addRemove(jarlibs.get(entry));
			}

			roster.performOperationsInAWTThread(window);
			pluginModel.update();
			if (table.getRowCount() != 0)
			{
				table.setRowSelectionInterval(0,0);
			}
			table.setColumnSelectionInterval(0,0);
			JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
			scrollbar.setValue(scrollbar.getMinimum());
			table.repaint();
		}
	} 

	
	private class HelpButton extends JButton implements ListSelectionListener, ActionListener
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
					if (label != null)
					{
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
				catch (Exception ex)
				{
					Log.log(Log.ERROR, this, "ManagePanel HelpButton Update", ex);
				}
			}
			setEnabled(false);
		}
	} 

	
	private static class EntryCompare implements Comparator<Entry>
	{
		public static final int NAME = 1;
		public static final int STATUS = 2;

		private final int type;

		EntryCompare(int type)
		{
			this.type = type;
		}

		public int compare(Entry e1, Entry e2)
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

		private static int compareNames(Entry e1, Entry e2)
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

	
	private class HeaderMouseHandler extends MouseAdapter
	{
		@Override
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
					int row = table.rowAtPoint(evt.getPoint());
					if (row != -1 &&
					    !table.isRowSelected(row))
					{
						table.setRowSelectionInterval(row,row);
					}
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
					if (entry.plugin != null)
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

				for (int i = 0; i < entries.size(); i++)
				{
					Entry entry = entries.get(i);
					File path = entry.plugin.getPluginHome();
					Log.log(Log.NOTICE, this, "Removing data of plugin " + entry.name + " home="+path);
					FileVFS.recursiveDelete(path);
					entry.dataSize = null;
				}
				table.repaint();
			}
		} 
	} 

	
	private class KeyboardAction extends AbstractAction
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
				for (int i = 0; i < rows.length; i++)
				{
					Object st = pluginModel.getValueAt(rows[i], 0);
					pluginModel.setValueAt(st.equals(Boolean.FALSE), rows[i], 0);
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

	
	private class TableFocusHandler extends FocusAdapter
	{
		@Override
		public void focusGained(FocusEvent fe)
		{
			if (table.getSelectedRow() == -1)
			{
				table.setRowSelectionInterval(0,0);
				JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
				scrollbar.setValue(scrollbar.getMinimum());
			}
			if (table.getSelectedColumn() == -1)
			{
				table.setColumnSelectionInterval(0,0);
			}
		}
	} 

	
	private class TableSelectionListener implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent e)
		{
			int row = table.getSelectedRow();
			if (row != -1)
			{
				Entry entry = pluginModel.getEntry(row);
				pluginDetailPanel.setPlugin(entry);
			}
		}
	} 
	
}
