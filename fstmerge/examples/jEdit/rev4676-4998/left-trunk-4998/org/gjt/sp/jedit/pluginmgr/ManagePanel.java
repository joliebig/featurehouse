

package org.gjt.sp.jedit.pluginmgr;


import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
import java.net.URL;
import java.util.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.help.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


public class ManagePanel extends JPanel
{
	
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
		table.setRequestFocusEnabled(false);
		table.setDefaultRenderer(Object.class, new TextRenderer(
			(DefaultTableCellRenderer)table.getDefaultRenderer(Object.class)));

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
		header.addMouseListener(new HeaderMouseHandler());

		JScrollPane scrollpane = new JScrollPane(table);
		scrollpane.getViewport().setBackground(table.getBackground());
		add(BorderLayout.CENTER,scrollpane);

		
		Box buttons = new Box(BoxLayout.X_AXIS);

		buttons.add(new RemoveButton());
		buttons.add(Box.createGlue());
		buttons.add(new HelpButton());

		add(BorderLayout.SOUTH,buttons);
	} 

	
	public void update()
	{
		pluginModel.update();
	} 

	
	private JCheckBox hideLibraries;
	private JTable table;
	private PluginTableModel pluginModel;
	private JButton remove;
	private JButton help;
	private PluginManager window;

	
	private int showListConfirm(String name, String[] args,
		Vector listModel)
	{
		JList list = new JList(listModel);
		list.setVisibleRowCount(8);

		Object[] message = {
			jEdit.getProperty(name + ".message",args),
			new JScrollPane(list)
		};

		return JOptionPane.showConfirmDialog(window,
			message,
			jEdit.getProperty(name + ".title"),
			JOptionPane.YES_NO_OPTION,
			JOptionPane.QUESTION_MESSAGE);
	} 

	

	

	
	class Entry
	{
		static final String ERROR = "error";
		static final String LOADED = "loaded";
		static final String NOT_LOADED = "not-loaded";

		String status;
		String jar;

		String clazz, name, version, author, docs;
		List jars;

		Entry(String jar)
		{
			jars = new LinkedList();
			this.jar = jar;
			jars.add(this.jar);
			status = NOT_LOADED;
		}

		Entry(PluginJAR jar)
		{
			jars = new LinkedList();
			this.jar = jar.getPath();
			jars.add(this.jar);

			EditPlugin plugin = jar.getPlugin();
			if(plugin != null)
			{
				status = (plugin instanceof EditPlugin.Broken
					? ERROR : LOADED);
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
			}
			else
				status = LOADED;
		}
	} 

	
	class PluginTableModel extends AbstractTableModel
	{
		private List entries;
		private int sortType = EntryCompare.NAME;

		
		public PluginTableModel()
		{
			entries = new ArrayList();
			update();
		} 

		
		public int getColumnCount()
		{
			return 4;
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
				default:
					throw new Error("Column out of range");
			}
		} 

		
		public Entry getEntry(int rowIndex)
		{
			return (Entry)entries.get(rowIndex);
		} 

		
		public int getRowCount()
		{
			return entries.size();
		} 

		
		public Object getValueAt(int rowIndex,int columnIndex)
		{
			Entry entry = (Entry)entries.get(rowIndex);
			switch (columnIndex)
			{
				case 0:
					return new Boolean(
						!entry.status.equals(
						Entry.NOT_LOADED));
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
					return jEdit.getProperty("plugin-manager.status."
						+ entry.status);
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
			Entry entry = (Entry)entries.get(rowIndex);
			if(columnIndex == 0)
			{
				PluginJAR jar = jEdit.getPluginJAR(entry.jar);
				if(jar == null)
				{
					if(value.equals(Boolean.FALSE))
						return;

					loadPluginJAR(entry.jar);
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
			Collections.sort(entries,new EntryCompare(type));
			fireTableChanged(new TableModelEvent(this));
		}
		

		
		public void update()
		{
			entries.clear();

			String systemJarDir = MiscUtilities.constructPath(
				jEdit.getJEditHome(),"jars");
			String userJarDir;
			if(jEdit.getSettingsDirectory() == null)
				userJarDir = null;
			else
			{
				userJarDir = MiscUtilities.constructPath(
					jEdit.getSettingsDirectory(),"jars");
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
				if(!hideLibraries.isSelected()
					|| e.clazz != null)
				{
					entries.add(e);
				}
			}

			sort(sortType);
		} 

		
		
		private void loadPluginJAR(String jarPath)
		{
			jEdit.addPluginJAR(jarPath);
			PluginJAR jar = jEdit.getPluginJAR(jarPath);
			if(jar == null || jar.getPlugin() == null)
				return;

			String jars = jEdit.getProperty("plugin."
				+ jar.getPlugin().getClassName() + ".jars");

			if(jars != null)
			{
				String dir = MiscUtilities.getParentOfPath(
					jarPath);

				StringTokenizer st = new StringTokenizer(jars);
				while(st.hasMoreTokens())
				{
					String _jarPath
						= MiscUtilities.constructPath(
						dir,st.nextToken());
					PluginJAR _jar = jEdit.getPluginJAR(
						_jarPath);
					if(_jar == null)
					{
						jEdit.addPluginJAR(_jarPath);
					}
				}
			}

			jar.checkDependencies();
			jar.activatePluginIfNecessary();
		} 

		
		private void unloadPluginJARWithDialog(PluginJAR jar)
		{
			String[] dependents = jar.getDependentPlugins();
			if(dependents.length == 0)
				unloadPluginJAR(jar);
			else
			{
				Vector listModel = new Vector();
				transitiveClosure(dependents,listModel);

				int button = showListConfirm(
					"plugin-manager.dependency",
					new String[] { jar.getFile()
					.getName() },listModel);
				if(button == JOptionPane.YES_OPTION)
					unloadPluginJAR(jar);
			}
		} 

		
		
		private void transitiveClosure(String[] dependents,
			Vector listModel)
		{
			for(int i = 0; i < dependents.length; i++)
			{
				String jarPath = dependents[i];
				if(!listModel.contains(jarPath))
				{
					listModel.add(jarPath);
					PluginJAR jar = jEdit.getPluginJAR(
						jarPath);
					transitiveClosure(jar.getDependentPlugins(),
						listModel);
				}
			}
		} 

		
		
		private void unloadPluginJAR(PluginJAR jar)
		{
			String[] dependents = jar.getDependentPlugins();
			for(int i = 0; i < dependents.length; i++)
			{
				PluginJAR _jar = jEdit.getPluginJAR(
					dependents[i]);
				if(_jar != null)
				{
					unloadPluginJAR(_jar);
				}
			}

			jEdit.removePluginJAR(jar,false);
		} 
	} 

	
	class TextRenderer extends DefaultTableCellRenderer
	{
		private DefaultTableCellRenderer tcr;

		public TextRenderer(DefaultTableCellRenderer tcr)
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
		public RemoveButton()
		{
			super(jEdit.getProperty("manage-plugins.remove"));
			table.getSelectionModel().addListSelectionListener(this);
			addActionListener(this);
			setEnabled(false);
		}

		public void actionPerformed(ActionEvent evt)
		{
			int[] selected = table.getSelectedRows();

			Vector listModel = new Vector();
			Roster roster = new Roster();
			for(int i = 0; i < selected.length; i++)
			{
				Entry entry = pluginModel.getEntry(selected[i]);
				Iterator iter = entry.jars.iterator();
				while(iter.hasNext())
				{
					String jar = (String)iter.next();
					listModel.addElement(jar);
					roster.addRemove(jar);
				}
			}

			int button = showListConfirm(
				"plugin-manager.remove-confirm",
				null,listModel);
			if(button == JOptionPane.YES_OPTION)
			{
				roster.performOperationsInAWTThread(window);
				pluginModel.update();
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

		public HelpButton()
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
					PluginJAR jar = jEdit.getPlugin(label)
						.getPluginJAR();
					if(jar != null && label != null && docs != null)
					{
						URL url = jar.getClassLoader()
							.getResource(docs);
						if(url != null)
						{
							docURL = url;
							setEnabled(true);
							return;
						}
					}
				}
				catch (Exception ex) {}
			}
			setEnabled(false);
		}
	} 

	
	static class EntryCompare implements Comparator
	{
		public static final int NAME = 1;
		public static final int STATUS = 2;

		private int type;

		public EntryCompare(int type)
		{
			this.type = type;
		}

		public int compare(Object o1, Object o2)
		{
			Entry e1 = (Entry)o1;
			Entry e2 = (Entry)o2;

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

		private int compareNames(Entry e1, Entry e2)
		{
			String s1, s2;
			if(e1.name == null)
				s1 = MiscUtilities.getFileName(e1.jar);
			else
				s1 = e1.name;
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
	} 

	
}
