

package org.gjt.sp.jedit.browser;


import org.gjt.sp.jedit.EditBus.EBHandler;
import org.gjt.sp.jedit.bsh.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.File;
import java.util.*;
import java.util.List;

import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.search.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.util.Log;
import org.gjt.sp.jedit.menu.MenuItemTextComparator;



public class VFSBrowser extends JPanel implements DefaultFocusComponent,
	DockableWindow
{
	public static final String NAME = "vfs.browser";

	
	
	public static final int OPEN_DIALOG = 0;

	
	public static final int SAVE_DIALOG = 1;
	
	public static final int BROWSER_DIALOG = 4;
	
	public static final int CHOOSE_DIRECTORY_DIALOG = 3;

	
	public static final int BROWSER = 2;
	

	
	
	public static void browseDirectoryInNewWindow(View view, String path)
	{
		DockableWindowManager wm = view.getDockableWindowManager();
		if(path != null)
		{
			
			jEdit.setTemporaryProperty("vfs.browser.path.tmp",path);
		}
		wm.floatDockableWindow("vfs.browser");
		jEdit.unsetProperty("vfs.browser.path.tmp");
	} 

	
	
	public static void browseDirectory(View view, String path)
	{
		DockableWindowManager wm = view.getDockableWindowManager();
		VFSBrowser browser = (VFSBrowser)wm.getDockable(NAME);
		if(browser != null)
		{
			wm.showDockableWindow(NAME);
			browser.setDirectory(path);
		}
		else
		{
			if(path != null)
			{
				
				jEdit.setTemporaryProperty("vfs.browser.path.tmp",path);
			}
			wm.addDockableWindow("vfs.browser");
			jEdit.unsetProperty("vfs.browser.path.tmp");
		}
	} 

	
	
	public static ActionContext getActionContext()
	{
		return actionContext;
	} 

	
	
	public VFSBrowser(View view, String position)
	{
		this(view,null,BROWSER,true,position);
	} 

	
	
	public VFSBrowser(View view, String path, int mode,
		boolean multipleSelection, String position)
	{
		super(new BorderLayout());

		listenerList = new EventListenerList();

		this.mode = mode;
		this.multipleSelection = multipleSelection;
		this.view = view;

		DockableWindowManager dwm = view.getDockableWindowManager();
		KeyListener keyListener = dwm.closeListener(NAME);
		addKeyListener(keyListener);
		
		currentEncoding = jEdit.getProperty("buffer.encoding",
			System.getProperty("file.encoding"));
		autoDetectEncoding = jEdit.getBooleanProperty(
			"buffer.encodingAutodetect");

		ActionHandler actionHandler = new ActionHandler();

		topBox = new Box(BoxLayout.Y_AXIS);
		horizontalLayout = mode != BROWSER
			|| DockableWindowManager.TOP.equals(position)
			|| DockableWindowManager.BOTTOM.equals(position);

		toolbarBox = new Box(horizontalLayout
			? BoxLayout.X_AXIS
			: BoxLayout.Y_AXIS);

		topBox.add(toolbarBox);

		GridBagLayout layout = new GridBagLayout();
		pathAndFilterPanel = new JPanel(layout);
		if(isHorizontalLayout())
			pathAndFilterPanel.setBorder(new EmptyBorder(12,12,12,12));

		GridBagConstraints cons = new GridBagConstraints();
		cons.gridwidth = cons.gridheight = 1;
		cons.gridx = cons.gridy = 0;
		cons.fill = GridBagConstraints.BOTH;
		cons.anchor = GridBagConstraints.EAST;
		JLabel label = new JLabel(jEdit.getProperty("vfs.browser.path"),
			SwingConstants.RIGHT);
		label.setBorder(new EmptyBorder(0,0,0,12));
		layout.setConstraints(label,cons);
		pathAndFilterPanel.add(label);

		pathField = new HistoryTextField("vfs.browser.path");
		pathField.setName("path");
		pathField.addKeyListener(keyListener);
		pathField.setInstantPopups(true);
		pathField.setEnterAddsToHistory(false);
		pathField.setSelectAllOnFocus(true);
		
		if (mode == BROWSER)
		{
			pathField.addKeyListener(new KeyAdapter()
			{
				@Override
				public void keyReleased(KeyEvent e)
				{
					if (e.getKeyCode() == KeyEvent.VK_ESCAPE)
					{
						pathField.setText(VFSBrowser.this.path);
					}
				}
			});
		}

		
		
		
		Dimension prefSize = pathField.getPreferredSize();
		prefSize.width = 0;
		pathField.setPreferredSize(prefSize);
		pathField.addActionListener(actionHandler);
		cons.gridx = 1;
		cons.weightx = 1.0;
		cons.gridwidth = GridBagConstraints.REMAINDER;

		layout.setConstraints(pathField,cons);
		pathAndFilterPanel.add(pathField);

		filterCheckbox = new JCheckBox(jEdit.getProperty("vfs.browser.filter"));
		filterCheckbox.setMargin(new Insets(0,0,0,0));

		filterCheckbox.setBorder(new EmptyBorder(0,0,0,12));
		filterCheckbox.setSelected(jEdit.getBooleanProperty(
			"vfs.browser.filter-enabled"));

		filterCheckbox.addActionListener(actionHandler);
		filterCheckbox.addKeyListener(keyListener);
		filterCheckbox.setName("filter-checkbox");
		if(mode != CHOOSE_DIRECTORY_DIALOG)
		{
			cons.gridwidth = 1;
			cons.gridx = 0;
			cons.weightx = 0.0;
			cons.gridy = 1;
			layout.setConstraints(filterCheckbox,cons);
			pathAndFilterPanel.add(filterCheckbox);
		}

		filterField = new JComboBox();
		filterEditor = new HistoryComboBoxEditor("vfs.browser.filter");
		filterEditor.setToolTipText(jEdit.getProperty("glob.tooltip"));
		filterEditor.setInstantPopups(true);
		filterEditor.setSelectAllOnFocus(true);
		filterEditor.addActionListener(actionHandler);
		filterEditor.addKeyListener(keyListener);
		filterField.setName("filter-field");
		String filter;
		if(mode == BROWSER || !jEdit.getBooleanProperty(
			"vfs.browser.currentBufferFilter"))
		{
			filter = jEdit.getProperty("vfs.browser.last-filter");
			if(filter == null)
				filter = jEdit.getProperty("vfs.browser.default-filter");
		}
		else
		{
			String ext = MiscUtilities.getFileExtension(
				view.getBuffer().getName());
			if(ext.length() == 0)
				filter = jEdit.getProperty("vfs.browser.default-filter");
			else
				filter = '*' + ext;
		}

		
		
		filterEditor.setItem(new GlobVFSFileFilter(filter));
		filterField.addItem(filterEditor.getItem());
		filterField.addItemListener(actionHandler);
		filterField.setRenderer(new VFSFileFilterRenderer());

		
		String[] _filters = ServiceManager.getServiceNames(VFSFileFilter.SERVICE_NAME);
		for (int i = 0; i < _filters.length; i++)
		{
			VFSFileFilter _filter = (VFSFileFilter)
				ServiceManager.getService(VFSFileFilter.SERVICE_NAME, _filters[i]);
			filterField.addItem(_filter);
		}

		if(mode != CHOOSE_DIRECTORY_DIALOG)
		{
			cons.gridwidth = GridBagConstraints.REMAINDER;
			cons.fill = GridBagConstraints.HORIZONTAL;
			cons.gridx = 1;
			cons.weightx = 1.0;
			if (filterField.getItemCount() > 1)
			{
				filterField.setEditor(filterEditor);
				filterField.setEditable(true);
				layout.setConstraints(filterField,cons);
				pathAndFilterPanel.add(filterField);
			}
			else
			{
				layout.setConstraints(filterEditor,cons);
				pathAndFilterPanel.add(filterEditor);
			}
		}

		topBox.add(pathAndFilterPanel);
		add(BorderLayout.NORTH,topBox);

		add(BorderLayout.CENTER,browserView = new BrowserView(this));
		if(isHorizontalLayout())
			browserView.setBorder(new EmptyBorder(0,12,0,12));
		defaultFocusComponent = browserView.getTable();
		propertiesChanged();

		updateFilterEnabled();

		setFocusTraversalPolicy(new LayoutFocusTraversalPolicy());
		
		if(path == null)
			path = jEdit.getProperty("vfs.browser.path.tmp");

		if(path == null || path.length() == 0)
		{
			String userHome = System.getProperty("user.home");
			String defaultPath = jEdit.getProperty("vfs.browser.defaultPath");
			if("home".equals(defaultPath))
				path = userHome;
			else if("working".equals(defaultPath))
				path = System.getProperty("user.dir");
			else if("buffer".equals(defaultPath))
			{
				Buffer buffer = view.getBuffer();
				path = buffer.getDirectory();
			}
			else if("last".equals(defaultPath))
			{
				HistoryModel pathModel = HistoryModel.getModel("vfs.browser.path");
				if(pathModel.getSize() == 0)
					path = "~";
				else
					path = pathModel.getItem(0);
			}
			else if("favorites".equals(defaultPath))
				path = "favorites:";
			else
			{
				
				path = userHome;
			}
		}

		final String _path = path;

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				setDirectory(_path);
			}
		});
	} 

	
	public void focusOnDefaultComponent()
	{
		
		defaultFocusComponent.requestFocus();
	} 

	
	
	void setDefaultFocusComponent(JComponent c) 
	{
		defaultFocusComponent = c;
	}
	
	
	@Override
	public void addNotify()
	{
		super.addNotify();
		EditBus.addToBus(this);
	} 

	
	@Override
	public void removeNotify()
	{
		super.removeNotify();
		jEdit.setBooleanProperty("vfs.browser.filter-enabled",
			filterCheckbox.isSelected());
		if(mode == BROWSER || !jEdit.getBooleanProperty(
			"vfs.browser.currentBufferFilter"))
		{
			VFSFileFilter selectedFilter =
				(VFSFileFilter) filterField.getSelectedItem();
			if (selectedFilter instanceof GlobVFSFileFilter)
				jEdit.setProperty("vfs.browser.last-filter",
					((GlobVFSFileFilter)selectedFilter).getGlob());
		}
		EditBus.removeFromBus(this);
	} 

	
	@EBHandler
	public void handlePropertiesChanged(PropertiesChanged msg)
	{
		propertiesChanged();
	} 

	
	@EBHandler
	public void handleBufferUpdate(BufferUpdate bmsg)
	{
		if (bmsg.getWhat() == BufferUpdate.CREATED ||
			bmsg.getWhat() == BufferUpdate.CLOSED)
		{
			browserView.updateFileView();
		}
	} 

	
	@EBHandler
	public void handlePluginUpdate(PluginUpdate pmsg)
	{
		if((pmsg.getWhat() == PluginUpdate.LOADED ||
		   pmsg.getWhat() == PluginUpdate.UNLOADED) &&
		   plugins != null )
		{
			plugins.updatePopupMenu();
		}
	} 

	
	@EBHandler
	public void handleVFSUpdate(VFSUpdate msg)
	{
		maybeReloadDirectory(msg.getPath());
	} 

	
	public View getView()
	{
		return view;
	} 

	
	public int getMode()
	{
		return mode;
	} 

	
	public boolean isMultipleSelectionEnabled()
	{
		return multipleSelection;
	} 

	
	public boolean isHorizontalLayout()
	{
		return horizontalLayout;
	} 

	
	public boolean getShowHiddenFiles()
	{
		return showHiddenFiles;
	} 

	
	public void setShowHiddenFiles(boolean showHiddenFiles)
	{
		this.showHiddenFiles = showHiddenFiles;
	} 

	
	
	@Deprecated
	public String getFilenameFilter()
	{
		if(filterCheckbox.isSelected())
		{
			String filter = filterField.getSelectedItem().toString();
			if(filter.length() == 0)
				return "*";
			else
				return filter;
		}
		else
			return "*";
	} 

	
	
	public VFSFileFilter getVFSFileFilter()
	{
		if (mode == CHOOSE_DIRECTORY_DIALOG)
			return new DirectoriesOnlyFilter();
		return 	(VFSFileFilter) filterField.getSelectedItem();
	} 

	
	
	public void addVFSFileFilter(VFSFileFilter filter)
	{
		filterField.addItem(filter);
		if (filterField.getItemCount() == 2)
		{
			filterField.setEditor(filterEditor);
			filterField.setEditable(true);

			GridBagLayout layout = (GridBagLayout) pathAndFilterPanel.getLayout();
			GridBagConstraints cons =layout.getConstraints(filterEditor);
			cons.gridwidth = GridBagConstraints.REMAINDER;
			cons.fill = GridBagConstraints.HORIZONTAL;
			cons.gridx = 1;
			cons.weightx = 1;

			pathAndFilterPanel.remove(filterEditor);
			layout.setConstraints(filterField, cons);
			pathAndFilterPanel.add(filterField);
			pathAndFilterPanel.validate();
			pathAndFilterPanel.repaint();
		}
	} 

	
	public void setFilenameFilter(String filter)
	{
		if(filter == null || filter.length() == 0 || "*".equals(filter))
			filterCheckbox.setSelected(false);
		else
		{
			filterCheckbox.setSelected(true);
			filterEditor.setItem(new GlobVFSFileFilter(filter));
		}
	} 

	
	public HistoryTextField getDirectoryField()
	{
		return pathField;
	} 

	
	public String getDirectory()
	{
		return path;
	} 

	
	
	public void previousDirectory() 
	{
		if (historyStack.size() > 1)
		{
			historyStack.pop();
			nextDirectoryStack.push(path);
			setDirectory(historyStack.peek());
			historyStack.pop();
		}
	}
	
	
	
	public void nextDirectory() 
	{
		if (!nextDirectoryStack.isEmpty())
		{
			setDirectory(nextDirectoryStack.pop());
		}
	}
	
	
	
	public void setDirectory(String path)
	{
		if(path.startsWith("file:"))
			path = path.substring(5);
		path = MiscUtilities.expandVariables(path);
		pathField.setText(path);

		if(!startRequest())
			return;

		historyStack.push(path);
		browserView.saveExpansionState();
		browserView.loadDirectory(null,path,true);
		this.path = path;

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				endRequest();
			}
		});
	} 

	
	public static String getRootDirectory()
	{
		if(OperatingSystem.isMacOS() || OperatingSystem.isDOSDerived())
			return FileRootsVFS.PROTOCOL + ':';
		else
			return "/";
	} 

	
	
	public void rootDirectory()
	{
		setDirectory(getRootDirectory());
	} 

	
	public void reloadDirectory()
	{
		
		VFSManager.getVFSForPath(path).reloadDirectory(path);

		browserView.saveExpansionState();
		browserView.loadDirectory(null,path,false);
	} 

	
	
	public void delete(VFSFile[] files)
	{
		String dialogType;

		if(MiscUtilities.isURL(files[0].getDeletePath())
			&& FavoritesVFS.PROTOCOL.equals(
			MiscUtilities.getProtocolOfURL(files[0].getDeletePath())))
		{
			dialogType = "vfs.browser.delete-favorites";
		}
		else
		{
			dialogType = "vfs.browser.delete-confirm";
		}

		StringBuilder buf = new StringBuilder();
		String typeStr = "files";
		for(int i = 0; i < files.length; i++)
		{
			buf.append(files[i].getPath());
			buf.append('\n');
			if (files[i].getType() == VFSFile.DIRECTORY)
				typeStr = "directories and their contents";
		}

		Object[] args = { buf.toString(), typeStr};
		
		int result = GUIUtilities.confirm(this,dialogType,args,
			JOptionPane.YES_NO_OPTION,
			JOptionPane.WARNING_MESSAGE);
		if(result != JOptionPane.YES_OPTION)
			return;

		VFS vfs = VFSManager.getVFSForPath(files[0].getDeletePath());

		if(!startRequest())
			return;

		for(int i = 0; i < files.length; i++)
		{
			Object session = vfs.createVFSSession(files[i].getDeletePath(),this);
			if(session == null)
				continue;

			VFSManager.runInWorkThread(new BrowserIORequest(
				BrowserIORequest.DELETE,this,
				session,vfs,files[i].getDeletePath(),
				null,null));
		}

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				endRequest();
			}
		});
	} 

	
	public void rename(String from)
	{
		VFS vfs = VFSManager.getVFSForPath(from);

		String filename = vfs.getFileName(from);
		String[] args = { filename };
		String to = GUIUtilities.input(this,"vfs.browser.rename",
			args,filename);
		if(to == null)
			return;

		to = MiscUtilities.constructPath(vfs.getParentOfPath(from),to);

		Object session = vfs.createVFSSession(from,this);
		if(session == null)
			return;

		if(!startRequest())
			return;

		VFSManager.runInWorkThread(new BrowserIORequest(
			BrowserIORequest.RENAME,this,
			session,vfs,from,to,null));

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				endRequest();
			}
		});
	} 

	
	public void rename(String from, String newname)
	{
		VFS vfs = VFSManager.getVFSForPath(from);

		String filename = vfs.getFileName(from);
		String to = newname;
		
		if(to == null || filename.equals(newname))
			return;

		to = MiscUtilities.constructPath(vfs.getParentOfPath(from),to);

		Object session = vfs.createVFSSession(from,this);
		if(session == null)
			return;

		if(!startRequest())
			return;

		VFSManager.runInWorkThread(new BrowserIORequest(
			BrowserIORequest.RENAME,this,
			session,vfs,from,to,null));

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				endRequest();
			}
		});
	} 

	
	public void mkdir()
	{
		String newDirectory = GUIUtilities.input(this,"vfs.browser.mkdir",null);
		if(newDirectory == null)
			return;

		
		
		final VFSFile[] selected = getSelectedFiles();
		String parent;
		if(selected.length == 0)
			parent = path;
		else if(selected[0].getType() == VFSFile.FILE)
		{
			parent = selected[0].getPath();
			parent = VFSManager.getVFSForPath(parent)
				.getParentOfPath(parent);
		}
		else
			parent = selected[0].getPath();

		VFS vfs = VFSManager.getVFSForPath(parent);

		
		newDirectory = MiscUtilities.constructPath(parent,newDirectory);

		Object session = vfs.createVFSSession(newDirectory,this);
		if(session == null)
			return;

		if(!startRequest())
			return;

		VFSManager.runInWorkThread(new BrowserIORequest(
			BrowserIORequest.MKDIR,this,
			session,vfs,newDirectory,null,null));

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				endRequest();
				if (selected.length != 0 && selected[0].getType() != VFSFile.FILE)
				{
					VFSDirectoryEntryTable directoryEntryTable = browserView.getTable();
					int selectedRow = directoryEntryTable.getSelectedRow();
					VFSDirectoryEntryTableModel model = (VFSDirectoryEntryTableModel) directoryEntryTable.getModel();
					VFSDirectoryEntryTableModel.Entry entry = model.files[selectedRow];
					if (!entry.expanded)
					{
						browserView.clearExpansionState();
						browserView.loadDirectory(entry,entry.dirEntry.getPath(),
							false);
					}
				}
			}
		});
	} 

	
	
	public void newFile()
	{
		VFSFile[] selected = getSelectedFiles();
		if(selected.length >= 1)
		{
			VFSFile file = selected[0];
			if(file.getType() == VFSFile.DIRECTORY)
				jEdit.newFile(view,file.getPath());
			else
			{
				VFS vfs = VFSManager.getVFSForPath(file.getPath());
				jEdit.newFile(view,vfs.getParentOfPath(file.getPath()));
			}
		}
		else
			jEdit.newFile(view,path);
	} 

	
	
	public void fileProperties(VFSFile[] files)
	{
		new FilePropertiesDialog(view, this, files);
	} 
		
	
	
	public void searchInDirectory()
	{
		VFSFile[] selected = getSelectedFiles();
		if(selected.length >= 1)
		{
			VFSFile file = selected[0];
			searchInDirectory(file.getPath(),file.getType() != VFSFile.FILE);
		}
		else
		{
			searchInDirectory(path,true);
		}
	} 

	
	
	public void searchInDirectory(String path, boolean directory)
	{
		String filter;
		VFSFileFilter vfsff = getVFSFileFilter();
		if (vfsff instanceof GlobVFSFileFilter)
			filter = ((GlobVFSFileFilter)vfsff).getGlob();
		else
			filter = "*";

		if (!directory)
		{
			String name = MiscUtilities.getFileName(path);
			String ext = MiscUtilities.getFileExtension(name);
			filter = ext == null || ext.length() == 0
				? filter : '*' + ext;
			path = MiscUtilities.getParentOfPath(path);
		}

		SearchAndReplace.setSearchFileSet(new DirectoryListSet(
			path,filter,true));
		SearchDialog.showSearchDialog(view,null,SearchDialog.DIRECTORY);
	} 

	
	BrowserView getBrowserView()
	{
		return browserView;
	} 

	
	
	public VFSFile[] getSelectedFiles()
	{
		return browserView.getSelectedFiles();
	} 

	
	public VFSFile[] getSelectedFiles(Component source)
	{
		if(GUIUtilities.getComponentParent(source, BrowserView.ParentDirectoryList.class)
			!= null)
		{
			Object[] selected = getBrowserView()
				.getParentDirectoryList()
				.getSelectedValues();
			VFSFile[] returnValue = new VFSFile[
				selected.length];
			System.arraycopy(selected,0,returnValue,0,
				selected.length);
			return returnValue;
		}
		else
		{
			return getSelectedFiles();
		}
	}

	
	
	public void locateFile(final String path)
	{
		VFSFileFilter filter = getVFSFileFilter();
		if(!filter.accept(MiscUtilities.getFileName(path)))
			setFilenameFilter(null);

		setDirectory(MiscUtilities.getParentOfPath(path));
		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				browserView.getTable().selectFile(path);
			}
		});
	} 

	
	public JComponent createPluginsMenu(JComponent pluginMenu, boolean showManagerOptions)
	{
		ActionHandler actionHandler = new ActionHandler();
		if(showManagerOptions && getMode() == BROWSER)
		{
			pluginMenu.add(GUIUtilities.loadMenuItem("plugin-manager",false));
			pluginMenu.add(GUIUtilities.loadMenuItem("plugin-options",false));
			if (pluginMenu instanceof JMenu)
				((JMenu)pluginMenu).addSeparator();
			else if (pluginMenu instanceof JPopupMenu)
				((JPopupMenu)pluginMenu).addSeparator();

		}
		else
			;

		List<JMenuItem> vec = new ArrayList<JMenuItem>();

		
		Enumeration<VFS> e = VFSManager.getFilesystems();

		while(e.hasMoreElements())
		{
			VFS vfs = e.nextElement();
			if((vfs.getCapabilities() & VFS.BROWSE_CAP) == 0)
				continue;

			JMenuItem menuItem = new JMenuItem(jEdit.getProperty(
					"vfs." + vfs.getName() + ".label"));
			menuItem.setActionCommand(vfs.getName());
			menuItem.addActionListener(actionHandler);
			vec.add(menuItem);
		} 

		
		EditPlugin[] plugins = jEdit.getPlugins();
		for (int i = 0; i < plugins.length; i++)
		{
			JMenuItem menuItem = plugins[i].createBrowserMenuItems();
			if(menuItem != null)
				vec.add(menuItem);
		} 

		if (!vec.isEmpty())
		{
			Collections.sort(vec,new MenuItemTextComparator());
			for(int i = 0; i < vec.size(); i++)
				pluginMenu.add(vec.get(i));
		}
		else
		{
			JMenuItem mi = new JMenuItem(jEdit.getProperty(
					"vfs.browser.plugins.no-plugins.label"));
			mi.setEnabled(false);
			pluginMenu.add(mi);
		}

		return pluginMenu;
	} 

	
	public void addBrowserListener(BrowserListener l)
	{
		listenerList.add(BrowserListener.class,l);
	} 

	
	public void removeBrowserListener(BrowserListener l)
	{
		listenerList.remove(BrowserListener.class,l);
	} 

	
	
	public static final int M_OPEN = 0;
	public static final int M_OPEN_NEW_VIEW = 1;
	public static final int M_OPEN_NEW_PLAIN_VIEW = 2;
	public static final int M_OPEN_NEW_SPLIT = 3;
	public static final int M_INSERT = 4;

	
	public void filesActivated(int mode, boolean canDoubleClickClose)
	{
		VFSFile[] selectedFiles = browserView.getSelectedFiles();

		Buffer buffer = null;

check_selected: for(int i = 0; i < selectedFiles.length; i++)
		{
			VFSFile file = selectedFiles[i];

			if(file.getType() == VFSFile.DIRECTORY
				|| file.getType() == VFSFile.FILESYSTEM)
			{
				if(mode == M_OPEN_NEW_VIEW && this.mode == BROWSER)
					browseDirectoryInNewWindow(view,file.getPath());
				else
					setDirectory(file.getPath());
			}
			else if(this.mode == BROWSER || this.mode == BROWSER_DIALOG)
			{
				if(mode == M_INSERT)
				{
					view.getBuffer().insertFile(view,
						file.getPath());
					continue check_selected;
				}

				Buffer _buffer = jEdit.getBuffer(file.getPath());
				if(_buffer == null)
				{
					Hashtable<String, Object> props = new Hashtable<String, Object>();
					props.put(JEditBuffer.ENCODING,currentEncoding);
					props.put(Buffer.ENCODING_AUTODETECT,
						autoDetectEncoding);
					_buffer = jEdit.openFile(view, null,
						file.getPath(),false,props);
				}
				else if(doubleClickClose && canDoubleClickClose
					&& this.mode != BROWSER_DIALOG
					&& selectedFiles.length == 1)
				{
					
					
					EditPane[] editPanes = view.getEditPanes();
					for(int j = 0; j < editPanes.length; j++)
					{
						if(editPanes[j].getBuffer() == _buffer)
						{
							jEdit.closeBuffer(view,_buffer);
							return;
						}
					}
				}

				if(_buffer != null)
					buffer = _buffer;
			}
			else
			{
				
				
				
			}
		}

		if(buffer != null)
		{
			switch(mode)
			{
			case M_OPEN:
				view.setBuffer(buffer);
				break;
			case M_OPEN_NEW_VIEW:
				jEdit.newView(view,buffer,false);
				break;
			case M_OPEN_NEW_PLAIN_VIEW:
				jEdit.newView(view,buffer,true);
				break;
			case M_OPEN_NEW_SPLIT:
				view.splitHorizontally().setBuffer(buffer);
				break;
			}
		}

		Object[] listeners = listenerList.getListenerList();
		for(int i = 0; i < listeners.length; i++)
		{
			if(listeners[i] == BrowserListener.class)
			{
				BrowserListener l = (BrowserListener)listeners[i+1];
				l.filesActivated(this,selectedFiles);
			}
		}
	} 


	
	public void move(String newPosition)
	{
		boolean horz = mode != BROWSER
				|| DockableWindowManager.TOP.equals(newPosition)
				|| DockableWindowManager.BOTTOM.equals(newPosition);
		if (horz == horizontalLayout)
			return;
		horizontalLayout = horz;
		topBox.remove(toolbarBox);
		toolbarBox = new Box(horizontalLayout
				? BoxLayout.X_AXIS
				: BoxLayout.Y_AXIS);
		topBox.add(toolbarBox, 0);
		propertiesChanged();
	} 
	
	
	String currentEncoding;
	boolean autoDetectEncoding;

	
	void directoryLoaded(Object node, Object[] loadInfo,
		boolean addToHistory)
	{
		VFSManager.runInAWTThread(new DirectoryLoadedAWTRequest(
			node,loadInfo,addToHistory));
	} 

	
	void filesSelected()
	{
		VFSFile[] selectedFiles = browserView.getSelectedFiles();

		if(mode == BROWSER)
		{
			for(int i = 0; i < selectedFiles.length; i++)
			{
				VFSFile file = selectedFiles[i];
				Buffer buffer = jEdit.getBuffer(file.getPath());
				if(buffer != null && view != null)
					view.setBuffer(buffer);
			}
		}

		Object[] listeners = listenerList.getListenerList();
		for(int i = 0; i < listeners.length; i++)
		{
			if(listeners[i] == BrowserListener.class)
			{
				BrowserListener l = (BrowserListener)listeners[i+1];
				l.filesSelected(this,selectedFiles);
			}
		}
	} 

	
	void endRequest()
	{
		requestRunning = false;
	} 

	

	

	private static final ActionContext actionContext;

	static
	{
		actionContext = new BrowserActionContext();

		ActionSet builtInActionSet = new ActionSet(null,null,null,
			jEdit.class.getResource("browser.actions.xml"));
		builtInActionSet.setLabel(jEdit.getProperty("action-set.browser"));
		builtInActionSet.load();
		actionContext.addActionSet(builtInActionSet);
	}

	
	private EventListenerList listenerList;
	private View view;
	private boolean horizontalLayout;
	private String path;
	private JPanel pathAndFilterPanel;
	private HistoryTextField pathField;
	private JComponent defaultFocusComponent;
	private JCheckBox filterCheckbox;
	private HistoryComboBoxEditor filterEditor;
	private JComboBox filterField;
	private Box toolbarBox;
	private Box topBox;
	private FavoritesMenuButton favorites;
	private PluginsMenuButton plugins;
	private BrowserView browserView;
	private int mode;
	private boolean multipleSelection;

	private boolean showHiddenFiles;
	private boolean sortMixFilesAndDirs;
	private boolean sortIgnoreCase;
	private boolean doubleClickClose;

	private boolean requestRunning;
	private boolean maybeReloadRequestRunning;
	
	private final Stack<String> historyStack = new Stack<String>();
	private final Stack<String> nextDirectoryStack = new Stack<String>();
	

	
	private Container createMenuBar()
	{
		JToolBar menuBar = new JToolBar();
		menuBar.setFloatable(false);

		menuBar.add(new CommandsMenuButton());
		menuBar.add(Box.createHorizontalStrut(3));
		menuBar.add(plugins = new PluginsMenuButton());
		menuBar.add(Box.createHorizontalStrut(3));
		menuBar.add(favorites = new FavoritesMenuButton());

		return menuBar;
	} 

	
	private Container createToolBar()
	{
		if(mode == BROWSER)
			return GUIUtilities.loadToolBar(actionContext,
				"vfs.browser.toolbar-browser");
		else
			return GUIUtilities.loadToolBar(actionContext,
				"vfs.browser.toolbar-dialog");
	} 

	
	private void propertiesChanged()
	{
		showHiddenFiles = jEdit.getBooleanProperty("vfs.browser.showHiddenFiles");
		sortMixFilesAndDirs = jEdit.getBooleanProperty("vfs.browser.sortMixFilesAndDirs");
		sortIgnoreCase = jEdit.getBooleanProperty("vfs.browser.sortIgnoreCase");
		doubleClickClose = jEdit.getBooleanProperty("vfs.browser.doubleClickClose");

		browserView.propertiesChanged();

		toolbarBox.removeAll();

		if(jEdit.getBooleanProperty("vfs.browser.showToolbar"))
		{
			Container toolbar = createToolBar();
			toolbarBox.add(toolbar);
		}

		if(jEdit.getBooleanProperty("vfs.browser.showMenubar"))
		{
			Container menubar = createMenuBar();
			if(horizontalLayout)
			{
				toolbarBox.add(menubar,0);
			}
			else
			{
				menubar.add(Box.createGlue());
				toolbarBox.add(menubar);
			}
		}
		else
		{
			plugins = null;
			favorites = null;
		}

		revalidate();

		if(path != null)
			reloadDirectory();
	} 

	

	
	private boolean startRequest()
	{
		if(requestRunning)
		{
			
			Log.log(Log.DEBUG,this,new Throwable("For debugging purposes"));

			GUIUtilities.error(this,"browser-multiple-io",null);
			return false;
		}
		else
		{
			requestRunning = true;
			return true;
		}
	} 

	
	private void updateFilterEnabled()
	{
		filterField.setEnabled(filterCheckbox.isSelected());
		filterEditor.setEnabled(filterCheckbox.isSelected());
	} 

	
	private void maybeReloadDirectory(String dir)
	{
		if(MiscUtilities.isURL(dir)
			&& MiscUtilities.getProtocolOfURL(dir).equals(
			FavoritesVFS.PROTOCOL))
		{
			if(favorites != null)
				favorites.popup = null;
		}

		
		
		
		
		
		
		
		
		if(maybeReloadRequestRunning)
		{
			
			return;
		}

		
		
		
		
		if(path != null)
		{
			try
			{
				maybeReloadRequestRunning = true;

				browserView.maybeReloadDirectory(dir);
			}
			finally
			{
				VFSManager.runInAWTThread(new Runnable()
				{
					public void run()
					{
						maybeReloadRequestRunning = false;
					}
				});
			}
		}
	} 

	

	

	
	class ActionHandler implements ActionListener, ItemListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if (isProcessingEvent)
				return;

			Object source = evt.getSource();

			if (source == pathField
			    || source == filterCheckbox)
			{
				isProcessingEvent = true;
				resetLater();

				updateFilterEnabled();

				String p = pathField.getText();
				
				if(p != null)
					setDirectory(p);
				browserView.focusOnFileView();
			}

			else if (source == filterField.getEditor())
			{
				
				filterField.getEditor().setItem(
					filterField.getEditor().getItem());
			}

			
			
			else if (source == filterEditor)
			{
				
				filterEditor.setItem(
					filterEditor.getItem());
				filterField.setSelectedItem(
					filterEditor.getItem());
				
				
				itemStateChanged(new ItemEvent(filterField,
					ItemEvent.ITEM_STATE_CHANGED,
					filterEditor.getItem(),
					ItemEvent.SELECTED));
			}
		}

		public void itemStateChanged(ItemEvent e)
		{
			if (isProcessingEvent)
				return;

			if (e.getStateChange() != ItemEvent.SELECTED)
				return;

			isProcessingEvent = true;
			resetLater();

			filterField.setEditable(e.getItem() instanceof GlobVFSFileFilter);
			updateFilterEnabled();
			String path = pathField.getText();
			if(path != null)
				setDirectory(path);

			browserView.focusOnFileView();
		}

		
		private void resetLater()
		{
			SwingUtilities.invokeLater(
				new Runnable()
				{
					public void run()
					{
						isProcessingEvent = false;
					}
				}
			);
		}

		private boolean isProcessingEvent;

	} 

	
	class CommandsMenuButton extends RolloverButton
	{
		
		CommandsMenuButton()
		{
			setText(jEdit.getProperty("vfs.browser.commands.label"));
			setIcon(GUIUtilities.loadIcon(jEdit.getProperty("dropdown-arrow.icon")));
			setHorizontalTextPosition(SwingConstants.LEADING);
			setName("commands");
			
			popup = new BrowserCommandsMenu(VFSBrowser.this,null);

			CommandsMenuButton.this.setRequestFocusEnabled(false);
			setMargin(new Insets(1,1,1,1));
			CommandsMenuButton.this.addMouseListener(new MouseHandler());

			if(OperatingSystem.isMacOSLF())
				CommandsMenuButton.this.putClientProperty("JButton.buttonType","toolbar");
		} 

		BrowserCommandsMenu popup;

		
		class MouseHandler extends MouseAdapter
		{
			@Override
			public void mousePressed(MouseEvent evt)
			{
				if(!popup.isVisible())
				{
					popup.update();

					GUIUtilities.showPopupMenu(
						popup,CommandsMenuButton.this,0,
						CommandsMenuButton.this.getHeight(),
						false);
				}
				else
				{
					popup.setVisible(false);
				}
			}
		} 
	} 

	
	class PluginsMenuButton extends RolloverButton
	{
		
		PluginsMenuButton()
		{
			setText(jEdit.getProperty("vfs.browser.plugins.label"));
			setIcon(GUIUtilities.loadIcon(jEdit.getProperty("dropdown-arrow.icon")));
			setHorizontalTextPosition(SwingConstants.LEADING);
			setName("plugins");
			
			PluginsMenuButton.this.setRequestFocusEnabled(false);
			setMargin(new Insets(1,1,1,1));
			PluginsMenuButton.this.addMouseListener(new MouseHandler());

			if(OperatingSystem.isMacOSLF())
				PluginsMenuButton.this.putClientProperty("JButton.buttonType","toolbar");
		} 

		JPopupMenu popup;

		
		void updatePopupMenu()
		{
			popup = null;
		} 

		
		private void createPopupMenu()
		{
			if(popup != null)
				return;

			popup = (JPopupMenu)createPluginsMenu(new JPopupMenu(),true);
		} 

		
		class MouseHandler extends MouseAdapter
		{
			@Override
			public void mousePressed(MouseEvent evt)
			{
				createPopupMenu();

				if(!popup.isVisible())
				{
					GUIUtilities.showPopupMenu(
						popup,PluginsMenuButton.this,0,
						PluginsMenuButton.this.getHeight(),
						false);
				}
				else
				{
					popup.setVisible(false);
				}
			}
		} 
	} 

	
	class FavoritesMenuButton extends RolloverButton
	{
		
		FavoritesMenuButton()
		{
			setText(jEdit.getProperty("vfs.browser.favorites.label"));
			setIcon(GUIUtilities.loadIcon(jEdit.getProperty("dropdown-arrow.icon")));
			setHorizontalTextPosition(SwingConstants.LEADING);
			setName("favorites");
			
			FavoritesMenuButton.this.setRequestFocusEnabled(false);
			setMargin(new Insets(1,1,1,1));
			FavoritesMenuButton.this.addMouseListener(new MouseHandler());

			if(OperatingSystem.isMacOSLF())
				FavoritesMenuButton.this.putClientProperty("JButton.buttonType","toolbar");
		} 

		JPopupMenu popup;

		
		void createPopupMenu()
		{
			popup = new JPopupMenu();
			ActionHandler actionHandler = new ActionHandler();

			JMenuItem mi = new JMenuItem(
				jEdit.getProperty(
				"vfs.browser.favorites"
				+ ".add-to-favorites.label"));
			mi.setActionCommand("add-to-favorites");
			mi.addActionListener(actionHandler);
			popup.add(mi);

			mi = new JMenuItem(
				jEdit.getProperty(
				"vfs.browser.favorites"
				+ ".edit-favorites.label"));
			mi.setActionCommand("dir@favorites:");
			mi.addActionListener(actionHandler);
			popup.add(mi);

			popup.addSeparator();

			VFSFile[] favorites = FavoritesVFS.getFavorites();
			if(favorites.length == 0)
			{
				mi = new JMenuItem(
					jEdit.getProperty(
					"vfs.browser.favorites"
					+ ".no-favorites.label"));
				mi.setEnabled(false);
				popup.add(mi);
			}
			else
			{
				Arrays.sort(favorites,
					new VFS.DirectoryEntryCompare(
					sortMixFilesAndDirs,
					sortIgnoreCase));
				for(int i = 0; i < favorites.length; i++)
				{
					VFSFile favorite = favorites[i];
					mi = new JMenuItem(favorite.getPath());
					mi.setIcon(FileCellRenderer
						.getIconForFile(
						favorite,false));
					String cmd = (favorite.getType() ==
						VFSFile.FILE
						? "file@" : "dir@")
						+ favorite.getPath();
					mi.setActionCommand(cmd);
					mi.addActionListener(actionHandler);
					popup.add(mi);
				}
			}
		} 

		
		class ActionHandler implements ActionListener
		{
			public void actionPerformed(ActionEvent evt)
			{
				String actionCommand = evt.getActionCommand();
				if("add-to-favorites".equals(actionCommand))
				{
					
					
					VFSFile[] selected = getSelectedFiles();
					if(selected == null || selected.length == 0)
					{
						if(path.equals(FavoritesVFS.PROTOCOL + ':'))
						{
							GUIUtilities.error(VFSBrowser.this,
								"vfs.browser.recurse-favorites",
								null);
						}
						else
						{
							FavoritesVFS.addToFavorites(path,
								VFSFile.DIRECTORY);
						}
					}
					else
					{
						for(int i = 0; i < selected.length; i++)
						{
							VFSFile file = selected[i];
							FavoritesVFS.addToFavorites(file.getPath(),
								file.getType());
						}
					}
				}
				else if(actionCommand.startsWith("dir@"))
				{
					setDirectory(actionCommand.substring(4));
				}
				else if(actionCommand.startsWith("file@"))
				{
					switch(getMode())
					{
					case BROWSER:
						jEdit.openFile(view,actionCommand.substring(5));
						break;
					default:
						locateFile(actionCommand.substring(5));
						break;
					}
				}
			}
		} 

		
		class MouseHandler extends MouseAdapter
		{
			@Override
			public void mousePressed(MouseEvent evt)
			{
				if(popup != null && popup.isVisible())
				{
					popup.setVisible(false);
					return;
				}

				if(popup == null)
					createPopupMenu();

				GUIUtilities.showPopupMenu(
					popup,FavoritesMenuButton.this,0,
					FavoritesMenuButton.this.getHeight(),
					false);
			}
		} 
	} 

	
	class DirectoryLoadedAWTRequest implements Runnable
	{
		private final Object node;
		private final Object[] loadInfo;
		private final boolean addToHistory;

		DirectoryLoadedAWTRequest(Object node, Object[] loadInfo,
			boolean addToHistory)
		{
			this.node = node;
			this.loadInfo = loadInfo;
			this.addToHistory = addToHistory;
		}

		public void run()
		{
			String path = (String)loadInfo[0];
			if(path == null)
			{
				
				return;
			}

			VFSFile[] list = (VFSFile[])loadInfo[1];

			if(node == null)
			{
				
				VFSBrowser.this.path = path;
				if(!pathField.getText().equals(path))
					pathField.setText(path);
				if(path.endsWith("/") ||
					path.endsWith(File.separator))
				{
					
					
					
					path = path.substring(0,
						path.length() - 1);
				}

				if(addToHistory)
				{
					HistoryModel.getModel("vfs.browser.path")
						.addItem(path);
				}
			}

			boolean filterEnabled = filterCheckbox.isSelected();

			List<VFSFile> directoryList = new ArrayList<VFSFile>();

			int directories = 0;
			int files = 0;
			int invisible = 0;

			if(list != null)
			{
				VFSFileFilter filter = getVFSFileFilter();

				for(int i = 0; i < list.length; i++)
				{
					VFSFile file = list[i];
					if(file.isHidden() && !showHiddenFiles)
					{
						invisible++;
						continue;
					}

					if (filter != null && (filterEnabled || filter instanceof DirectoriesOnlyFilter)
					    && !filter.accept(file))
					{
						invisible++;
						continue;
					}

					if(file.getType() == VFSFile.FILE)
						files++;
					else
						directories++;

					directoryList.add(file);
				}

				Collections.sort(directoryList,
					new VFS.DirectoryEntryCompare(
					sortMixFilesAndDirs,
					sortIgnoreCase));
			}

			browserView.directoryLoaded(node,path,
				directoryList);

			
			

			
			
			

			
			
			
			if(mode == CHOOSE_DIRECTORY_DIALOG)
				filesSelected();
		}

		@Override
		public String toString()
		{
			return (String)loadInfo[0];
		}
	} 

	
	static class BrowserActionContext extends ActionContext
	{
		@Override
		public void invokeAction(EventObject evt, EditAction action)
		{
			Component source = (Component)evt.getSource();
			VFSBrowser browser = (VFSBrowser)
				GUIUtilities.getComponentParent(
				source,
				VFSBrowser.class);

			VFSFile[] files = browser.getSelectedFiles(source);

			
			
			

			
			
			
			NameSpace global = BeanShell.getNameSpace();
			try
			{
				global.setVariable("browser",browser);
				global.setVariable("files",files);

				View view = browser.getView();
				
				
				
				
				
				
				
				if(view == null)
					view = jEdit.getActiveView();
				action.invoke(view);
			}
			catch(UtilEvalError err)
			{
				Log.log(Log.ERROR,this,err);
			}
			finally
			{
				try
				{
					global.setVariable("browser",null);
					global.setVariable("files",null);
				}
				catch(UtilEvalError err)
				{
					Log.log(Log.ERROR,this,err);
				}
			}
		}
	} 

	
	private static class HistoryComboBoxEditor
				extends HistoryTextField
				implements ComboBoxEditor
	{

		HistoryComboBoxEditor(String key)
		{
			super(key);
		}

		public Object getItem()
		{
			if (current == null)
			{
				current = new GlobVFSFileFilter(getText());
			}

			if (!current.getGlob().equals(getText()))
			{
				current.setGlob(getText());
			}

			return current;
		}

		public void setItem(Object item)
		{
			if (item == current)
			{
				
				
				
				
				
				if (item != null)
				{
					GlobVFSFileFilter filter = (GlobVFSFileFilter) item;
					current = new GlobVFSFileFilter(filter.getGlob());
					setText(current.getGlob());
				}
				return;
			}

			if (item != null)
			{
				
				
				
				
				if (!(item instanceof GlobVFSFileFilter))
					return;

				GlobVFSFileFilter filter = (GlobVFSFileFilter) item;
				filter = new GlobVFSFileFilter(filter.getGlob());
				setText(filter.getGlob());
				addCurrentToHistory();
				current = filter;
			}
			else
			{
				setText("*");
				current = new GlobVFSFileFilter("*");
			}
		}

		@Override
		protected void processFocusEvent(FocusEvent e)
		{
			
			
			
			if (e.getID() != FocusEvent.FOCUS_LOST)
				super.processFocusEvent(e);
			else 
			{
				setCaretPosition(0);
				getCaret().setVisible(false);
			}
		}

		public Component getEditorComponent()
		{
			return this;
		}

		private GlobVFSFileFilter current;

	} 

	
	private static class VFSFileFilterRenderer extends DefaultListCellRenderer
	{

		@Override
		public Component getListCellRendererComponent(JList list,
			Object value, int index, boolean isSelected,
			boolean cellHasFocus)
		{
			assert value instanceof VFSFileFilter : "Filter is not a VFSFileFilter";
			super.getListCellRendererComponent(
				list, value, index, isSelected, cellHasFocus);
			setText(((VFSFileFilter)value).getDescription());
			return this;
		}

	} 

	
	public static class DirectoriesOnlyFilter implements VFSFileFilter
	{

		public boolean accept(VFSFile file)
		{
			return file.getType() == VFSFile.DIRECTORY
				|| file.getType() == VFSFile.FILESYSTEM;
		}

		public boolean accept(String url)
		{
			return false;
		}

		public String getDescription()
		{
			return jEdit.getProperty("vfs.browser.file_filter.dir_only");
		}

	} 

	
}
