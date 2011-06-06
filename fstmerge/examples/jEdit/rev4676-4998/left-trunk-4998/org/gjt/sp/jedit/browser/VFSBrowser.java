

package org.gjt.sp.jedit.browser;


import bsh.*;
import gnu.regexp.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.File;
import java.util.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.search.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class VFSBrowser extends JPanel implements EBComponent, DefaultFocusComponent
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
		this.floating = floating;
		this.view = view;

		currentEncoding = jEdit.getProperty("buffer.encoding",
			System.getProperty("file.encoding"));
		autoDetectEncoding = jEdit.getBooleanProperty(
			"buffer.encodingAutodetect");

		ActionHandler actionHandler = new ActionHandler();

		Box topBox = new Box(BoxLayout.Y_AXIS);

		horizontalLayout = (mode != BROWSER
			|| DockableWindowManager.TOP.equals(position)
			|| DockableWindowManager.BOTTOM.equals(position));

		toolbarBox = new Box(horizontalLayout
			? BoxLayout.X_AXIS
			: BoxLayout.Y_AXIS);

		topBox.add(toolbarBox);

		GridBagLayout layout = new GridBagLayout();
		JPanel pathAndFilterPanel = new JPanel(layout);

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
		pathField.setInstantPopups(true);
		pathField.setEnterAddsToHistory(false);
		pathField.setSelectAllOnFocus(true);

		if(floating)
		{
			label.setDisplayedMnemonic(jEdit.getProperty(
				"vfs.browser.path.mnemonic").charAt(0));
			label.setLabelFor(pathField);
		}

		
		
		
		Dimension prefSize = pathField.getPreferredSize();
		prefSize.width = 0;
		pathField.setPreferredSize(prefSize);
		pathField.addActionListener(actionHandler);
		cons.gridx = 1;
		cons.weightx = 1.0f;

		layout.setConstraints(pathField,cons);
		pathAndFilterPanel.add(pathField);

		filterCheckbox = new JCheckBox(jEdit.getProperty("vfs.browser.filter"));
		filterCheckbox.setMargin(new Insets(0,0,0,0));
		filterCheckbox.setRequestFocusEnabled(false);
		filterCheckbox.setBorder(new EmptyBorder(0,0,0,12));
		filterCheckbox.setSelected(jEdit.getBooleanProperty(
			"vfs.browser.filter-enabled"));

		filterCheckbox.addActionListener(actionHandler);

		if(mode != CHOOSE_DIRECTORY_DIALOG)
		{
			cons.gridx = 0;
			cons.weightx = 0.0f;
			cons.gridy = 1;
			layout.setConstraints(filterCheckbox,cons);
			pathAndFilterPanel.add(filterCheckbox);
		}

		filterField = new HistoryTextField("vfs.browser.filter");
		filterField.setInstantPopups(true);
		filterField.setSelectAllOnFocus(true);
		filterField.addActionListener(actionHandler);

		if(mode != CHOOSE_DIRECTORY_DIALOG)
		{
			cons.gridx = 1;
			cons.weightx = 1.0f;
			layout.setConstraints(filterField,cons);
			pathAndFilterPanel.add(filterField);
		}

		topBox.add(pathAndFilterPanel);
		add(BorderLayout.NORTH,topBox);

		add(BorderLayout.CENTER,browserView = new BrowserView(this));

		propertiesChanged();

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
				filter = "*" + ext;
		}

		filterField.setText(filter);
		filterField.addCurrentToHistory();

		updateFilterEnabled();

		
		if(path == null)
			path = jEdit.getProperty("vfs.browser.path.tmp");

		if(path == null || path.length() == 0)
		{
			String userHome = System.getProperty("user.home");
			String defaultPath = jEdit.getProperty("vfs.browser.defaultPath");
			if(defaultPath.equals("home"))
				path = userHome;
			else if(defaultPath.equals("working"))
				path = System.getProperty("user.dir");
			else if(defaultPath.equals("buffer"))
			{
				if(view != null)
				{
					Buffer buffer = view.getBuffer();
					path = buffer.getDirectory();
				}
				else
					path = userHome;
			}
			else if(defaultPath.equals("last"))
			{
				HistoryModel pathModel = HistoryModel.getModel("vfs.browser.path");
				if(pathModel.getSize() == 0)
					path = "~";
				else
					path = pathModel.getItem(0);
			}
			else if(defaultPath.equals("favorites"))
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
		browserView.focusOnFileView();
	} 

	
	public void addNotify()
	{
		super.addNotify();
		EditBus.addToBus(this);
	} 

	
	public void removeNotify()
	{
		super.removeNotify();
		jEdit.setBooleanProperty("vfs.browser.filter-enabled",
			filterCheckbox.isSelected());
		if(mode == BROWSER || !jEdit.getBooleanProperty(
			"vfs.browser.currentBufferFilter"))
		{
			jEdit.setProperty("vfs.browser.last-filter",
				filterField.getText());
		}
		EditBus.removeFromBus(this);
	} 

	
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof PropertiesChanged)
			propertiesChanged();
		else if(msg instanceof BufferUpdate)
		{
			BufferUpdate bmsg = (BufferUpdate)msg;
			if(bmsg.getWhat() == BufferUpdate.CREATED
				|| bmsg.getWhat() == BufferUpdate.CLOSED)
				browserView.updateFileView();
			
			
			
		}
		else if(msg instanceof PluginUpdate)
		{
			PluginUpdate pmsg = (PluginUpdate)msg;
			if(pmsg.getWhat() == PluginUpdate.LOADED
				|| pmsg.getWhat() == PluginUpdate.UNLOADED)
			{
				plugins.updatePopupMenu();
			}
		}
		else if(msg instanceof VFSUpdate)
		{
			maybeReloadDirectory(((VFSUpdate)msg).getPath());
		}
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

	
	
	public String getFilenameFilter()
	{
		if(filterCheckbox.isSelected())
		{
			String filter = filterField.getText();
			if(filter.length() == 0)
				return "*";
			else
				return filter;
		}
		else
			return "*";
	} 

	
	public void setFilenameFilter(String filter)
	{
		if(filter == null || filter.length() == 0 || filter.equals("*"))
			filterCheckbox.setSelected(false);
		else
		{
			filterCheckbox.setSelected(true);
			filterField.setText(filter);
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

	
	public void setDirectory(String path)
	{
		if(path.startsWith("file:"))
			path = path.substring(5);

		pathField.setText(path);

		if(!startRequest())
			return;

		updateFilenameFilter();
		browserView.saveExpansionState();
		browserView.loadDirectory(null,path);
		this.path = path;

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				endRequest();
			}
		});
	} 

	
	
	public void rootDirectory()
	{
		if(OperatingSystem.isMacOS() || OperatingSystem.isDOSDerived())
			setDirectory(FileRootsVFS.PROTOCOL + ":");
		else
			setDirectory("/");
	} 

	
	public void reloadDirectory()
	{
		
		VFSManager.getVFSForPath(path).reloadDirectory(path);

		updateFilenameFilter();
		browserView.saveExpansionState();
		browserView.loadDirectory(null,path);
	} 

	
	
	public void delete(VFS.DirectoryEntry[] files)
	{
		String dialogType;

		if(MiscUtilities.isURL(files[0].deletePath)
			&& FavoritesVFS.PROTOCOL.equals(
			MiscUtilities.getProtocolOfURL(files[0].deletePath)))
		{
			dialogType = "vfs.browser.delete-favorites";
		}
		else
		{
			dialogType = "vfs.browser.delete-confirm";
		}

		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < files.length; i++)
		{
			buf.append(files[i].path);
			buf.append('\n');
		}

		Object[] args = { buf.toString() };
		int result = GUIUtilities.confirm(this,dialogType,args,
			JOptionPane.YES_NO_OPTION,
			JOptionPane.WARNING_MESSAGE);
		if(result != JOptionPane.YES_OPTION)
			return;

		VFS vfs = VFSManager.getVFSForPath(files[0].deletePath);

		if(!startRequest())
			return;

		for(int i = 0; i < files.length; i++)
		{
			Object session = vfs.createVFSSession(files[i].deletePath,this);
			if(session == null)
				continue;

			VFSManager.runInWorkThread(new BrowserIORequest(
				BrowserIORequest.DELETE,this,
				session,vfs,files[i].deletePath,
				null,null,null));
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
			session,vfs,from,to,null,null));

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

		
		
		VFS.DirectoryEntry[] selected = getSelectedFiles();
		String parent;
		if(selected.length == 0)
			parent = path;
		else if(selected[0].type == VFS.DirectoryEntry.FILE)
		{
			parent = selected[0].path;
			parent = VFSManager.getVFSForPath(parent)
				.getParentOfPath(parent);
		}
		else
			parent = selected[0].path;

		VFS vfs = VFSManager.getVFSForPath(parent);

		
		newDirectory = MiscUtilities.constructPath(parent,newDirectory);

		Object session = vfs.createVFSSession(newDirectory,this);
		if(session == null)
			return;

		if(!startRequest())
			return;

		VFSManager.runInWorkThread(new BrowserIORequest(
			BrowserIORequest.MKDIR,this,
			session,vfs,newDirectory,null,null,null));

		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				endRequest();
			}
		});
	} 

	
	
	public void newFile()
	{
		VFS.DirectoryEntry[] selected = getSelectedFiles();
		if(selected.length >= 1)
		{
			VFS.DirectoryEntry file = selected[0];
			if(file.type == VFS.DirectoryEntry.DIRECTORY)
				jEdit.newFile(view,file.path);
			else
			{
				VFS vfs = VFSManager.getVFSForPath(file.path);
				jEdit.newFile(view,vfs.getParentOfPath(file.path));
			}
		}
		else
			jEdit.newFile(view,path);
	} 

	
	
	public void searchInDirectory()
	{
		VFS.DirectoryEntry[] selected = getSelectedFiles();
		if(selected.length >= 1)
		{
			VFS.DirectoryEntry file = selected[0];
			searchInDirectory(file.path,file.type != VFS.DirectoryEntry.FILE);
		}
		else
		{
			searchInDirectory(this.path,true);
		}
	} 

	
	
	public void searchInDirectory(String path, boolean directory)
	{
		String filter;

		if(directory)
		{
			filter = getFilenameFilter();
		}
		else
		{
			String name = MiscUtilities.getFileName(path);
			String ext = MiscUtilities.getFileExtension(name);
			filter = (ext == null || ext.length() == 0
				? getFilenameFilter()
				: "*" + ext);
			path = MiscUtilities.getParentOfPath(path);
		}

		SearchAndReplace.setSearchFileSet(new DirectoryListSet(
			path,filter,true));
		SearchDialog.showSearchDialog(view,null,SearchDialog.DIRECTORY);
	} 

	
	public BrowserView getBrowserView()
	{
		return browserView;
	} 

	
	public VFS.DirectoryEntry[] getSelectedFiles()
	{
		return browserView.getSelectedFiles();
	} 

	
	
	public void locateFile(final String path)
	{
		if(!filenameFilter.isMatch(MiscUtilities.getFileName(path)))
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
		VFS.DirectoryEntry[] selectedFiles = browserView.getSelectedFiles();

		Buffer buffer = null;

check_selected: for(int i = 0; i < selectedFiles.length; i++)
		{
			VFS.DirectoryEntry file = selectedFiles[i];

			if(file.type == VFS.DirectoryEntry.DIRECTORY
				|| file.type == VFS.DirectoryEntry.FILESYSTEM)
			{
				if(mode == M_OPEN_NEW_VIEW && this.mode == BROWSER)
					browseDirectoryInNewWindow(view,file.path);
				else
					setDirectory(file.path);
			}
			else if(this.mode == BROWSER || this.mode == BROWSER_DIALOG)
			{
				if(mode == M_INSERT)
				{
					view.getBuffer().insertFile(view,
						file.path);
					continue check_selected;
				}

				Buffer _buffer = jEdit.getBuffer(file.path);
				if(_buffer == null)
				{
					Hashtable props = new Hashtable();
					props.put(Buffer.ENCODING,currentEncoding);
					props.put(Buffer.ENCODING_AUTODETECT,
						new Boolean(autoDetectEncoding));
					_buffer = jEdit.openFile(null,null,
						file.path,false,props);
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

	
	String currentEncoding;
	boolean autoDetectEncoding;

	
	
	static boolean pathsEqual(String p1, String p2)
	{
		if(p1.endsWith("/") || p1.endsWith(File.separator))
			p1 = p1.substring(0,p1.length() - 1);
		if(p2.endsWith("/") || p2.endsWith(File.separator))
			p2 = p2.substring(0,p2.length() - 1);
		return p1.equals(p2);
	} 

	
	void updateFilenameFilter()
	{
		try
		{
			String filter = filterField.getText();
			if(filter.length() == 0)
				filter = "*";
			filenameFilter = new RE(MiscUtilities.globToRE(filter),RE.REG_ICASE);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,VFSBrowser.this,e);
			String[] args = { filterField.getText(),
				e.getMessage() };
			GUIUtilities.error(this,"vfs.browser.bad-filter",args);
		}
	} 

	
	void directoryLoaded(Object node, Object[] loadInfo)
	{
		VFSManager.runInAWTThread(new DirectoryLoadedAWTRequest(
			node,loadInfo));
	} 

	
	void filesSelected()
	{
		VFS.DirectoryEntry[] selectedFiles = browserView.getSelectedFiles();

		if(mode == BROWSER)
		{
			for(int i = 0; i < selectedFiles.length; i++)
			{
				VFS.DirectoryEntry file = selectedFiles[i];
				Buffer buffer = jEdit.getBuffer(file.path);
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

	

	

	private static ActionContext actionContext;

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
	private boolean floating;
	private boolean horizontalLayout;
	private String path;
	private HistoryTextField pathField;
	private JCheckBox filterCheckbox;
	private HistoryTextField filterField;
	private Box toolbarBox;
	private FavoritesMenuButton favorites;
	private PluginsMenuButton plugins;
	private BrowserView browserView;
	private RE filenameFilter;
	private int mode;
	private boolean multipleSelection;

	private boolean showHiddenFiles;
	private boolean sortMixFilesAndDirs;
	private boolean sortIgnoreCase;
	private boolean doubleClickClose;

	private boolean requestRunning;
	private boolean maybeReloadRequestRunning;
	

	
	private JPanel createMenuBar()
	{
		JPanel menuBar = new JPanel();
		menuBar.setLayout(new BoxLayout(menuBar,BoxLayout.X_AXIS));
		menuBar.setBorder(new EmptyBorder(0,3,1,0));

		menuBar.add(new CommandsMenuButton());
		menuBar.add(Box.createHorizontalStrut(3));
		menuBar.add(plugins = new PluginsMenuButton());
		menuBar.add(Box.createHorizontalStrut(3));
		menuBar.add(favorites = new FavoritesMenuButton());

		return menuBar;
	} 

	
	private Box createToolBar()
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
			Box toolbar = createToolBar();
			if(horizontalLayout)
				toolbarBox.add(toolbar);
			else
			{
				toolbar.add(Box.createGlue());
				toolbarBox.add(toolbar);
			}
		}

		if(jEdit.getBooleanProperty("vfs.browser.showMenubar"))
		{
			JPanel menubar = createMenuBar();
			if(horizontalLayout)
			{
				toolbarBox.add(Box.createHorizontalStrut(6));
				toolbarBox.add(menubar,0);
			}
			else
			{
				menubar.add(Box.createGlue());
				toolbarBox.add(menubar);
			}
		}
		else
			favorites = null;

		toolbarBox.add(Box.createGlue());

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

	

	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == pathField || source == filterField
				|| source == filterCheckbox)
			{
				updateFilterEnabled();

				String path = pathField.getText();
				if(path != null)
					setDirectory(path);

				browserView.focusOnFileView();
			}
		}
	} 

	
	class CommandsMenuButton extends JButton
	{
		
		CommandsMenuButton()
		{
			setText(jEdit.getProperty("vfs.browser.commands.label"));
			setIcon(GUIUtilities.loadIcon("ToolbarMenu.gif"));
			setHorizontalTextPosition(SwingConstants.LEADING);

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

	
	class PluginsMenuButton extends JButton
	{
		
		PluginsMenuButton()
		{
			setText(jEdit.getProperty("vfs.browser.plugins.label"));
			setIcon(GUIUtilities.loadIcon("ToolbarMenu.gif"));
			setHorizontalTextPosition(SwingConstants.LEADING);

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

			popup = new JPopupMenu();
			ActionHandler actionHandler = new ActionHandler();

			if(getMode() == BROWSER)
			{
				popup.add(GUIUtilities.loadMenuItem("plugin-manager",false));
				popup.add(GUIUtilities.loadMenuItem("plugin-options",false));
				popup.addSeparator();
			}
			else
				;

			ArrayList vec = new ArrayList();

			
			Enumeration e = VFSManager.getFilesystems();

			while(e.hasMoreElements())
			{
				VFS vfs = (VFS)e.nextElement();
				if((vfs.getCapabilities() & VFS.BROWSE_CAP) == 0)
					continue;

				JMenuItem menuItem = new JMenuItem(jEdit.getProperty(
					"vfs." + vfs.getName() + ".label"));
				menuItem.setActionCommand(vfs.getName());
				menuItem.addActionListener(actionHandler);
				vec.add(menuItem);
			} 

			
			EditPlugin[] plugins = jEdit.getPlugins();
			for(int i = 0; i < plugins.length; i++)
			{
				JMenuItem menuItem = plugins[i].createBrowserMenuItems();
				if(menuItem != null)
					vec.add(menuItem);
			} 

			if(vec.size() != 0)
			{
				MiscUtilities.quicksort(vec,new MiscUtilities.MenuItemCompare());
				for(int i = 0; i < vec.size(); i++)
					popup.add((JMenuItem)vec.get(i));
			}
			else
			{
				JMenuItem mi = new JMenuItem(jEdit.getProperty(
					"vfs.browser.plugins.no-plugins.label"));
				mi.setEnabled(false);
				popup.add(mi);
			}
		} 

		
		class ActionHandler implements ActionListener
		{
			public void actionPerformed(ActionEvent evt)
			{
				VFS vfs = VFSManager.getVFSByName(evt.getActionCommand());
				String directory = vfs.showBrowseDialog(null,
					VFSBrowser.this);
				if(directory != null)
					setDirectory(directory);
			}
		} 

		
		class MouseHandler extends MouseAdapter
		{
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

	
	class FavoritesMenuButton extends JButton
	{
		
		FavoritesMenuButton()
		{
			setText(jEdit.getProperty("vfs.browser.favorites.label"));
			setIcon(GUIUtilities.loadIcon("ToolbarMenu.gif"));
			setHorizontalTextPosition(SwingConstants.LEADING);

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

			VFS.DirectoryEntry[] favorites
				= FavoritesVFS.getFavorites();
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
				MiscUtilities.quicksort(favorites,
					new VFS.DirectoryEntryCompare(
					sortMixFilesAndDirs,
					sortIgnoreCase));
				for(int i = 0; i < favorites.length; i++)
				{
					VFS.DirectoryEntry favorite
						= favorites[i];
					mi = new JMenuItem(favorite.path);
					mi.setIcon(FileCellRenderer
						.getIconForFile(
						favorite,false));
					String cmd = (favorite.type ==
						VFS.DirectoryEntry.FILE
						? "file@" : "dir@")
						+ favorite.path;
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
				if(actionCommand.equals("add-to-favorites"))
				{
					
					
					ArrayList toAdd = new ArrayList();
					VFS.DirectoryEntry[] selected = getSelectedFiles();
					if(selected == null || selected.length == 0)
					{
						if(path.equals(FavoritesVFS.PROTOCOL + ":"))
						{
							GUIUtilities.error(VFSBrowser.this,
								"vfs.browser.recurse-favorites",
								null);
						}
						else
						{
							FavoritesVFS.addToFavorites(path,
								VFS.DirectoryEntry.DIRECTORY);
						}
					}
					else
					{
						for(int i = 0; i < selected.length; i++)
						{
							VFS.DirectoryEntry file
								= selected[i];
							FavoritesVFS.addToFavorites(file.path,file.type);
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
		Object node;
		Object[] loadInfo;

		DirectoryLoadedAWTRequest(Object node, Object[] loadInfo)
		{
			this.node = node;
			this.loadInfo = loadInfo;
		}

		public void run()
		{
			String path = (String)loadInfo[0];
			VFS.DirectoryEntry[] list = (VFS.DirectoryEntry[])
				loadInfo[1];

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

				HistoryModel.getModel("vfs.browser.path")
					.addItem(path);
			}

			boolean filterEnabled = filterCheckbox.isSelected();

			ArrayList directoryVector = new ArrayList();

			int directories = 0;
			int files = 0;
			int invisible = 0;

			if(list != null)
			{
				for(int i = 0; i < list.length; i++)
				{
					VFS.DirectoryEntry file = list[i];
					if(file.hidden && !showHiddenFiles)
					{
						invisible++;
						continue;
					}

					if(file.type == VFS.DirectoryEntry.FILE
						&& filterEnabled
						&& filenameFilter != null
						&& !filenameFilter.isMatch(file.name))
					{
						invisible++;
						continue;
					}

					if(file.type == VFS.DirectoryEntry.FILE)
						files++;
					else
						directories++;

					directoryVector.add(file);
				}

				MiscUtilities.quicksort(directoryVector,
					new VFS.DirectoryEntryCompare(
					sortMixFilesAndDirs,
					sortIgnoreCase));
			}

			browserView.directoryLoaded(node,path,
				directoryVector);

			
			

			
			
			

			
			
			
			if(mode == CHOOSE_DIRECTORY_DIALOG)
				filesSelected();
		}

		public String toString()
		{
			return (String)loadInfo[0];
		}
	} 

	
	static class BrowserActionContext extends ActionContext
	{
		
		private VFS.DirectoryEntry[] getSelectedFiles(EventObject evt,
			VFSBrowser browser)
		{
			Component source = (Component)evt.getSource();

			if(GUIUtilities.getComponentParent(source,JList.class)
				!= null)
			{
				Object[] selected = browser.getBrowserView()
					.getParentDirectoryList()
					.getSelectedValues();
				VFS.DirectoryEntry[] returnValue
					= new VFS.DirectoryEntry[
					selected.length];
				System.arraycopy(selected,0,returnValue,0,
					selected.length);
				return returnValue;
			}
			else
			{
				return browser.getSelectedFiles();
			}
		}

		public void invokeAction(EventObject evt, EditAction action)
		{
			VFSBrowser browser = (VFSBrowser)
				GUIUtilities.getComponentParent(
				(Component)evt.getSource(),
				VFSBrowser.class);

			VFS.DirectoryEntry[] files = getSelectedFiles(evt,
				browser);

			
			
			

			
			
			
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

	
}
