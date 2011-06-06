

package org.gjt.sp.jedit;


import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.LayoutFocusTraversalPolicy;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;

import org.gjt.sp.jedit.bufferset.BufferSet;
import org.gjt.sp.jedit.bufferset.BufferSetManager;
import org.gjt.sp.jedit.gui.ActionBar;
import org.gjt.sp.jedit.gui.CloseDialog;
import org.gjt.sp.jedit.gui.DefaultInputHandler;
import org.gjt.sp.jedit.gui.DockableWindowFactory;
import org.gjt.sp.jedit.gui.DockableWindowManager;
import org.gjt.sp.jedit.gui.HistoryModel;
import org.gjt.sp.jedit.gui.DockingFrameworkProvider;
import org.gjt.sp.jedit.gui.InputHandler;
import org.gjt.sp.jedit.gui.StatusBar;
import org.gjt.sp.jedit.gui.ToolBarManager;
import org.gjt.sp.jedit.gui.VariableGridLayout;
import org.gjt.sp.jedit.gui.DockableWindowManager.DockingLayout;
import org.gjt.sp.jedit.input.InputHandlerProvider;
import org.gjt.sp.jedit.msg.BufferUpdate;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.jedit.msg.SearchSettingsChanged;
import org.gjt.sp.jedit.msg.ViewUpdate;
import org.gjt.sp.jedit.options.GeneralOptionPane;
import org.gjt.sp.jedit.search.CurrentBufferSet;
import org.gjt.sp.jedit.search.SearchAndReplace;
import org.gjt.sp.jedit.search.SearchBar;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.textarea.ScrollListener;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.jedit.visitors.JEditVisitor;
import org.gjt.sp.jedit.visitors.JEditVisitorAdapter;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;



public class View extends JFrame implements EBComponent, InputHandlerProvider
{
	

	

	public static final String VIEW_DOCKING_FRAMEWORK_PROPERTY = "view.docking.framework";
	private static final String ORIGINAL_DOCKING_FRAMEWORK = "Original";
	public static final String DOCKING_FRAMEWORK_PROVIDER_SERVICE =
		"org.gjt.sp.jedit.gui.DockingFrameworkProvider";
	private static DockingFrameworkProvider dockingFrameworkProvider;

	
	
	public static final int TOP_GROUP = 0;

	
	public static final int BOTTOM_GROUP = 1;
	public static final int DEFAULT_GROUP = TOP_GROUP;
	

	

	
	
	public static final int TOP_LAYER = Integer.MAX_VALUE;

	
	public static final int DEFAULT_LAYER = 0;

	
	public static final int BOTTOM_LAYER = Integer.MIN_VALUE;

	
	
	public static final int ABOVE_SYSTEM_BAR_LAYER = 150;

	
	public static final int SYSTEM_BAR_LAYER = 100;

	
	public static final int BELOW_SYSTEM_BAR_LAYER = 75;

	
	public static final int SEARCH_BAR_LAYER = 75;

	
	public static final int BELOW_SEARCH_BAR_LAYER = 50;

	
	
	@Deprecated
	public static final int ABOVE_ACTION_BAR_LAYER = -50;

	
	public static final int ACTION_BAR_LAYER = -75;

	
	public static final int STATUS_BAR_LAYER = -100;

	
	public static final int BELOW_STATUS_BAR_LAYER = -150;
	

	

	
	
	public DockableWindowManager getDockableWindowManager()
	{
		return dockableWindowManager;
	} 

	
	public static String getDockingFrameworkName()
	{
		String framework = jEdit.getProperty(
				VIEW_DOCKING_FRAMEWORK_PROPERTY, ORIGINAL_DOCKING_FRAMEWORK);
		return framework;
	} 

	
	public static DockingFrameworkProvider getDockingFrameworkProvider()
	{
		if (dockingFrameworkProvider == null)
		{
			String framework = getDockingFrameworkName();
			dockingFrameworkProvider = (DockingFrameworkProvider)
				ServiceManager.getService(
					DOCKING_FRAMEWORK_PROVIDER_SERVICE, framework);

			if (dockingFrameworkProvider == null)
			{
				Log.log(Log.ERROR, View.class, "No docking framework " + framework +
							       " available, using the original one");
				dockingFrameworkProvider = (DockingFrameworkProvider)
				ServiceManager.getService(
					DOCKING_FRAMEWORK_PROVIDER_SERVICE, ORIGINAL_DOCKING_FRAMEWORK);
			}
		}
		return dockingFrameworkProvider;
	} 

	
	
	public Container getToolBar()
	{
		return toolBar;
	} 

	
	
	public void addToolBar(Component toolBar)
	{
		addToolBar(DEFAULT_GROUP, DEFAULT_LAYER, toolBar);
	} 

	
	
	public void addToolBar(int group, Component toolBar)
	{
		addToolBar(group, DEFAULT_LAYER, toolBar);
	} 

	
	
	public void addToolBar(int group, int layer, Component toolBar)
	{
		toolBarManager.addToolBar(group, layer, toolBar);
		getRootPane().revalidate();
	} 

	
	
	public void removeToolBar(Component toolBar)
	{
		if (toolBarManager == null) return;
		if (toolBar == null) return;
		toolBarManager.removeToolBar(toolBar);
		getRootPane().revalidate();
	} 

	
	
	public synchronized void showWaitCursor()
	{
		if(waitCount++ == 0)
		{
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
			setCursor(cursor);
			visit(new SetCursorVisitor(cursor));
		}
	} 

	
	
	public synchronized void hideWaitCursor()
	{
		if(waitCount > 0)
			waitCount--;

		if(waitCount == 0)
		{
			
			
			Cursor cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
			setCursor(cursor);

			visit(new SetCursorVisitor(cursor));
		}
	} 

	
	
	public final SearchBar getSearchBar()
	{
		return searchBar;
	} 

	
	
	public final ActionBar getActionBar()
	{
		return actionBar;
	} 

	
	
	public StatusBar getStatus()
	{
		return status;
	} 

	
	
	public void quickIncrementalSearch(boolean word)
	{
		if(searchBar == null)
			searchBar = new SearchBar(this,true);
		if(searchBar.getParent() == null)
			addToolBar(TOP_GROUP,SEARCH_BAR_LAYER,searchBar);

		searchBar.setHyperSearch(false);

		JEditTextArea textArea = getTextArea();

		if(word)
		{
			String text = textArea.getSelectedText();
			if(text == null)
			{
				textArea.selectWord();
				text = textArea.getSelectedText();
			}
			else if(text.indexOf('\n') != -1)
				text = null;

			if(text != null && SearchAndReplace.getRegexp())
				text = SearchAndReplace.escapeRegexp(text,false);

			searchBar.getField().setText(text);
		}

		searchBar.getField().requestFocus();
		searchBar.getField().selectAll();
	} 

	
	
	public void quickHyperSearch(boolean word)
	{
		JEditTextArea textArea = getTextArea();

		if(word)
		{
			String text = textArea.getSelectedText();
			if(text == null)
			{
				textArea.selectWord();
				text = textArea.getSelectedText();
			}

			if(text != null && text.indexOf('\n') == -1)
			{
				if(SearchAndReplace.getRegexp())
				{
					text = SearchAndReplace.escapeRegexp(
						text,false);
				}

				HistoryModel.getModel("find").addItem(text);
				SearchAndReplace.setSearchString(text);
				SearchAndReplace.setSearchFileSet(new CurrentBufferSet());
				SearchAndReplace.hyperSearch(this);

				return;
			}
		}

		if(searchBar == null)
			searchBar = new SearchBar(this,true);
		if(searchBar.getParent() == null)
			addToolBar(TOP_GROUP,SEARCH_BAR_LAYER,searchBar);

		searchBar.setHyperSearch(true);
		searchBar.getField().setText(null);
		searchBar.getField().requestFocus();
		searchBar.getField().selectAll();
	} 

	
	
	public void actionBar()
	{
		if(actionBar == null)
			actionBar = new ActionBar(this,true);
		if(actionBar.getParent() == null)
			addToolBar(BOTTOM_GROUP,ACTION_BAR_LAYER,actionBar);

		actionBar.goToActionBar();
	} 

	

	

	
	
	public KeyListener getKeyEventInterceptor()
	{
		return inputHandler.getKeyEventInterceptor();
	} 

	
	
	public void setKeyEventInterceptor(KeyListener listener)
	{
		inputHandler.setKeyEventInterceptor(listener);
	} 

	
	
	public InputHandler getInputHandler()
	{
		return inputHandler;
	} 



	
	
	public void setInputHandler(InputHandler inputHandler)
	{
		this.inputHandler = inputHandler;
	} 

	
	
	public Macros.Recorder getMacroRecorder()
	{
		return recorder;
	} 

	
	
	public void setMacroRecorder(Macros.Recorder recorder)
	{
		this.recorder = recorder;
	} 

	
	
	@Override
	public void processKeyEvent(KeyEvent evt)
	{
		inputHandler.processKeyEvent(evt,VIEW, false);
		if(!evt.isConsumed())
			super.processKeyEvent(evt);
	} 

	
	
	public void processKeyEvent(KeyEvent evt, boolean calledFromTextArea)
	{
		processKeyEvent(evt,calledFromTextArea
			? TEXT_AREA
			: VIEW);
	} 

	
	public static final int VIEW = 0;
	public static final int TEXT_AREA = 1;
	public static final int ACTION_BAR = 2;
	
	public void processKeyEvent(KeyEvent evt, int from)
	{
		processKeyEvent(evt,from,false);
	}
	
	@Deprecated
	public void processKeyEvent(KeyEvent evt, int from, boolean global)
	{
		inputHandler.processKeyEvent(evt, from, global);
		if(!evt.isConsumed())
			super.processKeyEvent(evt);
	} 


	

	

	
	
	public EditPane splitHorizontally()
	{
		return split(JSplitPane.VERTICAL_SPLIT);
	} 

	
	
	public EditPane splitVertically()
	{
		return split(JSplitPane.HORIZONTAL_SPLIT);
	} 

	
	
	public EditPane split(int orientation)
	{
		PerspectiveManager.setPerspectiveDirty(true);

		editPane.saveCaretInfo();
		EditPane oldEditPane = editPane;
		String action = jEdit.getProperty("editpane.bufferset.new");
		BufferSetManager.NewBufferSetAction bufferSetAction = BufferSetManager.NewBufferSetAction.fromString(action);
		EditPane newEditPane;
		if (bufferSetAction == BufferSetManager.NewBufferSetAction.empty)
			newEditPane = createEditPane(null);
		else
			newEditPane = createEditPane(oldEditPane.getBuffer());

		newEditPane.loadCaretInfo();

		JComponent oldParent = (JComponent)oldEditPane.getParent();

		final JSplitPane newSplitPane = new JSplitPane(orientation,
							       jEdit.getBooleanProperty("appearance.continuousLayout"));
		newSplitPane.setOneTouchExpandable(true);
		newSplitPane.setBorder(null);
		newSplitPane.setMinimumSize(new Dimension(0,0));
		newSplitPane.setResizeWeight(0.5);

		int parentSize = orientation == JSplitPane.VERTICAL_SPLIT
			? oldEditPane.getHeight() : oldEditPane.getWidth();
		final int dividerPosition = (int)((parentSize
			- newSplitPane.getDividerSize()) * 0.5);
		newSplitPane.setDividerLocation(dividerPosition);

		if(oldParent instanceof JSplitPane)
		{
			JSplitPane oldSplitPane = (JSplitPane)oldParent;
			int dividerPos = oldSplitPane.getDividerLocation();

			Component left = oldSplitPane.getLeftComponent();

			if(left == oldEditPane)
				oldSplitPane.setLeftComponent(newSplitPane);
			else
				oldSplitPane.setRightComponent(newSplitPane);

			newSplitPane.setLeftComponent(oldEditPane);
			newSplitPane.setRightComponent(newEditPane);

			oldSplitPane.setDividerLocation(dividerPos);
		}
		else
		{
			splitPane = newSplitPane;

			newSplitPane.setLeftComponent(oldEditPane);
			newSplitPane.setRightComponent(newEditPane);

			setMainContent(newSplitPane);

		}

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				newSplitPane.setDividerLocation(dividerPosition);
			}
		});

		newEditPane.focusOnTextArea();

		return newEditPane;
	} 

	
	
	public void unsplit()
	{
		if(splitPane != null)
		{
			lastSplitConfig = getSplitConfig();

			PerspectiveManager.setPerspectiveDirty(true);

			for(EditPane _editPane: getEditPanes())
			{
				if(editPane != _editPane)
				{
					mergeBufferSets(editPane, _editPane);
					_editPane.close();
				}
			}

			setMainContent(editPane);

			splitPane = null;
			updateTitle();

			editPane.focusOnTextArea();
		}
		else
			getToolkit().beep();
	} 

	
	
	public void unsplitCurrent()
	{
		if(splitPane != null)
		{
			lastSplitConfig = getSplitConfig();

			PerspectiveManager.setPerspectiveDirty(true);

			
			Component comp = editPane;
			while(!(comp instanceof JSplitPane) && comp != null)
			{
				comp = comp.getParent();
			}

			
			
			for(EditPane _editPane: getEditPanes())
			{
				if(GUIUtilities.isAncestorOf(comp,_editPane)
					&& _editPane != editPane)
				{
					mergeBufferSets(editPane, _editPane);
					_editPane.close();
				}
			}

			JComponent parent = comp == null ? null : (JComponent)comp.getParent();

			if(parent instanceof JSplitPane)
			{
				JSplitPane parentSplit = (JSplitPane)parent;
				int pos = parentSplit.getDividerLocation();
				if(parentSplit.getLeftComponent() == comp)
					parentSplit.setLeftComponent(editPane);
				else
					parentSplit.setRightComponent(editPane);
				parentSplit.setDividerLocation(pos);
				parent.revalidate();
			}
			else
			{
				setMainContent(editPane);
				splitPane = null;
			}

			updateTitle();

			editPane.focusOnTextArea();
		}
		else
			getToolkit().beep();
	} 

	
	
	public void resplit()
	{
		if(lastSplitConfig == null)
			getToolkit().beep();
		else
			setSplitConfig(null,lastSplitConfig);
	} 

	
	
	public String getSplitConfig()
	{
		StringBuilder splitConfig = new StringBuilder();

		if(splitPane != null)
			getSplitConfig(splitPane,splitConfig);
		else
		{
			appendToSplitConfig(splitConfig, editPane);
		}

		return splitConfig.toString();
	} 

	
	
	public void setSplitConfig(Buffer buffer, String splitConfig)
	{
		try
		{
			Component comp = restoreSplitConfig(buffer,splitConfig);
			setMainContent(comp);
		}
		catch(IOException e)
		{
			
			throw new InternalError();
		}
	} 

	
	
	public void nextTextArea()
	{
		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			if(editPane == editPanes[i])
			{
				if(i == editPanes.length - 1)
					editPanes[0].focusOnTextArea();
				else
					editPanes[i+1].focusOnTextArea();
				break;
			}
		}
	} 

	
	
	public void prevTextArea()
	{
		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			if(editPane == editPanes[i])
			{
				if(i == 0)
					editPanes[editPanes.length - 1].focusOnTextArea();
				else
					editPanes[i-1].focusOnTextArea();
				break;
			}
		}
	} 

	
	
	public JSplitPane getSplitPane()
	{
		return splitPane;
	} 

	
	
	public Buffer getBuffer()
	{
		if(editPane == null)
			return null;
		else
			return editPane.getBuffer();
	} 

	
	
	public void setBuffer(Buffer buffer)
	{
		setBuffer(buffer,false);
	} 

	
	
	public void setBuffer(Buffer buffer, boolean disableFileStatusCheck)
	{
		setBuffer(buffer, disableFileStatusCheck, true);
	} 

	
	
	public void setBuffer(Buffer buffer, boolean disableFileStatusCheck, boolean focus)
	{
		editPane.setBuffer(buffer, focus);
		int check = jEdit.getIntegerProperty("checkFileStatus");
		if(!disableFileStatusCheck && (check == GeneralOptionPane.checkFileStatus_all ||
						  check == GeneralOptionPane.checkFileStatus_operations ||
						  check == GeneralOptionPane.checkFileStatus_focusBuffer))
			jEdit.checkBufferStatus(this, true);
	} 

	
	
	public EditPane goToBuffer(Buffer buffer)
	{
		return showBuffer(buffer, true);
	} 

	
	
	public EditPane showBuffer(Buffer buffer)
	{
		return showBuffer(buffer, false);
	} 

	
	
	public JEditTextArea getTextArea()
	{
		if(editPane == null)
			return null;
		else
			return editPane.getTextArea();
	} 

	
	
	public EditPane getEditPane()
	{
		return editPane;
	} 

	
	
	public EditPane[] getEditPanes()
	{
		if(splitPane == null)
		{
			EditPane[] ep = { editPane };
			return ep;
		}
		else
		{
			List<EditPane> vec = new ArrayList<EditPane>();
			getEditPanes(vec,splitPane);
			EditPane[] ep = new EditPane[vec.size()];
			vec.toArray(ep);
			return ep;
		}
	} 

	
	public BufferSet getLocalBufferSet()
	{
		return localBufferSet;
	}

	
	
	public ViewConfig getViewConfig()
	{
		ViewConfig config = new ViewConfig();
		config.plainView = isPlainView();
		config.splitConfig = getSplitConfig();
		config.extState = getExtendedState();
		config.docking = dockableWindowManager.getDockingLayout(config);
		String prefix = config.plainView ? "plain-view" : "view";
		switch (config.extState)
		{
			case Frame.MAXIMIZED_BOTH:
			case Frame.ICONIFIED:
				config.x = jEdit.getIntegerProperty(prefix + ".x",getX());
				config.y = jEdit.getIntegerProperty(prefix + ".y",getY());
				config.width = jEdit.getIntegerProperty(prefix + ".width",getWidth());
				config.height = jEdit.getIntegerProperty(prefix + ".height",getHeight());
				break;

			case Frame.MAXIMIZED_VERT:
				config.x = getX();
				config.y = jEdit.getIntegerProperty(prefix + ".y",getY());
				config.width = getWidth();
				config.height = jEdit.getIntegerProperty(prefix + ".height",getHeight());
				break;

			case Frame.MAXIMIZED_HORIZ:
				config.x = jEdit.getIntegerProperty(prefix + ".x",getX());
				config.y = getY();
				config.width = jEdit.getIntegerProperty(prefix + ".width",getWidth());
				config.height = getHeight();
				break;

			case Frame.NORMAL:
			default:
				config.x = getX();
				config.y = getY();
				config.width = getWidth();
				config.height = getHeight();
				break;
		}
		return config;
	} 

	

	
	
	public boolean isClosed()
	{
		return closed;
	} 

	
	
	public boolean isPlainView()
	{
		return plainView;
	} 

	
	
	public View getNext()
	{
		return next;
	} 

	
	
	public View getPrev()
	{
		return prev;
	} 

	
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof PropertiesChanged)
			propertiesChanged();
		else if(msg instanceof SearchSettingsChanged)
		{
			if(searchBar != null)
				searchBar.update();
		}
		else if(msg instanceof BufferUpdate)
			handleBufferUpdate((BufferUpdate)msg);
		else if(msg instanceof EditPaneUpdate)
			handleEditPaneUpdate((EditPaneUpdate)msg);
	} 

	
	@Override
	public Dimension getMinimumSize()
	{
		return new Dimension(0,0);
	} 

	
	
	public void setWaitSocket(Socket waitSocket)
	{
		this.waitSocket = waitSocket;
	} 

	
	@Override
	public String toString()
	{
		return getClass().getName() + '['
			+ (jEdit.getActiveView() == this
			? "active" : "inactive")
			+ ']';
	} 

	
	
	public void updateTitle()
	{
		List<Buffer> buffers = new ArrayList<Buffer>();
		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			Buffer buffer = editPanes[i].getBuffer();
			if(!buffers.contains(buffer))
				buffers.add(buffer);
		}

		StringBuilder title = new StringBuilder();

		
		if(!OperatingSystem.isMacOS())
			title.append(jEdit.getProperty("view.title"));

		for(int i = 0; i < buffers.size(); i++)
		{
			if(i != 0)
				title.append(", ");

			Buffer buffer = buffers.get(i);
			title.append(showFullPath && !buffer.isNewFile()
				? buffer.getPath(true) : buffer.getName());
			if(buffer.isDirty())
				title.append(jEdit.getProperty("view.title.dirty"));
		}

		setTitle(title.toString());
	} 

	
	public Component getPrefixFocusOwner()
	{
		return prefixFocusOwner;
	} 

	
	public void setPrefixFocusOwner(Component prefixFocusOwner)
	{
		this.prefixFocusOwner = prefixFocusOwner;
	} 

	
	
	public void visit(JEditVisitor visitor)
	{
		EditPane[] panes = getEditPanes();
		for (int i = 0; i < panes.length; i++)
		{
			EditPane editPane = panes[i];
			visitor.visit(editPane);
			visitor.visit(editPane.getTextArea());
		}
	} 

	
	View prev;
	View next;

	
	View(Buffer buffer, ViewConfig config)
	{
		fullScreenMode = false;
		menuBar = null;
		plainView = config.plainView;

		enableEvents(AWTEvent.KEY_EVENT_MASK);

		setIconImage(GUIUtilities.getEditorIcon());

		mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		dockableWindowManager = getDockingFrameworkProvider().create(this,
			DockableWindowFactory.getInstance(), config);
		dockableWindowManager.setMainPanel(mainPanel);

		topToolBars = new JPanel(new VariableGridLayout(
			VariableGridLayout.FIXED_NUM_COLUMNS,
			1));
		bottomToolBars = new JPanel(new VariableGridLayout(
			VariableGridLayout.FIXED_NUM_COLUMNS,
			1));

		toolBarManager = new ToolBarManager(topToolBars, bottomToolBars);

		status = new StatusBar(this);

		inputHandler = new DefaultInputHandler(this,(DefaultInputHandler)
			jEdit.getInputHandler());

		localBufferSet = new BufferSet();

		setSplitConfig(buffer,config.splitConfig);

		getContentPane().add(BorderLayout.CENTER,dockableWindowManager);

		dockableWindowManager.init();

		
		
		propertiesChanged();

		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowHandler());

		setFocusTraversalPolicy(new MyFocusTraversalPolicy());

		EditBus.addToBus(this);

		GUIUtilities.addSizeSaver(this, null, plainView ? "plain-view" : "view");
	} 

	
	public void updateFullScreenProps()
	{
		boolean alternateLayout = jEdit.getBooleanProperty(
			"view.toolbar.alternateLayout");
		boolean showMenu = jEdit.getBooleanProperty("fullScreenIncludesMenu");
		boolean showToolbars = jEdit.getBooleanProperty("fullScreenIncludesToolbar");
		boolean showStatus = jEdit.getBooleanProperty("fullScreenIncludesStatus");
		if (! showMenu)
		{
			menuBar = getJMenuBar();
			setJMenuBar(null);
		}
		else if (menuBar != null)
			setJMenuBar(menuBar);
		if (alternateLayout)
		{
			
			if (! showToolbars)
				getContentPane().remove(topToolBars);
			else
				getContentPane().add(BorderLayout.NORTH,topToolBars);
			if (! showStatus)
				removeToolBar(status);
			else
				addToolBar(BOTTOM_GROUP,STATUS_BAR_LAYER,status);
		}
		else
		{
			
			if (! showToolbars)
				mainPanel.remove(topToolBars);
			else
				mainPanel.add(topToolBars, BorderLayout.NORTH);
			if (! showStatus)
				getContentPane().remove(status);
			else
				getContentPane().add(BorderLayout.SOUTH,status);
		}
	} 

	
	public void toggleFullScreen()
	{
		fullScreenMode = (! fullScreenMode);
		GraphicsDevice sd = getGraphicsConfiguration().getDevice();
		dispose();
		if (fullScreenMode)
		{
			updateFullScreenProps();
			windowedBounds = getBounds();
			setUndecorated(true);
			setBounds(sd.getDefaultConfiguration().getBounds());
			validate();
		}
		else
		{
			boolean showStatus = plainView ? jEdit.getBooleanProperty("view.status.plainview.visible") :
				jEdit.getBooleanProperty("view.status.visible");
			if ((menuBar != null) && (getJMenuBar() != menuBar))
				setJMenuBar(menuBar);
			boolean alternateLayout = jEdit.getBooleanProperty(
				"view.toolbar.alternateLayout");
			if (alternateLayout)
			{
				getContentPane().add(BorderLayout.NORTH,topToolBars);
				if (showStatus)
					addToolBar(BOTTOM_GROUP,STATUS_BAR_LAYER,status);
			}
			else
			{
				mainPanel.add(topToolBars, BorderLayout.NORTH);
				if (showStatus)
					getContentPane().add(BorderLayout.SOUTH,status);
			}
			setUndecorated(false);
			setBounds(windowedBounds);
		}
		setVisible(true);
		toFront();
		
		editPane.getTextArea().requestFocus();
	} 

	
	
	boolean confirmToCloseDirty()
	{
		Set<Buffer> checkingBuffers = getOpenBuffers();
		for (View view: jEdit.getViews())
		{
			if (view != this)
			{
				checkingBuffers.removeAll(
					view.getOpenBuffers());
			}
		}
		for (Buffer buffer: checkingBuffers)
		{
			if (buffer.isDirty())
			{
				return new CloseDialog(this, checkingBuffers).isOK();
			}
		}
		return true;
	} 

	
	void close()
	{
		EditBus.send(new ViewUpdate(this,ViewUpdate.CLOSED));
		closed = true;

		
		dockableWindowManager.close();

		EditBus.removeFromBus(this);
		dispose();

		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
			editPanes[i].close();

		
		
		toolBarManager = null;
		toolBar = null;
		searchBar = null;
		splitPane = null;
		inputHandler = null;
		recorder = null;

		getContentPane().removeAll();

		
		if(waitSocket != null)
		{
			try
			{
				waitSocket.getOutputStream().write('\0');
				waitSocket.getOutputStream().flush();
				waitSocket.getInputStream().close();
				waitSocket.getOutputStream().close();
				waitSocket.close();
			}
			catch(IOException io)
			{
				
			}
		}
	} 

	

	

	
	private boolean closed;

	private DockableWindowManager dockableWindowManager;
	private JPanel mainPanel;

	private JPanel topToolBars;
	private JPanel bottomToolBars;
	private ToolBarManager toolBarManager;

	private Container toolBar;
	private SearchBar searchBar;
	private ActionBar actionBar;

	private EditPane editPane;
	private JSplitPane splitPane;
	private String lastSplitConfig;
	private final BufferSet localBufferSet;

	private StatusBar status;

	private InputHandler inputHandler;
	private Macros.Recorder recorder;
	private Component prefixFocusOwner;

	private int waitCount;

	private boolean showFullPath;

	private boolean plainView;

	private Socket waitSocket;
	private Component mainContent;

	private boolean fullScreenMode;
	private Rectangle windowedBounds;
	private JMenuBar menuBar;

	

	
	private void setMainContent(Component c)
	{
		if (mainContent != null)
			mainPanel.remove(mainContent);
		mainContent = c;
		mainPanel.add(mainContent, BorderLayout.CENTER);
		mainPanel.revalidate();
		mainPanel.repaint();
	} 

	
	private static void getEditPanes(List<EditPane> vec, Component comp)
	{
		if(comp instanceof EditPane)
			vec.add((EditPane) comp);
		else if(comp instanceof JSplitPane)
		{
			JSplitPane split = (JSplitPane)comp;
			getEditPanes(vec,split.getLeftComponent());
			getEditPanes(vec,split.getRightComponent());
		}
	} 

	
	private EditPane showBuffer(Buffer buffer, boolean focus)
	{
		if(editPane.getBuffer() == buffer
			&& editPane.getTextArea().getVisibleLines() > 1)
		{
			if (focus)
				editPane.focusOnTextArea();
			return editPane;
		}

		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			EditPane ep = editPanes[i];
			if(ep.getBuffer() == buffer
				
				&& ep.getTextArea().getVisibleLines() > 1)
			{
				setEditPane(ep);
				if (focus)
					ep.focusOnTextArea();
				return ep;
			}
		}

		setBuffer(buffer,false, focus);
		return editPane;
	} 

	
	
	private static void getSplitConfig(JSplitPane splitPane,
		StringBuilder splitConfig)
	{
		Component right = splitPane.getRightComponent();
		appendToSplitConfig(splitConfig, right);

		splitConfig.append(' ');

		Component left = splitPane.getLeftComponent();
		appendToSplitConfig(splitConfig, left);

		splitConfig.append(' ');
		splitConfig.append(splitPane.getDividerLocation());
		splitConfig.append(' ');
		splitConfig.append(splitPane.getOrientation()
			== JSplitPane.VERTICAL_SPLIT ? "vertical" : "horizontal");
	} 

	
	
	private static void appendToSplitConfig(StringBuilder splitConfig, Component component)
	{
		if(component instanceof JSplitPane)
		{
			
			getSplitConfig((JSplitPane)component,splitConfig);
		}
		else
		{
			
			EditPane editPane = (EditPane) component;
			splitConfig.append('"');
			splitConfig.append(StandardUtilities.charsToEscapes(
				editPane.getBuffer().getPath()));
			splitConfig.append("\" buffer");
			BufferSet bufferSet = editPane.getBufferSet();
			Buffer[] buffers = bufferSet.getAllBuffers();
			for (Buffer buffer : buffers)
			{
				if (!buffer.isNewFile())
				{
					splitConfig.append(" \"");
					splitConfig.append(StandardUtilities.charsToEscapes(
						buffer.getPath()));
					splitConfig.append("\" buff");
				}
			}
			splitConfig.append(" \"");
			splitConfig.append(editPane.getBufferSetScope());
			splitConfig.append("\" bufferset");
		}
	} 

	
	private Component restoreSplitConfig(Buffer buffer, String splitConfig)
		throws IOException
	
	
	{
		if(buffer != null)
		{
			return editPane = createEditPane(buffer);
		}
		else if(splitConfig == null)
		{
			return editPane = createEditPane(jEdit.getFirstBuffer());
		}
		Buffer[] buffers = jEdit.getBuffers();

		Stack<Object> stack = new Stack<Object>();

		
		
		StreamTokenizer st = new StreamTokenizer(new StringReader(
			splitConfig));
		st.whitespaceChars(0,' ');
		
		st.wordChars('#','~');
		st.commentChar('!');
		st.quoteChar('"');
		st.eolIsSignificant(false);
		boolean continuousLayout = jEdit.getBooleanProperty("appearance.continuousLayout");
		List<Buffer> editPaneBuffers = new ArrayList<Buffer>();
loop:		while (true)
		{
			switch(st.nextToken())
			{
			case StreamTokenizer.TT_EOF:
				break loop;
			case StreamTokenizer.TT_WORD:
				if(st.sval.equals("vertical") ||
					st.sval.equals("horizontal"))
				{
					int orientation
						= st.sval.equals("vertical")
						? JSplitPane.VERTICAL_SPLIT
						: JSplitPane.HORIZONTAL_SPLIT;
					int divider = ((Integer)stack.pop())
						.intValue();
					Object obj1 = stack.pop();
					Object obj2 = stack.pop();
					
					if (obj1 instanceof Buffer)
					{
						Buffer b1 = buffer = (Buffer) obj1;
						jEdit.getGlobalBufferSet().addBufferAt(b1, -1);
						obj1 = editPane = createEditPane(b1, BufferSet.Scope.global);
					}
					if (obj2 instanceof Buffer)
					{
						Buffer b2 = (Buffer) obj2;
						jEdit.getGlobalBufferSet().addBufferAt(b2, -1);
						obj2 = createEditPane(b2, BufferSet.Scope.global);
					}
					stack.push(splitPane = new JSplitPane(
						orientation,
						continuousLayout,
						(Component)obj1,
						(Component)obj2));
					splitPane.setOneTouchExpandable(true);
					splitPane.setBorder(null);
					splitPane.setMinimumSize(
						new Dimension(0,0));
					splitPane.setDividerLocation(divider);
				}
				else if(st.sval.equals("buffer"))
				{
					Object obj = stack.pop();
					if(obj instanceof Integer)
					{
						int index = ((Integer)obj).intValue();
						if(index >= 0 && index < buffers.length)
							buffer = buffers[index];
					}
					else if(obj instanceof String)
					{
						String path = (String)obj;
						buffer = jEdit.getBuffer(path);
						if (buffer == null)
						{
							int untitledCount = jEdit.getNextUntitledBufferId();
							buffer = jEdit.openFile(this,null,"Untitled-" + untitledCount,true,null);
						}
					}

					if(buffer == null)
						buffer = jEdit.getFirstBuffer();
					stack.push(buffer);
				}
				else if (st.sval.equals("buff"))
				{

					String path = (String)stack.pop();
					buffer = jEdit.getBuffer(path);
					if (buffer == null)
					{
						Log.log(Log.WARNING, this, "Buffer " + path + " doesn't exist");
					}
					else
					{
						editPaneBuffers.add(buffer);
					}
				}
				else if (st.sval.equals("bufferset"))
				{
					BufferSet.Scope scope = BufferSet.Scope.fromString((String) stack.pop());
					buffer = (Buffer) stack.pop();
					editPane = createEditPane(buffer, scope);
					stack.push(editPane);
					BufferSetManager bufferSetManager = jEdit.getBufferSetManager();
					BufferSet bufferSet = editPane.getBufferSet();
					int i = 0;
					for (Buffer buff : editPaneBuffers)
					{
						if (buff == buffer)
							bufferSet.addBufferAt(buffer, i);
						else
							bufferSetManager.addBuffer(bufferSet, buff);
						i++;
					}
					editPaneBuffers.clear();
				}
				break;
			case StreamTokenizer.TT_NUMBER:
				stack.push((int)st.nval);
				break;
			case '"':
				stack.push(st.sval);
				break;
			}
		}

		
		Object obj = stack.peek();
		if (obj instanceof Buffer)
		{
			jEdit.getGlobalBufferSet().addBufferAt((Buffer)obj, -1);
			obj = editPane = createEditPane((Buffer)obj,
				BufferSet.Scope.global);
		}

		updateGutterBorders();

		return (Component)obj;
	} 

	
	
	private void propertiesChanged()
	{
		setJMenuBar(GUIUtilities.loadMenuBar("view.mbar"));

		loadToolBars();

		showFullPath = jEdit.getBooleanProperty("view.showFullPath");
		updateTitle();

		status.propertiesChanged();

		removeToolBar(status);
		getContentPane().remove(status);

		boolean showStatus = plainView ? jEdit.getBooleanProperty("view.status.plainview.visible") :
				    jEdit.getBooleanProperty("view.status.visible");
		if (jEdit.getBooleanProperty("view.toolbar.alternateLayout"))
		{
			getContentPane().add(BorderLayout.NORTH,topToolBars);
			getContentPane().add(BorderLayout.SOUTH,bottomToolBars);
			if (showStatus)
				addToolBar(BOTTOM_GROUP,STATUS_BAR_LAYER,status);
		}
		else
		{
			mainPanel.add(topToolBars, BorderLayout.NORTH);
			mainPanel.add(bottomToolBars, BorderLayout.SOUTH);
			if (showStatus)
				getContentPane().add(BorderLayout.SOUTH,status);
		}
		updateBufferSwitcherStates();

		getRootPane().revalidate();

		if (splitPane != null)
			GUIUtilities.initContinuousLayout(splitPane);
		

		if (fullScreenMode)
			updateFullScreenProps();
	} 

	
	
	public void updateBufferSwitcherStates()
	{
		boolean show = jEdit.getBooleanProperty("view.showBufferSwitcher");
		JMenuBar menubar = getJMenuBar();
		if (menubar == null)
		{
			return;
		}
		String viewmenu_label = jEdit.getProperty("view.label");
		viewmenu_label = viewmenu_label.replace("$", "");
		String sbs_label = jEdit.getProperty("focus-buffer-switcher.label");
		sbs_label = sbs_label.replace("$", "");
		JMenu viewmenu = null;
		for (int i = 0; i < menubar.getMenuCount(); i++)
		{
			JMenu menu = menubar.getMenu(i);
			if (menu.getText().equals(viewmenu_label))
			{
				viewmenu = menu;
				break;
			}
		}
		if (viewmenu != null)
		{
			for (int i = 0; i < viewmenu.getMenuComponentCount(); i++)
			{
				Component item = viewmenu.getMenuComponent(i);
				if (item instanceof JMenuItem && ((JMenuItem)item).getText().equals(sbs_label))
				{
					((JMenuItem)item).setEnabled(show);
					
				}
			}
		}
		
		for (View v: jEdit.getViews())
			for (EditPane ep: v.getEditPanes())
				ep.loadBufferSwitcher();
	} 


	
	private void loadToolBars()
	{
		if(jEdit.getBooleanProperty("view.showToolbar") && !plainView)
		{
			if(toolBar != null)
				toolBarManager.removeToolBar(toolBar);

			toolBar = GUIUtilities.loadToolBar("view.toolbar");

			addToolBar(TOP_GROUP, SYSTEM_BAR_LAYER, toolBar);
		}
		else if(toolBar != null)
		{
			removeToolBar(toolBar);
			toolBar = null;
		}

		if(searchBar != null)
		{
			searchBar.propertiesChanged();
			removeToolBar(searchBar);
		}

		if(jEdit.getBooleanProperty("view.showSearchbar") && !plainView)
		{
			if(searchBar == null)
				searchBar = new SearchBar(this,false);
			addToolBar(TOP_GROUP,SEARCH_BAR_LAYER,searchBar);
		}
	} 

	
	private EditPane createEditPane(Buffer buffer)
	{
		return createEditPane(buffer, BufferSet.Scope.fromString(
			jEdit.getProperty("editpane.bufferset.default")));
	}

	private EditPane createEditPane(Buffer buffer, BufferSet.Scope scope)
	{
		EditPane editPane = new EditPane(this,buffer, scope);
		JEditTextArea textArea = editPane.getTextArea();
		textArea.addFocusListener(new FocusHandler());
		textArea.addCaretListener(new CaretHandler());
		textArea.addScrollListener(new ScrollHandler());
		EditBus.send(new EditPaneUpdate(editPane,EditPaneUpdate.CREATED));
		return editPane;
	} 

	
	private void setEditPane(EditPane editPane)
	{
		this.editPane = editPane;
		status.updateCaretStatus();
		status.updateBufferStatus();
		status.updateMiscStatus();

		
		
		updateGutterBorders();

		EditBus.send(new ViewUpdate(this,ViewUpdate.EDIT_PANE_CHANGED));
	} 

	
	private void handleBufferUpdate(BufferUpdate msg)
	{
		Buffer buffer = msg.getBuffer();
		if(msg.getWhat() == BufferUpdate.DIRTY_CHANGED
			|| msg.getWhat() == BufferUpdate.LOADED)
		{
			EditPane[] editPanes = getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				if(editPanes[i].getBuffer() == buffer)
				{
					updateTitle();
					break;
				}
			}
		}
	} 

	
	private void handleEditPaneUpdate(EditPaneUpdate msg)
	{
		EditPane editPane = msg.getEditPane();
		if(editPane.getView() == this
			&& msg.getWhat() == EditPaneUpdate.BUFFER_CHANGED
			&& editPane.getBuffer().isLoaded())
		{
			closeDuplicateBuffers(msg);
			status.updateCaretStatus();
			status.updateBufferStatus();
			status.updateMiscStatus();
		}
	} 

	
	private void closeDuplicateBuffers(EditPaneUpdate epu)
	{
		if (!jEdit.getBooleanProperty("buffersets.exclusive"))
			return;
		EditPane ep = epu.getEditPane();
		
		if (ep.getView() != this) return;
		Buffer b = ep.getBuffer();
		for (View v: jEdit.getViews())
		{
			for (EditPane epc : v.getEditPanes())
			{
				
				if (epc == ep) continue;
				
				if ((epc.getBufferSetScope() == BufferSet.Scope.view)
					&&  (v == this)) continue;
				
				if (epc.getBufferSet() == jEdit.getGlobalBufferSet()) continue;
				
				if (epc.getBufferSet().indexOf(b) < 0) continue;
				
				jEdit.getBufferSetManager().removeBuffer(epc, b);
			}
		}
	} 

	
	
	private void updateGutterBorders()
	{
		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
			editPanes[i].getTextArea().getGutter().updateBorder();
	} 

	
	private Set<Buffer> getOpenBuffers()
	{
		Set<Buffer> openBuffers = new HashSet<Buffer>();
		for (EditPane editPane: getEditPanes())
		{
			openBuffers.addAll(Arrays.asList(
				editPane.getBufferSet().getAllBuffers()));
		}
		return openBuffers;
	} 

	
	
	static private void mergeBufferSets(EditPane target, EditPane source)
	{
		BufferSet sourceBufferSet = source.getBufferSet();
		BufferSet targetBufferSet = target.getBufferSet();
		if (sourceBufferSet != targetBufferSet)
		{
			jEdit.getBufferSetManager().mergeBufferSet(
				targetBufferSet, sourceBufferSet);
		}
	} 

	

	

	
	private class CaretHandler implements CaretListener
	{
		public void caretUpdate(CaretEvent evt)
		{
			if(evt.getSource() == getTextArea())
				status.updateCaretStatus();
		}
	} 

	
	private class FocusHandler extends FocusAdapter
	{
		@Override
		public void focusGained(FocusEvent evt)
		{
			
			Component comp = (Component)evt.getSource();
			while(!(comp instanceof EditPane))
			{
				if(comp == null)
					return;

				comp = comp.getParent();
			}

			if(comp != editPane)
				setEditPane((EditPane)comp);
			else
				updateGutterBorders();
		}
	} 

	
	private class ScrollHandler implements ScrollListener
	{
		public void scrolledVertically(TextArea textArea)
		{
			if(getTextArea() == textArea)
				status.updateCaretStatus();
		}

		public void scrolledHorizontally(TextArea textArea) {}
	} 

	
	private class WindowHandler extends WindowAdapter
	{
		@Override
		public void windowActivated(WindowEvent evt)
		{
			boolean editPaneChanged =
				jEdit.getActiveViewInternal() != View.this;
			jEdit.setActiveView(View.this);

			
			
			
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					int check = jEdit.getIntegerProperty("checkFileStatus");
					if(check == GeneralOptionPane.checkFileStatus_focus ||
					   check == GeneralOptionPane.checkFileStatus_all)
						jEdit.checkBufferStatus(View.this,false);
					else if(check == GeneralOptionPane.checkFileStatus_focusBuffer)
						jEdit.checkBufferStatus(View.this,true);
				}
			});

			if (editPaneChanged)
			{
				EditBus.send(new ViewUpdate(View.this,ViewUpdate
					.ACTIVATED));
			}
		}

		@Override
		public void windowClosing(WindowEvent evt)
		{
			jEdit.closeView(View.this);
		}
	} 

	
	public static class ViewConfig
	{
		public int x, y, width, height, extState;
		public boolean plainView;
		public String splitConfig;
		public DockingLayout docking;

		public ViewConfig()
		{
		}

		public ViewConfig(boolean plainView)
		{
			this.plainView = plainView;
			String prefix = plainView ? "plain-view" : "view";
			x = jEdit.getIntegerProperty(prefix + ".x",0);
			y = jEdit.getIntegerProperty(prefix + ".y",0);
			width = jEdit.getIntegerProperty(prefix + ".width",0);
			height = jEdit.getIntegerProperty(prefix + ".height",0);
			extState = jEdit.getIntegerProperty(prefix + ".extendedState", Frame.NORMAL);
		}

		public ViewConfig(boolean plainView, String splitConfig,
			int x, int y, int width, int height, int extState)
		{
			this.plainView = plainView;
			this.splitConfig = splitConfig;
			this.x = x;
			this.y = y;
			this.width = width;
			this.height = height;
			this.extState = extState;
		}
	} 

	
	private boolean isInsideScreen(View parent, Rectangle r)
	{
		Rectangle bounds;
		if (parent == null)
			bounds = GUIUtilities.getScreenBounds();
		else
			bounds = parent.getGraphicsConfiguration().getBounds();
		int minWidth = jEdit.getIntegerProperty("view.minStartupWidth");
		int minHeight = jEdit.getIntegerProperty("view.minStartupHeight");
		return (r.x < bounds.width - minWidth &&
				r.x + r.width > minWidth &&
				r.y < bounds.height - minHeight &&
				r.y + r.height > minHeight);
	}

	public void adjust(View parent, ViewConfig config)
	{
		if(config.width != 0 && config.height != 0)
		{
			Rectangle desired = new Rectangle(
					config.x, config.y, config.width, config.height);
			if (! isInsideScreen(parent, desired))
				setLocationRelativeTo(parent);
			else
			{
				if(OperatingSystem.isX11() && Debug.GEOMETRY_WORKAROUND)
					new GUIUtilities.UnixWorkaround(this,"view",desired,config.extState);
				else
				{
					setBounds(desired);
					setExtendedState(config.extState);
				}
			}
		}
		else
			setLocationRelativeTo(parent);
	}

	
	private static class MyFocusTraversalPolicy extends LayoutFocusTraversalPolicy
	{
		@Override
		public Component getDefaultComponent(Container focusCycleRoot)
		{
			return GUIUtilities.getView(focusCycleRoot).getTextArea();
		}
	} 

	
	private static class SetCursorVisitor extends JEditVisitorAdapter
	{
		private final Cursor cursor;

		SetCursorVisitor(Cursor cursor)
		{
			this.cursor = cursor;
		}

		@Override
		public void visit(EditPane editPane)
		{
			editPane.setCursor(cursor);
		}
	}
	

}
