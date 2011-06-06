

package org.gjt.sp.jedit;


import javax.swing.event.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.net.Socket;
import java.util.*;
import java.util.List;

import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.search.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.jedit.input.InputHandlerProvider;



public class View extends JFrame implements EBComponent, InputHandlerProvider
{
	

	

	
	
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

	
	
	public static final int ABOVE_ACTION_BAR_LAYER = -50;

	
	public static final int ACTION_BAR_LAYER = -75;

	
	public static final int STATUS_BAR_LAYER = -100;

	
	public static final int BELOW_STATUS_BAR_LAYER = -150;
	

	

	
	
	public DockableWindowManager getDockableWindowManager()
	{
		return dockableWindowManager;
	} 

	
	
	public Box getToolBar()
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
			EditPane[] editPanes = getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				EditPane editPane = editPanes[i];
				editPane.getTextArea().getPainter()
					.setCursor(cursor);
			}
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
			cursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);
			EditPane[] editPanes = getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				EditPane editPane = editPanes[i];
				editPane.getTextArea().getPainter()
					.setCursor(cursor);
			}
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
		setEditPane(createEditPane(oldEditPane.getBuffer()));
		editPane.loadCaretInfo();

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
			newSplitPane.setRightComponent(editPane);

			oldSplitPane.setDividerLocation(dividerPos);
		}
		else
		{
			splitPane = newSplitPane;

			newSplitPane.setLeftComponent(oldEditPane);
			newSplitPane.setRightComponent(editPane);

			oldParent.add(newSplitPane,0);
			oldParent.revalidate();
		}

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				newSplitPane.setDividerLocation(dividerPosition);
			}
		});

		editPane.focusOnTextArea();

		return editPane;
	} 

	
	
	public void unsplit()
	{
		if(splitPane != null)
		{
			lastSplitConfig = getSplitConfig();

			PerspectiveManager.setPerspectiveDirty(true);

			EditPane[] editPanes = getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				EditPane _editPane = editPanes[i];
				if(editPane != _editPane)
					_editPane.close();
			}

			JComponent parent = (JComponent)splitPane.getParent();

			parent.remove(splitPane);
			parent.add(editPane,0);
			parent.revalidate();

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
			while(!(comp instanceof JSplitPane))
			{
				comp = comp.getParent();
			}

			
			
			EditPane[] editPanes = getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				EditPane _editPane = editPanes[i];
				if(GUIUtilities.isAncestorOf(comp,_editPane)
					&& _editPane != editPane)
					_editPane.close();
			}

			JComponent parent = (JComponent)comp.getParent();

			if(parent instanceof JSplitPane)
			{
				JSplitPane parentSplit = (JSplitPane)parent;
				int pos = parentSplit.getDividerLocation();
				if(parentSplit.getLeftComponent() == comp)
					parentSplit.setLeftComponent(editPane);
				else
					parentSplit.setRightComponent(editPane);
				parentSplit.setDividerLocation(pos);
			}
			else
			{
				parent.remove(comp);
				parent.add(editPane,0);
				splitPane = null;
			}

			parent.revalidate();

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
		editPane.setBuffer(buffer);
	} 

	
	
	public EditPane goToBuffer(Buffer buffer)
	{
		if(editPane.getBuffer() == buffer
			&& editPane.getTextArea().getVisibleLines() > 1)
		{
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
				ep.focusOnTextArea();
				return ep;
			}
		}

		setBuffer(buffer);
		return editPane;
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

	
	
	public ViewConfig getViewConfig()
	{
		ViewConfig config = new ViewConfig();
		config.plainView = isPlainView();
		config.splitConfig = getSplitConfig();
		config.extState = getExtendedState();
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

		config.top = dockableWindowManager.getTopDockingArea().getCurrent();
		config.left = dockableWindowManager.getLeftDockingArea().getCurrent();
		config.bottom = dockableWindowManager.getBottomDockingArea().getCurrent();
		config.right = dockableWindowManager.getRightDockingArea().getCurrent();

		config.topPos = dockableWindowManager.getTopDockingArea().getDimension();
		config.leftPos = dockableWindowManager.getLeftDockingArea().getDimension();
		config.bottomPos = dockableWindowManager.getBottomDockingArea().getDimension();
		config.rightPos = dockableWindowManager.getRightDockingArea().getDimension();

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

	
	public Dimension getMinimumSize()
	{
		return new Dimension(0,0);
	} 

	
	
	public void setWaitSocket(Socket waitSocket)
	{
		this.waitSocket = waitSocket;
	} 

	
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
			if(buffers.indexOf(buffer) == -1)
				buffers.add(buffer);
		}

		StringBuilder title = new StringBuilder();

		
		if(!OperatingSystem.isMacOS())
			title.append(jEdit.getProperty("view.title"));

		boolean unsavedChanges = false;

		for(int i = 0; i < buffers.size(); i++)
		{
			if(i != 0)
				title.append(", ");

			Buffer buffer = buffers.get(i);
			title.append((showFullPath && !buffer.isNewFile())
				? buffer.getPath() : buffer.getName());
			if(buffer.isDirty())
			{
				unsavedChanges = true;
				title.append(jEdit.getProperty("view.title.dirty"));
			}
		}

		setTitle(title.toString());

		
		final String WINDOW_MODIFIED = "windowModified";
		getRootPane().putClientProperty(WINDOW_MODIFIED,
			unsavedChanges);
	} 


	public Component getPrefixFocusOwner()
	{
		return prefixFocusOwner;
	}


	public void setPrefixFocusOwner(Component prefixFocusOwner)
	{
		this.prefixFocusOwner = prefixFocusOwner;
	}

	
	View prev;
	View next;

	
	View(Buffer buffer, ViewConfig config)
	{
		plainView = config.plainView;

		enableEvents(AWTEvent.KEY_EVENT_MASK);

		setIconImage(GUIUtilities.getEditorIcon());

		dockableWindowManager = new DockableWindowManager(this,
			DockableWindowFactory.getInstance(),config);

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

	
	void close()
	{
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

	private JPanel topToolBars;
	private JPanel bottomToolBars;
	private ToolBarManager toolBarManager;

	private Box toolBar;
	private SearchBar searchBar;
	private ActionBar actionBar;

	private EditPane editPane;
	private JSplitPane splitPane;
	private String lastSplitConfig;

	private StatusBar status;

	private InputHandler inputHandler;
	private Macros.Recorder recorder;
	private Component prefixFocusOwner;

	private int waitCount;

	private boolean showFullPath;

	private boolean plainView;

	private Socket waitSocket;
	

	
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

	
	private String getSplitConfig()
	{
		StringBuilder splitConfig = new StringBuilder();

		if(splitPane != null)
			getSplitConfig(splitPane,splitConfig);
		else
		{
			splitConfig.append('"');
			splitConfig.append(MiscUtilities.charsToEscapes(
				getBuffer().getPath()));
			splitConfig.append("\" buffer");
		}

		return splitConfig.toString();
	} 

	
	
	private static void getSplitConfig(JSplitPane splitPane,
		StringBuilder splitConfig)
	{
		Component right = splitPane.getRightComponent();
		if(right instanceof JSplitPane)
			getSplitConfig((JSplitPane)right,splitConfig);
		else
		{
			splitConfig.append('"');
			splitConfig.append(MiscUtilities.charsToEscapes(
				((EditPane)right).getBuffer().getPath()));
			splitConfig.append("\" buffer");
		}

		splitConfig.append(' ');

		Component left = splitPane.getLeftComponent();
		if(left instanceof JSplitPane)
			getSplitConfig((JSplitPane)left,splitConfig);
		else
		{
			splitConfig.append('"');
			splitConfig.append(MiscUtilities.charsToEscapes(
				((EditPane)left).getBuffer().getPath()));
			splitConfig.append("\" buffer");
		}

		splitConfig.append(' ');
		splitConfig.append(splitPane.getDividerLocation());
		splitConfig.append(' ');
		splitConfig.append(splitPane.getOrientation()
			== JSplitPane.VERTICAL_SPLIT ? "vertical" : "horizontal");
	} 

	
	private void setSplitConfig(Buffer buffer, String splitConfig)
	{
		if(editPane != null)
			dockableWindowManager.remove(editPane);

		if(splitPane != null)
			dockableWindowManager.remove(splitPane);

		try
		{
			Component comp = restoreSplitConfig(buffer,splitConfig);
			dockableWindowManager.add(comp,0);
		}
		catch(IOException e)
		{
			
			throw new InternalError();
		}

		dockableWindowManager.revalidate();
		dockableWindowManager.repaint();
	} 

	
	private Component restoreSplitConfig(Buffer buffer, String splitConfig)
		throws IOException
	
	
	{
		if(buffer != null)
			return editPane = createEditPane(buffer);
		else if(splitConfig == null)
			return editPane = createEditPane(jEdit.getFirstBuffer());

		Buffer[] buffers = jEdit.getBuffers();

		Stack stack = new Stack();

		
		
		StreamTokenizer st = new StreamTokenizer(new StringReader(
			splitConfig));
		st.whitespaceChars(0,' ');
		
		st.wordChars('#','~');
		st.commentChar('!');
		st.quoteChar('"');
		st.eolIsSignificant(false);
		boolean continuousLayout = jEdit.getBooleanProperty("appearance.continuousLayout");
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
						= (st.sval.equals("vertical")
						? JSplitPane.VERTICAL_SPLIT
						: JSplitPane.HORIZONTAL_SPLIT);
					int divider = ((Integer)stack.pop())
						.intValue();
					stack.push(splitPane = new JSplitPane(
						orientation,
						continuousLayout,
						(Component)stack.pop(),
						(Component)stack.pop()));
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
					}

					if(buffer == null)
						buffer = jEdit.getFirstBuffer();

					stack.push(editPane = createEditPane(
						buffer));
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

		updateGutterBorders();

		return (Component)stack.peek();
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

		if(jEdit.getBooleanProperty("view.toolbar.alternateLayout"))
		{
			getContentPane().add(BorderLayout.NORTH,topToolBars);
			getContentPane().add(BorderLayout.SOUTH,bottomToolBars);
			if(!plainView && jEdit.getBooleanProperty("view.status.visible"))
				addToolBar(BOTTOM_GROUP,STATUS_BAR_LAYER,status);
		}
		else
		{
			dockableWindowManager.add(topToolBars,
				DockableLayout.TOP_TOOLBARS,0);
			dockableWindowManager.add(bottomToolBars,
				DockableLayout.BOTTOM_TOOLBARS,0);
			if(!plainView && jEdit.getBooleanProperty("view.status.visible"))
				getContentPane().add(BorderLayout.SOUTH,status);
		}

		getRootPane().revalidate();

		if (splitPane != null)
			GUIUtilities.initContinuousLayout(splitPane);
		
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
			removeToolBar(searchBar);

		if(jEdit.getBooleanProperty("view.showSearchbar") && !plainView)
		{
			if(searchBar == null)
				searchBar = new SearchBar(this,false);
			searchBar.propertiesChanged();
			addToolBar(TOP_GROUP,SEARCH_BAR_LAYER,searchBar);
		}
	} 

	
	private EditPane createEditPane(Buffer buffer)
	{
		EditPane editPane = new EditPane(this,buffer);
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
			status.updateCaretStatus();
			status.updateBufferStatus();
			status.updateMiscStatus();
		}
	} 

	
	
	private void updateGutterBorders()
	{
		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
			editPanes[i].getTextArea().getGutter().updateBorder();
	} 

	

	

	
	class CaretHandler implements CaretListener
	{
		public void caretUpdate(CaretEvent evt)
		{
			if(evt.getSource() == getTextArea())
				status.updateCaretStatus();
		}
	} 

	
	class FocusHandler extends FocusAdapter
	{
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

	
	class ScrollHandler implements ScrollListener
	{
		public void scrolledVertically(TextArea textArea)
		{
			if(getTextArea() == textArea)
				status.updateCaretStatus();
		}

		public void scrolledHorizontally(TextArea textArea) {}
	} 

	
	class WindowHandler extends WindowAdapter
	{
		public void windowActivated(WindowEvent evt)
		{
			boolean editPaneChanged =
				jEdit.getActiveViewInternal() != View.this;
			jEdit.setActiveView(View.this);

			
			
			
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					jEdit.checkBufferStatus(View.this);
				}
			});

			if (editPaneChanged)
			{
				EditBus.send(new ViewUpdate(View.this,ViewUpdate
					.ACTIVATED));
			}
		}

		public void windowClosing(WindowEvent evt)
		{
			jEdit.closeView(View.this);
		}
	} 

	
	public static class ViewConfig
	{
		public boolean plainView;
		public String splitConfig;
		public int x, y, width, height, extState;

		
		public String top, left, bottom, right;
		public int topPos, leftPos, bottomPos, rightPos;

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
			extState = jEdit.getIntegerProperty(prefix + ".extendedState",NORMAL);
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

	
	static class MyFocusTraversalPolicy extends LayoutFocusTraversalPolicy
	{
		public Component getDefaultComponent(Container focusCycleRoot)
		{
			return GUIUtilities.getView(focusCycleRoot).getTextArea();
		}
	} 

	
}
