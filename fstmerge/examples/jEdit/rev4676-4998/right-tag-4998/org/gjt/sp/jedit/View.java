

package org.gjt.sp.jedit;


import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.net.Socket;
import java.util.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.search.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class View extends JFrame implements EBComponent
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

	
	
	public JToolBar getToolBar()
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

	
	
	public StatusBar getStatus()
	{
		return status;
	} 

	

	

	
	
	public KeyListener getKeyEventInterceptor()
	{
		return keyEventInterceptor;
	} 

	
	
	public void setKeyEventInterceptor(KeyListener listener)
	{
		this.keyEventInterceptor = listener;
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
		if(isClosed())
			return;

		if(getFocusOwner() instanceof JComponent)
		{
			JComponent comp = (JComponent)getFocusOwner();
			InputMap map = comp.getInputMap();
			ActionMap am = comp.getActionMap();

			if(map != null && am != null && comp.isEnabled())
			{
				Object binding = map.get(KeyStroke.getKeyStrokeForEvent(evt));
				if(binding != null && am.get(binding) != null)
				{
					return;
				}
			}
		}

		if(getFocusOwner() instanceof JTextComponent)
		{
			
			
			if(evt.getID() == KeyEvent.KEY_PRESSED)
			{
				switch(evt.getKeyCode())
				{
				case KeyEvent.VK_BACK_SPACE:
				case KeyEvent.VK_TAB:
				case KeyEvent.VK_ENTER:
					return;
				}
			}
		}

		if(evt.isConsumed())
			return;

		evt = KeyEventWorkaround.processKeyEvent(evt);
		if(evt == null)
			return;

		switch(evt.getID())
		{
		case KeyEvent.KEY_TYPED:
			
			if(keyEventInterceptor != null)
				;
			else if(inputHandler.isPrefixActive()
				&& !getTextArea().hasFocus())
				inputHandler.keyTyped(evt);
			break;
		case KeyEvent.KEY_PRESSED:
			if(keyEventInterceptor != null)
				keyEventInterceptor.keyPressed(evt);
			else
			{
				inputHandler.keyPressed(evt);
				
				
				
				if(inputHandler.isPrefixActive()
					&& getFocusOwner() instanceof JTextComponent)
					getTextArea().requestFocus();
			}
			break;
		case KeyEvent.KEY_RELEASED:
			if(keyEventInterceptor != null)
				keyEventInterceptor.keyReleased(evt);
			else
				inputHandler.keyReleased(evt);
			break;
		}

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
		editPane.saveCaretInfo();
		EditPane oldEditPane = editPane;
		setEditPane(createEditPane(oldEditPane.getBuffer()));
		editPane.loadCaretInfo();

		JComponent oldParent = (JComponent)oldEditPane.getParent();

		final JSplitPane newSplitPane = new JSplitPane(orientation);
		newSplitPane.setOneTouchExpandable(true);
		newSplitPane.setBorder(null);
		newSplitPane.setMinimumSize(new Dimension(0,0));

		int parentSize = (orientation == JSplitPane.VERTICAL_SPLIT
			? oldEditPane.getHeight() : oldEditPane.getWidth());
		final int dividerPosition = (int)((double)(parentSize
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
			this.splitPane = newSplitPane;

			newSplitPane.setLeftComponent(oldEditPane);
			newSplitPane.setRightComponent(editPane);

			oldParent.add(newSplitPane);
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
			EditPane[] editPanes = getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				EditPane _editPane = editPanes[i];
				if(editPane != _editPane)
					_editPane.close();
			}

			JComponent parent = (JComponent)splitPane.getParent();

			parent.remove(splitPane);
			parent.add(editPane);
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
				parent.add(editPane);
				splitPane = null;
			}

			parent.revalidate();

			updateTitle();

			editPane.focusOnTextArea();
		}
		else
			getToolkit().beep();
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
		if(editPane.getBuffer() == buffer)
		{
			editPane.focusOnTextArea();
			return editPane;
		}

		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			EditPane ep = editPanes[i];
			if(ep.getBuffer() == buffer)
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
			Vector vec = new Vector();
			getEditPanes(vec,splitPane);
			EditPane[] ep = new EditPane[vec.size()];
			vec.copyInto(ep);
			return ep;
		}
	} 

	
	
	public ViewConfig getViewConfig()
	{
		StringBuffer splitConfig = new StringBuffer();
		if(splitPane != null)
			getSplitConfig(splitPane,splitConfig);
		else
		{
			splitConfig.append(getBuffer().getIndex());
			splitConfig.append(" buffer");
		}

		ViewConfig config = new ViewConfig();
		config.plainView = isPlainView();
		config.splitConfig = splitConfig.toString();
		config.x = getX();
		config.y = getY();
		config.width = getWidth();
		config.height = getHeight();
		config.extState = GUIUtilities.getExtendedState(this);

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

	

	

	
	
	public boolean isSynchroScrollEnabled()
	{
		return synchroScroll;
	} 

	
	
	public void toggleSynchroScrollEnabled()
	{
		setSynchroScrollEnabled(!synchroScroll);
	} 

	
	
	public void setSynchroScrollEnabled(boolean synchroScroll)
	{
		this.synchroScroll = synchroScroll;
		JEditTextArea textArea = getTextArea();
		int firstLine = textArea.getFirstLine();
		int horizontalOffset = textArea.getHorizontalOffset();
		synchroScrollVertical(textArea,firstLine);
		synchroScrollHorizontal(textArea,horizontalOffset);
	} 

	
	
	public void synchroScrollVertical(JEditTextArea textArea, int firstLine)
	{
		if(!synchroScroll)
			return;

		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			if(editPanes[i].getTextArea() != textArea)
				editPanes[i].getTextArea().setFirstLine(firstLine);
		}
	} 

	
	
	public void synchroScrollHorizontal(JEditTextArea textArea, int horizontalOffset)
	{
		if(!synchroScroll)
			return;

		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			if(editPanes[i].getTextArea() != textArea)
				editPanes[i].getTextArea().setHorizontalOffset(horizontalOffset);
		}
	} 

	

	
	
	public void quickIncrementalSearch(boolean word)
	{
		if(searchBar == null)
			searchBar = new SearchBar(this,true);
		if(searchBar.getParent() == null)
			addToolBar(TOP_GROUP,SEARCH_BAR_LAYER,searchBar);

		JEditTextArea textArea = getTextArea();

		String text = textArea.getSelectedText();
		if(text == null && word)
		{
			textArea.selectWord();
			text = textArea.getSelectedText();
		}
		else if(text != null && text.indexOf('\n') != -1)
			text = null;

		searchBar.setHyperSearch(false);
		searchBar.getField().setText(text);
		searchBar.getField().requestFocus();
		searchBar.getField().selectAll();
	} 

	
	
	public void quickHyperSearch(boolean word)
	{
		JEditTextArea textArea = getTextArea();

		String text = textArea.getSelectedText();
		if(text == null && word)
		{
			textArea.selectWord();
			text = textArea.getSelectedText();
		}

		if(text != null && text.indexOf('\n') == -1)
		{
			HistoryModel.getModel("find").addItem(text);
			SearchAndReplace.setSearchString(text);
			SearchAndReplace.setSearchFileSet(new CurrentBufferSet());
			SearchAndReplace.hyperSearch(this);
		}
		else
		{
			if(searchBar == null)
				searchBar = new SearchBar(this,true);
			if(searchBar.getParent() == null)
				addToolBar(TOP_GROUP,SEARCH_BAR_LAYER,searchBar);

			searchBar.setHyperSearch(true);
			searchBar.getField().setText(null);
			searchBar.getField().requestFocus();
			searchBar.getField().selectAll();
		}
	} 

	
	
	public void actionBar()
	{
		if(actionBar == null)
			actionBar = new ActionBar(this,true);
		if(actionBar.getParent() == null)
			addToolBar(BOTTOM_GROUP,ACTION_BAR_LAYER,actionBar);

		actionBar.goToActionBar();
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
		return getClass().getName() + "["
			+ (jEdit.getActiveView() == this
			? "active" : "inactive")
			+ "]";
	} 

	
	View prev;
	View next;

	
	View(Buffer buffer, ViewConfig config)
	{
		this.plainView = config.plainView;

		enableEvents(AWTEvent.KEY_EVENT_MASK);

		setIconImage(GUIUtilities.getEditorIcon());

		dockableWindowManager = new DockableWindowManager(this,config);

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

		Component comp = restoreSplitConfig(buffer,config.splitConfig);
		dockableWindowManager.add(comp);

		getContentPane().add(BorderLayout.CENTER,dockableWindowManager);

		dockableWindowManager.init();

		
		
		propertiesChanged();

		if(!plainView)
		{
			if(config.top != null && config.top.length() != 0)
				dockableWindowManager.showDockableWindow(config.top);

			if(config.left != null && config.left.length() != 0)
				dockableWindowManager.showDockableWindow(config.left);

			if(config.bottom != null && config.bottom.length() != 0)
				dockableWindowManager.showDockableWindow(config.bottom);

			if(config.right != null && config.right.length() != 0)
				dockableWindowManager.showDockableWindow(config.right);
		}

		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowHandler());

		pack();

		EditBus.addToBus(this);
	} 

	
	void close()
	{
		GUIUtilities.saveGeometry(this,plainView ? "plain-view" : "view");
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

		
		if(waitSocket != null && !waitSocket.isClosed())
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

	
	
	void updateTitle()
	{
		Vector buffers = new Vector();
		EditPane[] editPanes = getEditPanes();
		for(int i = 0; i < editPanes.length; i++)
		{
			Buffer buffer = editPanes[i].getBuffer();
			if(buffers.indexOf(buffer) == -1)
				buffers.addElement(buffer);
		}

		StringBuffer title = new StringBuffer(jEdit.getProperty("view.title"));
		for(int i = 0; i < buffers.size(); i++)
		{
			if(i != 0)
				title.append(", ");

			Buffer buffer = (Buffer)buffers.elementAt(i);
			title.append((showFullPath && !buffer.isNewFile())
				? buffer.getPath() : buffer.getName());
			if(buffer.isDirty())
				title.append(jEdit.getProperty("view.title.dirty"));
		}
		setTitle(title.toString());
	} 

	

	

	
	private boolean closed;

	private DockableWindowManager dockableWindowManager;

	private JPanel topToolBars;
	private JPanel bottomToolBars;
	private ToolBarManager toolBarManager;

	private JToolBar toolBar;
	private SearchBar searchBar;
	private ActionBar actionBar;

	private boolean synchroScroll;

	private EditPane editPane;
	private JSplitPane splitPane;

	private StatusBar status;

	private KeyListener keyEventInterceptor;
	private InputHandler inputHandler;
	private Macros.Recorder recorder;

	private int waitCount;

	private boolean showFullPath;

	private boolean plainView;

	private Socket waitSocket;
	

	
	private void getEditPanes(Vector vec, Component comp)
	{
		if(comp instanceof EditPane)
			vec.addElement(comp);
		else if(comp instanceof JSplitPane)
		{
			JSplitPane split = (JSplitPane)comp;
			getEditPanes(vec,split.getLeftComponent());
			getEditPanes(vec,split.getRightComponent());
		}
	} 

	
	
	private void getSplitConfig(JSplitPane splitPane,
		StringBuffer splitConfig)
	{
		Component right = splitPane.getRightComponent();
		if(right instanceof JSplitPane)
			getSplitConfig((JSplitPane)right,splitConfig);
		else
		{
			splitConfig.append(((EditPane)right).getBuffer().getIndex());
			splitConfig.append(" buffer");
		}

		splitConfig.append(' ');

		Component left = splitPane.getLeftComponent();
		if(left instanceof JSplitPane)
			getSplitConfig((JSplitPane)left,splitConfig);
		else
		{
			splitConfig.append(((EditPane)left).getBuffer().getIndex());
			splitConfig.append(" buffer");
		}

		splitConfig.append(' ');
		splitConfig.append(splitPane.getDividerLocation());
		splitConfig.append(' ');
		splitConfig.append(splitPane.getOrientation()
			== JSplitPane.VERTICAL_SPLIT ? "vertical" : "horizontal");
	} 

	
	private Component restoreSplitConfig(Buffer buffer, String splitConfig)
	{
		if(buffer != null)
			return (editPane = createEditPane(buffer));
		else if(splitConfig == null)
			return (editPane = createEditPane(jEdit.getFirstBuffer()));

		Buffer[] buffers = jEdit.getBuffers();

		Stack stack = new Stack();

		StringTokenizer st = new StringTokenizer(splitConfig);

		while(st.hasMoreTokens())
		{
			String token = st.nextToken();
			if(token.equals("vertical") || token.equals("horizontal"))
			{
				int orientation = (token.equals("vertical")
					? JSplitPane.VERTICAL_SPLIT
					: JSplitPane.HORIZONTAL_SPLIT);
				int divider = Integer.parseInt((String)stack.pop());
				stack.push(splitPane = new JSplitPane(
					orientation,
					(Component)stack.pop(),
					(Component)stack.pop()));
				splitPane.setOneTouchExpandable(true);
				splitPane.setBorder(null);
				splitPane.setMinimumSize(new Dimension(0,0));
				splitPane.setDividerLocation(divider);
			}
			else if(token.equals("buffer"))
			{
				int index = Integer.parseInt((String)stack.pop());
				if(index < buffers.length && index > 0)
					buffer = buffers[index];
				if(buffer == null)
					buffer = jEdit.getFirstBuffer();

				stack.push(editPane = createEditPane(buffer));
			}
			else
			{
				stack.push(token);
			}
		}

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
			dockableWindowManager.add(DockableWindowManager.DockableLayout
				.TOP_TOOLBARS,topToolBars);
			dockableWindowManager.add(DockableWindowManager.DockableLayout
				.BOTTOM_TOOLBARS,bottomToolBars);
			if(!plainView && jEdit.getBooleanProperty("view.status.visible"))
				getContentPane().add(BorderLayout.SOUTH,status);
		}

		getRootPane().revalidate();

		
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
		if(msg.getWhat() == BufferUpdate.DIRTY_CHANGED)
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
		public void scrolledVertically(JEditTextArea textArea)
		{
			if(getTextArea() == textArea)
				status.updateCaretStatus();
		}

		public void scrolledHorizontally(JEditTextArea textArea) {}
	} 

	
	class WindowHandler extends WindowAdapter
	{
		public void windowActivated(WindowEvent evt)
		{
			jEdit.setActiveView(View.this);

			
			
			
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					jEdit.checkBufferStatus(View.this);
				}
			});
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
			String prefix = (plainView ? "plain-view" : "view");
			x = jEdit.getIntegerProperty(prefix + ".x",0);
			y = jEdit.getIntegerProperty(prefix + ".y",0);
			width = jEdit.getIntegerProperty(prefix + ".width",0);
			height = jEdit.getIntegerProperty(prefix + ".height",0);
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

	
}
