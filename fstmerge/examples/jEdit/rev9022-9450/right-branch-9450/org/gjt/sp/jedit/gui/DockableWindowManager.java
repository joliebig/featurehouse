

package org.gjt.sp.jedit.gui;


import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;

import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.PluginJAR;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.gui.KeyEventTranslator.Key;
import org.gjt.sp.jedit.msg.DockableWindowUpdate;
import org.gjt.sp.jedit.msg.PluginUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.util.Log;



public class DockableWindowManager extends JPanel implements EBComponent
{
	
	
	public static final String FLOATING = "floating";
	
	
	public static final String TOP = "top";

	
	public static final String LEFT = "left";

	
	public static final String BOTTOM = "bottom";

	
	public static final String RIGHT = "right";
	
	
	
	private View view;
	private DockableWindowFactory factory;

	
	private Map<String, Entry> windows;
	private PanelWindowContainer left;
	private PanelWindowContainer right;
	private PanelWindowContainer top;
	private PanelWindowContainer bottom;
	private List<Entry> clones;
	
	private Entry lastEntry;
	public Stack showStack = new Stack();
	
	
	
	
	public static String[] getRegisteredDockableWindows()
	{
		return DockableWindowFactory.getInstance()
			.getRegisteredDockableWindows();
	} 

	
	
	public DockableWindowManager(View view, DockableWindowFactory factory,
		View.ViewConfig config)
	{
		setLayout(new DockableLayout());
		this.view = view;
		this.factory = factory;

		windows = new HashMap<String, Entry>();
		clones = new ArrayList<Entry>();

		top = new PanelWindowContainer(this,TOP,config.topPos);
		left = new PanelWindowContainer(this,LEFT,config.leftPos);
		bottom = new PanelWindowContainer(this,BOTTOM,config.bottomPos);
		right = new PanelWindowContainer(this,RIGHT,config.rightPos);

		add(DockableLayout.TOP_BUTTONS,top.buttonPanel);
		add(DockableLayout.LEFT_BUTTONS,left.buttonPanel);
		add(DockableLayout.BOTTOM_BUTTONS,bottom.buttonPanel);
		add(DockableLayout.RIGHT_BUTTONS,right.buttonPanel);

		add(TOP,top.dockablePanel);
		add(LEFT,left.dockablePanel);
		add(BOTTOM,bottom.dockablePanel);
		add(RIGHT,right.dockablePanel);
	} 

	
	
	public void init()
	{
		EditBus.addToBus(this);

		Iterator<DockableWindowFactory.Window> entries = factory.getDockableWindowIterator();

		while(entries.hasNext())
			addEntry(entries.next());

		propertiesChanged();
	} 

	
	
	public KeyListener closeListener(String dockableName) {
		return new KeyHandler(dockableName);
	}
	
	
	
	
	public View getView()
	{
		return view;
	} 

	
	
	public JComponent floatDockableWindow(String name)
	{
		Entry entry = windows.get(name);
		if(entry == null)
		{
			Log.log(Log.ERROR,this,"Unknown dockable window: " + name);
			return null;
		}
		
		
		Entry newEntry = new Entry(entry.factory,FLOATING);
		newEntry.win = newEntry.factory.createDockableWindow(view,FLOATING);
		
		if(newEntry.win != null)
		{
			FloatingWindowContainer fwc = new FloatingWindowContainer(this,true); 
			newEntry.container = fwc;
			newEntry.container.register(newEntry);
			newEntry.container.show(newEntry);
			
			
		}
		clones.add(newEntry);
		return newEntry.win;
	} 

	
	
	public void showDockableWindow(String name)
	{
		lastEntry = windows.get(name);
		if(lastEntry == null)
		{
			Log.log(Log.ERROR,this,"Unknown dockable window: " + name);
			return;
		}

		if(lastEntry.win == null)
		{
			lastEntry.win = lastEntry.factory.createDockableWindow(
				view,lastEntry.position);
		}

		if(lastEntry.win != null)
		{
			if(lastEntry.position.equals(FLOATING)
				&& lastEntry.container == null)
			{
				FloatingWindowContainer fwc = new FloatingWindowContainer(
					this,view.isPlainView()); 
				lastEntry.container = fwc;
				lastEntry.container.register(lastEntry);
			}
			showStack.push(name);
			lastEntry.container.show(lastEntry);
			Object reason = DockableWindowUpdate.ACTIVATED;
			EditBus.send(new DockableWindowUpdate(this, reason, name));
		}
		else
			;
	} 

	
	
	public void addDockableWindow(String name)
	{
		showDockableWindow(name);
	} 

	
	
	public void hideDockableWindow(String name)
	{

		Entry entry = windows.get(name);
		if(entry == null)
		{
			Log.log(Log.ERROR,this,"Unknown dockable window: " + name);
			return;
		}



		if(entry.win == null)
			return;
		Object reason = DockableWindowUpdate.DEACTIVATED;
		EditBus.send(new DockableWindowUpdate(this, reason, name));

		entry.container.show(null);
	} 

	
	
	public void removeDockableWindow(String name)
	{
		hideDockableWindow(name);
	} 

	
	
	public void toggleDockableWindow(String name)
	{
		if(isDockableWindowVisible(name))
			removeDockableWindow(name);
		else
			addDockableWindow(name);
	} 

	
	
	public JComponent getDockableWindow(String name)
	{
		return getDockable(name);
	} 

	
	
	public JComponent getDockable(String name)
	{
		
		Entry entry = windows.get(name);
		if(entry == null || entry.win == null)
			return null;
		else
			return entry.win;
	} 

	
	
	public String getDockableTitle(String name)
	{
		Entry e = windows.get(name);
		return e.longTitle();
	} 

	
	
	public void  setDockableTitle(String dockableName, String newTitle) {
		Entry entry = windows.get(dockableName);
		String propName = entry.factory.name + ".longtitle";
		String oldTitle = jEdit.getProperty(propName);
		jEdit.setProperty(propName, newTitle);
		firePropertyChange(propName, oldTitle, newTitle);
	}
	
	
	
	
	public boolean isDockableWindowVisible(String name)
	{
		Entry entry = windows.get(name);
		if(entry == null || entry.win == null)
			return false;
		else
			return entry.container.isVisible(entry);
	} 

	
	
	public boolean isDockableWindowDocked(String name)
	{
		Entry entry = windows.get(name);
		if(entry == null)
			return false;
		else
			return !entry.position.equals(FLOATING);
	} 

	
	
	public void closeCurrentArea()
	{
		
		
		
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				
				try {
					String dockableName = showStack.pop().toString(); 
					hideDockableWindow(dockableName);
					return;
				}
				catch (Exception e) {}
				
				Component comp = view.getFocusOwner();
				while(comp != null)
				{
					
					if(comp instanceof DockablePanel)
					{
						DockablePanel panel = (DockablePanel) comp;
						
						PanelWindowContainer container = panel.getWindowContainer();
						
						container.show(null);
						return;
					}

					comp = comp.getParent();
				}

				getToolkit().beep();
			}
		});
	} 

	
	
	public void close()
	{
		EditBus.removeFromBus(this);

		for (Entry entry : windows.values())
		{
			if (entry.win != null)
				entry.container.unregister(entry);
		}

		for (Entry clone : clones)
		{
			if (clone.win != null)
				clone.container.unregister(clone);
		}
	} 

	
	public PanelWindowContainer getTopDockingArea()
	{
		return top;
	} 

	
	public PanelWindowContainer getLeftDockingArea()
	{
		return left;
	} 

	
	public PanelWindowContainer getBottomDockingArea()
	{
		return bottom;
	} 

	
	public PanelWindowContainer getRightDockingArea()
	{
		return right;
	} 

	
	public JPopupMenu createPopupMenu(
		final DockableWindowContainer container,
		final String dockable,
		final boolean clone)
	{
		JPopupMenu popup = new JPopupMenu();
		if(dockable == null && container instanceof PanelWindowContainer)
		{
			ActionListener listener = new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					showDockableWindow(evt.getActionCommand());
				}
			};

			String[] dockables = ((PanelWindowContainer)
				container).getDockables();
			Map<String,String> dockableMap = new TreeMap<String, String>();
			for (int i = 0; i < dockables.length; i++)
			{
				String action = dockables[i];
				dockableMap.put(getDockableTitle(action), action);
			}
			for (Map.Entry<String, String> entry : dockableMap.entrySet())
			{
				JMenuItem item = new JMenuItem(entry.getKey());
				item.setActionCommand(entry.getValue());
				item.addActionListener(listener);
				popup.add(item);
			}
		}
		else
		{
			JMenuItem caption = new JMenuItem(getDockableTitle(dockable));
			caption.setEnabled(false);
			popup.add(caption);
			popup.addSeparator();
			String currentPos = jEdit.getProperty(dockable + ".dock-position",FLOATING);
			if(!clone)
			{
				String[] positions = { FLOATING, TOP, LEFT, BOTTOM, RIGHT };
				for(int i = 0; i < positions.length; i++)
				{
					final String pos = positions[i];
					if(pos.equals(currentPos))
						continue;

					JMenuItem moveMenuItem = new JMenuItem(jEdit.getProperty("view.docking.menu-"
						+ pos));

					moveMenuItem.addActionListener(new ActionListener()
					{
						public void actionPerformed(ActionEvent evt)
						{
							jEdit.setProperty(dockable + ".dock-position",pos);
							EditBus.send(new DockableWindowUpdate(
								DockableWindowManager.this,
								DockableWindowUpdate.PROPERTIES_CHANGED,
								null
							));
							showDockableWindow(dockable);
						}
					});
					popup.add(moveMenuItem);
				}

				popup.addSeparator();
			}

			JMenuItem cloneMenuItem = new JMenuItem(jEdit.getProperty("view.docking.menu-clone"));

			cloneMenuItem.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					floatDockableWindow(dockable);
				}
			});
			popup.add(cloneMenuItem);

			popup.addSeparator();

			JMenuItem closeMenuItem = new JMenuItem(jEdit.getProperty("view.docking.menu-close"));

			closeMenuItem.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					if(clone)
						((FloatingWindowContainer)container).dispose();
					else
						removeDockableWindow(dockable);
				}
			});
			popup.add(closeMenuItem);

			if(!(clone || currentPos.equals(FLOATING)))
			{
				JMenuItem undockMenuItem = new JMenuItem(jEdit.getProperty("view.docking.menu-undock"));

				undockMenuItem.addActionListener(new ActionListener()
				{
					public void actionPerformed(ActionEvent evt)
					{
						jEdit.setProperty(dockable + ".dock-position",FLOATING);
						EditBus.send(new DockableWindowUpdate(
							DockableWindowManager.this,
							DockableWindowUpdate.PROPERTIES_CHANGED,
							null
						));
					}
				});
				popup.add(undockMenuItem);
			}
		}

		return popup;
	} 

	
	public void paintChildren(Graphics g)
	{
		super.paintChildren(g);

		if(resizeRect != null)
		{
			g.setColor(Color.darkGray);
			g.fillRect(resizeRect.x,resizeRect.y,
				resizeRect.width,resizeRect.height);
		}
	} 

	
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof DockableWindowUpdate)
		{
			if(((DockableWindowUpdate)msg).getWhat()
				== DockableWindowUpdate.PROPERTIES_CHANGED)
				propertiesChanged();
		}
		else if(msg instanceof PropertiesChanged)
			propertiesChanged();
		else if(msg instanceof PluginUpdate)
		{
			PluginUpdate pmsg = (PluginUpdate)msg;
			if(pmsg.getWhat() == PluginUpdate.LOADED)
			{
				Iterator<DockableWindowFactory.Window> iter = factory.getDockableWindowIterator();

				while(iter.hasNext())
				{
					DockableWindowFactory.Window w = iter.next();
					if(w.plugin == pmsg.getPluginJAR())
						addEntry(w);
				}

				propertiesChanged();
			}
			else if(pmsg.isExiting())
			{
				
			}
			else if(pmsg.getWhat() == PluginUpdate.DEACTIVATED)
			{
				Iterator<Entry> iter = getAllPluginEntries(
					pmsg.getPluginJAR(),false);
				while(iter.hasNext())
				{
					Entry entry = iter.next();
					if(entry.container != null)
						entry.container.remove(entry);
				}
			}
			else if(pmsg.getWhat() == PluginUpdate.UNLOADED)
			{
				Iterator<Entry> iter = getAllPluginEntries(
					pmsg.getPluginJAR(),true);
				while(iter.hasNext())
				{
					Entry entry = iter.next();
					if(entry.container != null)
					{
						entry.container.unregister(entry);
						entry.win = null;
						entry.container = null;
					}
				}
			}
		}
	} 

	
	int resizePos;
	
	Rectangle resizeRect;

	
	void setResizePos(int resizePos, PanelWindowContainer resizing)
	{
		this.resizePos = resizePos;

		if(resizePos < 0)
			resizePos = 0;

		if (continuousLayout)
			return;

		Rectangle newResizeRect = new Rectangle(0,0,
			PanelWindowContainer.SPLITTER_WIDTH - 2,
			PanelWindowContainer.SPLITTER_WIDTH - 2);
		if(resizing == top)
		{
			resizePos = Math.min(resizePos,getHeight()
				- top.buttonPanel.getHeight()
				- bottom.dockablePanel.getHeight()
				- bottom.buttonPanel.getHeight()
				- PanelWindowContainer.SPLITTER_WIDTH);
			newResizeRect.x = top.dockablePanel.getX() + 1;
			newResizeRect.y = resizePos + top.buttonPanel.getHeight() + 1;
			newResizeRect.width = top.dockablePanel.getWidth() - 2;
		}
		else if(resizing == left)
		{
			resizePos = Math.min(resizePos,getWidth()
				- left.buttonPanel.getWidth()
				- right.dockablePanel.getWidth()
				- right.buttonPanel.getWidth()
				- PanelWindowContainer.SPLITTER_WIDTH);
			newResizeRect.x = resizePos + left.buttonPanel.getWidth() + 1;
			newResizeRect.y = left.dockablePanel.getY() + 1;
			newResizeRect.height = left.dockablePanel.getHeight() - 2;
		}
		else if(resizing == bottom)
		{
			resizePos = Math.min(resizePos,getHeight()
				- bottom.buttonPanel.getHeight()
				- top.dockablePanel.getHeight()
				- top.buttonPanel.getHeight()
				- PanelWindowContainer.SPLITTER_WIDTH);
			newResizeRect.x = bottom.dockablePanel.getX() + 1;
			newResizeRect.y = getHeight() - bottom.buttonPanel.getHeight() - resizePos
				- PanelWindowContainer.SPLITTER_WIDTH + 2;
			newResizeRect.width = bottom.dockablePanel.getWidth() - 2;
		}
		else if(resizing == right)
		{
			resizePos = Math.min(resizePos,getWidth()
				- right.buttonPanel.getWidth()
				- left.dockablePanel.getWidth()
				- left.buttonPanel.getWidth()
				- PanelWindowContainer.SPLITTER_WIDTH);
			newResizeRect.x = getWidth() - right.buttonPanel.getWidth() - resizePos
				- PanelWindowContainer.SPLITTER_WIDTH + 1;
			newResizeRect.y = right.dockablePanel.getY() + 1;
			newResizeRect.height = right.dockablePanel.getHeight() - 2;
		}

		Rectangle toRepaint;
		if(resizeRect == null)
			toRepaint = newResizeRect;
		else
			toRepaint = resizeRect.union(newResizeRect);
		resizeRect = newResizeRect;
		repaint(toRepaint);
	} 

	
	void finishResizing()
	{
		resizeRect = null;
		repaint();
	} 

	

	
	private void propertiesChanged()
	{
		if(view.isPlainView())
			return;

		((DockableLayout)getLayout()).setAlternateLayout(
			jEdit.getBooleanProperty("view.docking.alternateLayout"));

		String[] windowList = factory.getRegisteredDockableWindows();

		for(int i = 0; i < windowList.length; i++)
		{
			String dockable = windowList[i];
			Entry entry = windows.get(dockable);

			String newPosition = jEdit.getProperty(dockable
				+ ".dock-position",FLOATING);
			if(newPosition.equals(entry.position))
			{
				continue;
			}

			entry.position = newPosition;
			if(entry.container != null)
			{
				entry.container.unregister(entry);
				entry.container = null;
				entry.win = null;
			}

			if(newPosition.equals(FLOATING)) 
			{
			}
				
			else
			{
				if(newPosition.equals(TOP))
					entry.container = top;
				else if(newPosition.equals(LEFT))
					entry.container = left;
				else if(newPosition.equals(BOTTOM))
					entry.container = bottom;
				else if(newPosition.equals(RIGHT))
					entry.container = right;
				else
				{
					Log.log(Log.WARNING,this,
						"Unknown position: "
						+ newPosition);
					continue;
				}

				entry.container.register(entry);
			}
		}

		top.sortDockables();
		left.sortDockables();
		bottom.sortDockables();
		right.sortDockables();

		continuousLayout = jEdit.getBooleanProperty("appearance.continuousLayout");
		revalidate();
		repaint();
	} 

	
	private void addEntry(DockableWindowFactory.Window factory)
	{
		Entry e;
		if(view.isPlainView())
		{
			
			e = new Entry(factory,FLOATING);
		}
		else
		{
			e = new Entry(factory);
			if(e.position.equals(FLOATING))
				;
			else if(e.position.equals(TOP))
				e.container = top;
			else if(e.position.equals(LEFT))
				e.container = left;
			else if(e.position.equals(BOTTOM))
				e.container = bottom;
			else if(e.position.equals(RIGHT))
				e.container = right;
			else
			{
				Log.log(Log.WARNING,this,
					"Unknown position: "
					+ e.position);
			}

			if(e.container != null)
				e.container.register(e);
		}
		windows.put(factory.name,e);
	} 

	
	
	private Iterator<Entry> getAllPluginEntries(PluginJAR plugin, boolean remove)
	{
		List<Entry> returnValue = new LinkedList<Entry>();
		Iterator<Entry> iter = windows.values().iterator();
		while(iter.hasNext())
		{
			Entry entry = iter.next();
			if(entry.factory.plugin == plugin)
			{
				returnValue.add(entry);
				if(remove)
					iter.remove();
			}
		}

		iter = clones.iterator();
		while(iter.hasNext())
		{
			Entry entry = iter.next();
			if(entry.factory.plugin == plugin)
			{
				returnValue.add(entry);
				iter.remove();
			}
		}

		return returnValue.iterator();
	} 

	private boolean continuousLayout;

	
	class Entry
	{
		DockableWindowFactory.Window factory;


		String position;
		DockableWindowContainer container;

		
		JComponent win;

		
		AbstractButton btn;

		
		Entry(DockableWindowFactory.Window factory)
		{
			this(factory,jEdit.getProperty(factory.name
				+ ".dock-position",FLOATING));
		} 

		
		
		public String longTitle() 
		{
			String title = jEdit.getProperty(factory.name + ".longtitle");
			if (title == null) return shortTitle();
			else return title;
			
		}
		
		
		public String shortTitle() 
		{
			
			String title = jEdit.getProperty(factory.name + ".title");
			if(title == null)
				return "NO TITLE PROPERTY: " + factory.name;
			else
				return title;
		}
		
		public String label() {
			String retval = jEdit.getProperty(factory.name + ".label");
			retval = retval.replaceAll("\\$", "");
			return retval; 
		}
		
		Entry(DockableWindowFactory.Window factory, String position)
		{
			this.factory = factory;
			this.position = position;

			
			
			
			
		} 
	} 

	
	class KeyHandler extends KeyAdapter {
		static final String action = "close-docking-area";  
		Key b1, b2;
		String name;
		
		public KeyHandler(String dockableName) {
			String shortcut1=jEdit.getProperty(action + ".shortcut");
			String shortcut2=jEdit.getProperty(action + ".shortcut2");
			if (shortcut1 != null)
				b1 = KeyEventTranslator.parseKey(shortcut1);
			if (shortcut2 != null)
				b2 = KeyEventTranslator.parseKey(shortcut2);
			name = dockableName;
		}
		public void keyTyped(KeyEvent e)
		{
			char cc = e.getKeyChar();
			if ((b1 != null && cc == b1.key) ||
			     (b2 != null && cc == b2.key)) 
				hideDockableWindow(name);
		}


	}
	
}
