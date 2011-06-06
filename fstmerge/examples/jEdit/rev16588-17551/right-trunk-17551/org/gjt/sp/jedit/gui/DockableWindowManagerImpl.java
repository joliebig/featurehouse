

package org.gjt.sp.jedit.gui;


import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeMap;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;

import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.PluginJAR;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.EditBus.EBHandler;
import org.gjt.sp.jedit.View.ViewConfig;
import org.gjt.sp.jedit.msg.DockableWindowUpdate;
import org.gjt.sp.jedit.msg.PluginUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.util.IOUtilities;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;



public class DockableWindowManagerImpl extends DockableWindowManager
{
	
	
	public static class DockableWindowConfig extends DockingLayout
	{
		
		public class PerspectiveHandler extends DefaultHandler
		{
			public void startElement(String uri, String localName,
					 String qName, Attributes attrs)
			{
				for (int i = 0; i < attrs.getLength(); i++)
					attribute(attrs.getQName(i), attrs.getValue(i));
			}

			private void attribute(String aname, String value)
			{
				if(aname.equals("TOP"))
					top = value;
				else if(aname.equals("LEFT"))
					left = value;
				else if(aname.equals("BOTTOM"))
					bottom = value;
				else if(aname.equals("RIGHT"))
					right = value;
				else if(aname.equals("TOP_POS"))
					topPos = Integer.parseInt(value);
				else if(aname.equals("LEFT_POS"))
					leftPos = Integer.parseInt(value);
				else if(aname.equals("BOTTOM_POS"))
					bottomPos = Integer.parseInt(value);
				else if(aname.equals("RIGHT_POS"))
					rightPos = Integer.parseInt(value);
			}
		} 
		
		
		public String top, left, bottom, right;
		public int topPos, leftPos, bottomPos, rightPos;

		public DefaultHandler getPerspectiveHandler()
		{
			return new PerspectiveHandler();
		}

		public boolean saveLayout(String baseName, int viewIndex)
		{
			String lineSep = System.getProperty("line.separator");
			String filename = getLayoutFilename(baseName, viewIndex);
			BufferedWriter out = null;
			try
			{
				out = new BufferedWriter(new FileWriter(filename));
				out.write("<DOCKING LEFT=\"");
				out.write(left == null ? "" : left);
				out.write("\" TOP=\"");
				out.write(top == null ? "" : top);
				out.write("\" RIGHT=\"");
				out.write(right == null ? "" : right);
				out.write("\" BOTTOM=\"");
				out.write(bottom == null ? "" : bottom);
				out.write("\" LEFT_POS=\"");
				out.write(String.valueOf(leftPos));
				out.write("\" TOP_POS=\"");
				out.write(String.valueOf(topPos));
				out.write("\" RIGHT_POS=\"");
				out.write(String.valueOf(rightPos));
				out.write("\" BOTTOM_POS=\"");
				out.write(String.valueOf(bottomPos));
				out.write("\" />");
				out.write(lineSep);
			}
			catch (IOException e)
			{
				Log.log(Log.ERROR, this, e, e);
				return false;
			}
			finally
			{
				IOUtilities.closeQuietly(out);
			}
			return true;
		}

		@Override
		public boolean loadLayout(String baseName, int viewIndex)
		{
			String filename = getLayoutFilename(baseName, viewIndex);
			DefaultHandler handler = getPerspectiveHandler();
			try
			{
				
				XMLUtilities.parseXML(new FileInputStream(filename), handler);
			}
			catch (FileNotFoundException e)
			{
				return false;
			}
			catch (IOException e)
			{
				return false;
			}
			return true;
		}
		
		@Override
		public String getName()
		{
			return "DockableWindowManager";
		}

	} 

	
	
	private Map<String, Entry> windows;
	private PanelWindowContainer left;
	private PanelWindowContainer right;
	private PanelWindowContainer top;
	private PanelWindowContainer bottom;
	private List<Entry> clones;
	private Entry lastEntry;
	public Stack<String> showStack = new Stack<String>();
	

	
	public void setDockingLayout(DockingLayout docking)
	{
		DockableWindowConfig config = (DockableWindowConfig) docking;
		if (config == null)
			return;
		if(config.top != null && config.top.length() != 0)
				showDockableWindow(config.top);

		if(config.left != null && config.left.length() != 0)
				showDockableWindow(config.left);

		if(config.bottom != null && config.bottom.length() != 0)
				showDockableWindow(config.bottom);

		if(config.right != null && config.right.length() != 0)
				showDockableWindow(config.right);
		
	} 
	
	
	@Override
	public DockingLayout getDockingLayout(ViewConfig config)
	{
		DockableWindowConfig docking = new DockableWindowConfig();
		
		docking.top = getTopDockingArea().getCurrent();
		docking.left = getLeftDockingArea().getCurrent();
		docking.bottom = getBottomDockingArea().getCurrent();
		docking.right = getRightDockingArea().getCurrent();

		docking.topPos = getTopDockingArea().getDimension();
		docking.leftPos = getLeftDockingArea().getDimension();
		docking.bottomPos = getBottomDockingArea().getDimension();
		docking.rightPos = getRightDockingArea().getDimension();
		return docking;
	} 

	
	
	public DockableWindowManagerImpl(View view, DockableWindowFactory factory,
		View.ViewConfig config)
	{
		super(view, factory, config);
		setLayout(new DockableLayout());

		windows = new HashMap<String, Entry>();
		clones = new ArrayList<Entry>();

		DockableWindowConfig docking = (DockableWindowConfig) config.docking;
		if (docking == null)
			docking = new DockableWindowConfig();
		top = new PanelWindowContainer(this,TOP,docking.topPos);
		left = new PanelWindowContainer(this,LEFT,docking.leftPos);
		bottom = new PanelWindowContainer(this,BOTTOM,docking.bottomPos);
		right = new PanelWindowContainer(this,RIGHT,docking.rightPos);

		add(DockableLayout.TOP_BUTTONS,top.buttonPanel);
		add(DockableLayout.LEFT_BUTTONS,left.buttonPanel);
		add(DockableLayout.BOTTOM_BUTTONS,bottom.buttonPanel);
		add(DockableLayout.RIGHT_BUTTONS,right.buttonPanel);

		add(TOP,top.dockablePanel);
		add(LEFT,left.dockablePanel);
		add(BOTTOM,bottom.dockablePanel);
		add(RIGHT,right.dockablePanel);
	} 

	
	public void setMainPanel(JPanel panel) 
	{
		add(panel, 0);
	} 

	
	
	
	public void init()
	{
		super.init();
		Iterator<DockableWindowFactory.Window> entries = factory.getDockableWindowIterator();

		while(entries.hasNext())
			addEntry(entries.next());

		propertiesChanged();
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
		entry.container.show(null);
	} 

	
	
	public JComponent getDockable(String name)
	{
		Entry entry = windows.get(name);
		if(entry == null || entry.win == null)
			return null;
		else
			return entry.win;
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
				
				try
				{
					String dockableName = showStack.pop();
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
						
						container.show((DockableWindowManagerImpl.Entry) null);
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
		super.close();

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
								DockableWindowManagerImpl.this,
								DockableWindowUpdate.PROPERTIES_CHANGED,
								dockable
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
							DockableWindowManagerImpl.this,
							DockableWindowUpdate.PROPERTIES_CHANGED,
							dockable
						));
						
						
						Entry entry = windows.get(dockable);
						if (entry == null)
							Log.log(Log.ERROR,this,"Unknown dockable window: " + dockable);
						else
							entry.win = null;
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

	
	@EBHandler
	public void handleDockableWindowUpdate(DockableWindowUpdate msg)
	{
		if (msg.getWhat() == DockableWindowUpdate.PROPERTIES_CHANGED)
			propertiesChanged();
	} 

	
	@EBHandler
	public void handlePropertiesChanged(PropertiesChanged msg)
	{
		propertiesChanged();
	} 

	
	@EBHandler
	public void handlePluginUpdate(PluginUpdate pmsg)
	{
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

	

	
	protected void propertiesChanged()
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
				if (entry.factory.movable && (! newPosition.equals(FLOATING)))
				{
					if (entry.win instanceof DockableWindow)
						((DockableWindow)entry.win).move(newPosition);
				}
				else
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

	
	static class Entry
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

		
		public String label()
		{
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

	@Override
	public void disposeDockableWindow(String name)
	{
		
		
	}
}
