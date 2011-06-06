package org.gjt.sp.jedit.gui;


import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.Map.Entry;

import javax.swing.JComponent;
import javax.swing.JPanel;

import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.PluginJAR;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.View.ViewConfig;
import org.gjt.sp.jedit.gui.KeyEventTranslator.Key;
import org.gjt.sp.jedit.msg.DockableWindowUpdate;
import org.gjt.sp.jedit.msg.PluginUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.util.Log;


@SuppressWarnings("serial")


 public abstract class DockableWindowManager extends JPanel implements EBComponent
{

	
	
	public static final String FLOATING = "floating";

	
	public static final String TOP = "top";

	
	public static final String LEFT = "left";

	
	public static final String BOTTOM = "bottom";

	
	public static final String RIGHT = "right";
	

	
	private final Map<PluginJAR, Set<String>> plugins = new HashMap<PluginJAR, Set<String>>(); 
	private final Map<String, String> positions = new HashMap<String, String>();
	protected View view;
	protected DockableWindowFactory factory;
	protected Map<String, JComponent> windows = new HashMap<String, JComponent>();

	
	private boolean tBottom, tTop, tLeft, tRight;
	private boolean closeToggle = true;

	private static final String ALTERNATE_LAYOUT_PROP = "view.docking.alternateLayout";
	private boolean alternateLayout;
	

	
	public DockableWindowManager(View view, DockableWindowFactory instance,
			ViewConfig config)
	{
		this.view = view;
		this.factory = instance;
		alternateLayout = jEdit.getBooleanProperty(ALTERNATE_LAYOUT_PROP);
	} 

	
	public abstract void setMainPanel(JPanel panel);
	public abstract void showDockableWindow(String name);
	public abstract void hideDockableWindow(String name);

	
	public abstract void disposeDockableWindow(String name);
	public abstract JComponent floatDockableWindow(String name);
	public abstract boolean isDockableWindowDocked(String name);
	public abstract boolean isDockableWindowVisible(String name);
	public abstract void closeCurrentArea();
	public abstract DockingLayout getDockingLayout(ViewConfig config);
	public abstract DockingArea getLeftDockingArea();
	public abstract DockingArea getRightDockingArea();
	public abstract DockingArea getTopDockingArea();
	public abstract DockingArea getBottomDockingArea();
	

	
	
	public void init()
	{
		EditBus.addToBus(this);

		Iterator<DockableWindowFactory.Window> entries = factory.getDockableWindowIterator();
		while(entries.hasNext())
		{
			DockableWindowFactory.Window window = entries.next();
			String dockable = window.name;
			positions.put(dockable, getDockablePosition(dockable));
			addPluginDockable(window.plugin, dockable);
		}
	} 

	
	public void close()
	{
		EditBus.removeFromBus(this);
	} 

	
	public void applyDockingLayout(DockingLayout docking)
	{
		
		Iterator<Entry<String, String>> iterator = positions.entrySet().iterator();
		while (iterator.hasNext())
		{
			Entry<String, String> entry = iterator.next();
			String dockable = entry.getKey();
			String position = entry.getValue();
			if (! position.equals(FLOATING))
				showDockableWindow(dockable);
		}
	} 

	
	
	public void addDockableWindow(String name)
	{
		showDockableWindow(name);
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

	
	
	public void toggleDockAreas()
	{
		if (closeToggle)
		{
			tTop = getTopDockingArea().getCurrent() != null;
			tLeft = getLeftDockingArea().getCurrent() != null;
			tRight = getRightDockingArea().getCurrent() != null;
			tBottom = getBottomDockingArea().getCurrent() != null;
			getBottomDockingArea().show(null);
			getTopDockingArea().show(null);
			getRightDockingArea().show(null);
			getLeftDockingArea().show(null);
		}
		else
		{
			if (tBottom) getBottomDockingArea().showMostRecent();
			if (tLeft) getLeftDockingArea().showMostRecent();
			if (tRight) getRightDockingArea().showMostRecent();
			if (tTop) getTopDockingArea().showMostRecent();
		}
		closeToggle = !closeToggle;
		view.getTextArea().requestFocus();
	} 

	
	public void dockableTitleChanged(String dockable, String newTitle)
	{
	} 

	
	
	public KeyListener closeListener(String dockableName)
	{
		return new KeyHandler(dockableName);
	}
	

	
	
	public View getView()
	{
		return view;
	} 

	
	
	public JComponent getDockable(String name)
	{
		return windows.get(name);
	} 

	
	
	public String getDockableTitle(String name)
	{
		return longTitle(name);
	}

	
	
	public void setDockableTitle(String dockable, String title)
	{
		String propName = getLongTitlePropertyName(dockable);
		String oldTitle = jEdit.getProperty(propName);
		jEdit.setProperty(propName, title);
		firePropertyChange(propName, oldTitle, title);
		dockableTitleChanged(dockable, title);
	}
	

	
	public static String[] getRegisteredDockableWindows()
	{
		return DockableWindowFactory.getInstance()
			.getRegisteredDockableWindows();
	} 

	
	public static String getDockableWindowPluginName(String name)
	{
		String pluginClass =
			DockableWindowFactory.getInstance().getDockableWindowPluginClass(name);
		if (pluginClass == null)
			return null;
		return jEdit.getProperty("plugin." + pluginClass + ".name");
	} 

	
	public void setDockingLayout(DockingLayout docking)
	{
		applyDockingLayout(docking);
		applyAlternateLayout(alternateLayout);
	} 

	
	private void addPluginDockable(PluginJAR plugin, String name)
	{
		Set<String> dockables = plugins.get(plugin);
		if (dockables == null)
		{
			dockables = new HashSet<String>();
			plugins.put(plugin, dockables);
		}
		dockables.add(name);
	}
	
	
	
	public void handleMessage(EBMessage msg)
	{
		if (msg instanceof DockableWindowUpdate)
		{
			if(((DockableWindowUpdate)msg).getWhat() == DockableWindowUpdate.PROPERTIES_CHANGED)
				propertiesChanged();
		}
		else if (msg instanceof PropertiesChanged)
			propertiesChanged();
		else if(msg instanceof PluginUpdate)
		{
			PluginUpdate pmsg = (PluginUpdate)msg;
			if (pmsg.getWhat() == PluginUpdate.LOADED)
			{
				Iterator<DockableWindowFactory.Window> iter = factory.getDockableWindowIterator();
				while (iter.hasNext())
				{
					DockableWindowFactory.Window w = iter.next();
					if (w.plugin == pmsg.getPluginJAR())
					{
						String position = getDockablePosition(w.name);
						positions.put(w.name, position);
						addPluginDockable(w.plugin, w.name);
						dockableLoaded(w.name, position);
					}
				}
				propertiesChanged();
			}
			else if(pmsg.isExiting())
			{
				
			}
			else if(pmsg.getWhat() == PluginUpdate.DEACTIVATED ||
					pmsg.getWhat() == PluginUpdate.UNLOADED)
			{
				Set<String> dockables = plugins.remove(pmsg.getPluginJAR());
				if (dockables != null)
				{
					for (String dockable: dockables)
					{
						disposeDockableWindow(dockable);
						windows.remove(dockable);
					}
				}
			}
		}
	} 

	
	public String longTitle(String name)
	{
		String title = jEdit.getProperty(getLongTitlePropertyName(name));
		if (title == null)
			return shortTitle(name);
		return title;
	} 

	
	public String shortTitle(String name)
	{
		String title = jEdit.getProperty(name + ".title");
		if(title == null)
			return "NO TITLE PROPERTY: " + name;
		return title;
	} 

	

	
	
	protected void applyAlternateLayout(boolean alternateLayout)
	{
	} 

	
	protected void dockableLoaded(String dockableName, String position)
	{
	}
	
	
	
	protected void dockingPositionChanged(String dockableName,
		String oldPosition, String newPosition)
	{
	} 

	
	protected boolean getAlternateLayoutProp()
	{
		return alternateLayout;
	} 

	
	protected void propertiesChanged()
	{
		if(view.isPlainView())
			return;

		boolean newAlternateLayout = jEdit.getBooleanProperty(ALTERNATE_LAYOUT_PROP);
		if (newAlternateLayout != alternateLayout)
		{
			alternateLayout = newAlternateLayout;
			applyAlternateLayout(newAlternateLayout);
		}

		String[] dockables = factory.getRegisteredDockableWindows();
		for(int i = 0; i < dockables.length; i++)
		{
			String dockable = dockables[i];
			String oldPosition = positions.get(dockable);
			String newPosition = getDockablePosition(dockable);
			if (oldPosition == null || !newPosition.equals(oldPosition))
			{
				positions.put(dockable, newPosition);
				dockingPositionChanged(dockable, oldPosition, newPosition);
			}
		}

	} 

	
	protected JComponent createDockable(String name)
	{
		DockableWindowFactory.Window wf = factory.getDockableWindowFactory(name);
		if (wf == null)
		{
			Log.log(Log.ERROR,this,"Unknown dockable window: " + name);
			return null;
		}
		String position = getDockablePosition(name);
		JComponent window = wf.createDockableWindow(view, position);
		if (window != null)
			windows.put(name, window);
		return window;
	} 

	
	protected String getDockablePosition(String name)
	{
		return jEdit.getProperty(name + ".dock-position", FLOATING);
	} 

	
	protected void focusDockable(String name)
	{
		JComponent c = getDockable(name);
		if (c == null)
			return;
		if (c instanceof DefaultFocusComponent)
			((DefaultFocusComponent)c).focusOnDefaultComponent();
		else
			c.requestFocus();
	} 

	
	protected String getLongTitlePropertyName(String dockableName)
	{
		return dockableName + ".longtitle";
	} 
	


	
	
	public interface DockingArea
	{
		void showMostRecent();
		String getCurrent();
		void show(String name);
		String [] getDockables();
	}
	

	
	
	class KeyHandler extends KeyAdapter
	{
		static final String action = "close-docking-area";
		Vector<Key> b1, b2;
		String name;
		int match1, match2;

		public KeyHandler(String dockableName)
		{
			String shortcut1=jEdit.getProperty(action + ".shortcut");
			String shortcut2=jEdit.getProperty(action + ".shortcut2");
			if (shortcut1 != null)
				b1 = parseShortcut(shortcut1);
			if (shortcut2 != null)
				b2 = parseShortcut(shortcut2);
			name = dockableName;
			match1 = match2 = 0;
		}

		@Override
		public void keyTyped(KeyEvent e)
		{
			if (b1 != null)
				match1 = match(e, b1, match1);
			if (b2 != null)
				match2 = match(e, b2, match2);
			if ((match1 > 0 && match1 == b1.size()) ||
				(match2 > 0 && match2 == b2.size()))
			{
				hideDockableWindow(name);
				match1 = match2 = 0;
			}
		}

		private int match(KeyEvent e, Vector<Key> shortcut, int index)
		{
			char c = e.getKeyChar();
			if (shortcut != null && c == shortcut.get(index).key)
				return index + 1;
			return 0;
		}

		private Vector<Key> parseShortcut(String shortcut)
		{
			Vector<Key> keys = new Vector<Key>();
			String [] parts = shortcut.split("\\s+");
			for (String part: parts)
			{
				if (part.length() > 0)
					keys.add(KeyEventTranslator.parseKey(part));
			}
			return keys;
		}
	} 

	
	
	public abstract static class DockingLayout
	{
		public static final int NO_VIEW_INDEX = -1;
		public abstract boolean loadLayout(String baseName, int viewIndex);
		public abstract boolean saveLayout(String baseName, int viewIndex);
		public abstract String getName();

		public void setPlainView(boolean plain)
		{
		}

		public String [] getSavedLayouts()
		{
			String layoutDir = getLayoutDirectory();
			if (layoutDir == null)
				return null;
			File dir = new File(layoutDir);
			File[] files = dir.listFiles(new FilenameFilter()
			{
				public boolean accept(File dir, String name)
				{
					return name.endsWith(".xml");
				}
			});
			String[] layouts = new String[files.length];
			for (int i = 0; i < files.length; i++)
				layouts[i] = fileToLayout(files[i].getName());
			return layouts;
		}

		private static String fileToLayout(String filename)
		{
			return filename.replaceFirst(".xml", "");
		}

		private static String layoutToFile(String baseName, int viewIndex)
		{
			StringBuilder name = new StringBuilder(baseName);
			if (viewIndex != NO_VIEW_INDEX)
				name.append("-view").append(viewIndex);
			name.append(".xml");
			return name.toString();
		}

		public String getLayoutFilename(String baseName, int viewIndex)
		{
			String dir = getLayoutDirectory();
			if (dir == null)
				return null;
			return dir + File.separator + layoutToFile(baseName, viewIndex);
		}

		private String getLayoutDirectory()
		{
			String name = getName();
			if (name == null)
				return null;
			String dir = jEdit.getSettingsDirectory();
			if (dir == null)
				return null;
			dir = dir + File.separator + name;
			File d = new File(dir);
			if (!d.exists())
				d.mkdir();
			return dir;
		}
	} 

} 
