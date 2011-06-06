

package org.gjt.sp.jedit.gui;


import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Stack;

import javax.swing.JComponent;

import org.gjt.sp.jedit.ActionSet;
import org.gjt.sp.jedit.BeanShell;
import org.gjt.sp.jedit.EditAction;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.PluginJAR;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.bsh.NameSpace;
import org.gjt.sp.jedit.bsh.UtilEvalError;



public class DockableWindowFactory
{
	
	public static synchronized DockableWindowFactory getInstance()
	{
		if(instance == null)
			instance = new DockableWindowFactory();
		return instance;
	} 

	
	public DockableWindowFactory()
	{
		dockableWindowFactories = new HashMap<String, Window>();
	} 

	
	
	public void loadDockableWindows(PluginJAR plugin, URL uri,
		PluginJAR.PluginCacheEntry cache)
	{
		try
		{
			Log.log(Log.DEBUG,DockableWindowManager.class,
				"Loading dockables from " + uri);
			DockableListHandler dh = new DockableListHandler(plugin,uri);
			boolean failure = XMLUtilities.parseXML(uri.openStream(), dh);

			if (!failure && cache != null)
			{
				cache.cachedDockableNames = dh.getCachedDockableNames();
				cache.cachedDockableActionFlags = dh.getCachedDockableActionFlags();
				cache.cachedDockableMovableFlags = dh.getCachedDockableMovableFlags();
			}
		}
		catch(IOException e)
		{
			Log.log(Log.ERROR,DockableWindowManager.class,e);
		}
	} 

	
	
	public void unloadDockableWindows(PluginJAR plugin)
	{
		Iterator entries = dockableWindowFactories.entrySet().iterator();
		while(entries.hasNext())
		{
			Map.Entry entry = (Map.Entry)entries.next();
			Window factory = (Window)entry.getValue();
			if(factory.plugin == plugin)
				entries.remove();
		}
	} 

	
	
	public void cacheDockableWindows(PluginJAR plugin,
		String[] name, boolean[] actions, boolean[] movable)
	{
		for(int i = 0; i < name.length; i++)
		{
			Window factory = new Window(plugin,
				name[i],null,actions[i],movable[i]);
			dockableWindowFactories.put(name[i],factory);
		}
	} 

	
	public void registerDockableWindow(PluginJAR plugin,
		String name, String code, boolean actions, boolean movable)
	{
		Window factory = dockableWindowFactories.get(name);
		if(factory != null)
		{
			factory.code = code;
			factory.loaded = true;
		}
		else
		{
			factory = new Window(plugin,name,code,actions, movable);
			dockableWindowFactories.put(name,factory);
		}
	} 

	
	public String[] getRegisteredDockableWindows()
	{
		String[] retVal = new String[dockableWindowFactories.size()];
		Iterator<Window> entries = dockableWindowFactories.values().iterator();
		int i = 0;
		while(entries.hasNext())
		{
			Window factory = entries.next();
			retVal[i++] = factory.name;
		}

		return retVal;
	} 

	public Window getDockableWindowFactory(String name)
	{
		return dockableWindowFactories.get(name);
	}
	
	public String getDockableWindowPluginClass(String name)
	{
		Window w = getDockableWindowFactory(name);
		if (w == null || w.plugin == null || w.plugin.getPlugin() == null)
			return null;
		return w.plugin.getPlugin().getClassName();
	}
	
	
	Iterator<Window> getDockableWindowIterator()
	{
		return dockableWindowFactories.values().iterator();
	} 

	
	class DockableListHandler extends DefaultHandler
	{
		
		
		DockableListHandler(PluginJAR plugin, URL uri)
		{
			this.plugin = plugin;
			this.uri = uri;
			stateStack = new Stack<String>();
			actions = true;
			movable = MOVABLE_DEFAULT;

			code = new StringBuilder();
			cachedDockableNames = new LinkedList<String>();
			cachedDockableActionFlags = new LinkedList<Boolean>();
			cachedDockableMovableFlags = new LinkedList<Boolean>();
		} 

		
		@Override
		public InputSource resolveEntity(String publicId, String systemId)
		{
			return XMLUtilities.findEntity(systemId, "dockables.dtd", MiscUtilities.class);
		} 

		
		@Override
		public void characters(char[] c, int off, int len)
		{
			String tag = peekElement();
			if (tag.equals("DOCKABLE"))
				code.append(c, off, len);
		} 

		
		@Override
		public void startElement(String uri, String localName,
					 String qName, Attributes attrs)
		{
			String tag = pushElement(qName);
			if (tag.equals("DOCKABLE"))
			{
				dockableName = attrs.getValue("NAME");
				actions = "FALSE".equals(attrs.getValue("NO_ACTIONS"));
				String movableAttr = attrs.getValue("MOVABLE");
				if (movableAttr != null)
					movable = movableAttr.equalsIgnoreCase("TRUE");
			}
		} 

		
		@Override
		public void endElement(String uri, String localName, String name)
		{
			if(name == null)
				return;

			String tag = peekElement();

			if(name.equals(tag))
			{
				if(tag.equals("DOCKABLE"))
				{
					registerDockableWindow(plugin,
						dockableName,code.toString(),actions, movable);
					cachedDockableNames.add(dockableName);
					cachedDockableActionFlags.add(
						Boolean.valueOf(actions));
					cachedDockableMovableFlags.add(
							Boolean.valueOf(movable));
					
					
					actions = true;
					movable = MOVABLE_DEFAULT;
					code.setLength(0);
				}

				popElement();
			}
			else
			{
				
				throw new InternalError();
			}
		} 

		
		@Override
		public void startDocument()
		{
			try
			{
				pushElement(null);
			}
			catch (Exception e)
			{
				Log.log(Log.ERROR, this, e);
			}
		} 

		
		public String[] getCachedDockableNames()
		{
			return cachedDockableNames.toArray(new String[cachedDockableNames.size()]);
		} 

		
		public boolean[] getCachedDockableActionFlags()
		{
			return booleanListToArray(cachedDockableActionFlags);
		} 

		
		public boolean[] getCachedDockableMovableFlags()
		{
			return booleanListToArray(cachedDockableMovableFlags);
		} 
		
		
		
		private boolean[] booleanListToArray(java.util.List<Boolean> list)
		{
			boolean[] returnValue = new boolean[list.size()];
			int i = 0;
			for (Boolean value : list)
			{
				returnValue[i++] = value.booleanValue();
			}

			return returnValue;
		} 

		

		
		private PluginJAR plugin;
		
		private URL uri;

		private java.util.List<String> cachedDockableNames;
		private java.util.List<Boolean> cachedDockableActionFlags;
		private java.util.List<Boolean> cachedDockableMovableFlags;
		
		private String dockableName;
		private StringBuilder code;
		private boolean actions;
		private boolean movable;
		final boolean MOVABLE_DEFAULT = false;
		
		private Stack<String> stateStack;
		

		
		private String pushElement(String name)
		{
			name = (name == null) ? null : name.intern();

			stateStack.push(name);

			return name;
		} 

		
		private String peekElement()
		{
			return stateStack.peek();
		} 

		
		private String popElement()
		{
			return stateStack.pop();
		} 

		
	} 

	
	class Window
	{
		PluginJAR plugin;
		String name;
		String code;
		boolean loaded;
		boolean movable;
		boolean isBeingCreated = false;

		
		Window(PluginJAR plugin, String name, String code,
			boolean actions, boolean movable)
		{
			this.plugin = plugin;
			this.name = name;
			this.code = code;
			this.movable = movable;

			if(code != null)
				loaded = true;

			if(actions)
			{
				ActionSet actionSet = (plugin == null
					? jEdit.getBuiltInActionSet()
					: plugin.getActionSet());
				actionSet.addAction(new OpenAction(name));
				actionSet.addAction(new ToggleAction(name));
				actionSet.addAction(new FloatAction(name));

				String label = jEdit.getProperty(name
					+ ".label");
				if(label == null)
					label = "NO LABEL PROPERTY: " + name;

				String[] args = { label };
				jEdit.setTemporaryProperty(name + ".label",
					label);
				jEdit.setTemporaryProperty(name
					+ "-toggle.label",
					jEdit.getProperty(
					"view.docking.toggle.label",args));
				jEdit.setTemporaryProperty(name
					+ "-toggle.toggle","true");
				jEdit.setTemporaryProperty(name
					+ "-float.label",
					jEdit.getProperty(
					"view.docking.float.label",args));
			}
		} 

		
		void load()
		{
			if(loaded)
				return;

			loadDockableWindows(plugin,plugin.getDockablesURI(),null);
		} 

		
		JComponent createDockableWindow(View view, String position)
		{
			
			synchronized(this)
			{
				if (isBeingCreated)
					return null;
				isBeingCreated = true;
			}

			load();

			if(!loaded)
			{
				Log.log(Log.WARNING,this,"Outdated cache");
				return null;
			}

			NameSpace nameSpace = new NameSpace(
				BeanShell.getNameSpace(),
				"DockableWindowManager.Factory"
				+ ".createDockableWindow()");
			try
			{
				nameSpace.setVariable(
					"position",position);
			}
			catch(UtilEvalError e)
			{
				Log.log(Log.ERROR,this,e);
			}
			JComponent win = (JComponent)BeanShell.eval(view,
				nameSpace,code);
			synchronized(this)
			{
				isBeingCreated = false;
			}
			return win;
		} 

		
		class OpenAction extends EditAction
		{
			private String dockable;

			
			OpenAction(String name)
			{
				super(name);
				this.dockable = name;
			} 

			
			public void invoke(View view)
			{
				view.getDockableWindowManager()
					.showDockableWindow(dockable);
			} 

			
			@Override
			public String getCode()
			{
				return "view.getDockableWindowManager()"
					+ ".showDockableWindow(\"" + dockable + "\");";
			} 
		} 

		
		class ToggleAction extends EditAction
		{
			private String dockable;

			
			ToggleAction(String name)
			{
				super(name + "-toggle");
				this.dockable = name;
			} 

			
			public void invoke(View view)
			{
				view.getDockableWindowManager()
					.toggleDockableWindow(dockable);
			} 

			
			public boolean isSelected(View view)
			{
				return view.getDockableWindowManager()
					.isDockableWindowVisible(dockable);
			} 

			
			@Override
			public String getCode()
			{
				return "view.getDockableWindowManager()"
					+ ".toggleDockableWindow(\"" + dockable + "\");";
			} 
		} 

		
		class FloatAction extends EditAction
		{
			private String dockable;

			
			FloatAction(String name)
			{
				super(name + "-float");
				this.dockable = name;
			} 

			
			public void invoke(View view)
			{
				view.getDockableWindowManager()
					.floatDockableWindow(dockable);
			} 

			
			@Override
			public String getCode()
			{
				return "view.getDockableWindowManager()"
					+ ".floatDockableWindow(\"" + dockable + "\");";
			} 
		} 
	} 

	private static DockableWindowFactory instance;
	private final Map<String, Window> dockableWindowFactories;
}
