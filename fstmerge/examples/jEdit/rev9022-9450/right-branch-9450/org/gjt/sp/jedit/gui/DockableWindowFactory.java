

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

import bsh.NameSpace;
import bsh.UtilEvalError;



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
		String[] name, boolean[] actions)
	{
		for(int i = 0; i < name.length; i++)
		{
			Window factory = new Window(plugin,
				name[i],null,actions[i]);
			dockableWindowFactories.put(name[i],factory);
		}
	} 

	
	public void registerDockableWindow(PluginJAR plugin,
		String name, String code, boolean actions)
	{
		Window factory = dockableWindowFactories.get(name);
		if(factory != null)
		{
			factory.code = code;
			factory.loaded = true;
		}
		else
		{
			factory = new Window(plugin,name,code,actions);
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
			stateStack = new Stack();
			actions = true;

			code = new StringBuffer();
			cachedDockableNames = new LinkedList<String>();
			cachedDockableActionFlags = new LinkedList();
		} 

		
		public InputSource resolveEntity(String publicId, String systemId)
		{
			return XMLUtilities.findEntity(systemId, "dockables.dtd", MiscUtilities.class);
		} 

		
		public void characters(char[] c, int off, int len)
		{
			String tag = peekElement();
			if (tag.equals("DOCKABLE"))
				code.append(c, off, len);
		} 

		
		public void startElement(String uri, String localName,
					 String qName, Attributes attrs)
		{
			String tag = pushElement(qName);
			if (tag.equals("DOCKABLE"))
			{
				dockableName = attrs.getValue("NAME");
				actions = "FALSE".equals(attrs.getValue("NO_ACTIONS"));
			}
		} 

		
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
						dockableName,code.toString(),actions);
					cachedDockableNames.add(dockableName);
					cachedDockableActionFlags.add(
						new Boolean(actions));
					
					
					actions = true;
					code.setLength(0);
				}

				popElement();
			}
			else
			{
				
				throw new InternalError();
			}
		} 

		
		public void startDocument()
		{
			try
			{
				pushElement(null);
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		} 

		
		public String[] getCachedDockableNames()
		{
			return (String[])cachedDockableNames.toArray(new String[cachedDockableNames.size()]);
		} 

		
		public boolean[] getCachedDockableActionFlags()
		{
			boolean[] returnValue = new boolean[
				cachedDockableActionFlags.size()];
			Iterator iter = cachedDockableActionFlags.iterator();
			int i = 0;
			while(iter.hasNext())
			{
				boolean flag = ((Boolean)iter.next())
					.booleanValue();
				returnValue[i++] = flag;
			}

			return returnValue;
		} 

		

		
		private PluginJAR plugin;
		
		private URL uri;

		private java.util.List<String> cachedDockableNames;
		private java.util.List cachedDockableActionFlags;

		private String dockableName;
		private StringBuffer code;
		private boolean actions;

		private Stack stateStack;
		

		
		private String pushElement(String name)
		{
			name = (name == null) ? null : name.intern();

			stateStack.push(name);

			return name;
		} 

		
		private String peekElement()
		{
			return (String) stateStack.peek();
		} 

		
		private String popElement()
		{
			return (String) stateStack.pop();
		} 

		
	} 

	
	class Window
	{
		PluginJAR plugin;
		String name;
		String code;
		boolean loaded;

		
		Window(PluginJAR plugin, String name, String code,
			boolean actions)
		{
			this.plugin = plugin;
			this.name = name;
			this.code = code;

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

			
			public String getCode()
			{
				return "view.getDockableWindowManager()"
					+ ".floatDockableWindow(\"" + dockable + "\");";
			} 
		} 
	} 

	private static DockableWindowFactory instance;
	private HashMap<String, Window> dockableWindowFactories;
}
