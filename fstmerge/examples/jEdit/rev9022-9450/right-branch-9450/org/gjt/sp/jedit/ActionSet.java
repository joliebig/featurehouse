

package org.gjt.sp.jedit;

import java.io.*;
import java.net.URL;
import java.util.*;

import org.gjt.sp.jedit.gui.InputHandler;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;


public class ActionSet
{
	
	
	public ActionSet()
	{
		actions = new Hashtable();
		loaded = true;
		label = "<no label set; plugin bug>";
	} 

	
	
	public ActionSet(PluginJAR plugin, String[] cachedActionNames,
		boolean[] cachedActionToggleFlags, URL uri)
	{
		this();
		this.plugin = plugin;
		this.uri = uri;
		if(cachedActionNames != null)
		{
			for(int i = 0; i < cachedActionNames.length; i++)
			{
				actions.put(cachedActionNames[i],placeholder);
				jEdit.setTemporaryProperty(cachedActionNames[i]
					+ ".toggle",cachedActionToggleFlags[i]
					? "true" : "false");
			}
		}
		loaded = false;
	} 

	
	
	public ActionSet(String label)
	{
		this();
		setLabel(label);
	} 

	
	
	public String getLabel()
	{
		return label;
	} 

	
	
	public void setLabel(String label)
	{
		if(label == null)
			throw new NullPointerException();
		this.label = label;
	} 

	
	
	public PluginJAR getPluginJAR()
	{
		return plugin;
	} 

	
	
	public void addAction(EditAction action)
	{
		actions.put(action.getName(),action);
		if(context != null)
		{
			context.actionNames = null;
			context.actionHash.put(action.getName(),this);
		}
	} 

	
	
	public void removeAction(String name)
	{
		actions.remove(name);
		if(context != null)
		{
			context.actionNames = null;
			context.actionHash.remove(name);
		}
	} 

	
	
	public void removeAllActions()
	{
		if(context != null)
		{
			context.actionNames = null;
			String[] actions = getActionNames();
			for(int i = 0; i < actions.length; i++)
			{
				context.actionHash.remove(actions[i]);
			}
		}
		this.actions.clear();
	} 

	
	
	public EditAction getAction(String name)
	{
		Object obj = actions.get(name);
		if(obj == placeholder)
		{
			load();
			obj = actions.get(name);
			if(obj == placeholder)
			{
				Log.log(Log.WARNING,this,"Outdated cache");
				obj = null;
			}
		}

		return (EditAction)obj;
	} 

	
	
	public int getActionCount()
	{
		return actions.size();
	} 

	
	
	public String[] getActionNames()
	{
		String[] retVal = new String[actions.size()];
		Enumeration e = actions.keys();
		int i = 0;
		while(e.hasMoreElements())
		{
			retVal[i++] = (String)e.nextElement();
		}
		return retVal;
	} 

	
	
	public String[] getCacheableActionNames()
	{
		LinkedList retVal = new LinkedList();
		Enumeration e = actions.elements();
		while(e.hasMoreElements())
		{
			Object obj = e.nextElement();
			if(obj == placeholder)
			{
				
				
				Log.log(Log.WARNING,this,"Action set not up "
					+ "to date");
			}
			else if(obj instanceof BeanShellAction)
				retVal.add(((BeanShellAction)obj).getName());
		}
		return (String[])retVal.toArray(new String[retVal.size()]);
	} 

	
	
	public EditAction[] getActions()
	{
		load();

		EditAction[] retVal = new EditAction[actions.size()];
		Enumeration e = actions.elements();
		int i = 0;
		while(e.hasMoreElements())
		{
			retVal[i++] = (EditAction)e.nextElement();
		}
		return retVal;
	} 

	
	
	public boolean contains(String action)
	{
		boolean retval = actions.containsKey(action);
		return retval;

	} 

	
	
	public int size()
	{
		return actions.size();
	} 

	
	public String toString()
	{
		return label;
	} 

	
	
	public void initKeyBindings()
	{
		InputHandler inputHandler = jEdit.getInputHandler();

		Iterator iter = actions.entrySet().iterator();
		while(iter.hasNext())
		{
			Map.Entry entry = (Map.Entry)iter.next();
			String name = (String)entry.getKey();

			String shortcut1 = jEdit.getProperty(name + ".shortcut");
			if(shortcut1 != null)
				inputHandler.addKeyBinding(shortcut1,name);

			String shortcut2 = jEdit.getProperty(name + ".shortcut2");
			if(shortcut2 != null)
				inputHandler.addKeyBinding(shortcut2,name);
		}
	} 

	
	
	public void load()
	{
		if(loaded)
			return;

		loaded = true;
		

		Reader stream = null;

		try
		{
			Log.log(Log.DEBUG,this,"Loading actions from " + uri);
			ActionListHandler ah = new ActionListHandler(uri.toString(),this);
			XMLUtilities.parseXML(uri.openStream(), ah);
		}
		catch(IOException e)
		{
			Log.log(Log.ERROR,uri,e);
		}
	} 

	
	ActionContext context;

	
	void getActionNames(List vec)
	{
		Enumeration e = actions.keys();
		while(e.hasMoreElements())
			vec.add(e.nextElement());
	} 

	

	
	private String label;
	private Hashtable actions;
	private PluginJAR plugin;
	private URL uri;
	private boolean loaded;

	private static final Object placeholder = new Object();

	
}
