

package org.gjt.sp.jedit;

import com.microstar.xml.*;
import java.io.*;
import java.net.URL;
import java.util.*;
import org.gjt.sp.jedit.gui.InputHandler;
import org.gjt.sp.util.Log;


public class ActionSet
{
	
	
	public ActionSet(PluginJAR plugin, String[] cachedActionNames,
		URL uri)
	{
		this();
		this.plugin = plugin;
		this.uri = uri;
		if(cachedActionNames != null)
		{
			for(int i = 0; i < cachedActionNames.length; i++)
			{
				actions.put(cachedActionNames[i],placeholder);
			}
		}
		loaded = false;
	} 

	
	
	public ActionSet()
	{
		actions = new Hashtable();
		loaded = true;
	} 

	
	
	public ActionSet(String label)
	{
		this();
		this.label = label;
	} 

	
	
	public String getLabel()
	{
		return label;
	} 

	
	
	public void setLabel(String label)
	{
		this.label = label;
	} 

	
	
	public void addAction(EditAction action)
	{
		actions.put(action.getName(),action);
		if(context != null)
			context.actionHash.put(action.getName(),this);
	} 

	
	
	public void removeAction(String name)
	{
		actions.remove(name);
		if(context != null)
			context.actionHash.remove(name);
	} 

	
	
	public void removeAllActions()
	{
		if(context != null)
		{
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
		int i = 0;
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
		return actions.containsKey(action);
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
		

		try
		{
			Log.log(Log.DEBUG,this,"Loading actions from " + uri);

			ActionListHandler ah = new ActionListHandler(uri.toString(),this);
			XmlParser parser = new XmlParser();
			parser.setHandler(ah);
			parser.parse(null, null, new BufferedReader(
				new InputStreamReader(
				uri.openStream())));
		}
		catch(XmlException xe)
		{
			int line = xe.getLine();
			String message = xe.getMessage();
			Log.log(Log.ERROR,this,uri + ":" + line
				+ ": " + message);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,uri,e);
		}
	} 

	
	ActionContext context;

	
	void getActionNames(ArrayList vec)
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
