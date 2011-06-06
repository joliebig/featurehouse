

package org.gjt.sp.jedit;

import java.io.*;
import java.net.URL;
import java.util.*;

import org.gjt.sp.jedit.input.AbstractInputHandler;
import org.gjt.sp.jedit.input.InputHandlerProvider;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;


public abstract class JEditActionSet<E extends JEditAbstractEditAction> implements InputHandlerProvider
{
	
	
	public JEditActionSet()
	{
		actions = new Hashtable<String, Object>();
		loaded = true;
	} 
	
	
	
	public JEditActionSet(String[] cachedActionNames, URL uri)
	{
		this();
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

	
	
	public void addAction(E action)
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

	
	
	public E getAction(String name)
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

		return (E) obj;
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
		LinkedList<String> retVal = new LinkedList<String>();
		Enumeration e = actions.elements();
		while(e.hasMoreElements())
		{
			Object obj = e.nextElement();
			if(obj == placeholder)
			{
				
				
				Log.log(Log.WARNING,this,"Action set not up "
					+ "to date");
			}
			else if(obj instanceof JEditBeanShellAction)
				retVal.add(((JEditBeanShellAction)obj).getName());
		}
		return retVal.toArray(new String[retVal.size()]);
	} 
	
	
	
	protected abstract E[] getArray(int size);		
	

	
	
	public E[] getActions()
	{
		load();
		E[] retVal = getArray(actions.size());
		Enumeration e = actions.elements();
		int i = 0;
		while(e.hasMoreElements())
		{
			retVal[i++] = (E) e.nextElement();
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

	
	
	public void load()
	{
		if(loaded)
			return;

		loaded = true;
		

		if (uri == null)
			return;
		try
		{
			Log.log(Log.DEBUG,this,"Loading actions from " + uri);
			ActionListHandler ah = new ActionListHandler(uri.toString(),this);
			if ( XMLUtilities.parseXML(uri.openStream(), ah))
			{
				Log.log(Log.ERROR, this, "Unable to parse: " + uri);
			}
		}
		catch(IOException e)
		{
			Log.log(Log.ERROR,this,uri,e);
		}
	} 
	
	
	
	protected abstract JEditAbstractEditAction createBeanShellAction(String actionName,
									   String code,
									   String selected,
									   boolean noRepeat,
									   boolean noRecord,
									   boolean noRememberLast);
	
	
	
	
	public void initKeyBindings()
	{
		AbstractInputHandler inputHandler = getInputHandler();

		Iterator<Map.Entry<String,Object>> iter = actions.entrySet().iterator();
		while(iter.hasNext())
		{
			Map.Entry<String,Object> entry = iter.next();
			String name = entry.getKey();

			String shortcut1 = getProperty(name + ".shortcut");
			if(shortcut1 != null)
				inputHandler.addKeyBinding(shortcut1,name);

			String shortcut2 = getProperty(name + ".shortcut2");
			if(shortcut2 != null)
				inputHandler.addKeyBinding(shortcut2,name);
		}
	} 
	
	
	
	protected abstract String getProperty(String name);
	

	
	JEditActionContext context;

	
	void getActionNames(List<String> vec)
	{
		Enumeration<String> e = actions.keys();
		while(e.hasMoreElements())
			vec.add(e.nextElement());
	} 

	

	
	protected Hashtable<String,Object> actions;
	protected URL uri;
	protected boolean loaded;

	protected static final Object placeholder = new Object();

	
}
