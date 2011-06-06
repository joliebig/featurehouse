

package org.gjt.sp.jedit;


import java.net.URL;
import java.util.*;

import org.gjt.sp.jedit.input.AbstractInputHandler;
import org.gjt.sp.util.Log;



public class ActionSet extends JEditActionSet<EditAction> implements Comparable
{
	
	
	public ActionSet()
	{
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
	
	
	
	@Override
	public void addAction(EditAction action)
	{
		super.addAction(action);
	} 
	
	
	protected EditAction[] getArray(int size)
	{
		return new EditAction[size];
	} 
	
	
	
	@Override
	public EditAction[] getActions()
	{
		return super.getActions();
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

	
	
	@Override
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
			else if(obj instanceof BeanShellAction)
				retVal.add(((BeanShellAction)obj).getName());
		}
		return retVal.toArray(new String[retVal.size()]);
	} 
	
	
	protected String getProperty(String name)
	{
		return jEdit.getProperty(name);
	} 
	
	
	public AbstractInputHandler getInputHandler()
	{
		return jEdit.getInputHandler();
	} 

	
	public int compareTo(Object o)
	{
		return label.compareTo(((ActionSet)o).label);
	}

	
	@Override
	public String toString()
	{
		return label;
	} 
	
	
	
	 protected EditAction createBeanShellAction(String actionName,
						    String code,
						    String selected,
						    boolean noRepeat,
						    boolean noRecord,
						    boolean noRememberLast)
	{
		return new BeanShellAction(actionName,code,selected,noRepeat,noRecord,noRememberLast);
	}
	

	
	private String label;
	private PluginJAR plugin;
	

}
