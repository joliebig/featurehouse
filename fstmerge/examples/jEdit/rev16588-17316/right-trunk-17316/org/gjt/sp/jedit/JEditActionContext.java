

package org.gjt.sp.jedit;

import org.gjt.sp.util.StandardUtilities;

import java.lang.reflect.Array;
import java.util.*;


public abstract class JEditActionContext<F extends JEditAbstractEditAction, E extends JEditActionSet<F>>
{
	
	
	public abstract void invokeAction(EventObject evt, F action);
	

	
	
	public void addActionSet(E actionSet)
	{
		actionNames = null;
		actionSets.addElement(actionSet);
		actionSet.context = this;
		String[] actions = actionSet.getActionNames();
		for(int i = 0; i < actions.length; i++)
		{
			
			if (actionHash.containsKey(actions[i])) 
			{
				
				E oldAction = actionHash.get(actions[i]);
				overriddenActions.put(actions[i], oldAction);
			}
			actionHash.put(actions[i],actionSet);
		}
	} 

	
	
	public void removeActionSet(E actionSet)
	{
		actionNames = null;
		actionSets.removeElement(actionSet);
		actionSet.context = null;
		String[] actions = actionSet.getActionNames();
		for(int i = 0; i < actions.length; i++)
		{
			actionHash.remove(actions[i]);
			if (overriddenActions.containsKey(actions[i])) 
			{
				E oldAction = overriddenActions.remove(actions[i]);
				actionHash.put(actions[i], oldAction);
			}
		}
	} 

	
	
	public E[] getActionSets()
	{
		if (actionSets.isEmpty())
			return null;
		Class clazz = actionSets.get(0).getClass();
		E[] retVal =(E[]) Array.newInstance(clazz, actionSets.size());
		actionSets.copyInto(retVal);
		return retVal;
	} 

	
	
	public F getAction(String name)
	{
		E set = actionHash.get(name);
		if(set == null)
			return null;
		else
			return set.getAction(name);
	} 

	
	
	public E getActionSetForAction(String action)
	{
		return actionHash.get(action);
	} 

	
	
	public String[] getActionNames()
	{
		if(actionNames == null)
		{
			List<String> vec = new LinkedList<String>();
			for(int i = 0; i < actionSets.size(); i++)
				(actionSets.elementAt(i)).getActionNames(vec);

			actionNames = vec.toArray(new String[vec.size()]);
			Arrays.sort(actionNames,
				new StandardUtilities.StringCompare<String>(true));
		}

		return actionNames;
	} 

	
	String[] actionNames;
	
	Hashtable<String, E> actionHash = new Hashtable<String, E>();
	
	
	Hashtable<String, E> overriddenActions = new Hashtable<String, E>(); 
	

	
	private final Vector<E> actionSets = new Vector<E>();
	
}
