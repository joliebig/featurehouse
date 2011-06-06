

package org.gjt.sp.jedit;

import java.util.*;


public abstract class ActionContext
{
	
	
	public abstract void invokeAction(EventObject evt, EditAction action);
	

	
	
	public void addActionSet(ActionSet actionSet)
	{
		actionNames = null;
		actionSets.addElement(actionSet);
		actionSet.context = this;
		String[] actions = actionSet.getActionNames();
		for(int i = 0; i < actions.length; i++)
		{
			
			if (actionHash.containsKey(actions[i])) 
			{
				
				ActionSet oldAction = actionHash.get(actions[i]);
				overriddenActions.put(actions[i], oldAction);
			}
			actionHash.put(actions[i],actionSet);
		}
	} 

	
	
	public void removeActionSet(ActionSet actionSet)
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
				ActionSet oldAction = overriddenActions.remove(actions[i]);
				actionHash.put(actions[i], oldAction);
			}
		}
	} 

	
	
	public ActionSet[] getActionSets()
	{
		ActionSet[] retVal = new ActionSet[actionSets.size()];
		actionSets.copyInto(retVal);
		return retVal;
	} 

	
	
	public EditAction getAction(String name)
	{
		ActionSet set = (ActionSet)actionHash.get(name);
		if(set == null)
			return null;
		else
			return set.getAction(name);
	} 

	
	
	public ActionSet getActionSetForAction(String action)
	{
		return (ActionSet)actionHash.get(action);
	} 

	
	
	public String[] getActionNames()
	{
		if(actionNames == null)
		{
			List vec = new LinkedList();
			for(int i = 0; i < actionSets.size(); i++)
				(actionSets.elementAt(i)).getActionNames(vec);

			actionNames = (String[])vec.toArray(
				new String[vec.size()]);
			Arrays.sort(actionNames,
				new MiscUtilities.StringICaseCompare());
		}

		return actionNames;
	} 

	
	String[] actionNames;
	Hashtable<String, ActionSet> actionHash = new Hashtable<String, ActionSet>();
	
	
	Hashtable<String, ActionSet> overriddenActions = new Hashtable<String, ActionSet>(); 
	

	
	private Vector<ActionSet> actionSets = new Vector<ActionSet>();
	
}
