

package org.gjt.sp.jedit;

import java.util.*;


public abstract class ActionContext
{
	
	
	public abstract void invokeAction(EventObject evt, EditAction action);
	

	
	
	public void addActionSet(ActionSet actionSet)
	{
		actionSets.addElement(actionSet);
		actionSet.context = this;
		String[] actions = actionSet.getActionNames();
		for(int i = 0; i < actions.length; i++)
		{
			actionHash.put(actions[i],actionSet);
		}
	} 

	
	
	public void removeActionSet(ActionSet actionSet)
	{
		actionSets.removeElement(actionSet);
		actionSet.context = null;
		String[] actions = actionSet.getActionNames();
		for(int i = 0; i < actions.length; i++)
		{
			actionHash.remove(actions[i]);
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
		ArrayList vec = new ArrayList();
		for(int i = 0; i < actionSets.size(); i++)
			((ActionSet)actionSets.elementAt(i)).getActionNames(vec);

		return (String[])vec.toArray(new String[vec.size()]);
	} 

	
	Hashtable actionHash = new Hashtable();
	

	
	private Vector actionSets = new Vector();
	
}
