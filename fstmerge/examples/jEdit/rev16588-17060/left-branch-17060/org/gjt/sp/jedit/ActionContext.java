

package org.gjt.sp.jedit;

import java.util.*;


public abstract class ActionContext extends JEditActionContext<EditAction, ActionSet>
{
	
	
	@Override
	public ActionSet getActionSetForAction(String action)
	{
		return super.getActionSetForAction(action);
	} 

	
	
	@Override
	public EditAction getAction(String name)
	{
		return super.getAction(name);
	} 
	
	
}
