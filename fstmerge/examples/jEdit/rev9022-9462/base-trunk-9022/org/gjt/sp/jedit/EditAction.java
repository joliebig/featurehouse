

package org.gjt.sp.jedit;


import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Component;

import org.gjt.sp.util.Log;



public abstract class EditAction
{
	
	private String name;
	
	protected Object[] args;

	
	
	
	
	public EditAction(String name)
	{
		this.name = name;
	}
	
	public EditAction(String name, Object[] newArgs) {
		this.name = name;
		this.args = newArgs;
	}
		
	
	
	public String getName()
	{
		return name;
	} 
	
	
	public void setName(String newName) {
		name = newName;
	}

	
	
	public String getLabel()
	{
		if (args != null) {
			return jEdit.getProperty(name + ".label", args);
		}
		return jEdit.getProperty(name + ".label");
	} 

	
	
	public final String getMouseOverText()
	{
		return jEdit.getProperty(name + ".mouse-over");
	} 

	
	
	abstract public void invoke(View view);

	
	final public void invoke(View view, Object[] newArgs) {
		args = newArgs;
		invoke(view);
	}
	
	
	
	public static View getView(Component comp)
	{
		
		return GUIUtilities.getView(comp);
	} 

	
	
	public final boolean isToggle()
	{
		return jEdit.getBooleanProperty(name + ".toggle");
	} 

	
	
	public boolean isSelected(Component comp)
	{
		return false;
	} 

	
	
	public boolean noRepeat()
	{
		return false;
	} 

	
	
	public boolean noRecord()
	{
		return false;
	} 

	
	
	public boolean noRememberLast()
	{
		return false;
	} 

	
	
	public String getCode() 
	{
		return "jEdit.getAction(" + name + ").invoke(view); ";
	}
	

	
	public String toString()
	{
		return name;
	} 

	
	
	public static class Wrapper implements ActionListener
	{

		private ActionContext context;
		private String actionName;
		
		
		public Wrapper(ActionContext context, String actionName)
		{
			this.context = context;
			this.actionName = actionName;
		}

		
		public void actionPerformed(ActionEvent evt)
		{
			EditAction action = context.getAction(actionName);
			if(action == null)
			{
				Log.log(Log.WARNING,this,"Unknown action: "
					+ actionName);
			}
			else
				context.invokeAction(evt,action);
		}


	} 
}
