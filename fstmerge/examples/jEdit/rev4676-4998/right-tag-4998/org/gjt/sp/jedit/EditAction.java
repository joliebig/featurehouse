

package org.gjt.sp.jedit;


import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Component;
import org.gjt.sp.jedit.gui.InputHandler;
import org.gjt.sp.util.Log;



public abstract class EditAction
{
	
	
	public EditAction(String name)
	{
		this.name = name;
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public final String getLabel()
	{
		return jEdit.getProperty(name + ".label");
	} 

	
	
	public final String getMouseOverText()
	{
		return jEdit.getProperty(name + ".mouse-over");
	} 

	
	
	public void invoke(View view)
	{
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

	
	
	public abstract String getCode();
	

	
	public String toString()
	{
		return name;
	} 

	
	private String name;
	

	
	
	public static class Wrapper implements ActionListener
	{
		
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

		private ActionContext context;
		private String actionName;
	} 
}
