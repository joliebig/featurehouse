

package org.gjt.sp.jedit;


import org.gjt.sp.util.Log;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;



public abstract class EditAction extends JEditAbstractEditAction<View>
{
	
	
	public EditAction(String name)
	{
		super(name);
	}
	
	public EditAction(String name, Object[] newArgs) 
	{
		super(name, newArgs);
	} 
			
	
	
	public String getLabel()
	{
		if (args != null)
		{
			return jEdit.getProperty(name + ".label", args);
		}
		return jEdit.getProperty(name + ".label");
	} 

	
	
	public final String getMouseOverText()
	{
		return jEdit.getProperty(name + ".mouse-over");
	} 

	
	
	abstract public void invoke(View view);
	
	
	
	 @Deprecated
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
	
	
	
	
	public static class Wrapper implements ActionListener
	{

		private final ActionContext context;
		private final String actionName;
		
		
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
