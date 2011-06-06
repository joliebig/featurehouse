

package org.gjt.sp.jedit.textarea;

import java.awt.event.MouseEvent;
import org.gjt.sp.jedit.gui.DefaultInputHandler;
import org.gjt.sp.jedit.jEdit;

public class MouseActions
{
	
	MouseActions(String name)
	{
		this.name = name;
	} 

	
	String getActionForEvent(MouseEvent evt, String variant)
	{
		String modStr = DefaultInputHandler.getModifierString(evt);
		if(modStr == null)
		{
			return jEdit.getProperty("view." + name + "."
				+ variant + "Click");
		}
		else
		{
			return jEdit.getProperty("view." + name + "."
				+ DefaultInputHandler.getModifierString(evt)
				+ variant + "Click");
		}
	} 

	private String name;
}
