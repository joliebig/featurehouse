

package org.gjt.sp.jedit.textarea;

import java.awt.event.MouseEvent;
import org.gjt.sp.jedit.gui.KeyEventTranslator;
import org.gjt.sp.jedit.IPropertyManager;

public class MouseActions implements MouseActionsProvider
{
	private IPropertyManager propertyManager;

	
	MouseActions(IPropertyManager propertyManager, String name)
	{
		this.propertyManager = propertyManager;
		this.name = name;
	} 

	
	public String getActionForEvent(MouseEvent evt, String variant)
	{
		String modStr = KeyEventTranslator.getModifierString(evt);
		if(modStr == null)
		{
			return propertyManager.getProperty("view." + name + '.'
				+ variant + "Click");
		}
		else
		{
			return propertyManager.getProperty("view." + name + '.'
				+ KeyEventTranslator.getModifierString(evt)
				+ variant + "Click");
		}
	} 

	private String name;
}
