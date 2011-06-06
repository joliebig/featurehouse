

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;


public class EnhancedButton extends RolloverButton
{
	
	public EnhancedButton(Icon icon, String toolTip, String action,
		ActionContext context)
	{
		super(icon);

		this.action = action;

		if(action != null)
		{
			setEnabled(true);
			addActionListener(new EditAction.Wrapper(context,action));
			addMouseListener(new MouseHandler());
		}
		else
			setEnabled(false);

		setToolTipText(toolTip);
	} 

	
	public boolean isFocusTraversable()
	{
		return false;
	} 

	
	private String action;
	

	
	class MouseHandler extends MouseAdapter
	{
		boolean msgSet = false;

		public void mouseReleased(MouseEvent evt)
		{
			if(msgSet)
			{
				GUIUtilities.getView((Component)evt.getSource())
					.getStatus().setMessage(null);
				msgSet = false;
			}
		}

		public void mouseEntered(MouseEvent evt)
		{
			String msg = jEdit.getProperty(action + ".mouse-over");
			if(msg != null)
			{
				GUIUtilities.getView((Component)evt.getSource())
					.getStatus().setMessage(msg);
				msgSet = true;
			}
		}

		public void mouseExited(MouseEvent evt)
		{
			if(msgSet)
			{
				GUIUtilities.getView((Component)evt.getSource())
					.getStatus().setMessage(null);
				msgSet = false;
			}
		}
	} 
}
