

package org.gjt.sp.jedit.menu;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class EnhancedCheckBoxMenuItem extends JCheckBoxMenuItem
{
	
	
	public EnhancedCheckBoxMenuItem(String label, String action,
		ActionContext context)
	{
		this.context = context;
		this.action = action;
		this.shortcut = getShortcut();
		if(OperatingSystem.hasScreenMenuBar() && shortcut != null)
		{
			setText(label + " (" + shortcut + ")");
			shortcut = null;
		}
		else
			setText(label);

		if(action != null)
		{
			setEnabled(true);
			addActionListener(new EditAction.Wrapper(context,action));

			addMouseListener(new MouseHandler());
		}
		else
			setEnabled(false);

		setModel(new Model());
	} 

	
	public Dimension getPreferredSize()
	{
		Dimension d = super.getPreferredSize();

		if(shortcut != null)
		{
			d.width += (getFontMetrics(EnhancedMenuItem.acceleratorFont)
				.stringWidth(shortcut) + 15);
		}
		return d;
	} 

	
	public void paint(Graphics g)
	{
		super.paint(g);

		if(shortcut != null)
		{
			g.setFont(EnhancedMenuItem.acceleratorFont);
			g.setColor(getModel().isArmed() ?
				EnhancedMenuItem.acceleratorSelectionForeground :
				EnhancedMenuItem.acceleratorForeground);
			FontMetrics fm = g.getFontMetrics();
			Insets insets = getInsets();
			g.drawString(shortcut,getWidth() - (fm.stringWidth(
				shortcut) + insets.right + insets.left + 5),
				getFont().getSize() + (insets.top - 
				(OperatingSystem.isMacOSLF() ? 0 : 1))
				);
		}
	} 

	

	
	private ActionContext context;
	private String shortcut;
	private String action;
	

	
	private String getShortcut()
	{
		if(action == null)
			return null;
		else
		{
			String shortcut1 = jEdit.getProperty(action + ".shortcut");
			String shortcut2 = jEdit.getProperty(action + ".shortcut2");

			if(shortcut1 == null || shortcut1.length() == 0)
			{
				if(shortcut2 == null || shortcut2.length() == 0)
					return null;
				else
					return shortcut2;
			}
			else
			{
				if(shortcut2 == null || shortcut2.length() == 0)
					return shortcut1;
				else
					return shortcut1 + " or " + shortcut2;
			}
		}
	} 

	

	
	class Model extends DefaultButtonModel
	{
		public boolean isSelected()
		{
			if(!isShowing())
				return false;

			EditAction a = context.getAction(action);
			if(a == null)
			{
				Log.log(Log.WARNING,this,"Unknown action: "
					+ action);
				return false;
			}

			try
			{
				return a.isSelected(EnhancedCheckBoxMenuItem.this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,t);
				return false;
			}
		}

		public void setSelected(boolean b) {}
	} 

	
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
