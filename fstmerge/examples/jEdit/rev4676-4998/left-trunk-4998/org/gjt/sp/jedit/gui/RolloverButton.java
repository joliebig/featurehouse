

package org.gjt.sp.jedit.gui;


import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.Method;
import javax.swing.*;
import javax.swing.border.*;
import org.gjt.sp.jedit.OperatingSystem;
import org.gjt.sp.util.Log;



public class RolloverButton extends JButton
{
	
	
	public RolloverButton()
	{
		if(OperatingSystem.hasJava15())
			setContentAreaFilled(false);

		if(method != null)
		{
			try
			{
				method.invoke(this,new Boolean[] { Boolean.TRUE });
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,this,e);
			}
		}
		else
		{
			setBorder(new EtchedBorder());
			setBorderPainted(false);
			setMargin(new Insets(1,1,1,1));

			setRequestFocusEnabled(false);

			addMouseListener(new MouseOverHandler());
		}
	} 

	
	
	public RolloverButton(Icon icon)
	{
		this();

		setIcon(icon);
	} 

	
	public boolean isOpaque()
	{
		return false;
	} 

	
	public void setEnabled(boolean b)
	{
		super.setEnabled(b);
		if(method == null)
		{
			setBorderPainted(false);
			repaint();
		}
	} 

	
	public void setBorderPainted(boolean b)
	{
		try
		{
			revalidateBlocked = true;
			super.setBorderPainted(b);
		}
		finally
		{
			revalidateBlocked = false;
		}
	} 

	
	
	public void revalidate()
	{
		if(!revalidateBlocked)
			super.revalidate();
	} 

	
	public void paint(Graphics g)
	{
		if(method != null || isEnabled())
			super.paint(g);
		else
		{
			Graphics2D g2 = (Graphics2D)g;
			g2.setComposite(c);
			super.paint(g2);
		}
	} 

	
	private static AlphaComposite c = AlphaComposite.getInstance(
		AlphaComposite.SRC_OVER, 0.5f);

	private static Method method;

	private boolean revalidateBlocked;

	static
	{
		
	} 

	
	
	class MouseOverHandler extends MouseAdapter
	{
		public void mouseEntered(MouseEvent e)
		{
			if (isEnabled())
				setBorderPainted(true);
		}

		public void mouseExited(MouseEvent e)
		{
			setBorderPainted(false);
		}
	} 
}
