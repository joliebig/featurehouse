

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.OperatingSystem;



public class ColorWellButton extends JButton
{
	
	public ColorWellButton(Color color)
	{
		setIcon(new ColorWell(color));
		setMargin(new Insets(2,2,2,2));
		addActionListener(new ActionHandler());

		
		if(OperatingSystem.isMacOSLF())
			putClientProperty("JButton.buttonType","toolbar");
	} 

	
	public Color getSelectedColor()
	{
		return ((ColorWell)getIcon()).color;
	} 

	
	public void setSelectedColor(Color color)
	{
		((ColorWell)getIcon()).color = color;
		repaint();
	} 

	
	static class ColorWell implements Icon
	{
		Color color;

		ColorWell(Color color)
		{
			this.color = color;
		}

		public int getIconWidth()
		{
			return 35;
		}

		public int getIconHeight()
		{
			return 10;
		}

		public void paintIcon(Component c, Graphics g, int x, int y)
		{
			if(color == null)
				return;

			g.setColor(color);
			g.fillRect(x,y,getIconWidth(),getIconHeight());
			g.setColor(color.darker());
			g.drawRect(x,y,getIconWidth()-1,getIconHeight()-1);
		}
	} 

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			JDialog parent = GUIUtilities.getParentDialog(ColorWellButton.this);
			JDialog dialog;
			if (parent != null)
			{
				dialog = new ColorPickerDialog(parent,
					jEdit.getProperty("colorChooser.title"),
					true);
			}
			else
			{
				dialog = new ColorPickerDialog(
					JOptionPane.getFrameForComponent(
					ColorWellButton.this),
					jEdit.getProperty("colorChooser.title"),
					true);
			}
			dialog.pack();
			dialog.setVisible(true);
		}
	} 

	
	
	private class ColorPickerDialog extends EnhancedDialog implements ActionListener
	{
		public ColorPickerDialog(Frame parent, String title, boolean modal)
		{
			super(parent,title,modal);

			init();
		}

		public ColorPickerDialog(Dialog parent, String title, boolean modal)
		{
			super(parent,title,modal);

			getContentPane().setLayout(new BorderLayout());

			init();
		}

		public void ok()
		{
			Color c = chooser.getColor();
			if (c != null)
				setSelectedColor(c);
			setVisible(false);
		}

		public void cancel()
		{
			setVisible(false);
		}

		public void actionPerformed(ActionEvent evt)
		{
			if (evt.getSource() == ok)
				ok();
			else
				cancel();
		}

		
		private JColorChooser chooser;
		private JButton ok;
		private JButton cancel;

		private void init()
		{
			Color c = getSelectedColor();
			if(c == null)
				chooser = new JColorChooser();
			else
				chooser = new JColorChooser(c);

			getContentPane().add(BorderLayout.CENTER, chooser);

			Box buttons = new Box(BoxLayout.X_AXIS);
			buttons.add(Box.createGlue());

			ok = new JButton(jEdit.getProperty("common.ok"));
			ok.addActionListener(this);
			buttons.add(ok);
			buttons.add(Box.createHorizontalStrut(6));
			getRootPane().setDefaultButton(ok);
			cancel = new JButton(jEdit.getProperty("common.cancel"));
			cancel.addActionListener(this);
			buttons.add(cancel);
			buttons.add(Box.createGlue());

			getContentPane().add(BorderLayout.SOUTH, buttons);
			pack();
			setLocationRelativeTo(getParent());
		} 
	} 
}
