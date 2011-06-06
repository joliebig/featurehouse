

package org.gjt.sp.jedit.gui;


import java.awt.event.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;




public class FontSelector extends JButton
{
	
	
	public FontSelector(Font font)
	{
		this(font,false);
	} 

	
	
	public FontSelector(Font font, boolean antiAlias)
	{
		setFont(font);
		this.antiAlias = antiAlias;

		updateText();

		setRequestFocusEnabled(false);

		addActionListener(new ActionHandler());
	} 

	
	public void paintComponent(Graphics g)
	{
		setAntiAliasEnabled(g);
		super.paintComponent(g);
	} 

	
	public boolean isAntiAliasEnabled()
	{
		return antiAlias;
	} 

	
	public void setAntiAliasEnabled(boolean antiAlias)
	{
		this.antiAlias = antiAlias;
	} 

	
	private void updateText()
	{
		Font font = getFont();
		String styleString;
		switch(font.getStyle())
		{
		case Font.PLAIN:
			styleString = jEdit.getProperty("font-selector.plain");
			break;
		case Font.BOLD:
			styleString = jEdit.getProperty("font-selector.bold");
			break;
		case Font.ITALIC:
			styleString = jEdit.getProperty("font-selector.italic");
			break;
		case Font.BOLD | Font.ITALIC:
			styleString = jEdit.getProperty("font-selector.bolditalic");
			break;
		default:
			styleString = "UNKNOWN!!!???";
			break;
		}

		setText(font.getName() + ' ' + font.getSize() + ' ' + styleString);
	} 

	
	void setAntiAliasEnabled(Graphics g)
	{
		if (antiAlias)
		{
			Graphics2D g2 = (Graphics2D)g;
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
		}
	} 

	private boolean antiAlias;

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Font font;

			JDialog dialog = GUIUtilities.getParentDialog(FontSelector.this);
			if(dialog == null)
			{
				font = new FontSelectorDialog(
					JOptionPane.getFrameForComponent(
					FontSelector.this),getFont(),
					FontSelector.this)
					.getSelectedFont();
			}
			else
			{
				font = new FontSelectorDialog(dialog,getFont(),
					FontSelector.this)
					.getSelectedFont();
			}

			if(font != null)
			{
				setFont(font);
				updateText();
			}
		}
	} 
} 

