

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;


class ErrorListCellRenderer extends JComponent implements ListCellRenderer
{
	
	ErrorListCellRenderer()
	{
		plainFont = UIManager.getFont("Label.font");
		boldFont = new Font(plainFont.getName(),Font.BOLD,plainFont.getSize());
		plainFM = getFontMetrics(plainFont);
		boldFM = getFontMetrics(boldFont);

		setBorder(new EmptyBorder(2,2,2,2));
	} 

	
	public Component getListCellRendererComponent(JList list, Object value,
		int index, boolean isSelected, boolean cellHasFocus)
	{
		ErrorListDialog.ErrorEntry entry = (ErrorListDialog.ErrorEntry)value;
		this.path = entry.path + ":";
		this.messages = entry.messages;
		return this;
	} 

	
	public Dimension getPreferredSize()
	{
		int width = boldFM.stringWidth(path);
		int height = boldFM.getHeight();
		for(int i = 0; i < messages.length; i++)
		{
			width = Math.max(plainFM.stringWidth(messages[i]),width);
			height += plainFM.getHeight();
		}

		Insets insets = getBorder().getBorderInsets(this);
		width += insets.left + insets.right;
		height += insets.top + insets.bottom;

		return new Dimension(width,height);
	} 

	
	public void paintComponent(Graphics g)
	{
		Insets insets = getBorder().getBorderInsets(this);
		g.setFont(boldFont);
		g.drawString(path,insets.left,insets.top + boldFM.getAscent());
		int y = insets.top + boldFM.getHeight() + 2;
		g.setFont(plainFont);
		for(int i = 0; i < messages.length; i++)
		{
			g.drawString(messages[i],insets.left,y + plainFM.getAscent());
			y += plainFM.getHeight();
		}
	} 

	
	private String path;
	private String[] messages;
	private Font plainFont;
	private Font boldFont;
	private FontMetrics plainFM;
	private FontMetrics boldFM;
	
}
