

package org.gjt.sp.jedit.menu;


import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import org.gjt.sp.jedit.*;


public class MarkersProvider implements DynamicMenuProvider
{
	
	public boolean updateEveryTime()
	{
		return true;
	} 

	
	public void update(JMenu menu)
	{
		final View view = GUIUtilities.getView(menu);
		Buffer buffer = view.getBuffer();

		Vector markers = buffer.getMarkers();

		if(markers.size() == 0)
		{
			JMenuItem mi = new JMenuItem(jEdit.getProperty(
				"no-markers.label"));
			mi.setEnabled(false);
			menu.add(mi);
			return;
		}

		int maxItems = jEdit.getIntegerProperty("menu.spillover",20);

		JMenu current = menu;

		for(int i = 0; i < markers.size(); i++)
		{
			final Marker marker = (Marker)markers.elementAt(i);
			int lineNo = buffer.getLineOfOffset(marker.getPosition());

			if(current.getItemCount() >= maxItems && i != markers.size() - 1)
			{
				
				JMenu newCurrent = new JMenu(
					jEdit.getProperty(
					"common.more"));
				current.add(newCurrent);
				current = newCurrent;
			}

			JMenuItem mi = new MarkersMenuItem(buffer,
				lineNo,marker.getShortcut());
			mi.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent evt)
				{
					view.getTextArea().setCaretPosition(
						marker.getPosition());
				}
			});
			current.add(mi);
		}
	} 

	
	static class MarkersMenuItem extends JMenuItem
	{
		
		MarkersMenuItem(Buffer buffer, int lineNo, char shortcut)
		{
			String text = buffer.getLineText(lineNo).trim();
			if(text.length() == 0)
				text = jEdit.getProperty("markers.blank-line");
			setText((lineNo + 1) + ": " + text);

			shortcutProp = "goto-marker.shortcut";
			MarkersMenuItem.this.shortcut = shortcut;
		} 

		
		public Dimension getPreferredSize()
		{
			Dimension d = super.getPreferredSize();

			String shortcut = getShortcut();

			if(shortcut != null)
			{
				d.width += (getFontMetrics(acceleratorFont)
					.stringWidth(shortcut) + 15);
			}
			return d;
		} 

		
		public void paint(Graphics g)
		{
			super.paint(g);

			String shortcut = getShortcut();

			if(shortcut != null)
			{
				g.setFont(acceleratorFont);
				g.setColor(getModel().isArmed() ?
					acceleratorSelectionForeground :
					acceleratorForeground);
				FontMetrics fm = g.getFontMetrics();
				Insets insets = getInsets();
				g.drawString(shortcut,getWidth() - (fm.stringWidth(
					shortcut) + insets.right + insets.left + 5),
					getFont().getSize() + (insets.top - 1)
					);
			}
		} 

		
		private String shortcutProp;
		private char shortcut;
		private static Font acceleratorFont;
		private static Color acceleratorForeground;
		private static Color acceleratorSelectionForeground;

		
		private String getShortcut()
		{
			if(shortcut == '\0')
				return null;
			else
			{
				String shortcutPrefix = jEdit.getProperty(shortcutProp);

				if(shortcutPrefix == null)
					return null;
				else
				{
					return shortcutPrefix + " " + shortcut;
				}
			}
		} 

		
		static
		{
			acceleratorFont = UIManager.getFont("MenuItem.acceleratorFont");
			acceleratorFont = new Font("Monospaced",
				acceleratorFont.getStyle(),
				acceleratorFont.getSize());
			acceleratorForeground = UIManager
				.getColor("MenuItem.acceleratorForeground");
			acceleratorSelectionForeground = UIManager
				.getColor("MenuItem.acceleratorSelectionForeground");
		} 

		
	} 
}
