

package org.gjt.sp.jedit.gui.statusbar;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.buffer.JEditBuffer;


public class LineSepWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view) 
	{
		Widget lineSep = new LineSepWidget(view);
		return lineSep;
	} 
	
	
	private static class LineSepWidget implements Widget
	{
		private final JLabel lineSep;
		private final View view;
		
		
		LineSepWidget(final View view) 
		{
			lineSep = new ToolTipLabel();
			lineSep.setHorizontalAlignment(SwingConstants.CENTER);
			lineSep.setToolTipText(jEdit.getProperty("view.status.linesep-tooltip"));
			this.view = view;
			lineSep.addMouseListener(new MouseAdapter() 
						 {
							 @Override
							 public void mouseClicked(MouseEvent evt)
							 {
								 view.getBuffer().toggleLineSeparator(view);
							 }
						 });
		} 

		
		
		public JComponent getComponent() 
		{
			return lineSep;
		} 

		
		
		public void update() 
		{
			Buffer buffer = view.getBuffer();
			String lineSep = buffer.getStringProperty(JEditBuffer.LINESEP);
			if("\n".equals(lineSep))
				this.lineSep.setText("U");
			else if("\r\n".equals(lineSep))
				this.lineSep.setText("W");
			else if("\r".equals(lineSep))
				this.lineSep.setText("M");
		} 

		
	        
	        public void propertiesChanged()
		{
			
			Font font = new JLabel().getFont();
			
			FontMetrics fm = lineSep.getFontMetrics(font);
			Dimension dim = new Dimension(Math.max(
							       Math.max(fm.charWidth('U'),
									fm.charWidth('W')),
							       fm.charWidth('M')) + 1,
				fm.getHeight());
			lineSep.setPreferredSize(dim);
			lineSep.setMaximumSize(dim);
		} 
	} 

}
