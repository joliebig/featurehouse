

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



public class WrapWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view) 
	{
		Widget wrap = new WrapWidget(view);
		return wrap;
	} 
	
	
	private static class WrapWidget implements Widget
	{
		private final JLabel wrap;
		private final View view;
		public WrapWidget(final View view) 
		{
			wrap = new ToolTipLabel();
			wrap.setHorizontalAlignment(SwingConstants.CENTER);
			wrap.setToolTipText(jEdit.getProperty("view.status.wrap-tooltip"));
			this.view = view;
			wrap.addMouseListener(new MouseAdapter() 
					      {
						      @Override
						      public void mouseClicked(MouseEvent evt)
						      {
							      view.getBuffer().toggleWordWrap(view);
						      }
					      });
		}
		
		public JComponent getComponent() 
		{
			return wrap;
		}
		
		public void update() 
		{
			Buffer buffer = view.getBuffer();
			String wrap = buffer.getStringProperty("wrap");
			if(wrap.equals("none"))
				this.wrap.setText("-");
			else if(wrap.equals("hard"))
				this.wrap.setText("H");
			else if(wrap.equals("soft"))
				this.wrap.setText("S");
		}
		
		public void propertiesChanged()
		{
			
			Font font = new JLabel().getFont();
			
			FontMetrics fm = wrap.getFontMetrics(font);
			Dimension dim = new Dimension(Math.max(Math.max(fm.charWidth('-'),
									fm.charWidth('H')),
					fm.charWidth('S')) + 1,
				fm.getHeight());
			wrap.setPreferredSize(dim);
			wrap.setMaximumSize(dim);
		}
	} 

}
