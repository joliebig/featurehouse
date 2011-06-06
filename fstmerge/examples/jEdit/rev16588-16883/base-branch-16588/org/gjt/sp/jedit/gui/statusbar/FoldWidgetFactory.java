

package org.gjt.sp.jedit.gui.statusbar;


import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JComponent;
import javax.swing.JLabel;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.gui.BufferOptions;
import org.gjt.sp.jedit.jEdit;



public class FoldWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view) 
	{
		Widget fold = new FoldWidget(view);
		return fold;
	} 

	
	private static class FoldWidget implements Widget
	{
		private final JLabel fold;
		private final View view;
		public FoldWidget(final View view) 
		{
			fold = new ToolTipLabel();
			this.view = view;
			fold.setToolTipText(jEdit.getProperty("view.status.mode-tooltip"));
			fold.addMouseListener(new MouseAdapter() 
					      {
						      @Override
						      public void mouseClicked(MouseEvent evt)
						      {
							      if(evt.getClickCount() == 2)
								      new BufferOptions(view,view.getBuffer());
						      }
					      });
		}
		
		public JComponent getComponent() 
		{
			return fold;
		}
		
		public void update() 
		{
			Buffer buffer = view.getBuffer();
			if (buffer.isLoaded())
				fold.setText((String)view.getBuffer().getProperty("folding"));
		}
		
		public void propertiesChanged()
		{
		}
	} 
}
