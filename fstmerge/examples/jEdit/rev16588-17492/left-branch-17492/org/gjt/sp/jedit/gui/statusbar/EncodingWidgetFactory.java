

package org.gjt.sp.jedit.gui.statusbar;


import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JComponent;
import javax.swing.JLabel;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.gui.BufferOptions;
import org.gjt.sp.jedit.jEdit;



public class EncodingWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view) 
	{
		EncodingWidget mode = new EncodingWidget(view);
		return mode;
	} 

	
	private static class EncodingWidget implements Widget
	{
		private final JLabel encoding;
		private final View view;
		public EncodingWidget(final View view) 
		{
			encoding = new ToolTipLabel();
			this.view = view;
			encoding.setToolTipText(jEdit.getProperty("view.status.mode-tooltip"));
			encoding.addMouseListener(new MouseAdapter() 
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
			return encoding;
		}
		
		public void update() 
		{
			Buffer buffer = view.getBuffer();
			if (buffer.isLoaded())
				encoding.setText(buffer.getStringProperty("encoding"));
		}
		
		public void propertiesChanged()
		{
		}
		
	} 
}
