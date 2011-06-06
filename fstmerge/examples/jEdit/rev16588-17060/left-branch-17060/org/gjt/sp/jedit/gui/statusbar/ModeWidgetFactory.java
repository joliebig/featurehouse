

package org.gjt.sp.jedit.gui.statusbar;


import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JComponent;
import javax.swing.JLabel;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.gui.BufferOptions;
import org.gjt.sp.jedit.jEdit;



public class ModeWidgetFactory implements StatusWidgetFactory
{
    
    public Widget getWidget(View view) 
    {
	ModeWidget mode = new ModeWidget(view);
	return mode;
    } 

    
    private static class ModeWidget implements Widget
    {
	    private final JLabel mode;
	    private final View view;
	    public ModeWidget(final View view) 
	    {
		    mode = new ToolTipLabel();
		    this.view = view;
		    mode.setToolTipText(jEdit.getProperty("view.status.mode-tooltip"));
		    mode.addMouseListener(new MouseAdapter() 
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
		    return mode;
	    }
	    
	    public void update() 
	    {
		    Buffer buffer = view.getBuffer();
		    if (buffer.isLoaded())
			    mode.setText(buffer.getMode().toString());
	    }
	    
	    public void propertiesChanged()
	    {
	    }
	    
    } 

}
