

package org.gjt.sp.jedit.gui.statusbar;


import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.textarea.JEditTextArea;



public class RectSelectWidgetFactory implements StatusWidgetFactory
{
    
    public Widget getWidget(View view) 
    {
	    Widget rect = new RectSelectWidget(view);
	    return rect;
    } 

    
    private static class RectSelectWidget implements Widget
    {
	    private final JLabel rectSelect;
	    private final View view;
	    RectSelectWidget(final View view)
	    {
		    rectSelect = new ToolTipLabel();
		    rectSelect.setHorizontalAlignment(SwingConstants.CENTER);
		    rectSelect.setToolTipText(jEdit.getProperty("view.status.rect-tooltip"));
		    this.view = view;
		    rectSelect.addMouseListener(new MouseAdapter()
		    {
			    @Override
			    public void mouseClicked(MouseEvent evt)
			    {
				    JEditTextArea textArea = view.getTextArea();
				    if (textArea != null)
				    	textArea.toggleRectangularSelectionEnabled();
			    }
		    });
	    }
	    
	    public JComponent getComponent() 
	    {
		    return rectSelect;
	    }
	    
	    public void update() 
	    {
		    JEditTextArea textArea = view.getTextArea();
		    if (textArea != null)
		    {
			    if (textArea.isRectangularSelectionEnabled())
			    {
				    rectSelect.setText("R");
				    rectSelect.setEnabled(true);
			    }
			    else
			    {
				    rectSelect.setText("r");
				    rectSelect.setEnabled(false);
			    }
		    }
	    }
	    
            public void propertiesChanged()
	    {
		    
		    Font font = new JLabel().getFont();
		    
		    FontMetrics fm = rectSelect.getFontMetrics(font);
		    Dimension dim = new Dimension(
						  Math.max(fm.charWidth('r'),fm.charWidth('R')) + 1,
						  fm.getHeight());
		    rectSelect.setPreferredSize(dim);
		    rectSelect.setMaximumSize(dim);
		    
	    }
    } 

}
