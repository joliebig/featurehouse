

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



public class OverwriteWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view)
	{
		Widget overwrite = new OverwriteWidget(view);
		return overwrite;
	} 

	
	private static class OverwriteWidget implements Widget
	{
		private final JLabel overwrite;
		private final View view;

		OverwriteWidget(final View view)
		{
			overwrite = new ToolTipLabel();
			overwrite.setHorizontalAlignment(SwingConstants.CENTER);
			overwrite.setToolTipText(jEdit.getProperty("view.status.overwrite-tooltip"));
			this.view = view;
			overwrite.addMouseListener(new MouseAdapter()
			{
				@Override
				public void mouseClicked(MouseEvent evt)
				{
					JEditTextArea textArea = view.getTextArea();
					if (textArea != null)
						textArea.toggleOverwriteEnabled();
				}
			});
		}

		
		public JComponent getComponent()
		{
			return overwrite;
		} 


		
		public void update()
		{
			JEditTextArea textArea = view.getTextArea();
			if (textArea != null)
				overwrite.setText(textArea.isOverwriteEnabled()
						  ? "O" : "-");
		} 


		
		public void propertiesChanged()
		{
			
			Font font = new JLabel().getFont();
			
			FontMetrics fm = overwrite.getFontMetrics(font);
			Dimension dim = new Dimension(
						      Math.max(fm.charWidth('-'),fm.charWidth('O')) + 1,
						      fm.getHeight());
			overwrite.setPreferredSize(dim);
			overwrite.setMaximumSize(dim);
		} 

	} 
}
