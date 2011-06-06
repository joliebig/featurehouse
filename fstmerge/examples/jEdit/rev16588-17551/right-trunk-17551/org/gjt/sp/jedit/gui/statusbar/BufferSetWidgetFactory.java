

package org.gjt.sp.jedit.gui.statusbar;


import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.*;
import javax.swing.JComponent;
import javax.swing.JLabel;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.bufferset.BufferSetManager;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.bufferset.BufferSet;
import org.gjt.sp.jedit.msg.PropertiesChanged;



public class BufferSetWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view)
	{
		Widget bufferSetWidget = new BufferSetWidget();
		return bufferSetWidget;
	} 

	
	private static class BufferSetWidget implements Widget, EBComponent
	{
		private final JLabel bufferSetLabel;
		private BufferSet.Scope currentScope;

		BufferSetWidget()
		{
			bufferSetLabel = new ToolTipLabel()
			{
				@Override
				public void addNotify()
				{
					super.addNotify();
					BufferSetWidget.this.update();
					EditBus.addToBus(BufferSetWidget.this);
				}

				@Override
				public void removeNotify()
				{
					super.removeNotify();
					EditBus.removeFromBus(BufferSetWidget.this);
				}
			};
			update();
			bufferSetLabel.addMouseListener(new MouseAdapter()
			{
				@Override
				public void mouseClicked(MouseEvent evt)
				{
					if (evt.getClickCount() == 2)
					{
						BufferSetManager bufferSetManager = jEdit.getBufferSetManager();
						BufferSet.Scope scope = bufferSetManager.getScope();
						switch (scope)
						{
							case global:
								scope = BufferSet.Scope.view;
								break;
							case view:
								scope = BufferSet.Scope.editpane;
								break;
							case editpane:
								scope = BufferSet.Scope.global;
								break;
						}
						bufferSetManager.setScope(scope);
					}
				}
			});
		}

		
		public JComponent getComponent()
		{
			return bufferSetLabel;
		} 

		
		public void update()
		{
			BufferSet.Scope scope = jEdit.getBufferSetManager().getScope();
			if (currentScope == null || !currentScope.equals(scope))
			{
				bufferSetLabel.setText(scope.toString().substring(0,1).toUpperCase());
				bufferSetLabel.setToolTipText(jEdit.getProperty("view.status.bufferset-tooltip", new Object[] {scope}));
				currentScope = scope;
			}
		} 

		
		public void propertiesChanged()
		{
			
			Font font = new JLabel().getFont();
			
			FontMetrics fm = bufferSetLabel.getFontMetrics(font);
			Dimension dim = new Dimension(Math.max(fm.charWidth('E'),Math.max(fm.charWidth('V'),
								fm.charWidth('G'))),
								fm.getHeight());
			bufferSetLabel.setPreferredSize(dim);
			bufferSetLabel.setMaximumSize(dim);
		} 

		
		public void handleMessage(EBMessage message)
		{
			if (message instanceof PropertiesChanged)
			{
				PropertiesChanged propertiesChanged = (PropertiesChanged) message;
				if (propertiesChanged.getSource() instanceof BufferSetManager)
				{
					update();
				}
			}
		} 

	} 

}