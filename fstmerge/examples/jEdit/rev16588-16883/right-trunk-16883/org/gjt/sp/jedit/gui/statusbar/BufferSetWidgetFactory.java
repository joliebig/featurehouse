

package org.gjt.sp.jedit.gui.statusbar;


import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.*;
import javax.swing.JComponent;
import javax.swing.JLabel;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.msg.ViewUpdate;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.EditBus.EBHandler;
import org.gjt.sp.jedit.bufferset.BufferSet;



public class BufferSetWidgetFactory implements StatusWidgetFactory
{
	
	public Widget getWidget(View view)
	{
		Widget bufferSetWidget = new BufferSetWidget(view);
		return bufferSetWidget;
	} 

	
	public static class BufferSetWidget implements Widget
	{
		private final JLabel bufferSetLabel;
		private final View view;
		private BufferSet.Scope currentScope;

		BufferSetWidget(final View view)
		{
			bufferSetLabel = new ToolTipLabel()
			{
				@Override
				public void addNotify()
				{
					super.addNotify();
					EditBus.addToBus(BufferSetWidget.this);
				}

				@Override
				public void removeNotify()
				{
					super.removeNotify();
					EditBus.removeFromBus(BufferSetWidget.this);
				}
			};
			this.view = view;
			update();
			bufferSetLabel.addMouseListener(new MouseAdapter()
			{
				@Override
				public void mouseClicked(MouseEvent evt)
				{
					if (evt.getClickCount() == 2)
					{
						EditPane editPane = view.getEditPane();
						BufferSet.Scope scope = editPane.getBufferSetScope();
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
						editPane.setBufferSetScope(scope);
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
			BufferSet.Scope scope = view.getEditPane().getBufferSetScope();
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

		
		@EBHandler
		public void handleViewUpdate(ViewUpdate viewUpdate)
		{
			if (viewUpdate.getWhat() == ViewUpdate.EDIT_PANE_CHANGED)
				update();
		} 

		
		@EBHandler
		public void handleEditPaneUpdate(EditPaneUpdate editPaneUpdate)
		{
			if (editPaneUpdate.getEditPane() == view.getEditPane() &&
				editPaneUpdate.getWhat() == EditPaneUpdate.BUFFERSET_CHANGED)
			{
				update();
			}
		} 

	} 

}