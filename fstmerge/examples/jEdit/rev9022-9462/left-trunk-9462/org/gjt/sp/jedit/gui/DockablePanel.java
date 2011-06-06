

package org.gjt.sp.jedit.gui;


import org.gjt.sp.jedit.jEdit;

import java.awt.CardLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.JPanel;
import javax.swing.border.Border;



class DockablePanel extends JPanel
{
	private PanelWindowContainer panel;
	private DockableWindowManager wm;

	
	DockablePanel(PanelWindowContainer panel)
	{
		super(new CardLayout());

		this.panel = panel;
		this.wm = panel.getDockableWindowManager();

		ResizeMouseHandler resizeMouseHandler = new ResizeMouseHandler();
		addMouseListener(resizeMouseHandler);
		addMouseMotionListener(resizeMouseHandler);
	} 

	
	PanelWindowContainer getWindowContainer()
	{
		return panel;
	} 

	
	void showDockable(String name)
	{
		((CardLayout)getLayout()).show(this,name);
	} 

	
	public Dimension getMinimumSize()
	{
		return new Dimension(0,0);
	} 

	
	public Dimension getPreferredSize()
	{
		final String position = panel.getPosition();
		final int dimension = panel.getDimension();

		if(panel.getCurrent() == null)
			return new Dimension(0,0);
		else
		{
			if(position.equals(DockableWindowManager.TOP)
				|| position.equals(DockableWindowManager.BOTTOM))
			{
				if(dimension <= 0)
				{
					int height = super.getPreferredSize().height;
					panel.setDimension(height);
				}
				return new Dimension(0,
					dimension + PanelWindowContainer
					.SPLITTER_WIDTH);
			}
			else
			{
				if(dimension <= 0)
				{
					int width = super.getPreferredSize().width;
					panel.setDimension(width);
				}
				return new Dimension(dimension +
					PanelWindowContainer.SPLITTER_WIDTH,
					0);
			}
		}
	} 

	
	public void setBounds(int x, int y, int width, int height)
	{
		final String position = panel.getPosition();
		final int dimension = panel.getDimension();

		if(position.equals(DockableWindowManager.TOP) ||
			position.equals(DockableWindowManager.BOTTOM))
		{
			if(dimension != 0 && height <= PanelWindowContainer.SPLITTER_WIDTH)
				panel.show(null);
			else
				panel.setDimension(height);
		}
		else
		{
			if(dimension != 0 && width <= PanelWindowContainer.SPLITTER_WIDTH)
				panel.show(null);
			else
				panel.setDimension(width);
		}

		super.setBounds(x,y,width,height);
	} 

	
	static Point dragStart;
	
	
	class ResizeMouseHandler extends MouseAdapter implements MouseMotionListener
	{
		
		boolean canDrag;

		
		public void mousePressed(MouseEvent evt)
		{
			if(canDrag)
			{
				continuousLayout = jEdit.getBooleanProperty("appearance.continuousLayout");
				wm.setResizePos(panel.getDimension(),panel);
				dragStart = evt.getPoint();
			}
		} 

		
		public void mouseReleased(MouseEvent evt)
		{
			if(canDrag)
			{
				if (!continuousLayout)
				{
					panel.setDimension(wm.resizePos
							   + PanelWindowContainer
						.SPLITTER_WIDTH);
				}
				wm.finishResizing();
				dragStart = null;
				wm.revalidate();
			}
		} 

		
		public void mouseMoved(MouseEvent evt)
		{
			Border border = getBorder();
			if(border == null)
			{
				
				return;
			}

			String position = panel.getPosition();

			Insets insets = border.getBorderInsets(DockablePanel.this);
			canDrag = false;
			
			if(position.equals(DockableWindowManager.TOP))
			{
				if(evt.getY() >= getHeight() - insets.bottom)
					canDrag = true;
			} 
			
			else if(position.equals(DockableWindowManager.LEFT))
			{
				if(evt.getX() >= getWidth() - insets.right)
					canDrag = true;
			} 
			
			else if(position.equals(DockableWindowManager.BOTTOM))
			{
				if(evt.getY() <= insets.top)
					canDrag = true;
			} 
			
			else if(position.equals(DockableWindowManager.RIGHT))
			{
				if(evt.getX() <= insets.left)
					canDrag = true;
			} 

			if (dragStart == null)
			{
				if(canDrag)
				{
					wm.setCursor(Cursor.getPredefinedCursor(
						getAppropriateCursor()));
				}
				else
				{
					wm.setCursor(Cursor.getPredefinedCursor(
						Cursor.DEFAULT_CURSOR));
				}
			}
		} 

		
		public void mouseDragged(MouseEvent evt)
		{
			if(!canDrag)
				return;

			if(dragStart == null) 
				return;

			int dimension = panel.getDimension();

			String position = panel.getPosition();

			int newSize = 0;
			
			if(position.equals(DockableWindowManager.TOP))
			{
				newSize = evt.getY();
				wm.setResizePos(
					evt.getY() - dragStart.y
					+ dimension,
					panel);
			} 
			
			else if(position.equals(DockableWindowManager.LEFT))
			{
				newSize = evt.getX();
				wm.setResizePos(evt.getX() - dragStart.x
					+ dimension,
					panel);
			} 
			
			else if(position.equals(DockableWindowManager.BOTTOM))
			{
				newSize = dimension - evt.getY();
				wm.setResizePos(dimension - evt.getY()
					+ dragStart.y,
					panel);
			} 
			
			else if(position.equals(DockableWindowManager.RIGHT))
			{
				newSize = dimension - evt.getX();
				wm.setResizePos(dimension - evt.getX()
					+ dragStart.x,
					panel);
			} 

			if (continuousLayout)
			{
				panel.setDimension(newSize
						   + PanelWindowContainer.SPLITTER_WIDTH);
				wm.revalidate();
			}
		} 

		
		public void mouseExited(MouseEvent evt)
		{
			if (dragStart == null)
			{
				wm.setCursor(Cursor.getPredefinedCursor(
					Cursor.DEFAULT_CURSOR));
			}
		} 

		
		private int getAppropriateCursor()
		{
			String position = panel.getPosition();

			if(position.equals(DockableWindowManager.TOP))
				return Cursor.N_RESIZE_CURSOR;
			else if(position.equals(DockableWindowManager.LEFT))
				return Cursor.W_RESIZE_CURSOR;
			else if(position.equals(DockableWindowManager.BOTTOM))
				return Cursor.S_RESIZE_CURSOR;
			else if(position.equals(DockableWindowManager.RIGHT))
				return Cursor.E_RESIZE_CURSOR;
			else
				throw new InternalError();
		} 

		private boolean continuousLayout;
	} 
}
