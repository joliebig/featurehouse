

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.AbstractBorder;
import javax.swing.border.CompoundBorder;
import javax.swing.event.MouseInputAdapter;
import java.awt.*;
import java.awt.event.*;
import org.gjt.sp.jedit.*;



public class HistoryTextField extends JTextField
{
	
	
	public HistoryTextField()
	{
		this(null);
	} 

	
	
	public HistoryTextField(String name)
	{
		this(name,false,true);
	} 

	
	
	public HistoryTextField(String name, boolean instantPopups)
	{
		this(name,instantPopups,true);
	} 

	
	
	public HistoryTextField(String name, boolean instantPopups,
		boolean enterAddsToHistory)
	{
		controller = new HistoryText(this,null)
		{
			public void fireActionPerformed()
			{
				HistoryTextField.this.fireActionPerformed();
			}
		};

		setModel(name);
		MouseHandler mouseHandler = new MouseHandler();
		addMouseListener(mouseHandler);
		addMouseMotionListener(mouseHandler);

		setInstantPopups(instantPopups);
		setEnterAddsToHistory(enterAddsToHistory);
	} 

	
	
	public void setInstantPopups(boolean instantPopups)
	{
		controller.setInstantPopups(instantPopups);
	} 

	
	
	public boolean getInstantPopups()
	{
		return controller.getInstantPopups();
	} 

	
	
	public void setEnterAddsToHistory(boolean enterAddsToHistory)
	{
		this.enterAddsToHistory = enterAddsToHistory;
	} 

	
	
	public boolean setEnterAddsToHistory()
	{
		return enterAddsToHistory;
	} 

	
	
	public void setSelectAllOnFocus(boolean selectAllOnFocus)
	{
		this.selectAllOnFocus = selectAllOnFocus;
	} 

	
	
	public boolean setSelectAllOnFocus()
	{
		return selectAllOnFocus;
	} 

	
	
	public HistoryModel getModel()
	{
		return controller.getModel();
	} 

	
	
	public void setModel(String name)
	{
		controller.setModel(name);

		Border textFieldBorder = UIManager.getBorder("TextField.border");

		if(name == null)
		{
			if(textFieldBorder != null)
				setBorder(textFieldBorder);
		}
		else
		{
			if(textFieldBorder != null)
			{
				setBorder(new CompoundBorder(textFieldBorder,
					new HistoryBorder()));
			}
		}
		repaint();
	} 

	
	
	public void addCurrentToHistory()
	{
		controller.addCurrentToHistory();
	} 

	
	
	public void setText(String text)
	{
		super.setText(text);
		controller.setIndex(-1);
	} 

	
	
	public void fireActionPerformed()
	{
		super.fireActionPerformed();
	} 

	

	
	protected void processKeyEvent(KeyEvent evt)
	{
		if(!isEnabled())
			return;

		if(evt.getID() == KeyEvent.KEY_PRESSED)
		{
			switch(evt.getKeyCode())
			{
			case KeyEvent.VK_ENTER:
				if(enterAddsToHistory)
					addCurrentToHistory();

				if(evt.getModifiers() == 0)
				{
					fireActionPerformed();
					evt.consume();
				}
				break;
			case KeyEvent.VK_UP:
				if(evt.isShiftDown())
					controller.doBackwardSearch();
				else
					controller.historyPrevious();
				evt.consume();
				break;
			case KeyEvent.VK_DOWN:
				if(evt.isShiftDown())
					controller.doForwardSearch();
				else if(evt.isAltDown())
				{
					controller.showPopupMenu(
						evt.isShiftDown());
				}
				else
					controller.historyNext();
				evt.consume();
				break;
			case KeyEvent.VK_TAB:
				if(evt.isControlDown())
				{
					controller.doBackwardSearch();
					evt.consume();
				}
				break;
			}
		}

		if(!evt.isConsumed())
			super.processKeyEvent(evt);
	} 

	
	protected void processMouseEvent(MouseEvent evt)
	{
		if(!isEnabled())
			return;

		switch(evt.getID())
		{
		case MouseEvent.MOUSE_PRESSED:
			Border border = getBorder();
			Insets insets = border.getBorderInsets(HistoryTextField.this);

			if(evt.getX() >= getWidth() - insets.right
				|| GUIUtilities.isPopupTrigger(evt))
			{
				controller.showPopupMenu(evt.isShiftDown());
			}
			else
				super.processMouseEvent(evt);

			break;
		case MouseEvent.MOUSE_EXITED:
			setCursor(Cursor.getDefaultCursor());
			super.processMouseEvent(evt);
			break;
		default:
			super.processMouseEvent(evt);
			break;
		}
	} 

	

	

	
	private HistoryText controller;
	private boolean enterAddsToHistory;
	private boolean selectAllOnFocus;
	

	

	

	
	class MouseHandler extends MouseInputAdapter
	{
		boolean selectAll;

		
		public void mousePressed(MouseEvent evt)
		{
			selectAll = (!hasFocus() && selectAllOnFocus);
		} 

		
		public void mouseReleased(MouseEvent evt)
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					if(selectAll)
						selectAll();
				}
			});
		} 

		
		public void mouseMoved(MouseEvent evt)
		{
			Border border = getBorder();
			Insets insets = border.getBorderInsets(HistoryTextField.this);

			if(evt.getX() >= getWidth() - insets.right)
				setCursor(Cursor.getDefaultCursor());
			else
				setCursor(Cursor.getPredefinedCursor(
					Cursor.TEXT_CURSOR));
		} 

		
		public void mouseDragged(MouseEvent evt)
		{
			selectAll = false;
		} 
	} 

	
	static class HistoryBorder extends AbstractBorder
	{
		static final int WIDTH = 16;

		public void paintBorder(Component c, Graphics g,
			int x, int y, int w, int h)
		{
			g.translate(x+w-WIDTH,y-1);

			
			
			
			
			
			

			
			int w2 = WIDTH/2;
			int h2 = h/2;
			g.setColor(UIManager.getColor(c.isEnabled()
				&& ((HistoryTextField)c).getModel() != null
				? "TextField.foreground" : "TextField.disabledForeground"));
			g.drawLine(w2-5,h2-2,w2+4,h2-2);
			g.drawLine(w2-4,h2-1,w2+3,h2-1);
			g.drawLine(w2-3,h2  ,w2+2,h2  );
			g.drawLine(w2-2,h2+1,w2+1,h2+1);
			g.drawLine(w2-1,h2+2,w2  ,h2+2);

			g.translate(-(x+w-WIDTH),-(y-1));
		}

		public Insets getBorderInsets(Component c)
		{
			return new Insets(0,0,0,WIDTH);
		}
	} 

	
}
