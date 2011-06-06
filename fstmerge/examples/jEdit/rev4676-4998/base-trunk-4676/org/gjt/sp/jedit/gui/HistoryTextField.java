

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
		setModel(name);

		MouseHandler mouseHandler = new MouseHandler();
		addMouseListener(mouseHandler);
		addMouseMotionListener(mouseHandler);

		this.instantPopups = instantPopups;
		this.enterAddsToHistory = enterAddsToHistory;

		index = -1;
	} 

	
	
	public void setInstantPopups(boolean instantPopups)
	{
		this.instantPopups = instantPopups;
	} 

	
	
	public boolean getInstantPopups()
	{
		return instantPopups;
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

	
	
	public void setModel(String name)
	{
		if(name == null)
		{
			historyModel = null;
			setBorder(UIManager.getBorder("TextField.border"));
		}
		else
		{
			historyModel = HistoryModel.getModel(name);
			setBorder(new CompoundBorder(UIManager.getBorder("TextField.border"),
			new HistoryBorder()));
		}
		index = -1;
		repaint();
	} 

	
	
	public void addCurrentToHistory()
	{
		if(historyModel != null)
			historyModel.addItem(getText());
		index = 0;
	} 

	
	
	public void setText(String text)
	{
		super.setText(text);
		index = -1;
	} 

	
	
	public HistoryModel getModel()
	{
		return historyModel;
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
			if(evt.getKeyCode() == KeyEvent.VK_ENTER)
			{
				if(enterAddsToHistory)
					addCurrentToHistory();

				if(evt.getModifiers() == 0)
				{
					fireActionPerformed();
					evt.consume();
				}
			}
			else if(evt.getKeyCode() == KeyEvent.VK_UP)
			{
				if(evt.isShiftDown())
					doBackwardSearch();
				else
					historyPrevious();
				evt.consume();
			}
			else if(evt.getKeyCode() == KeyEvent.VK_DOWN)
			{
				if(evt.isShiftDown())
					doForwardSearch();
				else
					historyNext();
				evt.consume();
			}
			else if(evt.getKeyCode() == KeyEvent.VK_TAB
				&& evt.isControlDown())
			{
				doBackwardSearch();
				evt.consume();
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
				if(evt.isShiftDown())
					showPopupMenu(getText().substring(0,
						getSelectionStart()),0,getHeight());
				else
					showPopupMenu("",0,getHeight());
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

	

	

	
	private HistoryModel historyModel;
	private JPopupMenu popup;
	private boolean instantPopups;
	private boolean enterAddsToHistory;
	private boolean selectAllOnFocus;
	private String current;
	private int index;
	

	
	private void doBackwardSearch()
	{
		if(historyModel == null)
			return;

		if(getSelectionEnd() != getDocument().getLength())
		{
			setCaretPosition(getDocument().getLength());
		}

		String text = getText().substring(0,getSelectionStart());
		if(text == null)
		{
			historyPrevious();
			return;
		}

		for(int i = index + 1; i < historyModel.getSize(); i++)
		{
			String item = historyModel.getItem(i);
			if(item.startsWith(text))
			{
				replaceSelection(item.substring(text.length()));
				select(text.length(),getDocument().getLength());
				index = i;
				return;
			}
		}

		getToolkit().beep();
	} 

	
	private void doForwardSearch()
	{
		if(historyModel == null)
			return;

		if(getSelectionEnd() != getDocument().getLength())
		{
			setCaretPosition(getDocument().getLength());
		}

		String text = getText().substring(0,getSelectionStart());
		if(text == null)
		{
			historyNext();
			return;
		}

		for(int i = index - 1; i >= 0; i--)
		{
			String item = historyModel.getItem(i);
			if(item.startsWith(text))
			{
				replaceSelection(item.substring(text.length()));
				select(text.length(),getDocument().getLength());
				index = i;
				return;
			}
		}

		getToolkit().beep();
	} 

	
	private void historyPrevious()
	{
		if(historyModel == null)
			return;

		if(index == historyModel.getSize() - 1)
			getToolkit().beep();
		else if(index == -1)
		{
			current = getText();
			setText(historyModel.getItem(0));
			index = 0;
		}
		else
		{
			
			int newIndex = index + 1;
			setText(historyModel.getItem(newIndex));
			index = newIndex;
		}
	} 

	
	private void historyNext()
	{
		if(historyModel == null)
			return;

		if(index == -1)
			getToolkit().beep();
		else if(index == 0)
			setText(current);
		else
		{
			
			int newIndex = index - 1;
			setText(historyModel.getItem(newIndex));
			index = newIndex;
		}
	} 

	
	private void showPopupMenu(String text, int x, int y)
	{
		if(historyModel == null)
			return;

		requestFocus();

		if(popup != null && popup.isVisible())
		{
			popup.setVisible(false);
			return;
		}

		ActionHandler actionListener = new ActionHandler();

		popup = new JPopupMenu();
		JMenuItem caption = new JMenuItem(jEdit.getProperty(
			"history.caption"));
		caption.getModel().setEnabled(false);
 		popup.add(caption);
 		popup.addSeparator();

		for(int i = 0; i < historyModel.getSize(); i++)
		{
			String item = historyModel.getItem(i);
			if(item.startsWith(text))
			{
				JMenuItem menuItem = new JMenuItem(item);
				menuItem.setActionCommand(String.valueOf(i));
				menuItem.addActionListener(actionListener);
				popup.add(menuItem);
			}
		}

		GUIUtilities.showPopupMenu(popup,this,x,y,false);
	} 

	

	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			int ind = Integer.parseInt(evt.getActionCommand());
			if(ind == -1)
			{
				if(index != -1)
					setText(current);
			}
			else
			{
				setText(historyModel.getItem(ind));
				index = ind;
			}
			if(instantPopups)
			{
				addCurrentToHistory();
				fireActionPerformed();
			}
		}
	} 

	
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
