

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.AbstractBorder;
import javax.swing.border.CompoundBorder;
import javax.swing.event.MouseInputAdapter;
import java.awt.*;
import java.awt.event.*;
import java.util.Collections;
import org.gjt.sp.jedit.*;



public class HistoryTextArea extends JTextArea
{
	
	public HistoryTextArea(String name)
	{
		super(3,15);
		controller = new HistoryText(this,name);
		setFocusTraversalKeys(
			KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS,
			Collections.singleton(
				KeyStroke.getKeyStroke(KeyEvent.VK_TAB,0)));
		setFocusTraversalKeys(
			KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS,
			Collections.singleton(
				KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
					InputEvent.SHIFT_MASK)));
	} 

	
	
	public HistoryModel getModel()
	{
		return controller.getModel();
	} 

	
	
	public void setModel(String name)
	{
		controller.setModel(name);
	} 

	
	
	public void setInstantPopups(boolean instantPopups)
	{
		controller.setInstantPopups(instantPopups);
	} 

	
	
	public boolean getInstantPopups()
	{
		return controller.getInstantPopups();
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

	

	
	protected void processKeyEvent(KeyEvent evt)
	{
		if(!isEnabled())
			return;

		if(evt.getID() == KeyEvent.KEY_PRESSED)
		{
			switch(evt.getKeyCode())
			{
			case KeyEvent.VK_ENTER:
				if(evt.isControlDown())
				{
					replaceSelection("\n");
					evt.consume();
				}
				break;
			case KeyEvent.VK_TAB:
				if(evt.isControlDown())
				{
					replaceSelection("\t");
					evt.consume();
				}
				break;
			case KeyEvent.VK_PAGE_UP:
				if(evt.isShiftDown())
					controller.doBackwardSearch();
				else
					controller.historyPrevious();
				evt.consume();
				break;
			case KeyEvent.VK_PAGE_DOWN:
				if(evt.isShiftDown())
					controller.doForwardSearch();
				else
					controller.historyNext();
				evt.consume();
				break;
			case KeyEvent.VK_UP:
				if(evt.isAltDown())
				{
					controller.showPopupMenu(
						evt.isShiftDown());
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
			if(GUIUtilities.isPopupTrigger(evt))
				controller.showPopupMenu(evt.isShiftDown());
			else
				super.processMouseEvent(evt);

			break;
		default:
			super.processMouseEvent(evt);
			break;
		}
	} 
	
	

	
	private HistoryText controller;
	
}
