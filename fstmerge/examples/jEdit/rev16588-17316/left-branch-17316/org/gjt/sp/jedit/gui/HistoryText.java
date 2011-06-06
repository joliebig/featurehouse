

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.MouseInputAdapter;
import java.awt.*;
import java.awt.event.*;
import org.gjt.sp.jedit.*;



public class HistoryText
{
	
	public HistoryText(JTextComponent text, String name)
	{
		this.text = text;
		setModel(name);
		index = -1;
	} 

	
	public void fireActionPerformed()
	{
	} 

	
	public int getIndex()
	{
		return index;
	} 

	
	public void setIndex(int index)
	{
		this.index = index;
	} 

	
	
	public HistoryModel getModel()
	{
		return historyModel;
	} 

	
	
	public void setModel(String name)
	{
		if(name == null)
			historyModel = null;
		else
			historyModel = HistoryModel.getModel(name);
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

	
	
	public void addCurrentToHistory()
	{
		if(historyModel != null)
			historyModel.addItem(getText());
		index = 0;
	} 

	
	public void doBackwardSearch()
	{
		if(historyModel == null)
			return;

		if(text.getSelectionEnd() != getDocument().getLength())
		{
			text.setCaretPosition(getDocument().getLength());
		}

		int start = getInputStart();
		String t = getText().substring(0,
			text.getSelectionStart() - start);
		if(t == null)
		{
			historyPrevious();
			return;
		}

		for(int i = index + 1; i < historyModel.getSize(); i++)
		{
			String item = historyModel.getItem(i);
			if(item.startsWith(t))
			{
				text.replaceSelection(item.substring(t.length()));
				text.select(getInputStart() + t.length(),
					getDocument().getLength());
				index = i;
				return;
			}
		}

		text.getToolkit().beep();
	} 

	
	public void doForwardSearch()
	{
		if(historyModel == null)
			return;

		if(text.getSelectionEnd() != getDocument().getLength())
		{
			text.setCaretPosition(getDocument().getLength());
		}

		int start = getInputStart();
		String t = getText().substring(0,
			text.getSelectionStart() - start);
		if(t == null)
		{
			historyNext();
			return;
		}

		for(int i = index - 1; i >= 0; i--)
		{
			String item = historyModel.getItem(i);
			if(item.startsWith(t))
			{
				text.replaceSelection(item.substring(t.length()));
				text.select(getInputStart() + t.length(),
					getDocument().getLength());
				index = i;
				return;
			}
		}

		text.getToolkit().beep();
	} 

	
	public void historyPrevious()
	{
		if(historyModel == null)
			return;

		if(index == historyModel.getSize() - 1)
			text.getToolkit().beep();
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

	
	public void historyNext()
	{
		if(historyModel == null)
			return;

		if(index == -1)
			text.getToolkit().beep();
		else if(index == 0)
			setText(current);
		else
		{
			
			int newIndex = index - 1;
			setText(historyModel.getItem(newIndex));
			index = newIndex;
		}
	} 
	
	
	public Document getDocument()
	{
		return text.getDocument();
	} 
	
	
	
	public String getText()
	{
		return text.getText();
	} 
	
	
	
	public void setText(String text)
	{
		this.index = -1;
		this.text.setText(text);
	} 

	
	
	public int getInputStart()
	{
		return 0;
	} 

	
	public void showPopupMenu(String t, int x, int y)
	{
		if(historyModel == null)
			return;

		text.requestFocus();

		if(popup != null && popup.isVisible())
		{
			popup.setVisible(false);
			popup = null;
			return;
		}

		popup = new JPopupMenu()
		{
			@Override
			public void setVisible(boolean b)
			{
				if (!b)
				{
					popup = null;
				}
				super.setVisible(b);
			}
		};
		JMenuItem caption = new JMenuItem(jEdit.getProperty(
			"history.caption"));
		caption.addActionListener(new ActionListener()
		{
		  public void actionPerformed(ActionEvent e) 
		  {
		    new ListModelEditor().open(historyModel);
		  }
		});		
 		popup.add(caption);
 		popup.addSeparator();

		for(int i = 0; i < historyModel.getSize(); i++)
		{
			String item = historyModel.getItem(i);
			if(item.startsWith(t))
			{
				JMenuItem menuItem = new JMenuItem(item);
				menuItem.setActionCommand(String.valueOf(i));
				menuItem.addActionListener(
					new ActionHandler());
				popup.add(menuItem);
			}
		}

		GUIUtilities.showPopupMenu(popup,text,x,y,false);
	} 

	
	public void showPopupMenu(boolean search)
	{
		if(search)
			showPopupMenu(getText().substring(getInputStart(),
				text.getSelectionStart()),0,text.getHeight());
		else
			showPopupMenu("",0,text.getHeight());
	} 

	
	private JTextComponent text;
	private HistoryModel historyModel;
	private int index;
	private String current;
	private JPopupMenu popup;
	private boolean instantPopups;
	

	
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
}
