

package org.gjt.sp.jedit.gui;

import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.bufferset.BufferSet;



public class BufferSwitcher extends JComboBox
{
    
	private EditPane editPane;
	private boolean updating;

	public BufferSwitcher(final EditPane editPane)
	{
		this.editPane = editPane;

		
		setRenderer(new BufferCellRenderer());
		setMaximumRowCount(jEdit.getIntegerProperty("bufferSwitcher.maxRowCount",10));
		addActionListener(new ActionHandler());
		addPopupMenuListener(new PopupMenuListener()
		{
			public void popupMenuWillBecomeVisible(
				PopupMenuEvent e) {}

			public void popupMenuWillBecomeInvisible(
				PopupMenuEvent e)
			{
				editPane.getTextArea().requestFocus();
			}

			public void popupMenuCanceled(PopupMenuEvent e)
			{
				editPane.getTextArea().requestFocus();
			}
		});
	}

	public void updateBufferList()
	{
		
		
		BufferSet bufferSet = editPane.getBufferSet();
		if(bufferSet.size() == 0)
			return;

		updating = true;
		setMaximumRowCount(jEdit.getIntegerProperty("bufferSwitcher.maxRowCount",10));
		setModel(new DefaultComboBoxModel(bufferSet.getAllBuffers()));
		setSelectedItem(editPane.getBuffer());
		setToolTipText(editPane.getBuffer().getPath(true));
		updating = false;
	}

	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if(!updating)
			{
				Buffer buffer = (Buffer)getSelectedItem();
				if(buffer != null) 
					editPane.setBuffer(buffer);
			}
		}
	}

	class BufferCellRenderer extends DefaultListCellRenderer
	{
		public Component getListCellRendererComponent(
			JList list, Object value, int index,
			boolean isSelected, boolean cellHasFocus)
		{
			super.getListCellRendererComponent(list,value,index,
				isSelected,cellHasFocus);
			Buffer buffer = (Buffer)value;
			
			if(buffer == null)
				setIcon(null);
			else
			{
				setIcon(buffer.getIcon());
				setToolTipText(buffer.getPath());
			}
			return this;
		}
	}
}
