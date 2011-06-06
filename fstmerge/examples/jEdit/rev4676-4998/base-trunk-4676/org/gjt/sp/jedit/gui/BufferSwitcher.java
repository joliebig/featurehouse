

package org.gjt.sp.jedit.gui;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;

public class BufferSwitcher extends JComboBox
{
	public BufferSwitcher(EditPane editPane)
	{
		this.editPane = editPane;

		
		setRenderer(new BufferCellRenderer());
		setMaximumRowCount(jEdit.getIntegerProperty("bufferSwitcher.maxRowCount",10));
		addActionListener(new ActionHandler());
	}

	public void updateBufferList()
	{
		
		
		if(jEdit.getBufferCount() == 0)
			return;

		updating = true;
		setMaximumRowCount(jEdit.getIntegerProperty("bufferSwitcher.maxRowCount",10));
		setModel(new DefaultComboBoxModel(jEdit.getBuffers()));
		setSelectedItem(editPane.getBuffer());
		updating = false;
	}

	
	private EditPane editPane;
	private boolean updating;

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
				setIcon(buffer.getIcon());
			return this;
		}
	}
}
