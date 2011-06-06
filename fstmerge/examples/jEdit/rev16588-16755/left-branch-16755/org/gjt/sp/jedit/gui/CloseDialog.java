

package org.gjt.sp.jedit.gui;


import java.util.Collection;
import java.util.Arrays;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.bufferio.BufferIORequest;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class CloseDialog extends EnhancedDialog
{
	
	public CloseDialog(View view)
	{
		this(view, Arrays.asList(jEdit.getBuffers()));
	}

	public CloseDialog(View view, Collection<Buffer> buffers)
	{
		super(view,jEdit.getProperty("close.title"),true);

		this.view = view;

		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		Box iconBox = new Box(BoxLayout.Y_AXIS);
		iconBox.add(new JLabel(UIManager.getIcon("OptionPane.warningIcon")));
		iconBox.add(Box.createGlue());
		content.add(BorderLayout.WEST,iconBox);

		JPanel centerPanel = new JPanel(new BorderLayout());

		JLabel label = new JLabel(jEdit.getProperty("close.caption"));
		label.setBorder(new EmptyBorder(0,0,6,0));
		centerPanel.add(BorderLayout.NORTH,label);

		bufferList = new JList(bufferModel = new DefaultListModel());
		bufferList.setVisibleRowCount(10);
		bufferList.addListSelectionListener(new ListHandler());

		for(Buffer buffer: buffers)
		{
			if(buffer.isDirty()) 
				bufferModel.addElement(buffer.getPath()); 
		}

		centerPanel.add(BorderLayout.CENTER,new JScrollPane(bufferList));

		content.add(BorderLayout.CENTER,centerPanel);

		ActionHandler actionListener = new ActionHandler();

		Box buttons = new Box(BoxLayout.X_AXIS);
		buttons.add(Box.createGlue());
		buttons.add(selectAll = new JButton(jEdit.getProperty("close.selectAll")));
		selectAll.setMnemonic(jEdit.getProperty("close.selectAll.mnemonic").charAt(0));
		selectAll.addActionListener(actionListener);
		buttons.add(Box.createHorizontalStrut(6));
		buttons.add(save = new JButton(jEdit.getProperty("close.save")));
		save.setMnemonic(jEdit.getProperty("close.save.mnemonic").charAt(0));
		save.addActionListener(actionListener);
		buttons.add(Box.createHorizontalStrut(6));
		buttons.add(discard = new JButton(jEdit.getProperty("close.discard")));
		discard.setMnemonic(jEdit.getProperty("close.discard.mnemonic").charAt(0));
		discard.addActionListener(actionListener);
		buttons.add(Box.createHorizontalStrut(6));
		buttons.add(cancel = new JButton(jEdit.getProperty("common.cancel")));
		cancel.addActionListener(actionListener);
		buttons.add(Box.createGlue());
		bufferList.setSelectedIndex(0);
		content.add(BorderLayout.SOUTH,buttons);
		content.getRootPane().setDefaultButton(cancel);
		GUIUtilities.requestFocus(this,bufferList);
		pack();
		setLocationRelativeTo(view);
		setVisible(true);
	} 

	
	public boolean isOK()
	{
		return ok;
	} 

	
	@Override
	public void ok()
	{
		
	} 

	
	@Override
	public void cancel()
	{
		dispose();
	} 

	
	private final View view;
	private final JList bufferList;
	private final DefaultListModel bufferModel;
	private final JButton selectAll;
	private final JButton save;
	private final JButton discard;
	private final JButton cancel;

	private boolean ok; 

	boolean selectAllFlag;

	private void updateButtons()
	{
		int index = bufferList.getSelectedIndex();
		save.getModel().setEnabled(index != -1);
		discard.getModel().setEnabled(index != -1);
	} 

	
	private class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == selectAll)
			{
				
				
				try
				{
					selectAllFlag = true;

					bufferList.setSelectionInterval(0,
						bufferModel.getSize() - 1);
				}
				finally
				{
					selectAllFlag = false;
				}
				bufferList.requestFocus();
			}
			else if(source == save)
			{
				Object[] paths = bufferList.getSelectedValues();

				for(int i = 0; i < paths.length; i++)
				{
					String path = (String)paths[i];
					Buffer buffer = jEdit.getBuffer(path);
					if(!buffer.save(view,null,true,true))
						return;
					VFSManager.waitForRequests();
					if(buffer.getBooleanProperty(BufferIORequest
						.ERROR_OCCURRED))
						return;
					jEdit._closeBuffer(view,buffer);
					bufferModel.removeElement(path);
				}

				if(bufferModel.getSize() == 0)
				{
					ok = true;
					dispose();
				}
				else
				{
					bufferList.setSelectedIndex(0);
					bufferList.requestFocus();
				}
			}
			else if(source == discard)
			{
				Object[] paths = bufferList.getSelectedValues();

				for(int i = 0; i < paths.length; i++)
				{
					String path = (String)paths[i];
					Buffer buffer = jEdit.getBuffer(path);
					jEdit._closeBuffer(view,buffer);
					bufferModel.removeElement(path);
				}

				if(bufferModel.getSize() == 0)
				{
					ok = true;
					dispose();
				}
				else
				{
					bufferList.setSelectedIndex(0);
					bufferList.requestFocus();
				}
			}
			else if(source == cancel)
				cancel();
		}
	} 

	
	private class ListHandler implements ListSelectionListener
	{
		public void valueChanged(ListSelectionEvent evt)
		{
			if(selectAllFlag)
				return;

			int index = bufferList.getSelectedIndex();
			if(index != -1)
			{
				String path = (String) bufferModel.getElementAt(index);
				Buffer buffer = jEdit.getBuffer(path);
				if (buffer == null)
				{
					
					Log.log(Log.DEBUG, this, "Buffer " + path + " is already closed");
					bufferModel.removeElementAt(index);
				}
				else
				{
					view.showBuffer(buffer);
				}
			}

			updateButtons();
		}
	} 
}
