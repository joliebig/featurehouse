

package org.gjt.sp.jedit.gui;


import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.border.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


public class ErrorListDialog extends EnhancedDialog
{
	
	public static class ErrorEntry
	{
		String path;
		String[] messages;

		public ErrorEntry(String path, String messageProp, Object[] args)
		{
			this.path = path;

			String message = jEdit.getProperty(messageProp,args);
			if(message == null)
				message = "Undefined property: " + messageProp;

			Log.log(Log.ERROR,this,path + ":");
			Log.log(Log.ERROR,this,message);

			Vector tokenizedMessage = new Vector();
			int lastIndex = -1;
			for(int i = 0; i < message.length(); i++)
			{
				if(message.charAt(i) == '\n')
				{
					tokenizedMessage.addElement(message.substring(
						lastIndex + 1,i));
					lastIndex = i;
				}
			}

			if(lastIndex != message.length())
			{
				tokenizedMessage.addElement(message.substring(
					lastIndex + 1));
			}

			messages = new String[tokenizedMessage.size()];
			tokenizedMessage.copyInto(messages);
		}
	} 

	
	public ErrorListDialog(Frame frame, String title, String caption,
		Vector messages, boolean showPluginMgrButton)
	{
		super(frame,title,true);

		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		Box iconBox = new Box(BoxLayout.Y_AXIS);
		iconBox.add(new JLabel(UIManager.getIcon("OptionPane.errorIcon")));
		iconBox.add(Box.createGlue());
		content.add(BorderLayout.WEST,iconBox);

		JPanel centerPanel = new JPanel(new BorderLayout());

		JLabel label = new JLabel(caption);
		label.setBorder(new EmptyBorder(0,0,6,0));
		centerPanel.add(BorderLayout.NORTH,label);

		JList errors = new JList(messages);
		errors.setCellRenderer(new ErrorListCellRenderer());
		errors.setVisibleRowCount(Math.min(messages.size(),4));

		
		
		JScrollPane scrollPane = new JScrollPane(errors,
			JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		Dimension size = scrollPane.getPreferredSize();
		size.width = Math.min(size.width,400);
		scrollPane.setPreferredSize(size);

		centerPanel.add(BorderLayout.CENTER,scrollPane);

		content.add(BorderLayout.CENTER,centerPanel);

		Box buttons = new Box(BoxLayout.X_AXIS);
		buttons.add(Box.createGlue());

		ok = new JButton(jEdit.getProperty("common.ok"));
		ok.addActionListener(new ActionHandler());

		if(showPluginMgrButton)
		{
			pluginMgr = new JButton(jEdit.getProperty("error-list.plugin-manager"));
			pluginMgr.addActionListener(new ActionHandler());
			buttons.add(pluginMgr);
			buttons.add(Box.createHorizontalStrut(6));
		}

		buttons.add(ok);

		buttons.add(Box.createGlue());
		content.add(BorderLayout.SOUTH,buttons);

		getRootPane().setDefaultButton(ok);

		pack();
		setLocationRelativeTo(frame);
		show();
	} 

	
	public void ok()
	{
		dispose();
	} 

	
	public void cancel()
	{
		dispose();
	} 

	
	private JButton ok, pluginMgr;
	

	
	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			if(evt.getSource() == ok)
				dispose();
			else if(evt.getSource() == pluginMgr)
			{
				new org.gjt.sp.jedit.pluginmgr.PluginManager(
					JOptionPane.getFrameForComponent(
					ErrorListDialog.this));
			}
		} 
	} 
}
