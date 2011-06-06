

package org.gjt.sp.jedit.gui;




import javax.swing.border.EmptyBorder;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import java.util.Arrays;

import org.gjt.sp.jedit.buffer.FoldHandler;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.options.BufferOptionPane;

import org.gjt.sp.jedit.*;





public class BufferOptions extends EnhancedDialog
{
	

	public BufferOptions(View view, Buffer buffer)
	{
		super(view,jEdit.getProperty("buffer-options.title"),true);
		this.view = view;
		this.buffer = buffer;

		JPanel content = new JPanel(new BorderLayout());
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		ActionHandler actionListener = new ActionHandler();
		panel = new BufferOptionPane();

		content.add(BorderLayout.NORTH,panel);

		

		JPanel buttons = new JPanel();
		buttons.setLayout(new BoxLayout(buttons,BoxLayout.X_AXIS));
		buttons.setBorder(new EmptyBorder(12,0,0,0));
		buttons.add(Box.createGlue());

		ok = new JButton(jEdit.getProperty("common.ok"));
		ok.addActionListener(actionListener);
		getRootPane().setDefaultButton(ok);
		buttons.add(ok);

		buttons.add(Box.createHorizontalStrut(6));

		cancel = new JButton(jEdit.getProperty("common.cancel"));
		cancel.addActionListener(actionListener);
		buttons.add(cancel);

		buttons.add(Box.createGlue());
		content.add(BorderLayout.SOUTH,buttons);

		

		pack();
		setLocationRelativeTo(view);
		setVisible(true);
	} 



	

	public void ok()
	{
		panel.save();
		dispose();
	} 

	

	public void cancel()
	{
		dispose();
	} 

        

	private View view;
	private Buffer buffer;
	private BufferOptionPane panel;
	private JButton ok;
	private JButton cancel;

	

	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == ok)
				ok();
			else if(source == cancel)
				cancel();
		} 

	} 

}

