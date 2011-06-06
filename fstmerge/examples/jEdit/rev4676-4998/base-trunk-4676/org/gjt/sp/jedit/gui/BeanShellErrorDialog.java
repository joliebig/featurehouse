

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.io.PrintWriter;
import java.io.StringWriter;
import org.gjt.sp.jedit.*;


public class BeanShellErrorDialog extends EnhancedDialog
{
	
	public BeanShellErrorDialog(View view, Throwable t)
	{
		super(view,jEdit.getProperty("beanshell-error.title"),true);

		JPanel content = new JPanel(new BorderLayout(12,12));
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		Box iconBox = new Box(BoxLayout.Y_AXIS);
		iconBox.add(new JLabel(UIManager.getIcon("OptionPane.errorIcon")));
		iconBox.add(Box.createGlue());
		content.add(BorderLayout.WEST,iconBox);

		JPanel centerPanel = new JPanel(new BorderLayout(6,6));

		centerPanel.add(BorderLayout.NORTH,new JLabel(
			jEdit.getProperty("beanshell-error.message")));

		JTextArea textArea = new JTextArea(10,80);

		StringWriter s = new StringWriter();
		t.printStackTrace(new PrintWriter(s));

		textArea.setText(s.toString());
		textArea.setLineWrap(true);
		textArea.setCaretPosition(0);
		centerPanel.add(BorderLayout.CENTER,new JScrollPane(textArea));

		content.add(BorderLayout.CENTER,centerPanel);

		Box buttons = new Box(BoxLayout.X_AXIS);
		buttons.add(Box.createGlue());
		JButton ok = new JButton(jEdit.getProperty("common.ok"));
		ok.addActionListener(new ActionHandler());
		buttons.add(ok);
		buttons.add(Box.createGlue());
		content.add(BorderLayout.SOUTH,buttons);

		getRootPane().setDefaultButton(ok);

		pack();
		setLocationRelativeTo(view);
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

	
	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			dispose();
		} 
	} 
}
