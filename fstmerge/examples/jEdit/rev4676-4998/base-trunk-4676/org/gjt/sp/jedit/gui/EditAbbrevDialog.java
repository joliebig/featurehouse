

package org.gjt.sp.jedit.gui;


import javax.swing.border.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.*;


public class EditAbbrevDialog extends JDialog
{
	
	public EditAbbrevDialog(Frame frame, String abbrev, String expansion)
	{
		super(frame,jEdit.getProperty("edit-abbrev.title"),true);
		init(abbrev, expansion);
	} 

	
	public EditAbbrevDialog(Dialog dialog, String abbrev, String expansion)
	{
		super(dialog,jEdit.getProperty("edit-abbrev.title"),true);
		init(abbrev, expansion);
	} 

	
	public String getAbbrev()
	{
		if(!isOK)
			return null;

		return editor.getAbbrev();
	} 

	
	public String getExpansion()
	{
		if(!isOK)
			return null;

		return editor.getExpansion();
	} 

	
	private AbbrevEditor editor;
	private JButton ok;
	private JButton cancel;
	private boolean isOK;

	
	private void init(String abbrev, String expansion)
	{
		JPanel content = new JPanel(new BorderLayout());
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		editor = new AbbrevEditor();
		editor.setAbbrev(abbrev);
		editor.setExpansion(expansion);
		editor.setBorder(new EmptyBorder(0,0,12,0));
		content.add(BorderLayout.CENTER,editor);

		Box box = new Box(BoxLayout.X_AXIS);
		box.add(Box.createGlue());
		ok = new JButton(jEdit.getProperty("common.ok"));
		ok.addActionListener(new ActionHandler());
		getRootPane().setDefaultButton(ok);
		box.add(ok);
		box.add(Box.createHorizontalStrut(6));
		cancel = new JButton(jEdit.getProperty("common.cancel"));
		cancel.addActionListener(new ActionHandler());
		box.add(cancel);
		box.add(Box.createGlue());
		content.add(BorderLayout.SOUTH,box);

		KeyListener listener = new KeyHandler();
		addKeyListener(listener);
		editor.getBeforeCaretTextArea().addKeyListener(listener);
		editor.getAfterCaretTextArea().addKeyListener(listener);

		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		pack();
		setLocationRelativeTo(getParent());
		show();
	} 

	

	
	class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if(evt.getSource() == ok)
			{
				if(editor.getAbbrev() == null
					|| editor.getAbbrev().length() == 0)
				{
					getToolkit().beep();
					return;
				}

				isOK = true;
			}

			dispose();
		}
	} 

	
	class KeyHandler extends KeyAdapter
	{
		public void keyPressed(KeyEvent evt)
		{
			if(evt.getKeyCode() == KeyEvent.VK_ESCAPE)
				dispose();
		}
	} 
}
