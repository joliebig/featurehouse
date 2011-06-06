

package org.gjt.sp.jedit.gui;


import javax.swing.border.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.lang.reflect.Field;
import java.util.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class GrabKeyDialog extends JDialog
{
	
	
	public GrabKeyDialog(Dialog parent, KeyBinding binding,
		Vector allBindings, Buffer debugBuffer)
	{
		super(parent,jEdit.getProperty("grab-key.title"),true);

		init(binding,allBindings,debugBuffer);
	} 

	
	
	public GrabKeyDialog(Frame parent, KeyBinding binding,
		Vector allBindings, Buffer debugBuffer)
	{
		super(parent,jEdit.getProperty("grab-key.title"),true);

		init(binding,allBindings,debugBuffer);
	} 

	
	
	public String getShortcut()
	{
		if(isOK)
			return shortcut.getText();
		else
			return null;
	} 

	
	
	public boolean isOK()
	{
		return isOK;
	} 

	
	
	public boolean isManagingFocus()
	{
		return false;
	} 

	
	
	public boolean getFocusTraversalKeysEnabled()
	{
		return false;
	} 

	
	protected void processKeyEvent(KeyEvent evt)
	{
		shortcut.processKeyEvent(evt);
	} 

	

	
	private InputPane shortcut; 
	private JLabel assignedTo;
	private JButton ok;
	private JButton remove;
	private JButton cancel;
	private JButton clear;
	private boolean isOK;
	private KeyBinding binding;
	private Vector allBindings;
	private Buffer debugBuffer;
	

	
	private void init(KeyBinding binding, Vector allBindings, Buffer debugBuffer)
	{
		this.binding = binding;
		this.allBindings = allBindings;
		this.debugBuffer = debugBuffer;

		enableEvents(AWTEvent.KEY_EVENT_MASK);

		
		
		JPanel content = new JPanel(new GridLayout(0,1,0,6))
		{
			
			public boolean isManagingFocus()
			{
				return false;
			}

			
			public boolean getFocusTraversalKeysEnabled()
			{
				return false;
			}
		};
		content.setBorder(new EmptyBorder(12,12,12,12));
		setContentPane(content);

		JLabel label = new JLabel(
			debugBuffer == null ? jEdit.getProperty(
			"grab-key.caption",new String[] { binding.label })
			: jEdit.getProperty("grab-key.keyboard-test"));

		Box input = Box.createHorizontalBox();

		shortcut = new InputPane();
		input.add(shortcut);
		input.add(Box.createHorizontalStrut(12));

		clear = new JButton(jEdit.getProperty("grab-key.clear"));
		clear.addActionListener(new ActionHandler());
		input.add(clear);

		assignedTo = new JLabel();
		if(debugBuffer == null)
			updateAssignedTo(null);

		Box buttons = Box.createHorizontalBox();
		buttons.add(Box.createGlue());

		if(debugBuffer == null)
		{
			ok = new JButton(jEdit.getProperty("common.ok"));
			ok.addActionListener(new ActionHandler());
			buttons.add(ok);
			buttons.add(Box.createHorizontalStrut(12));

			if(binding.isAssigned()) {
				
				remove = new JButton(jEdit.getProperty("grab-key.remove"));
				remove.addActionListener(new ActionHandler());
				buttons.add(remove);
				buttons.add(Box.createHorizontalStrut(12));
			}
		}

		cancel = new JButton(jEdit.getProperty("common.cancel"));
		cancel.addActionListener(new ActionHandler());
		buttons.add(cancel);
		buttons.add(Box.createGlue());

		content.add(label);
		content.add(input);
		if(debugBuffer == null)
			content.add(assignedTo);
		content.add(buttons);

		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		pack();
		setLocationRelativeTo(getParent());
		setResizable(false);
		show();
	} 

	
	private String getSymbolicName(int keyCode)
	{
		if(keyCode == KeyEvent.VK_UNDEFINED)
			return null;
		

		if(keyCode >= KeyEvent.VK_A && keyCode <= KeyEvent.VK_Z)
			return String.valueOf(Character.toLowerCase((char)keyCode));

		try
		{
			Field[] fields = KeyEvent.class.getFields();
			for(int i = 0; i < fields.length; i++)
			{
				Field field = fields[i];
				String name = field.getName();
				if(name.startsWith("VK_")
					&& field.getInt(null) == keyCode)
				{
					return name.substring(3);
				}
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
		}

		return null;
	} 

	
	private void updateAssignedTo(String shortcut)
	{
		String text = jEdit.getProperty("grab-key.assigned-to.none");
		KeyBinding kb = getKeyBinding(shortcut);

		if(kb != null)
			if(kb.isPrefix)
				text = jEdit.getProperty(
					"grab-key.assigned-to.prefix",
					new String[] { shortcut });
			else
				text = kb.label;

		if(ok != null)
			ok.setEnabled(kb == null || !kb.isPrefix);

		assignedTo.setText(
			jEdit.getProperty("grab-key.assigned-to",
				new String[] { text }));
	} 

	
	private KeyBinding getKeyBinding(String shortcut)
	{
		if(shortcut == null || shortcut.length() == 0)
			return null;

		String spacedShortcut = shortcut + " ";
		Enumeration e = allBindings.elements();

		while(e.hasMoreElements())
		{
			KeyBinding kb = (KeyBinding)e.nextElement();

			if(!kb.isAssigned())
				continue;

			String spacedKbShortcut = kb.shortcut + " ";

			
			if(spacedShortcut.startsWith(spacedKbShortcut))
				return kb;

			
			if(spacedKbShortcut.startsWith(spacedShortcut))
			{
				
				
				return new KeyBinding(kb.name,kb.label,
					shortcut,true);
			}
		}

		return null;
	} 

	

	
	
	public static class KeyBinding
	{
		public KeyBinding(String name, String label,
			String shortcut, boolean isPrefix)
		{
			this.name = name;
			this.label = label;
			this.shortcut = shortcut;
			this.isPrefix = isPrefix;
		}

		public String name;
		public String label;
		public String shortcut;
		public boolean isPrefix;

		public boolean isAssigned()
		{
			return shortcut != null && shortcut.length() > 0;
		}
	} 

	
	class InputPane extends JTextField
	{
		
		
		public boolean getFocusTraversalKeysEnabled()
		{
			return false;
		} 

		
		protected void processKeyEvent(KeyEvent _evt)
		{
			KeyEvent evt = KeyEventWorkaround.processKeyEvent(_evt);
			if(evt == null)
			{
				if(debugBuffer != null)
				{
					debugBuffer.insert(debugBuffer.getLength(),
						"Event " + _evt + " filtered\n");
				}
				return;
			}
			else
			{
				if(debugBuffer != null)
				{
					debugBuffer.insert(debugBuffer.getLength(),
						"Event " + _evt + " passed\n");
				}
			}

			evt.consume();

			StringBuffer keyString = new StringBuffer(getText());

			if(getDocument().getLength() != 0)
				keyString.append(' ');

			if(evt.getID() == KeyEvent.KEY_TYPED)
			{
				if(!Character.isLetterOrDigit(evt.getKeyChar())
					&& !Character.isUpperCase(evt.getKeyChar()))
					return;

				keyString.append(evt.getKeyChar());
			}
			else if(evt.getID() == KeyEvent.KEY_PRESSED)
			{
				String modifiers = DefaultInputHandler
					.getModifierString(evt);
				if(modifiers.length() != 0)
				{
					keyString.append(modifiers);
					keyString.append('+');
				}

				int keyCode = evt.getKeyCode();

				if(((keyCode >= KeyEvent.VK_A && keyCode <= KeyEvent.VK_Z)
					|| (keyCode >= KeyEvent.VK_0 && keyCode <= KeyEvent.VK_9))
					&& modifiers.length() == 0)
				{
					
					return;
				}

				String symbolicName = getSymbolicName(keyCode);

				if(symbolicName == null)
					return;

				keyString.append(symbolicName);
			}
			else if(evt.getID() == KeyEvent.KEY_RELEASED)
				return;

			setText(keyString.toString());
			if(debugBuffer == null)
				updateAssignedTo(keyString.toString());
		} 
	} 

	
	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			if(evt.getSource() == ok)
			{
				if(canClose())
					dispose();
			}
			else if(evt.getSource() == remove)
			{
				shortcut.setText(null);
				isOK = true;
				dispose();
			}
			else if(evt.getSource() == cancel)
				dispose();
			else if(evt.getSource() == clear)
			{
				shortcut.setText(null);
				if(debugBuffer == null)
					updateAssignedTo(null);
				shortcut.requestFocus();
			}
		} 

		
		private boolean canClose()
		{
			String shortcutString = shortcut.getText();
			if(shortcutString.length() == 0
				&& binding.isAssigned())
			{
				
				int answer = GUIUtilities.confirm(
					GrabKeyDialog.this,
					"grab-key.remove-ask",
					null,
					JOptionPane.YES_NO_CANCEL_OPTION,
					JOptionPane.QUESTION_MESSAGE);
				if(answer == JOptionPane.YES_OPTION)
				{
					shortcut.setText(null);
					isOK = true;
				}
				else if(answer == JOptionPane.CANCEL_OPTION)
					return false;
				return true;
			}

			
			KeyBinding other = getKeyBinding(shortcutString);
			if(other == null || other == binding)
			{
				isOK = true;
				return true;
			}

			
			if(other.name == binding.name)
			{
				
				GUIUtilities.error(GrabKeyDialog.this,
					"grab-key.duplicate-alt-shortcut",
					null);
				return false;
			}

			
			if(other.isPrefix)
			{
				
				GUIUtilities.error(GrabKeyDialog.this,
					"grab-key.prefix-shortcut",
					null);
				return false;
			}

			
			int answer = GUIUtilities.confirm(GrabKeyDialog.this,
				"grab-key.duplicate-shortcut",
				new Object[] { other.label },
				JOptionPane.YES_NO_CANCEL_OPTION,
				JOptionPane.QUESTION_MESSAGE);
			if(answer == JOptionPane.YES_OPTION)
			{
				if(other.shortcut != null
					&& shortcutString.startsWith(other.shortcut))
				{
					other.shortcut = null;
				}
				isOK = true;
				return true;
			}
			else if(answer == JOptionPane.CANCEL_OPTION)
				return false;

			return true;
		} 
	} 
}
