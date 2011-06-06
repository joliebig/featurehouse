

package org.gjt.sp.jedit.gui;


import org.gjt.sp.jedit.bsh.NameSpace;
import java.awt.event.*;
import java.awt.*;
import java.util.ArrayList;
import javax.swing.event.*;
import javax.swing.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.StandardUtilities;



public class ActionBar extends JToolBar
{
	
	public ActionBar(View view, boolean temp)
	{
		this.view = view;
		this.temp = temp;
		
		setLayout(new BoxLayout(this,BoxLayout.X_AXIS));
		setFloatable(false);
		add(Box.createHorizontalStrut(2));

		JLabel label = new JLabel(jEdit.getProperty("view.action.prompt"));
		add(label);
		add(Box.createHorizontalStrut(12));
		add(action = new ActionTextField());
		action.setEnterAddsToHistory(false);
		Dimension max = action.getPreferredSize();
		max.width = Integer.MAX_VALUE;
		action.setMaximumSize(max);
		action.addActionListener(new ActionHandler());
		action.getDocument().addDocumentListener(new DocumentHandler());

		if(temp)
		{
			close = new RolloverButton(GUIUtilities.loadIcon("closebox.gif"));
			close.addActionListener(new ActionHandler());
			close.setToolTipText(jEdit.getProperty(
				"view.action.close-tooltip"));
			add(close);
		}

		
		this.temp = temp;
	} 

	
	public HistoryTextField getField()
	{
		return action;
	} 

	
	public void goToActionBar()
	{
		repeatCount = view.getInputHandler().getRepeatCount();
		action.setText(null);
		action.requestFocus();
	} 

	

	private static NameSpace namespace = new NameSpace(
		BeanShell.getNameSpace(),"action bar namespace");

	
	private View view;
	private boolean temp;
	private int repeatCount;
	private HistoryTextField action;
	private CompletionPopup popup;
	private RolloverButton close;
	

	
	private void invoke()
	{
		String cmd;
		if(popup != null)
			cmd = popup.list.getSelectedValue().toString();
		else
		{
			cmd = action.getText().trim();
			int index = cmd.indexOf('=');
			if(index != -1)
			{
				action.addCurrentToHistory();
				String propName = cmd.substring(0,index).trim();
				String propValue = cmd.substring(index + 1).trim();
				String code;
				
				if(propName.startsWith("buffer."))
				{
					if(propName.equals("buffer.mode"))
					{
						code = "buffer.setMode(\""
							+ StandardUtilities.charsToEscapes(
							propValue) + "\");";
					}
					else
					{
						code = "buffer.setStringProperty(\""
							+ StandardUtilities.charsToEscapes(
							propName.substring("buffer.".length())
							) + "\",\""
							+ StandardUtilities.charsToEscapes(
							propValue) + "\");";
					}

					code += "\nbuffer.propertiesChanged();";
				}
				else if(propName.startsWith("!buffer."))
				{
					code = "jEdit.setProperty(\""
						+ StandardUtilities.charsToEscapes(
						propName.substring(1)) + "\",\""
						+ StandardUtilities.charsToEscapes(
						propValue) + "\");\n"
						+ "jEdit.propertiesChanged();";
				}
				else
				{
					code = "jEdit.setProperty(\""
						+ StandardUtilities.charsToEscapes(
						propName) + "\",\""
						+ StandardUtilities.charsToEscapes(
						propValue) + "\");\n"
						+ "jEdit.propertiesChanged();";
				}

				Macros.Recorder recorder = view.getMacroRecorder();
				if(recorder != null)
					recorder.record(code);
				BeanShell.eval(view,namespace,code);
				cmd = null;
			}
			else if(cmd.length() != 0)
			{
				String[] completions = getCompletions(cmd);
				if(completions.length != 0)
				{
					cmd = completions[0];
				}
			}
			else
				cmd = null;
		}

		if(popup != null)
		{
			popup.dispose();
			popup = null;
		}

		final String finalCmd = cmd;
		final EditAction act = (finalCmd == null ? null : jEdit.getAction(finalCmd));
		if(temp)
			view.removeToolBar(this);

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				view.getTextArea().requestFocus();
				if(act == null)
				{
					if(finalCmd != null)
					{
						view.getStatus().setMessageAndClear(
							jEdit.getProperty(
							"view.action.no-completions"));
					}
				}
				else
				{
					view.getInputHandler().setRepeatCount(repeatCount);
					view.getInputHandler().invokeAction(act);
				}
			}
		});
	} 

	
	private static String[] getCompletions(String str)
	{
		str = str.toLowerCase();
		String[] actions = jEdit.getActionNames();
		ArrayList<String> returnValue = new ArrayList<String>(actions.length);
		for(int i = 0; i < actions.length; i++)
		{
			if(actions[i].toLowerCase().contains(str))
				returnValue.add(actions[i]);
		}

		return returnValue.toArray(new String[returnValue.size()]);
	} 

	
	private void complete(boolean insertLongestPrefix)
	{
		String text = action.getText().trim();
		String[] completions = getCompletions(text);
		if(completions.length == 1)
		{
			if(insertLongestPrefix)
				action.setText(completions[0]);
		}
		else if(completions.length != 0)
		{
			if(insertLongestPrefix)
			{
				String prefix = MiscUtilities.getLongestPrefix(
					completions,true);
				if(prefix.contains(text))
					action.setText(prefix);
			}

			if(popup != null)
				popup.setModel(completions);
			else
				popup = new CompletionPopup(completions);
			return;
		}

		if(popup != null)
		{
			popup.dispose();
			popup = null;
		}
	} 

	

	

	
	private class ActionHandler implements ActionListener
	{
		public void actionPerformed(ActionEvent evt)
		{
			if(evt.getSource() == close)
				view.removeToolBar(ActionBar.this);
			else
				invoke();
		}
	} 

	
	private class DocumentHandler implements DocumentListener
	{
		
		public void insertUpdate(DocumentEvent evt)
		{
			if(popup != null)
				complete(false);
		} 

		
		public void removeUpdate(DocumentEvent evt)
		{
			if(popup != null)
				complete(false);
		} 

		
		public void changedUpdate(DocumentEvent evt) {}
		
	} 

	
	private class ActionTextField extends HistoryTextField
	{
		boolean repeat;
		boolean nonDigit;

		ActionTextField()
		{
			super("action");
			setSelectAllOnFocus(true);
		}

		@Override
		public boolean getFocusTraversalKeysEnabled()
		{
			return false;
		}

		@Override
		public void processKeyEvent(KeyEvent evt)
		{
			evt = KeyEventWorkaround.processKeyEvent(evt);
			if(evt == null)
				return;

			switch(evt.getID())
			{
			case KeyEvent.KEY_TYPED:
				char ch = evt.getKeyChar();
				if(!nonDigit && Character.isDigit(ch))
				{
					super.processKeyEvent(evt);
					repeat = true;
					repeatCount = Integer.parseInt(action.getText());
				}
				else
				{
					nonDigit = true;
					if(repeat)
					{
						passToView(evt);
					}
					else
						super.processKeyEvent(evt);
				}
				break;
			case KeyEvent.KEY_PRESSED:
				int keyCode = evt.getKeyCode();
				if(evt.isActionKey()
					|| evt.isControlDown()
					|| evt.isAltDown()
					|| evt.isMetaDown()
					|| keyCode == KeyEvent.VK_BACK_SPACE
					|| keyCode == KeyEvent.VK_DELETE
					|| keyCode == KeyEvent.VK_ENTER
					|| keyCode == KeyEvent.VK_TAB
					|| keyCode == KeyEvent.VK_ESCAPE)
				{
					nonDigit = true;
					if(repeat)
					{
						passToView(evt);
						break;
					}
					else if(keyCode == KeyEvent.VK_TAB)
					{
						complete(true);
						evt.consume();
					}
					else if(keyCode == KeyEvent.VK_ESCAPE)
					{
						evt.consume();
						if(popup != null)
						{
							popup.dispose();
							popup = null;
							action.requestFocus();
						}
						else
						{
							if(temp)
								view.removeToolBar(ActionBar.this);
							view.getEditPane().focusOnTextArea();
						}
						break;
					}
					else if((keyCode == KeyEvent.VK_UP
						|| keyCode == KeyEvent.VK_DOWN)
						&& popup != null)
					{
						popup.list.processKeyEvent(evt);
						break;
					}
				}
				super.processKeyEvent(evt);
				break;
			}
		}

		private void passToView(final KeyEvent evt)
		{
			if(temp)
				view.removeToolBar(ActionBar.this);
			view.getTextArea().requestFocus();
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					view.getTextArea().requestFocus();
					view.getInputHandler().setRepeatCount(repeatCount);
					view.getInputHandler().processKeyEvent(evt,
						View.ACTION_BAR, false);
				}
			});
		}

		@Override
		public void addNotify()
		{
			super.addNotify();
			repeat = nonDigit = false;
		}
	} 

	
	private class CompletionPopup extends JWindow
	{
		CompletionList list;

		
		CompletionPopup(String[] actions)
		{
			super(view);

			setContentPane(new JPanel(new BorderLayout())
			{
				
				@Override
				public boolean getFocusTraversalKeysEnabled()
				{
					return false;
				}
			});

			list = new CompletionList(actions);
			list.setVisibleRowCount(8);
			list.addMouseListener(new MouseHandler());
			list.setSelectedIndex(0);
			list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

			
			
			JScrollPane scroller = new JScrollPane(list,
				ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

			getContentPane().add(scroller, BorderLayout.CENTER);

			GUIUtilities.requestFocus(this,list);

			pack();
			Point p = new Point(0,-getHeight());
			SwingUtilities.convertPointToScreen(p,action);
			setLocation(p);
			setVisible(true);

			KeyHandler keyHandler = new KeyHandler();
			addKeyListener(keyHandler);
			list.addKeyListener(keyHandler);
		} 

		
		void setModel(String[] actions)
		{
			list.setListData(actions);
			list.setSelectedIndex(0);
		} 

		
		private class MouseHandler extends MouseAdapter
		{
			@Override
			public void mouseClicked(MouseEvent evt)
			{
				invoke();
			}
		} 

		
		class CompletionList extends JList
		{
			CompletionList(Object[] data)
			{
				super(data);
			}

			
			@Override
			public void processKeyEvent(KeyEvent evt)
			{
				super.processKeyEvent(evt);
			}
		} 

		
		private class KeyHandler extends KeyAdapter
		{
			@Override
			public void keyTyped(KeyEvent evt)
			{
				action.processKeyEvent(evt);
			}

			@Override
			public void keyPressed(KeyEvent evt)
			{
				int keyCode = evt.getKeyCode();
				if(keyCode == KeyEvent.VK_ESCAPE)
					action.processKeyEvent(evt);
				else if(keyCode == KeyEvent.VK_ENTER)
					invoke();
				else if(keyCode == KeyEvent.VK_UP)
				{
					int selected = list.getSelectedIndex();
					if(selected == 0)
					{
						list.setSelectedIndex(
							list.getModel().getSize()
							- 1);
						evt.consume();
					}
				}
				else if(keyCode == KeyEvent.VK_DOWN)
				{
					int selected = list.getSelectedIndex();
					if(selected == list.getModel().getSize() - 1)
					{
						list.setSelectedIndex(0);
						evt.consume();
					}
				}
			}
		} 
	} 

	
}
