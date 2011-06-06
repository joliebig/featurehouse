

package org.gjt.sp.jedit.gui;


import javax.swing.JOptionPane;
import java.awt.event.*;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.*;



public abstract class InputHandler extends KeyAdapter
{
	
	
	public InputHandler(View view)
	{
		this.view = view;
		repeatCount = 1;
	} 

	
	
	public void processKeyEvent(KeyEvent evt)
	{
		switch(evt.getID())
		{
		case KeyEvent.KEY_TYPED:
			keyTyped(evt);
			break;
		case KeyEvent.KEY_PRESSED:
			keyPressed(evt);
			break;
		case KeyEvent.KEY_RELEASED:
			keyReleased(evt);
			break;
		}
	} 

	
	
	public abstract void addKeyBinding(String keyBinding, String action);
	

	
	
	public abstract void addKeyBinding(String keyBinding, EditAction action);
	

	
	
	public abstract void removeKeyBinding(String keyBinding);
	

	
	
	public abstract void removeAllKeyBindings();
	

	
	
	public boolean isPrefixActive()
	{
		return false;
	} 

	
	
	public int getRepeatCount()
	{
		return repeatCount;
	} 

	
	
	public void setRepeatCount(int repeatCount)
	{
		int oldRepeatCount = this.repeatCount;
		this.repeatCount = repeatCount;
		if(oldRepeatCount != repeatCount)
			view.getStatus().setMessage(null);
	} 

	
	
	public EditAction getLastAction()
	{
		return lastAction;
	} 

	
	
	public int getLastActionCount()
	{
		return lastActionCount;
	} 

	
	
	public void readNextChar(String msg, String code)
	{
		view.getStatus().setMessage(msg);
		readNextChar = code;
	} 

	
	
	public void readNextChar(String code)
	{
		readNextChar = code;
	} 

	
	
	public void resetLastActionCount()
	{
		lastActionCount = 0;
	} 

	
	
	public void invokeAction(String action)
	{
		invokeAction(jEdit.getAction(action));
	} 

	
	
	public void invokeAction(EditAction action)
	{
		Buffer buffer = view.getBuffer();

		

		
		if(!action.noRememberLast())
		{
			HistoryModel.getModel("action").addItem(action.getName());
			if(lastAction == action)
				lastActionCount++;
			else
			{
				lastAction = action;
				lastActionCount = 1;
			}
		}

		
		int _repeatCount = repeatCount;

		
		if(action.noRepeat() || _repeatCount == 1)
			action.invoke(view);
		else
		{
			
			if(_repeatCount > REPEAT_COUNT_THRESHOLD)
			{
				String label = action.getLabel();
				if(label == null)
					label = action.getName();
				else
					label = GUIUtilities.prettifyMenuLabel(label);

				Object[] pp = { label, new Integer(_repeatCount) };

				if(GUIUtilities.confirm(view,"large-repeat-count",pp,
					JOptionPane.WARNING_MESSAGE,
					JOptionPane.YES_NO_OPTION)
					!= JOptionPane.YES_OPTION)
				{
					repeatCount = 1;
					view.getStatus().setMessage(null);
					return;
				}
			}

			try
			{
				buffer.beginCompoundEdit();

				for(int i = 0; i < _repeatCount; i++)
					action.invoke(view);
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}

		Macros.Recorder recorder = view.getMacroRecorder();

		if(recorder != null && !action.noRecord())
			recorder.record(_repeatCount,action.getCode());

		
		
		if(_repeatCount != 1)
		{
			
			
			if(readNextChar != null)
				return;

			repeatCount = 1;
			view.getStatus().setMessage(null);
		}
	} 

	
	public void invokeLastAction()
	{
		if(lastAction == null)
			view.getToolkit().beep();
		else
			invokeAction(lastAction);
	} 

	
	private static final int REPEAT_COUNT_THRESHOLD = 20;

	
	protected View view;
	protected int repeatCount;

	protected EditAction lastAction;
	protected int lastActionCount;

	protected String readNextChar;
	

	
	protected void userInput(char ch)
	{
		lastActionCount = 0;

		JEditTextArea textArea = view.getTextArea();

		

		if(repeatCount == 1)
			textArea.userInput(ch);
		else
		{
			
			if(repeatCount > REPEAT_COUNT_THRESHOLD)
			{
				Object[] pp = { String.valueOf(ch),
					new Integer(repeatCount) };

				if(GUIUtilities.confirm(view,
					"large-repeat-count.user-input",pp,
					JOptionPane.WARNING_MESSAGE,
					JOptionPane.YES_NO_OPTION)
					!= JOptionPane.YES_OPTION)
				{
					repeatCount = 1;
					view.getStatus().setMessage(null);
					return;
				}
			}

			Buffer buffer = view.getBuffer();
			try
			{
				if(repeatCount != 1)
					buffer.beginCompoundEdit();
				for(int i = 0; i < repeatCount; i++)
					textArea.userInput(ch);
			}
			finally
			{
				if(repeatCount != 1)
					buffer.endCompoundEdit();
			}
		}

		Macros.Recorder recorder = view.getMacroRecorder();

		if(recorder != null)
			recorder.record(repeatCount,ch);

		repeatCount = 1;
	} 

	
	protected void invokeReadNextChar(char ch)
	{
		Buffer buffer = view.getBuffer();

		

		String charStr = MiscUtilities.charsToEscapes(String.valueOf(ch));

		
		int index;
		while((index = readNextChar.indexOf("__char__")) != -1)
		{
			readNextChar = readNextChar.substring(0,index)
				+ '\'' + charStr + '\''
				+ readNextChar.substring(index + 8);
		}

		Macros.Recorder recorder = view.getMacroRecorder();
		if(recorder != null)
			recorder.record(getRepeatCount(),readNextChar);

		if(getRepeatCount() != 1)
		{
			try
			{
				buffer.beginCompoundEdit();

				BeanShell.eval(view,BeanShell.getNameSpace(),
					"for(int i = 1; i < "
					+ getRepeatCount() + "; i++)\n{\n"
					+ readNextChar + "\n}");
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}
		else
			BeanShell.eval(view,BeanShell.getNameSpace(),readNextChar);

		readNextChar = null;

		view.getStatus().setMessage(null);
	} 

	
}
