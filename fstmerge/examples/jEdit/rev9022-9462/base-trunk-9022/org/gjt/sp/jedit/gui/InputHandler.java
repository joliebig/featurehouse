

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import javax.swing.text.JTextComponent;

import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.input.AbstractInputHandler;
import org.gjt.sp.util.Log;

import java.awt.event.KeyEvent;
import java.awt.*;



public abstract class InputHandler extends AbstractInputHandler
{
	
	
	public InputHandler(View view)
	{
		super();
		this.view = view;
	} 

	
	
	public abstract void addKeyBinding(String keyBinding, String action);
	

	
	
	public abstract void addKeyBinding(String keyBinding, EditAction action);
	

	
	
	public abstract void removeKeyBinding(String keyBinding);
	

	
	
	public abstract void removeAllKeyBindings();
	

	
	public final boolean handleKey(KeyEventTranslator.Key keyStroke)
	{
		return handleKey(keyStroke, false);
	}

	
	public void processKeyEvent(KeyEvent evt, int from, boolean global)
	{
		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event                 : "
				+ GrabKeyDialog.toString(evt) + " from " + from);
			Log.log(Log.DEBUG,this,view+".isFocused()="+view.isFocused()+'.',new Exception());
		}

		if(view.getTextArea().hasFocus() && from == View.VIEW)
			return;

		evt = _preprocessKeyEvent(evt);
		if(evt == null)
			return;

		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event after workaround: "
				+ GrabKeyDialog.toString(evt) + " from " + from);
		}

		Component prefixFocusOwner = view.getPrefixFocusOwner();
		boolean focusOnTextArea = false;
		switch(evt.getID())
		{
		case KeyEvent.KEY_TYPED:
			
			
			
			if(prefixFocusOwner != null)
			{
				if(prefixFocusOwner.isShowing())
				{
					prefixFocusOwner.requestFocus();
					focusOnTextArea = true;
				}
			}

			if(keyEventInterceptor != null)
				keyEventInterceptor.keyTyped(evt);
			else if(from == View.ACTION_BAR
				|| (Debug.GLOBAL_SHORTCUTS_FOR_DOCKED_DOCKABLES &&
				    Options.SIMPLIFIED_KEY_HANDLING)
				|| isPrefixActive()
				|| view.getTextArea().hasFocus())
			{
				processKeyEventKeyStrokeHandling(evt,from,"type ",global);
			}


			processKeyEventSub(focusOnTextArea);

			break;
		case KeyEvent.KEY_PRESSED:
			if(keyEventInterceptor != null)
				keyEventInterceptor.keyPressed(evt);
			else if(KeyEventWorkaround.isBindable(evt.getKeyCode()))
			{
				if(prefixFocusOwner != null)
				{
					if(prefixFocusOwner.isShowing())
					{
						prefixFocusOwner.requestFocus();
						focusOnTextArea = true;
					}
					view.setPrefixFocusOwner(null);
				}

				processKeyEventKeyStrokeHandling(evt,from,"press",global);

				processKeyEventSub(focusOnTextArea);

			}
			break;
		case KeyEvent.KEY_RELEASED:
			if(keyEventInterceptor != null)
				keyEventInterceptor.keyReleased(evt);
			break;
		}
	} 
	
	
	private KeyEvent _preprocessKeyEvent(KeyEvent evt)
	{
		if(view.isClosed())
			return null;
		Component focusOwner = view.getFocusOwner();
		if (Options.SIMPLIFIED_KEY_HANDLING)
		{
			
		}
		else
		{
			if(focusOwner instanceof JComponent)
			{
				JComponent comp = (JComponent)focusOwner;
				InputMap map = comp.getInputMap();
				ActionMap am = comp.getActionMap();

				if(map != null && am != null && comp.isEnabled())
				{
					KeyStroke	keyStroke	= KeyStroke.getKeyStrokeForEvent(evt);
					Object binding = map.get(keyStroke);
					if(binding != null && am.get(binding) != null)
					{
						return null;
					}
				}
			}
		}

		if(focusOwner instanceof JTextComponent)
		{
			
			
			if(evt.getID() == KeyEvent.KEY_PRESSED)
			{
				switch(evt.getKeyCode())
				{
				case KeyEvent.VK_ENTER:
				case KeyEvent.VK_TAB:
				case KeyEvent.VK_BACK_SPACE:
				case KeyEvent.VK_SPACE:
					return null;
				}
			}
		}

		if(evt.isConsumed())
			return null;

		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event (preprocessing) : "
					+ GrabKeyDialog.toString(evt));
		}

		return KeyEventWorkaround.processKeyEvent(evt);
	} 

	
	private void processKeyEventSub(boolean focusOnTextArea)
	{
		
		
		if(view.isClosed())
			return;

		
		
		
		if(isPrefixActive())
		{
			Component focusOwner = view.getFocusOwner();
			if(focusOwner instanceof JTextComponent)
			{
				view.setPrefixFocusOwner(focusOwner);
				view.getTextArea().requestFocus();
			}
			else if(focusOnTextArea)
			{
				view.getTextArea().requestFocus();
			}
			else
			{
				view.setPrefixFocusOwner(null);
			}
		}
		else
		{
			view.setPrefixFocusOwner(null);
		}
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

	
	
	public void readNextChar(String msg, String code)
	{
		view.getStatus().setMessage(msg);
		readNextChar = code;
	} 

	
	
	public void readNextChar(String code)
	{
		readNextChar = code;
	} 

	
	
	public void invokeAction(String action)
	{
		invokeAction(jEdit.getAction(action));
	} 

	
	
	public void invokeAction(EditAction action)
	{
		JEditBuffer buffer = view.getBuffer();

		

		
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

				Object[] pp = { label, _repeatCount };

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


	
	protected View view;

	protected EditAction lastAction;

	

	
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
					repeatCount };

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

			JEditBuffer buffer = view.getBuffer();
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
		{
			recorder.recordInput(repeatCount,ch,
				textArea.isOverwriteEnabled());
		}

		repeatCount = 1;
	} 

	
	protected void invokeReadNextChar(char ch)
	{
		JEditBuffer buffer = view.getBuffer();

		

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

		view.getStatus().setMessage(null);

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
	} 

	
}
