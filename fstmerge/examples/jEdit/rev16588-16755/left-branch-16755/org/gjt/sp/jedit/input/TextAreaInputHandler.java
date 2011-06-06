
package org.gjt.sp.jedit.input;


import org.gjt.sp.jedit.Debug;
import org.gjt.sp.jedit.gui.KeyEventTranslator;
import org.gjt.sp.jedit.gui.KeyEventWorkaround;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.Hashtable;
import org.gjt.sp.jedit.JEditBeanShellAction;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.gui.ShortcutPrefixActiveEvent;



public abstract class TextAreaInputHandler extends AbstractInputHandler<JEditBeanShellAction>
{
	private final TextArea textArea;

	
	protected TextAreaInputHandler(TextArea textArea)
	{
		this.textArea = textArea;
		bindings = currentBindings = new Hashtable();
	} 

	
	
	@Override
	public void processKeyEvent(KeyEvent evt, int from, boolean global)
	{
		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event                 : "
				+ toString(evt) + " from " + from);
		
		}

		evt = _preprocessKeyEvent(evt);
		if(evt == null)
			return;

		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event after workaround: "
				+ toString(evt) + " from " + from);
		}

		boolean focusOnTextArea = false;
		switch(evt.getID())
		{
		case KeyEvent.KEY_TYPED:
			
			
			


			if(keyEventInterceptor != null)
				keyEventInterceptor.keyTyped(evt);
			else if(isPrefixActive() || textArea.hasFocus())
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
		if(evt.isConsumed())
			return null;

		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event (preprocessing) : "
					+ toString(evt));
		}

		return KeyEventWorkaround.processKeyEvent(evt);
	} 

	
	private void processKeyEventSub(boolean focusOnTextArea)
	{
		
		
		
		if (isPrefixActive() && focusOnTextArea)
		{
			textArea.requestFocus();
		}
	} 

	
	protected abstract JEditBeanShellAction getAction(String action);
	

	
	
	@Override
	public void invokeAction(String action)
	{
		invokeAction(getAction(action));
	} 

	
	
	@Override
	public void invokeAction(JEditBeanShellAction action)
	{
		JEditBuffer buffer = textArea.getBuffer();

		

		
		if(!action.noRememberLast())
		{
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
			action.invoke(textArea);
		else
		{
			try
			{
				buffer.beginCompoundEdit();

				for(int i = 0; i < _repeatCount; i++)
					action.invoke(textArea);
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}

		
		
		if(_repeatCount != 1)
		{
			
			
			if(readNextChar != null)
				return;

			repeatCount = 1;
		}
	} 

	
	
	@Override
	public boolean handleKey(KeyEventTranslator.Key keyStroke,boolean dryRun)
	{
		char input = '\0';
		if(keyStroke.modifiers == null
			|| keyStroke.modifiers.equals("S"))
		{
			switch(keyStroke.key)
			{
			case '\n':
			case '\t':
				input = (char)keyStroke.key;
				break;
			default:
				input = keyStroke.input;
				break;
			}
		}

		if(readNextChar != null)
		{
			if(input != '\0')
			{
				if (!dryRun)
				{
					setCurrentBindings(bindings);
					invokeReadNextChar(input);
					repeatCount = 1;
				}
				return true;
			}
			else
			{
				if (!dryRun)
				{
					readNextChar = null;
				}
			}
		}

		Object o = currentBindings.get(keyStroke);
		if(o == null)
		{
			if (!dryRun)
			{
				
				
				
				
				if(currentBindings != bindings)
				{
					Toolkit.getDefaultToolkit().beep();
					
					
					repeatCount = 1;
					setCurrentBindings(bindings);
				}
				else if(input != '\0')
				{
					if (!keyStroke.isFromGlobalContext())
					{ 
						userInput(input);
					}
				}
				else
				{
					
					
					if(KeyEventWorkaround.isNumericKeypad(keyStroke.key))
						KeyEventWorkaround.numericKeypadKey();
				}
				sendShortcutPrefixOff();
			}
		}
		else if(o instanceof Hashtable)
		{
			if (!dryRun)
			{
				setCurrentBindings((Hashtable)o);
				ShortcutPrefixActiveEvent.firePrefixStateChange(currentBindings, true);
				shortcutOn = true;
			}
			return true;
		}
		else if(o instanceof String)
		{
			if (!dryRun)
			{
				setCurrentBindings(bindings);
				sendShortcutPrefixOff();
				invokeAction((String)o);
			}
			return true;
		}
		else if(o instanceof JEditBeanShellAction)
		{
			if (!dryRun)
			{
				setCurrentBindings(bindings);
				sendShortcutPrefixOff();
				invokeAction((JEditBeanShellAction)o);
			}
			return true;
		}
		if (!dryRun)
		{
			sendShortcutPrefixOff();
		}
		return false;
	} 

	
	protected void userInput(char ch)
	{
		lastActionCount = 0;


		if(repeatCount == 1)
			textArea.userInput(ch);

		repeatCount = 1;
	} 

	
	protected void invokeReadNextChar(char ch)
	{
		String charStr = StandardUtilities.charsToEscapes(String.valueOf(ch));

		
		int index;
		while((index = readNextChar.indexOf("__char__")) != -1)
		{
			readNextChar = readNextChar.substring(0,index)
				+ '\'' + charStr + '\''
				+ readNextChar.substring(index + 8);
		}
		readNextChar = null;
	} 
}
