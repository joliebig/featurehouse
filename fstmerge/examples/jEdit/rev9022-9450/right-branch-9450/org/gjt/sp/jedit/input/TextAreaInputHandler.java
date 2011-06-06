
package org.gjt.sp.jedit.input;

import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.jedit.gui.GrabKeyDialog;
import org.gjt.sp.jedit.gui.KeyEventWorkaround;
import org.gjt.sp.jedit.gui.KeyEventTranslator;
import org.gjt.sp.jedit.Debug;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.util.Log;

import javax.swing.*;
import java.awt.event.KeyEvent;
import java.awt.*;


public class TextAreaInputHandler extends AbstractInputHandler
{
	private TextArea textArea;


	public TextAreaInputHandler(TextArea textArea)
	{
		this.textArea = textArea;
	}

	
	public void processKeyEvent(KeyEvent evt, int from, boolean global)
	{
		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event                 : "
				+ GrabKeyDialog.toString(evt) + " from " + from);
		
		}

		evt = _preprocessKeyEvent(evt);
		if(evt == null)
			return;

		if(Debug.DUMP_KEY_EVENTS)
		{
			Log.log(Log.DEBUG,this,"Key event after workaround: "
				+ GrabKeyDialog.toString(evt) + " from " + from);
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
		Component focusOwner = textArea;
		if (true )
		{
			
		}
		else
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
		
		
		
		if (isPrefixActive() && focusOnTextArea)
		{
			textArea.requestFocus();
		}
	} 

	
	
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
				if (!dryRun) {
					invokeReadNextChar(input);
					repeatCount = 1;
				}
				return true;
			}
			else
			{
				if (!dryRun) {
					readNextChar = null;
				}
			}
		}
		if (!dryRun)
		{
			if(input != '\0') {
				if (!keyStroke.isFromGlobalContext()) { 
					userInput(input);
				}
			} else	{
				
				
				if(KeyEventWorkaround.isNumericKeypad(keyStroke.key))
					KeyEventWorkaround.numericKeypadKey();
				else
				{
					switch (keyStroke.key)
					{
						case KeyEvent.VK_LEFT:
							textArea.goToPrevCharacter("S".equals(keyStroke.modifiers));
							break;
						case KeyEvent.VK_RIGHT:
							textArea.goToNextCharacter("S".equals(keyStroke.modifiers));
							break;
						case KeyEvent.VK_UP:
							textArea.goToPrevLine("S".equals(keyStroke.modifiers));
							break;
						case KeyEvent.VK_DOWN:
							textArea.goToNextLine("S".equals(keyStroke.modifiers));
							break;

					}
				}

			}
		}
		return false;
	} 

		
	protected void userInput(char ch)
	{
		lastActionCount = 0;


		if(repeatCount == 1)
			textArea.userInput(ch);
		else
		{
			
			
		}

		repeatCount = 1;
	} 

	
	protected void invokeReadNextChar(char ch)
	{
		String charStr = MiscUtilities.charsToEscapes(String.valueOf(ch));

		
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
