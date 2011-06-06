

package org.gjt.sp.jedit.gui;


import java.awt.event.InputEvent;
import java.awt.Toolkit;
import java.util.Hashtable;
import org.gjt.sp.jedit.*;



public class DefaultInputHandler extends InputHandler
{
	
	
	public DefaultInputHandler(View view, Hashtable bindings)
	{
		super(view);

		if(bindings == null)
			throw new NullPointerException();
		this.bindings = this.currentBindings = bindings;
	} 

	
	
	public DefaultInputHandler(View view)
	{
		this(view,new Hashtable());
	} 

	
	
	public DefaultInputHandler(View view, DefaultInputHandler copy)
	{
		this(view,copy.bindings);
	} 

	
	
	@Override
	public boolean isPrefixActive()
	{
		return bindings != currentBindings
			|| super.isPrefixActive();
	} 

	
	@Override
	public void setCurrentBindings(Hashtable bindings)
	{
		view.getStatus().setMessage((String)bindings.get(PREFIX_STR));
		currentBindings = bindings;
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
					view.getStatus().setMessage(null);
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
		else if(o instanceof EditAction)
		{
			if (!dryRun)
			{
				setCurrentBindings(bindings);
				sendShortcutPrefixOff();
				invokeAction((EditAction)o);
			}
			return true;
		}
		if (!dryRun)
		{
			sendShortcutPrefixOff();
		}
		return false;
	} 
	
	
	
	public static char getSymbolicModifierName(int mod)
	{
		return KeyEventTranslator.getSymbolicModifierName(mod);
	} 

	
	
	public static String getModifierString(InputEvent evt)
	{
		return KeyEventTranslator.getModifierString(evt);
	} 
}
