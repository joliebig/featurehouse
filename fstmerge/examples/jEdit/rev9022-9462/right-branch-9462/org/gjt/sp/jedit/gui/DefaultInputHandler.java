

package org.gjt.sp.jedit.gui;


import javax.swing.KeyStroke;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.awt.Toolkit;
import java.util.Hashtable;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.jedit.msg.*;
import javax.swing.event.*;



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

	
	
	public void addKeyBinding(String keyBinding, String action)
	{
		addKeyBinding(keyBinding,(Object)action);
	} 

	
	
	public void addKeyBinding(String keyBinding, EditAction action)
	{
		addKeyBinding(keyBinding,(Object)action);
	} 

	
	
	public void addKeyBinding(String keyBinding, Object action)
	{
		Hashtable current = bindings;

		String prefixStr = null;

		StringTokenizer st = new StringTokenizer(keyBinding);
		while(st.hasMoreTokens())
		{
			String keyCodeStr = st.nextToken();
			if(prefixStr == null)
				prefixStr = keyCodeStr;
			else
				prefixStr = prefixStr + " " + keyCodeStr;

			KeyEventTranslator.Key keyStroke = KeyEventTranslator.parseKey(keyCodeStr);
			if(keyStroke == null)
				return;

			if(st.hasMoreTokens())
			{
				Object o = current.get(keyStroke);
				if(o instanceof Hashtable)
					current = (Hashtable)o;
				else
				{
					Hashtable hash = new Hashtable();
					hash.put(PREFIX_STR,prefixStr);
					o = hash;
					current.put(keyStroke,o);
					current = (Hashtable)o;
				}
			}
			else
				current.put(keyStroke,action);
		}
	} 

	
	
	public void removeKeyBinding(String keyBinding)
	{
		Hashtable current = bindings;

		StringTokenizer st = new StringTokenizer(keyBinding);
		while(st.hasMoreTokens())
		{
			String keyCodeStr = st.nextToken();
			KeyEventTranslator.Key keyStroke = KeyEventTranslator.parseKey(keyCodeStr);
			if(keyStroke == null)
				return;

			if(st.hasMoreTokens())
			{
				Object o = current.get(keyStroke);
				if(o instanceof Hashtable)
					current = ((Hashtable)o);
				else if(o != null)
				{
					
					
					current.remove(keyStroke);
					return;
				}
				else
				{
					
					return;
				}
			}
			else
				current.remove(keyStroke);
		}
	} 

	
	
	public void removeAllKeyBindings()
	{
		bindings.clear();
	} 

	
	
	public Object getKeyBinding(String keyBinding)
	{
		Hashtable current = bindings;
		StringTokenizer st = new StringTokenizer(keyBinding);

		while(st.hasMoreTokens())
		{
			KeyEventTranslator.Key keyStroke = KeyEventTranslator.parseKey(
				st.nextToken());
			if(keyStroke == null)
				return null;

			if(st.hasMoreTokens())
			{
				Object o = current.get(keyStroke);
				if(o instanceof Hashtable)
				{
					if(!st.hasMoreTokens())
						return o;
					else
						current = (Hashtable)o;
				}
				else
					return o;
			}
			else
			{
				return current.get(keyStroke);
			}
		}

		return null;
	} 

	
	
	public boolean isPrefixActive()
	{
		return bindings != currentBindings
			|| super.isPrefixActive();
	} 

	
	
	public void setBindings(Hashtable bindings)
	{
		this.bindings = this.currentBindings = bindings;
	} 

	
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
				if (!dryRun) {
					setCurrentBindings(bindings);
					invokeReadNextChar(input);
					repeatCount = 1;
				}
				return true;
			}
			else
			{
				if (!dryRun) {
					readNextChar = null;
					view.getStatus().setMessage(null);
				}
			}
		}

		Object o = currentBindings.get(keyStroke);
		if(o == null)
		{
			if (!dryRun) {
				
				
				
				
				if(currentBindings != bindings)
				{
					Toolkit.getDefaultToolkit().beep();
					
					
					repeatCount = 1;
					setCurrentBindings(bindings);
				}
				else if(input != '\0') {
					if (!keyStroke.isFromGlobalContext()) { 
						userInput(input);
					}
				} else	{
					
					
					if(KeyEventWorkaround.isNumericKeypad(keyStroke.key))
						KeyEventWorkaround.numericKeypadKey();
				}
				sendShortcutPrefixOff();
			}
		}
		else if(o instanceof Hashtable)
		{
			if (!dryRun) {
				setCurrentBindings((Hashtable)o);
				ShortcutPrefixActiveEvent.firePrefixStateChange(currentBindings, true);
				shortcutOn = true;
			}
			return true;
		}
		else if(o instanceof String)
		{
			if (!dryRun) {
				setCurrentBindings(bindings);
				sendShortcutPrefixOff();
				invokeAction((String)o);
			}
			return true;
		}
		else if(o instanceof EditAction)
		{
			if (!dryRun) {
				setCurrentBindings(bindings);
				sendShortcutPrefixOff();
				invokeAction((EditAction)o);
			}
			return true;
		}
		if (!dryRun) {
			sendShortcutPrefixOff();
		}
		return false;
	} 

	
	
	protected void sendShortcutPrefixOff()
	{
		if( shortcutOn == true )
		{
			ShortcutPrefixActiveEvent.firePrefixStateChange(null, false);
			shortcutOn = false;
		}
	} 
	
	protected boolean shortcutOn=false;
	
	
	
	public static char getSymbolicModifierName(int mod)
	{
		return KeyEventTranslator.getSymbolicModifierName(mod);
	} 

	
	
	public static String getModifierString(InputEvent evt)
	{
		return KeyEventTranslator.getModifierString(evt);
	} 

	

	
	public static Object PREFIX_STR = "PREFIX_STR";

	private Hashtable bindings;
	private Hashtable currentBindings;
	

}
