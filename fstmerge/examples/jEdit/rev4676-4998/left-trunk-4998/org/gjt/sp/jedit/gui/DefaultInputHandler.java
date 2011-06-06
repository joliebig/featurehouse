

package org.gjt.sp.jedit.gui;


import javax.swing.KeyStroke;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.awt.Toolkit;
import java.util.Hashtable;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class DefaultInputHandler extends InputHandler
{
	
	
	public DefaultInputHandler(View view)
	{
		super(view);

		bindings = currentBindings = new Hashtable();
	} 

	
	
	public DefaultInputHandler(View view, DefaultInputHandler copy)
	{
		super(view);

		bindings = currentBindings = copy.bindings;
	} 

	
	
	public void addKeyBinding(String keyBinding, String action)
	{
		_addKeyBinding(keyBinding,(Object)action);
	} 

	
	
	public void addKeyBinding(String keyBinding, EditAction action)
	{
		_addKeyBinding(keyBinding,(Object)action);
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
			KeyEventTranslator.Key keyStroke = KeyEventTranslator.parseKey(keyBinding);
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

	
	
	public boolean handleKey(KeyEventTranslator.Key keyStroke)
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
				setCurrentBindings(bindings);
				invokeReadNextChar(input);
				repeatCount = 1;
				return true;
			}
			else
			{
				readNextChar = null;
				view.getStatus().setMessage(null);
			}
		}

		Object o = currentBindings.get(keyStroke);
		if(o == null)
		{
			
			
			
			
			if(currentBindings != bindings)
			{
				Toolkit.getDefaultToolkit().beep();
				
				
				repeatCount = 1;
				setCurrentBindings(bindings);
			}

			if(input != '\0')
				userInput(input);
			else
			{
				
				
				switch(keyStroke.key)
				{
				case KeyEvent.VK_NUMPAD0:
				case KeyEvent.VK_NUMPAD1:
				case KeyEvent.VK_NUMPAD2:
				case KeyEvent.VK_NUMPAD3:
				case KeyEvent.VK_NUMPAD4:
				case KeyEvent.VK_NUMPAD5:
				case KeyEvent.VK_NUMPAD6:
				case KeyEvent.VK_NUMPAD7:
				case KeyEvent.VK_NUMPAD8:
				case KeyEvent.VK_NUMPAD9:
				case KeyEvent.VK_MULTIPLY:
				case KeyEvent.VK_ADD:
				
				case KeyEvent.VK_SUBTRACT:
				case KeyEvent.VK_DECIMAL:
				case KeyEvent.VK_DIVIDE:
					KeyEventWorkaround.numericKeypadKey();
					break;
				}
			}
		}
		else if(o instanceof Hashtable)
		{
			setCurrentBindings((Hashtable)o);
			return true;
		}
		else if(o instanceof String)
		{
			setCurrentBindings(bindings);
			invokeAction((String)o);
			return true;
		}
		else if(o instanceof EditAction)
		{
			setCurrentBindings(bindings);
			invokeAction((EditAction)o);
			return true;
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

	
	
	public static KeyStroke parseKeyStroke(String keyStroke)
	{
		if(keyStroke == null)
			return null;
		int modifiers = 0;
		int index = keyStroke.indexOf('+');
		if(index != -1)
		{
			for(int i = 0; i < index; i++)
			{
				switch(Character.toUpperCase(keyStroke
					.charAt(i)))
				{
				case 'A':
					modifiers |= KeyEventTranslator.a;
					break;
				case 'C':
					modifiers |= KeyEventTranslator.c;
					break;
				case 'M':
					modifiers |= KeyEventTranslator.m;
					break;
				case 'S':
					modifiers |= KeyEventTranslator.s;
					break;
				}
			}
		}
		String key = keyStroke.substring(index + 1);
		if(key.length() == 1)
		{
			char ch = key.charAt(0);
			if(modifiers == 0)
				return KeyStroke.getKeyStroke(ch);
			else
			{
				return KeyStroke.getKeyStroke(Character.toUpperCase(ch),
					modifiers);
			}
		}
		else if(key.length() == 0)
		{
			Log.log(Log.ERROR,DefaultInputHandler.class,
				"Invalid key stroke: " + keyStroke);
			return null;
		}
		else
		{
			int ch;

			try
			{
				ch = KeyEvent.class.getField("VK_".concat(key))
					.getInt(null);
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,DefaultInputHandler.class,
					"Invalid key stroke: "
					+ keyStroke);
				return null;
			}

			return KeyStroke.getKeyStroke(ch,modifiers);
		}
	} 

	

	
	private static Object PREFIX_STR = "PREFIX_STR";

	private Hashtable bindings;
	private Hashtable currentBindings;

	
	private void setCurrentBindings(Hashtable bindings)
	{
		view.getStatus().setMessage((String)bindings.get(PREFIX_STR));
		currentBindings = bindings;
	} 

	
	
	public void _addKeyBinding(String keyBinding, Object action)
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

	
}
