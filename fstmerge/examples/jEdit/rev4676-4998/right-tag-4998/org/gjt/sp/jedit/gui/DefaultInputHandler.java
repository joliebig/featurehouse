

package org.gjt.sp.jedit.gui;


import javax.swing.KeyStroke;
import java.awt.event.*;
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
		throw new InternalError("Not yet implemented");
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
			KeyStroke keyStroke = parseKeyStroke(st.nextToken());
			if(keyStroke == null)
				return null;

			if(st.hasMoreTokens())
			{
				Object o = current.get(keyStroke);
				if(o instanceof Hashtable)
					current = (Hashtable)o;
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
		return bindings != currentBindings;
	} 

	
	
	public void keyPressed(KeyEvent evt)
	{
		int keyCode = evt.getKeyCode();
		int modifiers = evt.getModifiers();

		if(!(evt.isControlDown() || evt.isAltDown() || evt.isMetaDown()))
		{
			
			
			if((keyCode >= KeyEvent.VK_A && keyCode <= KeyEvent.VK_Z)
				|| (keyCode >= KeyEvent.VK_0 && keyCode <= KeyEvent.VK_9))
			{
				return;
			}
			else if(keyCode == KeyEvent.VK_SPACE)
			{
				return;
			}
			else if(readNextChar != null)
			{
				if(keyCode == KeyEvent.VK_ESCAPE)
				{
					readNextChar = null;
					view.getStatus().setMessage(null);
				}
				else if(!evt.isActionKey()
					&& keyCode != KeyEvent.VK_TAB
					&& keyCode != KeyEvent.VK_ENTER)
				{
					return;
				}
			}
			else
			{
				
			}
		}

		KeyStroke keyStroke = KeyStroke.getKeyStroke(keyCode,
			modifiers);

		Object o = currentBindings.get(keyStroke);
		if(o == null)
		{
			
			
			
			
			if(currentBindings != bindings)
			{
				Toolkit.getDefaultToolkit().beep();
				
				
				repeatCount = 1;
				evt.consume();
				setCurrentBindings(bindings);
			}
		}

		if(readNextChar != null)
		{
			readNextChar = null;
			view.getStatus().setMessage(null);
		}

		if(o instanceof String)
		{
			setCurrentBindings(bindings);
			invokeAction((String)o);
			evt.consume();
		}
		else if(o instanceof EditAction)
		{
			setCurrentBindings(bindings);
			invokeAction((EditAction)o);
			evt.consume();
		}
		else if(o instanceof Hashtable)
		{
			setCurrentBindings((Hashtable)o);
			evt.consume();
		}

		if(o == null)
		{
			switch(evt.getKeyCode())
			{
				case KeyEvent.VK_NUMPAD0:   case KeyEvent.VK_NUMPAD1:
				case KeyEvent.VK_NUMPAD2:   case KeyEvent.VK_NUMPAD3:
				case KeyEvent.VK_NUMPAD4:   case KeyEvent.VK_NUMPAD5:
				case KeyEvent.VK_NUMPAD6:   case KeyEvent.VK_NUMPAD7:
				case KeyEvent.VK_NUMPAD8:   case KeyEvent.VK_NUMPAD9:
				case KeyEvent.VK_MULTIPLY:  case KeyEvent.VK_ADD:
				 case KeyEvent.VK_SUBTRACT:
				case KeyEvent.VK_DECIMAL:   case KeyEvent.VK_DIVIDE:
					KeyEventWorkaround.numericKeypadKey();
					break;
			}
		}
	} 

	
	
	public void keyTyped(KeyEvent evt)
	{
		char c = evt.getKeyChar();

		
		if(c == '\b')
			return;

		if(readNextChar != null)
		{
			setCurrentBindings(bindings);
			invokeReadNextChar(c);
			repeatCount = 1;
			return;
		}

		KeyStroke keyStroke;

		
		
		
		switch(c)
		{
		case ' ':
			keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE,
				evt.getModifiers());
			break;
		case '\t':
			keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
				evt.getModifiers());
			break;
		case '\n':
			keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
				evt.getModifiers());
			break;
		default:
			keyStroke = KeyStroke.getKeyStroke(c);
			break;
		}

		Object o = currentBindings.get(keyStroke);

		if(o instanceof Hashtable)
		{
			setCurrentBindings((Hashtable)o);
		}
		else if(o instanceof String)
		{
			setCurrentBindings(bindings);
			invokeAction((String)o);
		}
		else if(o instanceof EditAction)
		{
			setCurrentBindings(bindings);
			invokeAction((EditAction)o);
		}
		else
		{
			setCurrentBindings(bindings);
			userInput(c);
		}
	} 

	
	
	public static void setModifierMapping(int c, int a, int m, int s)
	{
		DefaultInputHandler.c = c;
		DefaultInputHandler.a = a;
		DefaultInputHandler.m = m;
		DefaultInputHandler.s = s;
	} 

	
	
	public static char getSymbolicModifierName(int mod)
	{
		
		
		if(mod == c)
			return 'C';
		else if(mod == a)
			return 'A';
		else if(mod == m)
			return 'M';
		else if(mod == s)
			return 'S';
		else
			return '\0';
	} 

	
	
	public static String getModifierString(InputEvent evt)
	{
		StringBuffer buf = new StringBuffer();
		if(evt.isControlDown())
			buf.append(getSymbolicModifierName(InputEvent.CTRL_MASK));
		if(evt.isAltDown())
			buf.append(getSymbolicModifierName(InputEvent.ALT_MASK));
		if(evt.isMetaDown())
			buf.append(getSymbolicModifierName(InputEvent.META_MASK));
		if(evt.isShiftDown())
			buf.append(getSymbolicModifierName(InputEvent.SHIFT_MASK));
		return buf.toString();
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
					modifiers |= a;
					break;
				case 'C':
					modifiers |= c;
					break;
				case 'M':
					modifiers |= m;
					break;
				case 'S':
					modifiers |= s;
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

	

	
	static
	{
		if(OperatingSystem.isMacOS())
		{
			setModifierMapping(
				InputEvent.META_MASK,
				InputEvent.ALT_MASK,
				InputEvent.CTRL_MASK,
				InputEvent.SHIFT_MASK);
		}
		else
		{
			setModifierMapping(
				InputEvent.CTRL_MASK,
				InputEvent.ALT_MASK,
				InputEvent.META_MASK,
				InputEvent.SHIFT_MASK);
		}
	} 

	private static int c, a, m, s;

	
	private static Object PREFIX_STR = "PREFIX_STR";

	private Hashtable bindings;
	private Hashtable currentBindings;

	
	private void setCurrentBindings(Hashtable bindings)
	{
		String prefixStr = (String)bindings.get(PREFIX_STR);
		if(prefixStr != null)
		{
			if(currentBindings != this.bindings)
			{
				
				prefixStr = currentBindings.get(PREFIX_STR)
					+ " " + prefixStr;
			}

			view.getStatus().setMessage(prefixStr);
		}
		else
			view.getStatus().setMessage(null);

		currentBindings = bindings;
	} 

	
	
	public void _addKeyBinding(String keyBinding, Object action)
	{
		Hashtable current = bindings;

		StringTokenizer st = new StringTokenizer(keyBinding);
		while(st.hasMoreTokens())
		{
			String keyCodeStr = st.nextToken();
			KeyStroke keyStroke = parseKeyStroke(keyCodeStr);
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
					hash.put(PREFIX_STR,keyCodeStr);
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
