

package org.gjt.sp.jedit.gui;


import java.awt.event.*;
import java.awt.Toolkit;
import java.util.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class KeyEventTranslator
{
	
	
	public static void addTranslation(Key key1, Key key2)
	{
		transMap.put(key1,key2);
	} 

	
	
	public static Key translateKeyEvent(KeyEvent evt)
	{
		int modifiers = evt.getModifiers();
		Key returnValue = null;

		switch(evt.getID())
		{
		case KeyEvent.KEY_PRESSED:
			int keyCode = evt.getKeyCode();
			if((keyCode >= KeyEvent.VK_0
				&& keyCode <= KeyEvent.VK_9)
				|| (keyCode >= KeyEvent.VK_A
				&& keyCode <= KeyEvent.VK_Z))
			{
				if(Debug.ALTERNATIVE_DISPATCHER)
					return null;
				else
				{
					returnValue = new Key(
						modifiersToString(modifiers),
						'\0',Character.toLowerCase(
						(char)keyCode));
				}
			}
			else
			{
				if(keyCode == KeyEvent.VK_TAB)
				{
					evt.consume();
					returnValue = new Key(
						modifiersToString(modifiers),
						keyCode,'\0');
				}
				else if(keyCode == KeyEvent.VK_SPACE)
				{
					
					
					
					
					
					
					if((modifiers & ~InputEvent.SHIFT_MASK) == 0)
						returnValue = null;
					else
					{
						returnValue = new Key(
							modifiersToString(modifiers),
							0,' ');
					}
				}
				else
				{
					returnValue = new Key(
						modifiersToString(modifiers),
						keyCode,'\0');
				}
			}
			break;
		case KeyEvent.KEY_TYPED:
			char ch = evt.getKeyChar();

			switch(ch)
			{
			case '\n':
			case '\t':
			case '\b':
				return null;
			case ' ':
				if((modifiers & ~InputEvent.SHIFT_MASK) != 0)
					return null;
			}

			int ignoreMods;
			if(Debug.ALT_KEY_PRESSED_DISABLED)
			{
				
				ignoreMods = (InputEvent.SHIFT_MASK
					| InputEvent.ALT_GRAPH_MASK
					| InputEvent.ALT_MASK);
			}
			else
			{
				
				ignoreMods = (InputEvent.SHIFT_MASK
					| InputEvent.ALT_GRAPH_MASK);
			}

			boolean mod;
			if((modifiers & InputEvent.ALT_GRAPH_MASK) == 0
				&& evt.getWhen()
				-  KeyEventWorkaround.lastKeyTime < 750
				&& (KeyEventWorkaround.modifiers & ~ignoreMods)
				!= 0)
			{
				if(Debug.ALTERNATIVE_DISPATCHER)
				{
					returnValue = new Key(
						modifiersToString(modifiers),
						0,ch);
				}
				else
					return null;
			}
			else
			{
				if(ch == ' ')
				{
					returnValue = new Key(
						modifiersToString(modifiers),
						0,ch);
				}
				else
					returnValue = new Key(null,0,ch);
			}
			break;
		default:
			return null;
		}

		
		Key trans = (Key)transMap.get(returnValue);
		if(trans == null)
			return returnValue;
		else
			return trans;
	} 

	
	
	public static Key parseKey(String keyStroke)
	{
		if(keyStroke == null)
			return null;
		int index = keyStroke.indexOf('+');
		int modifiers = 0;
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
			return new Key(modifiersToString(modifiers),0,key.charAt(0));
		}
		else if(key.length() == 0)
		{
			Log.log(Log.ERROR,DefaultInputHandler.class,
				"Invalid key stroke: " + keyStroke);
			return null;
		}
		else if(key.equals("SPACE"))
		{
			return new Key(modifiersToString(modifiers),0,' ');
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

			return new Key(modifiersToString(modifiers),ch,'\0');
		}
	} 

	
	
	public static void setModifierMapping(int c, int a, int m, int s)
	{
		KeyEventTranslator.c = c;
		KeyEventTranslator.a = a;
		KeyEventTranslator.m = m;
		KeyEventTranslator.s = s;
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

	
	public static String modifiersToString(int mods)
	{
		StringBuffer buf = null;

		if((mods & InputEvent.CTRL_MASK) != 0)
		{
			if(buf == null)
				buf = new StringBuffer();
			buf.append(getSymbolicModifierName(InputEvent.CTRL_MASK));
		}
		if((mods & InputEvent.ALT_MASK) != 0)
		{
			if(buf == null)
				buf = new StringBuffer();
			buf.append(getSymbolicModifierName(InputEvent.ALT_MASK));
		}
		if((mods & InputEvent.META_MASK) != 0)
		{
			if(buf == null)
				buf = new StringBuffer();
			buf.append(getSymbolicModifierName(InputEvent.META_MASK));
		}
		if((mods & InputEvent.SHIFT_MASK) != 0)
		{
			if(buf == null)
				buf = new StringBuffer();
			buf.append(getSymbolicModifierName(InputEvent.SHIFT_MASK));
		}

		if(buf == null)
			return null;
		else
			return buf.toString();
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
		return (buf.length() == 0 ? null : buf.toString());
	} 

	static int c, a, m, s;

	
	private static Map transMap = new HashMap();

	static
	{
		if(OperatingSystem.isMacOS())
		{
			setModifierMapping(
				InputEvent.META_MASK,  
				InputEvent.CTRL_MASK,  
				
				InputEvent.ALT_MASK,   
				InputEvent.SHIFT_MASK  );
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

	
	public static class Key
	{
		public String modifiers;
		public int key;
		public char input;

		public Key(String modifiers, int key, char input)
		{
			this.modifiers = modifiers;
			this.key = key;
			this.input = input;
		}

		public int hashCode()
		{
			return key + input;
		}

		public boolean equals(Object o)
		{
			if(o instanceof Key)
			{
				Key k = (Key)o;
				if(MiscUtilities.objectsEqual(modifiers,
					k.modifiers) && key == k.key
					&& input == k.input)
				{
					return true;
				}
			}

			return false;
		}

		public String toString()
		{
			return (modifiers == null ? "" : modifiers)
				+ "<"
				+ Integer.toString(key,16)
				+ ","
				+ Integer.toString(input,16)
				+ ">";
		}
	} 
}
