

package org.gjt.sp.jedit.gui;


import java.awt.event.*;
import java.util.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;



public class KeyEventTranslator
{
	
	
	public static void addTranslation(Key key1, Key key2)
	{
		transMap.put(key1,key2);
	} 

	
	
	protected static KeyEvent lastKeyPressEvent;
	
	protected static boolean lastKeyPressAccepted;

	
	public static Key translateKeyEvent(KeyEvent evt)
	{
		Key key = translateKeyEvent2(evt);
		
		if (key!=null) {
			if (key.isPhantom()) {
				key = null;
			}
		}
		
		return key;
	}
	
	
	public static Key translateKeyEvent2(KeyEvent evt)
	{
		if (Options.SIMPLIFIED_KEY_HANDLING)
		{	

			
			char	keyChar		= evt.getKeyChar();
			int	keyCode		= evt.getKeyCode();
			int	modifiers	= evt.getModifiers();
			boolean usecooked	= !evt.isActionKey();


			
			
			if ((modifiers&InputEvent.CTRL_MASK)!=0) {

				if (keyChar<0x20) {

					if (keyChar!=keyCode) { 

						keyChar+=0x40;
						
						if ((keyChar>='A')&&(keyChar<='Z')) {	
								keyChar+=0x20; 		
						}


					}
				}
				
				if (keyChar=='\\') { 

					keyChar		= 0;
					keyCode		= KeyEvent.VK_BACK_SLASH;
				}
				

			}
			
			
			
			
			switch (keyChar) {
				case 0x1b:	
				case 0x08:	
				case 0x7f:	
				case 0x09:	
				case 0x0a:	
				case KeyEvent.CHAR_UNDEFINED:
					usecooked	= false;
					keyChar		= 0;
			}

			boolean accept = false;
			boolean acceptAsPhantom = false;
			if (true) {
				switch(evt.getID()) {
					case KeyEvent.KEY_PRESSED:
						accept			= !usecooked;
						acceptAsPhantom		= !accept;
						lastKeyPressAccepted	= accept;
						lastKeyPressEvent	= evt;
					break;
					case KeyEvent.KEY_TYPED:
						if (lastKeyPressAccepted&&(lastKeyPressEvent!=null)&&(lastKeyPressEvent.getKeyChar()==evt.getKeyChar())) {
							
							
						} else {
							accept		= usecooked;
							acceptAsPhantom	= !accept;
						}
					break;
					default:
				}
			} else { 
				
				
				switch(evt.getID()) {
					case KeyEvent.KEY_PRESSED:
						accept	= true;
						if (usecooked) { 		
							keyCode = 0;
						}
					break;
					default:
				}
			}
				
			Key returnValue = null;
			
			if (accept||acceptAsPhantom) {
				if (!accept && acceptAsPhantom) {
					if (keyChar!=0) {
						keyCode = 0;
					}
				}
				
				returnValue = new Key(modifiersToString(modifiers),keyCode,keyChar);
				
				if (!accept && acceptAsPhantom) {
					if (keyChar!=0) {
					}
					returnValue.setIsPhantom(true);
				}
			}

			return returnValue;
		}
		else
		{
			int modifiers = evt.getModifiers();
			Key returnValue;
	
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
	
				if(KeyEventWorkaround.isMacControl(evt))
					ch |= 0x60;
	
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
					
					ignoreMods = InputEvent.SHIFT_MASK
						| InputEvent.ALT_GRAPH_MASK
						| InputEvent.ALT_MASK;
				}
				else
				{
					
					ignoreMods = InputEvent.SHIFT_MASK
						| InputEvent.ALT_GRAPH_MASK;
				}
	
				if((modifiers & InputEvent.ALT_GRAPH_MASK) == 0
					&& evt.getWhen()
					- KeyEventWorkaround.lastKeyTime < 750L
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
	
			
			Key trans = transMap.get(returnValue);
			if(trans == null)
				return returnValue;
			else
				return trans;
		}
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
			Log.log(Log.ERROR,KeyEventTranslator.class,
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
				Log.log(Log.ERROR,KeyEventTranslator.class,
					"Invalid key stroke: "
					+ keyStroke);
				return null;
			}

			return new Key(modifiersToString(modifiers),ch,'\0');
		}
	} 

	
	
	public static void setModifierMapping(int c, int a, int m, int s)
	{
	
		int duplicateMapping = 
			(c & a) | (c & m) | (c & s) | (a & m) | (a & s) | (m & s);
		
		if((duplicateMapping & InputEvent.CTRL_MASK) != 0) {
			throw new IllegalArgumentException(
				"CTRL is mapped to more than one modifier");
		}
		if((duplicateMapping & InputEvent.ALT_MASK) != 0) {
			throw new IllegalArgumentException(
				"ALT is mapped to more than one modifier");
		}
		if((duplicateMapping & InputEvent.META_MASK) != 0) {
			throw new IllegalArgumentException(
				"META is mapped to more than one modifier");
		}
		if((duplicateMapping & InputEvent.SHIFT_MASK) != 0) {
			throw new IllegalArgumentException(
				"SHIFT is mapped to more than one modifier");
		}
			
		KeyEventTranslator.c = c;
		KeyEventTranslator.a = a;
		KeyEventTranslator.m = m;
		KeyEventTranslator.s = s;
	} 

	
	
	public static char getSymbolicModifierName(int mod)
	{
		if((mod & c) != 0)
			return 'C';
		else if((mod & a) != 0)
			return 'A';
		else if((mod & m) != 0)
			return 'M';
		else if((mod & s) != 0)
			return 'S';
		else
			return '\0';
	} 

	
	private static final int[] MODS = {
		InputEvent.CTRL_MASK,
		InputEvent.ALT_MASK,
		InputEvent.META_MASK,
		InputEvent.SHIFT_MASK
	};

	public static String modifiersToString(int mods)
	{
		StringBuffer buf = null;

		for(int i = 0; i < MODS.length; i++)
		{
			if((mods & MODS[i]) != 0)
				buf = lazyAppend(buf,getSymbolicModifierName(MODS[i]));
		}

		if(buf == null)
			return null;
		else
			return buf.toString();
	} 

	
	
	public static String getModifierString(InputEvent evt)
	{
		StringBuilder buf = new StringBuilder();
		if(evt.isControlDown())
			buf.append(getSymbolicModifierName(InputEvent.CTRL_MASK));
		if(evt.isAltDown())
			buf.append(getSymbolicModifierName(InputEvent.ALT_MASK));
		if(evt.isMetaDown())
			buf.append(getSymbolicModifierName(InputEvent.META_MASK));
		if(evt.isShiftDown())
			buf.append(getSymbolicModifierName(InputEvent.SHIFT_MASK));
		return buf.length() == 0 ? null : buf.toString();
	} 

	static int c, a, m, s;

	
	
	private static final Map<Key, Key> transMap = new HashMap<Key, Key>();

	private static StringBuffer lazyAppend(StringBuffer buf, char ch)
	{
		if(buf == null)
			buf = new StringBuffer();
		if(buf.indexOf(String.valueOf(ch)) == -1)
			buf.append(ch);
		return buf;
	} 

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
		public final String modifiers;
		public final int key;
		public final char input;

		private final int hashCode;
		
		protected boolean isFromGlobalContext;
		
		
		protected boolean isPhantom;

		public Key(String modifiers, int key, char input)
		{
			this.modifiers = modifiers;
			this.key = key;
			this.input = input;
			hashCode = key + input;
		}

		public int hashCode()
		{
			return hashCode;
		}

		public boolean equals(Object o)
		{
			if(o instanceof Key)
			{
				Key k = (Key)o;
				if(StandardUtilities.objectsEqual(modifiers,
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
				+ '<'
				+ Integer.toString(key,16)
				+ ','
				+ Integer.toString(input,16)
				+ '>';
		}
		
		public void setIsFromGlobalContext(boolean to) {
			isFromGlobalContext = to;
		}
		
		public boolean isFromGlobalContext() {
			return isFromGlobalContext;
		}
		
		public void setIsPhantom(boolean to) {
			isPhantom = to;
		}
		
		public boolean isPhantom() {
			return isPhantom;
		}
	} 
}
