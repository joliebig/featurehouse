

package org.gjt.sp.jedit.gui;


import java.awt.event.*;
import org.gjt.sp.jedit.Debug;



public class KeyEventWorkaround
{
	
	public static KeyEvent processKeyEvent(KeyEvent evt)
	{
		int keyCode = evt.getKeyCode();
		char ch = evt.getKeyChar();

		switch(evt.getID())
		{
		
		case KeyEvent.KEY_PRESSED:
			lastKeyTime = evt.getWhen();
			
			switch(keyCode)
			{
			case KeyEvent.VK_DEAD_GRAVE:
			case KeyEvent.VK_DEAD_ACUTE:
			case KeyEvent.VK_DEAD_CIRCUMFLEX:
			case KeyEvent.VK_DEAD_TILDE:
			case KeyEvent.VK_DEAD_MACRON:
			case KeyEvent.VK_DEAD_BREVE:
			case KeyEvent.VK_DEAD_ABOVEDOT:
			case KeyEvent.VK_DEAD_DIAERESIS:
			case KeyEvent.VK_DEAD_ABOVERING:
			case KeyEvent.VK_DEAD_DOUBLEACUTE:
			case KeyEvent.VK_DEAD_CARON:
			case KeyEvent.VK_DEAD_CEDILLA:
			case KeyEvent.VK_DEAD_OGONEK:
			case KeyEvent.VK_DEAD_IOTA:
			case KeyEvent.VK_DEAD_VOICED_SOUND:
			case KeyEvent.VK_DEAD_SEMIVOICED_SOUND:
			case '\0':
				return null;
			case KeyEvent.VK_ALT:
				modifiers |= InputEvent.ALT_MASK;
				return null;
			case KeyEvent.VK_ALT_GRAPH:
				modifiers |= InputEvent.ALT_GRAPH_MASK;
				return null;
			case KeyEvent.VK_CONTROL:
				modifiers |= InputEvent.CTRL_MASK;
				return null;
			case KeyEvent.VK_SHIFT:
				modifiers |= InputEvent.SHIFT_MASK;
				return null;
			case KeyEvent.VK_META:
				modifiers |= InputEvent.META_MASK;
				return null;
			default:
				if(!evt.isMetaDown())
				{
					if(evt.isControlDown()
						&& evt.isAltDown())
					{
						lastKeyTime = 0L;
					}
					else if(!evt.isControlDown()
						&& !evt.isAltDown())
					{
						lastKeyTime = 0L;

						if(keyCode >= KeyEvent.VK_0
							&& keyCode <= KeyEvent.VK_9)
						{
							return null;
						}

						if(keyCode >= KeyEvent.VK_A
							&& keyCode <= KeyEvent.VK_Z)
						{
							return null;
						}
					}
				}

				if(Debug.ALT_KEY_PRESSED_DISABLED)
				{
					
					
					if((modifiers & InputEvent.ALT_MASK) != 0)
						return null;
				}

				switch(keyCode)
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
					last = LAST_NUMKEYPAD;
					break;
				default:
					last = LAST_NOTHING;
					break;
				}

				return evt;
			}
		
		
		case KeyEvent.KEY_TYPED:
			
			
			if((ch < 0x20 || ch == 0x7f || ch == 0xff)
				&& ch != '\b' && ch != '\t' && ch != '\n')
			{
				return null;
			}

			if(evt.getWhen() - lastKeyTime < 750)
			{
				if(!Debug.ALTERNATIVE_DISPATCHER)
				{
					if(((modifiers & InputEvent.CTRL_MASK) != 0
						^ (modifiers & InputEvent.ALT_MASK) != 0)
						|| (modifiers & InputEvent.META_MASK) != 0)
					{
						return null;
					}
				}

				
				
				if(last == LAST_NUMKEYPAD)
				{
					last = LAST_NOTHING;
					if((ch >= '0' && ch <= '9') || ch == '.'
						|| ch == '/' || ch == '*'
						|| ch == '-' || ch == '+')
					{
						return null;
					}
				}
				
				else if(last == LAST_ALT)
				{
					last = LAST_NOTHING;
					switch(ch)
					{
					case 'B':
					case 'M':
					case 'X':
					case 'c':
					case '!':
					case ',':
					case '?':
						return null;
					}
				}
			}
			else
			{
				if((modifiers & InputEvent.SHIFT_MASK) != 0)
				{
					switch(ch)
					{
					case '\n':
					case '\t':
						return null;
					}
				}
				modifiers = 0;
			}

			return evt;
		
		
		case KeyEvent.KEY_RELEASED:
			switch(keyCode)
			{
			case KeyEvent.VK_ALT:
				modifiers &= ~InputEvent.ALT_MASK;
				lastKeyTime = evt.getWhen();
				
				
				
				evt.consume();
				return null;
			case KeyEvent.VK_ALT_GRAPH:
				modifiers &= ~InputEvent.ALT_GRAPH_MASK;
				return null;
			case KeyEvent.VK_CONTROL:
				modifiers &= ~InputEvent.CTRL_MASK;
				return null;
			case KeyEvent.VK_SHIFT:
				modifiers &= ~InputEvent.SHIFT_MASK;
				return null;
			case KeyEvent.VK_META:
				modifiers &= ~InputEvent.META_MASK;
				return null;
			case KeyEvent.VK_LEFT:
			case KeyEvent.VK_RIGHT:
				
				if(modifiers == InputEvent.ALT_MASK)
					last = LAST_ALT;
				break;
			}
			return evt;
		
		default:
			return evt;
		}
	} 

	
	
	public static void numericKeypadKey()
	{
		last = LAST_NOTHING;
	} 

	
	static long lastKeyTime;
	static int modifiers;
	

	
	private static int last;
	private static final int LAST_NOTHING = 0;
	private static final int LAST_NUMKEYPAD = 1;
	private static final int LAST_ALT = 2;
	
}
