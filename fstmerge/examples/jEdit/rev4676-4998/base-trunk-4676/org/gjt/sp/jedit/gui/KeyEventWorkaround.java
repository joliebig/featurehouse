

package org.gjt.sp.jedit.gui;


import java.awt.event.*;
import org.gjt.sp.jedit.OperatingSystem;



public class KeyEventWorkaround
{
	
	public static KeyEvent processKeyEvent(KeyEvent evt)
	{
		int keyCode = evt.getKeyCode();
		char ch = evt.getKeyChar();

		switch(evt.getID())
		{
		
		case KeyEvent.KEY_PRESSED:
			
			switch(keyCode)
			{
			case KeyEvent.VK_ALT:
			case KeyEvent.VK_ALT_GRAPH:
			case KeyEvent.VK_CONTROL:
			case KeyEvent.VK_SHIFT:
			case KeyEvent.VK_META:
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
			default:
				switch(keyCode)
				{
					case KeyEvent.VK_NUMPAD0:   case KeyEvent.VK_NUMPAD1:
					case KeyEvent.VK_NUMPAD2:   case KeyEvent.VK_NUMPAD3:
					case KeyEvent.VK_NUMPAD4:   case KeyEvent.VK_NUMPAD5:
					case KeyEvent.VK_NUMPAD6:   case KeyEvent.VK_NUMPAD7:
					case KeyEvent.VK_NUMPAD8:   case KeyEvent.VK_NUMPAD9:
					case KeyEvent.VK_MULTIPLY:  case KeyEvent.VK_ADD:
					 case KeyEvent.VK_SUBTRACT:
					case KeyEvent.VK_DECIMAL:   case KeyEvent.VK_DIVIDE:
						last = LAST_NUMKEYPAD;
						lastKeyTime = System.currentTimeMillis();
						return evt;
				}

				if(!OperatingSystem.isMacOS())
					handleBrokenKeys(evt,keyCode);
				else
					last = LAST_NOTHING;
				break;
			}

			return evt;
		
		
		case KeyEvent.KEY_TYPED:
			
			
			if((ch < 0x20 || ch == 0x7f || ch == 0xff) && ch != '\b')
				return null;

			
			
			if(OperatingSystem.isMacOS())
			{
				if(evt.isControlDown() || evt.isMetaDown())
					return null;
			}
			else
			{
				if((evt.isControlDown() ^ evt.isAltDown())
					|| evt.isMetaDown())
					return null;
			}

			
			
			
			if(last == LAST_MOD)
			{
				switch(ch)
				{
				case 'B':
				case 'M':
				case 'X':
				case 'c':
				case '!':
				case ',':
				case '?':
					last = LAST_NOTHING;
					return null;
				}
			}

			
			
			if(last == LAST_NUMKEYPAD && System.currentTimeMillis()
				- lastKeyTime < 750)
			{
				last = LAST_NOTHING;
				if((ch >= '0' && ch <= '9') || ch == '.'
					|| ch == '/' || ch == '*'
					|| ch == '-' || ch == '+')
				{
					return null;
				}
			}
			
			
			else if(last == LAST_BROKEN && System.currentTimeMillis()
				- lastKeyTime < 750 && !Character.isLetter(ch))
			{
				last = LAST_NOTHING;
				return null;
			}
			
			else if(last == LAST_ALT && System.currentTimeMillis()
				- lastKeyTime < 750)
			{
				last = LAST_NOTHING;
				return null;
			}

			return evt;
		
		
		case KeyEvent.KEY_RELEASED:
			if(keyCode == KeyEvent.VK_ALT)
			{
				
				
				if(OperatingSystem.isWindows()
					&& OperatingSystem.hasJava14())
					last = LAST_MOD;
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

	

	
	private static long lastKeyTime;

	private static int last;
	private static final int LAST_NOTHING = 0;
	private static final int LAST_ALT = 1;
	private static final int LAST_BROKEN = 2;
	private static final int LAST_NUMKEYPAD = 3;
	private static final int LAST_MOD = 4;
	

	
	private static void handleBrokenKeys(KeyEvent evt, int keyCode)
	{
		if(evt.isAltDown() && evt.isControlDown()
			&& !evt.isMetaDown())
		{
			last = LAST_NOTHING;
			return;
		}
		else if(!(evt.isAltDown() || evt.isControlDown() || evt.isMetaDown()))
		{
			last = LAST_NOTHING;
			return;
		}

		if(evt.isAltDown())
			last = LAST_ALT;

		switch(keyCode)
		{
			case KeyEvent.VK_LEFT:      case KeyEvent.VK_RIGHT:
			case KeyEvent.VK_UP:        case KeyEvent.VK_DOWN:
			case KeyEvent.VK_DELETE:    case KeyEvent.VK_BACK_SPACE:
			case KeyEvent.VK_TAB:       case KeyEvent.VK_ENTER:
				last = LAST_NOTHING;
				break;
			default:
				if(keyCode < KeyEvent.VK_A || keyCode > KeyEvent.VK_Z)
					last = LAST_BROKEN;
				else
					last = LAST_NOTHING;
				break;
		}

		lastKeyTime = System.currentTimeMillis();
	} 

	
}
