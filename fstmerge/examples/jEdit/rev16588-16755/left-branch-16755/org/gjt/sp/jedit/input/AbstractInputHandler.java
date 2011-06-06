
package org.gjt.sp.jedit.input;

import org.gjt.sp.jedit.gui.KeyEventTranslator;
import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.Log;

import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;
import java.util.Hashtable;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.JEditAbstractEditAction;
import org.gjt.sp.jedit.gui.ShortcutPrefixActiveEvent;


public abstract class AbstractInputHandler<E extends JEditAbstractEditAction>
{
	protected int lastActionCount;
	
	protected KeyListener keyEventInterceptor;
	protected String readNextChar;
	protected int repeatCount;
	protected E lastAction;


	protected static final int REPEAT_COUNT_THRESHOLD = 20;

	
	public AbstractInputHandler()
	{
		repeatCount = 1;
	} 
	
		
	
	public void addKeyBinding(String keyBinding, String action)
	{
		addKeyBinding(keyBinding,(Object)action);
	} 

	
	
	public void addKeyBinding(String keyBinding, E action)
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

	
	
	public int getLastActionCount()
	{
		return lastActionCount;
	} 

	
	
	public void resetLastActionCount()
	{
		lastActionCount = 0;
	} 

	
	public KeyListener getKeyEventInterceptor()
	{
		return keyEventInterceptor;
	} 

	
	
	public void setKeyEventInterceptor(KeyListener keyEventInterceptor)
	{
		this.keyEventInterceptor = keyEventInterceptor;
	} 

	
	
	public boolean isPrefixActive()
	{
		return readNextChar != null;
	} 
	
	
	
	public void setBindings(Hashtable bindings)
	{
		this.bindings = this.currentBindings = bindings;
	} 
	
	
	public void setCurrentBindings(Hashtable bindings)
	{
		currentBindings = bindings;
	} 

	
	
	public abstract boolean handleKey(KeyEventTranslator.Key keyStroke,boolean dryRun);
	

	

	
	public abstract void processKeyEvent(KeyEvent evt, int from, boolean global); 
	

	

	
	
	protected void sendShortcutPrefixOff()
	{
		if(shortcutOn)
		{
			ShortcutPrefixActiveEvent.firePrefixStateChange(null, false);
			shortcutOn = false;
		}
	} 
	
	public abstract void invokeAction(String action);
	
	public abstract void invokeAction(E action);

	
	
	public static String toString(KeyEvent evt)
	{
		String id;
		switch(evt.getID())
		{
		case KeyEvent.KEY_PRESSED:
			id = "KEY_PRESSED";
			break;
		case KeyEvent.KEY_RELEASED:
			id = "KEY_RELEASED";
			break;
		case KeyEvent.KEY_TYPED:
			id = "KEY_TYPED";
			break;
		default:
			id = "unknown type";
			break;
		}

		StringBuilder b = new StringBuilder(50);

		b.append(id);
		b.append(",keyCode=0x").append(Integer.toString(evt.getKeyCode(), 16));
		b.append(",keyChar=0x").append(Integer.toString(evt.getKeyChar(), 16));
		b.append(",modifiers=0x").append(Integer.toString(evt.getModifiers(), 16));

		b.append(",consumed=");
		b.append(evt.isConsumed()?'1':'0');

		return b.toString();
	} 

	
	protected void processKeyEventKeyStrokeHandling(KeyEvent evt, int from, String mode, boolean global)
	{
		KeyEventTranslator.Key keyStroke = KeyEventTranslator.translateKeyEvent2(evt);

		if(keyStroke != null)
		{
			keyStroke.setIsFromGlobalContext(global);
			if(Debug.DUMP_KEY_EVENTS)
			{
				Log.log(Log.DEBUG,this,"Translated (key "+mode+"): "+keyStroke+" from "+from);
			}
			boolean consumed = false;
			if(handleKey(keyStroke,keyStroke.isPhantom()))
			{
				evt.consume();

				consumed = true;
			}
			if(Debug.DUMP_KEY_EVENTS)
			{
				Log.log(Log.DEBUG,this,"Translated (key "+mode+"): "+keyStroke+" from "+from+": consumed="+consumed+'.');
			}
		}
	} 
	
	

	
	public static Object PREFIX_STR = "PREFIX_STR";
	protected boolean shortcutOn = false;
	

	protected Hashtable bindings;
	protected Hashtable currentBindings;
	
}
