
package org.gjt.sp.jedit.input;

import org.gjt.sp.jedit.gui.KeyEventTranslator;
import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.Log;

import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;


public abstract class AbstractInputHandler
{
	protected int lastActionCount;
	protected KeyListener keyEventInterceptor;
	protected String readNextChar;
	protected int repeatCount;
	
	protected static final int REPEAT_COUNT_THRESHOLD = 20;

	public AbstractInputHandler()
	{
		repeatCount = 1;
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

	
	
	public abstract boolean handleKey(KeyEventTranslator.Key keyStroke,boolean dryRun);
	

	public abstract void processKeyEvent(KeyEvent evt, int from, boolean global);

	
	protected void processKeyEventKeyStrokeHandling(KeyEvent evt,int from,String mode,boolean global)
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
			if(handleKey(keyStroke,keyStroke.isPhantom())) {
				evt.consume();

				consumed = true;
			}
			if(Debug.DUMP_KEY_EVENTS)
			{
				Log.log(Log.DEBUG,this,"Translated (key "+mode+"): "+keyStroke+" from "+from+": consumed="+consumed+'.');
			}
		}
	} 
}
