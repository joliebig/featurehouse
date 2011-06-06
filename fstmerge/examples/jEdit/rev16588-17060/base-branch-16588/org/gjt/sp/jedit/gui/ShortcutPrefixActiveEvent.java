
package org.gjt.sp.jedit.gui;


import java.util.Hashtable;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

import org.gjt.sp.util.Log;



public class ShortcutPrefixActiveEvent extends ChangeEvent
{

	
	protected Hashtable bindings;
	
	protected boolean active;

	
	protected static EventListenerList listenerList = new EventListenerList();

	
	
	public ShortcutPrefixActiveEvent(Hashtable bindings, boolean active)
	{
		super(new Object());
		this.bindings = bindings;
		this.active = active;
	} 

	
	
	public static void addChangeEventListener(ChangeListener l)
	{
		listenerList.add(ChangeListener.class, l);
		Log.log(Log.DEBUG, ShortcutPrefixActiveEvent.class, "Listener added.  " + listenerList.getListenerList().length + " left.");
	}

	
	
	public static void removeChangeEventListener(ChangeListener l)
	{
		listenerList.remove(ChangeListener.class, l);
		Log.log(Log.DEBUG, ShortcutPrefixActiveEvent.class, "Listener removed.  " + listenerList.getListenerList().length + " left.");
	}

	
	
	public static void firePrefixStateChange(Hashtable bindings, boolean listeningForShortcutCompletion)
	{
		
		
		Object[] listeners = listenerList.getListenerList();
		
		
		
		for (int i = listeners.length - 2; i >= 0; i -= 2)
		{
			
			ChangeEvent event = new ShortcutPrefixActiveEvent(bindings, listeningForShortcutCompletion);
			((ChangeListener) listeners[i + 1]).stateChanged(event);
		}
	}


	
	
	public Hashtable getBindings()
	{
		return bindings;
	}

	
	
	public boolean getActive()
	{
		return active;
	}
	
}

