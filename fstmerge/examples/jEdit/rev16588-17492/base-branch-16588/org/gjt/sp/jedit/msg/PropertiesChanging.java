

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;


public class PropertiesChanging extends EBMessage
{

	public enum State {
		LOADING,
		CANCELED
	}

	
	public PropertiesChanging(EBComponent source, State state)
	{
		super(source);
		assert (state != null) : "state shouldn't be null";
		this.state = state;
	}

	public State getState()
	{
		return state;
	}

	private final State state;

}
