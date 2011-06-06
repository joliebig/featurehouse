

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;


public class PropertiesChanged extends EBMessage
{
	
	public PropertiesChanged(EBComponent source)
	{
		super(source);
	}

	
	public PropertiesChanged(Object source)
	{
		super(source);
	}
}
