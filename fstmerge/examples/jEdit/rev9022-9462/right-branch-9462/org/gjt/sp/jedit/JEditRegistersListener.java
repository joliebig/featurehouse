
package org.gjt.sp.jedit;

import org.gjt.sp.jedit.msg.RegisterChanged;


public class JEditRegistersListener implements RegistersListener
{
	public void registerChanged(char name)
	{
		EditBus.send(new RegisterChanged(null, name));
	}
}
