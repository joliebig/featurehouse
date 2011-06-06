

package org.gjt.sp.jedit;

import org.gjt.sp.util.Log;


public abstract class EBPlugin extends EditPlugin implements EBComponent
{
	
	
	public void handleMessage(EBMessage message)
	{
		EditBus.removeFromBus(this);
		if(seenWarning)
			return;
		seenWarning = true;
		Log.log(Log.WARNING,this,getClassName() + " should extend"
			+ " EditPlugin not EBPlugin since it has an empty"
			+ " handleMessage()");
	}

	
	protected EBPlugin() {}

	
	private boolean seenWarning;
}
