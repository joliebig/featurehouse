

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.View;


public class EditorExitRequested extends EBMessage
{
	private boolean hasBeenExitCancelled;
	
	
	public EditorExitRequested(View view)
	{
		super(view);
	}

	
	public View getView()
	{
		return (View)getSource();
	}
	
	
	public void cancelExit()
	{
		hasBeenExitCancelled = true;
	}
	
	 
	 public boolean hasBeenExitCancelled()
	 {
		 return hasBeenExitCancelled;
	 }
}
