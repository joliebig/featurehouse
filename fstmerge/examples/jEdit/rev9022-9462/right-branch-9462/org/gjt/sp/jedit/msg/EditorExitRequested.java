

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.View;


public class EditorExitRequested extends EBMessage
{
	
	public EditorExitRequested(View view)
	{
		super(view);
	}

	
	public View getView()
	{
		return (View)getSource();
	}
}
