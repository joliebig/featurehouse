

package org.gjt.sp.jedit.textarea;

import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.Log;


class ScrollLineCount extends Anchor
{
	
	ScrollLineCount(DisplayManager displayManager,
		TextArea textArea)
	{
		super(displayManager,textArea);
	} 

	@Override
	public void changed() {}

	
	@Override
	public void reset()
	{
		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"reset()");

		physicalLine = displayManager.getFirstVisibleLine();
		int scrollLine = 0;
		while(physicalLine != -1)
		{
			scrollLine += displayManager
				.getScreenLineCount(physicalLine);
			physicalLine = displayManager
				.getNextVisibleLine(physicalLine);
		}

		this.scrollLine = scrollLine;
		physicalLine = displayManager.getBuffer().getLineCount();
	} 
}
