

package org.gjt.sp.jedit.textarea;

import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.Log;


abstract class Anchor
{
	protected final DisplayManager displayManager;
	protected final TextArea textArea;
	
	int physicalLine;
	
	int scrollLine;
	
	boolean callChanged;
	
	boolean callReset;

	
	protected Anchor(DisplayManager displayManager,
		TextArea textArea)
	{
		this.displayManager = displayManager;
		this.textArea = textArea;
	} 

	
	abstract void reset();
	abstract void changed();

	
	@Override
	public String toString()
	{
		return getClass().getName() + '[' + physicalLine + ','
		       + scrollLine + ']';
	} 

	
	
	void contentInserted(int startLine, int numLines)
	{
		
		if(physicalLine >= startLine)
		{
			if(physicalLine != startLine)
				physicalLine += numLines;
			callChanged = true;
		}
	} 

	
	
	void preContentRemoved(int startLine, int offset, int numLines)
	{
		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"preContentRemoved() before:" + this);
		
		if(physicalLine >= startLine)
		{
			if(physicalLine == startLine)
				callChanged = true;
			else
			{
				int end = Math.min(startLine + numLines, physicalLine);
				
				

				

				
				
				

				for(int i = startLine + 1; i <= end; i++)
				{
					
					if(displayManager.isLineVisible(i))
					{
						scrollLine -=
							displayManager
								.screenLineMgr
								.getScreenLineCount(i);
					}
				}
				physicalLine -= end - startLine;
				callChanged = true;
			}
		}
		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"preContentRemoved() after:" + this);
	} 
}
