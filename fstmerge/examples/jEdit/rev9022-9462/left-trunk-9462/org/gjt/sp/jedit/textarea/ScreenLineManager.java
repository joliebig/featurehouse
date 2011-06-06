

package org.gjt.sp.jedit.textarea;


import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.Log;



class ScreenLineManager
{
	
	ScreenLineManager(JEditBuffer buffer)
	{
		this.buffer = buffer;
		if(!buffer.isLoading())
			reset();
	} 
	
	
	boolean isScreenLineCountValid(int line)
	{
		return (screenLines[line] & SCREEN_LINES_VALID_MASK) != 0;
	} 

	
	
	int getScreenLineCount(int line)
	{
		return screenLines[line] >> SCREEN_LINES_SHIFT;
	} 

	
	void setScreenLineCount(int line, int count)
	{
		if(count > Short.MAX_VALUE)
		{
			
			count = Short.MAX_VALUE;
		}

		if(Debug.SCREEN_LINES_DEBUG)
			Log.log(Log.DEBUG,this,new Exception("setScreenLineCount(" + line + ',' + count + ')'));
		screenLines[line] = (short)(count << SCREEN_LINES_SHIFT
			| SCREEN_LINES_VALID_MASK);
	} 

	
	void invalidateScreenLineCounts()
	{
		int lineCount = buffer.getLineCount();
		for(int i = 0; i < lineCount; i++)
			screenLines[i] &= ~SCREEN_LINES_VALID_MASK;
	} 

	
	void reset()
	{
		screenLines = new short[buffer.getLineCount()];
		for(int i = 0; i < screenLines.length; i++)
			screenLines[i] = 0;
	} 

	
	public void contentInserted(int startLine, int numLines)
	{
		int endLine = startLine + numLines;
		screenLines[startLine] &= ~SCREEN_LINES_VALID_MASK;

		int lineCount = buffer.getLineCount();

		if(numLines > 0)
		{
			if(screenLines.length <= lineCount)
			{
				short[] screenLinesN = new short[((lineCount + 1) << 1)];
				System.arraycopy(screenLines,0,screenLinesN,0,
						 screenLines.length);
				screenLines = screenLinesN;
			}

			System.arraycopy(screenLines,startLine,screenLines,
				endLine,lineCount - endLine);

			for(int i = 0; i < numLines; i++)
				screenLines[startLine + i] = 0;
		}
	} 

	
	public void contentRemoved(int startLine, int numLines)
	{
		int endLine = startLine + numLines;
		screenLines[startLine] &= ~SCREEN_LINES_VALID_MASK;

		if(numLines > 0 && endLine != screenLines.length)
		{
			System.arraycopy(screenLines,endLine + 1,screenLines,
				startLine + 1,screenLines.length - endLine - 1);
		}
	} 

	
	private static final int SCREEN_LINES_SHIFT = 1;
	private static final int SCREEN_LINES_VALID_MASK = 1;

	private JEditBuffer buffer;
	private short[] screenLines;
	
}
