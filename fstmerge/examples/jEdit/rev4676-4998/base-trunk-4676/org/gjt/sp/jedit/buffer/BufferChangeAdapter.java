

package org.gjt.sp.jedit.buffer;

import org.gjt.sp.jedit.Buffer;


public abstract class BufferChangeAdapter implements BufferChangeListener
{
	
	
	public void foldLevelChanged(Buffer buffer, int start, int end)
	{
	} 

	
	
	public void contentInserted(Buffer buffer, int startLine, int offset,
		int numLines, int length) {}
	

	
	
	public void preContentRemoved(Buffer buffer, int startLine, int offset,
		int numLines, int length) {}
	

	
	
	public void contentRemoved(Buffer buffer, int startLine, int offset,
		int numLines, int length) {}
	

	
	
	public void transactionComplete(Buffer buffer) {}
	
}
