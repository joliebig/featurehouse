

package org.gjt.sp.jedit.buffer;


public abstract class BufferAdapter implements BufferListener
{
	
	
	public void foldLevelChanged(JEditBuffer buffer, int start, int end)
	{
	} 

	
	
	public void contentInserted(JEditBuffer buffer, int startLine, int offset,
		int numLines, int length) {}
	

	
	public void preContentInserted(JEditBuffer buffer, int startLine, int offset, int numLines, int length)
	{
	}

	
	
	public void preContentRemoved(JEditBuffer buffer, int startLine, int offset,
		int numLines, int length) {}
	

	
	
	public void contentRemoved(JEditBuffer buffer, int startLine, int offset,
		int numLines, int length) {}
	

	
	
	public void transactionComplete(JEditBuffer buffer) {}
	

	
	
	public void foldHandlerChanged(JEditBuffer buffer) {}
	

	
	
	public void bufferLoaded(JEditBuffer buffer) {}
	
}
