

package org.gjt.sp.jedit.buffer;


public interface BufferListener
{
	
	
	void foldLevelChanged(JEditBuffer buffer, int startLine, int endLine);
	

	
	
	void contentInserted(JEditBuffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void contentRemoved(JEditBuffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void preContentInserted(JEditBuffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void preContentRemoved(JEditBuffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void transactionComplete(JEditBuffer buffer);
	

	
	
	void foldHandlerChanged(JEditBuffer buffer);
	

	
	
	void bufferLoaded(JEditBuffer buffer);
	
}
