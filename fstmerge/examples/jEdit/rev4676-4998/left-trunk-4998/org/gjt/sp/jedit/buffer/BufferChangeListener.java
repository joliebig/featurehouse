

package org.gjt.sp.jedit.buffer;

import org.gjt.sp.jedit.Buffer;


public interface BufferChangeListener
{
	
	
	void foldLevelChanged(Buffer buffer, int startLine, int endLine);
	

	
	
	void contentInserted(Buffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void contentRemoved(Buffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	public void preContentRemoved(Buffer buffer, int startLine, int offset,
		int numLines, int length);
	

	
	
	void transactionComplete(Buffer buffer);
	

	
	
	void foldHandlerChanged(Buffer buffer);
	
}
