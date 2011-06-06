

package org.gjt.sp.jedit.buffer;


public interface BufferUndoListener
{
	
	
	void beginUndo(JEditBuffer buffer);
	

	
	
	void endUndo(JEditBuffer buffer);
	

	
	
	void beginRedo(JEditBuffer buffer);
	

	
	
	void endRedo(JEditBuffer buffer);
	
}
