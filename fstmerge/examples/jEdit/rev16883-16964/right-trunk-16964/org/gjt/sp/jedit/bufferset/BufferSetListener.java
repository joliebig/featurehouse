
package org.gjt.sp.jedit.bufferset;

import org.gjt.sp.jedit.Buffer;

import java.util.EventListener;


public interface BufferSetListener extends EventListener
{
	
	void bufferAdded(Buffer buffer, int index);

	
	void bufferRemoved(Buffer buffer, int index);

	
	void bufferMoved(Buffer buffer, int oldIndex, int newIndex);

	
	void bufferSetSorted();

}
