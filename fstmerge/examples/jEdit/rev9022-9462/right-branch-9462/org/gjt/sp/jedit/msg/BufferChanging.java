
 
package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EditPane;


public class BufferChanging extends EditPaneUpdate
{
	
	public BufferChanging(EditPane editPane, Buffer newBuffer) {
		super(editPane, BUFFER_CHANGING);
		m_buffer = newBuffer;
	}
	
	public Buffer getBuffer() {
		return m_buffer;
	}

	private Buffer m_buffer;
}
