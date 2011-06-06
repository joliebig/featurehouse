
 
package org.gjt.sp.jedit.msg;

import java.util.Arrays;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EditPane;
import org.gjt.sp.util.Log;


public class BufferChanging extends PositionChanging
{
	
	public BufferChanging(EditPane editPane, Buffer newBuffer)
	{
		super(editPane, EditPaneUpdate.BUFFER_CHANGING);
		if (newBuffer == null)
		{
			String s = Arrays.toString(Thread.currentThread().getStackTrace());
			Log.log (Log.ERROR, this, "BufferChanging to null Buffer? Emit PositionChanging instead." + s);
		}
		m_buffer = newBuffer;
	}
	
	
	public Buffer getBuffer()
	{
		return m_buffer;
	}

	private Buffer m_buffer;
}
