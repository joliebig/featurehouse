

package org.gjt.sp.jedit.buffer;

import javax.swing.text.Segment;
import org.gjt.sp.jedit.Buffer;


public class DummyFoldHandler extends FoldHandler
{
	
	public DummyFoldHandler()
	{
		super("none");
	}
	

	
	
	public int getFoldLevel(Buffer buffer, int lineIndex, Segment seg)
	{
		return 0;
	} 
}
