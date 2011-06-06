

package org.gjt.sp.jedit.buffer;

import javax.swing.text.Segment;


public class DummyFoldHandler extends FoldHandler
{
	
	public DummyFoldHandler()
	{
		super("none");
	}
	

	
	
	public int getFoldLevel(JEditBuffer buffer, int lineIndex, Segment seg)
	{
		return 0;
	} 
}
