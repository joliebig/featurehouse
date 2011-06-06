

package org.gjt.sp.jedit.syntax;

import javax.swing.text.Segment;


public class DummyTokenHandler implements TokenHandler
{
	
	public static final DummyTokenHandler INSTANCE = new DummyTokenHandler();

	
	
	public void handleToken(Segment seg, byte id, int offset, int length,
		TokenMarker.LineContext context) {} 

	
	
	public void setLineContext(TokenMarker.LineContext lineContext)
	{
	} 
}
