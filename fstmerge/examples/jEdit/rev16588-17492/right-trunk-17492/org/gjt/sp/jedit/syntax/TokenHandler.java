

package org.gjt.sp.jedit.syntax;

import javax.swing.text.Segment;


public interface TokenHandler
{
	
	public void handleToken(Segment seg, byte id, int offset, int length,
		TokenMarker.LineContext context);

	
	public void setLineContext(TokenMarker.LineContext lineContext);
}
