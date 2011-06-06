

package org.gjt.sp.jedit.syntax;


public interface TokenHandler
{
	
	public void handleToken(byte id, int offset, int length,
		TokenMarker.LineContext context);
}
