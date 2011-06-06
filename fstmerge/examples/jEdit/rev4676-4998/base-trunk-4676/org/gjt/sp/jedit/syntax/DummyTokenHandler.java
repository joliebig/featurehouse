

package org.gjt.sp.jedit.syntax;


public class DummyTokenHandler implements TokenHandler
{
	
	public static final DummyTokenHandler INSTANCE = new DummyTokenHandler();

	
	
	public void handleToken(byte id, int offset, int length,
		TokenMarker.LineContext context) {}
}
