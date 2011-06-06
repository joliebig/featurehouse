

package org.gjt.sp.jedit.buffer;


public interface FoldHandlerProvider
{
	
	FoldHandler getFoldHandler(String name);

	
	String[] getFoldModes();
}
