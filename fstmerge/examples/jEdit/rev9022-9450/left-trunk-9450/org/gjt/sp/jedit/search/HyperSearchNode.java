

package org.gjt.sp.jedit.search;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EditPane;

public interface HyperSearchNode
{
	public Buffer getBuffer();
	public void goTo(EditPane editPane);
}
