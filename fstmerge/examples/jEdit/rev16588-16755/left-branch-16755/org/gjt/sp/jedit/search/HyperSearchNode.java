

package org.gjt.sp.jedit.search;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EditPane;
import org.gjt.sp.jedit.View;


public interface HyperSearchNode
{
	Buffer getBuffer(View view);

	void goTo(EditPane editPane);
}
