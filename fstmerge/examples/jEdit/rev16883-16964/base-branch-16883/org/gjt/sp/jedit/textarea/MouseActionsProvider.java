
package org.gjt.sp.jedit.textarea;

import java.awt.event.MouseEvent;


public interface MouseActionsProvider
{
	String getActionForEvent(MouseEvent evt, String variant);
}
