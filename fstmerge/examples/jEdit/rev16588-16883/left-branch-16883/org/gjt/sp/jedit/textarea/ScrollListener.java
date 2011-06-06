

package org.gjt.sp.jedit.textarea;


public interface ScrollListener extends java.util.EventListener
{
	void scrolledVertically(TextArea textArea);
	void scrolledHorizontally(TextArea textArea);
}
