

package org.gjt.sp.jedit.textarea;


public interface ScrollListener extends java.util.EventListener
{
	void scrolledVertically(JEditTextArea textArea);
	void scrolledHorizontally(JEditTextArea textArea);
}
