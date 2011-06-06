

package org.gjt.sp.jedit.textarea;


public interface StatusListener extends java.util.EventListener
{
	int OVERWRITE_CHANGED = 0;
	int MULTI_SELECT_CHANGED = 1;
	int RECT_SELECT_CHANGED = 2;

	void statusChanged(TextArea textArea, int flag, boolean value);
	
	void bracketSelected(TextArea textArea, int line, String text);
	
	void narrowActive(TextArea textArea);
}
