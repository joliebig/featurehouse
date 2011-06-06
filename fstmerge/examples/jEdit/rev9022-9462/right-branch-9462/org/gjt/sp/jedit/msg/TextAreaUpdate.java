package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.EditPane;
import org.gjt.sp.jedit.textarea.JEditTextArea;


public class TextAreaUpdate extends EBMessage
{
	
	public static final String CARET_CHANGING ="CARET_CHANGING";
	String what;
	int caret = 0;
	public TextAreaUpdate(JEditTextArea textArea, String what) 
	{	
		super(textArea);
		this.what = what;
		caret = textArea.getCaretPosition();
	}
	public String getWhat() {
		return what;
	}
	public JEditTextArea getTextArea() {
		return (JEditTextArea) getSource();
	}
}
