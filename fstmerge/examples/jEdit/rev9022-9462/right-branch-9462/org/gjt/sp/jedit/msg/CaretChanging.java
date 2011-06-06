

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.textarea.JEditTextArea;



public class CaretChanging extends TextAreaUpdate
{
	JEditTextArea jta;
	int caret;
	public CaretChanging(JEditTextArea jta) 
    {
		super(jta, TextAreaUpdate.CARET_CHANGING);
		this.jta = jta;
		caret = jta.getCaretPosition();
	}

}

