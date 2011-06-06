

package org.gjt.sp.jedit.visitors;


import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.textarea.JEditTextArea;



public interface JEditVisitor
{
	
	void visit(View view);
		   
	
	void visit(EditPane editPane);

	
	void visit(JEditTextArea textArea);
}
