
package org.gjt.sp.jedit.textarea;


import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.EditPane;
import org.gjt.sp.jedit.syntax.ModeProvider;
import org.gjt.sp.jedit.buffer.JEditBuffer;



public class JEditEmbeddedTextArea extends TextArea
{
	
	
	public JEditEmbeddedTextArea()
	{
		super(jEdit.getPropertyManager(), null);
		initInputHandler();
		EditPane.initPainter(getPainter());
		JEditBuffer buffer = new JEditBuffer();
		buffer.setMode(ModeProvider.instance.getMode("text"));
		setBuffer(buffer);
	} 
}
