
package org.gjt.sp.jedit.datatransfer;

import org.gjt.sp.jedit.textarea.TextArea;

import java.awt.datatransfer.Transferable;


public class RichJEditTextTransferableService implements JEditTransferableService
{
	public boolean accept(TextArea textArea, String text)
	{
		return textArea != null;
	}

	public Transferable getTransferable(TextArea textArea, String text)
	{
		return new RichTextTransferable(text, textArea.getBuffer().getMode().getName());
	}
}
