
package org.gjt.sp.jedit.datatransfer;

import org.gjt.sp.jedit.textarea.TextArea;

import java.awt.datatransfer.*;


public class StringTransferableService implements JEditTransferableService
{
	public boolean accept(TextArea textArea, String text)
	{
		return true;
	}

	public Transferable getTransferable(TextArea textArea, String text)
	{
		return new StringSelection(text);
	}
}
