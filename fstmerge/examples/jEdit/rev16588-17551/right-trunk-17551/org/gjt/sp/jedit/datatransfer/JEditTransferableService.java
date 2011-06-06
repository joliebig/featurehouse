
package org.gjt.sp.jedit.datatransfer;

import org.gjt.sp.jedit.textarea.TextArea;

import java.awt.datatransfer.Transferable;


public interface JEditTransferableService
{
	boolean accept(TextArea textArea, String text);
	Transferable getTransferable(TextArea textArea, String text);
}
