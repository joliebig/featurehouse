
package org.gjt.sp.jedit.datatransfer;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;


public class RichTextTransferable implements Transferable
{
	public static final DataFlavor jEditRichTextDataFlavor = new DataFlavor(JEditRichText.class, DataFlavor.javaJVMLocalObjectMimeType);
	private static final DataFlavor[] supportedDataFlavor = {jEditRichTextDataFlavor};

	private final String text;
	private final String mode;

	public RichTextTransferable(String text, String mode)
	{
		this.text = text;
		this.mode = mode;
	}

	public DataFlavor[] getTransferDataFlavors()
	{
		return supportedDataFlavor;
	}

	public boolean isDataFlavorSupported(DataFlavor flavor)
	{
		return jEditRichTextDataFlavor.equals(flavor);
	}

	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException
	{
		if (!isDataFlavorSupported(flavor))
			throw new UnsupportedFlavorException(flavor);
		return new JEditRichText(text, mode);
	}
}
