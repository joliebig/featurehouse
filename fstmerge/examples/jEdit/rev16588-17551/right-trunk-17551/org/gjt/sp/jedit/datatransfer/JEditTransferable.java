
package org.gjt.sp.jedit.datatransfer;

import java.awt.datatransfer.*;
import java.io.IOException;
import java.util.Map;


public class JEditTransferable implements Transferable
{
	private final DataFlavor[] dataFlavors;

	private final Map<DataFlavor, Transferable> flavors;

	public JEditTransferable(Map<DataFlavor, Transferable> flavors)
	{
		this.flavors = flavors;
		dataFlavors = flavors.keySet().toArray(new DataFlavor[flavors.size()]);
	}

	public DataFlavor[] getTransferDataFlavors()
	{
		return dataFlavors;
	}

	public boolean isDataFlavorSupported(DataFlavor flavor)
	{
		return flavors.containsKey(flavor);
	}

	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException
	{
		if (!isDataFlavorSupported(flavor))
			throw new UnsupportedFlavorException(flavor);
		Transferable transferable = flavors.get(flavor);
		return transferable.getTransferData(flavor);
	}
}
