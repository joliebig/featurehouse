
package org.gjt.sp.jedit.datatransfer;

import org.gjt.sp.jedit.textarea.TextArea;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class TransferHandler
{
	private static final TransferHandler instance = new TransferHandler();

	private final List<JEditTransferableService> services;

	private TransferHandler()
	{
		services = new ArrayList<JEditTransferableService>();
	}

	public static TransferHandler getInstance()
	{
		return instance;
	}

	public void registerTransferableService(JEditTransferableService transferableService)
	{
		services.add(transferableService);
	}

	public Transferable getTransferable(TextArea textArea, String text)
	{
		Map<DataFlavor, Transferable> flavors = new HashMap<DataFlavor, Transferable>();
		for (JEditTransferableService service : services)
		{
			if (service.accept(textArea, text))
			{
				Transferable t = service.getTransferable(textArea, text);
				DataFlavor[] supportedDataFlavor = t.getTransferDataFlavors();
				for (DataFlavor dataFlavor : supportedDataFlavor)
				{
					flavors.put(dataFlavor, t);
				}
			}
		}
		 Transferable transferable = new JEditTransferable(flavors);

		return transferable;
	}
}
