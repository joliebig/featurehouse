

package net.sf.jabref;

import net.sf.jabref.export.LatexFieldFormatter;
import java.awt.datatransfer.*;
import java.io.*;
import javax.swing.JOptionPane;


public class TransferableBibtexEntry implements Transferable {

    private BibtexEntry[] data;
    public static DataFlavor entryFlavor = new DataFlavor(BibtexEntry.class, "JabRef entry");

    public TransferableBibtexEntry(BibtexEntry[] data) {
	this.data = data;
    }

    public DataFlavor[] getTransferDataFlavors() {
	return new DataFlavor[] {TransferableBibtexEntry.entryFlavor,
	                         DataFlavor.stringFlavor};
    }

    public boolean isDataFlavorSupported(DataFlavor flavor) {
	return (flavor.equals(entryFlavor) ||
		flavor.equals(DataFlavor.stringFlavor));
    }

    public Object getTransferData(DataFlavor flavor)
	throws UnsupportedFlavorException {
	if (flavor.equals(entryFlavor))
	    return data;
	else if (flavor.equals(DataFlavor.stringFlavor)) {
	    try {
		StringWriter sw = new StringWriter();
		LatexFieldFormatter ff = new LatexFieldFormatter();
		for (int i=0; i<data.length; i++)
		    data[i].write(sw, ff, false);
		return sw.toString();
	    } catch (IOException ex) {
		JOptionPane.showMessageDialog
		    (null, "Could not paste entry as text:\n"+ex.getMessage(),
		     "Clipboard", JOptionPane.ERROR_MESSAGE);
		return "";
	    }
	} else
	    throw new UnsupportedFlavorException(flavor);
    }
}

