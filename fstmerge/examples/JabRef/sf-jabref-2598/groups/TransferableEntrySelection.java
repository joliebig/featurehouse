

package net.sf.jabref.groups;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.ByteArrayInputStream;
import java.io.IOException;

import net.sf.jabref.BibtexEntry;

public class TransferableEntrySelection implements Transferable {
    public static final DataFlavor flavorInternal;
    public static final DataFlavor flavorExternal;
    public static final DataFlavor[] flavors;
    public final BibtexEntry[] selectedEntries;
    public final String selectedEntriesCiteKeys;
    
    protected boolean includeCiteKeyword = false;

    static {
        DataFlavor df1 = null;
        DataFlavor df2 = null;
        try {
            df1 = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType
                    + ";class=net.sf.jabref.groups.TransferableEntrySelection");
            df2 = DataFlavor.getTextPlainUnicodeFlavor();
        } catch (ClassNotFoundException e) {
            
        }
        flavorInternal = df1;
        flavorExternal = df2;
        flavors = new DataFlavor[] { flavorInternal, flavorExternal };
    }

    public TransferableEntrySelection(BibtexEntry[] selectedEntries) {
        this.selectedEntries = selectedEntries;
        StringBuffer keys = new StringBuffer();
        for (int i = 0; i < selectedEntries.length; ++i) {
            keys.append(selectedEntries[i].getCiteKey());
            if (i + 1 < selectedEntries.length)
                keys.append(",");
        }
        selectedEntriesCiteKeys = keys.toString();
    }

    public DataFlavor[] getTransferDataFlavors() {
        return flavors;
    }

    public boolean isDataFlavorSupported(DataFlavor someFlavor) {
        return someFlavor.equals(flavorInternal)
                || someFlavor.equals(flavorExternal);
    }

    public Object getTransferData(DataFlavor someFlavor)
            throws UnsupportedFlavorException, IOException {
        if (!isDataFlavorSupported(someFlavor))
            throw new UnsupportedFlavorException(someFlavor);
        if (someFlavor.equals(flavorInternal))
            return this;
        String s = includeCiteKeyword ?
                "\\cite{" + selectedEntriesCiteKeys + "}" 
                : selectedEntriesCiteKeys;
        return new ByteArrayInputStream(s.getBytes(
                flavorExternal.getParameter("charset").trim()));
    }

    public BibtexEntry[] getSelection() {
        return selectedEntries;
    }

    public void setIncludeCiteKeyword(boolean includeCiteKeyword) {
        this.includeCiteKeyword = includeCiteKeyword;
    }
    

}
