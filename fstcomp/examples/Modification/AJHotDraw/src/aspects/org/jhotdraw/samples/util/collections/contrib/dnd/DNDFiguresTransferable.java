
package org.jhotdraw.contrib.dnd; 
import java.awt.datatransfer.*; 
import java.io.*; 
public  class  DNDFiguresTransferable  implements Transferable , Serializable {
		public static DataFlavor DNDFiguresFlavor = new DataFlavor(DNDFigures.class,"DNDFigures");

		private Object o;

		public DNDFiguresTransferable(Object newObject) {	o = newObject;	}

		public DataFlavor[] getTransferDataFlavors() {	return new DataFlavor [] {DNDFiguresFlavor };	}

		public boolean isDataFlavorSupported(DataFlavor flavor) {	return flavor.equals(DNDFiguresFlavor);	}

		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {	if ( isDataFlavorSupported(flavor) == false) {	throw new UnsupportedFlavorException( flavor );	}	return o;	}


}
