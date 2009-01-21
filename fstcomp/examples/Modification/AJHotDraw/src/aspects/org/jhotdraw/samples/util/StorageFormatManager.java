
package org.jhotdraw.util; 
import javax.swing.JFileChooser; 
import javax.swing.filechooser.FileFilter; 
import java.io.File; 
import java.util.List; 
import java.util.Iterator; 
public  class  StorageFormatManager {
		private List myStorageFormats;

		private StorageFormat myDefaultStorageFormat;

		public StorageFormatManager() {	myStorageFormats = CollectionsFactory.current().createList();	}

		public void addStorageFormat(StorageFormat newStorageFormat) {	myStorageFormats.add(newStorageFormat);	}

		public void removeStorageFormat(StorageFormat oldStorageFormat) {	myStorageFormats.remove(oldStorageFormat);	}

		public boolean containsStorageFormat(StorageFormat checkStorageFormat){	return myStorageFormats.contains(checkStorageFormat);	}

		public void setDefaultStorageFormat(StorageFormat newDefaultStorageFormat) {	myDefaultStorageFormat = newDefaultStorageFormat;	}

		public StorageFormat getDefaultStorageFormat() {	return myDefaultStorageFormat;	}

		public void registerFileFilters(JFileChooser fileChooser) {	if (fileChooser.getDialogType() == JFileChooser.OPEN_DIALOG) {	StorageFormat sf;	for (Iterator e = myStorageFormats.iterator(); e.hasNext();) {	sf = (StorageFormat) e.next();	if (sf.isRestoreFormat()) {	fileChooser.addChoosableFileFilter(sf.getFileFilter());	}	}	sf = getDefaultStorageFormat();	if (sf != null && sf.isRestoreFormat()) {	fileChooser.setFileFilter(sf.getFileFilter());	}	}	else if (fileChooser.getDialogType() == JFileChooser.SAVE_DIALOG) {	StorageFormat sf;	for (Iterator e = myStorageFormats.iterator(); e.hasNext();) {	sf = (StorageFormat) e.next();	if (sf.isStoreFormat()) {	fileChooser.addChoosableFileFilter(sf.getFileFilter());	}	}	sf = getDefaultStorageFormat();	if (sf != null && sf.isStoreFormat()) {	fileChooser.setFileFilter(sf.getFileFilter());	}	}	else {	StorageFormat sf;	for (Iterator e = myStorageFormats.iterator(); e.hasNext();) {	sf = (StorageFormat) e.next();	fileChooser.addChoosableFileFilter(sf.getFileFilter());	}	sf = getDefaultStorageFormat();	if (sf != null) {	fileChooser.setFileFilter(sf.getFileFilter());	}	}	}

		public StorageFormat findStorageFormat(FileFilter findFileFilter) {	Iterator formatsIterator = myStorageFormats.iterator();	StorageFormat currentStorageFormat = null;	while (formatsIterator.hasNext()) {	currentStorageFormat = (StorageFormat)formatsIterator.next();	if (currentStorageFormat.getFileFilter().equals(findFileFilter)) {	return currentStorageFormat;	}	}	return null;	}

		public StorageFormat findStorageFormat(File file) {	Iterator formatsIterator = myStorageFormats.iterator();	StorageFormat currentStorageFormat;	while (formatsIterator.hasNext()) {	currentStorageFormat = (StorageFormat) formatsIterator.next();	if (currentStorageFormat.getFileFilter().accept(file)) {	return currentStorageFormat;	}	}	return null;	}


}
