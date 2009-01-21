
package org.jhotdraw.util; 
import javax.swing.filechooser.FileFilter; 
import java.io.IOException; 
import java.io.FileInputStream; 
import java.io.FileOutputStream; 
import java.io.File; 
import org.jhotdraw.framework.Drawing; 
public  class  StandardStorageFormat  implements StorageFormat {
		private FileFilter myFileFilter;

		private String myFileExtension;

		private String myFileDescription;

		public StandardStorageFormat() {	setFileExtension(createFileExtension());	setFileDescription(createFileDescription());	setFileFilter(createFileFilter());	}

		protected String createFileExtension() {	return myFileExtension = "draw";	}

		public void setFileExtension(String newFileExtension) {	myFileExtension = newFileExtension;	}

		public String getFileExtension() {	return myFileExtension;	}

		public String createFileDescription() {	return "Internal Format (" + getFileExtension() + ")";	}

		public void setFileDescription(String newFileDescription) {	myFileDescription = newFileDescription;	}

		public String getFileDescription() {	return myFileDescription;	}

		protected FileFilter createFileFilter() {	return new FileFilter() {	public boolean accept(File checkFile) {	if (checkFile.isDirectory()) {	return true;	}	else {	return checkFile.getName().endsWith("." + getFileExtension());	}	}	public String getDescription() {	return getFileDescription();	}	};	}

		public void setFileFilter(FileFilter newFileFilter) {	myFileFilter = newFileFilter;	}

		public FileFilter getFileFilter() {	return myFileFilter;	}

		public boolean isRestoreFormat() {	return true;	}

		public boolean isStoreFormat() {	return true;	}

		public String store(String fileName, Drawing saveDrawing) throws IOException {	FileOutputStream stream = new FileOutputStream(adjustFileName(fileName));	StorableOutput output = new StorableOutput(stream);	output.writeStorable(saveDrawing);	output.close();	return adjustFileName(fileName);	}

		public Drawing restore(String fileName) throws IOException {	if (!hasCorrectFileExtension(fileName)) {	return null;	}	else {	FileInputStream stream = new FileInputStream(fileName);	StorableInput input = new StorableInput(stream);	return (Drawing)input.readStorable();	}	}

		public boolean equals(Object compareObject) {	if (compareObject instanceof StandardStorageFormat) {	return getFileExtension().equals(((StandardStorageFormat)compareObject).getFileExtension());	}	else {	return false;	}	}

		protected String adjustFileName(String testFileName) {	if (!hasCorrectFileExtension(testFileName)) {	return testFileName + "." + getFileExtension();	}	else {	return testFileName;	}	}

		protected boolean hasCorrectFileExtension(String testFileName) {	return testFileName.endsWith("." + getFileExtension());	}


}
