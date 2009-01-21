
package org.jhotdraw.util; 
import java.io.*; 
import org.jhotdraw.framework.Drawing; 
public  class  SerializationStorageFormat  extends StandardStorageFormat {
		public SerializationStorageFormat() {	super();	}

		protected String createFileExtension() {	return "ser";	}

		public String createFileDescription() {	return "Serialization (" + getFileExtension() + ")";	}

		public String store(String fileName, Drawing saveDrawing) throws IOException {	FileOutputStream stream = new FileOutputStream(adjustFileName(fileName));	ObjectOutput output = new ObjectOutputStream(stream);	output.writeObject(saveDrawing);	output.close();	return adjustFileName(fileName);	}

		public Drawing restore(String fileName) throws IOException {	try {	FileInputStream stream = new FileInputStream(fileName);	ObjectInput input = new ObjectInputStream(stream);	return (Drawing)input.readObject();	}	catch (ClassNotFoundException exception) {	throw new IOException("Could not restore drawing '" + fileName +"': class not found!");	}	}


}
