
package org.jhotdraw.util; 
import javax.swing.filechooser.FileFilter; 
import java.io.IOException; 
import org.jhotdraw.framework.Drawing; 
public  interface  StorageFormat {
		public FileFilter getFileFilter();

		public boolean isStoreFormat();

		public boolean isRestoreFormat();

		public String store(String fileName, Drawing saveDrawing) throws IOException;

		public Drawing restore(String fileName) throws IOException;


}
