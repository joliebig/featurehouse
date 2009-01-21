
package org.jhotdraw.contrib; 
import java.io.FileOutputStream; 
import java.io.IOException; 
import java.io.OutputStreamWriter; 
import java.io.Writer; 
import org.apache.batik.dom.GenericDOMImplementation; 
import org.apache.batik.svggen.SVGGraphics2D; 
import org.w3c.dom.DOMImplementation; 
import org.w3c.dom.Document; 
import org.jhotdraw.framework.Drawing; 
import org.jhotdraw.util.StandardStorageFormat; 
public  class  SVGStorageFormat  extends StandardStorageFormat {
		protected String createFileExtension() {	return "svg";	}

		public String createFileDescription() {	return "Scalable Vector Graphics (svg)";	}

		public boolean isRestoreFormat() {	return false;	}

		public boolean isStoreFormat() {	return true;	}

	 public String store(String fileName, Drawing saveDrawing) throws IOException {	DOMImplementation domImpl = GenericDOMImplementation.getDOMImplementation();	Document document = domImpl.createDocument(null, "svg", null);	SVGGraphics2D svgGenerator = new SVGGraphics2D(document);	saveDrawing.draw(svgGenerator);	fileName = adjustFileName(fileName);	FileOutputStream fos = new FileOutputStream(fileName);	Writer out = new OutputStreamWriter(fos, "UTF-8");	svgGenerator.stream(out, true);	return fileName; }

	 public Drawing restore(String fileName) throws IOException { throw new IOException("Not implemented"); }


}
