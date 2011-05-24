package org.jhotdraw.contrib; 
import org.jhotdraw.application.DrawApplication; 
import org.jhotdraw.samples.javadraw.JavaDrawApp; 
import org.jhotdraw.util.StorageFormatManager; 
public  class  SVGDrawApp  extends JavaDrawApp {
		public static void main(String[] args) {	SVGDrawApp window = new SVGDrawApp();	window.open();	}

		public SVGDrawApp() {	super("AJHotDraw");	}

		protected DrawApplication createApplication() {	return new SVGDrawApp();	}

		public StorageFormatManager createStorageFormatManager() {	StorageFormatManager storageFormatManager = new StorageFormatManager();	SVGStorageFormat format = new SVGStorageFormat();	storageFormatManager.addStorageFormat(format);	storageFormatManager.setDefaultStorageFormat(format);	return storageFormatManager;	}


}
