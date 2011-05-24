
package org.jhotdraw.util; 
import javax.swing.*; 
import java.awt.*; 
import java.awt.image.ImageProducer; 
import java.net.URL; 
import java.util.Iterator; 
import java.util.List; 
import java.util.Map; 
import java.util.Hashtable; 
public  class  Iconkit {
		private Map fMap;

		private List fRegisteredImages;

		private Component fComponent;

		private final static int ID = 123;

		private static Iconkit fgIconkit = null;

		private static boolean fgDebug = false;

		public Iconkit(Component component) {	fMap = new Hashtable(53);	fRegisteredImages = CollectionsFactory.current().createList(10);	fComponent = component;	fgIconkit = this;	}

		public static Iconkit instance() {	return fgIconkit;	}

		public void loadRegisteredImages(Component component) {	if (fRegisteredImages.size() == 0)	return;	MediaTracker tracker = new MediaTracker(component);	Iterator iter = fRegisteredImages.iterator();	while (iter.hasNext()) {	String fileName = (String)iter.next();	if (basicGetImage(fileName) == null) {	tracker.addImage(loadImage(fileName), ID);	}	}	fRegisteredImages.clear();	try {	tracker.waitForAll();	}	catch (Exception e) {	}	}

		public void registerImage(String fileName) {	fRegisteredImages.add(fileName);	}

		public Image registerAndLoadImage(Component component, String fileName) {	registerImage(fileName);	loadRegisteredImages(component);	return getImage(fileName);	}

		public Image loadImage(String filename) {	if (fMap.containsKey(filename)) {	return (Image) fMap.get(filename);	}	Image image = loadImageResource(filename);	if (image != null) {	fMap.put(filename, image);	}	return image;	}

		public Image loadImage(String filename, boolean waitForLoad) {	Image image = loadImage(filename);	if (image!=null && waitForLoad) {	ImageIcon icon = new ImageIcon(image);	image = icon.getImage();	}	return image;	}

		public Image loadImageResource(String resourcename) {	Toolkit toolkit = Toolkit.getDefaultToolkit();	try {	URL url = getClass().getResource(resourcename);	if (fgDebug) {	System.out.println(resourcename);	}	return toolkit.createImage((ImageProducer) url.getContent());	}	catch (Exception ex) {	return null;	}	}

		public Image getImage(String filename) {	Image image = basicGetImage(filename);	if (image != null) {	return image;	}	loadRegisteredImages(fComponent);	return basicGetImage(filename);	}

		private Image basicGetImage(String filename) {	if (fMap.containsKey(filename)) {	return (Image) fMap.get(filename);	}	return null;	}


}
