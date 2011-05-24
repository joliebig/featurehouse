
package org.jhotdraw.figures; 
import java.awt.*; 
import java.io.*; 
import java.util.List; 
import java.awt.image.ImageObserver; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
public  class  ImageFigure 	extends AttributeFigure  implements ImageObserver {
		private String fFileName;

		private transient Image fImage;

		private Rectangle fDisplayBox;

		private static final long serialVersionUID = 148012030121282439L;

		private int imageFigureSerializedDataVersion = 1;

		public ImageFigure() {	fFileName = null;	fImage = null;	fDisplayBox = null;	}

		public ImageFigure(Image image, String fileName, Point origin) {	fFileName = fileName;	fImage = image;	basicDisplayBox(origin, new Point(origin.x + fImage.getWidth(this), origin.y + fImage.getHeight(this)));	}

		public void basicDisplayBox(Point origin, Point corner) {	fDisplayBox = new Rectangle(origin);	fDisplayBox.add(corner);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	BoxHandleKit.addHandles(this, handles);	return new HandleEnumerator(handles);	}

		public Rectangle displayBox() {	return new Rectangle(	fDisplayBox.x,	fDisplayBox.y,	fDisplayBox.width,	fDisplayBox.height);	}

		protected void basicMoveBy(int x, int y) {	fDisplayBox.translate(x,y);	}

		public void draw(Graphics g) {	if (fImage == null) {	fImage = Iconkit.instance().getImage(fFileName);	}	if (fImage != null) {	g.drawImage(fImage, fDisplayBox.x, fDisplayBox.y, fDisplayBox.width, fDisplayBox.height, this);	}	else {	drawGhost(g);	}	}

		private void drawGhost(Graphics g) {	g.setColor(Color.gray);	g.fillRect(fDisplayBox.x, fDisplayBox.y, fDisplayBox.width, fDisplayBox.height);	}

		public boolean imageUpdate(Image img, int flags, int x, int y, int w, int h) {	if ((flags & (FRAMEBITS|ALLBITS)) != 0) {	invalidate();	if (listener() != null) {	listener().figureRequestUpdate(new FigureChangeEvent(this));	}	}	return (flags & (ALLBITS|ABORT)) == 0;	}

		public void release() {	fImage.flush();	}

		private void readObject(ObjectInputStream s)	throws ClassNotFoundException, IOException {	s.defaultReadObject();	Iconkit.instance().registerImage(fFileName);	fImage = null;	}


}
