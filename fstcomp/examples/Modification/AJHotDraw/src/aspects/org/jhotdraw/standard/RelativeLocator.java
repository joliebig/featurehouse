
package org.jhotdraw.standard; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.Locator; 
public  class  RelativeLocator  extends AbstractLocator {
		private static final long serialVersionUID = 2619148876087898602L;

		private int relativeLocatorSerializedDataVersion = 1;

		double fRelativeX;

		double fRelativeY;

		public RelativeLocator() {	fRelativeX = 0.0;	fRelativeY = 0.0;	}

		public boolean equals(Object o) {	if (RelativeLocator.class.isInstance(o)) {	RelativeLocator rl = (RelativeLocator) o;	if ((rl.fRelativeX) == fRelativeX && (rl.fRelativeY == fRelativeY)) {	return true;	}	}	return false;	}

		public RelativeLocator(double relativeX, double relativeY) {	fRelativeX = relativeX;	fRelativeY = relativeY;	}

		public Point locate(Figure owner) {	Rectangle r = owner.displayBox();	return new Point(	r.x + (int)(r.width * fRelativeX),	r.y + (int)(r.height * fRelativeY)	);	}

		static public Locator east() {	return new RelativeLocator(1.0, 0.5);	}

		static public Locator north() {	return new RelativeLocator(0.5, 0.0);	}

		static public Locator west() {	return new RelativeLocator(0.0, 0.5);	}

		static public Locator northEast() {	return new RelativeLocator(1.0, 0.0);	}

		static public Locator northWest() {	return new RelativeLocator(0.0, 0.0);	}

		static public Locator south() {	return new RelativeLocator(0.5, 1.0);	}

		static public Locator southEast() {	return new RelativeLocator(1.0, 1.0);	}

		static public Locator southWest() {	return new RelativeLocator(0.0, 1.0);	}

		static public Locator center() {	return new RelativeLocator(0.5, 0.5);	}


}
