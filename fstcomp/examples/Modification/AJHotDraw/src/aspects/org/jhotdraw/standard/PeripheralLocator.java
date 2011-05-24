
package org.jhotdraw.standard; 
import java.awt.Point; 
import org.jhotdraw.framework.Figure; 
import java.awt.*; 
import java.lang.Math; 
public  class  PeripheralLocator  extends AbstractLocator {
		private static int CORNERSPACE = 1;

		private Figure fOwner;

		private int fPPS;

		private int fIndex;

		private PeripheralLocator() {	}

		public PeripheralLocator(int pointsPerSide, int index) {	fPPS = pointsPerSide;	fIndex = index;	if (index >= pointsPerSide *4) {	throw new IllegalArgumentException("Index must be within the range of points starting with index = 0.");	}	}

	 public Point locate(Figure parm1) {	Rectangle r = parm1.displayBox();	float hSpacing = (float)r.width / (fPPS +1);	float vSpacing = (float)r.height / (fPPS +1);	int x = 0;	int y = 0;	if (fIndex < fPPS) {	x = Math.round((fIndex + 1.0f ) * hSpacing);	y = 0;	}	else if (fIndex < (fPPS*2)) {	x = Math.round((fPPS + 1 ) * hSpacing) ;	y = Math.round((fIndex +1 - fPPS) * vSpacing);	}	else if (fIndex < (fPPS*3)) {	x = Math.round(((fPPS + 1 ) - (fIndex +1 - fPPS*2))* hSpacing);	y = Math.round((fPPS + 1 )* vSpacing)/*r.height*/;	}	else {	x = 0;	y = Math.round(((fPPS +1) - (fIndex +1 - fPPS*3))*vSpacing);	}	x = x+r.x;	y = y+r.y;	return new Point((int)x, (int)y); }


}
