
package org.jhotdraw.standard; 
import org.jhotdraw.framework.PointConstrainer; 
import java.awt.*; 
import java.io.Serializable; 
public  class  GridConstrainer  implements PointConstrainer, Serializable {
		private int fGridX;

		private int fGridY;

		public GridConstrainer(int x, int y) {	fGridX = Math.max(1, x);	fGridY = Math.max(1, y);	}

		public Point constrainPoint(Point p) {	p.x = ((p.x+fGridX/2) / fGridX) * fGridX;	p.y = ((p.y+fGridY/2) / fGridY) * fGridY;	return p;	}

		public int getStepX() {	return fGridX;	}

		public int getStepY() {	return fGridY;	}


}
