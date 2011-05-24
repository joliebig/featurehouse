
package org.jhotdraw.framework; 
import java.awt.*; 
public  interface  PointConstrainer {
		public Point constrainPoint(Point p);

		public int getStepX();

		public int getStepY();


}
