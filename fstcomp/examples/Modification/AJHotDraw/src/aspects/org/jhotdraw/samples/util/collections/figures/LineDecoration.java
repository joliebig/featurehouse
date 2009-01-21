
package org.jhotdraw.figures; 
import java.awt.Graphics; 
import java.awt.Rectangle; 
import java.io.Serializable; 
public  interface  LineDecoration 	extends Cloneable, Serializable {
		public void draw(Graphics g, int x1, int y1, int x2, int y2);

		public Rectangle displayBox();


}
