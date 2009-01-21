
package org.jhotdraw.standard; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
public  class  SimpleUpdateStrategy  implements Painter {
		private static final long serialVersionUID = -7539925820692134566L;

		public void draw(Graphics g, DrawingView view) {	view.drawAll(g);	}


}
