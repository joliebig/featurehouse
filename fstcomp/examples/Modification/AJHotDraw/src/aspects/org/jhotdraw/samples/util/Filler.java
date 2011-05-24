
package org.jhotdraw.util; 
import javax.swing.JPanel; 
import java.awt.*; 
public  class  Filler 	extends JPanel {
		private int fWidth;

		private int fHeight;

		private Color fBackground;

		public Filler(int width, int height) {	this(width, height, null);	}

		public Filler(int width, int height, Color background) {	fWidth = width;	fHeight = height;	fBackground = background;	}

		public Dimension getMinimumSize() {	return new Dimension(fWidth, fHeight);	}

		public Dimension getPreferredSize() {	return getMinimumSize();	}

		public Color getBackground() {	if (fBackground != null) {	return fBackground;	}	return super.getBackground();	}


}
