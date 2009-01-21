
package org.jhotdraw.contrib.zoom; 
import java.awt.*; 
import java.awt.image.ImageObserver; 
import java.awt.image.ImageProducer; 
public  class  DoubleBufferImage  extends java.awt.Image {
		private Image real;

		private double scale;

		public DoubleBufferImage(Image newReal, double newScale) {	real = newReal;	scale = newScale;	}

		public Image getRealImage() {	return real;	}

		public void flush() {	real.flush();	}

		public Graphics getGraphics() {	ScalingGraphics result = new ScalingGraphics(real.getGraphics());	result.setScale(scale);	return result;	}

		public int getHeight(ImageObserver observer) {	return real.getHeight(observer);	}

		public Object getProperty(String name, ImageObserver observer) {	return real.getProperty(name, observer);	}

		public Image getScaledInstance(int width, int height, int hints) {	return real.getScaledInstance(width, height, hints);	}

		public ImageProducer getSource() {	return real.getSource();	}

		public int getWidth(ImageObserver observer) {	return real.getWidth(observer);	}


}
