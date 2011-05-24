
package org.jhotdraw.contrib.zoom; 
import java.awt.*; 
import java.awt.image.ImageObserver; 
public  class  ScalingGraphics  extends java.awt.Graphics {
		private double scale = 1.0;

		private Graphics real;

		private Font userFont;

		private Rectangle userClip;

		public ScalingGraphics(Graphics realGraphics) {	real = realGraphics;	}

		public void setScale(double newScale) {	scale = newScale;	}

		public double getScale() {	return scale;	}

		private static Font scaledFont(Font f, double scale) {	int size = f.getSize();	int scaledSize = (int) (size * scale);	return new Font(f.getFamily(), f.getStyle(), scaledSize);	}

		private static Shape scaledShape(Shape s, double scale) {	if (s instanceof Rectangle) {	Rectangle r = (Rectangle) s;	return new Rectangle((int) (r.x * scale), (int) (r.y * scale),	(int) (r.width * scale), (int) (r.height * scale));	}	else {	throw new RuntimeException("Cannot scale shape: " + s.getClass().getName());	}	}

		public Graphics create() {	Graphics realCopy = real.create();	ScalingGraphics result = new ScalingGraphics(realCopy);	result.setScale(getScale());	return result;	}

		public void translate(int x, int y) {	real.translate((int) (x * scale), (int) (y * scale));	}

		public Color getColor() {	return real.getColor();	}

		public void setColor(Color c) {	real.setColor(c);	}

		public void setPaintMode() {	real.setPaintMode();	}

		public void setXORMode(Color c1) {	real.setXORMode(c1);	}

		public Font getFont() {	if (userFont == null)	userFont = real.getFont();	return userFont;	}

		public void setFont(Font font) {	userFont = font;	real.setFont(scaledFont(font, scale));	}

		public FontMetrics getFontMetrics() {	return new ScalingFontMetrics(userFont, real.getFontMetrics());	}

		public FontMetrics getFontMetrics(Font f) {	return new ScalingFontMetrics(f,	real.getFontMetrics(scaledFont(f, scale)));	}

		public Rectangle getClipBounds() {	return userClip;	}

		public void clipRect(int x, int y, int width, int height) {	if (userClip == null)	userClip = new Rectangle(x, y, width, height);	else	userClip = userClip.intersection(new Rectangle(x, y, width, height));	real.clipRect((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale));	}

		public void setClip(int x, int y, int width, int height) {	userClip = new Rectangle(x, y, width, height);	real.setClip((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale));	}

		public Shape getClip() {	return userClip;	}

		public void setClip(Shape clip) {	userClip = (Rectangle) clip;	if (clip != null)	real.setClip(scaledShape(clip, scale));	else	real.setClip(null);	}

		public void copyArea(int x, int y, int width, int height, int dx, int dy) {	real.copyArea((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale),	(int) (dx * scale), (int) (dy * scale));	}

		public void drawLine(int x1, int y1, int x2, int y2) {	real.drawLine((int) (x1 * scale), (int) (y1 * scale),	(int) (x2 * scale), (int) (y2 * scale));	}

		public void fillRect(int x, int y, int width, int height) {	real.fillRect((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale));	}

		public void clearRect(int x, int y, int width, int height) {	real.clearRect((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale));	}

		public void drawRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight) {	real.drawRoundRect((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale),	(int) (arcWidth * scale), (int) (arcHeight * scale));	}

		public void fillRoundRect(int x, int y, int width, int height, int arcWidth, int arcHeight) {	real.fillRoundRect((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale),	(int) (arcWidth * scale), (int) (arcHeight * scale));	}

		public void drawOval(int x, int y, int width, int height) {	real.drawOval((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale));	}

		public void fillOval(int x, int y, int width, int height) {	real.fillOval((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale));	}

		public void drawArc(int x, int y, int width, int height,	int startAngle, int arcAngle) {	real.drawArc((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale),	startAngle, arcAngle);	}

		public void fillArc(int x, int y, int width, int height,	int startAngle, int arcAngle) {	real.fillArc((int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale),	startAngle, arcAngle);	}

		public void drawPolyline(int xPoints[], int yPoints[], int nPoints) {	int[] realXPoints = new int[nPoints];	int[] realYPoints = new int[nPoints];	for (int i = 0; i < nPoints; i++) {	realXPoints[i] = (int) (xPoints[i] * scale);	realYPoints[i] = (int) (yPoints[i] * scale);	}	real.drawPolyline(realXPoints, realYPoints, nPoints);	}

		public void drawPolygon(int xPoints[], int yPoints[],	int nPoints) {	int[] realXPoints = new int[nPoints];	int[] realYPoints = new int[nPoints];	for (int i = 0; i < nPoints; i++) {	realXPoints[i] = (int) (xPoints[i] * scale);	realYPoints[i] = (int) (yPoints[i] * scale);	}	real.drawPolygon(realXPoints, realYPoints, nPoints);	}

		public void fillPolygon(int xPoints[], int yPoints[],	int nPoints) {	int[] realXPoints = new int[nPoints];	int[] realYPoints = new int[nPoints];	for (int i = 0; i < nPoints; i++) {	realXPoints[i] = (int) (xPoints[i] * scale);	realYPoints[i] = (int) (yPoints[i] * scale);	}	real.fillPolygon(realXPoints, realYPoints, nPoints);	}

		public void drawString(String str, int x, int y) {	real.drawString(str, (int) (x * scale), (int) (y * scale));	}

		public void drawString(java.text.AttributedCharacterIterator iterator, int x, int y) {	real.drawString(iterator, (int) (x * scale), (int) (y * scale));	}

		public boolean drawImage(Image img, int x, int y, ImageObserver observer) {	if (img instanceof DoubleBufferImage)	return real.drawImage(((DoubleBufferImage) img).getRealImage(),	x, y, observer);	else	return real.drawImage(img, (int) (x * scale), (int) (y * scale),	(int) (img.getWidth(observer) * scale),	(int) (img.getHeight(observer) * scale),	observer);	}

		public boolean drawImage(Image img, int x, int y, int width, int height, ImageObserver observer) {	if (img instanceof DoubleBufferImage)	return real.drawImage(((DoubleBufferImage) img).getRealImage(),	x, y, width, height, observer);	else	return real.drawImage(img, (int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale),	observer);	}

		public boolean drawImage(Image img, int x, int y, Color bgcolor, ImageObserver observer) {	if (img instanceof DoubleBufferImage)	return real.drawImage(((DoubleBufferImage) img).getRealImage(),	x, y, bgcolor, observer);	else	return real.drawImage(img, (int) (x * scale), (int) (y * scale),	(int) (img.getWidth(observer) * scale),	(int) (img.getHeight(observer) * scale),	bgcolor, observer);	}

		public boolean drawImage(Image img, int x, int y, int width, int height, Color bgcolor, ImageObserver observer) {	if (img instanceof DoubleBufferImage)	return real.drawImage(((DoubleBufferImage) img).getRealImage(),	x, y, width, height, bgcolor, observer);	else	return real.drawImage(img, (int) (x * scale), (int) (y * scale),	(int) (width * scale), (int) (height * scale),	bgcolor, observer);	}

		public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, ImageObserver observer) {	if (img instanceof DoubleBufferImage)	return real.drawImage(((DoubleBufferImage) img).getRealImage(),	dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2,	observer);	else	return real.drawImage(img, (int) (dx1 * scale), (int) (dy1 * scale),	(int) (dx2 * scale), (int) (dy2 * scale),	(int) (sx1 * scale), (int) (sy1 * scale),	(int) (sx2 * scale), (int) (sy2 * scale),	observer);	}

		public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2, int sx1, int sy1, int sx2, int sy2, Color bgcolor, ImageObserver observer) {	if (img instanceof DoubleBufferImage)	return real.drawImage(((DoubleBufferImage) img).getRealImage(),	dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2,	bgcolor, observer);	else	return real.drawImage(img, (int) (dx1 * scale), (int) (dy1 * scale),	(int) (dx2 * scale), (int) (dy2 * scale),	(int) (sx1 * scale), (int) (sy1 * scale),	(int) (sx2 * scale), (int) (sy2 * scale),	bgcolor, observer);	}

		public void dispose() {	real.dispose();	}

		private  class  ScalingFontMetrics  extends FontMetrics {
			private FontMetrics real;

			private Font userFont;

			public ScalingFontMetrics(Font newUserFont, FontMetrics newReal) {	super(null);	userFont = newUserFont;	real = newReal;	}

			public Font getFont() {	return userFont;	}

			public int getAscent() {	return (int) (real.getAscent() / ScalingGraphics.this.getScale());	}

			public int getLeading() {	return (int) (real.getLeading() / ScalingGraphics.this.getScale());	}

			public int getMaxAdvance() {	return (int) (real.getMaxAdvance() / ScalingGraphics.this.getScale());	}

			public int charWidth(char ch) {	return (int) (real.charWidth(ch) / ScalingGraphics.this.getScale());	}

			public int charsWidth(char[] data, int off, int len) {	return (int) (real.charsWidth(data, off, len) /	ScalingGraphics.this.getScale());	}


	}


}
