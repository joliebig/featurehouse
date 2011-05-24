
package org.jhotdraw.contrib.html; 
import java.awt.Color; 
import java.io.Serializable; 
public  class  ColorContentProducer  extends FigureDataContentProducer  implements Serializable {
		private Color fColor = null;

		public ColorContentProducer() { }

		public ColorContentProducer(Color color) {	setColor(color);	}

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	Color color = (getColor() != null) ? getColor() : (Color)ctxAttrValue;	String colorCode = Integer.toHexString(color.getRGB());	return "0x" + colorCode.substring(colorCode.length() - 6);	}

		public void setColor(Color color) {	fColor = color;	}

		public Color getColor() {	return fColor;	}


}
