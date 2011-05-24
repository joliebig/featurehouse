
package org.jhotdraw.contrib.html; 
import java.awt.Color; 
import java.io.Serializable; 
public  class  HTMLColorContentProducer  extends ColorContentProducer  implements Serializable {
		public HTMLColorContentProducer() { }

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	Color color = (getColor() != null) ? getColor() : (Color)ctxAttrValue;	return getHTMLColorCode(color);	}

		public static String getHTMLColorCode(Color color) {	String colorCode = Integer.toHexString(color.getRGB());	return "#" + colorCode.substring(colorCode.length() - 6);	}


}
