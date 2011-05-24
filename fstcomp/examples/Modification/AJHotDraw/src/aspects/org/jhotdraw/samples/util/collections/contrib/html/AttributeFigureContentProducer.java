
package org.jhotdraw.contrib.html; 
import java.io.Serializable; 
public  class  AttributeFigureContentProducer  extends FigureDataContentProducer  implements Serializable {
		public AttributeFigureContentProducer() { }

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	Object attrValue = super.getContent(context, ctxAttrName, ctxAttrValue);	if (attrValue != null) {	return attrValue;	}	return ((AttributeContentProducerContext)context).getAttribute(ctxAttrName);	}


}
