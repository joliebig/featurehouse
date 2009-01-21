
package org.jhotdraw.contrib.html; 
import java.io.Serializable; 
public  class  FigureDataContentProducer  extends AbstractContentProducer 	implements Serializable {
		public FigureDataContentProducer() { }

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	if (ctxAttrName.compareTo(ContentProducer.ENTITY_FIGURE_WIDTH) == 0) {	return Integer.toString(((FigureContentProducerContext)context).displayBox().width);	}	if (ctxAttrName.compareTo(ContentProducer.ENTITY_FIGURE_HEIGHT) == 0) {	return Integer.toString(((FigureContentProducerContext)context).displayBox().height);	}	if (ctxAttrName.compareTo(ContentProducer.ENTITY_FIGURE_POSX) == 0) {	return Integer.toString(((FigureContentProducerContext)context).displayBox().x);	}	if (ctxAttrName.compareTo(ContentProducer.ENTITY_FIGURE_POSY) == 0) {	return Integer.toString(((FigureContentProducerContext)context).displayBox().y);	}	return null;	}


}
