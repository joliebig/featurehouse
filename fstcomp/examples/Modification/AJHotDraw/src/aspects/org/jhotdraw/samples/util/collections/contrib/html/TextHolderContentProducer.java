
package org.jhotdraw.contrib.html; 
import java.io.Serializable; 
import org.jhotdraw.standard.TextHolder; 
public  class  TextHolderContentProducer  extends AbstractContentProducer  implements Serializable {
		private TextHolder myTextHolder;

		public TextHolderContentProducer() { }

		public TextHolderContentProducer(TextHolder figure) {	setTextHolder(figure);	}

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	TextHolder figure = (getTextHolder() != null) ? getTextHolder() : (TextHolder)ctxAttrValue;	return figure.getText();	}

		protected TextHolder getTextHolder() {	return myTextHolder;	}

		public void setTextHolder(TextHolder newFigure) {	myTextHolder = newFigure;	}


}
