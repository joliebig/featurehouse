
package org.jhotdraw.contrib.html; 
import java.io.Serializable; 
public  class  HTMLContentProducer  extends AttributeFigureContentProducer  implements Serializable {
		protected final static int[][] htmlFontSizeEquivalences =	{	{1, 0, 9},	{2, 10, 11},	{3, 12, 13},	{4, 14, 17},	{5, 18, 23},	{6, 24, 35},	{7, 36, Integer.MAX_VALUE}	};

		public HTMLContentProducer() { }

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue) {	HTMLContentProducerContext htmlContext = (HTMLContentProducerContext)context;	if (ctxAttrName.compareTo(ContentProducer.ENTITY_FONT_SIZE) == 0) {	return Integer.toString(getHTMLFontSizeEquivalent(htmlContext.getFont().getSize()));	}	return super.getContent(context, ctxAttrName, ctxAttrValue);	}

		public int getHTMLFontSizeEquivalent(int pointSize) {	for (int i = 0; i < htmlFontSizeEquivalences.length; i++) {	if (pointSize >= htmlFontSizeEquivalences[i][1] &&	pointSize <= htmlFontSizeEquivalences[i][2]) {	return htmlFontSizeEquivalences[i][0];	}	}	return 3;	}


}
