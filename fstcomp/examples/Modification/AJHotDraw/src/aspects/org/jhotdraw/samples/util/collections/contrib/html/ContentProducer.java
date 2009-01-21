
package org.jhotdraw.contrib.html; 
public  interface  ContentProducer {
		public final static String ENTITY_FIGURE_POSX = "FigurePosX";

		public final static String ENTITY_FIGURE_POSY = "FigurePosY";

		public final static String ENTITY_FIGURE_WIDTH = "FigureWidth";

		public final static String ENTITY_FIGURE_HEIGHT = "FigureHeight";

		public final static String ENTITY_FRAME_COLOR = "FrameColor";

		public final static String ENTITY_FILL_COLOR = "FillColor";

		public final static String ENTITY_ARROW_MODE = "ArrowMode";

		public final static String ENTITY_FONT_NAME = "FontName";

		public final static String ENTITY_FONT_SIZE = "FontSize";

		public final static String ENTITY_FONT_STYLE = "FontStyle";

		public Object getContent(ContentProducerContext context, String ctxAttrName, Object ctxAttrValue);


}
