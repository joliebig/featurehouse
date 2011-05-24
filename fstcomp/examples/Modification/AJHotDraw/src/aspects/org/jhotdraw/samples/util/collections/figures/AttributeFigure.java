
package org.jhotdraw.figures; 
import org.jhotdraw.util.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import java.awt.*; 
import java.io.*; 
public abstract  class  AttributeFigure  extends AbstractFigure {
		private FigureAttributes fAttributes;

		private static FigureAttributes fgDefaultAttributes = null;

		private static final long serialVersionUID = -10857585979273442L;

		private int attributeFigureSerializedDataVersion = 1;

		protected AttributeFigure() { }

		public void draw(Graphics g) {	Color fill = getFillColor();	if (!ColorMap.isTransparent(fill)) {	g.setColor(fill);	drawBackground(g);	}	Color frame = getFrameColor();	if (!ColorMap.isTransparent(frame)) {	g.setColor(frame);	drawFrame(g);	}	}

		protected void drawBackground(Graphics g) {	}

		protected void drawFrame(Graphics g) {	}

		public Color getFillColor() {	return (Color) getAttribute(FigureAttributeConstant.FILL_COLOR);	}

		public Color getFrameColor() {	return (Color) getAttribute(FigureAttributeConstant.FRAME_COLOR);	}

		private static void initializeAttributes() {	fgDefaultAttributes = new FigureAttributes();	fgDefaultAttributes.set(FigureAttributeConstant.FRAME_COLOR, Color.black);	fgDefaultAttributes.set(FigureAttributeConstant.FILL_COLOR, new Color(0x70DB93));	fgDefaultAttributes.set(FigureAttributeConstant.TEXT_COLOR, Color.black);	fgDefaultAttributes.set(FigureAttributeConstant.ARROW_MODE, new Integer(0));	fgDefaultAttributes.set(FigureAttributeConstant.FONT_NAME, "Helvetica");	fgDefaultAttributes.set(FigureAttributeConstant.FONT_SIZE, new Integer(12));	fgDefaultAttributes.set(FigureAttributeConstant.FONT_STYLE, new Integer(Font.PLAIN));	}

		public static Object setDefaultAttribute(String name, Object value) {	Object currentValue = getDefaultAttribute(name);	fgDefaultAttributes.set(FigureAttributeConstant.getConstant(name), value);	return currentValue;	}

		public static Object initDefaultAttribute(String name, Object value) {	Object currentValue = getDefaultAttribute(name);	if (currentValue != null) {	return currentValue;	}	fgDefaultAttributes.set(FigureAttributeConstant.getConstant(name), value);	return null;	}

		public static Object getDefaultAttribute(String name) {	if (fgDefaultAttributes == null) {	initializeAttributes();	}	return fgDefaultAttributes.get(FigureAttributeConstant.getConstant(name));	}

		public static Object getDefaultAttribute(FigureAttributeConstant attributeConstant) {	if (fgDefaultAttributes == null) {	initializeAttributes();	}	return fgDefaultAttributes.get(attributeConstant);	}

		public Object getAttribute(String name) {	return getAttribute(FigureAttributeConstant.getConstant(name));	}

		public Object getAttribute(FigureAttributeConstant attributeConstant) {	if (fAttributes != null) {	if (fAttributes.hasDefined(attributeConstant)) {	return fAttributes.get(attributeConstant);	}	}	return getDefaultAttribute(attributeConstant);	}

		public void setAttribute(String name, Object value) {	setAttribute(FigureAttributeConstant.getConstant(name), value);	}

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value) {	if (fAttributes == null) {	fAttributes = new FigureAttributes();	}	fAttributes.set(attributeConstant, value);	changed();	}

		private void writeObject(ObjectOutputStream o) throws IOException {	Object associatedMenu = getAttribute(Figure.POPUP_MENU);	if (associatedMenu != null) {	setAttribute(Figure.POPUP_MENU, null);	}	o.defaultWriteObject();	if (associatedMenu != null) {	setAttribute(Figure.POPUP_MENU, associatedMenu);	}	}


}
