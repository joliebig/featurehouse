
package org.jhotdraw.figures; 
import java.awt.Color; 
import java.awt.Dimension; 
import java.awt.Font; 
import java.awt.FontMetrics; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.awt.Toolkit; 
import java.io.IOException; 
import java.io.ObjectInputStream; 
import java.util.List; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureAttributeConstant; 
import org.jhotdraw.framework.FigureChangeEvent; 
import org.jhotdraw.framework.FigureChangeListener; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.standard.HandleEnumerator; 
import org.jhotdraw.standard.NullHandle; 
import org.jhotdraw.standard.OffsetLocator; 
import org.jhotdraw.standard.RelativeLocator; 
import org.jhotdraw.standard.TextHolder; 
import org.jhotdraw.util.CollectionsFactory; 
import org.jhotdraw.util.ColorMap; 
public  class  TextFigure 	extends AttributeFigure 	implements FigureChangeListener, TextHolder {
		private int fOriginX;

		private int fOriginY;

		transient private boolean fSizeIsDirty = true;

		transient private int fWidth;

		transient private int fHeight;

		private String fText;

		private Font fFont;

		private boolean fIsReadOnly;

		private Figure fObservedFigure = null;

		private OffsetLocator fLocator = null;

		private static String fgCurrentFontName = "Helvetica";

		private static int fgCurrentFontSize = 12;

		private static int fgCurrentFontStyle = Font.PLAIN;

		private static final long serialVersionUID = 4599820785949456124L;

		private int textFigureSerializedDataVersion = 1;

		public TextFigure() {	fOriginX = 0;	fOriginY = 0;	fFont = createCurrentFont();	setAttribute(FigureAttributeConstant.FILL_COLOR, ColorMap.color("None"));	fText = "";	fSizeIsDirty = true;	}

		public void moveBy(int x, int y) {	willChange();	basicMoveBy(x, y);	if (getLocator() != null) {	getLocator().moveBy(x, y);	}	changed();	}

		protected void basicMoveBy(int x, int y) {	fOriginX += x;	fOriginY += y;	}

		public void basicDisplayBox(Point newOrigin, Point newCorner) {	fOriginX = newOrigin.x;	fOriginY = newOrigin.y;	}

		public Rectangle displayBox() {	Dimension extent = textExtent();	return new Rectangle(fOriginX, fOriginY, extent.width, extent.height);	}

		public Rectangle textDisplayBox() {	return displayBox();	}

		public boolean readOnly() {	return fIsReadOnly;	}

		public void setReadOnly(boolean isReadOnly) {	fIsReadOnly = isReadOnly;	}

		public Font getFont() {	return fFont;	}

		public Figure getRepresentingFigure() {	return this;	}

		public void setFont(Font newFont) {	willChange();	fFont = newFont;	markDirty();	changed();	}

		public void changed() {	super.changed();	}

		public Object getAttribute(String name) {	return getAttribute(FigureAttributeConstant.getConstant(name));	}

		public Object getAttribute(FigureAttributeConstant attributeConstant) {	Font font = getFont();	if (attributeConstant.equals(FigureAttributeConstant.FONT_SIZE)) {	return new Integer(font.getSize());	}	if (attributeConstant.equals(FigureAttributeConstant.FONT_STYLE)) {	return new Integer(font.getStyle());	}	if (attributeConstant.equals(FigureAttributeConstant.FONT_NAME)) {	return font.getName();	}	return super.getAttribute(attributeConstant);	}

		public void setAttribute(String name, Object value) {	setAttribute(FigureAttributeConstant.getConstant(name), value);	}

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value) {	Font font = getFont();	if (attributeConstant.equals(FigureAttributeConstant.FONT_SIZE)) {	Integer s = (Integer) value;	setFont(new Font(font.getName(), font.getStyle(), s.intValue()));	}	else if (attributeConstant.equals(FigureAttributeConstant.FONT_STYLE)) {	Integer s = (Integer) value;	int style = font.getStyle();	if (s.intValue() == Font.PLAIN) {	style = Font.PLAIN;	}	else {	style = style ^ s.intValue();	}	setFont(new Font(font.getName(), style, font.getSize()));	}	else if (attributeConstant.equals(FigureAttributeConstant.FONT_NAME)) {	String n = (String) value;	setFont(new Font(n, font.getStyle(), font.getSize()));	}	else {	super.setAttribute(attributeConstant, value);	}	}

		public String getText() {	return fText;	}

		public void setText(String newText) {	if (newText == null || !newText.equals(fText)) {	willChange();	fText = newText;	markDirty();	changed();	}	}

		public boolean acceptsTyping() {	return !fIsReadOnly;	}

		public void drawBackground(Graphics g) {	Rectangle r = displayBox();	g.fillRect(r.x, r.y, r.width, r.height);	}

		public void drawFrame(Graphics g) {	g.setFont(fFont);	g.setColor((Color) getAttribute(FigureAttributeConstant.TEXT_COLOR));	FontMetrics metrics = Toolkit.getDefaultToolkit().getFontMetrics(fFont);	Rectangle r = displayBox();	g.drawString(getText(), r.x, r.y + metrics.getAscent());	}

		protected Dimension textExtent() {	if (!fSizeIsDirty) {	return new Dimension(fWidth, fHeight);	}	FontMetrics metrics = Toolkit.getDefaultToolkit().getFontMetrics(fFont);	fWidth = metrics.stringWidth(getText());	fHeight = metrics.getHeight();	fSizeIsDirty = false;	return new Dimension(fWidth, fHeight);	}

		protected void markDirty() {	fSizeIsDirty = true;	}

		public int overlayColumns() {	int length = getText().length();	int columns = 20;	if (length != 0) {	columns = getText().length() + 3;	}	return columns;	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	handles.add(new NullHandle(this, RelativeLocator.northWest()));	handles.add(new NullHandle(this, RelativeLocator.northEast()));	handles.add(new NullHandle(this, RelativeLocator.southEast()));	handles.add(new FontSizeHandle(this, RelativeLocator.southWest()));	return new HandleEnumerator(handles);	}

		private void readObject(ObjectInputStream s) throws ClassNotFoundException, IOException {	s.defaultReadObject();	if (getObservedFigure() != null) {	getObservedFigure().addFigureChangeListener(this);	}	markDirty();	}

		public void connect(Figure figure) {	if (getObservedFigure() != null) {	getObservedFigure().removeFigureChangeListener(this);	}	setObservedFigure(figure);	setLocator(new OffsetLocator(getObservedFigure().connectedTextLocator(this)));	getObservedFigure().addFigureChangeListener(this);	willChange();	updateLocation();	changed();	}

		public void figureChanged(FigureChangeEvent e) {	willChange();	updateLocation();	changed();	}

		public void figureRemoved(FigureChangeEvent e) {	if (listener() != null) {	Rectangle rect = invalidateRectangle(displayBox());	listener().figureRemoved(new FigureChangeEvent(this, rect, e));	}	}

		public void figureRequestRemove(FigureChangeEvent e) {	}

		public void figureInvalidated(FigureChangeEvent e) {	}

		public void figureRequestUpdate(FigureChangeEvent e) {	}

		protected void updateLocation() {	if (getLocator() != null) {	Point p = getLocator().locate(getObservedFigure());	p.x -= size().width / 2 + fOriginX;	p.y -= size().height / 2 + fOriginY;	if (p.x != 0 || p.y != 0) {	basicMoveBy(p.x, p.y);	}	}	}

		public void release() {	super.release();	disconnect(getObservedFigure());	}

		public void disconnect(Figure disconnectFigure) {	if (disconnectFigure != null) {	disconnectFigure.removeFigureChangeListener(this);	}	setLocator(null);	setObservedFigure(null);	}

		protected void setObservedFigure(Figure newObservedFigure) {	fObservedFigure = newObservedFigure;	}

		public Figure getObservedFigure() {	return fObservedFigure;	}

		protected void setLocator(OffsetLocator newLocator) {	fLocator = newLocator;	}

		protected OffsetLocator getLocator() {	return fLocator;	}

		public TextHolder getTextHolder() {	return this;	}

		static public Font createCurrentFont() {	return new Font(fgCurrentFontName, fgCurrentFontStyle, fgCurrentFontSize);	}

		static public void setCurrentFontName(String name) {	fgCurrentFontName = name;	}

		static public void setCurrentFontSize(int size) {	fgCurrentFontSize = size;	}

		static public void setCurrentFontStyle(int style) {	fgCurrentFontStyle = style;	}


}
