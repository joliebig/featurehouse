
package org.jhotdraw.contrib; 
import java.awt.*; 
import java.awt.font.FontRenderContext; 
import java.awt.font.LineBreakMeasurer; 
import java.awt.font.TextAttribute; 
import java.awt.font.TextLayout; 
import java.awt.geom.Rectangle2D; 
import java.io.IOException; 
import java.io.ObjectInputStream; 
import java.text.AttributedCharacterIterator; 
import java.text.AttributedString; 
import java.text.CharacterIterator; 
import java.util.*; 
import java.util.List; 
import org.jhotdraw.figures.AttributeFigure; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureChangeEvent; 
import org.jhotdraw.framework.FigureChangeListener; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
public  class  TextAreaFigure  extends AttributeFigure  implements FigureChangeListener, TextHolder {
		protected boolean fTextIsDirty = true;

		protected transient boolean fSizeIsDirty = true;

		private Rectangle fDisplayBox;

		protected List fParagraphs;

		protected String fText;

		protected Font fFont;

		protected boolean fFontIsDirty = true;

		protected float fFontWidth;

		protected Hashtable attributesMap = new Hashtable();

		protected boolean fIsReadOnly;

		protected Figure fObservedFigure = null;

		protected OffsetLocator fLocator = null;

		final static long serialVersionUID = 4993631445423148845L;

		static {	initDefaultAttribute("LeftMargin", new Float(5));	initDefaultAttribute("RightMargin", new Float(5));	initDefaultAttribute("TopMargin", new Float(5));	initDefaultAttribute("TabSize", new Float(8));	}

		public TextAreaFigure() {	fParagraphs = CollectionsFactory.current().createList();	fDisplayBox = new Rectangle(0, 0, 30, 15);	fFont = createFont();	fText = new String("");	fSizeIsDirty = true;	fTextIsDirty = true;	fFontIsDirty = true;	}

		public String getText() {	return fText;	}

		public void setText(String newText) {	if (newText == null || !newText.equals(fText)) {	markTextDirty();	fText = newText;	changed();	}	}

		public Rectangle textDisplayBox() {	return displayBox();	}

		public Font createFont() {	return new Font(	(String)getAttribute("FontName"),	((Integer)getAttribute("FontStyle")).intValue(),	((Integer)getAttribute("FontSize")).intValue());	}

		public boolean isReadOnly()	{	return fIsReadOnly;	}

		public void setReadOnly(boolean newReadOnly)	{	fIsReadOnly = newReadOnly;	}

		public boolean acceptsTyping() {	return !isReadOnly();	}

		protected void markTextDirty() {	setTextDirty(true);	}

		protected void setTextDirty(boolean newTextDirty) {	fTextIsDirty = newTextDirty;	}

		public boolean isTextDirty() {	return fTextIsDirty;	}

		protected void markSizeDirty() {	setSizeDirty(true);	}

		public void setSizeDirty(boolean newSizeIsDirty) {	fSizeIsDirty = newSizeIsDirty;	}

		public boolean isSizeDirty() {	return fSizeIsDirty;	}

		public Font getFont() {	return fFont;	}

		public void setFont(Font newFont) {	if(newFont == null) {	throw new IllegalArgumentException();	}	willChange();	fFont = newFont;	markSizeDirty();	markFontDirty();	attributesMap = new Hashtable(1);	attributesMap.put(TextAttribute.FONT, newFont);	changed();	}

		public int overlayColumns() {	return 0;	}

		public void basicDisplayBox(Point origin, Point corner) {	Dimension prevSize = fDisplayBox.getSize();	fDisplayBox = new Rectangle(origin);	fDisplayBox.add(corner);	if (!fDisplayBox.getSize().equals(prevSize)){ markSizeDirty();	}	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	BoxHandleKit.addHandles(this, handles);	return new HandleEnumerator(handles);	}

		public Rectangle displayBox() {	return new Rectangle(	fDisplayBox.x,	fDisplayBox.y,	fDisplayBox.width,	fDisplayBox.height);	}

		public void moveBy(int x, int y) {	willChange();	basicMoveBy(x, y);	if (fLocator != null) {	fLocator.moveBy(x, y);	}	changed();	}

		protected void basicMoveBy(int x, int y) {	fDisplayBox.translate(x, y);	}

		public void drawBackground(Graphics g) {	Rectangle r = displayBox();	g.fillRect(r.x, r.y, r.width, r.height);	}

		public void draw(Graphics g) {	super.draw(g);	drawText(g, displayBox());	}

		public void drawFrame(Graphics g) {	Rectangle r = displayBox();	g.setColor((Color)getAttribute("FrameColor"));	g.drawRect(r.x, r.y, r.width, r.height);	}

		protected float drawText(Graphics g, Rectangle displayBox) {	Graphics2D g2 = null;	Shape savedClipArea = null;	Color savedFontColor = null;	Rectangle2D clipRect = null;	RenderingHints savedRenderingHints = null;	if (g != null) {	g2 = (Graphics2D)g;	savedRenderingHints = g2.getRenderingHints();	g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,	RenderingHints.VALUE_ANTIALIAS_ON);	g2.setRenderingHint(RenderingHints.KEY_RENDERING,	RenderingHints.VALUE_RENDER_QUALITY);	savedFontColor = g2.getColor();	savedClipArea = g2.getClip();	if(savedClipArea != null) {	clipRect = displayBox.createIntersection((Rectangle2D)savedClipArea);	}	else {	clipRect = displayBox;	}	g2.setClip(clipRect);	Color textColor = getTextColor();	if (!ColorMap.isTransparent(textColor)) {	g2.setColor(textColor);	}	g2.setFont(getFont());	}	FontRenderContext fontRenderCtx = new FontRenderContext(null, false, false);	prepareText();	float leftMargin = displayBox.x + ((Float)getAttribute("LeftMargin")).floatValue();	float rightMargin = displayBox.x + displayBox.width - ((Float)getAttribute("RightMargin")).floatValue();	float topMargin = displayBox.y + ((Float)getAttribute("TopMargin")).floatValue();	float[] tabStops = new float[40];	float tabSize = ((Float)getAttribute("TabSize")).floatValue() * getFontWidth();	float tabPos = tabSize;	for (int tabCnt = 0; tabCnt < 40; tabCnt++) {	tabStops[tabCnt] = tabPos + leftMargin;	tabPos += tabSize;	}	float verticalPos = topMargin;	Iterator paragraphs = fParagraphs.iterator();	while (paragraphs.hasNext()) {	String paragraphText = (String)paragraphs.next();	AttributedString attrText = new AttributedString(paragraphText);	AttributedCharacterIterator paragraphIter = attrText.getIterator();	int[] tabLocations = new int[paragraphText.length()];	int tabCount = 0;	for (char c = paragraphIter.first(); c != CharacterIterator.DONE; c = paragraphIter.next()) {	if (c == '\t') {	tabLocations[tabCount++] = paragraphIter.getIndex();	}	}	tabLocations[tabCount] = paragraphIter.getEndIndex() - 1;	paragraphText = paragraphText.replace('\t', ' ');	attrText = new AttributedString(paragraphText, attributesMap);	paragraphIter = attrText.getIterator();	LineBreakMeasurer measurer = new LineBreakMeasurer(paragraphIter, fontRenderCtx);	int currentTab = 0;	while (measurer.getPosition() < paragraphIter.getEndIndex()) {	boolean lineContainsText = false;	boolean lineComplete = false;	float maxAscent = 0;	float maxDescent = 0;	float horizontalPos = leftMargin;	List layouts = CollectionsFactory.current().createList(1);	List penPositions = CollectionsFactory.current().createList(1);	while (!lineComplete) {	float wrappingWidth = rightMargin - horizontalPos;	wrappingWidth = Math.max(1, wrappingWidth);	TextLayout layout =	measurer.nextLayout(wrappingWidth,	tabLocations[currentTab] + 1,	lineContainsText);	if (layout != null) {	layouts.add(layout);	penPositions.add(new Float(horizontalPos));	horizontalPos += layout.getAdvance();	maxAscent = Math.max(maxAscent, layout.getAscent());	maxDescent = Math.max(maxDescent,	layout.getDescent() + layout.getLeading());	}	else {	lineComplete = true;	}	lineContainsText = true;	if (measurer.getPosition() == tabLocations[currentTab] + 1) {	currentTab++;	}	if (measurer.getPosition() == paragraphIter.getEndIndex()) {	lineComplete = true;	}	else if (horizontalPos >= tabStops[tabStops.length - 1]) {	lineComplete = true;	}	if (!lineComplete) {	int j;	for (j = 0; horizontalPos >= tabStops[j]; j++) {	}	horizontalPos = tabStops[j];	}	}	verticalPos += maxAscent;	Iterator layoutEnum = layouts.iterator();	Iterator positionEnum = penPositions.iterator();	while (layoutEnum.hasNext()) {	TextLayout nextLayout = (TextLayout)layoutEnum.next();	Float nextPosition = (Float)positionEnum.next();	if (g2 != null) {	nextLayout.draw(g2, nextPosition.floatValue(), verticalPos);	}	}	verticalPos += maxDescent;	}	}	if (g2 != null && verticalPos > clipRect.getMaxY() && clipRect.getMaxY() == displayBox.getMaxY()) {	Stroke savedStroke = g2.getStroke();	float[] dash = new float[2];	dash[0] = 2f;	dash[1] = 4f;	g2.setStroke(new BasicStroke(	1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER,	1f, dash, 0f));	g2.setColor(Color.red);	g2.drawLine((int)clipRect.getMinX() + 1, (int)clipRect.getMaxY() - 1,	(int)clipRect.getMaxX() - 1, (int)clipRect.getMaxY() - 1);	g2.setStroke(savedStroke);	}	if (g2 != null) {	if(savedClipArea != null) {	g2.setClip(savedClipArea);	}	g2.setColor(savedFontColor);	g2.setRenderingHints(savedRenderingHints);	}	return verticalPos;	}

		protected void prepareText() {	if (!isTextDirty()) {	return;	}	fParagraphs = CollectionsFactory.current().createList();	String paragraphText;	Point pos = new Point(-1, -1);	while ((paragraphText = getNextParagraph(fText, pos)) != null) {	if (paragraphText.length() == 0) {	paragraphText = " ";	}	fParagraphs.add(paragraphText);	}	setTextDirty(false);	}

		protected String getNextParagraph(String text, Point pos) {	int start = pos.y + 1;	if (start >= text.length()) {	return null;	}	pos.x = start;	int end = text.indexOf('\n', start);	if (end == -1) {	end = text.length();	}	pos.y = end;	if (text.charAt(end - 1) == '\r') {	return text.substring(start, end - 1);	}	else {	return text.substring(start, end);	}	}

		public Object getAttribute(String name) {	return super.getAttribute(name);	}

		public void setAttribute(String name, Object value) {	Font font = getFont();	if (name.equals("FontSize")) {	Integer s = (Integer)value;	setFont(new Font(font.getName(), font.getStyle(), s.intValue()));	super.setAttribute(name, value);	}	else if (name.equals("FontStyle")) {	Integer s = (Integer)value;	int style = font.getStyle();	if (s.intValue() == Font.PLAIN) {	style = Font.PLAIN;	}	else {	style = style ^ s.intValue();	}	setFont(new Font(font.getName(), style, font.getSize()));	super.setAttribute(name, new Integer(style));	}	else if (name.equals("FontName")) {	String n = (String)value;	setFont(new Font(n, font.getStyle(), font.getSize()));	super.setAttribute(name, value);	}	else {	super.setAttribute(name, value);	}	}

		protected void readObject(ObjectInputStream s)	throws ClassNotFoundException, IOException {	s.defaultReadObject();	if (fObservedFigure != null) {	fObservedFigure.addFigureChangeListener(this);	}	markSizeDirty();	markTextDirty();	markFontDirty();	}

		public void connect(Figure figure) {	if (fObservedFigure != null) {	fObservedFigure.removeFigureChangeListener(this);	}	fObservedFigure = figure;	fLocator = new OffsetLocator(figure.connectedTextLocator(this));	fObservedFigure.addFigureChangeListener(this);	updateLocation();	}

		public void disconnect(Figure disconnectFigure) {	if (disconnectFigure != null) {	disconnectFigure.removeFigureChangeListener(this);	}	fLocator = null;	}

		public void figureInvalidated(FigureChangeEvent e) {	}

		public void figureChanged(FigureChangeEvent e) {	updateLocation();	}

		protected void updateLocation() {	if (fLocator != null) {	Point p = fLocator.locate(fObservedFigure);	p.x -= size().width / 2 + fDisplayBox.x;	p.y -= size().height / 2 + fDisplayBox.y;	if (p.x != 0 || p.y != 0) {	willChange();	basicMoveBy(p.x, p.y);	changed();	}	}	}

		public void figureRemoved(FigureChangeEvent e) {	if (listener() != null) {	listener().figureRemoved(new FigureChangeEvent(this));	}	}

		public void figureRequestRemove(FigureChangeEvent e) {	if (listener() != null) {	listener().figureRequestRemove(new FigureChangeEvent(this));	}	}

		public void figureRequestUpdate(FigureChangeEvent e) {	}

		protected float getFontWidth() {	updateFontInfo();	return fFontWidth;	}

		protected void updateFontInfo() {	if (!isFontDirty()) {	return;	}	fFontWidth = (int) getFont().getMaxCharBounds(new FontRenderContext(null, false, false)).getWidth();	setFontDirty(false);	}

		public Color getTextColor() {	return (Color)getAttribute("TextColor");	}

		public boolean isEmpty() {	return (fText.length() == 0);	}

		protected void markFontDirty() {	setFontDirty(true);	}

		public boolean isFontDirty() {	return fFontIsDirty;	}

		public void setFontDirty(boolean newFontIsDirty) {	fFontIsDirty = newFontIsDirty;	}

		public Figure getRepresentingFigure() {	return this;	}


}
