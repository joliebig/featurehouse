
package org.jhotdraw.contrib.html; 
import java.awt.*; 
import java.awt.event.ActionEvent; 
import java.awt.geom.AffineTransform; 
import java.awt.geom.FlatteningPathIterator; 
import java.awt.image.BufferedImage; 
import javax.swing.*; 
import org.jhotdraw.contrib.TextAreaFigure; 
import org.jhotdraw.figures.RectangleFigure; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.Geom; 
public  class  HTMLTextAreaFigure  extends TextAreaFigure  implements HTMLContentProducerContext, FigureChangeListener {
		public final static char START_ENTITY_CHAR = '&';

		public final static char END_ENTITY_CHAR = ';';

		public final static char ESCAPE_CHAR = '\\';

		private transient DisposableResourceHolder fImageHolder;

		private transient JLabel fDisplayDelegate;

		private boolean fUseDirectDraw = false;

		private boolean fIsImageDirty = true;

		private boolean fRawHTML = false;

		private transient ContentProducer fIntrinsicContentProducer;

		private static ContentProducerRegistry fDefaultContentProducers = new ContentProducerRegistry();

		static {	fDefaultContentProducers.registerContentProducer(TextAreaFigure.class, new TextHolderContentProducer());	fDefaultContentProducers.registerContentProducer(Color.class, new HTMLColorContentProducer());	}

		private transient ContentProducerRegistry fContentProducers = null;

		private Figure fFrameFigure = null;

		static {	initDefaultAttribute("XAlignment", new Integer(SwingConstants.LEFT));	initDefaultAttribute("YAlignment", new Integer(SwingConstants.TOP));	initDefaultAttribute("LeftMargin", new Float(5));	initDefaultAttribute("RightMargin", new Float(5));	initDefaultAttribute("TopMargin", new Float(5));	initDefaultAttribute("BottomMargin", new Float(5));	initDefaultAttribute("TabSize", new Float(8));	}

		public HTMLTextAreaFigure() {	initialize();	}

		public Object clone() {	Object cloneObject = super.clone();	((HTMLTextAreaFigure)cloneObject).initialize();	return cloneObject;	}

		public void basicDisplayBox(Point origin, Point corner) {	super.basicDisplayBox(origin, corner);	getFrameFigure().displayBox(displayBox());	}

		public HandleEnumeration handles() {	return getFrameFigure().handles();	}

		public boolean containsPoint(int x, int y) {	return getFrameFigure().containsPoint(x, y);	}

		public void moveBy(int dx, int dy) {	super.moveBy(dx, dy);	getFrameFigure().moveBy(dx, dy);	}

		protected void initialize() {	fImageHolder = DisposableResourceManagerFactory.createStandardHolder(null);	setFrameFigure(new RectangleFigure());	setIntrinsicContentProducer(new HTMLContentProducer());	fContentProducers = new ContentProducerRegistry(fDefaultContentProducers);	markSizeDirty();	markImageDirty();	markTextDirty();	markFontDirty();	setAttribute(FigureAttributeConstant.POPUP_MENU, createPopupMenu());	}

		protected void markSizeDirty() {	markImageDirty();	super.markSizeDirty();	}

		protected void markTextDirty() {	markImageDirty();	super.markTextDirty();	}

		protected void markFontDirty() {	markImageDirty();	super.markFontDirty();	}

		public void draw(Graphics g) {	Color fill = getFillColor();	g.setColor(fill);	drawBackground(g);	drawText(g, displayBox());	Color frame = getFrameColor();	g.setColor(frame);	drawFrame(g);	}

		public void drawFrame(Graphics g) {	((Graphics2D)g).draw(getClippingShape());	}

		public void drawBackground(Graphics g) {	((Graphics2D)g).fill(getClippingShape());	}

		protected float drawText(Graphics g, Rectangle displayBox) {	Graphics2D g2 = null;	Shape savedClip = null;	if (g != null) {	g2 = (Graphics2D)g;	savedClip = g2.getClip();	}	Rectangle drawingBox = makeDrawingBox(displayBox);	if (drawingBox.isEmpty()) {	return drawingBox.height;	}	if (g != null) {	g2.clip(getClippingShape());	}	if (usesDirectDraw()) {	drawTextDirect(g2, drawingBox);	}	else {	fImageHolder.lock();	if (isImageDirty()) {	generateImage(drawingBox);	setSizeDirty(false);	}	if (g2 != null) {	g2.drawImage(getImage(), drawingBox.x, drawingBox.y, null);	}	fImageHolder.unlock();	}	if (g != null) {	g2.setClip(savedClip);	}	drawFrame(g);	return displayBox.height;	}

		protected void generateImage(Rectangle drawingBox) {	createImage(drawingBox.width, drawingBox.height);	Graphics2D g2 = (Graphics2D)getImage().getGraphics();	Rectangle finalBox = new Rectangle(drawingBox);	finalBox.setLocation(0, 0);	renderText(g2, finalBox);	g2.dispose();	}

		protected void drawTextDirect(Graphics2D g2, Rectangle drawingBox) {	Shape savedClipArea = null;	Color savedFontColor = null;	RenderingHints savedRenderingHints = null;	if (g2 != null) {	savedRenderingHints = g2.getRenderingHints();	savedClipArea = g2.getClip();	savedFontColor = g2.getColor();	g2.clip(drawingBox);	}	if (g2 != null) {	g2.setClip(savedClipArea);	g2.setColor(savedFontColor);	g2.setRenderingHints(savedRenderingHints);	}	}

		protected float renderText(Graphics2D g2, Rectangle drawingBox) {	g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,	RenderingHints.VALUE_ANTIALIAS_ON);	g2.setRenderingHint(RenderingHints.KEY_RENDERING,	RenderingHints.VALUE_RENDER_QUALITY);	g2.setBackground(getFillColor());	g2.setColor(getFillColor());	g2.clearRect(drawingBox.x, drawingBox.y, drawingBox.width, drawingBox.height);	g2.fillRect(drawingBox.x, drawingBox.y, drawingBox.width, drawingBox.height);	String text;	if (isRawHTML()) {	text = getText();	}	else {	text = getHTMLText(getText(), getFont(),	(String)getContentProducer(Color.class).getContent(this, FigureAttributeConstant.TEXT_COLOR_STR, getTextColor()),	(String)getContentProducer(Color.class).getContent(this, FigureAttributeConstant.FILL_COLOR_STR, getFillColor()),	drawingBox	);	}	text = substituteEntityKeywords(text);	JLabel displayDelegate = getDisplayDelegate();	displayDelegate.setText(text);	displayDelegate.setBackground(getFillColor());	displayDelegate.setLocation(0, 0);	displayDelegate.setSize(drawingBox.width, drawingBox.height);	displayDelegate.setHorizontalAlignment(((Integer)getAttribute(FigureAttributeConstant.XALIGNMENT)).intValue());	displayDelegate.setVerticalAlignment(((Integer)getAttribute(FigureAttributeConstant.YALIGNMENT)).intValue());	SwingUtilities.paintComponent(	g2,	displayDelegate,	getContainerPanel(displayDelegate, drawingBox),	drawingBox.x,	drawingBox.y,	drawingBox.width,	drawingBox.height);	return drawingBox.height;	}

		protected Rectangle makeDrawingBox(Rectangle displayBox) {	float leftMargin = ((Float)getAttribute(FigureAttributeConstant.LEFT_MARGIN)).floatValue();	float rightMargin = ((Float)getAttribute(FigureAttributeConstant.RIGHT_MARGIN)).floatValue();	float topMargin = ((Float)getAttribute(FigureAttributeConstant.TOP_MARGIN)).floatValue();	float bottomMargin = ((Float)getAttribute(FigureAttributeConstant.BOTTOM_MARGIN)).floatValue();	Rectangle drawingBox = new Rectangle(displayBox);	drawingBox.grow(-1, -1);	drawingBox.x += leftMargin;	drawingBox.width -= (leftMargin + rightMargin);	drawingBox.y += topMargin;	drawingBox.height -= topMargin + bottomMargin;	return drawingBox;	}

		protected JLabel getDisplayDelegate() {	if (fDisplayDelegate == null) {	fDisplayDelegate = new JLabel();	fDisplayDelegate.setBorder(null);	}	return fDisplayDelegate;	}

		protected void createImage(int width, int height) {	fImageHolder.lock();	if (!fImageHolder.isAvailable() ||	((BufferedImage)fImageHolder.getResource()).getWidth() != width ||	((BufferedImage)fImageHolder.getResource()).getHeight() != height) {	fImageHolder.setResource(new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB));	}	}

		protected JPanel getContainerPanel(Component drawingDelegate, Rectangle displayBox) {	JPanel panel = new JPanel();	return panel;	}

		protected String getHTMLText(String text, Font font, String textColor,	String backColor, Rectangle displayBox) {	StringBuffer htmlText = new StringBuffer();	htmlText.append("<html>");	htmlText.append(	"<table border='0' width='" +	displayBox.width +	"' height='" + displayBox.height +	"' cellpadding='0' cellspacing='0'" +	"bgcolor='&FillColor;'>");	htmlText.append("<tr><td width='100%'>");	htmlText.append("<font face='&FontName;' color='&TextColor;' size='&FontSize;'>");	if (((Integer)getAttribute(FigureAttributeConstant.XALIGNMENT)).intValue() == SwingConstants.CENTER) {	htmlText.append("<center>");	}	if (font.isItalic()) {	htmlText.append("<i>");	}	if (font.isBold()) {	htmlText.append("<b>");	}	htmlText.append(text);	if (font.isBold()) {	htmlText.append("</b>");	}	if (font.isItalic()) {	htmlText.append("</i>");	}	if (((Integer)getAttribute(FigureAttributeConstant.XALIGNMENT)).intValue() == SwingConstants.CENTER) {	htmlText.append("</center>");	}	htmlText.append("</font>");	htmlText.append("</td></tr></table>");	htmlText.append("</html>");	return htmlText.toString();	}

		protected String substituteEntityKeywords(String template) {	int endPos;	StringBuffer finalText = new StringBuffer();	int startPos = 0;	int chunkEnd = startPos;	try {	while ((startPos = template.indexOf(START_ENTITY_CHAR, startPos)) != -1) {	if (startPos != 0 && template.charAt(startPos - 1) == ESCAPE_CHAR) {	startPos++;	continue;	}	endPos = startPos + 1;	while ((endPos = template.indexOf(END_ENTITY_CHAR, endPos)) != -1) {	if (endPos == 0 || template.charAt(endPos - 1) != ESCAPE_CHAR) {	break;	}	throw new InvalidAttributeMarker();	}	String attrName = template.substring(startPos + 1, endPos);	String attrValue = getEntityHTMLRepresentation(attrName);	if (attrValue != null) {	finalText.append(template.substring(chunkEnd, startPos));	finalText.append(substituteEntityKeywords(attrValue));	startPos = endPos + 1;	chunkEnd = startPos;	}	else {	startPos++;	}	}	}	catch (InvalidAttributeMarker ex) {	}	finalText.append(template.substring(chunkEnd));	return finalText.toString();	}

		protected String getEntityHTMLRepresentation(String attrName) {	Object attrValue = getIntrinsicContentProducer().getContent(this, attrName, null);	if (attrValue == null) {	return null;	}	while (attrValue != null && !(attrValue instanceof String)) {	if (attrValue instanceof ContentProducer) {	attrValue = ((ContentProducer)attrValue).getContent(this, attrName, attrValue);	continue;	}	ContentProducer defaultProducer = getContentProducer(attrValue.getClass());	if (defaultProducer != null) {	attrValue = defaultProducer.getContent(this, attrName, attrValue);	continue;	}	attrValue = attrValue.toString();	}	return (String)attrValue;	}

		protected BufferedImage getImage() {	if (fImageHolder.isAvailable()) {	return (BufferedImage)fImageHolder.getResource();	}	return null;	}

		protected void setImage(BufferedImage newImage) {	fImageHolder.setResource(newImage);	}

		protected JPopupMenu createPopupMenu() {	JPopupMenu popupMenu = new JPopupMenu();	addPopupMenuItems(popupMenu);	popupMenu.setLightWeightPopupEnabled(true);	return popupMenu;	}

		protected void addPopupMenuItems(JPopupMenu popupMenu) {	ButtonGroup drawingPopupGroup;	JRadioButtonMenuItem rbOption;	drawingPopupGroup = new ButtonGroup();	rbOption = new JRadioButtonMenuItem(	new AbstractAction("Direct drawing") {	public void actionPerformed(ActionEvent event) {	setUseDirectDraw(true);	}	});	drawingPopupGroup.add(rbOption);	if (usesDirectDraw()) {	drawingPopupGroup.setSelected(rbOption.getModel(), true);	}	popupMenu.add(rbOption);	rbOption = new JRadioButtonMenuItem(	new AbstractAction("Buffered drawing") {	public void actionPerformed(ActionEvent event) {	setUseDirectDraw(false);	}	});	drawingPopupGroup.add(rbOption);	if (usesBufferedDraw()) {	drawingPopupGroup.setSelected(rbOption.getModel(), true);	}	popupMenu.add(rbOption);	popupMenu.addSeparator();	drawingPopupGroup = new ButtonGroup();	rbOption = new JRadioButtonMenuItem(	new AbstractAction("Normal HTML") {	public void actionPerformed(ActionEvent event) {	setRawHTML(false);	}	});	drawingPopupGroup.add(rbOption);	drawingPopupGroup.setSelected(rbOption.getModel(), true);	popupMenu.add(rbOption);	rbOption =	new JRadioButtonMenuItem(	new AbstractAction("Raw HTML") {	public void actionPerformed(ActionEvent event) {	setRawHTML(true);	}	});	drawingPopupGroup.add(rbOption);	popupMenu.add(rbOption);	}

		public boolean usesDirectDraw() {	return fUseDirectDraw;	}

		public void setUseDirectDraw(boolean newUseDirectDraw) {	fUseDirectDraw = newUseDirectDraw;	setAttribute(FigureAttributeConstant.POPUP_MENU, createPopupMenu());	markSizeDirty();	}

		public void setUseBufferedDraw(boolean newUseBufferedDraw) {	setUseDirectDraw(!newUseBufferedDraw);	}

		public boolean usesBufferedDraw() {	return !usesDirectDraw();	}

		protected void markImageDirty() {	fImageHolder.dispose();	}

		protected boolean isImageDirty() {	return !fImageHolder.isAvailable();	}

		public void setAttribute(FigureAttributeConstant name, Object value) {	super.setAttribute(name, value);	markImageDirty();	}

		public boolean isRawHTML() {	return fRawHTML;	}

		public void setRawHTML(boolean newRawHTML) {	fRawHTML = newRawHTML;	setAttribute(FigureAttributeConstant.POPUP_MENU, createPopupMenu());	}

		protected ContentProducer getIntrinsicContentProducer() {	return fIntrinsicContentProducer;	}

		public void setIntrinsicContentProducer(ContentProducer newIntrinsicContentProducer) {	fIntrinsicContentProducer = newIntrinsicContentProducer;	}

		public ContentProducer registerContentProducer(Class targetClass, ContentProducer producer) {	return fContentProducers.registerContentProducer(targetClass, producer);	}

		public void unregisterContentProducer(Class targetClass, ContentProducer producer) {	fContentProducers.unregisterContentProducer(targetClass, producer);	}

		protected ContentProducer getContentProducer(Class targetClass) {	return fContentProducers.getContentProducer(targetClass);	}

		public Polygon getPolygon() {	Polygon polygon = new Polygon();	AffineTransform at = AffineTransform.getScaleInstance(1, 1);	FlatteningPathIterator pIter = new FlatteningPathIterator(	getClippingShape().getPathIterator(at),	1);	double[] coords = new double[6];	while (!pIter.isDone()) {	pIter.currentSegment(coords);	polygon.addPoint((int)coords[0], (int)coords[1]);	pIter.next();	}	return polygon;	}

		protected Figure getFrameFigure() {	return fFrameFigure;	}

		public void setFrameFigure(Figure newFrameFigure) {	if (fFrameFigure != null) {	fFrameFigure.removeFigureChangeListener(this);	}	fFrameFigure = newFrameFigure;	fFrameFigure.addFigureChangeListener(this);	}

		protected Shape getClippingShape() {	Figure frame = getFrameFigure();	if (frame instanceof GeometricFigure) {	return ((GeometricFigure)frame).getShape();	}	return frame.displayBox();	}

		public void figureInvalidated(FigureChangeEvent e) { }

		public void figureChanged(FigureChangeEvent e) {	willChange();	super.basicDisplayBox(e.getFigure().displayBox().getLocation(), Geom.corner(e.getFigure().displayBox()));	changed();	}

		public void figureRemoved(FigureChangeEvent e) { }

		public void figureRequestRemove(FigureChangeEvent e) { }

		public void figureRequestUpdate(FigureChangeEvent e) { }

		private  class  InvalidAttributeMarker  extends Exception {

	}

		public Figure getRepresentingFigure() {	return this;	}


}
