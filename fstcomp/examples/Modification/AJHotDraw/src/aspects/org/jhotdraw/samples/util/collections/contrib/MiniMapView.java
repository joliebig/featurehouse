
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.DrawingChangeEvent; 
import org.jhotdraw.framework.DrawingChangeListener; 
import java.awt.*; 
import java.awt.event.*; 
import java.awt.geom.*; 
import javax.swing.*; 
import javax.swing.event.*; 
public  class  MiniMapView  extends JComponent {
		private JScrollPane m_subject;

		private DrawingView myMappedDrawingView;

		private SubjectListener m_subjectListener;

		private DrawingChangeListener myDrawingChangeListener;

		private Color m_viewBoxColor = Color.red;

		public MiniMapView(DrawingView newMappedDrawingView, JScrollPane subject) {	m_subjectListener = new SubjectListener();	setSubject(subject);	setMappedDrawingView(newMappedDrawingView);	myDrawingChangeListener = new MappedDrawingChangeListener();	getMappedDrawingView().drawing().addDrawingChangeListener(myDrawingChangeListener);	MouseListener ml = new MouseListener();	addMouseListener(new MouseListener());	addMouseMotionListener(ml);	}

		protected void setMappedDrawingView(DrawingView newMappedDrawingView) {	myMappedDrawingView = newMappedDrawingView;	}

		public DrawingView getMappedDrawingView() {	return myMappedDrawingView;	}

		protected void setSubject(JScrollPane subject) {	if (m_subject != null) {	m_subject.getViewport().removeChangeListener(m_subjectListener);	}	m_subject = subject;	if (m_subject != null) {	m_subject.getViewport().addChangeListener(m_subjectListener);	}	repaint();	}

		public JScrollPane getSubject() {	return m_subject;	}

		public Color getViewBowColor() {	return m_viewBoxColor;	}

		public void setViewBoxColor(Color c) {	m_viewBoxColor = c;	repaint();	}

		protected Component getMappedComponent() {	return (Component)getMappedDrawingView();	}

		public void paint(Graphics g) {	Graphics2D g2d = (Graphics2D)g;	Component mappedComponent = getMappedComponent();	AffineTransform at = getViewToMiniMapTransform(mappedComponent);	g2d.transform(at);	getMappedDrawingView().drawAll(g2d);	drawViewRectangle(g2d, getViewRectangle());	}

		protected void drawViewRectangle(Graphics2D g2d, Rectangle viewPortRectangle) {	AffineTransform at = new AffineTransform();	at.setToIdentity();	g2d.setTransform(at);	g2d.setColor(m_viewBoxColor);	g2d.draw(viewPortRectangle);	}

		protected AffineTransform getViewToMiniMapTransform(Component mappedComponent) {	double scaleX = ((double)getWidth()) / ((double)mappedComponent.getWidth());	double scaleY = ((double)getHeight()) / ((double)mappedComponent.getHeight());	AffineTransform at = getInverseSubjectTransform();	at.concatenate(AffineTransform.getScaleInstance(scaleX, scaleY));	return at;	}

		protected AffineTransform getInverseSubjectTransform() {	AffineTransform at = new AffineTransform();	at.setToIdentity();	return at;	}

		protected Rectangle getViewRectangle() {	Rectangle visiblePortion = m_subject.getViewportBorderBounds();	Point upperLeftViewPos = m_subject.getViewport().getViewPosition();	double [] srcRecCorners = new double[4];	double [] dstRecCorners = new double[4];	srcRecCorners[0] = upperLeftViewPos.x + visiblePortion.getX(); srcRecCorners[1] = upperLeftViewPos.y + visiblePortion.getY(); srcRecCorners[2] = upperLeftViewPos.x + visiblePortion.getX() + visiblePortion.getWidth(); srcRecCorners[3] = upperLeftViewPos.y + visiblePortion.getY() + visiblePortion.getHeight();	getViewToMiniMapTransform(getMappedComponent()).transform(srcRecCorners, 0, dstRecCorners, 0, srcRecCorners.length/2);	return new Rectangle((int)dstRecCorners[0], (int)dstRecCorners[1], (int)(dstRecCorners[2] - dstRecCorners[0]), (int)(dstRecCorners[3] - dstRecCorners[1]));	}

		protected void scrollSubjectTo(int upperLeftX, int upperLeftY) {	AffineTransform at = null;	try {	at = getViewToMiniMapTransform(getMappedComponent()).createInverse();	}	catch (NoninvertibleTransformException nite) {	nite.printStackTrace();	return;	}	double [] srcPoints = new double[2];	double [] destPoints = new double[2];	srcPoints[0] = upperLeftX;	srcPoints[1] = upperLeftY;	at.transform(srcPoints, 0, destPoints, 0, 1);	if (destPoints[0] < 0) {	destPoints[0] = 0;	}	if (destPoints[1] < 0) {	destPoints[1] = 0;	}	m_subject.getViewport().setViewPosition(new Point((int)destPoints[0], (int)destPoints[1]));	}

		protected int [] getUpperLeftPointsFromCenter(int centerX, int centerY) {	int [] upperLeft = new int[2];	Rectangle oldRectangle = getViewRectangle();	upperLeft[0] = centerX - oldRectangle.width/2;	upperLeft[1] = centerY - oldRectangle.height/2; if (upperLeft[0] + oldRectangle.width > getX() + getWidth()) { upperLeft[0] = getX() + getWidth() - oldRectangle.width; } if (upperLeft[1] + oldRectangle.height > getY() + getHeight()) { upperLeft[1] = getY() + getHeight() - oldRectangle.height; }	return upperLeft;	}

		public  class  MouseListener  extends MouseAdapter  implements MouseMotionListener {
			public void mousePressed(MouseEvent e) {	int [] rectangleUpperLeft = getUpperLeftPointsFromCenter(e.getX(), e.getY());	scrollSubjectTo(rectangleUpperLeft[0], rectangleUpperLeft[1]);	}

			public void mouseDragged(MouseEvent e) {	int [] rectangleUpperLeft = getUpperLeftPointsFromCenter(e.getX(), e.getY());	scrollSubjectTo(rectangleUpperLeft[0], rectangleUpperLeft[1]);	}

			public void mouseMoved(MouseEvent e) {	}


	}

		 	class  SubjectListener  implements ChangeListener {
			public void stateChanged(ChangeEvent e) {	repaint();	}


	}

		 	class  MappedDrawingChangeListener  implements DrawingChangeListener {
			public void drawingInvalidated(DrawingChangeEvent e) {	repaint();	}

			public void drawingRequestUpdate(DrawingChangeEvent e) {	repaint();	}

		 public void drawingTitleChanged(DrawingChangeEvent e) { }


	}


}
