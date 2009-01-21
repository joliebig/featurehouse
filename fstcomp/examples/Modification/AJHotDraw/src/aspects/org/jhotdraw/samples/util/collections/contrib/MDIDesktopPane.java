
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.standard.NullDrawingView; 
import org.jhotdraw.application.DrawApplication; 
import javax.swing.*; 
import javax.swing.event.InternalFrameListener; 
import javax.swing.event.InternalFrameAdapter; 
import javax.swing.event.InternalFrameEvent; 
import java.awt.*; 
import java.beans.*; 
public  class  MDIDesktopPane  extends JDesktopPane  implements Desktop {
		private static int FRAME_OFFSET=20;

		private MDIDesktopManager manager;

		private DrawApplication myDrawApplication;

		private DesktopEventService myDesktopEventService;

		private DrawingView selectedView;

		public MDIDesktopPane(DrawApplication newDrawApplication) {	setDesktopEventService(createDesktopEventService());	setDrawApplication(newDrawApplication);	manager=new MDIDesktopManager(this);	setDesktopManager(manager);	setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);	setAlignmentX(JComponent.LEFT_ALIGNMENT);	}

		protected InternalFrameListener internalFrameListener = new InternalFrameAdapter() { public void internalFrameOpened(InternalFrameEvent e) {	DrawingView dv = Helper.getDrawingView(e.getInternalFrame());	fireDrawingViewAddedEvent(dv); }	public void internalFrameClosed(InternalFrameEvent e) {	DrawingView dv = Helper.getDrawingView(e.getInternalFrame());	if (getComponentCount() == 0){	DrawingView oldView = getActiveDrawingView();	setActiveDrawingView(NullDrawingView.getManagedDrawingView(oldView.editor()));	fireDrawingViewSelectedEvent(oldView, getActiveDrawingView());	}	fireDrawingViewRemovedEvent(dv);	}	public void internalFrameActivated(InternalFrameEvent e) {	DrawingView dv = Helper.getDrawingView(e.getInternalFrame());	DrawingView oldView = getActiveDrawingView();	setActiveDrawingView(dv);	fireDrawingViewSelectedEvent(oldView, getActiveDrawingView());	}	};

		protected void fireDrawingViewAddedEvent(final DrawingView dv) {	getDesktopEventService().fireDrawingViewAddedEvent(dv);	}

		protected void fireDrawingViewRemovedEvent(final DrawingView dv) {	getDesktopEventService().fireDrawingViewRemovedEvent(dv);	}

		protected void fireDrawingViewSelectedEvent(final DrawingView oldView, final DrawingView newView) {	getDesktopEventService().fireDrawingViewSelectedEvent(oldView, newView);	}

		protected Component createContents(DrawingView dv) {	JScrollPane sp = new JScrollPane((Component) dv);	sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);	sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);	sp.setAlignmentX(LEFT_ALIGNMENT);	String applicationTitle;	if (dv.drawing().getTitle() == null) {	applicationTitle = getDrawApplication().getApplicationName() + " - " + getDrawApplication().getDefaultDrawingTitle();	}	else {	applicationTitle = getDrawApplication().getApplicationName() + " - " + dv.drawing().getTitle();	}	JInternalFrame internalFrame = new JInternalFrame(applicationTitle, true, true, true, true);	internalFrame.setName(applicationTitle);	internalFrame.getContentPane().add(sp);	internalFrame.setSize(200,200);	return internalFrame;	}

		public DrawingView getActiveDrawingView() {	return selectedView;	}

		protected void setActiveDrawingView(DrawingView newSelectedView) {	selectedView = newSelectedView;	}

		public void updateTitle(String newDrawingTitle) {	getSelectedFrame().setTitle(newDrawingTitle);	}

		public void addToDesktop(DrawingView dv, int location) {	JInternalFrame frame = (JInternalFrame)createContents(dv);	JInternalFrame[] array = getAllFrames();	Point p = null;	int w;	int h;	frame.addInternalFrameListener(internalFrameListener);	super.add(frame);	checkDesktopSize();	if (array.length > 0) {	p = array[0].getLocation();	p.x = p.x + FRAME_OFFSET;	p.y = p.y + FRAME_OFFSET;	}	else {	p = new Point(0, 0);	}	frame.setLocation(p.x, p.y);	if (frame.isResizable()) {	w = getWidth() - (getWidth() / 3);	h = getHeight() - (getHeight() / 3);	if (w < frame.getMinimumSize().getWidth()) {	w = (int)frame.getMinimumSize().getWidth();	}	if (h < frame.getMinimumSize().getHeight()) {	h = (int)frame.getMinimumSize().getHeight();	}	frame.setSize(w, h);	}	moveToFront(frame);	frame.setVisible(true);	try {	frame.setSelected(true);	}	catch (PropertyVetoException e) {	frame.toBack();	}	}

		public void removeFromDesktop(DrawingView dv, int location) {	Component[] comps = getComponents();	for (int x=0; x<comps.length; x++) {	if (dv == Helper.getDrawingView(comps[x])) {	((JInternalFrame)comps[x]).dispose(); break;	}	}	checkDesktopSize();	}

		public void removeAllFromDesktop(int location) {	JInternalFrame[] jifs = getAllFrames();	for (int x=0; x < jifs.length; x++) {	jifs[x].dispose();	}	}

		public DrawingView[] getAllFromDesktop(int location){	Component[] comps = getComponents();	java.util.ArrayList al = new java.util.ArrayList();	for (int x=0; x<comps.length; x++) {	DrawingView dv = Helper.getDrawingView(comps[x]);	if (dv != null) {	al.add(dv);	}	}	DrawingView[] dvs = new DrawingView[al.size()];	al.toArray(dvs);	return dvs;	}

		protected DesktopEventService getDesktopEventService() {	return myDesktopEventService;	}

		private void setDesktopEventService(DesktopEventService newDesktopEventService) {	myDesktopEventService = newDesktopEventService;	}

		protected DesktopEventService createDesktopEventService() {	return new DesktopEventService(this, this);	}

		public void addDesktopListener(DesktopListener dpl) {	getDesktopEventService().addDesktopListener(dpl);	}

		public void removeDesktopListener(DesktopListener dpl) {	getDesktopEventService().removeDesktopListener(dpl);	}

		public void cascadeFrames() {	int x = 0;	int y = 0;	JInternalFrame[] allFrames = getAllFrames();	if (allFrames.length == 0) {	return;	}	manager.setNormalSize();	int frameHeight = (getBounds().height - 5) - allFrames.length * FRAME_OFFSET;	int frameWidth = (getBounds().width - 5) - allFrames.length * FRAME_OFFSET;	for (int i = allFrames.length - 1; i >= 0; i--) {	try {	allFrames[i].setMaximum(false);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	allFrames[i].setBounds(x, y, frameWidth, frameHeight);	x = x + FRAME_OFFSET;	y = y + FRAME_OFFSET;	}	checkDesktopSize();	}

		public void tileFrames() {	tileFramesHorizontally();	}

		public void tileFramesHorizontally() {	Component[] allFrames = getAllFrames();	if (allFrames.length == 0) {	return;	}	manager.setNormalSize();	int frameHeight = getBounds().height/allFrames.length;	int y = 0;	for (int i = 0; i < allFrames.length; i++) {	try {	((JInternalFrame)allFrames[i]).setMaximum(false);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	allFrames[i].setBounds(0, y, getBounds().width,frameHeight);	y = y + frameHeight;	}	checkDesktopSize();	}

		public void tileFramesVertically() {	Component[] allFrames = getAllFrames();	if (allFrames.length == 0) {	return;	}	manager.setNormalSize();	int frameWidth = getBounds().width/allFrames.length;	int x = 0;	for (int i = 0; i < allFrames.length; i++) {	try {	((JInternalFrame)allFrames[i]).setMaximum(false);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	allFrames[i].setBounds(x, 0, frameWidth, getBounds().height);	x = x + frameWidth;	}	checkDesktopSize();	}

		public void arrangeFramesVertically() {	Component[] allFrames = getAllFrames();	if (allFrames.length == 0) {	return;	}	manager.setNormalSize();	int vertFrames = (int)Math.floor(Math.sqrt(allFrames.length));	int horFrames = (int)Math.ceil(Math.sqrt(allFrames.length));	int frameWidth = getBounds().width / horFrames;	int frameHeight = getBounds().height / vertFrames;	int x = 0;	int y = 0;	int frameIdx = 0;	for (int horCnt = 0; horCnt < horFrames-1; horCnt++) {	y = 0;	for (int vertCnt = 0; vertCnt < vertFrames; vertCnt++) {	try {	((JInternalFrame)allFrames[frameIdx]).setMaximum(false);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	allFrames[frameIdx].setBounds(x, y, frameWidth, frameHeight);	frameIdx++;	y = y + frameHeight;	}	x = x + frameWidth;	}	frameHeight = getBounds().height / (allFrames.length - frameIdx);	y = 0;	for (; frameIdx < allFrames.length; frameIdx++)	{	try {	((JInternalFrame)allFrames[frameIdx]).setMaximum(false);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	allFrames[frameIdx].setBounds(x, y, frameWidth, frameHeight);	y = y + frameHeight;	}	checkDesktopSize();	}

		public void arrangeFramesHorizontally() {	Component[] allFrames = getAllFrames();	if (allFrames.length == 0) {	return;	}	manager.setNormalSize();	int vertFrames = (int)Math.ceil(Math.sqrt(allFrames.length));	int horFrames = (int)Math.floor(Math.sqrt(allFrames.length));	int frameWidth = getBounds().width / horFrames;	int frameHeight = getBounds().height / vertFrames;	int x = 0;	int y = 0;	int frameIdx = 0;	for (int vertCnt = 0; vertCnt < vertFrames-1; vertCnt++) {	x = 0;	for (int horCnt = 0; horCnt < horFrames; horCnt++) {	try {	((JInternalFrame)allFrames[frameIdx]).setMaximum(false);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	allFrames[frameIdx].setBounds(x, y, frameWidth, frameHeight);	frameIdx++;	x = x + frameWidth;	}	y = y + frameHeight;	}	frameWidth = getBounds().width / (allFrames.length - frameIdx);	x = 0;	for (; frameIdx < allFrames.length; frameIdx++) {	try {	((JInternalFrame)allFrames[frameIdx]).setMaximum(false);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	allFrames[frameIdx].setBounds(x, y, frameWidth, frameHeight);	x = x + frameWidth;	}	checkDesktopSize();	}

		public void setAllSize(Dimension d) {	setMinimumSize(d);	setMaximumSize(d);	setPreferredSize(d);	setBounds(0, 0, d.width, d.height);	}

		public void setAllSize(int width, int height) {	setAllSize(new Dimension(width,height));	}

		private void checkDesktopSize() {	if ((getParent() != null) && isVisible()) {	manager.resizeDesktop();	}	}

		private void setDrawApplication(DrawApplication newDrawApplication) {	myDrawApplication = newDrawApplication;	}

		protected DrawApplication getDrawApplication() {	return myDrawApplication;	}


} 
 
class  MDIDesktopManager  extends DefaultDesktopManager {
		private MDIDesktopPane desktop;

		public MDIDesktopManager(MDIDesktopPane newDesktop) {	this.desktop = newDesktop;	}

		public void endResizingFrame(JComponent f) {	super.endResizingFrame(f);	resizeDesktop();	}

		public void endDraggingFrame(JComponent f) {	super.endDraggingFrame(f);	resizeDesktop();	}

		public void setNormalSize() {	JScrollPane scrollPane = getScrollPane();	Insets scrollInsets = getScrollPaneInsets();	if (scrollPane != null) {	Dimension d = scrollPane.getVisibleRect().getSize();	if (scrollPane.getBorder() != null) { d.setSize(d.getWidth() - scrollInsets.left - scrollInsets.right, d.getHeight() - scrollInsets.top - scrollInsets.bottom);	}	d.setSize(d.getWidth() - 20, d.getHeight() - 20);	desktop.setAllSize(d);	scrollPane.invalidate();	scrollPane.validate();	}	}

		private Insets getScrollPaneInsets() {	JScrollPane scrollPane = getScrollPane();	if ((scrollPane == null) || (getScrollPane().getBorder() == null)) {	return new Insets(0, 0, 0, 0);	}	else {	return getScrollPane().getBorder().getBorderInsets(scrollPane);	}	}

		public JScrollPane getScrollPane() {	if (desktop.getParent() instanceof JViewport) {	JViewport viewPort = (JViewport)desktop.getParent();	if (viewPort.getParent() instanceof JScrollPane)	return (JScrollPane)viewPort.getParent();	}	return null;	}

		protected void resizeDesktop() {	int x = 0;	int y = 0;	JScrollPane scrollPane = getScrollPane();	Insets scrollInsets = getScrollPaneInsets();	if (scrollPane != null) {	JInternalFrame allFrames[] = desktop.getAllFrames();	for (int i = 0; i < allFrames.length; i++) {	if (allFrames[i].getX() + allFrames[i].getWidth() > x) {	x = allFrames[i].getX() + allFrames[i].getWidth();	}	if (allFrames[i].getY() + allFrames[i].getHeight() > y) {	y = allFrames[i].getY() + allFrames[i].getHeight();	}	}	Dimension d=scrollPane.getVisibleRect().getSize();	if (scrollPane.getBorder() != null) { d.setSize(d.getWidth() - scrollInsets.left - scrollInsets.right, d.getHeight() - scrollInsets.top - scrollInsets.bottom);	}	if (x <= d.getWidth()) {	x = ((int)d.getWidth()) - 20;	}	if (y <= d.getHeight()) {	y = ((int)d.getHeight()) - 20;	}	desktop.setAllSize(x,y);	scrollPane.invalidate();	scrollPane.validate();	}	}


}
