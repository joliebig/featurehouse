
package org.jhotdraw.contrib; 
import javax.swing.*; 
import java.awt.*; 
import org.jhotdraw.application.*; 
import org.jhotdraw.framework.DrawingView; 
public  class  JPanelDesktop  extends JPanel  implements Desktop {
		private DesktopEventService myDesktopEventService;

		private DrawApplication myDrawApplication;

	 public JPanelDesktop(DrawApplication newDrawApplication) {	setDrawApplication(newDrawApplication);	setDesktopEventService(createDesktopEventService()); setAlignmentX(LEFT_ALIGNMENT);	setLayout(new BorderLayout()); }

		protected Component createContents(DrawingView dv) {	JScrollPane sp = new JScrollPane((Component)dv);	sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);	sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);	sp.setAlignmentX(LEFT_ALIGNMENT);	String applicationTitle;	if (dv.drawing().getTitle() == null) {	applicationTitle = getDrawApplication().getApplicationName()	+ " - " + getDrawApplication().getDefaultDrawingTitle();	}	else {	applicationTitle = getDrawApplication().getApplicationName() + " - " + dv.drawing().getTitle();	}	sp.setName(applicationTitle);	return sp;	}

		public DrawingView getActiveDrawingView() {	return getDesktopEventService().getActiveDrawingView();	}

		public void addToDesktop(DrawingView dv, int location) {	getDesktopEventService().addComponent(createContents(dv));	getContainer().validate();	}

		public void removeFromDesktop(DrawingView dv, int location) {	getDesktopEventService().removeComponent(dv);	getContainer().validate();	}

		public void removeAllFromDesktop(int location) { getDesktopEventService().removeAllComponents();	getContainer().validate();	}

		public DrawingView[] getAllFromDesktop(int location) {	return getDesktopEventService().getDrawingViews(getComponents());	}

		public void addDesktopListener(DesktopListener dpl) {	getDesktopEventService().addDesktopListener(dpl);	}

		public void removeDesktopListener(DesktopListener dpl) { getDesktopEventService().removeDesktopListener(dpl);	}

		private Container getContainer() {	return this;	}

		protected DesktopEventService getDesktopEventService() {	return myDesktopEventService;	}

		private void setDesktopEventService(DesktopEventService newDesktopEventService) {	myDesktopEventService = newDesktopEventService;	}

		protected DesktopEventService createDesktopEventService() {	return new DesktopEventService(this, getContainer());	}

		private void setDrawApplication(DrawApplication newDrawApplication) {	myDrawApplication = newDrawApplication;	}

		protected DrawApplication getDrawApplication() {	return myDrawApplication;	}

		public void updateTitle(String newDrawingTitle) {	setName(newDrawingTitle);	}


}
