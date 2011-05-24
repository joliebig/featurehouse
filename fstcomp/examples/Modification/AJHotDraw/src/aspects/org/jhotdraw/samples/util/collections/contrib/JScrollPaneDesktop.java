
package org.jhotdraw.contrib; 
import java.awt.Component; 
import java.awt.Container; // JUnitDoclet begin import
import javax.swing.JScrollPane; 
import org.jhotdraw.framework.DrawingView; 
public  class  JScrollPaneDesktop  extends JScrollPane  implements Desktop {
		private DesktopEventService myDesktopEventService;

	 public JScrollPaneDesktop() {	setDesktopEventService(createDesktopEventService()); setAlignmentX(LEFT_ALIGNMENT);	setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);	setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS); }

		protected Component createContents(DrawingView dv) {	return (Component)dv;	}

		public DrawingView getActiveDrawingView() {	return getDesktopEventService().getActiveDrawingView();	}

		public void addToDesktop(DrawingView dv, int location) {	getContainer().add(createContents(dv));	}

		public void removeFromDesktop(DrawingView dv, int location) {	getDesktopEventService().removeComponent(dv);	}

		public void removeAllFromDesktop(int location) {	getDesktopEventService().removeAllComponents();	}

		public DrawingView[] getAllFromDesktop(int location) {	return getDesktopEventService().getDrawingViews(getComponents());	}

		public void addDesktopListener(DesktopListener dpl) {	getDesktopEventService().addDesktopListener(dpl);	}

		public void removeDesktopListener(DesktopListener dpl) {	getDesktopEventService().removeDesktopListener(dpl);	}

		private Container getContainer() {	return getViewport();	}

		protected DesktopEventService getDesktopEventService() {	return myDesktopEventService;	}

		private void setDesktopEventService(DesktopEventService newDesktopEventService) {	myDesktopEventService = newDesktopEventService;	}

		protected DesktopEventService createDesktopEventService() {	return new DesktopEventService(this, getContainer());	}

		public void updateTitle(String newDrawingTitle) {	setName(newDrawingTitle);	}


}
