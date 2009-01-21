
package org.jhotdraw.contrib; 
import javax.swing.*; 
import java.awt.*; 
import org.jhotdraw.framework.DrawingView; 
import java.beans.PropertyChangeListener; 
import java.beans.PropertyChangeEvent; 
public  class  SplitPaneDesktop  extends JSplitPane  implements Desktop {
		private DesktopEventService myDesktopEventService;

	 public SplitPaneDesktop() {	setDesktopEventService(createDesktopEventService());	setAlignmentX(JSplitPane.LEFT_ALIGNMENT);	setOneTouchExpandable(true);	addPropertyChangeListener(createPropertyChangeListener()); }

		protected PropertyChangeListener createPropertyChangeListener() {	return new PropertyChangeListener() {	public void propertyChange(PropertyChangeEvent evt) {	if (getRightComponent() != null) {	getRightComponent().repaint();	}	if (getLeftComponent() != null) {	getLeftComponent().repaint();	}	}	};	}

		protected Component createContents(DrawingView dv, int location) {	setRightComponent(createRightComponent(dv));	setLeftComponent(createLeftComponent(dv)); switch (location) { case Desktop.PRIMARY: { return getLeftComponent();	}	case Desktop.SECONDARY: {	return getRightComponent();	}	default: { return null;	} }	}

		protected Component createRightComponent(DrawingView dv) {	JScrollPane sp = new JScrollPane((Component)dv);	sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);	sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);	sp.setAlignmentX(LEFT_ALIGNMENT);	return sp;	}

		protected Component createLeftComponent(DrawingView dv) {	return new JScrollPane(new JList());	}

		public DrawingView getActiveDrawingView() {	return getDesktopEventService().getActiveDrawingView();	}

		public void addToDesktop(DrawingView dv, int location) {	createContents(dv, Desktop.PRIMARY);	setDividerLocation(getInitDividerLocation());	}

		protected int getInitDividerLocation() {	return 150;	}

		public void removeFromDesktop(DrawingView dv, int location) {	Component[] comps = getContainer().getComponents();	for (int x = 0; x < comps.length; x++) {	if (dv == Helper.getDrawingView(comps[x])) {	getContainer().remove(comps[x]); break;	}	}	}

		public void removeAllFromDesktop(int location) { getContainer().removeAll();	}

		public DrawingView[] getAllFromDesktop(int location) {	return getDesktopEventService().getDrawingViews(getComponents());	}

		public void addDesktopListener(DesktopListener dpl) {	getDesktopEventService().addDesktopListener(dpl);	}

		public void removeDesktopListener(DesktopListener dpl) {	getDesktopEventService().removeDesktopListener(dpl);	}

		private Container getContainer() {	return this;	}

		protected DesktopEventService getDesktopEventService() {	return myDesktopEventService;	}

		private void setDesktopEventService(DesktopEventService newDesktopEventService) {	myDesktopEventService = newDesktopEventService;	}

		protected DesktopEventService createDesktopEventService() {	return new DesktopEventService(this, getContainer());	}

		public void updateTitle(String newDrawingTitle) {	setName(newDrawingTitle);	}


}
