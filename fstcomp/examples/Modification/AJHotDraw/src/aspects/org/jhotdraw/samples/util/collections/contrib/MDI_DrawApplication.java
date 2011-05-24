
package org.jhotdraw.contrib; 
import org.jhotdraw.application.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.contrib.dnd.DragNDropTool; 
import javax.swing.*; 
public  class  MDI_DrawApplication  extends DrawApplication {
		public MDI_DrawApplication() {	this("AJHotDraw");	}

		public MDI_DrawApplication(String title) {	super(title);	}

		protected DrawApplication createApplication() {	return new MDI_DrawApplication();	}

		protected void createTools(JToolBar palette) {	super.createTools(palette);	Tool tool = new DragNDropTool(this);	ToolButton tb = createToolButton(IMAGES+"SEL", "Drag N Drop Tool", tool);	palette.add( tb );	}

		public void promptNew() {	newWindow(createDrawing());	}

		public void newWindow(Drawing newDrawing) {	DrawingView newView = createDrawingView(newDrawing);	getDesktop().addToDesktop(newView, Desktop.PRIMARY);	toolDone();	}

		protected DrawingView createInitialDrawingView() {	return NullDrawingView.getManagedDrawingView(this);	}

		public void newView() {	if (!view().isInteractive()) {	return;	}	newWindow(view().drawing());	String copyTitle = getDrawingTitle();	if (copyTitle != null) {	setDrawingTitle(copyTitle);	}	else {	setDrawingTitle(getDefaultDrawingTitle());	}	}

		protected Desktop createDesktop() {	return new MDIDesktopPane(this);	}

		public DrawingView[] views() {	return getDesktop().getAllFromDesktop(Desktop.PRIMARY);	}

		public String getDefaultDrawingTitle() {	return super.getDefaultDrawingTitle() + views().length;	}

		protected void setDrawingTitle(String drawingTitle) {	getDesktop().updateTitle(drawingTitle);	}


}
