
package org.jhotdraw.samples.javadraw; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
import javax.swing.JApplet; 
import java.awt.*; 
import java.io.*; 
import java.net.*; 
public  class  JavaDrawViewer  extends JApplet  implements DrawingEditor {
		private Drawing fDrawing;

		private Tool fTool;

		private StandardDrawingView fView;

		private transient	UndoManager myUndoManager;

		public void init() {	setUndoManager(new UndoManager());	getContentPane().setLayout(new BorderLayout());	fView = new StandardDrawingView(this, 400, 370);	getContentPane().add("Center", fView);	setTool(new FollowURLTool(this, this));	String filename = getParameter("Drawing");	if (filename != null) {	loadDrawing(filename);	fView.setDrawing(fDrawing);	}	else {	showStatus("Unable to load drawing");	}	}

		public void addViewChangeListener(ViewChangeListener vsl) {	}

		public void removeViewChangeListener(ViewChangeListener vsl) {	}

		private void loadDrawing(String filename) {	try {	URL url = new URL(getCodeBase(), filename);	InputStream stream = url.openStream();	StorableInput reader = new StorableInput(stream);	fDrawing = (Drawing)reader.readStorable();	}	catch (IOException e) {	fDrawing = createDrawing();	System.err.println("Error when Loading: " + e);	showStatus("Error when Loading: " + e);	}	catch (org.aspectj.lang.SoftException e) {	fDrawing = createDrawing();	System.err.println("Error when Loading: " + e.getWrappedThrowable());	showStatus("Error when Loading: " + e.getWrappedThrowable());	}	}

		protected Drawing createDrawing() {	return new StandardDrawing();	}

		public DrawingView view() {	return fView;	}

		public DrawingView[] views() {	return new DrawingView[] { view() };	}

		public Drawing drawing() {	return fDrawing;	}

		public Tool tool() {	return fTool;	}

		public void setTool(Tool newTool) {	fTool = newTool;	}

		public void toolDone() {}

		protected void setUndoManager(UndoManager newUndoManager) {	myUndoManager = newUndoManager;	}

		public UndoManager getUndoManager() {	return myUndoManager;	}


}
