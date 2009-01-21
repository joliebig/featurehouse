
package org.jhotdraw.standard; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.ViewChangeListener; 
import org.jhotdraw.util.Command; 
public abstract  class  AbstractCommand  implements Command {
		private String myName;

		private boolean myIsViewRequired;

		private DrawingEditor myDrawingEditor;

		public AbstractCommand(String newName, DrawingEditor newDrawingEditor) {	this(newName, newDrawingEditor, true);	}

		public AbstractCommand(String newName, DrawingEditor newDrawingEditor, boolean newIsViewRequired) {	setName(newName);	setDrawingEditor(newDrawingEditor);	getDrawingEditor().addViewChangeListener(createViewChangeListener());	myIsViewRequired = newIsViewRequired;	}

		protected void viewSelectionChanged(DrawingView oldView, DrawingView newView) {	if (oldView != null) {	oldView.removeFigureSelectionListener(this);	}	if (newView != null) {	newView.addFigureSelectionListener(this);	}	}

		protected void viewCreated(DrawingView view) {	}

		protected void viewDestroying(DrawingView view) {	}

		public DrawingEditor getDrawingEditor() {	return myDrawingEditor;	}

		private void setDrawingEditor(DrawingEditor newDrawingEditor) {	myDrawingEditor = newDrawingEditor;	}

		public DrawingView view() {	return getDrawingEditor().view();	}

		public String name() {	return myName;	}

		public void setName(String newName) {	myName = newName;	}

		public void dispose() {	if (view() != null) {	view().removeFigureSelectionListener(this);	}	}

		public void execute() {	}

		public boolean isExecutable() {	if (isViewRequired()) {	if ((view() == null) || !view().isInteractive()) {	return false;	}	}	return isExecutableWithView();	}

	/*@AJHD protected*/public boolean isViewRequired() {	return myIsViewRequired;	}

		protected boolean isExecutableWithView() {	return true;	}

	 protected ViewChangeListener createViewChangeListener() { return new ViewChangeListener() { public void viewSelectionChanged(DrawingView oldView, DrawingView newView){ AbstractCommand.this.viewSelectionChanged(oldView, newView); } public void viewCreated(DrawingView view){ AbstractCommand.this.viewCreated(view); } public void viewDestroying(DrawingView view){ AbstractCommand.this.viewDestroying(view); } }; }


}
