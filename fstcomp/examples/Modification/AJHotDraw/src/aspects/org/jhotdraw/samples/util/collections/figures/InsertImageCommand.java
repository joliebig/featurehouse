
package org.jhotdraw.figures; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
import java.awt.*; 
import java.lang.ref.WeakReference; 
public  class  InsertImageCommand  extends AbstractCommand {
		private String myImageName;

		public InsertImageCommand(String name, String newImageName, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	myImageName = newImageName;	}

		public void execute() {	setUndoActivity(createUndoActivity());	((InsertImageCommand.UndoActivity)getUndoActivity()).insertImage();	}

		protected Undoable createUndoActivity() {	return new InsertImageCommand.UndoActivity(view(), myImageName);	}

		public  class  UndoActivity  extends UndoableAdapter {
			WeakReference	myAffectedImageFigure;

			private String myAffectedImageName;

			UndoActivity(DrawingView newDrawingView, String newAffectedImageName) {	super(newDrawingView);	myAffectedImageName = newAffectedImageName;	setUndoable(true);	setRedoable(true);	}

			protected void setImageFigure(ImageFigure newImageFigure) {	myAffectedImageFigure = new WeakReference(newImageFigure);	}

			protected ImageFigure getImageFigure() {	if ((myAffectedImageFigure == null) || (myAffectedImageFigure.get() == null)) {	Image image = Iconkit.instance().registerAndLoadImage(	(Component)getDrawingView(), myAffectedImageName);	setImageFigure(new ImageFigure(	image, myAffectedImageName, getDrawingView().lastClick()));	}	return (ImageFigure)myAffectedImageFigure.get();	}

			public boolean undo() {	if (super.undo()) {	getDrawingView().clearSelection();	getDrawingView().drawing().orphan(getImageFigure());	return true;	}	return false;	}

			public boolean redo() {	if (isRedoable()) {	insertImage();	return true;	}	return false;	}

			protected void insertImage() {	getDrawingView().add(getImageFigure());	getDrawingView().clearSelection();	getDrawingView().addToSelection(getImageFigure());	}


	}


}
