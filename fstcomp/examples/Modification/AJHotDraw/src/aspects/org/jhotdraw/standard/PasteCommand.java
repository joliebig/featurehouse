
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.*; 
import java.awt.*; 
public  class  PasteCommand  extends FigureTransferCommand {
		public PasteCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	Point lastClick = view().lastClick();	FigureSelection selection = (FigureSelection)Clipboard.getClipboard().getContents();	if (selection != null) {	FigureEnumerator affectedFigures = (FigureEnumerator)selection.getData(StandardFigureSelection.TYPE);	if (!/*@AJHD getUndoActivity().getAffectedFigures()*/affectedFigures.hasNextFigure()) {	return;	}	Rectangle r = getBounds(affectedFigures);	affectedFigures.reset();	view().clearSelection();	FigureEnumeration fe = insertFigures(affectedFigures, lastClick.x-r.x, lastClick.y-r.y);	}	}

		public boolean isExecutableWithView() {	return Clipboard.getClipboard().getContents() != null;	}

		private Rectangle getBounds(FigureEnumeration fe) {	Rectangle r = fe.nextFigure().displayBox();	while (fe.hasNextFigure()) {	r.add(fe.nextFigure().displayBox());	}	return r;	}


}
