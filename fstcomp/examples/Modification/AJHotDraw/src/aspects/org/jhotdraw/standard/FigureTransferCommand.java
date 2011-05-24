
package org.jhotdraw.standard; 
import java.util.List; 
import org.jhotdraw.util.*; 
import org.jhotdraw.framework.*; 
public abstract  class  FigureTransferCommand  extends AbstractCommand {
		protected FigureTransferCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		protected void deleteFigures(FigureEnumeration fe) { DeleteFromDrawingVisitor deleteVisitor = new DeleteFromDrawingVisitor(view().drawing());	while (fe.hasNextFigure()) {	fe.nextFigure().visit(deleteVisitor);	}	view().clearSelection();	}

		protected void copyFigures(FigureEnumeration fe, int figureCount) {	Clipboard.getClipboard().setContents(new StandardFigureSelection(fe, figureCount));	}

		public FigureEnumeration insertFigures(FigureEnumeration fe, int dx, int dy) {	return view().insertFigures(fe, dx, dy, false);	}

		protected FigureEnumeration collectAffectedFigures() {	FigureEnumeration fe = view().selection();	List affected = CollectionsFactory.current().createList();	Figure f;	FigureEnumeration dfe;	while (fe.hasNextFigure()) {	f = fe.nextFigure();	affected.add(0, f);	dfe = f.getDependendFigures();	if (dfe != null) {	while (dfe.hasNextFigure()) {	affected.add(0, dfe.nextFigure());	}	}	}	fe = new FigureEnumerator(affected);	return fe;	}


}
