
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.UndoableAdapter; 
import org.jhotdraw.util.Undoable; 
import java.util.*; 
public  class  SendToBackCommand  extends AbstractCommand {
		public SendToBackCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(view().selectionZOrdered());	FigureEnumeration fe = getUndoActivity().getAffectedFigures();	while (fe.hasNextFigure()) {	view().drawing().sendToBack(fe.nextFigure());	}	}

		protected boolean isExecutableWithView() {	return view().selectionCount() > 0;	}

		protected Undoable createUndoActivity() {	return new SendToBackCommand.UndoActivity(view());	}

		public static  class  UndoActivity  extends UndoableAdapter {
			private Hashtable myOriginalLayers;

			public UndoActivity(DrawingView newDrawingView) {	super(newDrawingView);	myOriginalLayers = new Hashtable();	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	int currentFigureLayer = getOriginalLayer(currentFigure);	getDrawingView().drawing().sendToLayer(currentFigure, currentFigureLayer);	}	return true;	}

			public boolean redo() {	if (!isRedoable()) {	return false;	}	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	sendToCommand(fe.nextFigure());	}	return true;	}

			protected void sendToCommand(Figure f) {	getDrawingView().drawing().sendToBack(f);	}

			protected void addOriginalLayer(Figure affectedFigure, int newOriginalLayer) {	myOriginalLayers.put(affectedFigure, new Integer(newOriginalLayer));	}

			protected int getOriginalLayer(Figure lookupAffectedFigure) {	return ((Integer)myOriginalLayers.get(lookupAffectedFigure)).intValue();	}

			public void setAffectedFigures(FigureEnumeration fe) {	super.setAffectedFigures(fe);	FigureEnumeration copyFe = getAffectedFigures();	while (copyFe.hasNextFigure()) {	Figure f = copyFe.nextFigure();	int originalLayer = getDrawingView().drawing().getLayer(f);	addOriginalLayer(f, originalLayer);	}	}


	}


}
