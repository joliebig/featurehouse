
package org.jhotdraw.figures; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
import java.awt.event.MouseEvent; 
import java.awt.event.InputEvent; 
import java.util.List; 
public  class  BorderTool  extends ActionTool {
		public BorderTool(DrawingEditor editor) {	super(editor);	}

		public void mouseDown(MouseEvent e, int x, int y) {	setView((DrawingView)e.getSource());	if ((e.getModifiers() & InputEvent.CTRL_MASK) == 0) {	super.mouseDown(e, x, y);	}	else {	Figure target = drawing().findFigure(x, y);	if ((target != null) && (target != target.getDecoratedFigure())) {	view().addToSelection(target);	reverseAction(target);	}	}	}

		public void action(Figure figure) {	setUndoActivity(createUndoActivity());	List l = CollectionsFactory.current().createList();	l.add(figure);	l.add(new BorderDecorator(figure));	getUndoActivity().setAffectedFigures(new FigureEnumerator(l));	((BorderTool.UndoActivity)getUndoActivity()).replaceAffectedFigures();	}

		public void reverseAction(Figure figure) {	setUndoActivity(createUndoActivity());	List l = CollectionsFactory.current().createList();	l.add(figure);	l.add(((DecoratorFigure)figure).peelDecoration());	getUndoActivity().setAffectedFigures(new FigureEnumerator(l));	((BorderTool.UndoActivity)getUndoActivity()).replaceAffectedFigures();	}

		protected Undoable createUndoActivity() {	return new BorderTool.UndoActivity(view());	}

		public static  class  UndoActivity  extends UndoableAdapter {
			public UndoActivity(DrawingView newDrawingView) {	super(newDrawingView);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	getDrawingView().clearSelection();	return replaceAffectedFigures();	}

			public boolean redo() {	if (!isRedoable()) {	return false;	}	getDrawingView().clearSelection();	return replaceAffectedFigures();	}

			public boolean replaceAffectedFigures() {	FigureEnumeration fe = getAffectedFigures();	if (!fe.hasNextFigure()) {	return false;	}	Figure oldFigure = fe.nextFigure();	if (!fe.hasNextFigure()) {	return false;	}	Figure replaceFigure = fe.nextFigure();	replaceFigure = getDrawingView().drawing().replace(oldFigure, replaceFigure);	List l = CollectionsFactory.current().createList();	l.add(replaceFigure);	l.add(oldFigure);	setAffectedFigures(new FigureEnumerator(l));	return true;	}


	}


}
