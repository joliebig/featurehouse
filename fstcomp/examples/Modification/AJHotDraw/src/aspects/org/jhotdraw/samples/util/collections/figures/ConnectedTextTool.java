
package org.jhotdraw.figures; 
import java.awt.event.MouseEvent; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.standard.FigureTransferCommand; 
import org.jhotdraw.standard.TextHolder; 
public  class  ConnectedTextTool  extends TextTool {
		private Figure myConnectedFigure;

		public ConnectedTextTool(DrawingEditor editor, Figure prototype) {	super(editor, prototype);	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e, x, y);	if (getTypingTarget() != null) {	TextHolder textHolder = getTypingTarget();	setConnectedFigure(drawing().findFigureInsideWithout(x, y, textHolder.getRepresentingFigure()));	if ((getConnectedFigure() != null) && (textHolder != null) && (getConnectedFigure().getTextHolder() != textHolder)) {	textHolder.connect(getConnectedFigure().getDecoratedFigure());	getConnectedFigure().addDependendFigure(getAddedFigure());	}	}	}

		protected void endEdit() {	super.endEdit();	if ((getUndoActivity() != null) && (getUndoActivity() instanceof ConnectedTextTool.UndoActivity)) {	((ConnectedTextTool.UndoActivity)getUndoActivity()).setConnectedFigure(getConnectedFigure());	}	else if ((getConnectedFigure() != null) && isDeleteTextFigure()) {	getConnectedFigure().removeDependendFigure(getAddedFigure());	}	}

		protected void setConnectedFigure(Figure pressedFigure) {	myConnectedFigure = pressedFigure;	}

		public Figure getConnectedFigure() {	return myConnectedFigure;	}

		public void activate() {	super.activate();	setConnectedFigure(null);	}

		public static  class  UndoActivity  extends TextTool.UndoActivity {
			private Figure myConnectedFigure;

			public UndoActivity(DrawingView newDrawingView, String newOriginalText) {	super(newDrawingView, newOriginalText);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	if (currentFigure.getTextHolder() != null) {	if (!isValidText(getOriginalText())) {	currentFigure.getTextHolder().disconnect(getConnectedFigure());	}	else if (!isValidText(getBackupText())) {	currentFigure.getTextHolder().connect(getConnectedFigure());	}	}	}	return true;	}

			public boolean redo() {	if (!super.redo()) {	return false;	}	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	if (currentFigure.getTextHolder() != null) {	if (!isValidText(getBackupText())) {	currentFigure.getTextHolder().disconnect(getConnectedFigure());	}	else if (!isValidText(getOriginalText())) {	currentFigure.getTextHolder().connect(getConnectedFigure());	}	}	}	return true;	}

			public void setConnectedFigure(Figure newConnectedFigure) {	myConnectedFigure = newConnectedFigure;	}

			public Figure getConnectedFigure() {	return myConnectedFigure;	}


	}

		public static  class  DeleteUndoActivity {
			private Figure myConnectedFigure;

			public DeleteUndoActivity(FigureTransferCommand cmd, Figure newConnectedFigure) {	super(cmd);	setConnectedFigure(newConnectedFigure);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	if (currentFigure.getTextHolder() != null) {	currentFigure.getTextHolder().connect(getConnectedFigure().getDecoratedFigure());	}	}	return true;	}

			public boolean redo() {	if (!super.redo()) {	return false;	}	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	if (currentFigure.getTextHolder() != null) {	currentFigure.getTextHolder().disconnect(getConnectedFigure().getDecoratedFigure());	}	}	return true;	}

			public void setConnectedFigure(Figure newConnectedFigure) {	myConnectedFigure = newConnectedFigure;	}

			public Figure getConnectedFigure() {	return myConnectedFigure;	}


	}


}
