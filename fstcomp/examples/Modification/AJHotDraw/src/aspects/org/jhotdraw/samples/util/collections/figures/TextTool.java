
package org.jhotdraw.figures; 
import java.awt.Container; 
import java.awt.Dimension; 
import java.awt.Rectangle; 
import java.awt.event.MouseEvent; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.standard.CreationTool; 
import org.jhotdraw.standard.SingleFigureEnumerator; 
import org.jhotdraw.standard.TextHolder; 
import org.jhotdraw.util.FloatingTextField; 
import org.jhotdraw.util.UndoableAdapter; 
public  class  TextTool  extends CreationTool {
		private FloatingTextField myTextField;

		private TextHolder myTypingTarget;

		private Figure mySelectedFigure;

		public TextTool(DrawingEditor newDrawingEditor, Figure prototype) {	super(newDrawingEditor, prototype);	}

		public void mouseDown(MouseEvent e, int x, int y)	{	setView((DrawingView)e.getSource());	if (getTypingTarget() != null) {	editor().toolDone();	return;	}	TextHolder textHolder = null;	Figure pressedFigure = drawing().findFigureInside(x, y);	if (pressedFigure != null) {	textHolder = pressedFigure.getTextHolder();	setSelectedFigure(pressedFigure);	}	if ((textHolder != null) && textHolder.acceptsTyping()) {	beginEdit(textHolder);	}	else {	super.mouseDown(e, x, y);	view().checkDamage();	beginEdit(getCreatedFigure().getTextHolder());	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	}

		public void mouseUp(MouseEvent e, int x, int y) {	if (!isActive()) {	editor().toolDone();	}	}

		public void deactivate() {	endEdit(); super.deactivate();	}

		public void activate() {	super.activate();	}

		public boolean isActive() {	return (getTypingTarget() != null);	}

		protected void beginEdit(TextHolder figure) {	if (getFloatingTextField() == null) {	setFloatingTextField(createFloatingTextField());	}	if (figure != getTypingTarget() && getTypingTarget() != null) {	endEdit();	}	getFloatingTextField().createOverlay((Container)view(), figure.getFont());	getFloatingTextField().setBounds(fieldBounds(figure), figure.getText());	setTypingTarget(figure);	}

		protected void endEdit() {	if (getTypingTarget() != null) {	if (getAddedFigure() != null) {	if (!isDeleteTextFigure()) {	setUndoActivity(createPasteUndoActivity());	getUndoActivity().setAffectedFigures(	new SingleFigureEnumerator(getAddedFigure())	);	getTypingTarget().setText(getFloatingTextField().getText());	}	}	else if (isDeleteTextFigure()) {	setUndoActivity(createDeleteUndoActivity());	getUndoActivity().setAffectedFigures(	new SingleFigureEnumerator(getSelectedFigure())	);	getUndoActivity().redo();	}	else {	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(	new SingleFigureEnumerator(getTypingTarget().getRepresentingFigure()));	getTypingTarget().setText(getFloatingTextField().getText());	((TextTool.UndoActivity)getUndoActivity()).setBackupText(getTypingTarget().getText());	}	setTypingTarget(null);	getFloatingTextField().endOverlay();	}	else {	setUndoActivity(null);	}	setAddedFigure(null);	setCreatedFigure(null);	setSelectedFigure(null);	}

		protected boolean isDeleteTextFigure() {	return getFloatingTextField().getText().length() == 0;	}

		private Rectangle fieldBounds(TextHolder figure) {	Rectangle box = figure.textDisplayBox();	int nChars = figure.overlayColumns();	Dimension d = getFloatingTextField().getPreferredSize(nChars);	return new Rectangle(box.x, box.y, d.width, d.height);	}

		protected void setTypingTarget(TextHolder newTypingTarget) {	myTypingTarget = newTypingTarget;	}

		protected TextHolder getTypingTarget() {	return myTypingTarget;	}

		private void setSelectedFigure(Figure newSelectedFigure) {	mySelectedFigure = newSelectedFigure;	}

		protected Figure getSelectedFigure() {	return mySelectedFigure;	}

		private FloatingTextField createFloatingTextField() {	return new FloatingTextField();	}

		private void setFloatingTextField(FloatingTextField newFloatingTextField) {	myTextField = newFloatingTextField;	}

		protected FloatingTextField getFloatingTextField() {	return myTextField;	}

		public static  class  UndoActivity  extends UndoableAdapter {
			private String myOriginalText;

			private String myBackupText;

			public UndoActivity(DrawingView newDrawingView, String newOriginalText) {	super(newDrawingView);	setOriginalText(newOriginalText);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	getDrawingView().clearSelection();	setText(getOriginalText());	return true;	}

			public boolean redo() {	if (!super.redo()) {	return false;	}	getDrawingView().clearSelection();	setText(getBackupText());	return true;	}

			protected boolean isValidText(String toBeChecked) {	return ((toBeChecked != null) && (toBeChecked.length() > 0));	}

			protected void setText(String newText) {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	if (currentFigure.getTextHolder() != null) {	currentFigure.getTextHolder().setText(newText);	}	}	}

			public void setBackupText(String newBackupText) {	myBackupText = newBackupText;	}

			public String getBackupText() {	return myBackupText;	}

			public void setOriginalText(String newOriginalText) {	myOriginalText = newOriginalText;	}

			public String getOriginalText() {	return myOriginalText;	}


	}


}
