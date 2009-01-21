
package org.jhotdraw.contrib; 
import java.awt.Container; 
import java.awt.Font; 
import java.awt.Rectangle; 
import java.awt.event.MouseEvent; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.standard.CreationTool; 
import org.jhotdraw.standard.SingleFigureEnumerator; 
import org.jhotdraw.standard.TextHolder; 
import org.jhotdraw.util.UndoableAdapter; 
public  class  TextAreaTool  extends CreationTool {
		protected FloatingTextArea fTextField;

		protected TextHolder fTypingTarget;

		protected Figure fEditedFigure;

		public TextAreaTool(DrawingEditor newDrawingEditor, Figure prototype) {	super(newDrawingEditor, prototype);	}

		public void mouseDown(MouseEvent e, int x, int y) {	setView((DrawingView)e.getSource());	Figure pressedFigure = drawing().findFigureInside(x, y);	TextHolder textHolder = null;	if (pressedFigure != null) {	textHolder = pressedFigure.getTextHolder();	}	if ((textHolder != null) && (textHolder.acceptsTyping())) {	beginEdit(textHolder, pressedFigure);	return;	}	if (getTypingTarget() != null) {	endEdit();	if (getCreatedFigure() != null && getCreatedFigure().isEmpty()) {	drawing().remove(getAddedFigure());	setUndoActivity(null);	}	else {	}	setTypingTarget(null);	setCreatedFigure(null);	setEditedFigure(null);	setAddedFigure(null);	editor().toolDone();	}	else {	super.mouseDown(e, x, y);	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	if (getCreatedFigure() == null) {	return;	}	super.mouseDrag(e, x, y);	}

		public void mouseUp(MouseEvent e, int x, int y) {	if (getCreatedFigure() == null) {	return;	}	view().checkDamage();	TextHolder textHolder = (TextHolder)getCreatedFigure();	if (textHolder.acceptsTyping()) {	beginEdit(textHolder, getCreatedFigure());	}	else {	editor().toolDone();	}	}

		public void deactivate() {	endEdit();	super.deactivate();	}

		public void activate() {	super.activate();	getActiveView().clearSelection();	}

		public boolean isActivated() {	return getTypingTarget() != null;	}

		protected void beginEdit(TextHolder figure, Figure selectedFigure) {	if (fTextField == null) {	fTextField = new FloatingTextArea();	}	if (figure != getTypingTarget() && getTypingTarget() != null) {	endEdit();	}	fTextField.createOverlay((Container)view(), getFont(figure));	fTextField.setBounds(fieldBounds(figure), figure.getText());	setTypingTarget(figure);	setEditedFigure(selectedFigure);	setUndoActivity(createUndoActivity());	}

		protected Font getFont(TextHolder figure) {	return figure.getFont();	}

		protected void endEdit() {	if ((getTypingTarget() != null) && (fTextField != null)) {	if (fTextField.getText().length() > 0) {	getTypingTarget().setText(fTextField.getText());	getUndoActivity().setAffectedFigures(	new SingleFigureEnumerator(getEditedFigure()));	((TextAreaTool.UndoActivity)getUndoActivity()).setBackupText(	getTypingTarget().getText());	}	else {	drawing().orphan(getAddedFigure());	}	fTextField.endOverlay();	fTextField = null;	}	}

		private Rectangle fieldBounds(TextHolder figure) {	return figure.textDisplayBox();	}

		protected void setTypingTarget(TextHolder newTypingTarget) {	fTypingTarget = newTypingTarget;	}

		protected Figure getEditedFigure() {	return fEditedFigure;	}

		protected void setEditedFigure(Figure figure) {	fEditedFigure = figure;	}

		protected TextHolder getTypingTarget() {	return fTypingTarget;	}

		public static  class  UndoActivity  extends UndoableAdapter {
			private String myOriginalText;

			private String myBackupText;

			public UndoActivity(DrawingView newDrawingView, String newOriginalText) {	super(newDrawingView);	setOriginalText(newOriginalText);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	getDrawingView().clearSelection();	if (!isValidText(getOriginalText())) {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	getDrawingView().drawing().orphan(fe.nextFigure());	}	}	else if (!isValidText(getBackupText())) {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	getDrawingView().add(fe.nextFigure());	}	setText(getOriginalText());	}	else {	setText(getOriginalText());	}	return true;	}

			public boolean redo() {	if (!super.redo()) {	return false;	}	getDrawingView().clearSelection();	if (!isValidText(getBackupText())) {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	getDrawingView().drawing().orphan(fe.nextFigure());	}	}	else if (!isValidText(getOriginalText())) {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	getDrawingView().drawing().add(fe.nextFigure());	setText(getBackupText());	}	}	else {	setText(getBackupText());	}	return true;	}

			protected boolean isValidText(String toBeChecked) {	return ((toBeChecked != null) && (toBeChecked.length() > 0));	}

			protected void setText(String newText) {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	if (currentFigure.getTextHolder() != null) {	currentFigure.getTextHolder().setText(newText);	}	}	}

			public void setBackupText(String newBackupText) {	myBackupText = newBackupText;	}

			public String getBackupText() {	return myBackupText;	}

			public void setOriginalText(String newOriginalText) {	myOriginalText = newOriginalText;	}

			public String getOriginalText() {	return myOriginalText;	}


	}


}
