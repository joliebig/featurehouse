
package org.jhotdraw.contrib.dnd; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.DeleteFromDrawingVisitor; 
import org.jhotdraw.util.Undoable; 
import java.awt.Component; 
import java.awt.dnd.*; 
import javax.swing.JComponent; 
public  class  JHDDragSourceListener  implements java.awt.dnd.DragSourceListener {
		private Undoable sourceUndoable;

		private Boolean autoscrollState;

		private DrawingEditor editor;

		public JHDDragSourceListener(DrawingEditor newEditor, DrawingView newView) {	this.editor = newEditor;	}

		protected DrawingEditor editor(){	return editor;	}

		public void dragDropEnd(java.awt.dnd.DragSourceDropEvent dsde) {	DrawingView view = (DrawingView) dsde.getDragSourceContext().getComponent();	log("DragSourceDropEvent-dragDropEnd");	if (dsde.getDropSuccess() == true) {	if (dsde.getDropAction() == DnDConstants.ACTION_MOVE) { log("DragSourceDropEvent-ACTION_MOVE");	setSourceUndoActivity( createSourceUndoActivity( view ) );	DNDFigures df = (DNDFigures)DNDHelper.processReceivedData(DNDFiguresTransferable.DNDFiguresFlavor, dsde.getDragSourceContext().getTransferable());	getSourceUndoActivity().setAffectedFigures( df.getFigures() );	DeleteFromDrawingVisitor deleteVisitor = new DeleteFromDrawingVisitor(view.drawing());	FigureEnumeration fe = getSourceUndoActivity().getAffectedFigures();	while (fe.hasNextFigure()) {	fe.nextFigure().visit(deleteVisitor);	}	view.clearSelection();	view.checkDamage();	editor().getUndoManager().pushUndo( getSourceUndoActivity() );	editor().getUndoManager().clearRedos();	editor().figureSelectionChanged( view );	}	else if (dsde.getDropAction() == DnDConstants.ACTION_COPY) {	log("DragSourceDropEvent-ACTION_COPY");	}	}	if (autoscrollState != null) {	Component c = dsde.getDragSourceContext().getComponent();	if (JComponent.class.isInstance( c )) {	JComponent jc = (JComponent)c;	jc.setAutoscrolls(autoscrollState.booleanValue());	autoscrollState= null;	}	}	}

		public void dragEnter(DragSourceDragEvent dsde) {	log("DragSourceDragEvent-dragEnter");	if (autoscrollState == null) {	Component c = dsde.getDragSourceContext().getComponent();	if (JComponent.class.isInstance( c )) {	JComponent jc = (JComponent)c;	autoscrollState= new Boolean(jc.getAutoscrolls());	jc.setAutoscrolls(false);	}	}	}

		public void dragExit(java.awt.dnd.DragSourceEvent dse) {	}

		public void dragOver(DragSourceDragEvent dsde) {	}

		public void dropActionChanged(DragSourceDragEvent dsde) {	log("DragSourceDragEvent-dropActionChanged");	}

		protected Undoable createSourceUndoActivity(DrawingView drawingView) {	return new RemoveUndoActivity( drawingView );	}

		protected void setSourceUndoActivity(Undoable undoable){	sourceUndoable = undoable;	}

		protected Undoable getSourceUndoActivity(){	return sourceUndoable;	}

		public static  class  RemoveUndoActivity  extends org.jhotdraw.util.UndoableAdapter {
			private boolean undone = false;

			public RemoveUndoActivity(DrawingView view) {	super( view );	log("RemoveUndoActivity created " + view);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (isUndoable()) {	if(getAffectedFigures().hasNextFigure()) {	log("RemoveUndoActivity undo");	getDrawingView().clearSelection();	setAffectedFigures( getDrawingView().insertFigures(getAffectedFigures(), 0, 0,false));	undone = true;	return true;	}	}	return false;	}

			public boolean redo() {	if (isRedoable()) {	log("RemoveUndoActivity redo");	DeleteFromDrawingVisitor deleteVisitor = new DeleteFromDrawingVisitor( getDrawingView().drawing());	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	fe.nextFigure().visit(deleteVisitor);	}	getDrawingView().clearSelection();	setAffectedFigures( deleteVisitor.getDeletedFigures() );	undone = false;	return true;	}	return false;	}

			public void release() {	if(undone == false){	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure f = fe.nextFigure();	getDrawingView().drawing().remove(f);	f.release();	}	}	setAffectedFigures(org.jhotdraw.standard.FigureEnumerator.getEmptyEnumeration());	}


	}

		private static void log(String message){	}


}
