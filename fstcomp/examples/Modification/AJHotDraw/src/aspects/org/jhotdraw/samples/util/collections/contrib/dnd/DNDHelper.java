
package org.jhotdraw.contrib.dnd; 
import org.jhotdraw.framework.*; 
import java.awt.Component; 
import java.awt.datatransfer.DataFlavor; 
import java.awt.datatransfer.UnsupportedFlavorException; 
import java.awt.datatransfer.Transferable; 
import java.awt.dnd.*; 
import java.io.*; 
import java.util.List; 
public abstract  class  DNDHelper {
		public static DataFlavor ASCIIFlavor = new DataFlavor("text/plain; charset=ascii", "ASCII text");

		private DragGestureRecognizer dgr;

		private DragGestureListener dragGestureListener;

		private DropTarget dropTarget;

		private DragSourceListener dragSourceListener;

		private DropTargetListener dropTargetListener;

		private boolean isDragSource = false;

		private boolean isDropTarget = false;

		public DNDHelper(boolean newIsDragSource, boolean newIsDropTarget){	isDragSource = newIsDragSource;	isDropTarget = newIsDropTarget;	}

		public void initialize(DragGestureListener dgl) {	if (isDragSource) {	setDragGestureListener(dgl);	setDragSourceListener(createDragSourceListener());	setDragGestureRecognizer(createDragGestureRecognizer(getDragGestureListener()));	}	if (isDropTarget) {	setDropTargetListener(createDropTargetListener());	setDropTarget(createDropTarget());	}	}

		public void deinitialize(){	if (getDragSourceListener() != null) {	destroyDragGestreRecognizer();	setDragSourceListener(null);	}	if (getDropTargetListener() != null) {	setDropTarget(null);	setDropTargetListener(null);	}	}

		protected abstract DrawingView view();

		protected abstract DrawingEditor editor();

		protected static Object processReceivedData(DataFlavor flavor, Transferable transferable) {	Object receivedData = null;	if (transferable == null) {	return null;	}	try { if (flavor.equals(DataFlavor.stringFlavor)) {	receivedData = transferable.getTransferData(DataFlavor.stringFlavor);	}	else if (flavor.equals(DataFlavor.javaFileListFlavor)) {	List aList = (List)transferable.getTransferData(DataFlavor.javaFileListFlavor);	File fList [] = new File[aList.size()];	aList.toArray(fList);	receivedData = fList;	}	else if (flavor.equals(ASCIIFlavor)) {	InputStream is = (InputStream)transferable.getTransferData(ASCIIFlavor);	int length = is.available();	byte[] bytes = new byte[length];	int n = is.read(bytes);	if (n > 0) {	receivedData = new String(bytes, 0, n);	}	}	else if (flavor.equals(DNDFiguresTransferable.DNDFiguresFlavor)) {	receivedData = transferable.getTransferData(DNDFiguresTransferable.DNDFiguresFlavor);	}	}	catch (java.io.IOException ioe) {	System.err.println(ioe);	}	catch (UnsupportedFlavorException ufe) {	System.err.println(ufe);	}	catch (ClassCastException cce) {	System.err.println(cce);	}	return receivedData;	}

		protected int getDragSourceActions() {	return DnDConstants.ACTION_COPY_OR_MOVE;	}

		protected int getDropTargetActions(){	return DnDConstants.ACTION_COPY_OR_MOVE;	}

		protected void setDragGestureListener(DragGestureListener dragGestureListener){	this.dragGestureListener = dragGestureListener;	}

		protected DragGestureListener getDragGestureListener(){	return dragGestureListener;	}

		protected void setDragGestureRecognizer(DragGestureRecognizer dragGestureRecognizer){	dgr = dragGestureRecognizer;	}

		protected DragGestureRecognizer getDragGestureRecognizer(){	return dgr;	}

		protected void setDropTarget(DropTarget newDropTarget){	if ((newDropTarget == null) && (dropTarget != null)) {	dropTarget.setComponent(null);	dropTarget.removeDropTargetListener(getDropTargetListener());	}	dropTarget = newDropTarget;	}

		protected DropTarget createDropTarget() {	DropTarget dt = null;	if (Component.class.isInstance(view())) {	try {	dt = new DropTarget((Component)view(), getDropTargetActions(), getDropTargetListener());	}	catch (NullPointerException npe) {	System.err.println("View Failed to initialize to DND.");	System.err.println("Container likely did not have peer before the DropTarget was added");	System.err.println(npe);	npe.printStackTrace();	}	}	return dt;	}

		protected DragGestureRecognizer createDragGestureRecognizer(DragGestureListener dgl) {	DragGestureRecognizer aDgr = null;	if (Component.class.isInstance(view())) {	Component c = (Component)view();	aDgr =	java.awt.dnd.DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(	c,	getDragSourceActions(),	dgl);	}	return aDgr;	}

		protected void destroyDragGestreRecognizer() {	if (getDragGestureRecognizer() != null) {	getDragGestureRecognizer().removeDragGestureListener(getDragGestureListener());	getDragGestureRecognizer().setComponent(null);	setDragGestureRecognizer(null);	}	}

		protected void setDropTargetListener(DropTargetListener dropTargetListener){	this.dropTargetListener = dropTargetListener;	}

		protected DropTargetListener getDropTargetListener(){	return dropTargetListener;	}

		protected DropTargetListener createDropTargetListener(){	return new JHDDropTargetListener(editor(),view());	}

		public DragSourceListener getDragSourceListener(){	return dragSourceListener;	}

		protected void setDragSourceListener(DragSourceListener dragSourceListener){	this.dragSourceListener = dragSourceListener;	}

		protected DragSourceListener createDragSourceListener(){	return new JHDDragSourceListener(editor(),view());	}


}
