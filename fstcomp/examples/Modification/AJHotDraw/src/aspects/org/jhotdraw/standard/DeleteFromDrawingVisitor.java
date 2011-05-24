
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.CollectionsFactory; 
import java.util.Set; 
public  class  DeleteFromDrawingVisitor  implements FigureVisitor {
		private Set myDeletedFigures;

		private Drawing myDrawing;

		public DeleteFromDrawingVisitor(Drawing newDrawing) {	myDeletedFigures = CollectionsFactory.current().createSet();	setDrawing(newDrawing);	}

		private void setDrawing(Drawing newDrawing) {	myDrawing = newDrawing;	}

		protected Drawing getDrawing() {	return myDrawing;	}

		public void visitFigure(Figure hostFigure) {	if (!myDeletedFigures.contains(hostFigure) && getDrawing().containsFigure(hostFigure)) {	Figure orphanedFigure = getDrawing().orphan(hostFigure);	myDeletedFigures.add(orphanedFigure);	}	}

		public void visitHandle(Handle hostHandle) {	}

		public void visitFigureChangeListener(FigureChangeListener hostFigureChangeListener) {	}

		public FigureEnumeration getDeletedFigures() {	return new FigureEnumerator(myDeletedFigures);	}


}
