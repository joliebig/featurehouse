
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.CollectionsFactory; 
import java.util.Set; 
public  class  InsertIntoDrawingVisitor  implements FigureVisitor {
		private Set myInsertedFigures;

		private Drawing myDrawing;

		public InsertIntoDrawingVisitor(Drawing newDrawing) {	myInsertedFigures = CollectionsFactory.current().createSet();	setDrawing(newDrawing);	}

		private void setDrawing(Drawing newDrawing) {	myDrawing = newDrawing;	}

		protected Drawing getDrawing() {	return myDrawing;	}

		public void visitFigure(Figure hostFigure) {	if (!myInsertedFigures.contains(hostFigure) && !getDrawing().includes(hostFigure)) {	Figure addedFigure = getDrawing().add(hostFigure);	myInsertedFigures.add(addedFigure);	}	}

		public void visitHandle(Handle hostHandle) {	}

		public void visitFigureChangeListener(FigureChangeListener hostFigureChangeListener) {	}

		public FigureEnumeration getInsertedFigures() {	return new FigureEnumerator(myInsertedFigures);	}


}
