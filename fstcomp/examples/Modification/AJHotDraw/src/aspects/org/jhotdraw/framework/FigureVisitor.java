
package org.jhotdraw.framework; 
public  interface  FigureVisitor {
		public void visitFigure(Figure hostFigure);

		public void visitHandle(Handle hostHandle);

		public void visitFigureChangeListener(FigureChangeListener hostFigureChangeListener);


}
