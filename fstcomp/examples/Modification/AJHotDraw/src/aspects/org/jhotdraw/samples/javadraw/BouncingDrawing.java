
package org.jhotdraw.samples.javadraw; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.Animatable; 
public  class  BouncingDrawing  extends StandardDrawing  implements Animatable {
		private static final long serialVersionUID = -8566272817418441758L;

		private int bouncingDrawingSerializedDataVersion = 1;

		public synchronized Figure add(Figure figure) {	if (!(figure instanceof AnimationDecorator) &&	!(figure instanceof ConnectionFigure)) {	figure = new AnimationDecorator(figure);	}	return super.add(figure);	}

		public synchronized Figure remove(Figure figure) {	Figure f = super.remove(figure);	if (f instanceof AnimationDecorator) {	return ((AnimationDecorator) f).peelDecoration();	}	return f;	}

		public synchronized Figure replace(Figure figure, Figure replacement) {	if (!(replacement instanceof AnimationDecorator) &&	!(replacement instanceof ConnectionFigure)) {	replacement = new AnimationDecorator(replacement);	}	return super.replace(figure, replacement);	}

		public void animationStep() {	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	Figure f = fe.nextFigure();	if(!(f instanceof ConnectionFigure)) {	((AnimationDecorator) f).animationStep();	}	}	}


}
