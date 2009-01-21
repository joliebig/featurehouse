
package org.jhotdraw.samples.javadraw; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.standard.DecoratorFigure; 
import org.jhotdraw.util.Animatable; 
public  class  AnimationDecorator  extends DecoratorFigure  implements Animatable {
		private int fXVelocity;

		private int fYVelocity;

		private static final long serialVersionUID = 7894632974364110685L;

		private int animationDecoratorSerializedDataVersion = 1;

		public AnimationDecorator() { }

		public AnimationDecorator(Figure figure) {	super(figure);	fXVelocity = 4;	fYVelocity = 4;	}

		public void velocity(int xVelocity, int yVelocity) {	fXVelocity = xVelocity;	fYVelocity = yVelocity;	}

		public Point velocity() {	return new Point(fXVelocity, fYVelocity);	}

		public void animationStep() {	int xSpeed = fXVelocity;	int ySpeed = fYVelocity;	Rectangle box = displayBox();	if ((box.x + box.width > 300) && (xSpeed > 0))	xSpeed = -xSpeed;	if ((box.y + box.height > 300) && (ySpeed > 0))	ySpeed = -ySpeed;	if ((box.x < 0) && (xSpeed < 0))	xSpeed = -xSpeed;	if ((box.y < 0) && (ySpeed < 0))	ySpeed = -ySpeed;	velocity(xSpeed, ySpeed);	moveBy(xSpeed, ySpeed);	}

		public synchronized void basicMoveBy(int x, int y) {	super.basicMoveBy(x, y);	}

		public synchronized void basicDisplayBox(Point origin, Point corner) {	super.basicDisplayBox(origin, corner);	}

		public synchronized Rectangle displayBox() {	return super.displayBox();	}


}
