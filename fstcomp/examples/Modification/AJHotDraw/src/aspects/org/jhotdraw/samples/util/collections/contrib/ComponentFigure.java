
package org.jhotdraw.contrib; 
import org.jhotdraw.figures.AttributeFigure; 
import org.jhotdraw.standard.BoxHandleKit; 
import org.jhotdraw.standard.HandleEnumerator; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.CollectionsFactory; 
import java.awt.Component; 
import java.awt.Rectangle; 
import java.awt.Point; 
import java.awt.Graphics; 
import java.util.List; 
public  class  ComponentFigure  extends AttributeFigure {
		private Rectangle bounds;

		private Component component;

		private ComponentFigure() {	bounds = new Rectangle();	}

		public ComponentFigure(Component newComponent) {	this();	setComponent(newComponent);	}

		public void basicDisplayBox(Point origin, Point corner) {	bounds = new Rectangle(origin);	bounds.add(corner);	}

		protected void basicMoveBy(int dx, int dy) {	bounds.translate(dx, dy);	}

		public Rectangle displayBox() {	return new Rectangle(bounds);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	BoxHandleKit.addHandles(this, handles);	return new HandleEnumerator(handles);	}

		public Component getComponent() {	return this.component;	}

		protected void setComponent(Component newComponent) {	this.component = newComponent;	}

		public void draw(Graphics g) {	getComponent().setBounds(displayBox());	Graphics componentG = g.create(bounds.x, bounds.y, bounds.width, bounds.height);	getComponent().paint(componentG);	}


}
