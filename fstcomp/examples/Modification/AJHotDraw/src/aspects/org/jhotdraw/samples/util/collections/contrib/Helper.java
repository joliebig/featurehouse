
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.DrawingView; 
import java.awt.*; 
public  class  Helper {
		static public DrawingView getDrawingView(Container container) {	DrawingView oldDrawingView = null;	Component[] components = container.getComponents();	for (int i = 0; i < components.length; i++) {	if (components[i] instanceof DrawingView) {	return (DrawingView)components[i];	}	else if (components[i] instanceof Container) {	oldDrawingView = getDrawingView((Container)components[i]);	if (oldDrawingView != null) {	return oldDrawingView;	}	}	}	return null;	}

		static public DrawingView getDrawingView(Component component) {	if (Container.class.isInstance(component)) {	return getDrawingView((Container)component);	}	else if (DrawingView.class.isInstance(component)) {	return (DrawingView)component;	}	else {	return null;	}	}


}
