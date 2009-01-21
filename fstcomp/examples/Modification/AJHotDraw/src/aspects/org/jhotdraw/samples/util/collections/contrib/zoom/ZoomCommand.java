
package org.jhotdraw.contrib.zoom; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.JHotDrawRuntimeException; 
import org.jhotdraw.standard.AbstractCommand; 
public  class  ZoomCommand  extends AbstractCommand {
		protected float scale = 1.0f;

		public ZoomCommand(String newSame, float newScale, DrawingEditor newDrawingEditor) {	super(newSame, newDrawingEditor, true);	scale = newScale;	}

		public void execute() {	zoomView().zoom(scale);	}

		public ZoomDrawingView zoomView() {	Object view = super.view();	if (view instanceof ZoomDrawingView) {	return (ZoomDrawingView)view;	}	throw new JHotDrawRuntimeException("execute should NOT be getting called when view not instanceof ZoomDrawingView");	}

		public float getScale() {	return scale;	}

		public void setScale(float newScale) {	scale = newScale;	}

		protected boolean isExecutableWithView() {	return (view() instanceof ZoomDrawingView);	}


}
